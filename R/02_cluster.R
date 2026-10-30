# Step 2 - Cluster crashes on their situational features.
#
# Primary method: Multiple Correspondence Analysis (MCA) on all crashes,
# followed by k-means on the retained MCA dimensions. In a first prototype,
# Gower + PAM on a 5,000-crash sample gave a lower silhouette (about 0.08) and
# unstable clusters across samples (ARI 0.2-0.4), while MCA + k-means on the full
# data was both better separated and reproducible, so it became the main method
# and Gower + PAM is kept as an independent cross-check.
#
# k is chosen using three criteria computed for every candidate k:
#   * average silhouette width using Gower distance on the raw categories
#     (comparable across methods), on a fixed random sample of crashes
#   * seed stability: ARI between k-means runs started from different seeds
#   * subsample stability: fit on a random half, predict the other half
# The smallest cluster must hold at least MIN_CLUSTER_PCT of crashes and k must
# be at least K_MIN (interpretability). Among the stable candidates we take the
# highest silhouette.
source("R/00_config.R")
suppressPackageStartupMessages({ library(cluster); library(aricode); library(FactoMineR) })

MIN_STABILITY   <- 0.90
MIN_CLUSTER_PCT <- 2

crash <- readRDS(out_path("crash_level.rds"))
X <- as.data.frame(crash[CLUSTER_FEATURES])
n <- nrow(X); p <- ncol(X)

quiet_kmeans <- function(...) withCallingHandlers(kmeans(...), warning = function(w)
  if (grepl("Quick-TRANSfer", conditionMessage(w))) invokeRestart("muffleWarning"))

nearest_centre <- function(C, centers)
  max.col(-sapply(seq_len(nrow(centers)), function(j) colSums((t(C) - centers[j, ])^2)),
          ties.method = "first")

# ---- 1. MCA on every crash ---------------------------------------------------
log_step("MCA on ", n, " crashes x ", p, " features")
mca_all <- MCA(X, ncp = 30, graph = FALSE)
eig <- as.data.frame(mca_all$eig)
names(eig) <- c("eigenvalue", "pct_var", "cum_pct_var")
# Raw MCA inertia percentages understate how much structure the first axes hold,
# so we use Benzecri's correction: only eigenvalues above 1/p carry signal, and
# we keep enough dimensions to reach 95% of the corrected inertia. (Keeping every
# dimension with eigenvalue > 1/p, 15 here, let noise axes make k-means unstable.)
eig$benzecri <- ifelse(eig$eigenvalue > 1 / p, (p / (p - 1))^2 * (eig$eigenvalue - 1 / p)^2, 0)
eig$benzecri_cum_pct <- 100 * cumsum(eig$benzecri) / sum(eig$benzecri)
n_dims <- which(eig$benzecri_cum_pct >= 95)[1]
C <- mca_all$ind$coord[, seq_len(n_dims)]
log_step(sprintf("Retained %d MCA dimensions (%.1f%% of Benzecri-corrected inertia, %.1f%% raw)",
                 n_dims, eig$benzecri_cum_pct[n_dims], eig$cum_pct_var[n_dims]))
write.csv(cbind(dim = seq_len(nrow(eig)), eig), out_path("mca_eigenvalues.csv"), row.names = FALSE)

# MCA variable contributions to the first dimensions, for the report.
saveRDS(list(eig = eig, n_dims = n_dims,
             var_coord = mca_all$var$coord[, 1:2], var_contrib = mca_all$var$contrib[, 1:2]),
        out_path("mca_summary.rds"))

# ---- 2. Evaluate candidate k ------------------------------------------------
set.seed(SEED)
sil_idx <- sample(n, SAMPLE_N)
D_gower <- daisy(X[sil_idx, ], metric = "gower")

set.seed(SEED)
half <- sample(n, n %/% 2)

eval_k <- lapply(K_RANGE, function(k) {
  runs <- lapply(1:3, function(s) { set.seed(SEED + s); quiet_kmeans(C, k, nstart = 10, iter.max = 100) })
  best <- runs[[which.min(sapply(runs, `[[`, "tot.withinss"))]]
  seed_ari <- mean(c(ARI(runs[[1]]$cluster, runs[[2]]$cluster), ARI(runs[[1]]$cluster, runs[[3]]$cluster)))
  set.seed(SEED)
  km_half <- quiet_kmeans(C[half, ], k, nstart = 10, iter.max = 100)
  pred_other <- nearest_centre(C[-half, ], km_half$centers)
  sub_ari <- ARI(pred_other, best$cluster[-half])
  sil_g <- mean(silhouette(best$cluster[sil_idx], D_gower)[, 3])
  row <- data.frame(k = k, silhouette_gower = sil_g, seed_stability = seed_ari,
                    subsample_stability = sub_ari, min_cluster_pct = 100 * min(best$size) / n,
                    between_ss_pct = 100 * best$betweenss / best$totss)
  log_step(sprintf("  k = %2d  silhouette = %.3f  seed ARI = %.3f  subsample ARI = %.3f  smallest = %.1f%%",
                   k, sil_g, seed_ari, sub_ari, row$min_cluster_pct))
  list(row = row, fit = best)
})
k_tab <- do.call(rbind, lapply(eval_k, `[[`, "row"))
k_tab$eligible <- with(k_tab, k >= K_MIN & seed_stability >= MIN_STABILITY &
                         subsample_stability >= MIN_STABILITY & min_cluster_pct >= MIN_CLUSTER_PCT)
if (!any(k_tab$eligible)) {
  warning("No k met the stability criteria; choosing on silhouette alone")
  k_tab$eligible <- TRUE
}
k_best <- if (!is.na(K_OVERRIDE)) K_OVERRIDE else
  k_tab$k[k_tab$eligible][which.max(k_tab$silhouette_gower[k_tab$eligible])]
k_tab$chosen <- k_tab$k == k_best
write.csv(k_tab, out_path("k_selection.csv"), row.names = FALSE)
log_step("Chosen k = ", k_best)

k_long <- k_tab |>
  select(k, `Silhouette (Gower)` = silhouette_gower, `Seed stability (ARI)` = seed_stability,
         `Subsample stability (ARI)` = subsample_stability) |>
  pivot_longer(-k)
ggplot(k_long, aes(k, value)) +
  geom_vline(xintercept = k_best, colour = "#c53030", linetype = 2) +
  geom_line(colour = "grey50") + geom_point(size = 2.2, colour = "#2b6cb0") +
  facet_wrap(~name, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = K_RANGE) +
  labs(title = "Choosing the number of clusters",
       subtitle = sprintf("MCA (%d dims) + k-means; dashed line = chosen k = %d", n_dims, k_best),
       x = "Number of clusters (k)", y = NULL) +
  theme_minimal(base_size = 11)
ggsave(fig_path("k_selection.png"), width = 7, height = 6.5, dpi = 150)

km <- eval_k[[match(k_best, K_RANGE)]]$fit

# Relabel so that cluster 1 is the largest.
size_order <- order(-km$size)
relabel    <- match(seq_len(k_best), size_order)
cl         <- relabel[km$cluster]

# ---- 3. Cross-check with Gower + PAM -----------------------------------------
log_step("Cross-check: Gower + PAM with k = ", k_best, " on the silhouette sample")
pam_fit <- pam(D_gower, k = k_best, diss = TRUE, pamonce = 5)
X_int <- sapply(X, as.integer)
med <- X_int[sil_idx[pam_fit$id.med], , drop = FALSE]
d_med <- sapply(seq_len(k_best), function(j) rowSums(X_int != matrix(med[j, ], n, p, byrow = TRUE)) / p)
cl_pam <- max.col(-d_med, ties.method = "first")
ari_pam <- ARI(cl, cl_pam); nmi_pam <- NMI(cl, cl_pam)
sil_pam <- pam_fit$silinfo$avg.width
log_step(sprintf("Gower + PAM: silhouette = %.3f (MCA + k-means %.3f); agreement ARI = %.3f, NMI = %.3f",
                 sil_pam, k_tab$silhouette_gower[k_tab$chosen], ari_pam, nmi_pam))

# Per-crash silhouette on the sample, for the report.
sil_obj <- silhouette(cl[sil_idx], D_gower)
saveRDS(data.frame(cluster = sil_obj[, 1], sil = sil_obj[, 3]), out_path("sample_silhouette.rds"))

validation <- list(
  k = k_best, n_dims = n_dims, inertia_pct = eig$cum_pct_var[n_dims], sample_n = SAMPLE_N,
  k_table = k_tab,
  silhouette = k_tab$silhouette_gower[k_tab$chosen],
  seed_stability = k_tab$seed_stability[k_tab$chosen],
  subsample_stability = k_tab$subsample_stability[k_tab$chosen],
  pam_silhouette = sil_pam, ari_pam = ari_pam, nmi_pam = nmi_pam
)
saveRDS(validation, out_path("validation.rds"))

crash$cluster     <- factor(cl, levels = seq_len(k_best))
crash$cluster_pam <- cl_pam
crash$mca1 <- C[, 1]; crash$mca2 <- C[, 2]
saveRDS(crash, out_path("crash_clustered.rds"))
log_step("Saved clustered dataset")
