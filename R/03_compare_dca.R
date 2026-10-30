# Step 3 - Compare the data-driven clusters with the official DCA categories.
#   * cross-tabulation and lift heat map (cluster x DCA group)
#   * agreement scores: adjusted Rand index (ARI) and normalised mutual information (NMI)
#   * how many DCA groups each cluster spans, and how each DCA group splits
#   * does knowing the cluster add information about severity beyond DCA?
source("R/00_config.R")
suppressPackageStartupMessages(library(aricode))

crash <- readRDS(out_path("crash_clustered.rds"))
k <- nlevels(crash$cluster)
crash$accident_type <- factor(crash$accident_type)
crash$dca_code <- factor(crash$dca_code)

# ---- Agreement scores ---------------------------------------------------------
agree <- data.frame(
  comparison = c("Clusters vs DCA group (10)", "Clusters vs DCA code (81)",
                 "Clusters vs accident type (9)", "DCA group vs accident type (reference)"),
  ARI = c(ARI(crash$cluster, crash$dca_group), ARI(crash$cluster, crash$dca_code),
          ARI(crash$cluster, crash$accident_type), ARI(crash$dca_group, crash$accident_type)),
  NMI = c(NMI(crash$cluster, crash$dca_group), NMI(crash$cluster, crash$dca_code),
          NMI(crash$cluster, crash$accident_type), NMI(crash$dca_group, crash$accident_type))
)
# Permutation baseline: what ARI/NMI look like when cluster labels are shuffled.
set.seed(SEED)
perm <- replicate(20, { s <- sample(crash$cluster); c(ARI(s, crash$dca_group), NMI(s, crash$dca_group)) })
agree <- rbind(agree, data.frame(comparison = "Shuffled clusters vs DCA group (baseline, mean of 20)",
                                 ARI = mean(perm[1, ]), NMI = mean(perm[2, ])))
write.csv(agree, out_path("dca_agreement.csv"), row.names = FALSE)
log_step("Agreement with DCA:"); print(agree, digits = 3, row.names = FALSE)

# ---- Cross-tabulation ---------------------------------------------------------
tab <- table(cluster = crash$cluster, dca_group = crash$dca_group)
write.csv(as.data.frame.matrix(tab), out_path("crosstab_cluster_dca_counts.csv"))
row_pct <- prop.table(tab, 1)
col_pct <- prop.table(tab, 2)
expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
lift <- tab / expected
chi <- suppressWarnings(chisq.test(tab))
cramers_v <- sqrt(chi$statistic / (sum(tab) * (min(dim(tab)) - 1)))

heat <- as.data.frame(as.table(row_pct)) |>
  rename(share = Freq) |>
  mutate(lift = as.vector(lift), n = as.vector(tab),
         label = ifelse(share >= 0.03, sprintf("%.0f%%", 100 * share), ""))
ggplot(heat, aes(dca_group, cluster, fill = log2(pmax(lift, 1 / 16)))) +
  geom_tile(colour = "white") +
  geom_text(aes(label = label), size = 3) +
  scale_fill_gradient2(low = "#2b6cb0", mid = "white", high = "#c53030", midpoint = 0,
                       limits = c(-4, 4), breaks = -4:4,
                       labels = c("1/16", "1/8", "1/4", "1/2", "1", "2", "4", "8", "16"),
                       name = "Lift vs\nall crashes") +
  scale_x_discrete(labels = function(x) str_wrap(x, 18)) +
  scale_y_discrete(limits = rev, labels = function(x) paste("Cluster", x)) +
  labs(title = "Official DCA groups inside each data-driven cluster",
       subtitle = "Numbers: % of the cluster's crashes in each DCA group. Colour: over/under-representation.",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), panel.grid = element_blank())
ggsave(fig_path("dca_heatmap.png"), width = 10, height = 5.5, dpi = 150)

# ---- Spread: how many DCA groups does each cluster need, and vice versa ------
groups_to_cover <- function(p, cover = 0.8) which(cumsum(sort(p, decreasing = TRUE)) >= cover)[1]
norm_entropy <- function(p) { p <- p[p > 0]; -sum(p * log(p)) / log(length(DCA_GROUPS)) }

cluster_spread <- data.frame(
  cluster = levels(crash$cluster),
  top_dca_group = colnames(row_pct)[apply(row_pct, 1, which.max)],
  top_dca_share = apply(row_pct, 1, max),
  dca_groups_for_80pct = apply(row_pct, 1, groups_to_cover),
  dca_entropy = apply(row_pct, 1, norm_entropy)
)
dca_spread <- data.frame(
  dca_group = colnames(tab), n = colSums(tab),
  top_cluster = rownames(col_pct)[apply(col_pct, 2, which.max)],
  top_cluster_share = apply(col_pct, 2, max),
  clusters_for_80pct = apply(col_pct, 2, groups_to_cover)
)
write.csv(cluster_spread, out_path("cluster_dca_spread.csv"), row.names = FALSE)
write.csv(dca_spread, out_path("dca_cluster_spread.csv"), row.names = FALSE)

# ---- Does the cluster explain severity beyond DCA? ---------------------------
auc <- function(y, p) { r <- rank(p); n1 <- as.numeric(sum(y)); n0 <- length(y) - n1
  (sum(r[y]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
fit_models <- function(outcome) {
  y <- crash[[outcome]]
  m0 <- glm(y ~ 1, family = binomial, data = crash)
  m_dca <- glm(y ~ dca_group, family = binomial, data = crash)
  m_cl  <- glm(y ~ cluster, family = binomial, data = crash)
  m_both <- glm(y ~ dca_group + cluster, family = binomial, data = crash)
  lrt <- anova(m_dca, m_both, test = "LRT")
  ll0 <- as.numeric(logLik(m0))
  data.frame(outcome = outcome,
             model = c("DCA group only", "Cluster only", "DCA group + cluster"),
             AIC = c(AIC(m_dca), AIC(m_cl), AIC(m_both)),
             mcfadden_r2 = 1 - c(logLik(m_dca), logLik(m_cl), logLik(m_both)) / ll0,
             AUC = c(auc(y, fitted(m_dca)), auc(y, fitted(m_cl)), auc(y, fitted(m_both))),
             lrt_p_cluster_added = c(NA, NA, lrt$`Pr(>Chi)`[2]),
             lrt_chisq = c(NA, NA, lrt$Deviance[2]))
}
sev_models <- rbind(fit_models("ksi"), fit_models("fatal"))
write.csv(sev_models, out_path("severity_models.csv"), row.names = FALSE)
log_step("Severity models:"); print(sev_models, digits = 4, row.names = FALSE)

saveRDS(list(agree = agree, tab = tab, row_pct = row_pct, col_pct = col_pct, lift = lift,
             chisq = chi, cramers_v = unname(cramers_v),
             cluster_spread = cluster_spread, dca_spread = dca_spread, sev_models = sev_models),
        out_path("dca_comparison.rds"))
log_step("Saved DCA comparison")
