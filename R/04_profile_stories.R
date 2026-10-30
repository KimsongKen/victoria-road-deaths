# Step 4 - Profile each cluster (severity, location, distinctive traits) and
# generate its plain-English description and story card, entirely in R from
# templates. story_prompts.txt holds aggregate-only prompts for an optional
# text-writing tool.
source("R/00_config.R")

crash <- readRDS(out_path("crash_clustered.rds"))
dca   <- readRDS(out_path("dca_comparison.rds"))
k <- nlevels(crash$cluster)
overall_ksi   <- mean(crash$ksi)
overall_fatal <- mean(crash$fatal)

# Short phrases used to build cluster names from their most distinctive traits.
PHRASES <- list(
  time_band = c("Night (0-5)" = "late-night", "Morning peak (6-9)" = "morning-peak",
                "Midday (10-14)" = "midday", "Afternoon peak (15-18)" = "afternoon-peak",
                "Evening (19-23)" = "evening"),
  day_type = c(Weekend = "weekend", Weekday = "weekday"),
  light = c("Dark, street lit" = "after-dark (street-lit)", "Dark, unlit" = "dark unlit-road",
            "Dusk/dawn" = "dusk/dawn", Unknown = "light not recorded"),
  speed_band = c("50 km/h or less" = "low-speed (50 km/h or less)", "60-70 km/h" = "60-70 km/h",
                 "80-90 km/h" = "80-90 km/h", "100-110 km/h" = "high-speed (100+ km/h)",
                 Unknown = "speed zone not recorded"),
  geometry = c("Mid-block" = "mid-block", "Cross intersection" = "cross-intersection",
               "T intersection" = "T-intersection"),
  road_class = c(Freeway = "freeway", "Arterial highway" = "arterial-highway",
                 "Arterial road" = "arterial-road", "Local road" = "local-street"),
  traffic_ctrl = c(Signals = "signalised", Roundabout = "roundabout",
                   "Give-way/stop sign" = "give-way/stop-sign"),
  weather = c("Rain/snow" = "rain", "Other adverse" = "fog/wind", Unknown = "weather not recorded"),
  surface = c("Wet/slippery" = "wet-road", Unknown = "surface not recorded"),
  n_vehicles = c("1 vehicle" = "single-vehicle", "2 vehicles" = "two-vehicle", "3+ vehicles" = "multi-vehicle"),
  ped_involved = c(Yes = "pedestrian"), moto_involved = c(Yes = "motorcycle"),
  bike_involved = c(Yes = "cyclist"), heavy_involved = c(Yes = "heavy-vehicle"),
  young_driver = c(Yes = "young-driver"), older_driver = c(Yes = "older-driver"),
  old_vehicle = c(Yes = "older-vehicle")
)

# ---- Distinctive traits: levels over-represented in the cluster ----------------
traits <- bind_rows(lapply(CLUSTER_FEATURES, function(f) {
  t <- prop.table(table(crash$cluster, crash[[f]]), 1)
  o <- prop.table(table(crash[[f]]))
  as.data.frame(as.table(t)) |>
    setNames(c("cluster", "level", "share")) |>
    mutate(feature = f, overall = as.vector(o[as.character(level)]), lift = share / overall)
})) |>
  mutate(phrase = mapply(function(f, l) unname(PHRASES[[f]][l]), feature, as.character(level)))
write.csv(traits, out_path("cluster_traits_all.csv"), row.names = FALSE)

distinct <- traits |>
  filter(share >= 0.30, lift >= 1.4) |>
  group_by(cluster) |>
  arrange(desc(lift), .by_group = TRUE)

name_cluster <- function(j) {
  # One phrase per feature (its most over-represented level), strongest first.
  ph <- distinct |> filter(cluster == j, !is.na(phrase)) |> distinct(feature, .keep_all = TRUE) |>
    pull(phrase)
  if (any(grepl("not recorded", ph))) return("Poorly recorded crashes (conditions missing)")
  if (!length(ph)) return("Mixed / average crashes")
  str_to_sentence(paste(head(ph, 3), collapse = ", "))
}

# ---- Profile table -----------------------------------------------------------
lga_top <- crash |> filter(lga != "Unknown") |> count(cluster, lga) |>
  group_by(cluster) |> slice_max(n, n = 3, with_ties = FALSE) |>
  summarise(top_lgas = paste(lga, collapse = ", "))

# LGAs where the cluster is most over-represented (LGAs with 300+ crashes).
lga_size <- crash |> count(lga, name = "lga_n")
lga_over <- crash |> filter(lga != "Unknown") |> count(cluster, lga) |>
  left_join(lga_size, by = "lga") |> filter(lga_n >= 300) |>
  group_by(cluster) |> mutate(cluster_share = sum(n) / nrow(crash)) |>
  mutate(lift = (n / lga_n) / cluster_share) |>
  slice_max(lift, n = 3, with_ties = FALSE) |>
  summarise(overrep_lgas = paste(sprintf("%s (x%.1f)", lga, lift), collapse = ", "))

profiles <- crash |>
  group_by(cluster) |>
  summarise(
    n = n(),
    share = n / nrow(crash),
    ksi_rate = mean(ksi),
    fatal_rate = mean(fatal),
    deaths = sum(n_killed),
    serious = sum(n_serious),
    metro = mean(region == "Metro Melbourne"),
    regional_city = mean(region == "Regional city"),
    rural = mean(region == "Rural/towns"),
    .groups = "drop"
  ) |>
  mutate(
    ksi_relative = ksi_rate / overall_ksi,
    fatal_relative = fatal_rate / overall_fatal,
    share_of_ksi = (ksi_rate * n) / sum(ksi_rate * n),
    share_of_deaths = deaths / sum(deaths),
    where = case_when(metro >= 0.70 ~ "Mostly metro Melbourne",
                      rural >= 0.35 ~ "Over-represented in rural Victoria",
                      TRUE ~ "Metro and regional mix"),
    name = sapply(cluster, name_cluster)
  ) |>
  left_join(lga_top, by = "cluster") |>
  left_join(lga_over, by = "cluster") |>
  left_join(dca$cluster_spread |> mutate(cluster = factor(cluster, levels = levels(crash$cluster))),
            by = "cluster")

# Rank-based severity label for the summary table.
profiles$severity_level <- cut(profiles$ksi_relative, c(0, 0.85, 1.15, Inf),
                               labels = c("Below average", "Average", "Above average"))

# ---- Descriptions and story cards ---------------------------------------------
pct <- function(x, d = 0) paste0(formatC(100 * x, format = "f", digits = d), "%")
lower_first <- function(x) paste0(tolower(substr(x, 1, 1)), substr(x, 2, nchar(x)))
a_an <- function(x) paste(ifelse(grepl("^[aeiou]", tolower(x)) & !grepl("^(uti|uni|use)", tolower(x)), "an", "a"), x)

# Plain-English renderings of feature levels for the story templates.
say <- function(f, level) {
  level <- as.character(level)
  switch(f,
    time_band  = c("Night (0-5)" = "late at night", "Morning peak (6-9)" = "in the morning peak",
                   "Midday (10-14)" = "around the middle of the day",
                   "Afternoon peak (15-18)" = "in the afternoon peak", "Evening (19-23)" = "in the evening")[level],
    light      = c("Daylight" = "in daylight", "Dusk/dawn" = "at dusk or dawn",
                   "Dark, street lit" = "after dark under street lights", "Dark, unlit" = "in the dark on an unlit road",
                   "Unknown" = "with lighting not recorded")[level],
    weather    = c("Clear" = "in clear weather", "Rain/snow" = "in rain", "Other adverse" = "in fog, smoke or strong wind",
                   "Unknown" = "with the weather not recorded")[level],
    surface    = c("Dry" = "on a dry road", "Wet/slippery" = "on a wet or slippery road",
                   "Unknown" = "with the road surface not recorded")[level],
    geometry   = c("Mid-block" = "away from any intersection", "Cross intersection" = "at a cross intersection",
                   "T intersection" = "at a T intersection", "Other/unknown" = "at another kind of junction")[level],
    road_class = c("Freeway" = "a freeway", "Arterial highway" = "an arterial highway", "Arterial road" = "an arterial road",
                   "Local road" = "a local road", "Unknown" = "a road of unrecorded class")[level],
    speed_band = if (level == "Unknown") "an unrecorded speed zone" else a_an(paste(level, "zone")),
    tolower(level))
}

describe <- function(j) {
  pr <- profiles[profiles$cluster == j, ]
  tr <- distinct |> filter(cluster == j) |> head(5)
  trait_txt <- if (nrow(tr)) paste(sprintf("%s: %s (%s of the cluster vs %s overall)",
                                            FEATURE_LABELS[tr$feature], tr$level, pct(tr$share), pct(tr$overall)),
                                    collapse = "; ") else "no single trait stands out"
  sev_word <- if (pr$ksi_relative > 1.15) "more" else if (pr$ksi_relative < 0.85) "less" else "about as"
  sprintf(paste0(
    "Cluster %s holds %s crashes (%s of all). Its most distinctive traits are %s. ",
    "%s of its crashes killed or seriously injured someone, %s the average crash (%s); ",
    "it accounts for %s of all deaths in the data. %s (%s metro, %s rural/towns), with the most crashes in %s. ",
    "Police most often coded these crashes as '%s' (%s), and 80%% of the cluster is spread over %d of the 10 DCA groups."),
    j, format(pr$n, big.mark = ","), pct(pr$share, 1), trait_txt,
    pct(pr$ksi_rate), if (sev_word == "about as") "about as likely as" else paste(sev_word, "likely than"),
    pct(overall_ksi),
    pct(pr$share_of_deaths, 1), pr$where, pct(pr$metro), pct(pr$rural), pr$top_lgas,
    pr$top_dca_group, pct(pr$top_dca_share), pr$dca_groups_for_80pct)
}

# "Typical crash": the most common value of each feature within the cluster.
typical_story <- function(j) {
  d <- crash[crash$cluster == j, ]
  mode_of <- function(f) names(which.max(table(d[[f]])))
  yes <- function(f) mean(d[[f]] == "Yes")
  nv <- mode_of("n_vehicles")
  s <- sprintf(paste0("A typical crash in this group happens on a %s %s, %s, %s, %s. It happens %s on %s in %s, ",
                      "and usually %s involved. "),
               tolower(mode_of("day_type")), say("time_band", mode_of("time_band")),
               say("light", mode_of("light")), say("weather", mode_of("weather")), say("surface", mode_of("surface")),
               say("geometry", mode_of("geometry")), say("road_class", mode_of("road_class")),
               say("speed_band", mode_of("speed_band")),
               c("1 vehicle" = "one vehicle is", "2 vehicles" = "two vehicles are", "3+ vehicles" = "three or more vehicles are")[nv])
  extras <- c(ped_involved = "a pedestrian is hit", bike_involved = "a cyclist is involved",
              moto_involved = "a motorcyclist is involved", heavy_involved = "a heavy vehicle is involved",
              young_driver = "a driver or rider aged 16-25 is involved",
              older_driver = "a driver aged 70+ is involved")
  ex <- sapply(names(extras), yes)
  ex <- sort(ex[ex >= 0.25], decreasing = TRUE)
  if (length(ex)) s <- paste0(s, paste(sprintf("In %s of these crashes %s.", pct(ex), extras[names(ex)]), collapse = " "), " ")
  sev <- prop.table(table(d$severity))
  paste0(s, sprintf("Injuries are %s: %s of crashes are fatal and %s cause a serious injury.",
                    ifelse(mean(d$ksi) > overall_ksi * 1.15, "more often severe than average",
                           ifelse(mean(d$ksi) < overall_ksi * 0.85, "usually minor", "mostly minor")),
                    pct(sev["Fatal"], 1), pct(sev["Serious injury"])))
}

# Representative real crash: the member whose combination of traits is most
# typical of its cluster (highest summed log-share of its own feature levels).
# This is the categorical analogue of a medoid.
typicality <- Reduce(`+`, lapply(CLUSTER_FEATURES, function(f) {
  sh <- prop.table(table(crash$cluster, crash[[f]]), 1)
  log(sh[cbind(as.integer(crash$cluster), as.integer(crash[[f]]))])
}))
set.seed(SEED)
rep_idx <- sapply(levels(crash$cluster), function(j) {
  m <- which(crash$cluster == j)
  best <- m[typicality[m] == max(typicality[m])]
  best[sample.int(length(best), 1)]
})
reps <- crash[rep_idx, ]
saveRDS(reps$accident_no, out_path("representatives.rds"))

veh_raw <- read_raw("vehicle.csv", select = c("ACCIDENT_NO", "VEHICLE_TYPE_DESC"))
real_story <- function(j) {
  r <- reps[reps$cluster == j, ]
  v <- veh_raw[ACCIDENT_NO == r$accident_no, VEHICLE_TYPE_DESC]
  v <- v[!v %in% c("Not Applicable", "Not Known")]
  vt <- table(tolower(v))
  vtxt <- if (length(v)) paste(ifelse(vt == 1, a_an(names(vt)), paste(vt, paste0(names(vt), "s"))),
                               collapse = " and ") else "a vehicle of unrecorded type"
  people <- c(if (r$ped_involved == "Yes") "a pedestrian")
  sprintf(paste0(
    "Real example (crash %s): On a %s in %s at about %s, %s, %s, %s, a crash happened %s on %s ",
    "(%s) in %s (%s). It involved %s%s. Police coded it as '%s' (DCA %s). Outcome: %s."),
    r$accident_no, r$day_of_week, format(r$date, "%B %Y"), r$time,
    say("light", r$light), say("weather", r$weather), say("surface", r$surface),
    say("geometry", r$geometry), say("road_class", r$road_class), say("speed_band", r$speed_band),
    r$lga, tolower(as.character(r$region)),
    vtxt, if (length(people)) paste0(" and ", people) else "",
    str_to_sentence(r$dca_desc), r$dca_code, tolower(as.character(r$severity)))
}

stories <- tibble(
  cluster = levels(crash$cluster),
  name = profiles$name,
  description = sapply(cluster, describe),
  typical_story = sapply(cluster, typical_story),
  real_story = sapply(cluster, real_story)
)

# Aggregate-only prompts for a text-writing tool (no individual crash records).
prompts <- sapply(seq_len(k), function(j) paste0(
  "Write a 90-word plain-English story card for a road-safety audience describing this group of ",
  "Victorian road crashes. Use only these facts and do not invent statistics.\n", stories$description[j], "\n"))
writeLines(paste0("### Cluster ", seq_len(k), "\n", prompts, collapse = "\n"), out_path("story_prompts.txt"))

saveRDS(list(profiles = profiles, stories = stories, distinct = distinct,
             overall_ksi = overall_ksi, overall_fatal = overall_fatal),
        out_path("profiles.rds"))
write.csv(profiles |> select(cluster, name, n, share, ksi_rate, fatal_rate, ksi_relative,
                             share_of_ksi, share_of_deaths, metro, regional_city, rural, where,
                             top_lgas, overrep_lgas, top_dca_group, top_dca_share, dca_groups_for_80pct),
          out_path("cluster_profiles.csv"), row.names = FALSE)
write.csv(stories, out_path("cluster_stories.csv"), row.names = FALSE)

# ---- Figures -----------------------------------------------------------------
lab  <- setNames(sprintf("%s. %s", profiles$cluster, profiles$name), profiles$cluster)
cols <- situation_colours(k)
theme_report <- function(base = 12) theme_minimal(base_size = base) +
  theme(plot.title = element_text(face = "bold", size = base + 2), plot.title.position = "plot",
        plot.subtitle = element_text(colour = "grey35"), panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(), axis.text = element_text(colour = "grey25"))

ggplot(profiles, aes(ksi_rate, reorder(lab[cluster], ksi_rate), fill = cluster)) +
  geom_vline(xintercept = overall_ksi, linetype = 2, colour = "grey30") +
  geom_col(width = 0.7) +
  geom_label(aes(label = sprintf("%s killed or seriously injured  |  %s fatal", pct(ksi_rate), pct(fatal_rate, 1))),
             hjust = -0.03, size = 3.4, colour = "grey20", fill = "white", linewidth = 0) +
  scale_fill_manual(values = cols, guide = "none") +
  scale_x_continuous(labels = scales::percent, expand = expansion(c(0, 0.6))) +
  scale_y_discrete(labels = function(x) str_wrap(x, 42)) +
  labs(title = "How often a crash kills or seriously injures someone",
       subtitle = sprintf("Dashed line = all crashes (%s)", pct(overall_ksi)), x = NULL, y = NULL) +
  theme_report()
ggsave(fig_path("severity_by_cluster.png"), width = 10, height = 5, dpi = 150, bg = "white")

region_df <- crash |> filter(region != "Unknown") |> count(cluster, region) |>
  group_by(cluster) |> mutate(p = n / sum(n))
ggplot(region_df, aes(p, factor(cluster, levels = rev(levels(crash$cluster))), fill = region)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = ifelse(p >= 0.08, pct(p), "")), position = position_stack(vjust = 0.5),
            colour = "white", size = 3.3, fontface = "bold") +
  scale_fill_manual(values = c("Metro Melbourne" = "#3B6FB6", "Regional city" = "#E8A33D", "Rural/towns" = "#2E9E6A")) +
  scale_x_continuous(labels = scales::percent, expand = expansion(c(0, 0.01))) +
  scale_y_discrete(labels = function(x) str_wrap(lab[x], 42)) +
  labs(title = "Where each situation happens", x = NULL, y = NULL, fill = NULL) +
  theme_report() + theme(legend.position = "top", legend.justification = "left")
ggsave(fig_path("region_by_cluster.png"), width = 10, height = 5, dpi = 150, bg = "white")

# Small multiples: every crash in light grey for context, the situation's crashes in its colour.
set.seed(SEED)
pts <- crash |> filter(!is.na(lat)) |> slice_sample(n = 60000)
bg  <- pts |> select(lon, lat)
ggplot(pts, aes(lon, lat)) +
  geom_point(data = bg, colour = "grey85", size = 0.1) +
  geom_point(aes(colour = cluster), size = 0.15, alpha = 0.5) +
  facet_wrap(~cluster, ncol = 4, labeller = labeller(cluster = function(x) str_wrap(lab[x], 32))) +
  scale_colour_manual(values = cols, guide = "none") +
  coord_quickmap() +
  labs(title = "Where each situation's crashes are located",
       subtitle = "Coloured: crashes in the situation. Grey: all crashes (random sample of 60,000).", x = NULL, y = NULL) +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 14), plot.subtitle = element_text(colour = "grey35"),
        strip.text = element_text(size = 9, face = "bold", margin = margin(b = 3)),
        plot.margin = margin(10, 10, 10, 10))
ggsave(fig_path("map_by_cluster.png"), width = 12, height = 5.6, dpi = 150, bg = "white")

log_step("Cluster profiles:")
print(profiles |> transmute(cluster, name, n, share = pct(share, 1), ksi = pct(ksi_rate),
                            fatal = pct(fatal_rate, 1), deaths = pct(share_of_deaths), where), width = 200)
