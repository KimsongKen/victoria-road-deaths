# Shared settings for the Victorian crash clustering project.
# Every script sources this file first.

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
})

# Project root = folder that contains this R/ directory.
PROJECT_DIR <- normalizePath(Sys.getenv("CRASH_PROJECT_DIR", "."), winslash = "/")

# Raw CSVs from the Victorian Crash Data release. Override with the
# CRASH_DATA_DIR environment variable if the data lives elsewhere.
DATA_DIR <- normalizePath(
  Sys.getenv("CRASH_DATA_DIR", file.path(PROJECT_DIR, "..", "data", "data")),
  winslash = "/", mustWork = FALSE
)

OUT_DIR <- file.path(PROJECT_DIR, "output")
FIG_DIR <- file.path(OUT_DIR, "figures")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

SEED        <- 2024
SAMPLE_N    <- 5000         # crashes used for silhouette and the Gower + PAM cross-check
K_RANGE     <- 4:14         # candidate numbers of clusters (all evaluated and reported)
K_MIN       <- 6            # smallest k we accept: fewer clusters cannot separate the
                            # pedestrian / motorcycle / rural-night profiles we need to study
K_OVERRIDE  <- NA           # set to an integer to force a specific k

# Features that describe the *situation* of the crash. We deliberately leave out
# DCA code and accident type (they are what we compare against), severity (the
# outcome we profile) and location (what we map), so none of those can leak into
# the clusters.
CLUSTER_FEATURES <- c(
  "time_band", "day_type", "light", "speed_band", "geometry", "road_class",
  "traffic_ctrl", "weather", "surface", "n_vehicles",
  "ped_involved", "moto_involved", "bike_involved", "heavy_involved",
  "young_driver", "older_driver", "old_vehicle"
)

FEATURE_LABELS <- c(
  time_band = "Time of day", day_type = "Day", light = "Light",
  speed_band = "Speed zone", geometry = "Road geometry", road_class = "Road class",
  traffic_ctrl = "Traffic control", weather = "Weather", surface = "Road surface",
  n_vehicles = "Vehicles involved", ped_involved = "Pedestrian involved",
  moto_involved = "Motorcycle involved", bike_involved = "Bicycle involved",
  heavy_involved = "Heavy vehicle involved", young_driver = "Driver/rider aged 16-25",
  older_driver = "Driver/rider aged 70+", old_vehicle = "Oldest vehicle 15+ years"
)

# Official DCA groups (first two digits of the three-digit DCA code).
DCA_GROUPS <- c(
  "10" = "Pedestrian",
  "11" = "Vehicles from adjacent directions (intersection)",
  "12" = "Vehicles from opposing directions",
  "13" = "Vehicles from same direction",
  "14" = "Manoeuvring",
  "15" = "Overtaking",
  "16" = "On path",
  "17" = "Off path on straight",
  "18" = "Off path on curve",
  "19" = "Passenger and miscellaneous"
)

# One colour per situation (cluster), shared by the figures, the report map and the Shiny app.
SITUATION_COLOURS <- c("#3B6FB6", "#D1495B", "#2E9E6A", "#7B5EA7", "#E8A33D", "#D96FA8", "#8C8C8C",
                       "#17A2B8", "#A6A23A", "#8C564B", "#393B79", "#637939", "#843C39", "#7B4173")
situation_colours <- function(k) setNames(SITUATION_COLOURS[seq_len(k)], seq_len(k))

# Basemap for leaflet maps. Esri's tiles load even when the HTML report is opened as a
# local file; OpenStreetMap blocks requests that carry no website referer.
BASEMAP <- "Esri.WorldGrayCanvas"
BASEMAP_LABELS <- "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}"

out_path <- function(...) file.path(OUT_DIR, ...)
fig_path <- function(...) file.path(FIG_DIR, ...)

read_raw <- function(name, ...) {
  f <- file.path(DATA_DIR, name)
  if (!file.exists(f)) stop("Missing data file: ", f, "\nSet CRASH_DATA_DIR to the folder with the CSVs.")
  dt <- fread(f, encoding = "UTF-8", na.strings = c("", "NA"), ...)
  setnames(dt, sub("^﻿", "", names(dt)))   # strip byte-order mark
  dt
}

log_step <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")
