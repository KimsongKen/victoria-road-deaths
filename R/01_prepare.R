# Step 1 - Join the accident, node, condition, vehicle and person tables into
# one row per crash, then derive the categorical clustering features.
source("R/00_config.R")

log_step("Reading raw tables from ", DATA_DIR)
acc  <- read_raw("accident.csv")
node <- read_raw("node.csv")
atm  <- read_raw("atmospheric_cond.csv")
surf <- read_raw("road_surface_cond.csv")
veh  <- read_raw("vehicle.csv", select = c("ACCIDENT_NO", "VEHICLE_ID", "VEHICLE_YEAR_MANUF",
                                          "VEHICLE_TYPE_DESC", "TRAFFIC_CONTROL_DESC"))
per  <- read_raw("person.csv", select = c("ACCIDENT_NO", "PERSON_ID", "AGE_GROUP",
                                         "INJ_LEVEL_DESC", "ROAD_USER_TYPE_DESC"))

# ---- Location: one node per crash -------------------------------------------
# A few crashes have two node rows (same point, different urban label); keep the first.
loc <- node[!duplicated(ACCIDENT_NO),
            .(ACCIDENT_NO, LGA_NAME, DEG_URBAN_NAME, LATITUDE, LONGITUDE, POSTCODE_CRASH)]

# ---- Weather and surface: a crash can list several, keep the most adverse ----
weather_rank <- c("Raining" = 1, "Snowing" = 1, "Fog" = 2, "Smoke" = 2, "Dust" = 2,
                  "Strong winds" = 3, "Clear" = 4, "Not known" = 5)
# Fog/smoke/dust and strong winds are each under 1% of crashes, so they share a level.
weather_lab  <- c("1" = "Rain/snow", "2" = "Other adverse", "3" = "Other adverse",
                  "4" = "Clear", "5" = "Unknown")
atm[, r := weather_rank[ATMOSPH_COND_DESC]]
wx <- atm[, .(weather = weather_lab[as.character(min(r, na.rm = TRUE))]), by = ACCIDENT_NO]

surface_rank <- c("Wet" = 1, "Icy" = 1, "Snowy" = 1, "Muddy" = 1, "Dry" = 2, "Unk." = 3)
surface_lab  <- c("1" = "Wet/slippery", "2" = "Dry", "3" = "Unknown")
surf[, r := surface_rank[SURFACE_COND_DESC]]
sf <- surf[, .(surface = surface_lab[as.character(min(r, na.rm = TRUE))]), by = ACCIDENT_NO]

# ---- Vehicles: mix of vehicle types, oldest vehicle, traffic control --------
crash_year <- acc[, .(ACCIDENT_NO, yr = as.integer(substr(ACCIDENT_DATE, 1, 4)))]
veh <- merge(veh, crash_year, by = "ACCIDENT_NO")
veh[, age := fifelse(VEHICLE_YEAR_MANUF >= 1950 & VEHICLE_YEAR_MANUF <= yr + 1,
                     pmax(yr - VEHICLE_YEAR_MANUF, 0L), NA_integer_)]
moto_types  <- c("Motor Cycle", "Motor Scooter", "Moped")
heavy_regex <- "Heavy Vehicle|Prime Mover|Bus/Coach|Rigid Truck"
ctrl_rank   <- function(x) fcase(x %in% c("Stop-go lights", "Ped. lights"), 1L,
                                 x == "Roundabout", 2L,
                                 x %in% c("Giveway sign", "Stop sign"), 3L,
                                 default = 4L)
ctrl_lab <- c("Signals", "Roundabout", "Give-way/stop sign", "None/other")

vf <- veh[, .(
  moto  = any(VEHICLE_TYPE_DESC %in% moto_types),
  bike  = any(VEHICLE_TYPE_DESC == "Bicycle"),
  heavy = any(grepl(heavy_regex, VEHICLE_TYPE_DESC)),
  oldest_age = if (all(is.na(age))) NA_integer_ else max(age, na.rm = TRUE),
  ctrl  = min(ctrl_rank(TRAFFIC_CONTROL_DESC))
), by = ACCIDENT_NO]

# ---- People: pedestrians, young and older drivers/riders --------------------
operators <- c("Drivers", "Motorcyclists")
pf <- per[, .(
  ped    = any(ROAD_USER_TYPE_DESC == "Pedestrians"),
  young  = any(ROAD_USER_TYPE_DESC %in% operators & AGE_GROUP %in% c("16-17", "18-21", "22-25")),
  older  = any(ROAD_USER_TYPE_DESC %in% operators & AGE_GROUP == "70+")
), by = ACCIDENT_NO]

# ---- Assemble one row per crash ---------------------------------------------
log_step("Joining tables")
crash <- acc |>
  merge(loc, by = "ACCIDENT_NO", all.x = TRUE) |>
  merge(wx,  by = "ACCIDENT_NO", all.x = TRUE) |>
  merge(sf,  by = "ACCIDENT_NO", all.x = TRUE) |>
  merge(vf,  by = "ACCIDENT_NO", all.x = TRUE) |>
  merge(pf,  by = "ACCIDENT_NO", all.x = TRUE)

yn <- function(x) factor(fifelse(!is.na(x) & x, "Yes", "No"), levels = c("No", "Yes"))
hour <- as.integer(substr(crash$ACCIDENT_TIME, 1, 2))

crash <- as_tibble(crash) |>
  transmute(
    accident_no = ACCIDENT_NO,
    date        = as.Date(ACCIDENT_DATE),
    year        = as.integer(format(date, "%Y")),
    time        = substr(ACCIDENT_TIME, 1, 5),
    hour        = hour,
    day_of_week = DAY_WEEK_DESC,

    # ---- clustering features ----
    time_band = factor(cut(hour, c(-1, 5, 9, 14, 18, 23),
                           labels = c("Night (0-5)", "Morning peak (6-9)", "Midday (10-14)",
                                      "Afternoon peak (15-18)", "Evening (19-23)"))),
    day_type  = factor(if_else(DAY_WEEK_DESC %in% c("Saturday", "Sunday"), "Weekend", "Weekday")),
    light     = factor(case_when(
      LIGHT_CONDITION == 1 ~ "Daylight",
      LIGHT_CONDITION == 2 ~ "Dusk/dawn",
      LIGHT_CONDITION == 3 ~ "Dark, street lit",
      LIGHT_CONDITION %in% c(4, 5, 6) ~ "Dark, unlit",
      TRUE ~ "Unknown")),
    speed_band = factor(case_when(
      SPEED_ZONE <= 50 ~ "50 km/h or less",
      SPEED_ZONE <= 75 ~ "60-70 km/h",
      SPEED_ZONE <= 90 ~ "80-90 km/h",
      SPEED_ZONE <= 110 ~ "100-110 km/h",
      TRUE ~ "Unknown"),
      levels = c("50 km/h or less", "60-70 km/h", "80-90 km/h", "100-110 km/h", "Unknown")),
    geometry = factor(case_when(
      ROAD_GEOMETRY_DESC == "Not at intersection" ~ "Mid-block",
      ROAD_GEOMETRY_DESC == "Cross intersection"  ~ "Cross intersection",
      ROAD_GEOMETRY_DESC == "T intersection"      ~ "T intersection",
      TRUE ~ "Other/unknown")),
    road_class = factor(case_when(
      RMA == "Freeway" ~ "Freeway",
      RMA == "Arterial Highway" ~ "Arterial highway",
      RMA == "Arterial Other" ~ "Arterial road",
      RMA %in% c("Local Road", "Non Arterial") ~ "Local road",
      TRUE ~ "Unknown")),
    traffic_ctrl = factor(ctrl_lab[coalesce(ctrl, 4L)], levels = ctrl_lab),
    weather    = factor(coalesce(weather, "Unknown")),
    surface    = factor(coalesce(surface, "Unknown")),
    n_vehicles = factor(case_when(NO_OF_VEHICLES <= 1 ~ "1 vehicle",
                                  NO_OF_VEHICLES == 2 ~ "2 vehicles",
                                  TRUE ~ "3+ vehicles")),
    ped_involved   = yn(ped),
    moto_involved  = yn(moto),
    bike_involved  = yn(bike),
    heavy_involved = yn(heavy),
    young_driver   = yn(young),
    older_driver   = yn(older),
    old_vehicle    = factor(case_when(is.na(oldest_age) ~ "Unknown",
                                      oldest_age >= 15 ~ "Yes", TRUE ~ "No"),
                            levels = c("No", "Yes", "Unknown")),

    # ---- comparison / outcome / location (NOT used for clustering) ----
    dca_code   = DCA_CODE,
    dca_desc   = str_squish(DCA_DESC),
    dca_group  = factor(DCA_GROUPS[substr(as.character(DCA_CODE), 1, 2)], levels = DCA_GROUPS),
    accident_type = ACCIDENT_TYPE_DESC,
    severity   = factor(case_when(SEVERITY == 1 ~ "Fatal", SEVERITY == 2 ~ "Serious injury",
                                  SEVERITY == 3 ~ "Other injury", TRUE ~ "Non-injury"),
                        levels = c("Fatal", "Serious injury", "Other injury", "Non-injury")),
    fatal      = SEVERITY == 1,
    ksi        = SEVERITY %in% c(1, 2),   # killed or seriously injured
    n_killed   = NO_PERSONS_KILLED,
    n_serious  = NO_PERSONS_INJ_2,
    n_persons  = NO_PERSONS,
    lga        = str_to_title(coalesce(LGA_NAME, "Unknown")),
    region     = factor(case_when(
      DEG_URBAN_NAME %in% c("MELB_URBAN", "MELBOURNE_CBD") ~ "Metro Melbourne",
      DEG_URBAN_NAME %in% c("LARGE_PROVINCIAL_CITIES", "SMALL_CITIES") ~ "Regional city",
      DEG_URBAN_NAME %in% c("TOWNS", "SMALL_TOWNS", "RURAL_VICTORIA") ~ "Rural/towns",
      TRUE ~ "Unknown"),
      levels = c("Metro Melbourne", "Regional city", "Rural/towns", "Unknown")),
    urban_detail = DEG_URBAN_NAME,
    lat = LATITUDE, lon = LONGITUDE
  )

# ---- Sanity checks ----------------------------------------------------------
stopifnot(!anyDuplicated(crash$accident_no))
stopifnot(nrow(crash) == nrow(acc))
stopifnot(!anyNA(crash[CLUSTER_FEATURES]))
stopifnot(all(!is.na(crash$dca_group)))

log_step("Crash-level dataset: ", nrow(crash), " crashes x ", ncol(crash), " columns")
for (f in CLUSTER_FEATURES) {
  cat(sprintf("  %-15s %s\n", f, paste(sprintf("%s=%.1f%%", levels(crash[[f]]),
      100 * prop.table(table(crash[[f]]))), collapse = ", ")))
}
saveRDS(crash, out_path("crash_level.rds"))
log_step("Saved ", out_path("crash_level.rds"))
