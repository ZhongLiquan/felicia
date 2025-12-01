# 0) Dependencies
library(dplyr)
library(tidyr)
library(sf)
library(INLA)
library(spdep)
# library(lubridate)  # can be used if you want lubridate::quarter

state_road_layer <- read.csv("state_road_layer.csv")

# -----------------------------
# 1) Create quarterly index from original monthly data
#    and aggregate to segment × quarter
# -----------------------------

crash_seg_summary_q <- state_road_layer %>%
  mutate(
    CTY_CODE    = as.numeric(CTY_CODE),         # ensure consistent type with roads_df
    CRASH_MONTH = as.integer(CRASH_MONTH),
    QUARTER     = ((CRASH_MONTH - 1L) %/% 3L) + 1L  # 1..4
  ) %>%
  group_by(ST_RT_NO, SEG_NO, CTY_CODE, CRASH_YEAR, QUARTER) %>%
  summarise(
    CRASH_COUNT = n()
    # CUR_AADT    = first(CUR_AADT),
    # LANE_CNT    = first(LANE_CNT),
    # TOTAL_WIDT  = first(TOTAL_WIDT),
    # URBAN_RURAL = first(URBAN_RURAL),
    # ACCESS_CTR  = first(ACCESS_CTR),
    # COUNTY      = first(COUNTY),
    # .groups = "drop"
  ) %>%
  mutate(
    SEG_KEY = paste(CTY_CODE, ST_RT_NO, SEG_NO, sep = "_")
  ) %>%
  distinct(CTY_CODE, ST_RT_NO, SEG_NO, SEG_KEY,
           CRASH_COUNT, CRASH_YEAR, QUARTER)

# Define min_year (for TIME_KEY_Q below)
min_year <- min(crash_seg_summary_q$CRASH_YEAR, na.rm = TRUE)

# -----------------------------
# 2) Read road geometry and prepare keys
# -----------------------------
roads <- st_read("RMSSEG_(State_Roads)/RMSSEG_(State_Roads).shp", quiet = TRUE)
roads_df <- roads %>%
  st_drop_geometry() %>%
  mutate(
    CTY_CODE = as.numeric(CTY_CODE),   # keep type consistent with crash data
    SEG_NO   = as.numeric(SEG_NO),
    SEG_KEY  = paste(CTY_CODE, ST_RT_NO, SEG_NO, sep = "_")
  )
# %>% distinct(CTY_CODE, ST_RT_NO, SEG_NO, SEG_KEY)

# -----------------------------
# 3) Build full segment × quarter grid and left-join crash counts
# -----------------------------
years    <- 2018:2023
quarters <- 1:4

time_grid_q <- expand_grid(CRASH_YEAR = years, QUARTER = quarters)

expanded_q <- roads_df %>%
  crossing(time_grid_q) %>%
  left_join(
    crash_seg_summary_q,
    by = c("SEG_KEY", "ST_RT_NO", "SEG_NO", "CTY_CODE", "CRASH_YEAR", "QUARTER")
  ) %>%
  mutate(
    CRASH_COUNT = ifelse(is.na(CRASH_COUNT), 0L, CRASH_COUNT),
    CUR_AADT    = ifelse(is.na(CUR_AADT), 0, CUR_AADT),
    TOTAL_WIDT  = ifelse(is.na(TOTAL_WIDT), NA, TOTAL_WIDT),
    LANE_CNT    = ifelse(is.na(LANE_CNT), NA, LANE_CNT),
    # NOTE: check column name URBAN_RURA vs URBAN_RURAL in your data
    URBAN_RURAL = ifelse(is.na(URBAN_RURA), NA, URBAN_RURA),
    ACCESS_CTR  = ifelse(is.na(ACCESS_CTR), NA, ACCESS_CTR),
    ROUGH_INDX  = ifelse(is.na(ROUGH_INDX), 0, ROUGH_INDX),
    SEGMENT_MI  = ifelse(is.na(SEGMENT_MI), 0, SEGMENT_MI),
    COUNTY      = ifelse(is.na(CTY_CODE), NA, CTY_CODE),
    TIME_KEY_Q  = (CRASH_YEAR - min_year) * 4L + QUARTER   # quarter index
  )

# Optional: spatio-temporal interaction index
# expanded_q <- expanded_q %>%
#   mutate(
#     st_inter = interaction(seg_idx, TIME_KEY_Q, drop = TRUE),
#     st_inter = as.integer(as.factor(st_inter))
#   )

# Quick checks
nrow(expanded_q)                         # should be n_segments × n_years × 4 = 113852*6*4=2732448
sum(expanded_q$CRASH_COUNT > 0)
sum(expanded_q$CRASH_COUNT == 0)

# Optional: save for later
# write.csv(expanded_q, "expanded_q.csv", row.names = FALSE)

# ============================
# 3) Read adjacency graph g (INLA graph)
#    If you already created roads.adj via nb2INLA("roads.adj", nb),
#    you can read it directly.
# ============================

lookup <- read.csv("lookup.csv")

adj <- read.csv("road_neighbors.csv")  # segment pairs

# 2. Build adjacency using SEG_KEY
adj <- adj %>%
  filter(
    !is.na(SEG_NO), !is.na(SEG_NO_2),
    !is.na(CTY_CODE), !is.na(CTY_CODE_2),
    !is.na(ST_RT_NO), !is.na(ST_RT_NO_2)
  ) %>%
  mutate(
    SEG_KEY   = paste(CTY_CODE,  ST_RT_NO,  SEG_NO,  sep = "_"),
    SEG_KEY_2 = paste(CTY_CODE_2, ST_RT_NO_2, SEG_NO_2, sep = "_")
  ) %>%
  distinct(SEG_KEY, SEG_KEY
           