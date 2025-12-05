setwd("~/Downloads/Liquan/ANLY599/Code")
#-----Source data-------#
#Pennsylvania Department of Transportation (PennDOT). (2024). Pennsylvania State Roads (RMSSEG) [Data set]. PennShare Open Data Portal. Retrieved from: https://data-pennshare.opendata.arcgis.com/datasets/d9a2a5df74cf4726980e5e276d51fe8d_0
#-----Source data Obtain-------#
#Obtain crash data
crash_2018<- read.csv("Statewide_2018/CRASH_2018.csv")
crash_2019<- read.csv('Statewide_2019/CRASH_2019.csv')
crash_2020<- read.csv('Statewide_2020/CRASH_2020.csv')
crash_2021<- read.csv('Statewide_2021/CRASH_2021.csv')
crash_2022<- read.csv('Statewide_2022/CRASH_2022.csv')
crash_2023<- read.csv('Statewide_2023/CRASH_2023.csv')
crash_2024<- read.csv('Statewide_2024/CRASH_2024.csv')
crash_2018_s <- dplyr::select(crash_2018, CRN, COUNTY, CRASH_MONTH, CRASH_YEAR, DAY_OF_WEEK, DEC_LATITUDE, DEC_LONGITUDE, HOUR_OF_DAY, INTERSECT_TYPE, MAX_SEVERITY_LEVEL, ILLUMINATION,  RELATION_TO_ROAD, URBAN_RURAL, WEATHER1, WEATHER2)
crash_2019_s <- dplyr::select(crash_2019, CRN, COUNTY, CRASH_MONTH, CRASH_YEAR, DAY_OF_WEEK, DEC_LATITUDE, DEC_LONGITUDE, HOUR_OF_DAY, INTERSECT_TYPE, MAX_SEVERITY_LEVEL, ILLUMINATION,  RELATION_TO_ROAD, URBAN_RURAL, WEATHER1, WEATHER2)
crash_2020_s <- dplyr::select(crash_2020, CRN, COUNTY, CRASH_MONTH, CRASH_YEAR, DAY_OF_WEEK, DEC_LATITUDE, DEC_LONGITUDE, HOUR_OF_DAY, INTERSECT_TYPE, MAX_SEVERITY_LEVEL, ILLUMINATION, RELATION_TO_ROAD, URBAN_RURAL, WEATHER1, WEATHER2)
crash_2021_s <- dplyr::select(crash_2021, CRN, COUNTY, CRASH_MONTH, CRASH_YEAR, DAY_OF_WEEK, DEC_LATITUDE, DEC_LONGITUDE, HOUR_OF_DAY, INTERSECT_TYPE, MAX_SEVERITY_LEVEL, ILLUMINATION,  RELATION_TO_ROAD, URBAN_RURAL, WEATHER1, WEATHER2)
crash_2022_s <- dplyr::select(crash_2022, CRN, COUNTY, CRASH_MONTH, CRASH_YEAR, DAY_OF_WEEK, DEC_LATITUDE, DEC_LONGITUDE, HOUR_OF_DAY, INTERSECT_TYPE, MAX_SEVERITY_LEVEL, ILLUMINATION, RELATION_TO_ROAD, URBAN_RURAL, WEATHER1, WEATHER2)
crash_2023_s <- dplyr::select(crash_2023, CRN, COUNTY, CRASH_MONTH, CRASH_YEAR, DAY_OF_WEEK, DEC_LAT, DEC_LONG, HOUR_OF_DAY, INTERSECT_TYPE, MAX_SEVERITY_LEVEL, ILLUMINATION, RELATION_TO_ROAD, URBAN_RURAL, WEATHER1, WEATHER2)
crash_2023_renamed <- rename(crash_2023_s, DEC_LATITUDE = DEC_LAT)
crash_2023_renamed <- rename(crash_2023_renamed , DEC_LONGITUDE = DEC_LONG)
crash_combined <- rbind(crash_2018_s,crash_2019_s, crash_2020_s, crash_2021_s, crash_2022_s, crash_2023_renamed )
#Obtain deer related flags
flag_2018<- read.csv( 'Statewide_2018/FLAGS_2018.csv')
flag_2019<- read.csv('Statewide_2019/FLAGS_2019.csv')
flag_2020<- read.csv('Statewide_2020/FLAGS_2020.csv')
flag_2021<- read.csv( 'Statewide_2021/FLAGS_2021.csv')
flag_2022<- read.csv('Statewide_2022/FLAGS_2022.csv')
flag_2023<- read.csv('Statewide_2023/FLAGS_2023.csv')
flag_2024<- read.csv('Statewide_2024/FLAGS_2024.csv')
flag_2018_s <- dplyr::select(flag_2018,CRN, DEER_RELATED, SPEEDING)
flag_2019_s <- dplyr::select(flag_2019,CRN, DEER_RELATED, SPEEDING)
flag_2020_s <- dplyr::select(flag_2020,CRN, DEER_RELATED, SPEEDING)
flag_2021_s <- dplyr::select(flag_2021,CRN, DEER_RELATED, SPEEDING)
flag_2022_s <- dplyr::select(flag_2022,CRN, DEER_RELATED, SPEEDING)
flag_2023_s <- dplyr::select(flag_2023,CRN, DEER_RELATED, SPEEDING)
flag_combined <- rbind(flag_2018_s, flag_2019_s, flag_2020_s,flag_2021_s, flag_2022_s, flag_2023_s)
#Combine crash with flag via CRN
crash1<-
  crash_combined %>%
  left_join(flag_combined, by = "CRN") 
sum(duplicated(crash1[, c("CRN")]))
#Select deer related crash
crash2<-crash1[crash1$DEER_RELATED==1,]

#############
#Use QGIS to link crash location with state road segments to obtain state_road_layer.csv
#############
# 0) Dependencies
library(dplyr)
library(tidyr)
library(sf)
library(INLA)
library(spdep)

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
    URBAN_RURAL = ifelse(is.na(URBAN_RURA), NA, URBAN_RURA),
    ACCESS_CTR  = ifelse(is.na(ACCESS_CTR), NA, ACCESS_CTR),
    ROUGH_INDX  = ifelse(is.na(ROUGH_INDX), 0, ROUGH_INDX),
    SEGMENT_MI  = ifelse(is.na(SEGMENT_MI), 0, SEGMENT_MI),
    COUNTY      = ifelse(is.na(CTY_CODE), NA, CTY_CODE),
    TIME_KEY_Q  = (CRASH_YEAR - min_year) * 4L + QUARTER   # quarter index
  )

# -----------------------------
# Build prediction model
# Compute center/scale from your TRAINING PERIOD (e.g., 2018–2023) using raw columns
# -----------------------------
#1
scales <- expanded_q %>%
  summarise(
    mu_widt = mean(TOTAL_WIDT,   na.rm = TRUE),
    sd_widt = sd(TOTAL_WIDT,     na.rm = TRUE),
    mu_lane = mean(LANE_CNT,     na.rm = TRUE),
    sd_lane = sd(LANE_CNT,       na.rm = TRUE),
    mu_roug = mean(ROUGH_INDX,     na.rm = TRUE),
    sd_roug = sd(ROUGH_INDX,       na.rm = TRUE),
    mu_len = mean(SEGMENT_MI,     na.rm = TRUE),
    sd_len = sd(SEGMENT_MI,       na.rm = TRUE),
    mu_ur   = mean(as.numeric(URBAN_RURAL), na.rm = TRUE),
    sd_ur   = sd(as.numeric(URBAN_RURAL),   na.rm = TRUE)
  )

scale_with <- function(x, mu, sd) (x - mu) / ifelse(sd > 0, sd, 1)

# Rebuild df_q with manual scaling (no loss of attributes)
df_q <- expanded_q %>%
  left_join(lookup, by = c("SEG_KEY" = "old_id")) %>%
  rename(seg_idx = new_id) %>%
  filter(!is.na(seg_idx)) %>%
  mutate(
    log_AADT = log(CUR_AADT + 1),
    TOTAL_WIDT_s  = scale_with(TOTAL_WIDT,   scales$mu_widt, scales$sd_widt),
    LANE_CNT_s    = scale_with(LANE_CNT,     scales$mu_lane, scales$sd_lane),
    ROUGH_INDX_s  = scale_with(ROUGH_INDX,   scales$mu_roug, scales$sd_roug),
    SEGMENT_MI_s    = scale_with(SEGMENT_MI,     scales$mu_len, scales$sd_len),
    URBAN_RURAL_s = scale_with(as.numeric(URBAN_RURA), scales$mu_ur, scales$sd_ur)
  )
#add interaction (not used infinal dataset)
df_q <- df_q %>%
  mutate(
    st_inter = interaction(seg_idx, TIME_KEY_Q, drop = TRUE),
    st_inter = as.integer(as.factor(st_inter))
  )
df_q_small <- df_q %>%
  dplyr::select(CTY_CODE, ST_RT_NO, SEG_NO, SEG_KEY, CRASH_COUNT, CRASH_YEAR, QUARTER,
                log_AADT, TOTAL_WIDT_s, LANE_CNT_s, URBAN_RURAL_s, ROUGH_INDX_s, SEGMENT_MI_s, seg_idx, TIME_KEY_Q, st_inter)

#2
# Base per-segment table (one row per segment)
base_2024 <- roads_df %>%
  mutate(SEG_KEY = as.character(SEG_KEY)) %>%
  left_join(lookup, by = c("SEG_KEY" = "old_id")) %>%
  rename(seg_idx = new_id) %>%
  filter(!is.na(seg_idx)) %>%
  mutate(
    log_AADT = log(CUR_AADT + 1),
    TOTAL_WIDT_s  = scale_with(TOTAL_WIDT,   scales$mu_widt, scales$sd_widt),
    LANE_CNT_s    = scale_with(LANE_CNT,     scales$mu_lane, scales$sd_lane),
    ROUGH_INDX_s  = scale_with(ROUGH_INDX,   scales$mu_roug, scales$sd_roug),
    SEGMENT_MI_s    = scale_with(SEGMENT_MI,     scales$mu_len, scales$sd_len),
    URBAN_RURAL_s = scale_with(as.numeric(URBAN_RURA), scales$mu_ur, scales$sd_ur)
  )

# Continue quarterly time index
last_time <- max(df_q_small$TIME_KEY_Q, na.rm = TRUE)

df_2024 <- tidyr::crossing(
  base_2024,
  QUARTER = 1:4
) %>%
  group_by(QUARTER) %>%
  mutate(TIME_KEY_Q = last_time + QUARTER) %>%
  ungroup() %>%
  mutate(CRASH_YEAR = 2024L,
         CRASH_COUNT = NA_integer_) %>%
  dplyr::select(CTY_CODE, ST_RT_NO, SEG_NO, CRASH_COUNT, log_AADT, TOTAL_WIDT_s, 
                LANE_CNT_s, URBAN_RURAL_s,ROUGH_INDX_s, SEGMENT_MI_s, seg_idx, TIME_KEY_Q, SEG_KEY, CRASH_YEAR, QUARTER)
df_2024 <- df_2024 %>%
  mutate(
    st_inter = interaction(seg_idx, TIME_KEY_Q, drop = TRUE),
    st_inter = as.integer(as.factor(st_inter))
  )


#3
formula_q2 <- CRASH_COUNT ~  
  1 +
  TOTAL_WIDT_s +
  LANE_CNT_s +
  URBAN_RURAL_s +
  f(QUARTER, model="seasonal", season.length=4) +
  f(seg_idx, model="bym2", graph=g, scale.model=TRUE,
    hyper=list(
      prec=list(prior="pc.prec", param=c(1,0.01)),
      phi =list(prior="pc", param=c(0.5,2/3))
    )) +
  f(TIME_KEY_Q, model="rw1",
    hyper=list(prec=list(prior="pc.prec", param=c(1,0.01)))) +
  offset(log_AADT)


df_pred_all <- bind_rows(df_q_small, df_2024)

res_pred <- inla(
  formula_q2,
  family = "nbinomial",#"zeroinflatednbinomial0""poisson",
  data   = df_pred_all,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.inla = list(
    strategy     = "simplified.laplace",  # more stable
    int.strategy = "ccd",                 # avoids crashes
    diagonal     = 1e-3,                  # softens precision matrix
    restart      = TRUE
  )
)

summary(res_pred)

#Check zero inflated 
#lambda_hat <- res_pred$summary.fitted.values$mean
#k <- res_pred$summary.hyperpar["size for the nbinomial observations (1/overdispersion)", "mean"]
#p_zero_nb <- (k / (k + lambda_hat))^k
#mean(p_zero_nb)
#rownames(res_pred$summary.hyperpar)

# Pull out the 2024 part
n_train <- nrow(df_q_small)
pick    <- (n_train + 1):nrow(df_pred_all)

#state managed state segement number*quater numer=113852*4=455408
df_2024_pred <- df_pred_all[pick, ] %>%
  mutate(
    pred_mean = res_pred$summary.fitted.values$mean[pick],
    pred_lo   = res_pred$summary.fitted.values$`0.025quant`[pick],
    pred_hi   = res_pred$summary.fitted.values$`0.975quant`[pick]
  )
#write.csv(df_2024_pred, "df_2024_pred.csv", row.names = FALSE)
#df_2024_pred <- read.csv("df_2024_pred.csv")

#Evaluate the model goodness of fitness
res_pred$waic$waic
res_pred$cpo$cpo
res_pred$dic$dic       # DIC value
res_pred$dic$p.eff     # effective number of parameters (pD)
res_pred$dic$mean.deviance

# -----------------------------
#Obtain 2024 deer-related crash dataset
# -----------------------------
#crash_2024<- read.csv('Statewide_2024/CRASH_2024.csv')
#crash_2024_s <- dplyr::select(crash_2024, CRN, COUNTY, CRASH_MONTH, CRASH_YEAR)
#flag_2024<- read.csv('Statewide_2024/FLAGS_2024.csv')
#flag_2024_s <- dplyr::select(flag_2024,CRN, DEER_RELATED, SPEEDING)
#crash<-
#  crash_2024 %>%
#  left_join(flag_2024 , by = "CRN") 
#sum(duplicated(crash[, c("CRN")]))
#crash2024<-crash[crash$DEER_RELATED==1,]
#output csv file
#write.csv(crash2024, "crash_test.csv", row.names = FALSE)
#Link 2024 crash point with state road segment to obtain state_road_layer2024.csv
#Combine crash with each segment
state_road_layer2024 <- read.csv("state_road_layer2024.csv")
#Summarize 
crash_seg_summary_q2024 <- state_road_layer2024 %>%
  mutate(
    CTY_CODE   = as.numeric(CTY_CODE),                  # ← keep consistent with roads_df 
    CRASH_MONTH = as.integer(CRASH_MONTH),
    QUARTER     = ((CRASH_MONTH - 1L) %/% 3L) + 1L      # 1..4
    
  ) %>%
  group_by(ST_RT_NO, SEG_NO, CTY_CODE, CRASH_YEAR, QUARTER) %>%
  summarise(
    CRASH_COUNT = n(),
    .groups = "drop"
  ) %>%
  mutate(
    SEG_KEY = paste(CTY_CODE, ST_RT_NO, SEG_NO, sep = "_")
  )%>%
  distinct(CTY_CODE, ST_RT_NO, SEG_NO, SEG_KEY,CRASH_COUNT, CRASH_YEAR, QUARTER)  

# -----------------------------
#join 2024 prediction data with real data
# -----------------------------
join <- left_join(df_2024_pred, crash_seg_summary_q2024, by = c("SEG_KEY", "CTY_CODE","ST_RT_NO","SEG_NO","CRASH_YEAR","QUARTER"))
#impute crash count=0 if no crash happen in a segment per quarter
join$CRASH_COUNT.y <- ifelse(is.na(join$CRASH_COUNT.y), 0, join$CRASH_COUNT.y)
mean(join$CRASH_COUNT.y == 0)
#Add 0 to segno and ctycode to make it consistent with map data
join$SEG_NO <- str_pad(join$SEG_NO, width = 4, pad = "0")
join$CTY_CODE <- str_pad(join$CTY_CODE, width = 2, pad = "0")
#separate quarter data
join_q1 <- join %>% 
  mutate(SEG_KEY = paste(CTY_CODE, ST_RT_NO, SEG_NO, sep = "_"))%>% 
  filter(join$QUARTER==1)
join_q2 <- join %>% 
  mutate(SEG_KEY = paste(CTY_CODE, ST_RT_NO, SEG_NO, sep = "_"))%>% 
  filter(join$QUARTER==2)
join_q3 <- join %>% 
  mutate(SEG_KEY = paste(CTY_CODE, ST_RT_NO, SEG_NO, sep = "_"))%>% 
  filter(join$QUARTER==3)
join_q4 <- join %>% 
  mutate(SEG_KEY = paste(CTY_CODE, ST_RT_NO, SEG_NO, sep = "_"))%>% 
  filter(join$QUARTER==4)
# -----------------------------
#Combine with road map to plot crash on map
# -----------------------------
roads <- st_read("RMSSEG_(State_Roads)/RMSSEG_(State_Roads).shp", quiet = TRUE)   

#library(readxl)
#library(writexl)
road_q1<-left_join(roads, join_q1, by = c("CTY_CODE","ST_RT_NO","SEG_NO"))
#write.csv(road_q1, "road_join2024q1_nb.xlsx", row.names = FALSE)
road_q2<-left_join(roads, join_q2, by = c("CTY_CODE","ST_RT_NO","SEG_NO"))
#write.csv(road_q2, "road_join2024q2_nb.csv", row.names = FALSE)
road_q3<-left_join(roads, join_q3, by = c("CTY_CODE","ST_RT_NO","SEG_NO"))
#write.csv(road_q3, "road_join2024q3_nb.csv", row.names = FALSE)
road_q4<-left_join(roads, join_q4, by = c("CTY_CODE","ST_RT_NO","SEG_NO"))
#write.csv(road_q4, "road_join2024q4_nb.csv", row.names = FALSE)
#write.csv(join, "pred_join.csv", row.names = FALSE)

# -----------------------------
# Predition performance
# -----------------------------
pred <- road_q4%>%
  filter(!is.na(CRASH_COUNT.y))
#predother <- pred%>%
#  filter(SEGMENT_MI>0.1)
# Root Mean Square Error
rmse_val <- rmse(pred$CRASH_COUNT.y, pred$pred_mean)

# Mean Absolute Error
mae_val <- mae(pred$CRASH_COUNT.y, pred$pred_mean)

# R-squared (coefficient of determination)
r2_val <- cor(pred$CRASH_COUNT.y, pred$pred_mean)^2

# Pearson correlation
corr_val <- cor(pred$CRASH_COUNT.y, pred$pred_mean)

# Mean Bias/MAD
bias_val <- mean(pred$pred_mean - pred$CRASH_COUNT.y)

#MSPE (Mean Squared Prediction Error)
MSPE <- mean((pred$CRASH_COUNT.y - pred$pred_mean)^2)


data.frame(quarter='Q4',
           RMSE = rmse_val,
           MAE = mae_val,
           R2 = r2_val,
           Pearson_r = corr_val,
           Mean_Bias = bias_val,
           MSPE=MSPE)

dat_eval2 <- road_q4 %>%
  mutate(
    risk_class = cut(
      pred_mean,
      breaks = c(0, 0.2, 0.4, 0.6, 0.9, Inf),
      labels = c("0–0.2", "0.2–0.4", "0.4–0.6","0.6–0.9", "0.9+"),
      right = TRUE
    ),
    has_crash = CRASH_COUNT.y > 0
  )

# Risk Class
tab_risk <- dat_eval2 %>%
  group_by(risk_class) %>%
  summarise(
    n_segments      = n(),
    mean_pred       = mean(pred_mean),
    mean_obs        = mean(CRASH_COUNT.y),
    prop_with_crash = mean(has_crash),
    total_obs       = sum(CRASH_COUNT.y)
  )
tab_risk

dat_eval2 <- dat_eval2 %>%
  arrange(desc(pred_mean)) %>%
  mutate(
    rank = row_number(),
    cum_seg_prop = rank / n(),
    cum_crash_prop = cumsum(CRASH_COUNT.y) / sum(CRASH_COUNT.y)
  )

           
