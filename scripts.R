
### preliminaries
library(tidyverse)
library(lubridate)
theme_set(theme_light())


### standardize column names
col_head <- c("Year", "Jan", "flag_Jan", "Feb", "flag_Feb", "Mar", "flag_Mar",
              "Apr", "flag_Apr", "May", "flag_May", "Jun", "flag_Jun", "Jul",
              "flag_Jul", "Aug", "flag_Aug", "Sep", "flag_Sep", "Oct",
              "flag_Oct", "Nov", "flag_Nov", "Dec", "flag_Dec", "Annual",
              "flag_Annual", "Winter", "flag_Winter", "Spring", "flag_Spring",
              "Summer", "flag_Summer", "Autumn", "flag_Autumn")


### read homogenized data and combine into single df
stn_file <- list.files("dat")
stn_file_ids <- gsub("mm|\\.txt", "", stn_file)
stn_file_df <- data.frame(stn_file, stnid = stn_file_ids, stringsAsFactors = FALSE)

stn_dat <- read_csv("Temperature_Stations.csv") %>% 
  setNames(gsub(" ", "_", tolower(names(.)))) %>% 
  left_join(stn_file_df, by = "stnid") %>% 
  select(prov, station = station_name, stnid, stn_file, beg_yr, end_yr)

ReadFn <- function(file) {
  dat <- read.csv(paste0("dat/", file), skip = 4, col.names = col_head, stringsAsFactors = FALSE)
  dat$stnid <- gsub("mm|\\.txt", "", file)
  return(dat)
}

clim_full_l <- lapply(stn_file, ReadFn)
clim_full <- do.call(rbind.data.frame, clim_full_l)

clim_out <- clim_full %>% 
  setNames(tolower(names(.))) %>%
  as_tibble() %>% 
  filter(year >= 1880) %>% 
  mutate(annual = ifelse(annual < -1000, NA, annual)) %>%  # NA code is -9999
  mutate(winter = ifelse(winter < -1000, NA, winter)) %>% 
  mutate(spring = ifelse(spring < -1000, NA, spring)) %>% 
  mutate(summer = ifelse(summer < -1000, NA, summer)) %>% 
  mutate(autumn = ifelse(autumn < -1000, NA, autumn)) %>% 
  select(stnid, year, annual, flag_annual, winter, flag_winter, spring, flag_spring,
         summer, flag_summer, autumn, flag_autumn) %>% 
  left_join(stn_dat, by = "stnid") %>% 
  select(-stnid, -stn_file, -beg_yr, -end_yr) %>% 
  select(station, prov, everything())

# write to file
write.csv(clim_out, "dat_clim.csv", row.names = FALSE)



### maximum range in mean annual temp over the period of interest
clim_out %>% 
  group_by(station, prov) %>% 
  summarize(z = diff(range(annual, na.rm = TRUE))) %>% 
  ungroup() %>% 
  arrange(desc(z)) %>% 
  slice(1:20) %>% 
  as.data.frame()


### examine linear vs nonlinear trends
clim_out %>% 
  filter(station == "WINNIPEG") %>% 
  ggplot(aes(year, annual)) +
  geom_line(col = "steelblue") +
  geom_smooth(method = 'lm', se = F, col = "black") +
  geom_smooth(method = 'loess', se = F, col = "darkgreen")


### get linear trends (slope and R^2)
GetTrend <- function(dat) {
  trend_annual <- coef(lm(annual ~ year, dat))[2] * 10
  trend_winter <- coef(lm(winter ~ year, dat))[2] * 10
  trend_spring <- coef(lm(spring ~ year, dat))[2] * 10
  trend_summer <- coef(lm(summer ~ year, dat))[2] * 10
  trend_autumn <- coef(lm(autumn ~ year, dat))[2] * 10
  r2_annual <- cor(dat$year, dat$annual, use = "complete.obs")^2
  r2_winter <- cor(dat$year, dat$winter, use = "complete.obs")^2
  r2_spring <- cor(dat$year, dat$spring, use = "complete.obs")^2
  r2_summer <- cor(dat$year, dat$summer, use = "complete.obs")^2
  r2_autumn <- cor(dat$year, dat$autumn, use = "complete.obs")^2
  return(data.frame(trend_annual, trend_winter, trend_spring, trend_summer,
                    trend_autumn, r2_annual, r2_winter, r2_spring, r2_summer,
                    r2_autumn))
}

clim_trends <- clim_out %>% 
  group_by(station, prov) %>% 
  do(GetTrend(.)) %>% 
  ungroup() %>% 
  arrange(prov)

# write all station-level data to file
stn_dat_out <- stn_dat %>% 
  left_join(clim_trends) %>% 
  select(-stn_file, -beg_yr, -end_yr, -stnid)

write.csv(stn_dat_out, "dat_station.csv", row.names = FALSE)



### get trend line
get_fit_line <- function(dat) {
  
  mod <- lm(annual ~ year, dat)
  xpred <- range(dat$year[!is.na(dat$annual)]) + c(-2, 2)
  annual <- as.numeric(predict(mod, newdata = data.frame(year = xpred)))
  
  mod <- lm(winter ~ year, dat)
  winter <- as.numeric(predict(mod, newdata = data.frame(year = xpred)))
  
  mod <- lm(spring ~ year, dat)
  spring <- as.numeric(predict(mod, newdata = data.frame(year = xpred)))
  
  mod <- lm(summer ~ year, dat)
  summer <- as.numeric(predict(mod, newdata = data.frame(year = xpred)))
  
  mod <- lm(autumn ~ year, dat)
  autumn <- as.numeric(predict(mod, newdata = data.frame(year = xpred)))
  
  return(data.frame(year = xpred, annual, winter, spring, summer, autumn))
}

fit_line_out <- clim_out %>% 
  group_by(station, prov) %>% 
  do(get_fit_line(.)) %>% 
  ungroup()

# write to file
write.csv(fit_line_out, "dat_fit.csv", row.names = FALSE)



### plot trend vs trend R^2
ggplot(stn_dat_out, aes(trend_annual, r2_annual)) +
  geom_point(size = 4, alpha = 0.12, pch = 21, fill = "black") +
  geom_point(data = filter(stn_dat_out, prov == "NL"), size = 4, fill = "steelblue", pch = 21, col = "black", alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.7) +
  coord_cartesian(xlim = c(-0.15, 0.79), ylim = c(0, 0.6)) +
  labs(x = "Temperature trend (°C per decade)", y = "Trend consistency (R²)") +
  theme(panel.grid = element_blank())



### plot histogram of trends by province
clim_full %>% 
  mutate(temp = ifelse(Winter < -1000, NA, Winter)) %>% 
  select(stnid, year = Year, temp) %>% 
  left_join(stn_dat, by = "stnid") %>% 
  as_tibble() %>% 
  group_by(station, prov) %>% 
  do(beta = coef(lm(temp ~ year, .))[2] * 10) %>% 
  unnest() %>% 
  arrange(prov) %>% 
  ggplot() +
  geom_histogram(aes(beta), bins = 30) +
  facet_wrap(~prov)

