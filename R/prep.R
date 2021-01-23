
### requires -------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
theme_set(theme_light())


## most recent data directory --------------------------------------------------
dir_data <- max(list.dirs("dat-raw", recursive = FALSE))
dir_data_temp <- file.path(dir_data, "Homog_monthly_mean_temp")


## prepare station data --------------------------------------------------------
df_stations <- readxl::read_excel(
  file.path(dir_data, "Temperature_Stations.xls"),
  skip = 3,
  .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
) %>% 
  janitor::clean_names() %>% 
  select(prov, station = nom_de_station, stn_id, beg_yr = de, end_yr = a) %>% 
  mutate(
    prov = recode(
      prov,
      "ALTA" = "AB",
      "SASK" = "SK",
      "MAN" = "MB",
      "ONT" = "ON",
      "QUE" = "QC",
      "NFLD" = "NL",
      "PEI" = "PE",
      "NWT" = "NT"
    ),
    # clean up station names
    station = stringr::str_to_title(gsub("_+", " ", station)),
    station = stringr::str_squish(station),
    station = gsub(" Cda", " CDA", station),
    station = gsub(" Ahrc", " AHRC", station),
    station = gsub(" Rcs", " RCS", station),
    station = gsub(" Rc", " RC", station),
    station = gsub(" Cs", " CS", station),
    station = gsub(" D ", " d'", station),
    station = gsub(" S ", "'s ", station),
    station = gsub("Mca", "McA", station),
    station = gsub("Mcb", "McB", station),
    station = gsub("Mcc", "McC", station),
    station = gsub("Mci", "McI", station),
    station = gsub("Mcp", "McP", station),
    station = gsub("Mcm", "McM", station),
    station = gsub("Qu'a", "Qu'A", station),
    # correction to 1 non-matching stn_id
    stn_id = recode(stn_id, "6110" = "611E001")
  )


## read all station-specific files and combine into single df ------------------
# standardize column names and types
col_head <- c(
  "year", "jan", "flag_jan", "feb", "flag_feb", "mar", "flag_mar", 
  "apr", "flag_apr", "may", "flag_may", "jun", "flag_jun", "jul", 
  "flag_jul", "aug", "flag_aug", "sep", "flag_sep", "oct", "flag_oct", 
  "nov", "flag_nov", "dec", "flag_dec", "annual", "flag_annual", 
  "winter", "flag_winter", "spring", "flag_spring", "summer", "flag_summer", 
  "autumn", "flag_autumn"
)

col_spec <- readr::cols(
  .default = col_double(),
  flag_jan = col_character(),
  flag_feb = col_character(),
  flag_mar = col_character(),
  flag_apr = col_character(),
  flag_may = col_character(),
  flag_jun = col_character(),
  flag_jul = col_character(),
  flag_aug = col_character(),
  flag_sep = col_character(),
  flag_oct = col_character(),
  flag_nov = col_character(),
  flag_dec = col_character(),
  flag_annual = col_character(),
  flag_winter = col_character(),
  flag_spring = col_character(),
  flag_summer = col_character(),
  flag_autumn = col_character()
)

# read and combine files
read_temperature_files <- function(file, col_head, col_spec) {
  readr::read_csv(
    file,
    skip = 4,
    col_names = col_head,
    col_types = col_spec
  ) %>% 
    tibble::as_tibble() %>% 
    mutate(stn_id = gsub("mm|\\.txt", "", basename(file)))
}

df_temps_raw <- list.files(dir_data_temp, pattern = "^m.*\\.txt", full.names = TRUE) %>% 
  purrr::map_dfr(., read_temperature_files, col_head = col_head, col_spec = col_spec)

# check for non-matching stn_id
df_temps_raw %>%
  anti_join(df_stations, by = "stn_id")

df_stations %>%
  anti_join(df_temps_raw, by = "stn_id")


## format temperature data for output ------------------------------------------
df_temps <- df_temps_raw %>% 
  filter(year >= 1880) %>% 
  mutate(annual = ifelse(annual < -1000, NA, annual)) %>%  # NA code is -9999
  select(stn_id, year, annual) %>% 
  left_join(df_stations, by = "stn_id") %>% 
  select(-stn_id, -beg_yr, -end_yr) %>% 
  select(station, prov, everything()) %>% 
  # filer to stations with >= 20 years of data
  group_by(station, prov) %>% 
  mutate(n = sum(!is.na(annual))) %>% 
  ungroup() %>% 
  filter(n >= 20) %>% 
  select(-n)

# write to file
readr::write_csv(df_temps, "dat/dat_clim.csv")


## check maximum range in mean annual temp over the period of interest ---------
df_temps %>% 
  group_by(station, prov) %>% 
  summarize(rng = diff(range(annual, na.rm = TRUE)), .groups = "drop") %>% 
  arrange(desc(rng))


## get linear trend point estimates --------------------------------------------
df_trends <- df_temps %>% 
  dplyr::nest_by(prov, station) %>% 
  dplyr::summarize(
    trend_annual = round(coef(lm(annual ~ year, data = data))[2] * 10, 5),
    .groups = "drop"
  )

# write all station-level data to file
df_stations_out <- df_stations %>% 
  inner_join(df_trends, by = c("prov", "station")) %>% 
  select(-beg_yr, -end_yr, -stn_id)

readr::write_csv(df_stations_out, "dat/dat_station.csv")


## get trend line --------------------------------------------------------------
get_fit_line <- function(dat) {
  mod <- lm(annual ~ year, dat)
  xpred <- range(dat$year[!is.na(dat$annual)]) + c(-1, 1)
  annual <- as.numeric(predict(mod, newdata = data.frame(year = xpred)))
  return(data.frame(year = xpred, annual))
}

df_fit <- df_temps %>% 
  group_by(station, prov) %>% 
  do(get_fit_line(.)) %>% 
  ungroup() %>% 
  mutate(annual = round(annual, 4))

# write to file
readr::write_csv(df_fit, "dat/dat_fit.csv")


## plot histogram of trends by province ----------------------------------------
df_trends %>% 
  ggplot() +
  geom_histogram(aes(trend_annual), bins = 30) +
  facet_wrap(~prov)

