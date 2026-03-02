# March 1, 2026 - data frame prep

# need to link the data frame from Mark with the TRF values with the dataframe from Sabrina for the adult carryover study 

pacman::p_load(dplyr, here, ggplot2, tidyr, lubridate)

adult_carryover_df1 <- read.csv(here("RawData", "adult_carryover_df.csv"))
adult_carryover_df2 <- read.csv(here("RawData", "adult_carryover2.csv"))

adult_carryover_df1 <- adult_carryover_df1 %>%
  dplyr::select(sample_id, TRF)

adult_carryover_df2 <- left_join(adult_carryover_df1, adult_carryover_df2, by = "sample_id")

write.csv(adult_carryover_df2, "IntermediateData/adult_carryover_df.csv", row.names = FALSE)

# need to link data frames to weather data

weather_df <- read.csv(here("RawData", "ithaca_airport_weather.csv")) # get weather data

## clean weather data
weather_df$date <- as.Date(weather_df$date, format = "%m/%d/%y")
weather_df$yday <- yday(weather_df$date)
weather_df$temp_C <- (weather_df$temp_F - 32) * 5/9
weather_df$year <- year(weather_df$date)
weather_df$yr_day_hr <- paste(weather_df$year, weather_df$yday, weather_df$hour, sep = "_")

## filter weather down only to years / days needed
weather_df <- weather_df %>%
  dplyr::select(date, hour, yday, temp_C, year, yr_day_hr)

## filter to only average daytime temp
weather_df <- weather_df %>% 
  filter(
    hour >= 6,
    hour <= 20,
    !is.na(temp_C)
  )

## get average and mean for capture day
weather_df <- weather_df %>%
  group_by(year, yday) %>%
  summarise(
    avgC_capture_day = mean(temp_C),
    maxC_capture_day = max(temp_C),
    .groups = "drop"
  )

## now get average max temp during incubation for each nest 

nest_df <- read.csv(here("RawData", "01-13-26_nest.csv")) # get nest data

incubation_df_max <- weather_df %>%
  dplyr::inner_join(nest_df, by = c("year" = "exp_year")) %>%
  dplyr::filter(dplyr::between(yday, clutch_comp_doy, hatch_doy)) %>%
  group_by(nest_key) %>%
  summarise(
    incubation_max_temp = mean(maxC_capture_day, na.rm = TRUE),
    .groups = "drop"
  )

nest_df <- nest_df %>%
  left_join(incubation_df_max, by = "nest_key")

# average incubation temp
incubation_df_avg <- weather_df %>%
  dplyr::inner_join(nest_df, by = c("year" = "exp_year")) %>%
  dplyr::filter(dplyr::between(yday, clutch_comp_doy, hatch_doy)) %>%
  group_by(nest_key) %>%
  summarise(
    incubation_avg_temp = mean(avgC_capture_day, na.rm = TRUE),
    .groups = "drop"
  )

nest_df <- nest_df %>%
  left_join(incubation_df_avg, by = "nest_key")

## get number of days before some threshold temp

threshold <- 18.5

nest_df <- nest_df %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    incubation_below_18.5 = sum(
      weather_df$maxC_capture_day[
        weather_df$year == exp_year &
          dplyr::between(weather_df$yday, clutch_comp_doy, hatch_doy)
      ] < threshold,
      na.rm = TRUE
    )
  ) %>%
  dplyr::ungroup()

nest_df <- nest_df %>% # keep only relevant nest variables
  dplyr::select(nest_key, incubation_max_temp, incubation_avg_temp, incubation_below_18.5)


## link weather of capture day to cold snap df based on date

coldsnap_df <- read.csv(here("RawData", "coldsnap_telomere_df.csv"))

coldsnap_df <- coldsnap_df %>% 
  dplyr::mutate(
    Capture.Date = as.Date(Capture.Date, format = "%d-%b-%y"), # fix date format
    yday = lubridate::yday(Capture.Date), # get day of year 
    year = lubridate::year(Capture.Date) # create variable for year
  )


coldsnap_df <- coldsnap_df %>%
  left_join(
    weather_df,
    by = c(
      "year" = "year",
      "yday" = "yday"
    )
  )

## link weather to nestling carryover df based on date

carryover_df <- read.csv(here("RawData", "carryover_df.csv"))

carryover_df <- carryover_df %>% 
  dplyr::mutate(
    Capture_Date = as.Date(Capture_Date, format = "%d-%b-%y"), # fix date format
    yday = lubridate::yday(Capture_Date), # get day of year 
    year = lubridate::year(Capture_Date) # create variable for year
    
  )


carryover_df <- carryover_df %>%
  left_join(
    weather_df,
    by = c(
      "year" = "year",
      "yday" = "yday"
    )
  )

## link incubation weather to encounter df

encounter_df <- read.csv(here("RawData", "01-13-26_encounters.csv")) # get encounter data

encounter_df <- encounter_df %>% # keep only relevant variables
  dplyr::select(nest_key, band, adult_or_nestling, exp_year, telomere_num)

nest_df <- nest_df %>%
  dplyr::left_join(encounter_df, by = "nest_key") # link encounter to nest 

## link incubation to coldsnap df

coldsnap_df <- coldsnap_df %>% # fix telomere variable
  dplyr::mutate(
    Telomere.ID = gsub(" ", "", Telomere.ID)
  )

nest_weather <- nest_df %>% # keep only weather variables to be joined
  dplyr::select(telomere_num, incubation_max_temp, incubation_avg_temp, incubation_below_18.5)

coldsnap_df <- coldsnap_df %>%
  dplyr::left_join(nest_weather, by = c("Telomere.ID" = "telomere_num"))

write.csv(coldsnap_df, "IntermediateData/3-2-26_coldsnap_df.csv", row.names = FALSE)


## link incubation to carryover df

carryover_df <- carryover_df %>%
  dplyr::left_join(nest_weather, by = c("Telomere_Sample_nb" = "telomere_num"))

write.csv(carryover_df, "IntermediateData/3-2-26_carryover_df.csv", row.names = FALSE)
