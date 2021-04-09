weather <- read.csv("../data/weather.csv",na.strings = "null")

weather
#, colClasses = c("Date",rep("numeric", 3)

weather <-
weather %>%
  mutate(tempdate = str_extract(timestamp,".+(?=(\\+))"), 
         time = as.POSIXct(tempdate,"%Y-%m-%d %H:%M:%S", tz = "GMT"),
         date = as.Date(tempdate))

updated_weather <- 
weather[c(2, 3, 4, 6, 7)] %>%
  mutate(day = weekdays(date), hour = as.numeric(format(time, "%H")),
         workingHour = ifelse(day %in% c("Saturday","Sunday"), 1, 0),
         workDay = ifelse(((9 <= hour & hour <= 13)| (14 <= hour & hour <= 17)), 1, 0),
         binned_hours = format(time, "%Y-%m-%d %H"))

write.csv(updated_weather,"updated_weather.csv" )



#merge by the hour to get
hourlyWeather <-
updated_weather %>%
  group_by(binned_hours) %>%
  summarise(airtemp = mean(airtemp, na.rm = TRUE), rainfal = mean(rainfall, na.rm = TRUE), 
            humidity = mean(humidity, na.rm = TRUE), hour = mean(hour, na.rm = TRUE), 
            workingHour = mean(workingHour, na.rm = TRUE), workday = mean(workDay, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(date = as.Date(binned_hours, "%Y-%m-%d %H"), weekDay = weekdays(date))

write.csv(hourlyWeather, "hourlyWeather.csv")
sequenceBinTimings <- seq(earliest, latest, by="1 hour")



### formating test
(weather$timestamp)[[1]]
test <- str_extract("2018-01-01 00:05:00+08:00",".+(?=(\\+))")
test1 <- as.Date(test, format = "%Y-%m-%d %H:%M:%S")
as.POSIXct("2018-01-01 00:05:00","%Y-%m-%d %H:%M:%S")
####

earliest <- min(updated_weather$time)
latest <- max(updated_weather$time)

head(updated_weather)



########################################################################################
sr_rain <- read.csv("kimchuan.csv",na.strings = "null")

sr_rain <- sr_rain %>% mutate(tempdate = str_extract(timestamp,".+(?=(\\+))"), 
         time = as.POSIXct(tempdate,"%Y-%m-%d %H:%M:%S", tz = "GMT"),
         date = as.Date(tempdate))

updated_srRain <- sr_rain[,c(-1, -6)] %>%
  mutate(binned_hours = format(time, "%Y-%m-%d %H"))


updated_srRain <- updated_srRain%>%
  group_by(binned_hours) %>%
  summarise(airtemp = mean(airtemp, na.rm = TRUE), rainfall = mean(rainfall, na.rm = TRUE), 
            humidity = mean(humidity, na.rm = TRUE), .groups = "drop") %>%
  mutate(date = as.Date(binned_hours, "%Y-%m-%d %H"), weekDay = weekdays(date))
         
# day = weekdays(date), hour = as.numeric(format(time, "%H")),
# workingHour = ifelse(day %in% c("Saturday","Sunday"), 1, 0),
# workDay = ifelse(((9 <= hour & hour <= 13)| (14 <= hour & hour <= 17)), 1, 0),
# , hour = mean(hour, na.rm = TRUE), 
# workingHour = mean(workingHour, na.rm = TRUE), workday = mean(workDay, na.rm = TRUE


  #        , weekNum = format(date, "%V")) %>%
  # group_by(weekNum) %>%
  # mutate(weeklyTemp = mean(airtemp,na.rm=TRUE), weeklyHumid = mean(humidity,na.rm=TRUE), weeklyrainfall = mean(rainfall,na.rm=TRUE)) %>%
  # ungroup() %>%
  # mutate(airtemp = ifelse(is.na(airtemp), weeklyTemp, airtemp), rainfall = ifelse(is.na(rainfall), weeklyrainfall, rainfall),
  #        humidity = ifelse(is.na(humidity), weeklyHumid, humidity))




# read & combine all the df
carpark_df <- data.frame()
carparkFiles <- list.files()[str_detect(list.files(),"carpark")]
for(i in carparkFiles[c(2,1,3,4)]) {
  temp_df <- read.csv(i, na.strings = "null")
  if(i == "carpark_Apr-Jul.csv"){
    temp_df <- temp_df %>% mutate(date = str_extract(timestamp,".+(?=[:blank:])"), 
        time = as.POSIXct(timestamp, format = "%d/%m/%Y %H:%M"), date = as.Date(date,"%Y-%m-%d "),
        binned_hours = format(time, "%Y-%m-%d %H"))
  }
  else{
  temp_df <- temp_df %>% mutate(date = str_extract(timestamp,".+(?=[:blank:])"), time = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"),
                                date = as.Date(date,"%Y-%m-%d "), binned_hours = format(time, "%Y-%m-%d %H"))}
  carpark_df <- rbind(carpark_df, temp_df)
}




# names(carparkFiles) <- c("timestamp", "carpark_data", "")
#str(carpark_df)
# carpark_df <- carpark_df %>%
#   mutate(date = str_extract(timestamp,".+(?=[:blank:])"), time = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"),
#          date = as.Date(date,"%Y-%m-%d "), binned_hours = format(time, "%Y-%m-%d %H"))
  
  
# binned by hours  
updated_carparkDf <- carpark_df %>% group_by(binned_hours) %>%
  summarise(mean(L1), mean(SE18), mean(SE20), mean(SE24), mean(SE26), mean(SE28), mean(SE29), .groups = "drop") %>% 
  arrange(binned_hours)

names(updated_carparkDf) <- names(updated_carparkDf) %>% str_replace_all("[\\(\\)]","") %>%
  str_replace_all("mean", "mean_hourly")


########################################################################################
head(updated_srRain)

#transform column for public holiday
public_holiday_2018 <- read.csv("public_hoilday_2018.csv", col.names = "dates", colClasses = "Date")
pHDates <- public_holiday_2018 %>% pull()

## Total lots
total_lots <- c(178, 279, 548, 167, 214, 186, 333)


finaldf <- full_join(updated_srRain, updated_carparkDf, by = "binned_hours") %>%
  mutate(TotalL1 = 178, L1available = TotalL1 - mean_hourlyL1,
         TotalSE18 = 279, SE18available = TotalSE18 - mean_hourlySE18,
         TotalSE20 = 548, SE20available = TotalSE20 - mean_hourlySE20,
         TotalSE24 = 167, SE24available = TotalSE24 - mean_hourlySE24,
         TotalSE26 = 214, SE26available = TotalSE26 - mean_hourlySE26,
         TotalSE28 = 186, SE28available = TotalSE28 - mean_hourlySE28,
         TotalSE29 = 333, SE29available = TotalSE29 - mean_hourlySE29,
         date = as.Date(binned_hours, "%Y-%m-%d %H"), weekDay = weekdays(date),
         weekNum = format(date, "%V"), hour = unlist(str_extract_all(binned_hours, "(?<=[:blank:])[0-9]{2}")),
         workDay = ifelse(weekDay %in% c("Saturday","Sunday"), 0, 1),
         workingHour = ifelse(((9 <= hour & hour <= 13)| (14 <= hour & hour <= 17)), 1, 0),
         PH = ifelse(date %in% pHDates, 1, 0)
         ) 

finaldf <- finaldf %>% group_by(weekNum) %>%
  mutate(weeklyTemp = mean(airtemp,na.rm=TRUE), weeklyHumid = mean(humidity,na.rm=TRUE), weeklyrainfall = mean(rainfall,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(airtemp = ifelse(is.na(airtemp), weeklyTemp, airtemp), rainfall = ifelse(is.na(rainfall), weeklyrainfall, rainfall),
         humidity = ifelse(is.na(humidity), weeklyHumid, humidity))

df <- df %>% arrange(date)
df <- data.frame(finaldf[1:6], apply(finaldf[7:27], 2, as.integer), finaldf[28:35])
write.csv(df,"../data/carparkdfs/df_2018.csv", row.names = FALSE)


#str_extract_all(updated_carparkDf$binned_hours,"(?<-[:blank:][0-9]{2})")

# 
# finaldf %>% mutate(weekNum = format(date, "%V")) %>% select(weekNum) %>% filter(is.na(date))
# finaldf %>% filter(is.na(date))
# 


# write.csv(finaldf,"Finaldf.csv", row.names = FALSE)
# View(finaldf)
# table(finaldf$PH)


################################################################################################################################################################################################################################
### For 2019
######

sr_rain <- read.csv("../data/2019/kimchuan_2019.csv",na.strings = "null")

sr_rain <- sr_rain %>% mutate(tempdate = str_extract(timestamp,".+(?=(\\+))"), 
                              time = as.POSIXct(tempdate,"%Y-%m-%d %H:%M:%S", tz = "GMT"),
                              date = as.Date(tempdate))

updated_srRain <- sr_rain[,c(-1, -6)] %>%
  mutate(binned_hours = format(time, "%Y-%m-%d %H"))


updated_srRain <- updated_srRain%>%
  group_by(binned_hours) %>%
  summarise(airtemp = mean(airtemp, na.rm = TRUE), rainfall = mean(rainfall, na.rm = TRUE), 
            humidity = mean(humidity, na.rm = TRUE), .groups = "drop") %>%
  mutate(date = as.Date(binned_hours, "%Y-%m-%d %H"), weekDay = weekdays(date))




# read & combine all the df
carpark_df <- data.frame()
carparkFiles <- list.files("../data/2019")[str_detect(list.files("../data/2019"),"carpark")]
for(i in carparkFiles[c(2,1,3,4)]) {
  temp_df <- read.csv(paste("../data/2019/", i, sep = ""), na.strings = "null")
  print(head(temp_df))
  if(i == "carpark_2019_Apr_Jul.csv"){
    temp_df <- temp_df %>% mutate(date = str_extract(timestamp,".+(?=[:blank:])"), 
                                  time = as.POSIXct(timestamp, format = "%d/%m/%Y %H:%M"), date = as.Date(date,"%Y-%m-%d "),
                                  binned_hours = format(time, "%Y-%m-%d %H"))}
  else{
    temp_df <- temp_df %>% mutate(date = str_extract(timestamp,".+(?=[:blank:])"), time = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"),
                                  date = as.Date(date,"%Y-%m-%d "), binned_hours = format(time, "%Y-%m-%d %H"))}
  carpark_df <- rbind(carpark_df, temp_df)
}




# names(carparkFiles) <- c("timestamp", "carpark_data", "")
#str(carpark_df)
# carpark_df <- carpark_df %>%
#   mutate(date = str_extract(timestamp,".+(?=[:blank:])"), time = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"),
#          date = as.Date(date,"%Y-%m-%d "), binned_hours = format(time, "%Y-%m-%d %H"))


# binned by hours  
updated_carparkDf <- carpark_df %>% group_by(binned_hours) %>%
  summarise(mean(L1), mean(SE18), mean(SE20), mean(SE24), mean(SE26), mean(SE28), mean(SE29), .groups = "drop") %>% 
  arrange(binned_hours)

names(updated_carparkDf) <- names(updated_carparkDf) %>% str_replace_all("[\\(\\)]","") %>%
  str_replace_all("mean", "mean_hourly")


########################################################################################
head(updated_srRain)

#transform column for public holiday
public_holiday_2018 <- read.csv("public_hoilday_2018.csv", col.names = "dates", colClasses = "Date")
pHDates <- public_holiday_2018 %>% pull()

## Total lots
total_lots <- c(178, 279, 548, 167, 214, 186, 333)


finaldf <- full_join(updated_srRain, updated_carparkDf, by = "binned_hours") %>%
  mutate(TotalL1 = 178, L1available = TotalL1 - mean_hourlyL1,
         TotalSE18 = 279, SE18available = TotalSE18 - mean_hourlySE18,
         TotalSE20 = 548, SE20available = TotalSE20 - mean_hourlySE20,
         TotalSE24 = 167, SE24available = TotalSE24 - mean_hourlySE24,
         TotalSE26 = 214, SE26available = TotalSE26 - mean_hourlySE26,
         TotalSE28 = 186, SE28available = TotalSE28 - mean_hourlySE28,
         TotalSE29 = 333, SE29available = TotalSE29 - mean_hourlySE29,
         date = as.Date(binned_hours, "%Y-%m-%d %H"), weekDay = weekdays(date),
         weekNum = format(date, "%V"), hour = unlist(str_extract_all(binned_hours, "(?<=[:blank:])[0-9]{2}")),
         workDay = ifelse(weekDay %in% c("Saturday","Sunday"), 1, 0),
         workingHour = ifelse(((9 <= hour & hour <= 13)| (14 <= hour & hour <= 17)), 1, 0),
         PH = ifelse(date %in% pHDates, 1, 0)
  ) 

finaldf <- finaldf %>% group_by(weekNum) %>%
  mutate(weeklyTemp = mean(airtemp,na.rm=TRUE), weeklyHumid = mean(humidity,na.rm=TRUE), weeklyrainfall = mean(rainfall,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(airtemp = ifelse(is.na(airtemp), weeklyTemp, airtemp), rainfall = ifelse(is.na(rainfall), weeklyrainfall, rainfall),
         humidity = ifelse(is.na(humidity), weeklyHumid, humidity))

df <- df %>% arrange(date)
df <- data.frame(finaldf[1:6], apply(finaldf[7:27], 2, as.integer), finaldf[28:35])
write.csv(df,"../data/carparkdfs/df_2019.csv", row.names = FALSE)


#str_extract_all(updated_carparkDf$binned_hours,"(?<-[:blank:][0-9]{2})")

# 
# finaldf %>% mutate(weekNum = format(date, "%V")) %>% select(weekNum) %>% filter(is.na(date))
# finaldf %>% filter(is.na(date))
# 


write.csv(finaldf,"../data/carparkdfs/df_2019.csv", row.names = FALSE)
View(finaldf)
table(finaldf$PH)

