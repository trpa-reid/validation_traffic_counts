library(pacman)
p_load(tidyverse, readxl, lubridate, purrr, janitor, scales, plotly, tmap, leaflet, DT,lubridate,sf,xfun, ggridges, ggmap)

## Caltrans PEMS ## 

setwd("H:/model/model_update_2019/validation/CalTrans_Pems_2018_hourly")
file.list <- list.files(pattern='*.xlsx')
file.list <- setNames(file.list, file.list) # only needed when you need an id-column with the file-names
pems <- map_df(file.list, read_excel, .id = "id") %>%
  mutate(Hour=as.character(as.POSIXct(Hour, format="%m/%d/%Y %H:%M"))) %>%
  mutate(station=sub("(^[^-]+)-.*", "\\1", id)) %>%
  rename(Date=Hour, Count=`Lane 1 Flow (Veh/Hour)`, station_code=id) %>%
  #filter(`% Observed` !=0) %>%
select(Date, station, Count, station_code,`% Observed`)
  
### NDOT ###

ndot<- function (filename){
 test99<-read_xls(paste0("H:/model/model_update_2019/validation/NDOT_2018_counts_hourly/all/",filename), sheet="DV02P", skip=8, col_names = F) %>%
   select(1,2,4,12,15,21,23,30,32,39,41,48,50,58,61)
  row1<-lead(test99[1,],1)
  test99[1,] <- row1
 test99 <-test99 %>% remove_empty("rows")%>% 
   remove_empty("cols")
 my.names <- test99[1,]
colnames(test99) <- my.names
test99 <- test99 %>%
  slice(-1) 
names(test99)[1]<-"time"
test99[test99 == 99999] <- NA
test99 <- test99 %>% remove_empty("rows") %>% slice(1:24) %>%
  pivot_longer(c(2:8))
}
setwd("H:/model/model_update_2019/validation/NDOT_2018_counts_hourly/all")
file.list <- list.files(pattern='*.xls')
file.list <- setNames(file.list, file.list) # only needed when you need an id-column with the file-names
ndot1 <- map_df(file.list, possibly(ndot, otherwise = tibble(x="error reading")), .id = "id")

ndot_clean <- ndot1 %>%
  mutate(Date=as.character(as.POSIXct(paste(sub(" ","",substring(name, 6)), time), format="%m/%d/%Y %H:%M")),
         `% Observed`=100,
         station_code=substr(id,26,31),
         value=as.numeric(value),
         station=case_when(station_code== "005211" ~ "US 50 & Lake Parkway",
                           station_code== "005315" ~ "Lower Kingsbury (SR 207)",
                           station_code== "025212" ~ "US 50 & Carson St (Spooner Summit)",
                           station_code== "031224" ~ "SR 28 & Lakeshore Dr (Incline)"),
         value=na_if(value, "99999")) %>%
  rename(Count=value) %>%
  select(Date, station, Count, station_code,`% Observed`) %>%
  filter(station_code != "005211")

## combine ##
continuous_prep<-bind_rows(ndot_clean, pems) %>%
  #complete(station,Date) %>%
  mutate(Date=as.POSIXct(Date, format="%Y-%m-%d %H:%M:%S"),weekday=wday(Date, label=TRUE), Day=date(Date), Day1=as.character(Day), month=month(Date, label=TRUE), hour=hour(Date),
         wday_wknd = case_when(weekday %in% c("Mon","Tue","Wed","Thu") ~ "Weekday",
                              weekday %in% c("Fri","Sat","Sun") ~ "Weekend"),
         ext_int=case_when(station %in% c("Echo_Summit","US 50 & Carson St (Spooner Summit)","Brockway_Summit") ~ "External Station",
                           TRUE ~ as.character("Internal Station"))) %>%
    filter(Day >= "2018-01-01" & Day <= "2018-12-31") %>%
  select(station,Date, Day, weekday, hour, month, Count, wday_wknd, ext_int, station_code,`% Observed`,Day1) %>%
left_join(
data.frame(
  station=c("Lower Kingsbury (SR 207)", #"US 50 & Lake Parkway",
            "US 50 & Carson St (Spooner Summit)","SR 28 & Lakeshore Dr (Incline)","Bigler","Brockway_Summit","Echo_Summit","F_Street","Midway","Sawmill"),
lat=c(38.967500,#38.965484,
      39.121111,39.249357,38.935389,39.260764,38.815358,38.904321,38.952279,38.875694), 
lon=c(-119.923004,#-119.937834,
      -119.824151,-119.985101,-119.977477,-120.071504,-120.027960,-119.999118,-119.949293,-120.005384)
), by="station")
  
# identify Days with NAs
paste("'",
 continuous_prep %>% filter(is.na(Count)) %>% count(station, Day1) %>% distinct(Day1) %>% pull(1),
 "'",collapse=", ",sep="")

# identify days with zero percent observed
paste("'",
 continuous_prep %>% filter(`% Observed` == 0) %>% count(station, Day1) %>% distinct(Day1) %>% pull(1),
 "'",collapse=", ",sep="")

# remove days with any stations that had zero percent observed or na counts
continuous_final<-continuous_prep %>%
  filter(!Day1 %in% c('2018-03-07', '2018-03-11', '2018-03-12', '2018-04-06', '2018-05-01', '2018-05-02', '2018-06-18', '2018-08-06', '2018-08-08', '2018-11-04', '2018-11-05', '2018-12-09', '2018-12-20', '2018-12-21', '2018-12-22', '2018-01-10', '2018-04-01', '2018-04-02', '2018-12-27', '2018-12-28', '2018-12-29', '2018-02-24', '2018-03-06', '2018-03-23', '2018-04-10', '2018-05-24', '2018-05-25', '2018-06-29', '2018-08-25', '2018-09-04', '2018-09-05', '2018-11-03', '2018-11-11', '2018-11-16', '2018-11-28', '2018-03-02', '2018-03-10', '2018-03-11', '2018-03-12', '2018-03-17', '2018-03-27', '2018-03-28', '2018-05-17', '2018-03-22', '2018-04-19', '2018-04-20', '2018-10-01', '2018-10-02', '2018-10-04', '2018-10-11', '2018-11-27', '2018-01-16', '2018-01-17', '2018-03-29', '2018-03-30')) %>%
  filter(!is.na(Count)) %>%
  filter(`% Observed` != 0)

write.csv(continuous_final,"H:/model/model_update_2019/validation/continuous.csv")

