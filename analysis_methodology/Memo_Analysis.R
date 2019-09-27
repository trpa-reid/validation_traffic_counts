## load libraries

library(pacman)
p_load(tidyverse, readxl, lubridate, purrr, janitor, scales, plotly, tmap, leaflet, DT,lubridate,sf,xfun, ggridges, ggmap, scales, formattable, goeveg)

## load dataset

continuous<-read_csv("H:/model/model_update_2019/validation/continuous.csv") %>%
  mutate(weekday=wday(Date, label=TRUE), month=month(Date, label=TRUE), Day1=as.character(Day))

## Count Station Map

register_google(key="AIzaSyA3lBYiTffOPFATH9Brv0eJ0IR6DgXK5tY")

map<-continuous %>% distinct(lat,lon, station) %>%
  st_as_sf(coords=c("lon", "lat"), crs=4326)

tahoe_map <- get_map(location = "Lake Tahoe, CA", zoom = 10)

ggmap(tahoe_map) + geom_sf(data=map, aes(fill=station), color="red", size=1.6, inherit.aes = FALSE) +
  theme(axis.text.x=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank(), legend.position = "none")

ggsave("H:/model/model_update_2019/validation/charts/map.png", height=5, width=4)

## Intro Stats

# percent of days and hours counted

n_distinct(continuous$Day1)/365

# July comparisons
july<-continuous %>% 
  filter(month=="Jul") %>%
  mutate(Day1=as.character(Day)) %>%
  group_by(Day1) %>% 
  summarise(Count=sum(Count, na.rm=T)) %>%
  mutate(median=median(Count))

mar<-continuous %>% 
  filter(month=="Mar") %>%
  mutate(Day1=as.character(Day)) %>%
  group_by(Day1) %>% 
  summarise(Count=sum(Count, na.rm=T)) %>%
  mutate(median=median(Count))

unique(july$median)/unique(mar$median)

jan<-continuous %>% 
  filter(month=="Jan") %>%
  mutate(Day1=as.character(Day)) %>%
  group_by(Day1) %>% 
  summarise(Count=sum(Count, na.rm=T)) %>%
  mutate(median=median(Count))

dec<-continuous %>% 
  filter(month=="Dec") %>%
  mutate(Day1=as.character(Day)) %>%
  group_by(Day1) %>% 
  summarise(Count=sum(Count, na.rm=T)) %>%
  mutate(median=median(Count))

1-unique(jan$median)/unique(july$median)
1-unique(dec$median)/unique(july$median)

## weekend comparison

fri_sat<-continuous %>% 
  filter(weekday %in% c("Fri","Sat")) %>%
  mutate(Day1=as.character(Day)) %>%
  group_by(Day1) %>% 
  summarise(Count=sum(Count, na.rm=T)) %>%
  mutate(median=median(Count))

all_others<-continuous %>% 
  filter(weekday %in% c("Sun","Mon","Tue","Wed","Thu")) %>%
  mutate(Day1=as.character(Day)) %>%
  group_by(Day1) %>% 
  summarise(Count=sum(Count, na.rm=T)) %>%
  mutate(median=median(Count))

1-unique(all_others$median)/unique(fri_sat$median)

## Median Daily Total Counts by Day of Week and Month

continuous %>% 
  group_by(Day,weekday, month) %>% 
  summarise(Count=sum(Count, na.rm=T)) %>%
  ungroup() %>%
  mutate(annual_median=median(c(Count))) %>%
  group_by(weekday, month, annual_median) %>% 
  summarise(day_month_median=median(Count)) %>%
  ggplot(aes(month, day_month_median, group=weekday, fill=weekday)) + geom_bar(position="dodge", stat="identity") + 
  scale_fill_manual(values=c("#08D6DA", "#06378D", "#BE0B05", "#F7A537", "#E2D103", "#35AA27", "#940CD8")) + 
  geom_hline(aes(yintercept=108579),color="black",size=1,linetype="dashed") + theme_minimal() +
  theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position="bottom", legend.title = element_blank()) +
  ggtitle("Median Daily Total Counts by Day of Week and Month") +
  geom_text(aes(2.8,annual_median,label = paste0("Annual Median: ",format(annual_median,big.mark=",")), vjust = -1.8)) +
  scale_y_continuous(labels = scales::comma)

ggsave("H:/model/model_update_2019/validation/charts/chart1.png", height=4, width=8)

## Summer Months Median Daily Counts by Day of Week

continuous %>% 
  filter(month %in% c("Jun","Aug","Jul","Sep")) %>%
  group_by(Day,weekday, month) %>% 
  summarise(Count=sum(Count, na.rm=T)) %>%
  ungroup() %>%
  mutate(median_day=median(c(Count)),
         color=case_when(month=="Jul" | weekday %in% c("Sun","Fri","Sat") ~ "exclude",
                         TRUE ~ as.character("include"))) %>%
  group_by(weekday, month, median_day, color) %>% 
  summarise(median_count=median(Count)) %>% 
  ggplot(aes(month, median_count, group=weekday, fill=weekday)) + geom_bar(position="dodge", stat="identity") + 
  scale_fill_manual(values=c("#08D6DA", "#06378D", "#BE0B05", "#F7A537", "#CCDA19", "#35AA27", "#940CD8")) + 
  geom_hline(aes(yintercept=median_day),color="black",size=1,linetype="dashed") + 
  theme_minimal() + theme(axis.title.y=element_blank(), axis.title.x=element_blank()) +
  ggtitle("Summer Months Median Daily Total Counts by Day of Week") +
  geom_text(aes(.9,median_day,label = paste0("median: ",format(median_day,big.mark=",")), vjust = -1)) +
  scale_y_continuous(label=scales::comma)

ggsave("H:/model/model_update_2019/validation/charts/chart2.png", height=4, width=8)

## June, August, & September Monday-Thursday - Median Daily Total Counts

continuous %>% 
  filter(month %in% c("Jun", "Jul","Aug","Sep") & weekday %in% c("Mon","Tue","Wed","Thu")) %>%
  group_by(Day,weekday, month) %>% 
  summarise(Count=sum(Count, na.rm=T)) %>%
  group_by(weekday, month) %>% 
  summarise(Count=median(Count)) %>% 
  ungroup() %>%
  mutate(median=median(Count)) %>% 
  ggplot(aes(month, Count, group=weekday, fill=weekday)) + geom_bar(position="dodge", stat="identity") + 
  scale_fill_manual(values=c("#06378D", "#BE0B05", "#F7A537", "#CCDA19")) + 
  geom_hline(aes(yintercept=median),color="black",size=1,linetype="dashed") + 
  geom_text(aes(.9,median,label = paste0("median: ",format(median,big.mark=",")), vjust = -1)) + 
  theme_minimal() + theme(axis.title.y=element_blank(), axis.title.x=element_blank()) +
  ggtitle("June, August, & September Monday-Thursday - Median Daily Total Counts") +
  scale_y_continuous(label=scales::comma)

ggsave("H:/model/model_update_2019/validation/charts/chart3.png", height=4, width=8)

## Mon,Tue, Wed Summarized
mon_tue_wed<-continuous %>% 
  mutate(Day1=as.character(Day)) %>%
  filter(Day1 %in% c("2018-06-04","2018-06-05","2018-06-06",
                     "2018-06-11","2018-06-12","2018-06-13",
                     "2018-08-29","2018-08-28","2018-08-27",
                     "2018-09-10","2018-09-11","2018-09-12",
                     "2018-09-17","2018-09-18","2018-09-19")) %>%
  group_by(Day1) %>% 
  summarise(Count=sum(Count, na.rm=T)) %>%
  mutate(median=median(Count))

## Mon,Tue, Wed, Thu Summarized

mon_tue_wed_thu<-continuous %>% 
  mutate(Day1=as.character(Day)) %>%
  filter(Day1 %in% c("2018-06-04","2018-06-05","2018-06-06","2018-06-07",
                     "2018-06-11","2018-06-12","2018-06-13","2018-06-14",
                     "2018-08-30","2018-08-29","2018-08-28","2018-08-27",
                     "2018-09-10","2018-09-11", "2018-09-12","2018-09-13",
                     "2018-09-17","2018-09-18","2018-09-19","2018-09-20")) %>%
  group_by(Day1) %>% 
  summarise(Count=sum(Count, na.rm=T)) %>%
  mutate(median=median(Count))

## Mon,Tue, Wed, Thu, Fri Summarized

mon_tue_wed_thu_fri<-continuous %>% 
  mutate(Day1=as.character(Day)) %>%
  filter(Day1 %in% c("2018-06-04","2018-06-05","2018-06-06","2018-06-07", "2018-06-08",
                     "2018-06-11","2018-06-12","2018-06-13","2018-06-14", "2018-06-15",
                     "2018-08-31","2018-08-29","2018-08-27","2018-08-30", "2018-08-28",
                     "2018-09-10","2018-09-11", "2018-09-12","2018-09-12", "2018-09-13",
                     "2018-09-17","2018-09-18","2018-09-19","2018-09-20","2018-09-21")) %>%
  group_by(Day1) %>% 
  summarise(Count=sum(Count, na.rm=T)) %>%
  mutate(median=median(Count))

## Fridays Summarized

fri<-continuous %>% 
  mutate(Day1=as.character(Day)) %>%
  filter(Day1 %in% c("2018-06-08",
                     "2018-06-15",
                     "2018-08-31",
                     "2018-09-13",
                     "2018-09-21")) %>%
  group_by(Day1) %>% 
  summarise(Count=sum(Count, na.rm=T)) %>%
  mutate(median=median(Count))

## Saturday & Sundays Summarized

sat_sun<-continuous %>% 
  mutate(Day1=as.character(Day)) %>%
  filter(Day1 %in% c("2018-06-09", "2018-06-10",
                     "2018-06-16", "2018-06-17",
                     "2018-09-01", "2018-09-02",
                     "2018-09-14", "2018-09-15",
                     "2018-09-22","2018-09-21")) %>%
  group_by(Day1) %>% 
  summarise(Count=sum(Count, na.rm=T)) %>%
  mutate(median=median(Count))

## all summer weekdays - june, july, aug, sep - Mon-thurs

sum_week<-continuous %>% 
  mutate(Day1=as.character(Day)) %>%
  filter(weekday %in% c("Mon","Tue","Wed","Thu") & month %in% c("Jun","Jul","Aug","Sep")) %>%
  group_by(Day1) %>% 
  summarise(Count=sum(Count, na.rm=T)) %>%
  mutate(median=median(Count))

## all summer days

summer<-continuous %>% 
  mutate(Day1=as.character(Day)) %>%
  filter(month %in% c("Jun","Jul","Aug","Sep")) %>%
  group_by(Day1) %>% 
  summarise(Count=sum(Count, na.rm=T)) %>%
  mutate(median=median(Count))

## All August Days

aug<-continuous %>% 
  mutate(Day1=as.character(Day)) %>%
  filter(month=="Aug") %>%
  group_by(Day1) %>% 
  summarise(Count=sum(Count, na.rm=T)) %>%
  mutate(median=median(Count))

## All Days 2018

year<-continuous %>% 
  mutate(Day1=as.character(Day)) %>%
  group_by(Day1) %>% 
  summarise(Count=sum(Count, na.rm=T)) %>%
  mutate(median=median(Count))

## Model Days Scenario Comparison

## summarize daily counts
continuous_day<-continuous %>%
  group_by(Day, weekday, month) %>% 
  summarise(Count=sum(Count, na.rm=T))

#define percentile function
percentile <- ecdf(continuous_day$Count)

scenario<-bind_rows(
  #mon tue wed scenario
  data.frame(
    "mean"=mean(mon_tue_wed$Count),
    "median"=median(mon_tue_wed$Count),
    "sd"=sd(mon_tue_wed$Count),
    "cv"=cv(mon_tue_wed$Count),
    "n"=nrow(mon_tue_wed),
    "percentile"=percentile(mean(mon_tue_wed$Count)),
    "ranked"=round(341-(percentile(mean(mon_tue_wed$Count)) * 341),0),
    "time period"="Mondays-Wednesdays"
  ),
  #mon tue wed thu scenario
  data.frame(
    "mean"=mean(mon_tue_wed_thu$Count),
    "median"=median(mon_tue_wed_thu$Count),
    "sd"=sd(mon_tue_wed_thu$Count),
    "cv"=cv(mon_tue_wed_thu$Count),
    "n"=nrow(mon_tue_wed_thu),
    "percentile"=percentile(mean(mon_tue_wed_thu$Count)),
    "ranked"=round(341-(percentile(mean(mon_tue_wed_thu$Count)) * 341),0),
    "time period" = "Mondays-Thursdays"
  ),
  #mon tue wed thu fri scenario
  data.frame(
    "mean"=mean(mon_tue_wed_thu_fri$Count),
    "median"=median(mon_tue_wed_thu_fri$Count),
    "sd"=sd(mon_tue_wed_thu_fri$Count),
    "cv"=cv(mon_tue_wed_thu_fri$Count),
    "n"=nrow(mon_tue_wed_thu_fri),
    "percentile"=percentile(mean(mon_tue_wed_thu_fri$Count)),
    "ranked"=round(341-(percentile(mean(mon_tue_wed_thu_fri$Count)) * 341),0),
    "time period" = "Mondays-Fridays"
  ),
  #fri scenario
  data.frame(
    "mean"=mean(fri$Count),
    "median"=median(fri$Count),
    "sd"=sd(fri$Count),
    "cv"=cv(fri$Count),
    "n"=nrow(fri),
    "percentile"=percentile(mean(fri$Count)),
    "ranked"=round(341-(percentile(mean(fri$Count)) * 341),0),
    "time period" = "Fridays"
  ),
  #sat and sun scenario
  data.frame(
    "mean"=mean(sat_sun$Count),
    "median"=median(sat_sun$Count),
    "sd"=sd(sat_sun$Count),
    "cv"=cv(sat_sun$Count),
    "n"=nrow(sat_sun),
    "percentile"=percentile(mean(sat_sun$Count)),
    "ranked"=round(341-(percentile(mean(sat_sun$Count)) * 341),0),
    "time period" = "Saturdays & Sundays"
  ),
  #august scenario
  data.frame(
    "mean"=mean(aug$Count),
    "median"=median(aug$Count),
    "sd"=sd(aug$Count),
    "cv"=cv(aug$Count),
    "n"=nrow(aug),
    "percentile"=percentile(mean(aug$Count)),
    "ranked"=round(341-(percentile(mean(aug$Count)) * 341),0),
    "time period" = "All Days in August"
  ),
  #all days annual scenario
  data.frame(
    "mean"=mean(year$Count),
    "median"=median(year$Count),
    "sd"=sd(year$Count),
    "cv"=cv(year$Count),
    "n"=nrow(year),
    "percentile"=percentile(mean(year$Count)),
    "ranked"=round(341-(percentile(mean(year$Count)) * 341),0),
    "time period" = "All Days in 2018"
  ),
  #all summer weekdays - june, july, aug, sep
  data.frame(
    "mean"=mean(sum_week$Count),
    "median"=median(sum_week$Count),
    "sd"=sd(sum_week$Count),
    "cv"=cv(sum_week$Count),
    "n"=nrow(sum_week),
    "percentile"=percentile(mean(sum_week$Count)),
    "ranked"=round(341-(percentile(mean(sum_week$Count)) * 341),0),
    "time period" = "All Summer Weekdays (June-Sep, Mon-Thurs)"
  ),
  #all summer days - june, july, aug, sep
  data.frame(
    "mean"=mean(summer$Count),
    "median"=median(summer$Count),
    "sd"=sd(summer$Count),
    "cv"=cv(summer$Count),
    "n"=nrow(summer),
    "percentile"=percentile(mean(summer$Count)),
    "ranked"=round(341-(percentile(mean(summer$Count)) * 341),0),
    "time period" = "All Summer Days (June-Sep)"
  )
) 

sign_formatter <- formatter("span", 
                            style = x ~ style(font.weight = "bold",color = ifelse(x == "Mondays-Thursdays", "red", "black")))

formattable(
  scenario %>% select(time.period, everything()) %>%
    mutate(Mean=format(round(mean), big.mark=","),
           Median=format(round(median), big.mark=","),
           `Standard Deviation`=format(round(sd), big.mark=","),
           Percentile=round(percentile,2),
           cv=round(cv,2)) %>%
    rename(`Number of Days`=n , `Day Ranked`=ranked, `Time Period`=time.period, `Coefficient of Variation`=cv) %>%
    select(`Time Period`, `Number of Days`, Mean, Median, `Standard Deviation`, `Coefficient of Variation`,Percentile, `Day Ranked`) %>%
    arrange(`Day Ranked`),
  list(`Time Period` = sign_formatter,
       `Percentile`= color_bar("#9CF0F9"))
)

## Modeled Days - Count Totals throughout the Summer

all<-continuous %>% 
  mutate(Day1=as.character(Day)) %>%
  filter(month %in% c("Jun","Jul","Aug","Sep")) %>%
  group_by(Day1) %>% 
  summarise(Count=sum(Count, na.rm=T)) %>%
  mutate(median=median(Count), 
         color=case_when(Day1 %in% c("2018-06-04","2018-06-05","2018-06-06","2018-06-07",
                                     "2018-06-11","2018-06-12","2018-06-13","2018-06-14",
                                     "2018-08-30","2018-08-29","2018-08-28","2018-08-27",
                                     "2018-09-10","2018-09-11", "2018-09-12","2018-09-13",
                                     "2018-09-17","2018-09-18","2018-09-19","2018-09-20") ~ "color",
                         TRUE ~ as.character("no_color")))

all %>% ggplot(aes(Day1, Count, fill=color)) + 
  geom_col() + 
  geom_hline(aes(yintercept=median),color="red",size=1,linetype="dashed") +
  geom_text(aes(30,median,label = paste0("summer daily total median: ",format(median,big.mark=",")), vjust = -1)) +
  theme_minimal() + 
  theme(axis.text.x=element_text(angle=45, hjust=1), axis.title.y=element_blank(), axis.title.x=element_blank(), legend.position = "none") +
  ggtitle("Modeled Days - Count Totals throughout the Summer") +
  scale_fill_manual(values=c("#08A498","#ABB0AF")) +
  scale_x_discrete(breaks= c("2018-06-04", "2018-06-11","2018-08-27","2018-09-10","2018-09-17","2018-07-01","2018-07-15","2018-08-01","2018-08-15"),
                   labels= c("2018-06-04", "2018-06-11","2018-08-27","2018-09-10","2018-09-17","2018-07-01","2018-07-15","2018-08-01","2018-08-15")) +
  scale_y_continuous(label=scales::comma)

ggsave("H:/model/model_update_2019/validation/charts/chart4.png", height=4, width=8)

## Model Days Recommendation Totals Only

mon_tue_wed_thu %>% ggplot(aes(Day1, Count)) + 
  geom_col(position="dodge") + 
  geom_hline(aes(yintercept=median),color="red",size=1,linetype="dashed") +
  geom_text(aes(2.2,median,label = paste0("median: ",format(round(median,0),big.mark=",")), vjust = -1)) +
  theme_minimal() + 
  theme(axis.text.x=element_text(angle=45, hjust=1), axis.title.y=element_blank(), axis.title.x=element_blank()) +
  ggtitle("Mon, Tues, Wed, Thurs Day Daily Counts - Early June, Late Aug, Mid Sept")

ggsave("H:/model/model_update_2019/validation/charts/chart5.png", height=4, width=8)
