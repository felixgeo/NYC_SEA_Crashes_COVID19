install.packages("RSocrata")
install.packages("lessR")
install.packages("rnoaa")
library(plyr)
library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)
library('aws.s3')
library("RSocrata")
library(plm)
library(lfe)
library(lmtest)
library(car)
library("RSocrata")
options(scipen=999)
options(max.print=9999999)
#spatial
library(rgdal)
library(sp)
library(GISTools)
library("lessR")
library("knitr")
library(arcpullr)
library(foreign)
library(pls)
library(lubridate)
library(tmap)
library(spatstat)


rm(list = ls(all.names = TRUE))

#####loading crash data from New York#######################
token <- "xxxxxxxxxx"
NYC_crashes<-read.socrata("https://data.cityofnewyork.us/resource/h9gi-nx95.json", app_token=token)
token <- "iZN2YnZxqgEWzlGsctEzaw6Mz"
NYC_crash.person<-read.socrata("https://data.cityofnewyork.us/resource/f55k-p6yu.json", app_token=token)
#NYC_crash.2019<-subset(NYC_crashes, crash_date>="2019-01-01" & crash_date<"2020-01-01")
NYC_crash.covid<-subset(NYC_crashes, crash_date>="2019-01-01" & crash_date<"2021-01-01")
NYC_crash.covid.2019<-subset(NYC_crash.covid, crash_date<"2020-01-01")
NYC_crash.covid.2020<-subset(NYC_crash.covid, crash_date>="2020-01-01")

NYC_crash.person.covid<-subset(NYC_crash.person, crash_date>="2019-01-01" & crash_date<"2021-01-01")

################vehicle counts###############################

token <- "xxxxxxxxxxxxxxxxxxxxxxxxxx"

NYC_crashes.veh<-read.socrata("https://data.cityofnewyork.us/resource/bm4k-52h4.json", app_token=token)

NYC_crash.veh.covid<-subset(NYC_crashes.veh, crash_date>="2019-01-01" & crash_date<"2021-01-01")

plyr::count(NYC_crash.covid$contributing_factor_vehicle_1)
plyr::count(NYC_crash.person.covid$person_injury)

#########identify single vehicle crashes####################

NYC_crashes.veh.join<-NYC_crash.veh.covid%>%
  group_by(collision_id)%>%
  summarize(count=n(), crash_date=mean(crash_date))

NYC_crash.covid.veh<-left_join(NYC_crash.covid, NYC_crashes.veh.join, by="collision_id")
NYC_crash.covid.single<-subset(NYC_crash.covid.veh, count==1)
NYC_crash.covid.single$week<-floor_date(NYC_crash.covid.single$crash_date.x, "week")
NYC_crash.covid.single$year<-year(NYC_crash.covid.single$week)

NYC_crash.covid.single.wk<-NYC_crash.covid.single%>%
  group_by(week)%>%
  summarize(count=n(), tinjuries=sum(as.numeric(number_of_persons_injured)), 
            finjuries=sum(as.numeric(number_of_persons_killed)), yr=mean(year))

NYC_crash.covid.single.wk<-subset(NYC_crash.covid.single.wk, yr>2018)
NYC_crash.covid.single.wk$month<-month(NYC_crash.covid.single.wk$week)

t.test(count~yr, data=subset(NYC_crash.covid.single.wk, month<3))
t.test(count~yr, data=subset(NYC_crash.covid.single.wk, month>2&month<6))
t.test(count~yr, data=subset(NYC_crash.covid.single.wk, month>5))

t.test(tinjuries~yr, data=subset(NYC_crash.covid.single.wk, month<3))
t.test(tinjuries~yr, data=subset(NYC_crash.covid.single.wk, month>2&month<6))
t.test(tinjuries~yr, data=subset(NYC_crash.covid.single.wk, month>5))

t.test(finjuries~yr, data=subset(NYC_crash.covid.single.wk, month<3))
t.test(finjuries~yr, data=subset(NYC_crash.covid.single.wk, month>2&month<6))
t.test(finjuries~yr, data=subset(NYC_crash.covid.single.wk, month>5))

write.csv(NYC_crash.covid.single.wk, file="NYC_crash_single.csv")
#########crashes summary####################################

NYC_crash.covid$week<-floor_date(NYC_crash.covid$crash_date, "week")
NYC_crash.covid$year<-year(NYC_crash.covid$week)

NYC_crash.covid.wk<-NYC_crash.covid%>%
  group_by(week)%>%
  summarize(count=n(), tinjuries=sum(as.numeric(number_of_persons_injured)), 
            finjuries=sum(as.numeric(number_of_persons_killed)), yr=mean(year))

NYC_crash.covid.week<-subset(NYC_crash.covid.wk, yr>2018)
NYC_crash.covid.week$month<-month(NYC_crash.covid.week$week)

t.test(count~yr, data=subset(NYC_crash.covid.week, month<3))
t.test(count~yr, data=subset(NYC_crash.covid.week, month>2&month<6))
t.test(count~yr, data=subset(NYC_crash.covid.week, month>5))

t.test(tinjuries~yr, data=subset(NYC_crash.covid.week, month<3))
t.test(tinjuries~yr, data=subset(NYC_crash.covid.week, month>2&month<6))
t.test(tinjuries~yr, data=subset(NYC_crash.covid.week, month>5))

t.test(finjuries~yr, data=subset(NYC_crash.covid.week, month<3))
t.test(finjuries~yr, data=subset(NYC_crash.covid.week, month>2&month<6))
t.test(finjuries~yr, data=subset(NYC_crash.covid.week, month>5))

write.csv()

###########NYC_crash.covid.week speeding related death############################################################

NYC_fatal.covid.june<-subset(NYC_crash.covid.2020, crash_date>="2020-06-01" & number_of_persons_killed>0)
plyr::count(NYC_fatal.covid.june$contributing_factor_vehicle_1)
plyr::count(NYC_fatal.covid.june$contributing_factor_vehicle_2)
plyr::count(NYC_fatal.covid.june$contributing_factor_vehicle_3)
plyr::count(NYC_fatal.covid.june$contributing_factor_vehicle_4)


##ped and cyclists in New York City########################################################
NYC_crash.pedcycle.covid<-subset(NYC_crash.person.covid, person_type=="Bicyclist" | person_type=="Pedestrian")
plyr::count(NYC_crash.person.covid$person_type)
plyr::count(NYC_crash.pedcycle.covid$person_type)
plyr::count(NYC_crash.pedcycle.covid$person_type)

NYC_crash.pedcycle.covid.cid<-NYC_crash.pedcycle.covid%>%
  group_by(collision_id)%>%
  summarize(count=n())

NYC_crash.pedcycle<-left_join(NYC_crash.covid, NYC_crash.pedcycle.covid.cid, by="collision_id")
NYC_crash.covid.pedcycle<-subset(NYC_crash.pedcycle, count!="NA")

NYC_crash.covid.pedcycle$week<-floor_date(NYC_crash.covid.pedcycle$crash_date, "week")

NYC_crash.covid.pedcycle$year<-year(NYC_crash.covid.pedcycle$week)

NYC_crash.covid.pedcycle.wk<-NYC_crash.covid.pedcycle%>%
  group_by(week)%>%
  summarize(num=n(), person=sum(count), tinjuries=sum(as.numeric(number_of_persons_injured)), 
            finjuries=sum(as.numeric(number_of_persons_killed)), yr=mean(year))
NYC_crash.covid.pedcycle.week<-subset(NYC_crash.covid.pedcycle.wk, yr>2018)
NYC_crash.covid.pedcycle.week$month<-month(NYC_crash.covid.pedcycle.week$week)


###################testing the mean ##########################################
t.test(num~yr, data=subset(NYC_crash.covid.pedcycle.week, month<3))
t.test(num~yr, data=subset(NYC_crash.covid.pedcycle.week, month>2&month<6))
t.test(num~yr, data=subset(NYC_crash.covid.pedcycle.week, month>5))
t.test(tinjuries~yr, data=subset(NYC_crash.covid.pedcycle.week, month<3))
t.test(tinjuries~yr, data=subset(NYC_crash.covid.pedcycle.week, month>2&month<6))
t.test(tinjuries~yr, data=subset(NYC_crash.covid.pedcycle.week, month>5))

t.test(finjuries~yr, data=subset(NYC_crash.covid.pedcycle.week, month<3))
t.test(finjuries~yr, data=subset(NYC_crash.covid.pedcycle.week, month>2&month<6))
t.test(finjuries~yr, data=subset(NYC_crash.covid.pedcycle.week, month>5))

#################speeding###################################################################
NYC_crash.covid.speeding<-subset(NYC_crash.covid, contributing_factor_vehicle_1=="Unsafe Speed"
                                 |contributing_factor_vehicle_2=="Unsafe Speed"
                                 |contributing_factor_vehicle_3=="Unsafe Speed"
                                 |contributing_factor_vehicle_4=="Unsafe Speed"
                                 |contributing_factor_vehicle_5=="Unsafe Speed")

NYC_crash.covid.speeding$week<-floor_date(NYC_crash.covid.speeding$crash_date, "week")
NYC_crash.covid.speeding$year<-year(NYC_crash.covid.speeding$week)

NYC_crash.covid.speeding.wk<-NYC_crash.covid.speeding%>%
  group_by(week)%>%
  summarize(count=n(), tinjuries=sum(as.numeric(number_of_persons_injured)), 
            finjuries=sum(as.numeric(number_of_persons_killed)), yr=mean(year))

NYC_crash.covid.speeding.wk<-subset(NYC_crash.covid.speeding.wk, yr>2018)
NYC_crash.covid.speeding.wk$month<-month(NYC_crash.covid.speeding.wk$week)

t.test(count~yr, data=subset(NYC_crash.covid.speeding.wk, month<3))
t.test(count~yr, data=subset(NYC_crash.covid.speeding.wk, month>2&month<6))
t.test(count~yr, data=subset(NYC_crash.covid.speeding.wk, month>5))

t.test(tinjuries~yr, data=subset(NYC_crash.covid.speeding.wk, month<3))
t.test(tinjuries~yr, data=subset(NYC_crash.covid.speeding.wk, month>2&month<6))
t.test(tinjuries~yr, data=subset(NYC_crash.covid.speeding.wk, month>5))

t.test(finjuries~yr, data=subset(NYC_crash.covid.week, month<3))
t.test(finjuries~yr, data=subset(NYC_crash.covid.week, month>2&month<6))

###########################################################export for regression###########################

NYC_crash.covid.all.wk<-left_join(NYC_crash.covid.week, NYC_crash.covid.pedcycle.week, "week")
NYC_crash.covid.all.wk<-left_join(NYC_crash.covid.all.wk, NYC_crash.covid.speeding.wk, "week")
write.csv(NYC_crash.covid.all.wk, file="NYC_crash.covid.all.csv")

##############kde analysis of spatial distribution#################################################
NYC.speeding.kde.covid<-subset(NYC_crash.covid.speeding, crash_date>="2020-03-01" & crash_date<="2020-12-31")
NYC.speeding.kde.2019<-subset(NYC_crash.covid.speeding, crash_date>="2019-03-01" & crash_date<="2019-12-31")
NYC.speeding.kde<-subset(NYC_crash.covid.speeding, crash_date>="2019-03-01" & crash_date<="2019-12-31" 
                         | crash_date>="2019-03-01" & crash_date<="2019-12-31")

###read collision data####
library(sf)
NYC.speeding.kde.covid$lat<-as.numeric(NYC.speeding.kde.covid$location.latitude)
NYC.speeding.kde.covid$long<-as.numeric(NYC.speeding.kde.covid$location.longitude)

NYC.speeding.covid.sf<-st_as_sf(subset(NYC.speeding.kde.covid, lat>0 | long>0), coords=c("long","lat"),crs=4326)
#st_crs(NYC.speeding.covid.sf)<-4326

tm_shape(NYC.speeding.covid.sf) + tm_dots(size=  .3, col = "red")
NYC.speeding.covid.sf.map<-subset(NYC.speeding.covid.sf, select=c("longitude","latitude"))
st_write(NYC.speeding.covid.sf.map, "nyc_crash_covid3.shp", driver="ESRI Shapefile")  # create to a shapefile 
tm_shape(NYC.speeding.covid.sf.map) + tm_dots(size=  .3, col = "red")

NYC.speeding.kde.2019$lat<-as.numeric(NYC.speeding.kde.2019$location.latitude)
NYC.speeding.kde.2019$long<-as.numeric(NYC.speeding.kde.2019$location.longitude)
NYC.speeding.2019.sf<-st_as_sf(subset(NYC.speeding.kde.2019, lat>0 | long>0), coords=c("long","lat"),crs=4326)
tm_shape(NYC.speeding.2019.sf) + tm_dots(size=  .3, col = "red")

#st_crs(sea_collision_sf)
#st_crs(sea_collision_sf)<-seacrs
#sea_city<-st_read(file.choose())
#st_crs(sea_city)

#plot(sea_collision_sf)
nyc<-st_read("NYC.shp")
tm_shape(nyc) + tm_polygons(col="grey", border.col="white") + 
  tm_shape(NYC.speeding.2019.sf) + tm_dots(size=  .3, col = "red") 
#seacrs<-st_crs(sea_city) 
#sea_collision_sf.proj<-st_transform(sea_collision_sf, seacrs=crs)
st_crs(nyc)
#sea_city.proj<-st_transform(sea_city, 32148)
#sea_collision_sf.proj<-st_set_crs(sea_collision_sf, 32148)
NYC.speeding.2019.proj<-st_transform(NYC.speeding.2019.sf, 2263)

tm_shape(nyc) + tm_polygons(col="grey", border.col="white") + 
  tm_shape(NYC.speeding.2019.proj) + tm_dots(size=  .3, col = "red") 


##############kde analysis of spatial distribution#################################################
nyc_collision.kde.2019.ppp<-as.ppp(NYC.speeding.2019.proj)

marks(nyc_collision.kde.2019.ppp) <- NULL
nyc.2019.collision.kde<-rescale(nyc_collision.kde.2019.ppp, 1000)

nyc_city_bg<-as.owin(nyc)
nyc_city_bg<-rescale(nyc_city_bg, 1000)

Window(nyc.2019.collision.kde) <- nyc_city_bg

plot(nyc.2019.collision.kde, main=NULL, cols=rgb(0,0,0,.2), pch=20)

k1.2019<-density(nyc.2019.collision.kde, sigma=1)
plot(k1.2019, main=NULL, las=1)

k2.2019<-density(nyc.2019.collision.kde, sigma=50)
plot(k2.covid, main=NULL, las=1)

##############kde in the period of lockdown########################
NYC.speeding.covid.proj<-st_transform(NYC.speeding.covid.sf, 2263)

NYC.speeding.covid.kde.ppp<-as.ppp(NYC.speeding.covid.proj)

tm_shape(nyc) + tm_polygons(col="grey", border.col="white")+
  tm_shape(NYC.speeding.covid.proj) + tm_dots(size=  .3, col = "red")

nyc.kde.covid.ppp<-as.ppp(NYC.speeding.covid.proj)

marks(nyc.kde.covid.ppp) <- NULL
nyc.kde.covid.ppp<-rescale(nyc.kde.covid.ppp, 1000)

Window(nyc.kde.covid.ppp) <- nyc_city_bg

plot(nyc.kde.covid.ppp, main=NULL, cols=rgb(0,0,0,.2), pch=20)

k1.covid<-density(nyc.kde.covid.ppp, sigma=1)
plot(k1.covid, main=NULL, las=1)

par(mfrow=c(1,2))
plot(k1.2019, main=NULL, las=1)
plot(k1.covid, main=NULL, las=1)


####################################################################

sea_collision.speeding$covid<-"N"
sea_collision.speeding$covid[sea_collision.speeding$date>="2020-03-01" & sea_collision.speeding$date<="2020-12-31"]<-"Y"
plyr::count(sea_collision.speeding$covid)

sea_collision.speeding.kde<-st_as_sf(subset(sea_collision.speeding, lat>0 | long>0), coords = c("lat","long" ), crs=4326)
plot(sea_collision.speeding.kde)
sea_collision.speeding.kde<-st_transform(sea_collision.speeding.kde, 32148)


sea_collision.speeding.ppp<-as.ppp(sea_collision.speeding.kde)
marks(sea_collision.speeding.ppp)<-factor(sea_collision.speeding.kde$covid)
plot(split(sea_collision.speeding.ppp))

sea_collision.speeding.ppp<-rescale(sea_collision.speeding.ppp, 1000)
k<-Kcross.inhom(sea_collision.speeding.ppp, correction="best")
plyr::count(sea_collision.speeding.ppp$marks)
plot(k, . - pi * r^2 ~ r)

e = envelope(sea_collision.speeding.ppp, Kcross.inhom, i = "Y", j = "N", simulate = expression(rlabel(sea_collision.speeding.ppp)), 
             verbose = FALSE)
plot(e, . - mmean ~ r, legend=FALSE)
plot(e, . - mmean ~ r)



###################speeding patterns############for injury patterns#########################################
NYC_crash.covid.speeding<-subset(NYC_crash.covid.injury, contributing_factor_vehicle_1=="Unsafe Speed"
                                 |contributing_factor_vehicle_2=="Unsafe Speed"
                                 |contributing_factor_vehicle_3=="Unsafe Speed"
                                 |contributing_factor_vehicle_4=="Unsafe Speed"
                                 |contributing_factor_vehicle_5=="Unsafe Speed")

NYC_crash.covid.injury.week<-subset(NYC_crash.covid.injury.week, yr>2018)
##################processing the injury data######################################
NYC_crash.covid$number_of_pedestrians_injured<-as.numeric(NYC_crash.covid$number_of_pedestrians_injured)
NYC_crash.covid$number_of_pedestrians_killed<-as.numeric(NYC_crash.covid$number_of_pedestrians_killed)
NYC_crash.covid$number_of_persons_injured<-as.numeric(NYC_crash.covid$number_of_persons_injured)
NYC_crash.covid$number_of_persons_killed<-as.numeric(NYC_crash.covid$number_of_persons_killed)
NYC_crash.covid$number_of_cyclist_injured<-as.numeric(NYC_crash.covid$number_of_cyclist_injured)
NYC_crash.covid$number_of_cyclist_killed<-as.numeric(NYC_crash.covid$number_of_cyclist_killed)
NYC_crash.covid$number_of_motorist_injured<-as.numeric(NYC_crash.covid$number_of_motorist_injured)
NYC_crash.covid$number_of_motorist_killed<-as.numeric(NYC_crash.covid$number_of_motorist_killed)

##########only consider injury crashes in NYC
NYC_crash.covid.injury<-subset(NYC_crash.covid, number_of_persons_killed>=1 | number_of_persons_injured>=1)
plyr::count(NYC_crash.covid.injury$number_of_persons_killed)

NYC_crash.covid.injury.date<-NYC_crash.covid.injury%>%
  group_by(crash_date)%>%
  summarize(count=n(), tinjuries=sum(as.numeric(number_of_persons_injured)), finjuries=sum(as.numeric(number_of_persons_killed)), yr=mean(year))


NYC_crash.covid.injury.date$year<-year(NYC_crash.covid.injury.date$crash_date)
NYC_crash.covid.injury$week<-floor_date(NYC_crash.covid.injury$crash_date, "week")
NYC_crash.covid.injury$year<-year(NYC_crash.covid.injury$week)

NYC_crash.covid.injury.week<-NYC_crash.covid.injury%>%
  group_by(week)%>%
  summarize(count=n(), tinjuries=sum(as.numeric(number_of_persons_injured)), 
            finjuries=sum(as.numeric(number_of_persons_killed)), yr=mean(year))
NYC_crash.covid.injury.week<-subset(NYC_crash.covid.injury.week, yr>2018)

t.test(tinjuries~yr, data=NYC_crash.covid.injury.week)
t.test(finjuries~yr, data=NYC_crash.covid.injury.week)


NYC_crash.covid.injury.week$month<-month(NYC_crash.covid.injury.week$week)

t.test(tinjuries~yr, data=subset(NYC_crash.covid.injury.week, month<3))
t.test(tinjuries~yr, data=subset(NYC_crash.covid.injury.week, month>2&month<6))
t.test(tinjuries~yr, data=subset(NYC_crash.covid.injury.week, month>5))

t.test(finjuries~yr, data=subset(NYC_crash.covid.injury.week, month<3))
t.test(finjuries~yr, data=subset(NYC_crash.covid.injury.week, month>2&month<6))
t.test(finjuries~yr, data=subset(NYC_crash.covid.injury.week, month>5))


#########ped and cyclist summary####################################

NYC_crash.covid.injury.pedcycle<-subset(NYC_crash.covid.injury, number_of_pedestrians_injured>=1|number_of_pedestrians_killed>=1
                                        |number_of_cyclist_injured>=1|number_of_cyclist_killed>=1)

NYC_crash.covid.injury.pedcycle.week<-NYC_crash.covid.injury.pedcycle%>%
  group_by(week)%>%
  summarize(count=n(), tinjuries=sum(as.numeric(number_of_persons_injured)), 
            finjuries=sum(as.numeric(number_of_persons_killed)), yr=mean(year))

NYC_crash.covid.injury.pedcycle.week<-subset(NYC_crash.covid.injury.pedcycle.week, yr>2018)

t.test(tinjuries~yr, data=NYC_crash.covid.injury.pedcycle.week)
t.test(finjuries~yr, data=NYC_crash.covid.injury.pedcycle.week)


NYC_crash.covid.injury.pedcycle.week$month<-month(NYC_crash.covid.injury.pedcycle.week$week)

t.test(tinjuries~yr, data=subset(NYC_crash.covid.injury.pedcycle.week, month<3))
t.test(tinjuries~yr, data=subset(NYC_crash.covid.injury.pedcycle.week, month>2&month<6))
t.test(tinjuries~yr, data=subset(NYC_crash.covid.injury.pedcycle.week, month>5))

t.test(finjuries~yr, data=subset(NYC_crash.covid.injury.pedcycle.week, month<3))
t.test(finjuries~yr, data=subset(NYC_crash.covid.injury.pedcycle.week, month>2&month<6))
t.test(finjuries~yr, data=subset(NYC_crash.covid.injury.pedcycle.week, month>5))



####speeding related crashes#################

NYC_crash.covid.injury.speeding<-subset(NYC_crash.covid.injury, contributing_factor_vehicle_1=="Unsafe Speed"
                                 |contributing_factor_vehicle_2=="Unsafe Speed"
                                 |contributing_factor_vehicle_3=="Unsafe Speed"
                                 |contributing_factor_vehicle_4=="Unsafe Speed"
                                 |contributing_factor_vehicle_5=="Unsafe Speed")

NYC_crash.covid.injury.speeding.week<-NYC_crash.covid.injury.speeding%>%
  group_by(week)%>%
  summarize(count=n(), tinjuries=sum(as.numeric(number_of_persons_injured)), 
            finjuries=sum(as.numeric(number_of_persons_killed)), yr=mean(year))

NYC_crash.covid.injury.speeding.week<-subset(NYC_crash.covid.injury.speeding.week, yr>2018)

t.test(tinjuries~yr, data=NYC_crash.covid.injury.speeding.week)
t.test(finjuries~yr, data=NYC_crash.covid.injury.speeding.week)


NYC_crash.covid.injury.speeding.week$month<-month(NYC_crash.covid.injury.speeding.week$week)

t.test(tinjuries~yr, data=subset(NYC_crash.covid.injury.speeding.week, month<3))
t.test(tinjuries~yr, data=subset(NYC_crash.covid.injury.speeding.week, month>2&month<6))
t.test(tinjuries~yr, data=subset(NYC_crash.covid.injury.speeding.week, month>5))
t.test(tinjuries~yr, data=subset(NYC_crash.covid.injury.speeding.week, month>3))



t.test(finjuries~yr, data=subset(NYC_crash.covid.injury.speeding.week, month<3))
t.test(finjuries~yr, data=subset(NYC_crash.covid.injury.speeding.week, month>2&month<6))
t.test(finjuries~yr, data=subset(NYC_crash.covid.injury.speeding.week, month>5))
#nrow(subset(NYC_crash.covid, crash_date<="2019-12-31" & number_of_cyclist_killed>0))

write.csv(NYC_crash.covid.injury.speeding.week, file="NYC_crash.speeding.csv")

###traffic volume data from NYC############################################################
token <- "xxxxxxxxxxxxxxxxxxxxx"

NYC_tflow<-read.socrata("https://data.ny.gov/resource/qzve-kjga.json", app_token=token)

NYC_flow.covid<-subset(NYC_tflow, date>"2017-12-31" & date<"2020-12-31")

NYC_tflow.covid2<-subset(NYC_tflow, date<="2020-12-31" & date>="2020-01-01")

NYC_tflow.covid2$flowcount<-as.numeric(NYC_tflow.covid2$vehicles_e_zpass)+as.numeric(NYC_tflow.covid2$vehicles_vtoll)

NYC_tflow.covid.date<-NYC_tflow.covid2 %>%
  group_by(date) %>%
  summarize(count=sum(flowcount))

write.csv(NYC_tflow.covid.date, file="nyc_traffic_flow0715.csv")

ggplot(data = NYC_tflow.covid.date, aes(x=date, y=count, group=1))+ 
  geom_line(color="red")+ 
  geom_point()#--- Run the two-sample t-test
#bab9 %$% t.test(bweight ~ sex, var.equal = T)

NYC_tflow.ts<-ts(NYC_tflow.covid.date$count, start=c(2020, as.POSIXlt("2020-01-01")$yday+1), frequency=365)

NYC_tflow.bp <- cpt.mean(NYC_tflow.ts, penalty = "BIC", method = "SegNeigh")
plot(NYC_tflow.bp, type = "l", xlab = "Index", xaxt="n", cpt.width = 4)
a = seq(as.Date("2020-01-01"), by="weeks", length=53)
axis(1, at = decimal_date(a), labels = format(a, "%Y %b %d"), cex.axis=0.8)

NYC_tflow.covid.date%>%slice(cpts(NYC_tflow.bp))

NYC_flow.covid$vehicles_e_zpass<-as.numeric(NYC_flow.covid$vehicles_e_zpass)
NYC_flow.covid$vehicles_vtoll<-as.numeric(NYC_flow.covid$vehicles_vtoll)

NYC_flow.byweek<-NYC_flow.covid %>%
  group_by(week)%>%
  summarize(tflow=sum(vehicles_e_zpass)+sum(vehicles_vtoll))

write.csv(NYC_flow.byweek, file="nyc_traffic_flow0627.csv")
ggplot(data = NYC_flow.byweek, aes(x=week, y=tflow, group=1))+ 
  geom_line(color="red")+ 
  geom_point()







##############breaking point analysis###########################################

NYC_tflow<-read.csv(file.choose())

NYC_tflow.covid$week<-floor_date(seabikecount2$date, "week")





###crash data processing########################################################

NYC_crash.covid$week<-floor_date(NYC_crash.covid$crash_date, "week")
NYC_crash.byweek<-NYC_crash.covid%>%
  group_by(week)%>%
  summarize(fatal=sum(number_of_persons_killed),injuries=sum(number_of_persons_injured), NUM=n())

#library(collapse)
NYC_crash.pedestrians.byweek<-subset(NYC_crash.covid,number_of_pedestrians_injured>0|number_of_pedestrians_killed>0)%>%
  group_by(week)%>%
  summarize(pfatal=sum(number_of_pedestrians_killed),pinjuries=sum(number_of_pedestrians_injured), 
            pNUM=n())

NYC_crash.covid.byweek<-left_join(NYC_crash.byweek, NYC_crash.pedestrians.byweek, by="week")

NYC_crash.cyclist.byweek<-subset(NYC_crash.covid, number_of_cyclist_injured>0 
                                 | number_of_cyclist_killed>0)%>%
  group_by(week)%>%
  summarize(bfatal=sum(number_of_cyclist_killed),binjuries=sum(number_of_cyclist_injured), 
            bNUM=n())


NYC_crash.covid.byweek<-left_join(NYC_crash.covid.byweek, NYC_crash.cyclist.byweek, by="week")


write.csv(NYC_crash.covid.byweek, file="NYC_crash.covidbyweek0624.csv")
ggplot(data = NYC_crash.covid.byweek, aes(x=week, y=pinjuries, group=1))+ 
  geom_line(color="red")+ 
  geom_point()


###crash data processing#####ped cyclist encoding###################################################

NYC_crash.person<-read.socrata("https://data.cityofnewyork.us/resource/f55k-p6yu.json", app_token=token)
NYC_crash.person.covid<-subset(NYC_crash.person, crash_date>="2019-01-01" & crash_date<"2021-01-01")


#NYC_crash.person.2019<-subset(NYC_crash.person, crash_date>="2019-01-01" & crash_date<"2020-01-01")
plyr::count(NYC_crash.person.covid$person_type)
NYC_crash.person.covid$week<-floor_date(NYC_crash.person.covid$crash_date, "week")

NYC_crash.person.byweektype<-NYC_crash.person.covid%>%
  group_by(week, person_type)%>%
  summarize(count=n())
NYC_crash.person.byweek<-left_join(NYC_crash.person.byweektype, NYC_flow.byweek, by="week")
write.csv(NYC_crash.person.byweek, file="nyc_crash_byweek.csv")


####bike counts in NYC#################
NYC_bike<-read.socrata("https://data.cityofnewyork.us/resource/uczf-rk3c.json", app_token=token)
NYC_bike.covid<-subset(NYC_bike, date>="2019-01-01" & date<"2021-01-01")
NYC_bike.covid$week<-floor_date(NYC_bike.covid$date, "week")
NYC_bike.covid$counts<-as.numeric(NYC_bike.covid$counts)
NYC_bike.covid.byweek<-NYC_bike.covid%>%
  group_by(week)%>%
  summarize(count=n(), bflow=sum(counts,na.rm=TRUE))

ggplot(data = NYC_bike.covid.byweek, aes(x=week, y=bflow, group=1))+ 
  geom_line(color="red")+ 
  geom_point()

NYC_crash.person.byweek<-left_join(NYC_crash.person.byweek, NYC_bike.covid.byweek, by="week")

NYC_crash.person.byweek<-subset(NYC_crash.person.byweek, select=-c(count.y, bflow.x))

write.csv(NYC_crash.person.byweek, file="NYC_crash_byweek.csv")

write.csv(NYC_bike.covid.byweek, file="NYC_bike_byweek.csv")


####climate in NYC#################
NYC_climate<-read.csv(file.choose())
NYC_climate.covid<-subset(NYC_climate, DATE>="2019-01-01" & DATE<"2021-01-01")
NYC_climate.covid$DATE<-as.Date(NYC_climate.covid$DATE)
NYC_crash.covid.speeding

ggplot(data = NYC_climate.covid.byweek, aes(x=week, y=TMAX, group=1))+ 
  geom_line(color="red")+ 
  geom_point()

ggplot(data = NYC_climate.covid.byweek, aes(x=week, y=PRCP, group=1))+ 
  geom_line(color="red")+ 
  geom_point()

write.csv(NYC_climate.covid.byweek, file="NYC_climatebyweek.csv")

####injuries data in NYC#################

NYC_crash.covid.speeding$week<-floor_date(NYC_crash.covid.speeding$crash_date, "week")
NYC_crash.covid.speeding.byweek<-NYC_crash.covid.speeding%>%
  group_by(week)%>%
  summarize(count=n())

ggplot(data = NYC_crash.covid.speeding.byweek, aes(x=week, y=count, group=1))+ 
  geom_line(color="red")+ 
  geom_point()


NYC_crash.2019.speeding$week<-floor_date(NYC_crash.2019.speeding$crash_date, "week")
NYC_crash.2019.speeding.byweek<-NYC_crash.2019.speeding%>%
  group_by(week)%>%
  summarize(count=n())

ggplot(data = NYC_crash.2019.speeding.byweek, aes(x=week, y=count, group=1))+ 
  geom_line(color="red")+ 
  geom_point()




#####################k function to compare point patterns##############################################
NYC_crash.speeding<-subset(NYC_crash.covid, contributing_factor_vehicle_1=="Unsafe Speed"
                           |contributing_factor_vehicle_2=="Unsafe Speed"
                           |contributing_factor_vehicle_3=="Unsafe Speed"
                           |contributing_factor_vehicle_4=="Unsafe Speed"
                           |contributing_factor_vehicle_5=="Unsafe Speed")

NYC.speeding.kde.combine<-subset(NYC_crash.speeding, (crash_date>="2019-03-01" & crash_date<="2019-05-31")|
                                   (crash_date>="2020-03-01" & crash_date<="2020-05-31"))


NYC.speeding.kde.combine$covid<-"N"
NYC.speeding.kde.combine$covid[NYC.speeding.kde.combine$crash_date>="2020-03-01" & NYC.speeding.kde.combine$crash_date<="2020-12-31"]<-"Y"
plyr::count(NYC.speeding.kde.combine$covid)

NYC.speeding.kde.combine$lat<-as.numeric(NYC.speeding.kde.combine$location.latitude)
NYC.speeding.kde.combine$long<-as.numeric(NYC.speeding.kde.combine$location.longitude)

NYC.speeding.sf<-st_as_sf(subset(NYC.speeding.kde.combine, lat>0 | long>0), coords=c("long","lat"),crs=4326)

plot(NYC.speeding.sf)

NYC.speeding.proj<-st_transform(NYC.speeding.sf, 2263)


NYC.speeding.kde.combine.ppp<-as.ppp(NYC.speeding.proj)
marks(NYC.speeding.kde.combine.ppp)<-factor(NYC.speeding.proj$covid)


tm_shape(nyc) + tm_polygons(col="grey", border.col="white")+
  tm_shape(NYC.speeding.proj) + tm_dots(size=  .3, col = "red")

NYC.speeding.kde.combine.ppp<-rescale(NYC.speeding.kde.combine.ppp, 1000)
k<-Kcross.inhom(NYC.speeding.kde.combine.ppp, correction="best")
plyr::count(NYC.speeding.kde.combine.ppp$marks)
plot(k, . - pi * r^2 ~ r)

e = envelope(NYC.speeding.kde.combine.ppp, Kcross.inhom, i = "Y", j = "N", simulate = expression(rlabel(NYC.speeding.kde.combine.ppp)), 
             verbose = FALSE)
plot(e, . - mmean ~ r, legend=FALSE)

NYC.speeding.proj.map<-subset(NYC.speeding.proj, select=c("longitude","latitude", "covid"))

st_write(NYC.speeding.proj.map, "nyc_crash_speeding1.shp", driver="ESRI Shapefile")  # create to a shapefile 

