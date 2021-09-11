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
options(scipen=999)
options(max.print=9999999)
#spatial
library(rgdal)
library(sp)
library(sf)
library(GISTools)
library("lessR")
library("knitr")
library(arcpullr)
library(foreign)
library(pls)
library(raster)
library(maptools)
library(spatstat)
library(tmap)
library(changepoint)
library(strucchange)
#library(sarbcurrent)
library(tidyverse)
library(lubridate)
if (!require(ggfortify)) install.packages("ggfortify")
library(ggfortify)

rm(list = ls(all.names = TRUE))

###############trafficflowcovid.csv which has been preprocessed###########################################
traffic.flow<-read.csv(file.choose())
traffic.flow.covid<-subset(traffic.flow, select=c(TrafficVolume, Date))

traffic.flow$date<-as.Date (traffic.flow.covid$Date, "%m/%d/%Y")
traffic.flow$week<-floor_date(traffic.flow.covid$date, "week")

ggplot(data = traffic.flow, aes(x=Date, y=TrafficVolume, group=1))+ 
  geom_line(color="red")+ 
  geom_point()


#sea_crash.covid.byweek<-left_join(sea_crash.covid.byweek, traffic.flow.covid.byweek, by="week")

#t.test(tflow~covid1, data=subset(sea_crash.covid.byweek, covid0==1|covid1==1))
#t.test(tflow~covid, data=sea_crash.covid.byweek)

###traffic flow BP dections
#traffic.flow<-subset(traffic.flow.covid, date >"2019-12-31")


#write.csv(traffic.flow.covid2, file="trafficflowcovid.csv")

#require(xts)
#library(zoo)

#sea_traffic.ts <- zoo(traffic.flow.covid2$TrafficVolume, seq(from = as.Date("2020-01-01"), to = as.Date("2020-12-31"), by = 1))
#sea_traffic.ts<-as.ts(sea_traffic.ts)
#sea_traffic.xts<-xts(traffic.flow.covid2$TrafficVolume, traffic.flow.covid2$date)

sea_traffic.ts<-ts(traffic.flow$TrafficVolume, start=c(2020, as.POSIXlt("2020-01-01")$yday+1), frequency=365)
sea.traffic.bp <- cpt.mean(sea_traffic.ts, penalty = "BIC", method = "SegNeigh")
plot(sea.traffic.bp, type = "l", xlab="Traffic volume at major highway locations", xaxt="n", ylab="traffic volume", cpt.width = 4)
a = seq(as.Date("2020-01-01"), by="weeks", length=53)
axis(1, at = decimal_date(a), labels = format(a, "%Y %b %d"), cex.axis=0.9)
cpts(sea.traffic.bp)

traffic.flow.covid%>%slice(cpts(sea.traffic.bp))

write.csv(traffic.flow.covid, file="seattle_traffic0715.csv")

########read collision data#####################################################
sea_collision<-read.csv(file.choose())

sea_collision$lat<-as.numeric(sea_collision$ï..X)
sea_collision$long<-as.numeric(sea_collision$Y)

sea_collision$date=as.Date(sea_collision$INCDATE)
sea_collision.after2019<-subset(sea_collision, date >= "2019-01-01" & date < "2021-01-01")
#sea_collision.speeding<-subset(sea_collision.after2019, SPEEDING=="Y")
sea_collision.covid<-subset(sea_collision, date >= "2019-01-01" & date < "2021-01-01")
sea_collision.covid2<-subset(sea_collision, date >= "2020-01-01" & date < "2021-01-01")

plyr::count(sea_collision.after2019$COLLISIONTYPE)


####sub group analysis###########################
sea_collision.after2019$week<-floor_date(sea_collision.after2019$date, "week")
sea_collision.wk<-sea_collision.after2019%>%
  group_by(week)%>%
  summarize(count=n(), person=sum(PERSONCOUNT), injuries=sum(INJURIES), sfinjuries=sum(SERIOUSINJURIES+FATALITIES))

sea_collision.wk$year<-year(sea_collision.wk$week)

sea_collision.wk$month<-month(sea_collision.wk$week)
#sea_collision.wk$week<-floor_date(sea_collision.date$date, "week")
sea_collision.week<-subset(sea_collision.wk, year>2018)

sea_collision$lat<-as.numeric(sea_collision$ï..X)
sea_collision$long<-as.numeric(sea_collision$Y)

###ttest for 2020 vs. 2019##########
t.test(person~year, data=sea_collision.week)
t.test(injuries~year, data=sea_collision.week)
t.test(Sfinjuries~year, data=sea_collision.week)
t.test(person~year, data=subset(sea_collision.week, month<3))
t.test(person~year, data=subset(sea_collision.week,month>2&month<6))
t.test(person~year, data=subset(sea_collision.week, month>5))
t.test(injuries~year, data=subset(sea_collision.week, month<3))
t.test(injuries~year, data=subset(sea_collision.week, month>2&month<6))
t.test(injuries~year, data=subset(sea_collision.week, month>5))
t.test(Sfinjuries~year, data=subset(sea_collision.week, month<3))
t.test(Sfinjuries~year, data=subset(sea_collision.week, month>2&month<6))
t.test(Sfinjuries~year, data=subset(sea_collision.week, month>5))

#pedestrians and cyclist type crashes####
sea_collision.pedcycle<-subset(sea_collision.after2019,  PEDCOUNT>0 | PEDCYLCOUNT>0)

sea_collision.pedcycle$week<-floor_date(sea_collision.pedcycle$date, "week")
sea_collision.pedcycle.wk<-sea_collision.pedcycle%>%
  group_by(week)%>%
  summarize(count=n(), person=sum(PERSONCOUNT), injuries=sum(INJURIES), sfinjuries=sum(SERIOUSINJURIES+FATALITIES))

sea_collision.pedcycle.wk$year<-year(sea_collision.pedcycle.wk$week)
sea_collision.pedcycle.wk$month<-month(sea_collision.pedcycle.wk$week)
#sea_collision.wk$week<-floor_date(sea_collision.date$date, "week")
sea_collision.pedcycle.wk<-subset(sea_collision.pedcycle.wk, year>2018)

#pedestrains and cyclist type crashes####
sea_collision.pedcycle<-subset(sea_collision.after2019,  PEDCOUNT>0 | PEDCYLCOUNT>0)

sea_collision.pedcycle$week<-floor_date(sea_collision.pedcycle$date, "week")
sea_collision.pedcycle.wk<-sea_collision.pedcycle%>%
  group_by(week)%>%
  summarize(count=n(), person=sum(PERSONCOUNT), injuries=sum(INJURIES), sfinjuries=sum(SERIOUSINJURIES+FATALITIES))

sea_collision.pedcycle.wk$year<-year(sea_collision.pedcycle.wk$week)
sea_collision.pedcycle.wk$month<-month(sea_collision.pedcycle.wk$week)
#sea_collision.wk$week<-floor_date(sea_collision.date$date, "week")
sea_collision.pedcycle.wk<-subset(sea_collision.pedcycle.wk, year>2018)


#ttest
t.test(person~year, data=sea_collision.pedcycle.wk)
t.test(injuries~year, data=sea_collision.pedcycle.wk)
t.test(sfinjuries~year, data=sea_collision.pedcycle.wk)

t.test(person~year, data=subset(sea_collision.pedcycle.wk, month<3))
t.test(person~year, data=subset(sea_collision.pedcycle.wk,month>2&month<6))
t.test(person~year, data=subset(sea_collision.pedcycle.wk, month>5))

t.test(injuries~year, data=subset(sea_collision.pedcycle.wk, month<3))
t.test(injuries~year, data=subset(sea_collision.pedcycle.wk, month>2&month<6))
t.test(injuries~year, data=subset(sea_collision.pedcycle.wk, month>5))

t.test(sfinjuries~year, data=subset(sea_collision.pedcycle.wk, month<3))
t.test(sfinjuries~year, data=subset(sea_collision.pedcycle.wk, month>2&month<6))
t.test(sfinjuries~year, data=subset(sea_collision.pedcycle.wk, month>5))


##################Speeding#########################################################

sea_collision.Speeding<-subset(sea_collision.after2019, SPEEDING=="Y")
                                                                         
sea_collision.Speeding$week<-floor_date(sea_collision.Speeding$date, "week")
sea_collision.Speeding.wk<-sea_collision.Speeding%>%
  group_by(week)%>%
  summarize(count=n(), person=sum(PERSONCOUNT), injuries=sum(INJURIES), sfinjuries=sum(SERIOUSINJURIES+FATALITIES))

sea_collision.Speeding.wk$year<-year(sea_collision.Speeding.wk$week)
sea_collision.Speeding.wk$month<-month(sea_collision.Speeding.wk$week)
#sea_collision.wk$week<-floor_date(sea_collision.date$date, "week")
sea_collision.Speeding.wk<-subset(sea_collision.Speeding.wk, year>2018)

#ttest

t.test(person~year, data=sea_collision.Speeding.wk)
t.test(injuries~year, data=sea_collision.Speeding.wk)
t.test(sfinjuries~year, data=sea_collision.Speeding.wk)

t.test(person~year, data=subset(sea_collision.Speeding.wk, month<3))
t.test(person~year, data=subset(sea_collision.Speeding.wk,month>2&month<6))
t.test(person~year, data=subset(sea_collision.Speeding.wk, month>5))

t.test(count~year, data=subset(sea_collision.Speeding.wk, month<3))
t.test(count~year, data=subset(sea_collision.Speeding.wk,month>2&month<6))
t.test(count~year, data=subset(sea_collision.Speeding.wk, month>5))

t.test(injuries~year, data=subset(sea_collision.Speeding.wk, month<3))
t.test(injuries~year, data=subset(sea_collision.Speeding.wk, month>2&month<6))
t.test(injuries~year, data=subset(sea_collision.Speeding.wk, month>5))

t.test(sfinjuries~year, data=subset(sea_collision.Speeding.wk, month<3))
t.test(sfinjuries~year, data=subset(sea_collision.Speeding.wk, month>2&month<6))
t.test(sfinjuries~year, data=subset(sea_collision.Speeding.wk, month>5))

######speeding mapping#################

sea_collision.speeding<-subset(sea_collision.after2019, SPEEDING=="Y" & ((date>="2019-03-01" & date<="2019-12-31")
                               | (date>="2020-03-01" & date<="2020-12-31")))

sea_collision.speeding.2019<-subset(sea_collision.after2019, date>="2019-03-01" & date<="2019-12-31" & SPEEDING=="Y")
sea_collision.speeding.2020<-subset(sea_collision.after2019, date>="2020-03-01" & date<="2020-12-31" & SPEEDING=="Y")

sea_collision.speeding.kde.2019<-st_as_sf(subset(sea_collision.speeding.2019, lat>0 | long>0), coords = c("lat","long" ), crs=4326)
plot(sea_collision.speeding.kde.2019)

sea_collision.speeding.kde.2020<-st_as_sf(subset(sea_collision.speeding.2020, lat>0 | long>0), coords = c("lat","long" ), crs=4326)
plot(sea_collision.speeding.kde.2020)

sea_collision.speeding.kde.2019 <-st_transform(sea_collision.speeding.kde.2019, 32148)
sea_collision.speeding.kde.2020 <-st_transform(sea_collision.speeding.kde.2020, 32148)


########single vehicle crashes####################

sea_collision.single<-subset(sea_collision.after2019, VEHCOUNT<=1)
sea_collision.single$week<-floor_date(sea_collision.single$date, "week")
sea_collision.single.wk<-sea_collision.single%>%
  group_by(week)%>%
  summarize(count=n(), person=sum(PERSONCOUNT), injuries=sum(INJURIES), sfinjuries=sum(SERIOUSINJURIES+FATALITIES))

sea_collision.single.wk$year<-year(sea_collision.single.wk$week)
sea_collision.single.wk$month<-month(sea_collision.single.wk$week)
#sea_collision.wk$week<-floor_date(sea_collision.date$date, "week")
sea_collision.single.wk<-subset(sea_collision.single.wk, year>2018)
write.csv(sea_collision.single.wk, file="sea_collisions_single.csv")

#ttest

t.test(person~year, data=sea_collision.single.wk)
t.test(injuries~year, data=sea_collision.single.wk)
t.test(sfinjuries~year, sea_collision.single.wk)

t.test(person~year, data=subset(sea_collision.single.wk, month<3))
t.test(person~year, data=subset(sea_collision.single.wk,month>2&month<6))
t.test(person~year, data=subset(sea_collision.single.wk, month>5))

t.test(count~year, data=subset(sea_collision.single.wk, month<3))
t.test(count~year, data=subset(sea_collision.single.wk,month>2&month<6))
t.test(count~year, data=subset(sea_collision.single.wk, month>5))

t.test(injuries~year, data=subset(sea_collision.single.wk, month<3))
t.test(injuries~year, data=subset(sea_collision.single.wk, month>2&month<6))
t.test(injuries~year, data=subset(sea_collision.single.wk, month>5))

t.test(sfinjuries~year, data=subset(sea_collision.single.wk, month<3))
t.test(sfinjuries~year, data=subset(sea_collision.single.wk, month>2&month<6))
t.test(sfinjuries~year, data=subset(sea_collision.single.wk, month>5))


######kde functions#################



###########################st_crs(sea_injuries.2019.proj)<-32148#################################################

plot(sea_collision.speeding.kde.2020 )
#st_crs(sea_collision_sf)
#st_crs(sea_collision_sf)<-seacrs
sea_city<-st_read(file.choose())
st_write(sea_city, "sea_city_taz.shp", driver="ESRI Shapefile")  # create to a shapefile 

st_crs(sea_city)
sea_city.proj<-st_transform(sea_city, 32148)

#plot(sea_collision_sf)

par(mfrow=c(1,2))
plot(sea_collision.speeding.kde.2019)
plot(sea_collision.speeding.kde.2020)
kde2019.speeding<-tm_shape(sea_city.proj) + tm_polygons(col="grey", border.col="white") + 
  tm_shape(sea_collision.speeding.kde.2019) + tm_dots(size=  .3, col = "red") 

kde2020.speeding<-tm_shape(sea_city.proj) + tm_polygons(col="grey", border.col="white") + 
  tm_shape(sea_collision.speeding.kde.2020) + tm_dots(size=  .3, col = "red") 

current.mode <- tmap_mode("plot")
tmap_arrange(kde2019.speeding, kde2020.speeding)
tmap_mode(current.mode)
#seacrs<-st_crs(sea_city) 
#sea_collision_sf.proj<-st_transform(sea_collision_sf, seacrs=crs)

#sea_collision_sf.proj<-st_set_crs(sea_collision_sf, 32148)


################kde function in R##################################################################


sea_collision.speeding.kde.2019.ppp<-as.ppp(sea_collision.speeding.kde.2019)

marks(sea_collision.speeding.kde.2019.ppp) <- NULL
sea.speeding.2019.kde<-rescale(sea_collision.speeding.kde.2019.ppp, 1000)

sea_city_bg<-as.owin(sea_city.proj)
sea_city_bg<-rescale(sea_city_bg, 1000)

Window(sea.speeding.2019.kde) <- sea_city_bg

plot(sea.speeding.2019.kde, main=NULL, cols=rgb(0,0,0,.2), pch=20)

sea.k1.covid<-density(sea.speeding.2019.kde, sigma=1)
plot(sea.k1.covid, main=NULL, las=1)


#kde for 2020

sea_collision.2020.kde<-as.ppp(sea_collision.speeding.kde.2020)

marks(sea_collision.2020.kde) <- NULL
sea_collision.2020.kde<-rescale(sea_collision.2020.kde, 1000)

sea_city_bg<-as.owin(sea_city.proj)
sea_city_bg<-rescale(sea_city_bg, 1000)

Window(sea_collision.2020.kde) <- sea_city_bg

plot(sea_collision.2020.kde, main=NULL, cols=rgb(0,0,0,.2), pch=20)

sea.k2.covid<-density(sea_collision.2020.kde, sigma=1)

par(mfrow=c(1,2))
plot(sea.k2.covid, main=NULL, las=1)
plot(sea.k1.covid, main=NULL, las=1)


#####################k function to compare point patterns##############################################
#sea_collision.kde.combine<-subset(sea_collision_sf.proj,
#(date>"2019-03-08" & date<"2019-12-31")|(date>"2020-03-08" & date<"2020-12-31"))
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
#Kred <- Kest(sea_collision.combine.ppp[sea_collision.combine.ppp$marks == "Y", ], correction = "best")
#Kwhite <- Kest(sea_collision.combine.ppp[sea_collision.combine.ppp$marks == "N", ], correction = "best")
#plot(Kred$iso - Kwhite$iso, type = "o", ylim = c(-0.01, 0.02))


ppm1<-ppm(sea_collision.2020.kde~sea.k1.covid)
ppm1

##############crash type analysis##########kernel densities##########################
sea_speeding.kde.combine<-subset(sea_collision.kde.combine, SPEEDING=="Y")
#sea_ped.kde.combine<-subset(sea_collision.kde.combine, SPEEDING=="Y")

#sea_collision.kde.combine$covid<-"N"
#sea_collision.kde.combine$covid[sea_collision.kde.combine$date>"2020-03-08" & sea_collision.kde.combine$date<"2020-04-05"]<-"Y"
plyr::count(sea_speeding.kde.combine$covid)
sea_speeding.combine.ppp<-as.ppp(sea_speeding.kde.combine)
marks(sea_speeding.combine.ppp)<-factor(sea_speeding.kde.combine$covid)
plot(split(sea_speeding.combine.ppp))

e = envelope(sea_speeding.combine.ppp, Kcross.inhom, i = "Y", j = "N", simulate = expression(rlabel(sea_speeding.combine.ppp)), 
             verbose = FALSE)
plot(e, . - mmean ~ r, legend=FALSE)

kspeeding.covid<-density(split(sea_speeding.combine.ppp))
plot(kspeeding.covid)

sea_speeding<-subset(sea_collision.covid, SPEEDING=="Y")
plyr::count(sea_speeding$SEVERITYDESC)
plyr::count(sea_speeding$PEDCYLCOUNT)


####type of crash analysis#########################
plyr::count(sea_collision.covid$COLLISIONTYPE)
plyr::count(sea_collision.covid$FATALITIES)
plyr::count(sea_collision.covid2$SDOT_COLDESC)
plyr::count(sea_collision.covid$JUNCTIONTYPE)
plyr::count(sea_collision.covid$SPEEDING)

plyr::count(sea_collision.covid2$LIGHTCOND)
plyr::count(sea_collision.covid$ST_COLDESC)
plyr::count(sea_collision.covid2$UNDERINFL)
plyr::count(sea_collision.covid2$EXCEPTRSNDESC)
plyr::count(sea_collision.covid2$INATTENTIONIND)
#####creating subset of crashes#############################
sea_collision.covid<-subset(sea_collision, date > "2019-12-31" & date < "2021-01-01")


###############further analysis of drug/alchol related##########################################################

sea_collision.covid.drug<-subset(sea_collision.covid,sea_collision.covid$UNDERINFL=="Y")

sea_collision.covid.drug$week<-floor_date(sea_collision.covid.drug$date, "week")

sea_collision.covid.drug.byweek<-sea_collision.covid.drug %>%
  group_by(week) %>%
  summarize(count=n())

ggplot(data = sea_collision.covid.drug.byweek, aes(x=week, y=count, group=1))+ 
  geom_line(color="red")+ 
  geom_point()


###########################
sea_crash.covid<-subset(sea_crash,INCDATE > "2015-01-01")
sea_crash.covid.bydate<-data.frame(with(sea_crash.covid, table(INCDATE)))
plyr::count(sea_crash.covid$SEVERITYDE)

sea_crash.covid.pedcycle<-subset(sea_crash.covid, PEDCOUNT>0 | PEDCYLCOUN>0)
sea_crash.ped.bydate<-data.frame(with(sea_crash.covid.pedcycle, table(INCDATE)))

sea_crash.covid.speeding<-subset(sea_crash.covid, SPEEDING=="Y")
sea_crash.covid.speeding.bydate<-data.frame(with(sea_crash.covid.speeding, table(INCDATE)))

sea_crash.covid.sinjuries<-subset(sea_crash.covid, (SEVERITYDE=="Fatality Collision" | SEVERITYDE=="Serious Injury Collision"))
sea_crash.covid.sinjuries.bydate<-data.frame(with(sea_crash.covid.sinjuries, table(INCDATE)))

sea_crash.covid.injuries<-subset(sea_crash.covid, (SEVERITYDE=="Injury Collision"))
sea_crash.covid.injuries.bydate<-data.frame(with(sea_crash.covid.injuries, table(INCDATE)))

sea_crash.bydate.joinspeeding<-left_join(sea_crash.covid.bydate, sea_crash.covid.speeding.bydate, by="INCDATE")
sea_crash.bydate.join<-left_join(sea_crash.bydate.joinspeeding, sea_crash.ped.bydate, by="INCDATE")
sea_crash.bydate.join<-left_join(sea_crash.bydate.join, sea_crash.covid.sinjuries.bydate, by="INCDATE")
sea_crash.bydate.join<-left_join(sea_crash.bydate.join, sea_crash.covid.injuries.bydate, by="INCDATE")

write.csv(sea_crash.bydate.join, file="sea_crash_join.csv")



####pedestrians and bicyclists###################
sea_pedcycle.kde.combine<-subset(sea_collision.kde.combine, PERSONCOUNT>0&(PEDCOUNT>0|PEDCYLCOUNT>0))
plyr::count(sea_pedcycle.kde.combine$covid)
sea_pedcycle.kde.combine.ppp<-as.ppp(sea_pedcycle.kde.combine)
marks(sea_pedcycle.kde.combine.ppp)<-factor(sea_pedcycle.kde.combine$covid)
ksPED.covid<-density(split(sea_pedcycle.kde.combine.ppp))
plot(ksPED.covid)

##############time of the day##########kernel densities##########################

library(lubridate)


####bike and ped counts in Seattle#####################################################################
token <- "?????????????????????????"
seabikecount<-read.socrata("https://data.seattle.gov/resource/aggm-esc4.json", app_token=token)
seabikecount$count<-as.numeric(seabikecount$fremont_bridge_nb)+as.numeric(seabikecount$fremont_bridge_sb)
seabikecount$week<-floor_date(seabikecount$date, "week")
seabikecount$Date<-floor_date(seabikecount$date, "day")

seabikecount.covid<-subset(seabikecount, Date<="2020-12-31" & Date>="2020-01-01")
#seabikecount$date<-floor_date(seabikecount$date, "week")

sea_bikecount.bydate<-seabikecount.covid %>%
  group_by(Date) %>%
  summarize(count=sum(count))

ggplot(data = sea_bikecount.bydate, aes(x=Date, y=count, group=1))+ 
  geom_line(color="red")+ 
  geom_point()

seabikecount2<-read.socrata("https://data.seattle.gov/resource/4qej-qvrz.json", app_token=token)
seabikecount2$bikecount<-as.numeric(seabikecount2$bike_north)+as.numeric(seabikecount2$bike_south)
seabikecount2$Date<-floor_date(seabikecount2$date, "day")
seabikecount2.covid<-subset(seabikecount2, Date<="2020-12-31" & Date>="2020-01-01")

sea_bikecount2.bydate<-seabikecount2.covid %>%
  group_by(Date) %>%
  summarize(count=sum(bikecount))

sea_bike_count<-left_join(sea_bikecount.bydate, sea_bikecount2.bydate, by="Date")
sea_bike_count$biketotal<-sea_bike_count$count.x+sea_bike_count$count.y

ggplot(data = sea_bike_count, aes(x=Date, y=biketotal, group=1))+ 
  geom_line(color="red")+ 
  geom_point()

####breaking point for biking and walking/running#######

sea_bike.ts<-ts(sea_bike_count$biketotal, start=c(2020, as.POSIXlt("2020-01-01")$yday+1), frequency=365)
sea.bike.bp <- cpt.mean(sea_bike.ts, penalty = "BIC", method = "SegNeigh", Q = 5)
plot(sea.bike.bp, type = "l", xlab = "Index", xaxt="n", cpt.width = 4)
a = seq(as.Date("2020-01-01"), by="weeks", length=53)
axis(1, at = decimal_date(a), labels = format(a, "%Y %b %d"), cex.axis=0.8)
sea_bike_count%>%slice(cpts(sea.bike.bp))

####bike and ped counts in Seattle#####################################################################

seabikecount2$week<-floor_date(seabikecount2$date, "week")

seabikecount2.byweek<-seabikecount2 %>%
  group_by(week) %>%
  summarize(count=sum(bikecount))

sea_bike_count.byweek<-left_join(seabikecount.covid, seabikecount2.byweek, by="week")
sea_bike_count.byweek$count<-sea_bike_count.byweek$count.x+sea_bike_count.byweek$count.y
ggplot(data = sea_bike_count.byweek, aes(x=week, y=count, group=1))+ 
  geom_line(color="red")+ 
  geom_point()

write.csv(sea_bike_count.byweek, file="sea_bik_count0627.csv")

####bike and ped counts in Seattle#####################################################################
sea_ped_count<-read.socrata("https://data.seattle.gov/resource/4qej-qvrz.json", app_token=token)
sea_ped_count$ped<-as.numeric(sea_ped_count$ped_north)+as.numeric(sea_ped_count$ped_south)
sea_ped_count$week<-floor_date(sea_ped_count$date, "week")

sea_bikeped_count<-left_join(sea_bike_count, sea_ped_count, by="date")

sea_bikeped_count$bikepedtotal<-sea_bikeped_count$ped+sea_bikeped_count$biketotal

ggplot(data = sea_bikeped_count, aes(x=date, y=bikepedtotal, group=1))+ 
  geom_line(color="red")+ 
  geom_point()

sea_ped_count.byweek<-sea_ped_count %>%
  group_by(week) %>%
  summarize(pedcount=sum(ped))

sea_bikeped_count.byweek<-left_join(sea_bike_count.byweek, sea_ped_count.byweek, by="week")
sea_bikeped_count.byweek$total<-sea_bikeped_count.byweek$count+sea_bikeped_count.byweek$pedcount

ggplot(data = sea_bikeped_count.byweek, aes(x=week, y=pedcount, group=1))+ 
  geom_line(color="red")+ 
  geom_point()
write.csv(sea_bikeped_count, file="sea_bikeped_count0628.csv")


########traffic flow by week#########




###bp analysis by week

#sea_trafficwk.ts<-ts(traffic.flow.covid.byweek$tflow, start=c(2020, as.POSIXlt("2020-01-01")$yday+1), frequency=365)
traffic.flow.covid.byweek<-subset(traffic.flow.covid.byweek, week>"2019-12-31")
sea.trafficwk.bp <- cpt.mean(traffic.flow.covid.byweek$tflow, penalty = "BIC", method = "SegNeigh")
cpts(sea.trafficwk.bp)
ggplot(data = traffic.flow.covid.byweek, aes(x=week, y=tflow, group=1))+ 
  geom_line(color="red")+ 
  geom_point()

plot(sea.trafficwk.bp, type = "l", xlab = "Index", xaxt="n", cpt.width = 4)
a = seq(as.Date("2020-01-01"), by="weeks", length=53)
axis(1, at = decimal_date(a), labels = format(a, "%Y %b %d"), cex.axis=0.8)
cpts(sea.traffic.bp)

traffic.flow.covid.byweek%>%slice(cpts(sea.trafficwk.bp))

#autoplot(cpt.mean(sea_traffic.ts, penalty = "BIC", method = "BinSeg", Q = 3), cpt.colour = 'blue', cpt.linetype = 'solid')
traffic.flow.covid2%>%slice(cpts(sea.traffic.bp))


traffic.flow.covid2%>%slice(cpts(sea.traffic.bp))

sea.traffic.bp2 <- cpt.mean(traffic.flow.covid2$TrafficVolume, penalty = "BIC", method = "SegNeigh")
plot(sea.traffic.bp2, type = "l", xlab = "Index", cpt.width = 4)
cpts(sea.traffic.bp2)
traffic.flow.covid2%>%slice(cpts(sea.traffic.bp2))

sea.traffic.bp3 <- cpt.mean(traffic.flow.covid2$TrafficVolume,  
                            penalty = "BIC", method = "SegNeigh", Q = 4)

plot(sea.traffic.bp3, type = "l", xlab = "Time", cpt.width = 4)
cpts(sea.traffic.bp3)
traffic.flow.covid2%>%slice(cpts(sea.traffic.bp3))


####export to modeling in stata#####################
write.csv(sea_crash.covid.byweek, file="sea_crash_covid0620.csv")

sea_crash.covid.byweek$month<-floor_date(sea_crash.covid.byweek$week, "month")


########climate by week#########

sea_climate<-read.csv(file.choose())

sea_climate.covid<-subset(sea_climate, select=c(DATE, PRCP, TAVG, TMAX))
sea_climate.covid$date<-as.Date (sea_climate.covid$DATE, "%Y-%m-%d")
sea_climate.covid$week<-floor_date(sea_climate.covid$date, "week")
sea_climate.covid.byweek<-sea_climate.covid %>%
  group_by(week) %>%
  summarize(Prec=mean(PRCP), Tavg=mean(TAVG))

sea_crash.covid.byweek<-read.csv(file.choose())
sea_crash.covid.byweek$wk<-as.Date (sea_crash.covid.byweek$week, "%m/%d/%Y")
sea_crash.covid.byweek<-left_join(sea_crash.covid.byweek, sea_climate.covid.byweek, by=c("wk"="week"))

write.csv(sea_crash.covid.byweek, file="sea_crash_covid0621.csv")


