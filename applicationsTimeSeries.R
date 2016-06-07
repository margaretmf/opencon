### DATA VISUALIZATION: TIME SERIES OF APPLICATIONS TO OPENCON (www.opencon2015.org) ###

# SET WORKING DIRECTORY #
setwd("~/Desktop/DataProjects")

# LIBRARIES #
library(rworldmap) # world map
library(ggplot2); library(reshape2); library(plyr); library(scales); library(ggthemes)
library(RColorBrewer) # ts heatmap
library(lubridate); library(base) # data

# READ DATA #
opencon2015.data <- read.csv("OpenCon2015OD.csv",header=TRUE,stringsAsFactors=FALSE)

# EVENT DATA #
event.participation1 <- opencon2015.data[,1:10]
event.participation2 <- opencon2015.data[,c(1:9,11)]
names(event.participation2) <- names(event.participation1)
event.participation3 <- opencon2015.data[,c(1:9,12)]
names(event.participation3) <- names(event.participation1)
event.participation <- rbind(event.participation1, event.participation2, event.participation3)

# SKILL DATA #
skill1 <- opencon2015.data[,c(1:9,13)]
skill2 <- opencon2015.data[,c(1:9,14)]
names(skill2) <- names(skill1)
skill3 <- opencon2015.data[,c(1:9,15)]
names(skill3) <- names(skill1)
skill4 <- opencon2015.data[,c(1:9,16)]
names(skill4) <- names(skill1)
skill5 <- opencon2015.data[,c(1:9,17)]
names(skill5) <- names(skill1)
skill <- rbind(skill1,skill2,skill3,skill4,skill5)

# OPEN CON DATA TS HEATMAP
opencon2015.time <- data.frame(table(opencon2015.data[,2]))
#opencon2015.time <- data.frame(table(ymd(strptime(opencon2015.data[,2], "%m/%d/%y",tz="America/New_York"))))
names(opencon2015.time) <- c("Date","Freq")
opencon2015.data$date <- data.frame(ymd(strptime(opencon2015.data[,2], "%m/%d/%y",tz="America/New_York")))

for (i in 1:length(opencon2015.data[,2])) {
  opencon2015.data$count[i] <- ifelse(opencon2015.data[i,2]==opencon2015.time[1,1],opencon2015.time[1,2],
                                      ifelse(opencon2015.data[i,2]==opencon2015.time[2,1],opencon2015.time[2,2],
                                             ifelse(opencon2015.data[i,2]==opencon2015.time[3,1],opencon2015.time[3,2],
                                                    ifelse(opencon2015.data[i,2]==opencon2015.time[4,1],opencon2015.time[4,2],
                                                           ifelse(opencon2015.data[i,2]==opencon2015.time[5,1],opencon2015.time[5,2],
                                                                  ifelse(opencon2015.data[i,2]==opencon2015.time[6,1],opencon2015.time[6,2],
                                                                         ifelse(opencon2015.data[i,2]==opencon2015.time[7,1],opencon2015.time[7,2],
                                                                                ifelse(opencon2015.data[i,2]==opencon2015.time[8,1],opencon2015.time[8,2],
                                                                                       ifelse(opencon2015.data[i,2]==opencon2015.time[9,1],opencon2015.time[9,2],
                                                                                              ifelse(opencon2015.data[i,2]==opencon2015.time[10,1],opencon2015.time[10,2],
                                                                                                     ifelse(opencon2015.data[i,2]==opencon2015.time[11,1],opencon2015.time[11,2],
                                                                                                            ifelse(opencon2015.data[i,2]==opencon2015.time[12,1],opencon2015.time[12,2],
                                                                                                                   ifelse(opencon2015.data[i,2]==opencon2015.time[13,1],opencon2015.time[13,2],
                                                                                                                          ifelse(opencon2015.data[i,2]==opencon2015.time[14,1],opencon2015.time[14,2],
                                                                                                                                 ifelse(opencon2015.data[i,2]==opencon2015.time[15,1],opencon2015.time[15,2],
                                                                                                                                        ifelse(opencon2015.data[i,2]==opencon2015.time[16,1],opencon2015.time[16,2],
                                                                                                                                               ifelse(opencon2015.data[i,2]==opencon2015.time[17,1],opencon2015.time[17,2],
                                                                                                                                                      ifelse(opencon2015.data[i,2]==opencon2015.time[18,1],opencon2015.time[18,2],
                                                                                                                                                             ifelse(opencon2015.data[i,2]==opencon2015.time[19,1],opencon2015.time[19,2],
                                                                                                                                                                    ifelse(opencon2015.data[i,2]==opencon2015.time[20,1],opencon2015.time[20,2],
                                                                                                                                                                           ifelse(opencon2015.data[i,2]==opencon2015.time[21,1],opencon2015.time[21,2],
                                                                                                                                                                                  ifelse(opencon2015.data[i,2]==opencon2015.time[22,1],opencon2015.time[22,2],
                                                                                                                                                                                         ifelse(opencon2015.data[i,2]==opencon2015.time[23,1],opencon2015.time[23,2],
                                                                                                                                                                                                ifelse(opencon2015.data[i,2]==opencon2015.time[24,1],opencon2015.time[24,2],
                                                                                                                                                                                                       ifelse(opencon2015.data[i,2]==opencon2015.time[25,1],opencon2015.time[25,2],
                                                                                                                                                                                                              ifelse(opencon2015.data[i,2]==opencon2015.time[26,1],opencon2015.time[26,2],
                                                                                                                                                                                                                     ifelse(opencon2015.data[i,2]==opencon2015.time[27,1],opencon2015.time[27,2],
                                                                                                                                                                                                                            ifelse(opencon2015.data[i,2]==opencon2015.time[28,1],opencon2015.time[28,2],
                                                                                                                                                                                                                                   ifelse(opencon2015.data[i,2]==opencon2015.time[29,1],opencon2015.time[29,2],
                                                                                                                                                                                                                                          ifelse(opencon2015.data[i,2]==opencon2015.time[30,1],opencon2015.time[30,2],0))))))))))))))))))))))))))))))
}

# year
opencon2015.data$year <- data.frame(year(strptime(opencon2015.data[,2], "%m%d%y",tz="America/New_York")))
# month
opencon2015.data$month <- data.frame(month(strptime(opencon2015.data[,2], "%m/%d/%y",tz="America/New_York")))
# wday
opencon2015.data$wday <- data.frame(wday(strptime(opencon2015.data[,2], "%m/%d/%y",tz="America/New_York")))
# turn month into ordered factors
opencon2015.data$monthf <- factor(opencon2015.data$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
# turn days into ordered factors
opencon2015.data$wkdayf <- factor(opencon2015.data$wday,levels=rev(1:7),labels=rev(c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")),ordered=TRUE)

# then find the "week of year" for each day
opencon2015.data$week <- data.frame(week(strptime(opencon2015.data[,2], "%m/%d/%y"))-21)

# Now for the plot
ggplot(opencon2015.data, aes(x=opencon2015.data$wday, y=opencon2015.data$week)) + geom_tile(aes(fill=count)) + ggtitle("Time Series of Applications") + labs(x="Weekday on Monday-Sunday",y="Week in June-July") + theme_solarized(light="true") + scale_fill_gradientn(colours = c("steelblue4","orangered2"))
