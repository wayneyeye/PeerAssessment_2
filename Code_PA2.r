#rm(list=ls())
if(!dir.exists("./data")){
        dir.create("./data")
}
dataURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if(!file.exists("./data/Stormdata.csv.bz2")){
download.file(dataURL,"./data/Stormdata.csv.bz2")
}
raw_stormdata<-read.csv(bzfile("./data/Stormdata.csv.bz2"))

###Extract the most interested variables###
library(dplyr)
my_stormdata<-select(raw_stormdata,EVTYPE,STATE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP)
my_stormdata<-mutate(my_stormdata,Event_Type=as.factor(toupper(as.character(EVTYPE))))
##############################
evt_factor_lvl<-levels(my_stormdata$Event_Type)
evt_factor_lvl[which(evt_factor_lvl=="TSTM WIND")]<-"THUNDERSTORM WIND"
evt_factor_lvl[which(evt_factor_lvl=="THUNDERSTORM WINDS")]<-"THUNDERSTORM WIND"
evt_factor_lvl[which(evt_factor_lvl=="WILD/FOREST FIRE")]<-"WILDFIRE"
evt_factor_lvl[which(evt_factor_lvl=="RIP CURRENTS")]<-"RIP CURRENT"
evt_factor_lvl[which(evt_factor_lvl=="HURRICANE")]<-"HURRICANE/TYPHOON"
levels(my_stormdata$Event_Type)<-evt_factor_lvl
##############################
my_stormdata<-mutate(my_stormdata,PropDmgExp=as.factor(toupper(as.character(PROPDMGEXP))))
my_stormdata<-mutate(my_stormdata,CropDmgExp=as.factor(toupper(as.character(CROPDMGEXP))))
prop_fac_lvl<-levels(my_stormdata$PropDmgExp)
prop_fac_lvl[!prop_fac_lvl %in% c("B","M","K","H")]<-"D"
crop_fac_lvl<-levels(my_stormdata$CropDmgExp)
crop_fac_lvl[!crop_fac_lvl %in% c("B","M","K","H")]<-"D"
levels(my_stormdata$PropDmgExp)<- prop_fac_lvl
levels(my_stormdata$CropDmgExp)<- crop_fac_lvl
Exp_vector_T<-c("D"=0.001/1000000000,"H"=0.1/1000000000,"K"=1/1000000000,"M"=1/1000000,"B"=1/1000)

##############################
grouped_stormdata<-group_by(my_stormdata,Event_Type)
sum_cas_stormdata<-summarize(grouped_stormdata,CASUALTIES=sum(FATALITIES+INJURIES),TOTAL_INJURIES=sum(INJURIES),TOTAL_FATALITIES=sum(FATALITIES))
sum_cas_stormdata<-arrange(sum_cas_stormdata,desc(CASUALTIES))
sum_cas_stormdata<-mutate(sum_cas_stormdata,CASUALTIES_K=CASUALTIES/1000,TOTAL_FATALITIES_K=TOTAL_FATALITIES/1000,TOTAL_INJURIES_K=TOTAL_INJURIES/1000)
my_cas_barplot_data<-rbind(sum_cas_stormdata[1:10,]$TOTAL_FATALITIES_K,sum_cas_stormdata[1:10,]$TOTAL_INJURIES_K)
names(my_cas_barplot_data)<-as.character(sum_cas_stormdata[1:10,]$Event_Type)
##############################
sum_eco_stormdata<-summarize(grouped_stormdata,PROPERTY_LOSS=sum(PROPDMG*Exp_vector_T[PropDmgExp]),CROP_LOSS=sum(CROPDMG*Exp_vector_T[CropDmgExp]))
sum_eco_stormdata<-mutate(sum_eco_stormdata,PROPERTY_LOSS,CROP_LOSS,TOTAL_LOSS=PROPERTY_LOSS+CROP_LOSS)
sum_eco_stormdata<-arrange(sum_eco_stormdata,desc(TOTAL_LOSS))
my_eco_barplot_data<-rbind(sum_eco_stormdata[1:10,]$PROPERTY_LOSS,sum_eco_stormdata[1:10,]$CROP_LOSS)
names(my_eco_barplot_data)<-as.character(sum_eco_stormdata[1:10,]$Event_Type)
####RESULT#####
library(xtable)
report_cas_stormdata<-sum_cas_stormdata[1:10,]
xreport_cas<-xtable(select(report_cas_stormdata,Event_Type,CASUALTIES,TOTAL_FATALITIES,TOTAL_INJURIES))
print.xtable(xreport_cas,type = "html")
####
par(mar=c(10,5,5,5))
barplot(my_cas_barplot_data,col=c("red","forestgreen"),names.arg = as.character(sum_cas_stormdata[1:10,]$Event_Type),las=2,ylim=c(0,100),cex.names=0.8)
title(main = "Top 10 Most Hazardous Events in USA (by Total Casualty Numbers)",ylab = "Total Casualty Number (Thousand)")
legend("topright",c("Fatalities","Injuries"),fill=c("red","forestgreen"))
##############################
report_eco_stormdata<-sum_eco_stormdata[1:10,]
xreport_eco<-xtable(select(report_eco_stormdata,Event_Type,PROPERTY_LOSS,CROP_LOSS,TOTAL_LOSS))
print.xtable(xreport_eco,type = "html")
##############################
par(mar=c(10,5,5,5))
barplot(my_eco_barplot_data,col=c("purple","tomato"),names.arg = as.character(sum_eco_stormdata[1:10,]$Event_Type),las=2,ylim=c(0,60),cex.names=0.8)
title(main = "Top 10 Most Destructive Events in USA (by Total Economic Loss )",ylab = "Economic Loss (Triillion Dollars)")
legend("topright",c("Property loss","Crop loss"),fill=c("purple","tomato"))
