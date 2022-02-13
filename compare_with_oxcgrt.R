rm(list=ls())
library(data.table)
library(curl)
df_agr <- fread("")
df_oxcgrt <- fread("https://github.com/OxCGRT/covid-policy-tracker/blob/master/data/OxCGRT_latest.csv?raw=true", select=c("Date", "CountryName", "RegionName", "C6_Stay at home requirements"))
df_oxcgrt<-df_oxcgrt[(df_oxcgrt$RegionName==""),][,RegionName:=NULL] #necessary to delete all sub-national data to have distinct data points for each country and day
df_oxcgrt$Date=as.Date(as.character(df_oxcgrt$Date), format="%Y%m%d")
setnames(df_oxcgrt, c("Date", "CountryName", "C6_Stay at home requirements"), c("date", "country", "C6_Stay_at_home_requirements"))
df_agr <- df_oxcgrt[df_agr, , on=c("country", "date")]
df_agr <- df_agr[(C6_Stay_at_home_requirements %in% c(2,3) & stayhome_day=="no" & stayhome_night=="no") | (C6_Stay_at_home_requirements %in% c(0,1) & (stayhome_day!="no" | stayhome_night!="no")), difference_with_oxcgrt:="yes"][is.na(difference_with_oxcgrt), difference_with_oxcgrt:="no"]
df_agr[,C6_Stay_at_home_requirements:=NULL]