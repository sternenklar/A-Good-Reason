rm(list=ls())
library(data.table)
library(curl)
library(readr)
df_agr <- fread("https://raw.githubusercontent.com/sternenklar/A-Good-Reason/main/Data/complete%20data.csv")

df_oxcgrt <- fread("https://github.com/OxCGRT/covid-policy-tracker/blob/master/data/OxCGRT_nat_latest.csv?raw=true", select=c("Date", "CountryName", "C6M_Stay at home requirements"))

df_agr$date=as.Date(df_agr$date, format="%Y%m%d")
df_oxcgrt$Date=as.Date(as.character(df_oxcgrt$Date), format="%Y%m%d")
setnames(df_oxcgrt, c("Date", "CountryName", "C6M_Stay at home requirements"), c("date", "country", "C6_Stay_at_home_requirements"))
df_agr <- df_oxcgrt[df_agr, , on=c("country", "date")]
df_agr <- df_agr[(C6_Stay_at_home_requirements %in% c(2,3) & stayhome_day=="no" & stayhome_night=="no"), difference_with_oxcgrt:="SAHO in OxCGRT only"]
df_agr <- df_agr[(C6_Stay_at_home_requirements %in% c(0,1) & (stayhome_day!="no" | stayhome_night!="no")), difference_with_oxcgrt:="SAHO in AGR only"]
df_agr <- df_agr[is.na(difference_with_oxcgrt), difference_with_oxcgrt:="no difference"]

df_agr[,C6_Stay_at_home_requirements:=NULL]

write_csv(df_agr, "C:/Users/Paul/Documents/Research/A Good Reason/data/AGR data/complete_data_compared.csv")