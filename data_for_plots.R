rm(list=ls())
library(data.table)
library(ggplot2)
library(curl)
library(dplyr)
library(zoo)
setwd("/home/jmva/Dokumente/A Good Reason")
df <- data.table(fread("https://raw.githubusercontent.com/sternenklar/A-Good-Reason/main/complete%20data.csv"))

values <- c("no stay-at-home restrictions", "only outside masks locally / for some", "only outside masks in entire country", "night curfew locally / for some", "night curfew in entire country", "stay-at-home order locally / for some", "stay-at-home order in entire country, walks allowed", "stay-at-home order in entire country, walks restricted locally for some", "stay-at-home order in entire country, walks restricted in entire country", "stay-at-home order, no walks allowed in entire country")
values_simplified <-c("no stay-at-home restrictions", "night curfews at least locally", "some daytime stay-at-home order at least locally", "stay-at-home order with restrictions on walks in entire country")
countries <- unique(df$country)
strictest_1st <- c(rep(values[1], length(countries)))
strictest_2nd <- c(rep(values_simplified[1], length(countries)))
strictest_1st_simplified <- c(rep(values_simplified[1], length(countries)))
strictest_2nd_simplified <- c(rep(values_simplified[1], length(countries)))
strictest_by_wave <- data.table(countries, strictest_1st, strictest_2nd, strictest_1st_simplified, strictest_2nd_simplified)


strictest_1st <- for (icountry in strictest_by_wave$countries) 
{
  for (idate in c(as.Date("2020-01-01"):as.Date("2020-08-31")))
  {
    if (!strictest_by_wave[countries==icountry]$strictest_1st %chin% values[3:10]
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$outside_masks!="no")
    {
      strictest_by_wave[countries==icountry]$strictest_1st<-values[2]
    }
    if (!strictest_by_wave[countries==icountry]$strictest_1st %chin% values[4:10]
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$outside_masks=="yes")
    {
      strictest_by_wave[countries==icountry]$strictest_1st<-values[3]
    }
    if (!strictest_by_wave[countries==icountry]$strictest_1st %chin% values[5:10]
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$stayhome_day=="no"
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$stayhome_night!="no")
    {
      strictest_by_wave[countries==icountry]$strictest_1st<-values[4]
    }
    if (!strictest_by_wave[countries==icountry]$strictest_1st %chin% values[6:10]
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$stayhome_day=="no"
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$stayhome_night=="yes")
    {
      strictest_by_wave[countries==icountry]$strictest_1st<-values[5]
    }
    if (!strictest_by_wave[countries==icountry]$strictest_1st %chin% values[7:10]
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$stayhome_day!="no")
    {
      strictest_by_wave[countries==icountry]$strictest_1st<-values[6]
    } 
    if (!strictest_by_wave[countries==icountry]$strictest_1st %chin% values[8:10]
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$stayhome_day=="yes"
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$walk_day=="yes")
    {
      strictest_by_wave[countries==icountry]$strictest_1st<-values[7]
    } 
    if (!strictest_by_wave[countries==icountry]$strictest_1st %chin% values[9:10]
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$stayhome_day=="yes"
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$walk_day!="yes")
    {
      strictest_by_wave[countries==icountry]$strictest_1st<-values[8]
    } 
    if (!strictest_by_wave[countries==icountry]$strictest_1st %chin% values[10]
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$stayhome_day=="yes"
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$walk_day=="no")
    {
      strictest_by_wave[countries==icountry]$strictest_1st<-values[9]
    } 
    if (df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$stayhome_day=="yes"
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$least_strict_day %chin% c("6", "7"))
    {
      strictest_by_wave[countries==icountry]$strictest_1st<-values[10]
    }
  }
}

strictest_by_wave <- strictest_by_wave[strictest_1st %chin% values[4:5], strictest_1st_simplified:=values_simplified[2]]
strictest_by_wave <- strictest_by_wave[strictest_1st %chin% values[6:8], strictest_1st_simplified:=values_simplified[3]]
strictest_by_wave <- strictest_by_wave[strictest_1st %chin% values[9:10], strictest_1st_simplified:=values_simplified[4]]



strictest_2nd <- for (icountry in strictest_by_wave$countries) 
{
  for (idate in c(as.Date("2020-09-01"):as.Date("2021-06-30")))
  {
    if (!strictest_by_wave[countries==icountry]$strictest_2nd %chin% values[3:10]
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$outside_masks!="no")
    {
      strictest_by_wave[countries==icountry]$strictest_2nd<-values[2]
    }
    if (!strictest_by_wave[countries==icountry]$strictest_2nd %chin% values[4:10]
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$outside_masks=="yes")
    {
      strictest_by_wave[countries==icountry]$strictest_2nd<-values[3]
    }
    if (!strictest_by_wave[countries==icountry]$strictest_2nd %chin% values[5:10]
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$stayhome_day=="no"
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$stayhome_night!="no")
    {
      strictest_by_wave[countries==icountry]$strictest_2nd<-values[4]
    }
    if (!strictest_by_wave[countries==icountry]$strictest_2nd %chin% values[6:10]
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$stayhome_day=="no"
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$stayhome_night=="yes")
    {
      strictest_by_wave[countries==icountry]$strictest_2nd<-values[5]
    }
    if (!strictest_by_wave[countries==icountry]$strictest_2nd %chin% values[7:10]
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$stayhome_day!="no")
    {
      strictest_by_wave[countries==icountry]$strictest_2nd<-values[6]
    } 
    if (!strictest_by_wave[countries==icountry]$strictest_2nd %chin% values[8:10]
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$stayhome_day=="yes"
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$walk_day=="yes")
    {
      strictest_by_wave[countries==icountry]$strictest_2nd<-values[7]
    } 
    if (!strictest_by_wave[countries==icountry]$strictest_2nd %chin% values[9:10]
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$stayhome_day=="yes"
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$walk_day!="yes")
    {
      strictest_by_wave[countries==icountry]$strictest_2nd<-values[8]
    } 
    if (!strictest_by_wave[countries==icountry]$strictest_2nd %chin% values[10]
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$stayhome_day=="yes"
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$walk_day=="no")
    {
      strictest_by_wave[countries==icountry]$strictest_2nd<-values[9]
    } 
    if (df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$stayhome_day=="yes"
        &df[country==icountry&date==as.Date(idate, origin="1970-01-01")]$least_strict_day %chin% c("6", "7"))
    {
      strictest_by_wave[countries==icountry]$strictest_2nd<-values[10]
    }
  }
}

strictest_by_wave <- strictest_by_wave[strictest_2nd %chin% values[4:5], strictest_2nd_simplified:=values_simplified[2]]
strictest_by_wave <- strictest_by_wave[strictest_2nd %chin% values[6:8], strictest_2nd_simplified:=values_simplified[3]]
strictest_by_wave <- strictest_by_wave[strictest_2nd %chin% values[9:10], strictest_2nd_simplified:=values_simplified[4]]


owid_data <- fread("https://covid.ourworldindata.org/data/owid-covid-data.csv", select=c("location", "date", "population", "total_cases_per_million", "new_cases_smoothed_per_million", "total_deaths_per_million", "new_deaths_smoothed_per_million", "excess_mortality_cumulative", "hosp_patients_per_million", "icu_patients_per_million", "new_deaths", "icu_patients", "hosp_patients"))
#check which countries can be found in owid data to avoid differences in naming
owid_data[strictest_by_wave$countries, on=.(location), .N, by=.EACHI]
owid_data[location=="Czechia", location:="Czech Republic"]
owid_data[location=="Slovakia", location:="Slovak Republic"]
setnames(owid_data, "location", "country")
setnames(strictest_by_wave, "countries", "country")

df_for_plots <- data.table(owid_data[df, , on=c("country", "date")])

df_for_plots <- df_for_plots[strictest_by_wave, on=.(country), restrictions_1st:=i.strictest_1st_simplified]
df_for_plots <- df_for_plots[strictest_by_wave, on=.(country), restrictions_2nd:=i.strictest_2nd_simplified]

df_for_plots <- df_for_plots[date<"2020-09-01", restrictions:=restrictions_1st]
df_for_plots <- df_for_plots[date>="2020-09-01", restrictions:=restrictions_2nd]

saveRDS(df_for_plots, "data/df_for_plots")