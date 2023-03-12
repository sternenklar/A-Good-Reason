rm(list=ls())
library(data.table)
library(ggplot2)
library(curl)
library(dplyr)
library(zoo)
setwd("/home/jmva/Dokumente/A Good Reason")

df_for_plots <- readRDS("data/df_for_plots")


#calculate CFR:

df_for_plots %>%
  filter(date=="2020-06-30") %>%
  mutate(deathrate=total_deaths_per_million/total_cases_per_million) %>%
  select(country, deathrate) %>%
  arrange(desc(deathrate))


df_for_plots %>%
  filter(date=="2021-06-30") %>%
  mutate(deathrate=total_deaths_per_million/total_cases_per_million) %>%
  select(country, deathrate) %>%
  arrange(desc(deathrate))

# scatter plot of cumulative excess mortality
no_soh_countries <- c("Belarus", "Croatia", "Denmark", "Finland", "Iceland", "Liechtenstein", "Norway", "Sweden", "Switzerland")
cumul_excess <- df_for_plots[date>="2021-06-27"&!is.na(excess_mortality_cumulative),][country %in% no_soh_countries, no_soh:=TRUE][is.na(no_soh), no_soh:=FALSE]

plot_mortality <- ggplot(cumul_excess)+
  geom_point(aes(total_deaths_per_million, excess_mortality_cumulative, colour=no_soh))+
  scale_colour_manual(values=c("black", "green"))+
  theme(legend.position="none")+
  labs(x="Total Covid-19 deaths per million", y="Cumulative excess mortality as percentage of expected mortality", caption="Data from 27 June (for 30 countries) / 30 June (for 12 countries) 2021,\n countries that never introduced any stay-at-home restrictions in green")
ggsave("website/plot_excess_mortality.png")

cumul_excess_lookup <- cumul_excess %>%
  arrange(desc(excess_mortality_cumulative))

# look up last available data for Belarus
#owid_data[!is.na(excess_mortality_cumulative)&country=="Belarus"]

#summarize data per category

plot_data_deaths <- df_for_plots %>%
  group_by(date, restrictions) %>%
  filter(!is.na(new_deaths)&!is.na(population)) %>%
  mutate(deaths_per_mil_by_category=sum(new_deaths)/sum(population)*1000000) %>%
  select(date, restrictions, deaths_per_mil_by_category) %>%
  distinct() %>%
  arrange(date, restrictions) %>%
  group_by(restrictions) %>%
  mutate(deaths_per_mil_by_category_7dayavg=rollmean(deaths_per_mil_by_category, k=7, fill=NA))

  
plot_data_hosp <- df_for_plots %>%
  group_by(date, restrictions) %>%
  filter(!is.na(hosp_patients)&!is.na(population)) %>%
  mutate(hosp_per_mil_by_category=sum(hosp_patients)/sum(population)*1000000) %>%
  select(date, restrictions, hosp_per_mil_by_category) %>%
  distinct() %>%
  arrange(date, restrictions)

plot_data_icu <- df_for_plots %>%
  group_by(date, restrictions) %>%
  filter(!is.na(icu_patients)&!is.na(population)) %>%
  mutate(icu_per_mil_by_category=sum(icu_patients)/sum(population)*1000000) %>%
  select(date, restrictions, icu_per_mil_by_category) %>%
  distinct() %>%
  arrange(date, restrictions)

plot_data_deaths <-data.table(plot_data_deaths)
plot_data_hosp <- data.table(plot_data_hosp)
plot_data_icu <- data.table(plot_data_icu)

plot_data_deaths$restrictions <- factor(plot_data_deaths$restrictions, levels=c("no stay-at-home restrictions", "night curfews at least locally", "some daytime stay-at-home order at least locally", "stay-at-home order with restrictions on walks in entire country"))
plot_deaths <- ggplot(plot_data_deaths)+
  geom_line(aes(date, deaths_per_mil_by_category, colour=restrictions))+
  scale_colour_manual(values=c("dark green", "yellow", "red", "dark red"))+
  labs(title="Covid-19 deaths per million",x=NULL, y=NULL)+
  scale_x_date(date_breaks="1 month", date_labels="%b")+
  coord_cartesian(ylim=c(0, 15))+
  geom_vline(xintercept=as.numeric(as.Date("2020-09-01")))+
  theme(legend.position="bottom")+
  geom_label(aes(label="Each country is classified in a category for the time until \n August 2020 and in one for the time thereafter", x=as.Date("2020-06-25"), y=10), size=3.5)
ggsave("website/plot_deaths.png", width=15, height=4, unit="in")

#same with 7 day rolling avg

plot_deaths_7dayavg <- ggplot(plot_data_deaths)+
  geom_line(aes(date, deaths_per_mil_by_category_7dayavg, colour=restrictions))+
  scale_colour_manual(values=c("dark green", "yellow", "red", "dark red"))+
  labs(title="Covid-19 deaths per million, 7 day average",x=NULL, y=NULL)+
  scale_x_date(date_breaks="1 month", date_labels="%b")+
  coord_cartesian(ylim=c(0, 15))+
  geom_vline(xintercept=as.numeric(as.Date("2020-09-01")))+
  theme(legend.position="bottom")+
  geom_label(aes(label="Each country is classified in a category for the time until \n August 2020 and in one for the time thereafter", x=as.Date("2020-06-25"), y=10), size=3.5)
ggsave("website/plot_deaths_7dayavg.png", width=15, height=4, unit="in")


#as a robustness check let's filter out Russia

plot_data_deaths_without_russia <- df_for_plots %>%
  group_by(date, restrictions) %>%
  filter(!is.na(new_deaths)&!is.na(population)&country!="Russia") %>%
  mutate(deaths_per_mil_by_category=sum(new_deaths)/sum(population)*1000000) %>%
  select(date, restrictions, deaths_per_mil_by_category) %>%
  distinct() %>%
  arrange(date, restrictions) %>%
  group_by(restrictions) %>%
  mutate(deaths_per_mil_by_category_7dayavg=rollmean(deaths_per_mil_by_category, k=7, fill=NA))
  


plot_data_deaths_without_russia$restrictions <- factor(plot_data_deaths_without_russia$restrictions, levels=c("no stay-at-home restrictions", "night curfews at least locally", "some daytime stay-at-home order at least locally", "stay-at-home order with restrictions on walks in entire country"))

plot_deaths_without_russia <- ggplot(plot_data_deaths_without_russia)+
  geom_line(aes(date, deaths_per_mil_by_category, colour=restrictions))+
  scale_colour_manual(values=c("dark green", "yellow", "red", "dark red"))+
  labs(title="Covid-19 deaths per million",x=NULL, y=NULL)+
  scale_x_date(date_breaks="1 month", date_labels="%b")+
  coord_cartesian(ylim=c(0, 15))+
  geom_vline(xintercept=as.numeric(as.Date("2020-09-01")))+
  theme(legend.position="bottom")+
  geom_label(aes(label="Each country is classified in a category for the time until \n August 2020 and in one for the time thereafter", x=as.Date("2020-06-25"), y=10), size=3.5)
ggsave("website/plot_deaths_without_russia.png", width=15, height=4, unit="in")

plot_deaths_7dayavg_without_russia <- ggplot(plot_data_deaths_without_russia)+
  geom_line(aes(date, deaths_per_mil_by_category_7dayavg, colour=restrictions))+
  scale_colour_manual(values=c("dark green", "yellow", "red", "dark red"))+
  labs(title="Covid-19 deaths per million, 7 day average",x=NULL, y=NULL)+
  scale_x_date(date_breaks="1 month", date_labels="%b")+
  coord_cartesian(ylim=c(0, 15))+
  geom_vline(xintercept=as.numeric(as.Date("2020-09-01")))+
  theme(legend.position="bottom")+
  geom_label(aes(label="Each country is classified in a category for the time until \n August 2020 and in one for the time thereafter", x=as.Date("2020-06-25"), y=10), size=3.5)
ggsave("website/plot_deaths_7dayavg_without_russia.png", width=15, height=4, unit="in")



#there is no data for hospital and ICU patients in Russia, so above distinction is not necessary here:

plot_data_hosp$restrictions <- factor(plot_data_hosp$restrictions, levels=c("no stay-at-home restrictions", "night curfews at least locally", "some daytime stay-at-home order at least locally", "stay-at-home order with restrictions on walks in entire country"))
plot_data_icu$restrictions <- factor(plot_data_icu$restrictions, levels=c("no stay-at-home restrictions", "night curfews at least locally", "some daytime stay-at-home order at least locally", "stay-at-home order with restrictions on walks in entire country"))


plot_hosp <- ggplot(plot_data_hosp)+
  geom_line(aes(date, hosp_per_mil_by_category, colour=restrictions))+
  scale_colour_manual(values=c("dark green", "yellow", "red", "dark red"))+  theme(legend.position="bottom")+
  labs(title="Covid-19 hospital patients per million",x=NULL, y=NULL)+
  scale_x_date(date_breaks="1 month", date_labels="%b")+
  geom_vline(xintercept=as.numeric(as.Date("2020-09-01")))+
  theme(legend.position="bottom")+
  geom_label(aes(label="Each country is classified in a category for the time until \n August 2020 and in one for the time thereafter", x=as.Date("2020-07-01"), y=450), size=3.5)
ggsave("website/plot_hospital.png", width=15, height=4, unit="in")

plot_icu <- ggplot(plot_data_icu)+
  geom_line(aes(date, icu_per_mil_by_category, colour=restrictions))+
  scale_colour_manual(values=c("dark green", "yellow", "red", "dark red"))+  theme(legend.position="bottom")+
  labs(title="Covid-19 ICU patients per million",x=NULL, y=NULL)+
  scale_x_date(date_breaks="1 month", date_labels="%b")+
  geom_vline(xintercept=as.numeric(as.Date("2020-09-01")))+
  theme(legend.position="bottom")+
  geom_label(aes(label="Each country is classified in a category for the time until \n August 2020 and in one for the time thereafter", x=as.Date("2020-06-25"), y=75), size=3.5)
ggsave("website/plot_icu.png", width=15, height=4, unit="in")

#another robustness check: let's assign Poland to a different category for the second wave:

df_for_plots_alt <- df_for_plots[date>="2020-09-01"&country=="Poland", restrictions:="no stay-at-home restrictions"]


plot_data_deaths_alt <- df_for_plots_alt %>%
  group_by(date, restrictions) %>%
  filter(!is.na(new_deaths)&!is.na(population)) %>%
  mutate(deaths_per_mil_by_category=sum(new_deaths)/sum(population)*1000000) %>%
  select(date, restrictions, deaths_per_mil_by_category) %>%
  distinct() %>%
  arrange(date, restrictions) %>%
  group_by(restrictions) %>%
  mutate(deaths_per_mil_by_category_7dayavg=rollmean(deaths_per_mil_by_category, k=7, fill=NA))



plot_data_hosp_alt <- df_for_plots_alt %>%
  group_by(date, restrictions) %>%
  filter(!is.na(hosp_patients)&!is.na(population)) %>%
  mutate(hosp_per_mil_by_category=sum(hosp_patients)/sum(population)*1000000) %>%
  select(date, restrictions, hosp_per_mil_by_category) %>%
  distinct() %>%
  arrange(date, restrictions)

plot_data_icu_alt <- df_for_plots_alt %>%
  group_by(date, restrictions) %>%
  filter(!is.na(icu_patients)&!is.na(population)) %>%
  mutate(icu_per_mil_by_category=sum(icu_patients)/sum(population)*1000000) %>%
  select(date, restrictions, icu_per_mil_by_category) %>%
  distinct() %>%
  arrange(date, restrictions)

plot_data_deaths_alt <-data.table(plot_data_deaths_alt)
plot_data_hosp_alt <- data.table(plot_data_hosp_alt)
plot_data_icu_alt <- data.table(plot_data_icu_alt)

plot_data_deaths_alt$restrictions <- factor(plot_data_deaths_alt$restrictions, levels=c("no stay-at-home restrictions", "night curfews at least locally", "some daytime stay-at-home order at least locally", "stay-at-home order with restrictions on walks in entire country"))
plot_deaths_alt <- ggplot(plot_data_deaths_alt)+
  geom_line(aes(date, deaths_per_mil_by_category, colour=restrictions))+
  scale_colour_manual(values=c("dark green", "yellow", "red", "dark red"))+
  labs(title="Covid-19 deaths per million",x=NULL, y=NULL)+
  scale_x_date(date_breaks="1 month", date_labels="%b")+
  coord_cartesian(ylim=c(0, 15))+
  geom_vline(xintercept=as.numeric(as.Date("2020-09-01")))+
  theme(legend.position="bottom")+
  geom_label(aes(label="Each country is classified in a category for the time until \n August 2020 and in one for the time thereafter", x=as.Date("2020-06-25"), y=10), size=3.5)
ggsave("website/plot_deaths_alt.png", width=15, height=4, unit="in")

#same with 7 day rolling avg

plot_deaths_7dayavg_alt <- ggplot(plot_data_deaths_alt)+
  geom_line(aes(date, deaths_per_mil_by_category_7dayavg, colour=restrictions))+
  scale_colour_manual(values=c("dark green", "yellow", "red", "dark red"))+
  labs(title="Covid-19 deaths per million, 7 day average",x=NULL, y=NULL)+
  scale_x_date(date_breaks="1 month", date_labels="%b")+
  coord_cartesian(ylim=c(0, 15))+
  geom_vline(xintercept=as.numeric(as.Date("2020-09-01")))+
  theme(legend.position="bottom")+
  geom_label(aes(label="Each country is classified in a category for the time until \n August 2020 and in one for the time thereafter", x=as.Date("2020-06-25"), y=10), size=3.5)
ggsave("website/plot_deaths_7dayavg_alt.png", width=15, height=4, unit="in")


#as a robustness check let's filter out Russia


plot_data_deaths_without_russia_alt <- df_for_plots_alt %>%
  group_by(date, restrictions) %>%
  filter(!is.na(new_deaths)&!is.na(population)&country!="Russia") %>%
  mutate(deaths_per_mil_by_category=sum(new_deaths)/sum(population)*1000000) %>%
  select(date, restrictions, deaths_per_mil_by_category) %>%
  distinct() %>%
  arrange(date, restrictions) %>%
  group_by(restrictions) %>%
  mutate(deaths_per_mil_by_category_7dayavg=rollmean(deaths_per_mil_by_category, k=7, fill=NA))
  


plot_deaths_without_russia_alt <- ggplot(plot_data_deaths_without_russia_alt)+
  geom_line(aes(date, deaths_per_mil_by_category, colour=restrictions))+
  scale_colour_manual(values=c("dark green", "yellow", "red", "dark red"))+
  labs(title="Covid-19 deaths per million",x=NULL, y=NULL)+
  scale_x_date(date_breaks="1 month", date_labels="%b")+
  coord_cartesian(ylim=c(0, 15))+
  geom_vline(xintercept=as.numeric(as.Date("2020-09-01")))+
  theme(legend.position="bottom")+
  geom_label(aes(label="Each country is classified in a category for the time until \n August 2020 and in one for the time thereafter", x=as.Date("2020-06-25"), y=10), size=3.5)
ggsave("website/plot_deaths_without_russia_alt.png", width=15, height=4, unit="in")

plot_data_deaths_without_russia_alt$restrictions <- factor(plot_data_deaths_without_russia_alt$restrictions, levels=c("no stay-at-home restrictions", "night curfews at least locally", "some daytime stay-at-home order at least locally", "stay-at-home order with restrictions on walks in entire country"))

plot_deaths_7dayavg_without_russia_alt <- ggplot(plot_data_deaths_without_russia_alt)+
  geom_line(aes(date, deaths_per_mil_by_category_7dayavg, colour=restrictions))+
  scale_colour_manual(values=c("dark green", "yellow", "red", "dark red"))+
  labs(title="Covid-19 deaths per million, 7 day average",x=NULL, y=NULL)+
  scale_x_date(date_breaks="1 month", date_labels="%b")+
  coord_cartesian(ylim=c(0, 15))+
  geom_vline(xintercept=as.numeric(as.Date("2020-09-01")))+
  theme(legend.position="bottom")+
  geom_label(aes(label="Each country is classified in a category for the time until \n August 2020 and in one for the time thereafter", x=as.Date("2020-06-25"), y=10), size=3.5)
ggsave("website/plot_deaths_7dayavg_without_russia_alt.png", width=15, height=4, unit="in")



#there is no data for hospital and ICU patients in Russia, so above distinction is not necessary here:

plot_data_hosp_alt$restrictions <- factor(plot_data_hosp_alt$restrictions, levels=c("no stay-at-home restrictions", "night curfews at least locally", "some daytime stay-at-home order at least locally", "stay-at-home order with restrictions on walks in entire country"))
plot_data_icu_alt$restrictions <- factor(plot_data_icu_alt$restrictions, levels=c("no stay-at-home restrictions", "night curfews at least locally", "some daytime stay-at-home order at least locally", "stay-at-home order with restrictions on walks in entire country"))


plot_hosp_alt <- ggplot(plot_data_hosp_alt)+
  geom_line(aes(date, hosp_per_mil_by_category, colour=restrictions))+
  scale_colour_manual(values=c("dark green", "yellow", "red", "dark red"))+  theme(legend.position="bottom")+
  labs(title="Covid-19 hospital patients per million",x=NULL, y=NULL)+
  scale_x_date(date_breaks="1 month", date_labels="%b")+
  geom_vline(xintercept=as.numeric(as.Date("2020-09-01")))+
  theme(legend.position="bottom")+
  geom_label(aes(label="Each country is classified in a category for the time until \n August 2020 and in one for the time thereafter", x=as.Date("2020-07-01"), y=450), size=3.5)
ggsave("website/plot_hospital_alt.png", width=15, height=4, unit="in")

