library(tidyverse)
library(ggthemes)
df_owid <- read.csv("masken/owid-covid-data.csv")
df_nuts3 <- read.csv("EUROPE_COVID19_master.csv", sep=";")
df_owid$date <- as.Date(df_owid$date)
df_nuts3$date <- as.Date(substring(df_nuts3$date, 1, nchar(df_nuts3$date)-2), format="%d-%b-%y")

df_owid <- df_owid %>%
  select(location, date, new_cases_smoothed_per_million) %>%
  filter(date>"2022-01-01")
df_nuts3 <- df_nuts3 %>%
  filter(date>"2022-01-01")

#define dates
end_mandate_shopsschools_de <- as.Date("2022-04-03") #zugleich Abschaffung 3G
end_mandate_shopsschools_mv <- as.Date("2022-04-22")
end_mandate_shopsschools_hh <- as.Date("2022-04-30")
end_mandate_dk <- as.Date("2022-02-01")
end_mandate_shops_nl <- as.Date("2022-02-25") #zugleich Abschaffung 3G
end_mandate_transport_nl <- as.Date("2022-03-23")
end_mandate_shopsschools_be <- as.Date("2022-03-07") #zugleich Abschaffung 3G
end_mandate_transport_be <- as.Date("2022-05-23")
end_mandate_shops_lu <- as.Date("2022-03-11")
end_mandate_transport_lu <- as.Date("2022-06-14")
end_mandate_shopsschools_fr <- as.Date("2022-03-14") #together with end of vaccine pass
end_mandate_transport_fr <- as.Date("2022-05-16")
end_mandate_shopsschools_ch <- as.Date("2022-02-17")
end_mandate_transporteverywhere_ch <- as.Date("2022-04-01")
end_mandate_schoolsclassroom_at = as.Date("2022-02-21")
end_mandate_schools_at = as.Date("2022-04-25")
end_mandate_shopstransport_at = as.Date("2022-06-01") #suspended for 3 months, exception: Vienna
end_mandate_shops_cz = as.Date("2022-03-14")
end_mandate_transport_cz <- as.Date("2022-04-14")
end_mandate_healthcare_cz <- as.Date("2022-05-05")
end_mandate_shopstransport_pl <- as.Date("2022-03-28")


#within Germany

df_de <- df_nuts3 %>%
  filter(country=="Germany") %>%
  mutate(bundesland = case_when(
    !substr(nuts3_id, 1, 4) %in% c("DE60", "DE80") ~ "andere Länder",
    nuts3_id=="DE600" ~ "Hamburg",
    substr(nuts3_id,1,4)=="DE80" ~ "Mecklenburg-Vorpommern"))
df_de <- df_de %>%
  group_by(bundesland, date) %>%
  summarize(cases_daily_pop_land=sum(cases_daily)/sum(population)) %>%
  ungroup() %>%
  group_by(bundesland) %>%
  mutate(cases_daily_pop_7day = zoo::rollmean(cases_daily_pop_land, 7, fill=NA))

#plot within Germany
ggplot(df_de, aes(date, cases_daily_pop_7day, color=bundesland))+
  geom_line()+
  theme_tufte()+
  labs(x="",y="7-Tage-Inzidenz (1.1.-24.6.)")+
  scale_color_manual(values=c("black", "red", "orange"))+
  geom_vline(xintercept=end_mandate_shopsschools_de, linetype="dashed")+
  geom_vline(xintercept=end_mandate_shopsschools_hh, colour="red", linetype="dashed")+
  geom_vline(xintercept=end_mandate_shopsschools_mv, colour="orange", linetype="dashed")+
  geom_text(aes(x=end_mandate_shopsschools_de, y=0, label="3. April: 14 Bundesländer"), colour="black", angle=90, vjust=-0.5, hjust=0.2, size=2.5)+
  geom_text(aes(x=end_mandate_shopsschools_mv, y=0, label="22. April: Mecklenburg-Vorpom."), colour="orange", angle=90, vjust=-0.5, hjust=0.16, size=2.5)+
  geom_text(aes(x=end_mandate_shopsschools_hh, y=0, label="30. April: Hamburg"), colour="red", angle=90, vjust=-0.5, hjust=0.25, size=2.5)+
  geom_text(aes(x=as.Date("2022-03-15"), y=0, label="Ende der Maskenpflicht:"), colour="black", size=3)+
  theme(legend.position=c(x=0.3, y=0.9), legend.title=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#Denmark

df_dk <- df_owid %>%
  filter(location %in% c("Germany", "Denmark")&!is.na(new_cases_smoothed_per_million))

ggplot(df_dk, aes(date, new_cases_smoothed_per_million, color=location))+
  geom_line()+
  theme_tufte()+
  labs(x="",y="7-Tage-Inzidenz (1.1.-6.8.)")+
  scale_color_manual(values=c("red", "black"))+
  geom_vline(xintercept=end_mandate_shopsschools_de, linetype="dashed")+
  geom_vline(xintercept=end_mandate_dk, colour="red", linetype="dashed")+
  geom_text(aes(x=end_mandate_dk, y=0, label="1. Februar"), colour="red", angle=90, vjust=-0.5, hjust=0, size=2.4)+
  geom_text(aes(x=end_mandate_shopsschools_de, y=0, label="3. April"), colour="black", angle=90, vjust=-0.5, hjust=0, size=2.4)+
  theme(legend.position=c(x=0.33, y=0.9), legend.title=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#Netherlands
df_nl <- df_owid %>%
  filter(location %in% c("Germany", "Netherlands")&!is.na(new_cases_smoothed_per_million))

ggplot(df_nl, aes(date, new_cases_smoothed_per_million, color=location))+
  geom_line()+
  theme_tufte()+
  labs(x="",y="7-Tage-Inzidenz (1.1.-6.8.)")+
  scale_color_manual(values=c("black", "orange"))+
  geom_vline(xintercept=end_mandate_shopsschools_de, linetype="dashed")+
  geom_vline(xintercept=end_mandate_transport_nl, colour="orange", linetype="dashed")+
  geom_vline(xintercept=end_mandate_shops_nl, colour="orange", linetype="dashed")+
  geom_text(aes(x=end_mandate_transport_nl, y=0, label="23. März"), colour="orange", angle=90, vjust=-0.5, hjust=-0.7, size=2.4)+
  geom_text(aes(x=end_mandate_shops_nl, y=0, label="25. Februar"), colour="orange", angle=90, vjust=-0.5, hjust=-0.7, size=2.4)+
  geom_text(aes(x=end_mandate_shopsschools_de, y=0, label="3. April"), colour="black", angle=90, vjust=-0.5, hjust=-0.7, size=2.4)+
  theme(legend.position=c(x=0.33, y=0.92), legend.title=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#Belgium
df_be <- df_owid %>%
  filter(location %in% c("Germany", "Belgium")&!is.na(new_cases_smoothed_per_million))

ggplot(df_be, aes(date, new_cases_smoothed_per_million, color=location))+
  geom_line()+
  theme_tufte()+
  labs(x="",y="7-Tage-Inzidenz (1.1.-6.8.)")+
  scale_color_manual(values=c("red", "black"))+
  geom_vline(xintercept=end_mandate_shopsschools_de, linetype="dashed")+
  geom_vline(xintercept=end_mandate_shopsschools_be, colour="red", linetype="dashed")+
  geom_vline(xintercept=end_mandate_transport_be, colour="red", linetype="dashed")+
  geom_text(aes(x=end_mandate_shopsschools_be, y=0, label="7. März"), colour="red", angle=90, vjust=-0.5, hjust=0, size=2.4)+
  geom_text(aes(x=end_mandate_transport_be, y=0, label="23. Mai"), colour="red", angle=90, vjust=-0.5, hjust=0.5, size=2.4)+
  geom_text(aes(x=end_mandate_shopsschools_de, y=0, label="3. April"), colour="black", angle=90, vjust=-0.5, hjust=0, size=2.4)+
  theme(legend.position=c(x=0.33, y=0.9), legend.title=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#Luxembourg
df_lu <- df_owid %>%
  filter(location %in% c("Germany", "Luxembourg")&!is.na(new_cases_smoothed_per_million))


ggplot(df_lu, aes(date, new_cases_smoothed_per_million, color=location))+
  geom_line()+
  theme_tufte()+
  labs(x="",y="7-Tage-Inzidenz (1.1.-6.8.)")+
  scale_color_manual(values=c("black", "red"))+
  geom_vline(xintercept=end_mandate_shopsschools_de, linetype="dashed")+
  geom_vline(xintercept=end_mandate_shops_lu, colour="red", linetype="dashed")+
  geom_vline(xintercept=end_mandate_transport_lu, colour="red", linetype="dashed")+
  geom_text(aes(x=end_mandate_shops_lu, y=0, label="11. März"), colour="red", angle=90, vjust=-0.5, hjust=0, size=2.4)+
  geom_text(aes(x=end_mandate_transport_lu, y=0, label="14. Juni"), colour="red", angle=90, vjust=-0.5, hjust=0.5, size=2.4)+
  geom_text(aes(x=end_mandate_shopsschools_de, y=0, label="3. April"), colour="black", angle=90, vjust=-0.5, hjust=0, size=2.4)+
  theme(legend.position=c(x=0.33, y=0.9), legend.title=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())


#France
df_fr <- df_owid %>%
  filter(location %in% c("Germany", "France")&!is.na(new_cases_smoothed_per_million))

ggplot(df_fr, aes(date, new_cases_smoothed_per_million, color=location))+
  geom_line()+
  theme_tufte()+
  labs(x="",y="7-Tage-Inzidenz (1.1.-6.8.)")+
  scale_color_manual(values=c("blue", "black"))+
  geom_vline(xintercept=end_mandate_shopsschools_de, linetype="dashed")+
  geom_vline(xintercept=end_mandate_shopsschools_fr, colour="blue", linetype="dashed")+
  geom_vline(xintercept=end_mandate_transport_fr, colour="blue", linetype="dashed")+
  geom_text(aes(x=end_mandate_shopsschools_fr, y=0, label="14. März"), colour="blue", angle=90, vjust=-0.5, hjust=-0.7, size=2.4)+
  geom_text(aes(x=end_mandate_transport_fr, y=0, label="16. Mai"), colour="blue", angle=90, vjust=-0.5, hjust=-0.7, size=2.4)+
  geom_text(aes(x=end_mandate_shopsschools_de, y=0, label="3. April"), colour="black", angle=90, vjust=-0.5, hjust=-0.7, size=2.4)+
  theme(legend.position=c(x=0.33, y=0.9), legend.title=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#Switzerland
df_ch <- df_owid %>%
  filter(location %in% c("Germany", "Switzerland")&!is.na(new_cases_smoothed_per_million))

ggplot(df_ch, aes(date, new_cases_smoothed_per_million, color=location))+
  geom_line()+
  theme_tufte()+
  labs(x="",y="7-Tage-Inzidenz (1.1.-6.8.)")+
  scale_color_manual(values=c("black", "red"))+
  geom_vline(xintercept=end_mandate_shopsschools_de, linetype="dashed")+
  geom_vline(xintercept=end_mandate_shopsschools_ch, colour="red", linetype="dashed")+
  geom_vline(xintercept=end_mandate_transporteverywhere_ch, colour="red", linetype="dashed")+
  geom_text(aes(x=end_mandate_shopsschools_ch, y=0, label="17. Februar"), colour="red", angle=90, vjust=-0.5, hjust=0, size=2.4)+
  geom_text(aes(x=end_mandate_transporteverywhere_ch, y=0, label="1. April"), colour="red", angle=90, vjust=-0.8, hjust=0, size=2.4)+
  geom_text(aes(x=end_mandate_shopsschools_de, y=0, label="3. April"), colour="black", angle=90, vjust=-0.3, hjust=0, size=2.4)+
  theme(legend.position=c(x=0.33, y=0.9), legend.title=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#Austria
df_at <- df_owid %>%
  filter(location %in% c("Germany", "Austria")&!is.na(new_cases_smoothed_per_million))

ggplot(df_at, aes(date, new_cases_smoothed_per_million, color=location))+
  geom_line()+
  theme_tufte()+
  labs(x="",y="7-Tage-Inzidenz (1.1.-6.8.)")+
  scale_color_manual(values=c("red", "black"))+
  geom_vline(xintercept=end_mandate_shopsschools_de, linetype="dashed")+
  geom_vline(xintercept=end_mandate_schoolsclassroom_at, colour="red", linetype="dashed")+
  geom_vline(xintercept=end_mandate_schools_at, colour="red", linetype="dashed")+
  geom_vline(xintercept=end_mandate_shopstransport_at, colour="red", linetype="dashed")+
  geom_text(aes(x=end_mandate_schoolsclassroom_at, y=0, label="21. Februar"), colour="red", angle=90, vjust=-0.5, hjust=0, size=2.4)+
  geom_text(aes(x=end_mandate_schools_at, y=0, label="25. April"), colour="red", angle=90, vjust=-0.5, hjust=0, size=2.4)+
  geom_text(aes(x=end_mandate_shopstransport_at, y=0, label="1. Juni"), colour="red", angle=90, vjust=-0.5, hjust=0.5, size=2.4)+
  geom_text(aes(x=end_mandate_shopsschools_de, y=0, label="3. April"), colour="black", angle=90, vjust=-0.5, hjust=0, size=2.4)+
  theme(legend.position=c(x=0.3, y=0.9), legend.title=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#Czech Republic

df_cz <- df_owid %>%
  filter(location %in% c("Germany", "Czechia")&!is.na(new_cases_smoothed_per_million))

ggplot(df_cz, aes(date, new_cases_smoothed_per_million, color=location))+
  geom_line()+
  theme_tufte()+
  labs(x="",y="7-Tage-Inzidenz (1.1.-6.8.)")+
  scale_color_manual(values=c("red", "black"))+
  geom_vline(xintercept=end_mandate_shopsschools_de, linetype="dashed")+
  geom_vline(xintercept=end_mandate_shops_cz, colour="red", linetype="dashed")+
  geom_vline(xintercept=end_mandate_transport_cz, colour="red", linetype="dashed")+
  geom_vline(xintercept=end_mandate_healthcare_cz, colour="red", linetype="dashed")+
  geom_text(aes(x=end_mandate_shops_cz, y=0, label="14. März"), colour="red", angle=90, vjust=-0.5, hjust=0, size=2.4)+
  geom_text(aes(x=end_mandate_transport_cz, y=0, label="14. April"), colour="red", angle=90, vjust=-0.5, hjust=0, size=2.4)+
  geom_text(aes(x=end_mandate_healthcare_cz, y=0, label="5. Mai"), colour="red", angle=90, vjust=-0.5, hjust=0.5, size=2.4)+
  geom_text(aes(x=end_mandate_shopsschools_de, y=0, label="3. April"), colour="black", angle=90, vjust=-0.5, hjust=0, size=2.4)+
  theme(legend.position=c(x=0.3, y=0.9), legend.title=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())


#Poland

df_pl <- df_owid %>%
  filter(location %in% c("Germany", "Poland")&!is.na(new_cases_smoothed_per_million))

ggplot(df_pl, aes(date, new_cases_smoothed_per_million, color=location))+
  geom_line()+
  theme_tufte()+
  labs(x="",y="7-Tage-Inzidenz (1.1.-6.8.)")+
  scale_color_manual(values=c("black", "red"))+
  geom_vline(xintercept=end_mandate_shopsschools_de, linetype="dashed")+
  geom_vline(xintercept=end_mandate_shopstransport_pl, colour="red", linetype="dashed")+
  geom_text(aes(x=end_mandate_shopstransport_pl, y=0, label="28. März"), colour="red", angle=90, vjust=-0.5, hjust=0.5, size=2.4)+
  geom_text(aes(x=end_mandate_shopsschools_de, y=0, label="3. April"), colour="black", angle=90, vjust=-0.5, hjust=0, size=2.4)+
  theme(legend.position=c(x=0.3, y=0.9), legend.title=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())