require(tidyverse)
require(rgdal)
require(ggsn)
require(httr)

ToClip = function(x) write.table(x, "clipboard", sep="\t", row.names = F)


setwd("C:/Users/dgoodman/OneDrive - Emerson/Reporting/Wuhan/")

ReferenceFile <- "COVID_Ref_File.v3.xlsx"
State_Codes <- read_excel(ReferenceFile, sheet="State_Codes")

BeginDate <- as.Date("2020-02-09")

#Read Johns Hopkins Data From Github

NameFixR <- function (x){
  case_when(
    x=="Mainland China" ~ "China",
    x=="Russia" ~ "Russian Federation",
    x=="Viet Nam" ~ "Vietnam",
    x=="Republic of Korea" ~ "Korea, South",
    x=="Moldova" ~ "Republic of Moldova",
    x=="Iran (Islamic Republic of)" ~ "Iran",
    x=="Taipei and environs"~"Taiwan*",
    x=="Czech Republic" ~ "Czechia",
    x=="Gibralter" ~ "United Kingdom",
    x=="Channel Islands" ~ "United Kingdom",
    x=="UK" ~ "United Kingdom",
    TRUE ~ x
  )
}

StateConvertR <- function(state,country,replacement) {
  case_when( (country=="US" & grepl(",", state)) ~ paste("US-", stringr::str_sub(state, -2), sep=""),
             (country=="US" & !is.na(replacement)) ~ replacement,
             (country=="US" & state %in% c("Diamond Princess", "Grand Princess")) ~ "Unassigned Location",
             TRUE ~ state
  )}

                                             

#Raw File Locations
JHConfirmPath = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
JHDeathPath = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
JHCurePath = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

JH_ConfirmedDataRaw <- read.csv(url(JHConfirmPath),stringsAsFactors = F)
JH_DeathDataRaw <-read.csv(url(JHDeathPath),stringsAsFactors = F)
JH_RecoveredDataRaw <- read.csv(url(JHCurePath),stringsAsFactors = F)



JH_ConfirmedData <- JH_ConfirmedDataRaw%>%
  left_join(State_Codes, by =c("Province.State"="name"))%>%
  select( -Lat, - Long)%>%
  mutate(Country.Region = NameFixR(Country.Region))%>%
  mutate(Province.State = if_else(Country.Region == "Taiwan*", "Taiwan", Province.State))%>%
  mutate(X3.10.20 = ifelse((Country.Region == "US" & grepl (", ", Province.State)),0, X3.10.20))%>%
  mutate(X3.11.20 = ifelse((Country.Region == "US" & grepl (", ", Province.State)),0, X3.11.20))%>%
  mutate(Province.State = StateConvertR (Province.State, Country.Region, iso_3166_2))%>%
  gather(key="DateCode", value = "Confirmed", -c(Province.State, Country.Region, iso_3166_2))%>%
  group_by(Province.State, Country.Region, DateCode)%>%
  summarize(Confirmed = sum(Confirmed, na.rm=T))%>%
  mutate(ReportTime = as.POSIXct(DateCode, format = "X%m.%d.%y"))

JH_DeathData <- JH_DeathDataRaw%>%
  left_join(State_Codes, by =c("Province.State"="name"))%>%
  select( -Lat, - Long)%>%
  mutate(Country.Region = NameFixR(Country.Region))%>%
  mutate(Province.State = if_else(Country.Region == "Taiwan*", "Taiwan", Province.State))%>%
  mutate(X3.10.20 = ifelse((Country.Region == "US" & grepl (", ", Province.State)),0, X3.10.20))%>%
  mutate(X3.11.20 = ifelse((Country.Region == "US" & grepl (", ", Province.State)),0, X3.11.20))%>%
  mutate(Province.State = StateConvertR (Province.State, Country.Region, iso_3166_2))%>%
  gather(key="DateCode", value = "Death", -c(Province.State, Country.Region, iso_3166_2))%>%
  group_by(Province.State, Country.Region, DateCode)%>%
  summarize(Deaths = sum(Death, na.rm=T))%>%
  mutate(ReportTime = as.POSIXct(DateCode, format = "X%m.%d.%y"))

JH_RecoverData <- JH_RecoveredDataRaw%>%
  left_join(State_Codes, by =c("Province.State"="name"))%>%
  select( -Lat, - Long)%>%
  mutate(Country.Region = NameFixR(Country.Region))%>%
  mutate(Province.State = if_else(Country.Region == "Taiwan*", "Taiwan", Province.State))%>%
  mutate(X3.10.20 = ifelse((Country.Region == "US" & grepl (", ", Province.State)),0, X3.10.20))%>%
  mutate(X3.11.20 = ifelse((Country.Region == "US" & grepl (", ", Province.State)),0, X3.11.20))%>%
  mutate(Province.State = StateConvertR (Province.State, Country.Region, iso_3166_2))%>%
  gather(key="DateCode", value = "Recovered", -c(Province.State, Country.Region, iso_3166_2))%>%
  group_by(Province.State, Country.Region, DateCode)%>%
  summarize(Recovered = sum(Recovered, na.rm=T))%>%
  mutate(ReportTime = as.POSIXct(DateCode, format = "X%m.%d.%y"))

JH_Data <- JH_ConfirmedData%>%
  left_join(JH_DeathData, by = c("Province.State", "Country.Region", "DateCode", "ReportTime"))%>%
  left_join(JH_RecoverData, by =c("Province.State", "Country.Region", "DateCode", "ReportTime"))%>%
  select(Province.State,Country.Region, DateCode,ReportTime,Confirmed,Deaths,Recovered)%>%
  filter(Country.Region != "Republic of Ireland")%>%
  mutate(Existing = Confirmed -Deaths -Recovered)%>%
  ungroup()%>%
  mutate(Country.Region=case_when (Country.Region == "Korea, South" ~ "South Korea",
                                   Country.Region == "Taiwan*" ~ "Taiwan",
                                   TRUE ~ Country.Region))

JH_DateTime <- max(JH_ConfirmedData$ReportTime)
DataDate = as.character(JH_DateTime, format = "%d %b %Y")

write_csv(JH_Data, paste0("JH_Data",DataDate,".csv"), na="")