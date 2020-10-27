#Install required Packages 
install.packages("tidyverse")
install.packages("httr")
install.packages("jsonlite")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("readr")


#Load Packages 
library(tidyverse)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(readr)

# adding url- you can replace Egypt with your country in the url
#you can also change the date [yyyymmdd] range in the url
path <- "https://covidmap.umd.edu/api/resources?indicator=mask&type=smoothed&country=Kuwait&daterange=20200401-20201010"
# request data from api
request <- GET(url = path)
# make sure the content is encoded with 'UTF-8'
response <- content(request, as = "text", encoding = "UTF-8")
# now we can have a dataframe for use!
Facebook_Surevy<- fromJSON(response, flatten = TRUE) %>% data.frame()
Facebook_Surevy$data.survey_date<-parse_datetime(as.character(Facebook_Surevy$data.survey_date), format = "", na = c("", "NA"))
Facebook_Surevy$data.survey_date<-as.Date(Facebook_Surevy$data.survey_date)
names(Facebook_Surevy)[names(Facebook_Surevy) == "data.country"] <- "Country"
names(Facebook_Surevy)[names(Facebook_Surevy) == "data.survey_date"] <- "Date"

Face_mask_Cov<-ggplot(Facebook_Surevy)+
  geom_line(aes(x=Date, y=data.smoothed_mc*100, ymax=100, ymin=0), color="red")+
  labs(x="Date", y="Mask Coverage (%)",size=1)+theme_bw()

Face_mask_Cov

OxResTrac<-read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv", header = T)
names(OxResTrac)[names(OxResTrac) == "CountryName"] <- "Country"
OxResTrac$Date<-parse_datetime(as.character(OxResTrac$Date), format = "", na = c("", "NA"))
OxResTrac$Date<-as.Date(OxResTrac$Date)

Merged<-right_join(OxResTrac,Facebook_Surevy,by=c("Country","Date" ))

Mask_Mandate<-ggplot(Merged)+
  geom_line(aes(x=Date, y=H6_Facial.Coverings, ymax=4), color="Blue")+
  labs(y="Face Mask Mandate",size=1)+theme_bw()

# 0 - No policy
# 1 - Recommended
# 2 - Required in some specified shared/public spaces outside the home with other people present, or some situations when social distancing not possible
# 3 - Required in all shared/public spaces outside the home with other people present or all situations when social distancing not possible
# 4 - Required outside the home at all times regardless of location or presence of other people

grid.arrange(Face_mask_Cov,Mask_Mandate, ncol=1)

