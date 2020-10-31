library(tidyverse)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(readr)

ui<- fluidPage(
  titlePanel("Mask Wearing in the Eastern Mediterranean Region"),
  sidebarLayout(
    sidebarPanel(img(src = "EMRO.png", height = 80, width = 200),
                 p(  ),
                 
                 selectInput("Country", "Please, Select the country",
                             choices = c("Afghanistan","Bahrain","Djibouti","Egypt","Iran","Iraq","Jordan",
                                         "Kuwait","Lebanon","Libya","Morocco","Oman","Pakistan","Qatar","Saudi Arabia",
                                         "Somalia","Sudan","Syria","Tunisia","United Arab Emirates","Yemen", "Palestine")),

                  conditionalPanel(condition = "input.Country == TRUE",
                                  selectInput("Indictor", "Please, Select the indicator",
                                               choices = c("mask","covid","flu","contact","finance"))),

                 dateInput('Start_date',
                           label = 'Start Date: yyyy-mm-dd',
                           value = Sys.Date()-240
                 ),
                 
                 dateInput('End_date',
                           label = 'End Date: yyyy-mm-dd',
                           value = Sys.Date()-10
                 ),
                 
                 actionButton("goButton", "Go!")
                 ),
  
  mainPanel(
    plotOutput("myplot"),                                
    p(strong("In the bottom plot")),
    p("0 - No policy"),
    p("1 - Recommended"),
    p("2 - Required in some specified shared/public spaces outside the home with other people present, or some situations when social distancing not possible"),
    p("3 - Required in all shared/public spaces outside the home with other people present or all situations when social distancing not possible"),
    p("4 - Required outside the home at all times regardless of location or presence of other people")

    
    
)
)
)


OxResTrac<-read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv", header = T)
names(OxResTrac)[names(OxResTrac) == "CountryName"] <- "Country"
OxResTrac$Date<-parse_datetime(as.character(OxResTrac$Date), format = "", na = c("", "NA"))
OxResTrac$Date<-as.Date(OxResTrac$Date)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$myplot <- renderPlot({
    # generate bins based on input$bins from ui.R
    input$goButton
    isolate({
      country0<- input$Country
      indicator<- input$Indictor
      start_date<-as.character(gsub("-","",input$Start_date))#you can also change the date [yyyymmdd] range
      end_date<-as.character(gsub("-","",input$End_date))
      # draw the histogram with the specified number of bins
      url1<-"https://covidmap.umd.edu/api/resources?"
      indicator1<-paste("indicator=",indicator,"&type=smoothed&", sep="")
      country1<-paste("country=",country0, sep = "")
      date_range1<-paste("&daterange=",start_date,"-",end_date,sep = "")
      path <- paste(url1,indicator1,country1,date_range1, sep="")
      
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
      
     
      
      Merged<-right_join(OxResTrac,Facebook_Surevy,by=c("Country","Date" ))
      
      Mask_Mandate<-ggplot(Merged)+
        geom_line(aes(x=Date, y=H6_Facial.Coverings, ymin=0, ymax=4), color="Blue")+
        labs(y="Face Mask Mandate",size=1)+theme_bw()
      
      
      myplot<- grid.arrange(Face_mask_Cov,Mask_Mandate, ncol=1, top=textGrob(country0, gp=gpar(fontsize=20,font=8)))
      
      
    })
    
  })
}



# Run the application 
shinyApp(ui = ui, server = server)


