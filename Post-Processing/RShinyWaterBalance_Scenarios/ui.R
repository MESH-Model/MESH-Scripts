# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
rm(list=ls())
library(shiny)

#Load the stream flow csv file

pathR <- "C:/Path/To/Parent/Folder/Of/Scenario/Subfolders/" #Specify the parent folder of the scenario subfolders

files <- list.files(pathR, pattern = "plots",full.names = TRUE,recursive=TRUE)

WB    <- read.csv(files[1],sep=",")
WB[1]    <- NULL
n<-dim(WB)

#Number of stations - Exclude first two columns and last column
ns<-ceiling((n[2]-3)/2)
nl<-n[1]

#set dates
min_date<-as.Date(paste(as.character(WB$DAY[1]),"/",as.character(WB$YEAR[1]),sep=""),format='%j/%Y')
max_date<-as.Date(paste(as.character(WB$DAY[nl]),"/",as.character(WB$YEAR[nl]),sep=""),format='%j/%Y')
start_date<-min_date
stop_date<-max_date

print(start_date)
print(stop_date)
col.name=names(WB)


shinyUI(fluidPage(
  
  # Application title
  titlePanel("MESH daily plotting application"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("file", "1. Choose a file:",files,
                  multiple = FALSE, selectize = FALSE),
      sliderInput("dateRange",
                     "Range:",
                     value = c(min_date, max_date),
                     min = min_date, max = max_date,
                     ),
      selectizeInput(
        'col.name', '2. Variables', choices = col.name, multiple = TRUE
      ),
      selectizeInput(
        'colors', '3. colors', choices = colors(), multiple = TRUE
      ),
      
      checkboxInput("norm","normalization",value=FALSE),
      
      checkboxInput("anual","anual",value=FALSE),
      
      checkboxInput("print_it","Save as PDF",value=FALSE),
      ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))


