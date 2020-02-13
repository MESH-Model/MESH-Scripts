# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:

# The user must update the variable "File" with the file path the "MESH_output_streamflow.csv" file in this script and the "server.R" script

# You can find out more about building applications with Shiny here:
# http://shiny.rstudio.com

rm(list=ls())
library(shiny)

#Load the stream flow csv file
File<-'C:/Path/To/The/File/MESH_output_streamflow.csv'

Q<-read.csv(File,sep=",")
n<-dim(Q)

#Number of stations - Exclude first two columns and last column
ns<-ceiling((n[2]-3)/2)
nl<-n[1]

#set dates
stat_name<-vector(length = ns)
min_date<-as.Date(paste(as.character(Q$DAY[1]),"/",as.character(Q$YEAR[1]),sep=""),format='%j/%Y')
max_date<-as.Date(paste(as.character(Q$DAY[nl]),"/",as.character(Q$YEAR[nl]),sep=""),format='%j/%Y')
start_date<-min_date
stop_date<-max_date

print(start_date)
print(stop_date)

#assing the header names for measured flow skipping the first 2 columns
for (j in 1:ns){
  index=1+j
  meas=j+index
  stat_name[j]<-colnames(Q[meas])
}

shinyUI(fluidPage(
  
  # Application title
  titlePanel("MESH daily plotting application"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("station", "Choose a station:",stat_name,
                  multiple = FALSE, selectize = FALSE),
      sliderInput("dateRange",
                     "Range:",
                     value = c(min_date, end =max_date),
                     min = min_date, max = max_date,
                     ),
      checkboxInput("print_it","Save as PDF",value=FALSE)),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))


