# This script is used to produce an RShiny web application to view the measured vs. simulated streamflow for a number of stations.

# The user must update the variable "File" with the file path the "MESH_output_streamflow.csv" file in this script and the "ui.R" script

# You can find out more about building applications with Shiny here:
# http://shiny.rstudio.com

library(shiny)
library(ggplot2)

NSeff<-function(x,y)
{
  NS <- round(1- sum((x-y)^2,na.rm=TRUE)/sum((x-mean(x, na.rm=TRUE))^2, na.rm=TRUE),2)
  return(NS)
}


PBIAS<-function(x,y)
{
  # PB<-round(sum(x-y)*100/sum(x),0)
  PB<-round(sum(x-y, na.rm=TRUE)*100/sum(x, na.rm=TRUE),0)
  return(PB)
}

shinyServer(function(input, output) {
  
  File<-'C:/Path/To/The/File/MESH_output_streamflow.csv'
  Q<-read.csv(File,sep=",")
  Q[Q < 0]<-NA
  Q$Ndate<-paste(as.character(Q$DAY),"/",as.character(Q$YEAR),sep="")
  Q$Ndate<-as.Date(Q$Ndate,format = '%j/%Y')
  
  counter=0
  
  output$distPlot <- renderPlot({
    
    #read list from GUI and get column number using the match function
    my_stat<-input$station
    index<-match(my_stat,colnames(Q))
    
    plot_text<- paste(colnames(Q[index]),"NS=",NSeff(Q[,index],Q[,index+1]),
                      " bias = ",PBIAS(Q[,index],Q[,index+1]))
    
    
    stop_date<-input$dateRange[2]
    start_date<-input$dateRange[1]
    subQ<-subset(Q,Ndate>start_date & Ndate<stop_date)
    
    p<-ggplot(subQ, aes(x=Ndate)) + geom_line(aes_string(y=colnames(subQ[index])),color='dodgerblue3',size=1) +
      geom_line(aes_string(y=colnames(subQ[index+1])),color='darkorange3',size=1) +
      ggtitle(plot_text,subtitle="the staticts apply to the entire simulation period not the zoomed in period") +
      scale_x_date(date_labels ="%b %Y") + xlab("") + ylab("Daily flow (m^3/s)")
    
    print(p)
    
    if(input$print_it){
      plot_file=paste(colnames(Q[index]),".pdf",sep="")
      ggsave(plot_file,plot=p,device="pdf")
    }
    
  })
  
})