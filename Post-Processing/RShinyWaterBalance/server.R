# This is script is a basic RShiny web application to plot the water balance variables from the "Basin_average_water_balance.csv" file output from MESH

# You can find out more about building applications with Shiny here:
# http://shiny.rstudio.com

# The file path is specified in the accompanying "ui.R" file
# Once the app is launched, the user selects the file, date range, and variables of interest and assigns colors to each variable.

library(shiny)
library(ggplot2)

NSeff<-function(x,y)
{
  NS<-sapply(signif(1-(sum((x-y)^2))/(sum((x-mean(x))^2))),sprintf,fmt="%#.2g")
  return(NS)
}
#
PBIAS<-function(x,y)
{
  PB<-round(sum(x-y)*100/sum(x),0)
  return(PB)
}

Norm<-function(x,y)
{
  PB<-round(sum(x-y)*100/sum(x),0)
  return(PB)
}

shinyServer(function(input, output) {
  
  
  output$distPlot <- renderPlot({
    
    #read list from GUI and get column number using the match function
    my_stat<-input$file
    File<-my_stat
    WB<-read.csv(File,sep=",")
    WB[WB==-9999]<-NA # Filter out error values
    if(input$norm){
      n = length(WB)
      WB[4:n-1] <- scale(WB[4:n-1])
    }
    WB$Ndate<-paste(as.character(WB$DAY),"/",as.character(WB$YEAR),sep="")
    WB$Ndate<-as.Date(WB$Ndate,format = '%j/%Y')
    
    stop_date<-input$dateRange[2]
    start_date<-input$dateRange[1]
    subWB<-subset(WB,Ndate>start_date & Ndate<stop_date)
    
    #plot_text<- paste(colnames(WB[index]),"NS=",NSeff(WB[,index],WB[,index+1]),
    #" bias = ",PBIAS(WB[,index],WB[,index+1]))
    
    colR    <- input$col.name
    colorsR <- input$colors
    n = length(colR)
    index<-match(colR,colnames(WB))
    
    p<-ggplot(subWB, aes(x=Ndate)) + 
      scale_x_date(date_labels ="%b %Y") + xlab("") + ylab("Amount (mm)")+
      theme(legend.position="right")
      
    
    for (i in 1:n) {
      p <- p + geom_line(aes_string(y=colnames(subWB[index[i]])),color=colorsR[i],size=1)
    }
    
    print(p)
    
    if(input$print_it){
      plot_file=paste(colnames(WB[index]),".pdf",sep="")
      ggsave(plot_file,plot=p,device="pdf")
    }
    
  })
  
})