# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(lubridate)
library(dplyr)

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
    File  <-input$file
    WB    <- read.csv(File,sep=",")
    WB[1] <- NULL
    WB[WB==-999.90]<-NA
    
    #normalize
    if(input$norm){
      n = length(WB)
      WB[3:n] <- scale(WB[3:n])
    }
    #print(WB)
    
    #agregate variables
    if (input$anual) {
      n = length(WB)
      names <- colnames(WB[3:n])
      WBYEAR <- data.frame()
      WBYEAR <- distinct(data.frame(WB$YEAR))
      colnames(WBYEAR) <- "YEAR"
      for(i in 3:n) {
        namecol        <- colnames(WB)[i]
        temp           <- data.frame(WB$YEAR)
        colnames(temp) <- "YEAR"
        temp$data      <- pull(WB,namecol)
        temp           <- temp %>%
          group_by(YEAR) %>%
          summarise(data = mean(data))
        WBYEAR <- cbind(WBYEAR,temp$data)
      }
      colnames(WBYEAR) <- c("YEAR",names)
      WBYEAR2 <- data.frame(WB[1:2])
      WB<- merge(WBYEAR2,WBYEAR, by.x = "YEAR", by.y = "YEAR")
      #print(WB)
    }
    
    #set of cols
    colR    <- input$col.name
    colorsR <- input$colors
    n = length(colR)
    index<-match(colR,colnames(WB))
    #print(index)
    
    #subset dates
    WB$Ndate<-paste(as.character(WB$DAY),"/",as.character(WB$YEAR),sep="")
    WB$Ndate<-as.Date(WB$Ndate,format = '%j/%Y')
    stop_date<-input$dateRange[2]
    start_date<-input$dateRange[1]
    subWB<-subset(WB,Ndate>start_date & Ndate<stop_date)
    
    #plot_text<- paste(colnames(WB[index[1]]),"NS=",NSeff(WB[index[1]],WB[index[2]]),
              #" bias = ",PBIAS(WB[index[1]],WB[index[2]]))
    
    #Plot
    p<-ggplot(subWB, aes(x=Ndate)) + 
       scale_x_date(date_labels ="%b %Y") + xlab("") + ylab("Daily flow (m^3/s)")
    #print(subWB)
    #+ggtitle(plot_text,subtitle="")
      
    for (i in 1:n) {
      p <- p + geom_line(aes_string(y=colnames(subWB[index[i]])),color=colorsR[i],size=1)
    }
    
    print(p)
    
    #save plot
    if(input$print_it){
      plot_file=paste(colnames(WB[index]),".pdf",sep="")
      ggsave(plot_file,plot=p,device="pdf")
    }
    
  })
  
})