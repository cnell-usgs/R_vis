
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(RCurl)##source_url


source_url("https://raw.githubusercontent.com/collnell/R_vis/master/theme_mooney.R")#ggplot theme


shinyServer(function(input,output){
  
  
  
  
  output$islands<-renderPlot({
    is.df<-data.frame(island = as.factor(c(1,2,3,4,5)),
                      size = as.integer(c(input$size1,input$size2,input$size3,input$size4,NA)),
                      dist = as.numeric(c(input$dist1,input$dist2,input$dist3,input$dist4,NA)))
                      
    isle.plot<-ggplot(is.df,aes(y=dist, x=island, color=island))+geom_point(aes(size=size),alpha=.5)+theme_mooney(legend.location="none")+
      labs(y="Distance from mainland",x="Mainland")+coord_flip()+scale_size(range=c(3,60))+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.x=element_blank())+
      ylim(0,50)+scale_color_manual(values=c("green","red","yellow","blue","purple"))
    isle.plot
    
  })
 
  
})