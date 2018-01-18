
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(RCurl)##source_url
library(devtools)
library(VennDiagram)

source_url("https://raw.githubusercontent.com/collnell/R_vis/master/theme_mooney.R")#ggplot theme

shinyServer(function(input,output){
  
  
  values<-reactiveValues()
  arich<-reactive({as.numeric(input$rich_A)})
  brich<-reactive({as.numeric(input$rich_B)})
  
  
  
  
  output$vend<-renderPlot({
    
    
    vd<-draw.pairwise.venn(area1=input$rich_A,area2=input$rich_B,cross.area=input$share_AB, 
                           category=c("Community A","Community B"),euler.d=TRUE,scaled=TRUE,
                           fill = c("darkgoldenrod3","darkblue"),alpha=.5,label.col="black",cex=3,col="white",lwd=3,
                           cat.cex=3,margin=.05)
    
    vd
    
  })
  
 
  
})