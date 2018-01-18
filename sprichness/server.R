###shiny template server

library(shiny)
library(ggplot2)
library(vegan)
library(EcoSimR)
library(dplyr)
library(iNEXT)
library(readr)


source_url("https://raw.githubusercontent.com/collnell/R_vis/master/theme_mooney.R")#ggplot theme


shinyServer(function(input,output){


  values<-reactiveValues()
  

  
  observe({

      #generate 2 community matrices based on user inputs
    A.spmat<-ranMatGen(aBetaRow=1, bBetaRow=1, ##set species to similar probability? #1 would make uniform probabilities
                       aBetaCol=1, bBetaCol =1, ## detection? vs evenness?
                       numRows=input$srA, numCols=input$eff_A, ##draws from poisson 
                       mFill =1, abun =input$abun_A,
                       emptyRow=FALSE, emptyCol=FALSE)
    B.spmat<-ranMatGen(aBetaRow=1, bBetaRow=1, 
                       aBetaCol=1, bBetaCol =1, 
                       numRows=input$srB, numCols=input$eff_B, 
                       mFill =1, abun =input$abun_B,
                       emptyRow=FALSE, emptyCol=FALSE)
    
    A.sum->colSums(A.spmat$m)
    B.sum->colSums(B.spmat$m)#need to make df first
    
    val.sums<-rbind(A.sum,B.sum)
      
    values$sum.vals<-val.sums
    
    
    A.df<-as.data.frame(t(A.spmat$m))
    A.df$plot_rich<-rowSums(A.df!=0)
    A.df$plot_abun<-rowSums(A.df)
    A.df$cum_sr<-cumsum(A.df$plot_rich)
    A.df$cum_abun<-cumsum(A.df$plot_abun)
    values$A.dfs<-A.df
    values$B.df<-as.data.frame(t(B.spmat$m))
    values$Amat<-A.spmat$m
    values$Bmat<-B.spmat$m
  })

   
  
  output$rarefaction<-renderPlot({
    
    Amath<-as.data.frame(values$Amat)
    sg<-specaccum(Amath,method="rarefaction")
    sdf<-data.frame(ind = sg$individuals,
                    rich =sg$richness,
                    site=sg$sites,
                    sd=sg$sd)
    dok<-ggplot(sdf,aes(x=ind,y=rich))+theme_mooney()+stat_smooth(method="loess")
    dok
    
    rar.plot<-ggplot(values$A.dfs,aes(x=cum_abun,y=cum_sr))+geom_point(size=2)+
      labs(x="Individuals",y="Species Richness")+
      theme_mooney(legend.location="bottom")
  
    
  })
  
})