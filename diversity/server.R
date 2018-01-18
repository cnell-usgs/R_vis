
library(devtools)
library(DT)
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(vegan)
library(RCurl)##source_url
library(reshape2)


source_url("https://raw.githubusercontent.com/collnell/R_vis/master/theme_mooney.R")#ggplot theme


shinyServer(function(input,output){
  sps<-c("sp1","sp2","sp3","sp4","sp5","sp6")

  
  values<-reactiveValues(abun.df = data.frame(Community = as.factor(c("A","B")),
                                              sp1 = as.numeric(c(1, 1)),
                                              sp2 = as.numeric(c(1, 1)),
                                              sp3 = as.numeric(c(1, 1)),
                                              sp4 = as.numeric(c(1, 1)),
                                              sp5 = as.numeric(c(1, 1)),
                                              sp6 = as.numeric(c(1, 1)),
                                              H = c(1.79,1.79),
                                              D = c(.83,.83),
                                              E = c(1,1),
                                              S = c(6,6),
                                              Abundance = c(6,6),
                                              p1 = c(1/6,1/6),
                                              p2 = c(1/6,1/6),
                                              p3 = c(1/6,1/6),
                                              p4 = c(1/6,1/6),
                                              p5 = c(1/6,1/6),
                                              p6 = c(1/6,1/6),
                                              sum = c(1,1)),
                         commA_shan =1.79,
                         commB_shan =1.79,
                         commA_even = 1,
                         commB_even =1,
                         commA_simp = .83,
                         commB_simp = .83,
                         commA_sr = 6,
                         commB_sr =6,
                         p1a = (.167),p2a = (.167),p3a = (.167),p4a = (.167),p5a = (.167),p6a = (.167),p1b = (.167),p2b= (.167),p3b = (.167),p4b = (.167),p5b = (.167),p6b = (.167)
                         )
  
  observeEvent(input$shann, {
    abun.df = data.frame(Community = as.factor(c("A","B")),
                         sp1 = as.numeric(c(input$sp1_a, input$sp1_b)),
                         sp2 = as.numeric(c(input$sp2_a, input$sp2_b)),
                         sp3 = as.numeric(c(input$sp3_a, input$sp3_b)),
                         sp4 = as.numeric(c(input$sp4_a, input$sp4_b)),
                         sp5 = as.numeric(c(input$sp5_a, input$sp5_b)),
                         sp6 = as.numeric(c(input$sp6_a, input$sp6_b)))
    abun.df$Abundance<-rowSums(abun.df[,sps])
    abun.df$H<-diversity(abun.df[,sps])
    abun.df$D<-diversity(abun.df[,sps],index="simpson")
    abun.df$E<-abun.df$H/log(specnumber(abun.df[,sps]))
    abun.df$S<-rowSums(abun.df[,sps] > 0)
    abun.df$p1<-abun.df$sp1/(abun.df$Abundance)
    abun.df$p2<-abun.df$sp2/(abun.df$Abundance)
    abun.df$p3<-abun.df$sp3/(abun.df$Abundance)
    abun.df$p4<-abun.df$sp4/(abun.df$Abundance)
    abun.df$p5<-abun.df$sp5/(abun.df$Abundance)
    abun.df$p6<-abun.df$sp6/(abun.df$Abundance)
    abun.df$sum<-rowSums(abun.df[,c("p1","p2","p3","p4","p5","p6")])
    
    values$table<-abun.df[,c("Community","Abundance","S","H","D","E","p1","p2","p3","p4","p5","p6","sum")]
    
    values$commA_shan<-abun.df$H[[1]]
    values$commB_shan<-abun.df$H[[2]]
    values$commA_even<-abun.df$E[[1]]
    values$commB_even<-abun.df$E[[2]]
    values$commA_simp<-abun.df$D[[1]]
    values$commB_simp<-abun.df$D[[2]]
    values$commA_sr<-abun.df$S[[1]]
    values$commB_sr<-abun.df$S[[2]]
    values$p1a<-abun.df$p1[[1]]
    values$p2a<-abun.df$p2[[1]]
    values$p3a<-abun.df$p3[[1]]
    values$p4a<-abun.df$p4[[1]]
    values$p5a<-abun.df$p5[[1]]
    values$p6a<-abun.df$p6[[1]]
    values$p1b<-abun.df$p1[[2]]
    values$p2b<-abun.df$p2[[2]]
    values$p3b<-abun.df$p3[[2]]
    values$p4b<-abun.df$p4[[2]]
    values$p5b<-abun.df$p5[[2]]
    values$p6b<-abun.df$p6[[2]]
    
    
    values$abun.df<-abun.df
    
  })
  output$shan_A<-renderText({paste0(round(values$commA_shan,2))})
  output$shan_B<-renderText({paste0(round(values$commB_shan,2))})
  output$even_A<-renderText({paste0(round(values$commA_even,2))})
  output$even_B<-renderText({paste0(round(values$commB_even,2))})
  output$simp_A<-renderText({paste0(round(values$commA_simp,2))})
  output$simp_B<-renderText({paste0(round(values$commB_simp,2))})
  
  output$plot1<-renderPlot({
    
    dataf<-melt(values$abun.df, id.vars = c("Community","H","E","D","S","Abundance"), measure.vars = sps, variable.name="Species")
    
    
    bp<-ggplot(dataf,aes(x=Community,y=value,color=Species,fill=Species))+geom_bar(position="fill",stat="identity")+
      theme_mooney(legend.location="top")+theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(),axis.line=element_blank())+
      labs(y="Proportion of Community",x="Community")+coord_flip()+guides(color=guide_legend(nrow=1))+
      scale_color_manual(values=c("navyblue","turquoise","lightcyan2","steelblue","darkslategrey","lightblue3"))+
      scale_fill_manual(values=c("navyblue","turquoise","lightcyan2","steelblue","darkslategrey","lightblue3"))
    bp
  })
 
  output$divtable<-renderDataTable({
    values$abun.df[,c("Community","Abundance","S","H","D","E")]%>%
      datatable(rownames=FALSE, colnames=c("Community","Abundance","Sp Richness (S)","Shannon (H)","Simpson (D)","Evenness (E)"),options= list(dom='t'))%>%
      formatRound(c('H','E','D'),digits=2)
    
    
    })
  output$eventable<-renderDataTable(
    values$abun.df[,c("Community","S","H","p1","p2","p3","p4","p5","p6","sum")]%>%
      datatable(rownames=FALSE,selection=list(target='cell'),options=list(dom='t'))%>%
      formatRound(c("p1","p2","p3","p4","p5","p6","sum","H"),digits=2)
  )
  output$shantable<-renderDataTable(
    values$abun.df[,c("Community","S","p1","p2","p3","p4","p5","p6","H")]%>%
      datatable(rownames=FALSE,selection=list(target='cell'),options=list(dom='t'))%>%
      formatRound(c("p1","p2","p3","p4","p5","p6","H"),digits=2)
  )
  output$simptable<-renderDataTable(
    values$abun.df[,c("Community","S","p1","p2","p3","p4","p5","p6","D")]%>%
      datatable(rownames=FALSE,selection=list(target='cell'),options=list(dom='t'))%>%
      formatRound(c("p1","p2","p3","p4","p5","p6","D"),digits=2)
  )
  
  output$E<-renderUI({
    withMathJax(
      helpText("$$E_{H} =  H / log(S)$$")
    )
  })
  
  output$H<-renderUI({
    withMathJax(
      helpText("$$H' = -~\\sum_{i=1}^{S}~p_{i}*ln(p_{i})$$")
    )
  })
  output$D<-renderUI({
    withMathJax(
      helpText("$$D_{1} = 1 - \\sum_{i=1}^{S}~p_{i}~^2$$")
    )
  })
  output$e_a<-renderUI(h4(tags$b("Community A:   "),round(values$commA_even,2)))
  output$e_b<-renderUI(h4(tags$b("Community B:   "),round(values$commB_even,2)))
  output$h_a<-renderUI(h4(tags$b("Community A:   "),round(values$commA_shan,2)))
  output$h_b<-renderUI(h4(tags$b("Community B:   "),round(values$commB_shan,2)))
  output$d_a<-renderUI(h4(tags$b("Community A:   "),round(values$commA_simp,2)))
  output$d_b<-renderUI(h4(tags$b("Community B:   "),round(values$commB_simp,2)))
  
  output$even_calca<-renderUI(
    if (input$showcalce==TRUE){
      h4(paste(round(values$commA_even,2), " = ", values$commA_sr, " * ","log(",round(values$commA_shan,2),")",sep=""))
    }else{
      
    })
  output$even_calcb<-renderUI(
    if (input$showcalce==TRUE){
      h4(paste(round(values$commB_even,2), " = ", values$commB_sr, " * ","log(",round(values$commB_shan,2),")",sep=""))
    }else{
      
    })
  
  output$shan_calca<-renderUI(
    if (input$showcalch==TRUE){
      h4(paste(round(values$commA_shan,2), " = (-1) * (", 
               round(values$p1a,2)," * log ",round(values$p1a,2), ") + (",
               round(values$p2a,2)," * log ",round(values$p2a,2), ") + (",
               round(values$p3a,2)," * log ",round(values$p3a,2), ") + (",
               round(values$p4a,2)," * log ",round(values$p4a,2), ") + (",
               round(values$p5a,2)," * log ",round(values$p5a,2), ") + (",
               round(values$p6a,2)," * log ",round(values$p6a,2), ")",sep=""))
    }else{
      
    })
  output$shan_calcb<-renderUI(
    if (input$showcalch==TRUE){
      h4(paste(round(values$commA_shan,2), " = (-1) * (", 
               round(values$p1b,2)," * log ",round(values$p1b,2), ") + (",
               round(values$p2b,2)," * log ",round(values$p2b,2), ") + (",
               round(values$p3b,2)," * log ",round(values$p3b,2), ") + (",
               round(values$p4b,2)," * log ",round(values$p4b,2), ") + (",
               round(values$p5b,2)," * log ",round(values$p5b,2), ") + (",
               round(values$p6b,2)," * log ",round(values$p6b,2), ")",sep=""))
    }else{
      
    })
  
  output$simp_calca<-renderUI(
    if (input$showcalcd==TRUE){
      h4(paste(round(values$commA_simp,2), " = 1 - (",
               round(values$p1a,2),"^2 + ",
               round(values$p2a,2),"^2 + ",
               round(values$p3a,2),"^2 + ",
               round(values$p4a,2),"^2 + ",
               round(values$p5a,2),"^2 + ",
               round(values$p6a,2),"^2 )",sep=""))
    }else{
      
    })
  output$simp_calcb<-renderUI(
    if (input$showcalcd==TRUE){
      h4(paste(round(values$commA_simp,2), " = 1 - (",
               round(values$p1b,2),"^2"," + ",
               round(values$p2b,2),"^2"," + ",
               round(values$p3b,2),"^2"," + ",
               round(values$p4b,2),"^2"," + ",
               round(values$p5b,2),"^2"," + ",
               round(values$p6b,2),"^2",")",sep=""))
    }else{
      
    })
                         
                         
  
 
  
})