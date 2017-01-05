theme_mooney<-function(base_size = 18, base_family = "Helvetica", legend = "bottom"){
  theme(
    line =               element_line(colour = "black", size = 1, linetype = 1, 
                                      lineend = "round"),
    rect =               element_rect(fill = "white", colour = "white", size = 0.5, linetype = 1),
    text =               element_text(family = base_family, face = "plain", colour = "black",
                                      size = base_size, hjust = 0.5, vjust = 0.5, angle = 0,
                                      lineheight = 0.9, margin=margin(), debug=FALSE),
    axis.text =          element_text(size = rel(0.8), colour = "black"),
    strip.text =         element_text(size = rel(0.8)),
    
    axis.line.x =         element_line(color = "black", size=1),
    axis.line.y =         element_line(color = "black", size=1),
    axis.text.x =        element_text(face = "plain", margin=margin(2,0,0,0)),
    axis.text.y =        element_text(face = "plain", margin=margin(0,2,0,0)),
    axis.ticks =         element_line(colour = "black", size= .6),
    axis.title =         element_text(colour = "black", face = "bold"),
    axis.title.x =       element_text(margin=margin(10,0,0,0)),
    axis.title.y =       element_text(angle = 90, margin=margin(0,10,0,0)),
    axis.ticks.length =  unit(0.15, "cm"),
    
    legend.background =  element_rect(colour = NA),
    legend.margin =      margin(0.2, 0.2, 0.2, 0.2),
    legend.key =         element_blank(),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(1)),
    legend.text.align =  NULL,
    legend.title =       element_text(size = rel(1), face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position =    legend,
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,
    
    panel.background =   element_rect(fill = "white", colour = NA),
    panel.border =       element_blank(),
    panel.grid.major =   element_blank(),
    panel.grid.minor =   element_blank(),
    panel.spacing =      unit(0.25, "lines"),
    
    strip.background =   element_rect(fill = "black", size = 0.5),
    strip.text.x =       element_text(),
    strip.text.y =       element_text(angle = -90),
    
    plot.background =    element_rect(colour = "white"),
    plot.title =         element_text(size = rel(1.2)),
    plot.margin =        unit(c(1, 1, 1, 1), "lines"),
    
    complete = TRUE
  )
}

pal_nell<-c("#25B886","#38149C","#FFB917","#6e0A4C","#DB3C1D","#CBA1F0","#85EB00","#1A6E87","#91FFCF")
pal_pastel_5<-c("#938FC9","#8E5572","#C1EDCC","#F&C1BB","#443850")
