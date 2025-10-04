library(plotthis)
library(ggplot2)
library(readxl)
library(scales) 
setwd("$HOME/HadalTrench_ARGs/Fig4g")
data<- read.table("Fig4g.sourceData.txt", sep="\t",  header=TRUE, stringsAsFactors=FALSE)
data_radar <- data.frame(
  MT = rep(names(data)[-1], each = nrow(data)),  
  ARG_risk_rank = rep(data$ARG_risk_rank, times = ncol(data)-1),  
  Abundance = unlist(data[-1])                  
)
Radar <- RadarPlot(
  data_radar,             
  x = "MT",                      
  group_by = "ARG_risk_rank",    
  y = "Abundance",               
  scale_y = "none",              
  y_min = 0,                     
  y_max = 100,                   
  y_nbreaks = 5,                 
  fill = TRUE,                   
  alpha = 0.1,                   
  linewidth = 1,                 
  pt_size = 3,                   
  theme = "theme_this",          
  palcolor = c("#d62728", "#F0E442", "#009E73", "#E69F00", "#0072B2"), 
  aspect.ratio = 1,              
  title = NULL,                  
)+  
  theme(
    legend.key = element_blank(),       
    legend.spacing.y = unit(0.5, "cm")  
  ) +
  guides(color = guide_legend(
    override.aes = list(shape = 16, size = 4, linetype = 0) 
  ))
Radar
ggsave("Fig4g.pdf", plot = Radar, width = 10, height = 5)   
