# Package
library(networkD3)
library(tidyverse)
library(viridis)
library(patchwork)
library(circlize)
library(dplyr)


setwd("$HOME/HadalTrench_ARGs/Fig6a")
merge_data_long<- read.table ("Fig6a.sourceData.txt",sep="\t",header=TRUE) 
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(merge_data_long$source), 
         as.character(merge_data_long$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
merge_data_long$IDsource=match(merge_data_long$source, nodes$name)-1 
merge_data_long$IDtarget=match(merge_data_long$target, nodes$name)-1

#prepare colour scale
ColourScal ='d3.scaleOrdinal() .range([
"#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF",
"#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

p<-sankeyNetwork(Links = merge_data_long, Nodes = nodes,
                 Source = "IDsource", Target = "IDtarget",
                 Value = "value", NodeID = "name", 
                 sinksRight=FALSE, 
                 #colourScale=ColourScal, 
                 nodeWidth=50, 
                 fontSize=10, 
                 nodePadding=20)
p

# Copy the URL of the html window you get
# load webshot library
library(webshot)

#install phantom:
#webshot::install_phantomjs(force = TRUE) , vwidth = 1200,vheight=1200
# Make a webshot in pdf : high quality but can not choose printed zone
saveNetwork(p,file = "Fig6a.html")
webshot::webshot("Fig6a.html", file="Fig6a.pdf", 
                 delay = 2,zoom = 2, vwidth = 1200, vheight=1200)







