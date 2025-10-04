# library
library(tidyverse)
library(viridis)

setwd("$HOME/HadalTrench_ARGs/Fig6b")
data<- read.table ("Fig6b.sourceData.txt",sep="\t",header=TRUE) 
# Transform data in a tidy format (long format)
data <- data %>% gather(key = "Ecotypes", value="value", -c(1,2)) 
empty_bar <- 2 # Set a number of 'empty bar' to add at the end of each group
nObsType <- nlevels(as.factor(data$Ecotypes))-2 
to_add <- data.frame( matrix(NA, empty_bar*nlevels(as.factor(data$group))*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(as.factor(data$group)), each=empty_bar*nObsType )
data <- rbind(data, to_add)
data <- data %>% arrange(group, individual)
data$id <- rep( seq(1, nrow(data)/(nlevels(as.factor(data$Ecotypes)))) , each=nlevels(as.factor(data$Ecotypes)))

label_data <- data %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
base_data
# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]
s<-grid_data$end
e<-grid_data$start

grid_data$start<-s
grid_data$end<-e
grid_data 



# Make the plot
p <- ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=Ecotypes), stat="identity", alpha=0.5) +
  scale_fill_manual(values = c("#967a4a", "#fcdaae", "#f09648", "#ae4a21"))+
  
  geom_segment(data=grid_data, aes(x = end+0.5, y = 0, xend = start+0.5, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end+0.5, y = 50, xend = start+0.5, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end+0.5, y = 100, xend = start+0.5, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  ggplot2::annotate("text", x = rep(max(data$id),1)+0.1, y = c(0, 30, 60, 90), label = c("0", "30", "60", "90") , color="grey", size=3 , angle=10, fontface="bold", hjust=1) +
  ylim(-100,100) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,10), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+0.5, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start-0.5, y = -5, xend = end+1.5, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(0.5,0.5,0.5), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

p

# Save at pdf
ggsave(p, file="Fig6b_data.pdf", width=10, height=10)

