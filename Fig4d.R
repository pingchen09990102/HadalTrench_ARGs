library(ggplot2)
library(tidyr)
library(dplyr)

setwd("$HOME/HadalTrench_ARGs/Fig4d")
filtered_df <- read.table("Fig4d.sourceData.txt",header = T,row.names = 1,sep = "\t", quote = "\"")
subtype <- row.names(filtered_df)
hf1 <- data.frame(subtype,filtered_df)
dt1<- hf1 %>% pivot_longer(
  cols = !subtype,
  values_drop_na = FALSE,
  names_to = "groups",
  values_to = "abundance")
dt1$subtype <- factor(dt1$subtype,
                      levels = rev(unique(dt1$subtype)),
                      ordered = T)
dt1$groups <- factor(dt1$groups,
                     levels = unique(dt1$groups),
                     ordered = T)
p1 <- ggplot(dt1, aes(groups, subtype)) +
  geom_tile(aes(fill = abundance),linewidth=0.3,
            height=1,width=1,
            colour = "white")+
  scale_fill_gradient2(
    low = "darkgreen",
    mid = "white",
    high = "orange",
    midpoint = median(dt1$abundance),
    na.value = "darkgreen",
    breaks = waiver(),
    transform = "log10",
    name="Abundance (Log10 (x))")+
  scale_x_discrete(position = "top")+
  scale_y_discrete(position = "left")+
  xlab('')+ylab('')+
  theme(axis.text.x.top = element_text(angle = 90,face = "italic",
                                       size=8,
                                       hjust=0,vjust = 0.5),
        axis.text.y.left = element_text(angle = 0,face = "plain",
                                         size=8,
                                         hjust=1,vjust = 0.5),
        panel.background = element_blank(),
        legend.title = element_text(size = 8),
        legend.position="right")+
  guides(fill = guide_colourbar(direction = "vertical",
                                title.hjust=0,
                                title.position ="top",
                                ticks.colour="white",
                                frame.colour="white",
                                barheight=5,
                                barwidth=0.7))+
  coord_fixed(ratio = 1,expand=T)
p1

pdf("Fig4d.pdf")
p1
dev.off()







