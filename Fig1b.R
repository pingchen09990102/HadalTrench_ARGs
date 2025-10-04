###气泡图###

library(plyr)
library(stringr)
library(ggplot2)
library(grid)
library(tidyr)
library(dplyr)
library(cowplot)

setwd("$HOMME/HadalTrench_ARGs/Fig1b")
df <- read.table("Fig1b.sourceData.txt", header = TRUE, row.names = 1,sep = "\t")
MT16<-df[,1:4]
MT17<-df[,5:7]
MT18<-df[,8:10]
MT19<-df[,11:12]
MT20<-df[,13:14]
MT21<-df[,15:18]
MT16_AVE <- apply(df[,1:4], 1, mean)
MT17_AVE <- apply(df[,5:7], 1, mean)
MT18_AVE <- apply(df[,8:10], 1, mean)
MT19_AVE <- apply(df[,11:12], 1, mean)
MT20_AVE <- apply(df[,13:14], 1, mean)
MT21_AVE <- apply(df[,15:18], 1, mean)
new_df <- data.frame(
  MT16 = MT16_AVE,
  MT17 = MT17_AVE,
  MT18 = MT18_AVE,
  MT19 = MT19_AVE,
  MT20 = MT20_AVE,
  MT21 = MT21_AVE
)
rownames(new_df) <- rownames(df)
rows_to_keep <- rowSums(new_df) != 0
new_df_filtered <- new_df[rows_to_keep, ]
head(new_df_filtered )
ARG_type <- row.names(new_df_filtered )
new_df1 <- data.frame(ARG_type ,new_df_filtered )
new_df2<- new_df1 %>% pivot_longer(
  cols = !ARG_type,
  values_drop_na = FALSE,
  names_to = "Trench_Group",
  values_to = "Value")
summary(new_df2$Value)
max(new_df2$Value)
#enrich Plot
enrich_Plot <- ggplot(new_df2, aes(x = Trench_Group, y = ARG_type)) +
  geom_point(aes(size = Value, colour = Trench_Group)) +
  scale_colour_manual(
    values = c("#fff9c7", "#fcdaae", "#f09648", "#e85a26", "#c11920", "#ae4a21")
  ) +
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.text = element_text(colour = 'black', size = 9),
    legend.position = "left",    
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  ) +
  labs(size = "Relative Abundance (copy/cell)") +
  scale_size(
    range = c(1, 10),
    breaks = c(0.001, 0.004, 0.008, 0.015),
    limits = c(0.0000000001, 0.015)
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 5))
  )
print(enrich_Plot)

new_df1$Average_Abundance <- rowMeans(new_df1[, -1])
ARG_type <- row.names(new_df1)
new_df1$ARG_type<-ARG_type
bar_plot <- ggplot(new_df1, aes(x = Average_Abundance, y =  ARG_type)) +
  geom_col(fill = "#f09648", alpha = 0.8, width = 0.5) +
  labs(x = "Average Abundance (copy/cell)",
       y = NULL) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.y = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black", size = 0.5), 
    ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)))
print(bar_plot)

combined_plot <- plot_grid(
  enrich_Plot, 
  bar_plot, 
  nrow = 1, 
  align = "h", 
  axis = "tb",
  rel_widths = c(1, 0.5)  
)

final_plot <- plot_grid(
  combined_plot,
  ncol = 1
)
print(final_plot)

# Save at pdf
ggsave(final_plot, file="Fig1b.pdf")


