library(tidyverse)
library(RColorBrewer)

setwd("$HOME/HadalTrench_ARGs/Fig5c")
mge_data <- read.delim("Fig5c.sourceData.txt", header = TRUE, sep = "\t") %>%
  group_by(Trench) %>%
  mutate(Relative_Abundance = RPKM / sum(RPKM) * 100) %>%
  ungroup()


mge_order <- c("integration/excision", "phage", "replication/recombination/repair",
               "stability/transfer/defense", "transfer")
mge_data$MGE.category <- factor(mge_data$MGE.category, levels = mge_order)

trench_order <- c("DT", "KT", "MST", "MT", "NBT", "Yap")
mge_data$Trench <- factor(mge_data$Trench, levels = trench_order)

mge_colors <- c(
  "integration/excision" = "#FB8072",
  "phage" = "#FDB462",
  "replication/recombination/repair" = "#FCCDE5",
  "stability/transfer/defense" = "#8DD3C7",
  "transfer" = "#BC80BD"
)

ggplot(mge_data, aes(y = Trench, x = Relative_Abundance, fill = MGE.category)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5, color = NA) + 
  scale_fill_manual(values = mge_colors) +
  labs(y = NULL, x = "Relative Abundance (%)", fill = "MGE Category") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) + 
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.margin = margin(10, 10, 10, 10),
    axis.text.y = element_text(hjust = 1), 
    axis.line.y = element_line(color = "black"), 
    axis.line.x = element_line(color = "black"), 
    axis.ticks.x = element_line(color = "black", size = 0.5)  
  ) +
  guides(fill = guide_legend(reverse = TRUE)) 


ggsave("Fig5c.pdf", width = 10, height = 6, dpi = 300)
