library(ggplot2)
library(tidyr)
library(dplyr)

setwd("$HOME/HadalTrench_ARGs/Fig1d")
data <- read.delim("Fig1d.sourceData.txt", header = TRUE, sep = "\t")

data_long <- data %>%
  pivot_longer(cols = -Mechanism_group, 
               names_to = "Sample", 
               values_to = "Value")

mechanism_order <- rev(c("Unknown",
                         "Reduced permeability",
                         "Enzymatic inactivation",
                         "Efflux pump",
                         "Antibiotic target replacement",
                         "Antibiotic target protection",
                         "Antibiotic target alteration"))

data_long$Mechanism_group <- factor(data_long$Mechanism_group, levels = mechanism_order)

ggplot(data_long, aes(x = Value, y = Sample, fill = Mechanism_group)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c(
    "Antibiotic target alteration" = "#FFDD8E",
    "Antibiotic target protection" = "#8FB4DC",
    "Antibiotic target replacement" = "#70CDBE",
    "Efflux pump" = "#EB7E60",
    "Enzymatic inactivation" = "#F5AA61",
    "Reduced permeability" = "#7AC3DF",
    "Unknown" = "#ae4a21"
  )) +
  labs(
    x = "Relative Abundance (%)",
    y ="",
    fill = "Mechanism Group"
  ) +
  scale_x_continuous(breaks = seq(0, 100, by = 20)) +  
  theme_minimal() +
  theme(
    axis.line.y = element_line(color = "black"), 
    axis.line.x = element_line(color = "black"), 
    axis.ticks.x = element_line(color = "black", size = 0.5),  
    #axis.text.y = element_text(hjust = 1),  
    legend.position = "right"
  )

ggsave("antibiotic_resistance_mechanisms.pdf", width = 10, height = 6, dpi = 300)
