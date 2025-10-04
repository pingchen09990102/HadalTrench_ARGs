library(ggplot2)
library(pheatmap)
library(reshape2)
library(patchwork)

setwd("$HOME/HadalTrench_ARGs/Fig1c")
MT_subtypeTrendPlot_line <- read.table("Fig1c.sourceData.txt",sep = "\t",header = T, row.names = 1, quote = "\"")
head(MT_subtypeTrendPlot_line)
data_new = melt(MT_subtypeTrendPlot_line)
head(data_new)
pdf("MT_subtypeTrendPlot.pdf")
create_plot <- function(data, cluster_val, xlab = "", ylab = "") {
  ggplot(data, aes(variable, value, group=subtype)) + 
    geom_line(color="gray90", size=0.8) + 
    geom_hline(yintercept = 0, linetype=2) +
    stat_summary(aes(group=1), fun=mean, geom="line", size=1.2, color="#FB8072") + 
    labs(x = xlab, y = ylab, title = "") +  
    theme_bw() + 
    ylim(-1.2, 2.2) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text = element_text(size=5, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(size=8, face = "bold"),  
          strip.text = element_text(size = 8, face = "bold"),
          plot.title = element_text(size=10, face="bold", hjust=0.5))  
}

main_cluster <- "Cluster0"  
other_clusters <- unique(data_new$Cluster[data_new$Cluster != main_cluster])

main_plot <- create_plot(data_new[data_new$Cluster == main_cluster, ], 
                         main_cluster,
                         xlab = "", 
                         ylab = "Standardized abundance of ARG subtypes")

small_plots <- lapply(other_clusters, function(cl) {
  create_plot(data_new[data_new$Cluster == cl, ], 
              cl,
              xlab = "",  
              ylab = "")  
})
combined_plot <- main_plot / (small_plots[[1]] | small_plots[[2]] | small_plots[[3]] | small_plots[[4]]) 
combined_plot
dev.off()



