library(plyr)
library(stringr)
library(ggplot2)
library(grid)
library(tidyr)
library(dplyr)

setwd("$HOME/HadalTrench_ARGs/Fig4c")
rm(list=ls())

df <- read.table("Fig4c.sourceData.txt", header = TRUE, row.names = 1,sep = "\t")

DT <- df[, 1:4]
KT <- df[, 5:8]
MST <- df[, 9:11]
MT<- df[, 12:29]
NBT <- df[, 30:32]
Yap <- df[, 33:37]

DT_AVE <- rowMeans(df[, 1:4])
KT_AVE <- rowMeans(df[, 5:8])
MST_AVE <- rowMeans(df[, 9:11])
MT_AVE<- rowMeans(df[, 12:29])
NBT_AVE <- rowMeans(df[, 30:32])
Yap_AVE <- rowMeans(df[, 33:37])

new_df <- data.frame(
  DT = DT_AVE,
  KT = KT_AVE,
  MST = MST_AVE,
  MT = MT_AVE,
  NBT = NBT_AVE,
  Yap = Yap_AVE
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

p <- ggplot(new_df2, aes(x=Trench_Group,y=ARG_type)) +
  geom_point(aes(size = Value,colour=Trench_Group))+
  scale_colour_manual(values=c("#F7A24F","#FBEB66","#CC247C","#E95351","#4EA660","#AA77E9"))+
  theme_bw(base_size = 8)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  labs(size = "Relative Abundance(copy/cell)")+
  scale_size(range = c(1, 10),breaks=c(0.001,0.004,0.008,0.018), limits=c(0.0000000001,0.018))+
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme(axis.text=element_text(colour='black',size=9))
p

# Save at pdf
ggsave(p, file="Fig4c.pdf",width=6, height=8)





