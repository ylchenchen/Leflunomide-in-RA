rm(list = ls())
options(stringsAsFactors = F)
# data input
load('data/hub_methy.rdata')
head(hub_methy)

# hubgene
gene <- c('cg17330251_31126','cg19814518_297',
          'cg20124410_58_NEW60','cg21109666_5197',
          'cg22572476_2487','cg23403192_69_NEW218',
          'cg24432675_44129')
hub_methy <- hub_methy[,gene]
out=hub_methy

# 850k
df <- read.csv('data/Different_Methylation.csv',header = T,row.names = 1)
head(df)

gene <- colnames(out)
gene
gene <- substring(gene,1,10)
gene
colnames(out) <- gene
head(out)

df <- df[gene,]
df <- df[,6:45]
colnames(df)
colnames(df) <- gsub('...\\.','',colnames(df))

df <- as.data.frame(t(df))
table(rownames(df) %in% rownames(out))

# 验证对应的40样本：eightm
# eightm：列明基因 行名样本
eightm <- out[rownames(df),]
identical(rownames(eightm),rownames(df))

for (i in 1:length(df)) {
  # i=1
  same <- data.frame(eight=df[,i],
                     methy=eightm[,i])
  
  library(ggplot2)
  library(hrbrthemes)
  #library(gcookbook)
  library(tidyverse)
  class='f2'
  ggplot(same, aes(eight, methy)) +
    geom_jitter(aes(color=class, fill=class), size=3, shape=21, alpha=1/2)+
    #geom_point() +
    scale_color_ipsum()+
    scale_fill_ipsum()+
    labs(x="850k", y="methyltarget",
         subtitle=colnames(out)[i]) + 
    # theme_ipsum()+
    theme_classic()+stat_cor(method ="pearson")+
    theme(panel.grid.major=element_blank(),
          legend.position = 'none',
          panel.grid.minor=element_line(colour='white'),
          panel.background=element_rect(fill='white'))
  ggsave(paste0('pic/com_',colnames(out)[i],'.pdf'),width = 3,height = 3)
  
}
  # i=1
  same <- data.frame(eight=df[,i],
                     methy=eightm[,i])
  
  library(ggplot2)
  library(hrbrthemes)
  #library(gcookbook)
  library(tidyverse)
  class='f2'
  ggplot(same, aes(eight, methy)) +
    geom_jitter(aes(color=class, fill=class), size=3, shape=21, alpha=1/2)+
    #geom_point() +
    scale_color_ipsum()+
    scale_fill_ipsum()+
    labs(x="850k", y="methyltarget",
         subtitle=colnames(out)[i]) + 
    # theme_ipsum()+
    theme_classic()+stat_cor(method ="pearson")+
    theme(panel.grid.major=element_blank(),
          legend.position = 'none',
          panel.grid.minor=element_line(colour='white'),
          panel.background=element_rect(fill='white'))
  ggsave(paste0('pic/com_',colnames(out)[i],'.pdf'),width = 3,height = 3)
  
}
