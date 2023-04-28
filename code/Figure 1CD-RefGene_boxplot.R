rm(list = ls())
options(stringsAsFactors = F)
# 1. 表达量
df <- read.csv('../data/Different_Methylation.csv',row.names = 1)
colnames(df)
library(ggplot2)
library(reshape2)
library(dplyr)
data <- df %>% group_by(Type) %>% count(UCSC_RefGene_Group) 

library(ggsci)
library(ggprism)
ggplot(data=data, aes(x=UCSC_RefGene_Group,y=n,fill = Type),xla)+
  geom_bar(stat='identity',position = 'fill')+
  ylab('proportion')+xlab('')+
  theme_prism()+
  scale_fill_npg()+
  theme(axis.text.x = element_text(angle =45,hjust=1,vjust = 1,size=13),
        axis.title.y = element_text(size=13))
ggsave(filename = '../pic/RefGene_boxplot.pdf',
       units = 'cm',width =18,height = 14)

#############Relation_to_UCSC_CpG_Island
data <- df %>% group_by(Type) %>% count(Relation_to_UCSC_CpG_Island) 

library(ggsci)
library(ggprism)
ggplot(data=data, aes(x=Relation_to_UCSC_CpG_Island,y=n,fill = Type),xla)+
  geom_bar(stat='identity',position = 'fill')+
  ylab('proportion')+xlab('')+
  theme_prism()+
  scale_fill_npg()+
  theme(axis.text.x = element_text(angle =45,hjust=1,vjust = 1,size=13),
        axis.title.y = element_text(size=13))
ggsave(filename = '../pic/CpG_Island_boxplot.pdf',
       units = 'cm',width =18,height = 14)
    

