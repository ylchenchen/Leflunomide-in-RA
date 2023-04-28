rm(list = ls())
options(stringsAsFactors = F)
getwd()
setwd(dir = '/home/data/ylchen/RA-DRUG/code')

gene <- read.table('../data/DMP.Sig.Gene.txt')
head(gene)

library(ggplot2)
library(clusterProfiler)
library(org.Hs.eg.db)
df <- bitr(unique(gene$V1), fromType = "SYMBOL",
           toType = c( "ENTREZID"),
           OrgDb = org.Hs.eg.db)
head(df)
write.csv(df,file = '../data/kegg_dmg.csv')

DEG <- gene
head(DEG)
DEG=merge(DEG,df,by.y='SYMBOL',by.x='V1')
head(DEG)
dim(DEG)
gene_all <- as.character(DEG[ ,'ENTREZID'] )

# source('kegg_and_go_up_and_down.R')
kegg <- enrichKEGG(gene_all, pvalueCutoff = 0.05,organism   = 'hsa',qvalueCutoff  = 0.05)
# dotplot(kegg,showCategory = 10)

library(DOSE)
r <- kegg@result
head(r)
r_0.05 <- r[r$pvalue<0.05,]
r_0.05$GeneRatio <- parse_ratio(r_0.05$GeneRatio)
r_0.05[1,2]='Aspartate and glutamate metabolism'

library(ggprism)
jpeg("../pic/kegg_dotplot.jpg",units="in", width=7, height=5,res=650)
## 绘制气泡图
{p <- ggplot(r_0.05, aes(x = GeneRatio, y = Description, size = Count, color=pvalue)) + 
  geom_point() +xlab("GeneRatio") +ylab(" ")
p
## 修改气泡颜色
p + scale_color_gradient(low='red',high='blue')+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'),
        axis.text.x = element_text(size = 13,face = 'bold'),
        axis.text.y = element_text(size = 13,face = 'bold'),
        axis.title.x = element_text(size = 13,face = 'bold'),
        axis.title.y = element_text(size=15,face = 'bold'),
        legend.text=element_text(size = 10))} #空白背景
dev.off()
