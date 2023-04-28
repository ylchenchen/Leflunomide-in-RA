rm(list = ls())
options(stringsAsFactors = F)
getwd()
setwd(dir = '/home/data/ylchen/RA-DRUG/code')

gene <- read.table('../data/DMP.Sig.Gene.txt')
head(gene)

# change ID
library(ggplot2)
library(clusterProfiler)
library(org.Hs.eg.db)
df <- bitr(unique(gene$V1), fromType = "SYMBOL",
           toType = c( "ENTREZID"),
           OrgDb = org.Hs.eg.db)
head(df)

DEG <- gene
head(DEG)
DEG=merge(DEG,df,by.y='SYMBOL',by.x='V1')
head(DEG)
dim(DEG)
gene_all <- as.character(DEG[ ,'ENTREZID'] )

go_MF <- enrichGO(gene_all, 
                  pvalueCutoff = 0.05,
                  qvalueCutoff  = 0.05,
                  readable=T,
                  OrgDb = org.Hs.eg.db,
                  ont='MF')
library(DOSE)
r <- go_MF@result
write.csv(r,'../data/go_MF.csv')

go_BP <- enrichGO(gene_all, 
                  pvalueCutoff = 0.05,
                  qvalueCutoff  = 0.05,
                  readable=T,
                  OrgDb = org.Hs.eg.db,
                  ont='BP')
R_BP <- go_BP@result
write.csv(R_BP,'../data/go_BP.csv')

go_CC <- enrichGO(gene_all, 
                  pvalueCutoff = 0.05,
                  qvalueCutoff  = 0.05,
                  readable=T,
                  OrgDb = org.Hs.eg.db,
                  ont='CC')
R_CC <- go_CC@result
write.csv(R_CC,'../data/go_CC.csv')

#所需R包载入：
library(dplyr)
library(ggplot2)
library(RColorBrewer)
# 载入go结果
go_MF <- read.csv('../data/go_MF.csv')
go_BP <- read.csv('../data/go_BP.csv')
go_CC <- read.csv('../data/go_CC.csv')
head(go_BP)
colnames(go_BP)

result=go_BP
pro <- 'Biological Process'

result=go_MF
pro <- 'Molecular Function'

result=go_CC
pro <- 'Cellular Component'
##计算Rich Factor（富集因子）：
result <- mutate(result,
                      RichFactor = Count / as.numeric(sub("/\\d+", "", BgRatio)))
##计算Fold Enrichment（富集倍数）：
result$FoldEnrichment <- apply(result,1,function(x){
  GeneRatio <- eval(parse(text = x["GeneRatio"]))
  BgRatio <- eval(parse(text = x["BgRatio"]))
  foldEnrichment <- round(GeneRatio/BgRatio,2)
  foldEnrichment
})
head(result$RichFactor)
head(result$FoldEnrichment)
colnames(result)

#提取结果前Top20绘图(或自定义所需pathway绘图)：
top20 <- result[1:10,]
#指定绘图顺序（转换为因子）：
top20$pathway <- factor(top20$Description,levels = rev(top20$Description))

######
#1.常规画法：
#自定义主题：
mytheme <- theme(axis.title = element_text(size = 13),
                 axis.text = element_text(size = 11),
                 plot.title = element_text(size = 14,
                                           hjust = 0.5,
                                           face = "bold"),
                 legend.title = element_text(size = 13),
                 legend.text = element_text(size = 11))
#Top20富集数目条形图：
p <- ggplot(data = top20,
            aes(x = Count, y = pathway, fill = -log10(pvalue)))+
  scale_fill_distiller(palette = "RdPu",direction = 1) +
  geom_bar(stat = "identity", width = 0.8) +
  labs(x = "Number of Gene", y = "pathway", title = pro) +
  theme_bw() + mytheme
q <- p + aes(stringr::str_wrap(pathway, 25), Count)+
  coord_flip()+xlab('Pathway') +
  ylab('Number of Gene')+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 15))
q


ggsave(q,
       filename = paste0('../pic/go_',pro,'.pdf'),
       units = 'in',width = 8,height = 8)
