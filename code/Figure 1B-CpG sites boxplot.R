rm(list = ls())
options(stringsAsFactors = F)
# 1. 表达量
df <- read.csv('../data/Different_Methylation.csv',row.names = 1)
colnames(df)
table(df$UCSC_RefGene_Group)
df=df[,grep('EXP|CON|(UCSC_RefGene_Group)',colnames(df))]
df=df[,-2]
ng=names(table(df$UCSC_RefGene_Group))
# [1] "1stExon" "3'UTR"   "5'UTR"   "Body"    "IGR"    
# [6] "TSS1500" "TSS200" 

i=3
for (i in 1:length(ng)) {
  # i=1 
  s=ng[i]
  out <- df[df$UCSC_RefGene_Group==s,]
  out <- as.data.frame(t(out))
  a=apply(out, 2, as.numeric)
  a <- as.data.frame(a)
  a$mean=apply(a,1,mean)
  a=a[-1,]
  a$group=c(rep('Responders',20),rep('Nonresponders',20))
  
  library(ggstatsplot)
  library(ggpubr)
  p1 = ggbetweenstats(
    data = a,
    x = 'group',
    y = 'mean',
    #title = y[i],
    caption = NULL,
    subtitle=NULL,
    type = "np", # 算法 非参
    ylab = 'Mean β value',
    xlab = s,
    median.plotting=F,
    pairwise.comparisons = TRUE,
    results.subtitle=F, # 结果描述
    centrality.label.args = list(size=5),# 平均数字体大小
    #centrality.plotting=F, #显示中位数
    ggtheme = theme(panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = 'black'),
                    axis.text.x = element_blank(),
                    axis.title.x = element_text(size = 15),
                    axis.title.y = element_text(size=15),
                    legend.text=element_text(size = 10)) #空白背景
    # package = "ggsci",
    # palette = "default_jco"
  )
  p1
  my_comparisons=list(c('Nonresponders','Responders'))#指定比较对象
  p1+stat_compare_means(comparisons = my_comparisons,
                               label = "p.signif")
  ggsave(filename = paste0('../pic/locations of CpG sites/box_',s,'.pdf'),
         units = 'cm',width = 10,height = 9)
  # dev.off()
}
  



# 所有dmp
for (i in length(ng)) {
  #i=7  
  # s=ng[i]
  # out <- df[df$UCSC_RefGene_Group==s,]
  out <- as.data.frame(t(df))
  a=apply(out, 2, as.numeric)
  a <- as.data.frame(a)
  a$mean=apply(a,1,mean)
  a=a[-1,]
  a$group=c(rep('Responders',20),rep('Nonresponders',20))
  
  library(ggstatsplot)
  library(ggpubr)
  s='All'
  p1 = ggbetweenstats(
    data = a,
    x = 'group',
    y = 'mean',
    #title = y[i],
    caption = NULL,
    subtitle=NULL,
    type = "np", # 算法 非参
    ylab = 'Mean β value',
    xlab = s,
    median.plotting=F,
    pairwise.comparisons = TRUE,
    results.subtitle=F, # 结果描述
    centrality.label.args = list(size=5),# 平均数字体大小
    cex.axis=8,
    #centrality.plotting=F, #显示中位数
    ggtheme = theme(panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = 'black'),
                    axis.text.x = element_blank(),
                    axis.title.x = element_text(size = 15),
                    axis.title.y = element_text(size=15),
                    legend.text=element_text(size = 10)) #空白背景
    # package = "ggsci",
    # palette = "default_jco"
  )
  p1
  my_comparisons=list(c('Nonresponders','Responders'))#指定比较对象
  p1+stat_compare_means(comparisons = my_comparisons,
                        label = "p.signif")
  ggsave(filename = paste0('../pic/locations of CpG sites/box_',s,'.pdf'),
         units = 'cm',width =10,height = 9)
  dev.off()
}

