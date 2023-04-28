rm(list = ls())   
options(stringsAsFactors = F)
getwd()
library(data.table)

load('../data/step3-output-myDMP.Rdata')
df=myDMP[[1]]
head(df)
#DMP.GUI(DMP=myDMP[[1]],beta=myNorm,pheno=pD)


colnames(df)
max(df$P.Value)
table(df$type)
head(df$probe)

this_tile <- paste0('Cutoff for meth.diff is ',0.1,
                    '\nThe number of hyperDMP is ',nrow(df[df$type == 'Hypermethylated',]) ,
                    '\nThe number of hypoDMP is ',nrow(df[df$type == 'Hypomethylated',])
)
#火山图p5
library(ggplot2)
p5 <- ggplot(data = df, 
             aes(x = meth.diff, 
                 y = -log10(P.Value))) +
  geom_point(alpha=0.6, size=1.5, 
             aes(color=type)) +
  ylab("-log10(Pvalue)")+
  scale_color_manual(values=c("#34bfb5", "#828586","#ff6633"))+
  geom_vline(xintercept= 0,lty=4,col="grey",lwd=0.8) +
  xlim(-0.5, 0.5)+
  theme_classic()+
  ggtitle(this_tile )+
  theme(plot.title = element_text(size=12,hjust = 0.5),
        legend.title = element_blank(),
  )
p5
ggsave('deg_volcano.pdf',units = 'cm',width = 15,height =11)
dev.off()
