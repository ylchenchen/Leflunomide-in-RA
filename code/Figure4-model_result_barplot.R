library(showtext)
font_add('Arial', 'arial/arial.ttf')
showtext_auto()

## 手动整理的数据
## 没有结合临床信息的，详见机器模型结果
df1 <- read.csv('data/machine_result1.csv')
df2 <- read.csv('data/machine_result2.csv')
df3 <- read.csv('data/machine_result3.csv')
head(df1)
library(tidyverse)
library(ggsci)
library(hrbrthemes)
library(cowplot)
f1 <- rbind(df1[1,],df2[1,],df3[1,])
head(f1)

f1[,1] <- c('train','test','total')
f1[,1] <- factor(f1[,1],levels = c('train','test','total'))
d <- pivot_longer(f1,-X,names_to = 'methods',values_to = 'score')
head(d)


acc <- rbind(df1[4,],df2[4,],df3[4,])
acc[,1] <- c('train','test','total')
acc[,1] <- factor(acc[,1],levels = c('train','test','total'))
d2 <- pivot_longer(acc,-X,names_to = 'methods',values_to = 'score')
head(d2)


p1 <- ggplot(data=d,aes(x=methods,y=score,fill=X))+
  geom_bar(stat="identity",width = 0.6,
           position = position_dodge(width=0.7))+
  theme_ipsum(grid='Y') +
  theme(legend.position = "none",
        axis.text.x = element_text(angle=0,hjust = 0.5,
                                   vjust = 0.5,face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        legend.title = element_blank(),
        legend.key.size = unit(0.5, 'cm'))+
  geom_text(aes(label = sprintf("%.2f", d$score)),
            position=position_dodge(width = 0.7),
            size = 3,vjust = -0.8,hjust=0.5,face='bold')+ ###########设置柱子上的标签文字，文字的position_dodge(width=0.5)设置，保证分隔宽度
  labs(x=NULL,y=NULL)+
  # scale_x_continuous(limits=c('0','1'))+
  # geom_vline(xintercept = 3.5,lty="dashed")+
  # geom_vline(xintercept = 5.5,lty="dashed")+
  # annotate(geom = "text",x=3,y=1.2,label="Train")+
  # annotate(geom = "text",x=4.5,y=9,label="P")+
  # annotate(geom = "text",x=8,y=9,label="T")+
  scale_fill_brewer(palette = "Set2")
p1
ggsave('pic/model_f1.pdf',units = 'in',width = 7,height = 2.5)


p2 <- ggplot(data=d2,aes(x=methods,y=score,fill=X))+
  geom_bar(stat="identity",width = 0.6,
           position = position_dodge(width=0.7))+
  theme_ipsum(grid='Y') +
  theme(legend.position = "none",
        axis.text.x = element_text(angle=0,hjust = 0.5,
                                   vjust = 0.5,face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        legend.title = element_blank(),
        legend.key.size = unit(0.5, 'cm'))+
  geom_text(aes(label = sprintf("%.2f", d2$score)),
            position=position_dodge(width = 0.7),
            size = 3,vjust = -0.8,hjust=0.5)+ ###########设置柱子上的标签文字，文字的position_dodge(width=0.5)设置，保证分隔宽度
  labs(x=NULL,y=NULL)+
  # geom_vline(xintercept = 3.5,lty="dashed")+
  # geom_vline(xintercept = 5.5,lty="dashed")+
  # annotate(geom = "text",x=3,y=1.2,label="Train")+
  # annotate(geom = "text",x=4.5,y=9,label="P")+
  # annotate(geom = "text",x=8,y=9,label="T")+
  scale_fill_brewer(palette = "Set2")
p2
ggsave('pic/model_acc.pdf',units = 'in',width = 7,height = 2.5)
