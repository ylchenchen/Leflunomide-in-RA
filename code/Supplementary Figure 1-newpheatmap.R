rm(list = ls())
options(stringsAsFactors = F)
# 1. 表达量
df <- read.csv('../data/Different_Methylation.csv',row.names = 1)
colnames(df)
dat <- df[,6:45]

#上下调的差异基因热图p6
library(pheatmap)
pheatmap(dat,show_colnames =F,show_rownames = F) #对dat按照cg取行，所得到的矩阵来画热图

# 2. 调整grouplist
n=dat
group=substring(colnames(n),1,3)
table(group)

data1=n[,group=='CON']
data2=n[,group=='EXP']
normalCount=ncol(data1)
tumorCount=ncol(data2)
data=cbind(data1,data2)
colnames(data)

group_list <- c(rep('Nonresponders',20),rep('Responders',20))
group_list 

# 3. 其他注释信息
identical(rownames(df),rownames(data))
df$CHR <- paste0('chr',df$CHR)

bc=data.frame(chr=df$CHR,anno_gene=df$UCSC_RefGene_Group,
              CpG_island=df$Relation_to_UCSC_CpG_Island)
rownames(bc)=rownames(df)
table(bc$CpG_island)
bc <- bc[order(bc$CpG_island),] # 排序island
data <- data[rownames(bc),]
identical(rownames(bc),rownames(data))

#-----------------------
#
#         美化热图
#
#-----------------------
#BiocManager::install('ComplexHeatmap')
library(ComplexHeatmap)
library(ggsci)
#?Heatmap
library('RColorBrewer')
#getPalette = colorRampPalette(brewer.pal(9, "Paired"))
# 4. 颜色list
ann_colors = list(
  chr = pal_igv('default', alpha =0.8)(19),
  CpG_island = pal_npg("nrc", alpha =0.7)(6),
  anno_gene=pal_jama('default', alpha =0.8)(7)
)
names(ann_colors[["group"]])=c('Nonresponders','Responders')
names(ann_colors[["chr"]])=names(table(bc$chr))
names(ann_colors[["CpG_island"]])=names(table(bc$CpG_island))
names(ann_colors[["anno_gene"]])=names(table(bc$anno_gene))

# 5. 热图注释
cn=c(rep('Nonresponders',20),rep('Responders',20))
table(bc$CpG_island)
rn=c(rep('Island',13),rep('N_Shelf',3),
     rep('N_Shore',14),rep('opensea',46),
     rep('S_Shelf',2),rep('S_Shore',3)) # 虽然上面已排序但这里还是得特别强调，不然画图是随机排序
ha=HeatmapAnnotation(group=cn,annotation_name_side = "left",
                     col = list(group = c("Nonresponders" = "#F3BA2A", "Responders" = "#7FA6B3")),
                     show_legend = FALSE,# 添加注释
                     simple_anno_size =unit(4, "mm"))# 宽度
row_ha=rowAnnotation(chr=bc$chr,
                     anno_gene=bc$anno_gene,
                     CpG_island=rn,
                     #width = unit(2, "mm"),
                     col = ann_colors,
                     show_legend = FALSE,
                     simple_anno_size =unit(4, "mm")
)

col_fun=circlize::colorRamp2(c(0.1, 0.5, 1.0), c("#0571b0", "white", "#f46d43"))
lgd_list = list(
  Legend(col_fun=col_fun,title = 'score',direction = "horizontal"),# 连续变量方向
  Legend(labels = names(table(bc$chr)), title = "chr",legend_gp = gpar(fill=pal_igv('default', alpha =0.8)(19)),ncol = 5),# 分类变量
  Legend(labels = names(table(bc$anno_gene)), title = "anno_gene", legend_gp = gpar(fill=pal_jama('default', alpha =0.8)(7)),ncol = 3),
  Legend(labels = names(table(bc$CpG_island)), title = "CpG_island", legend_gp = gpar(fill=pal_npg("nrc", alpha =0.7)(6)),ncol = 2,direction = "horizontal"),
  Legend(labels = c("Nonresponders", "Responders"), title = "group",legend_gp = gpar(fill=c("#F3BA2A", "#7FA6B3")),direction = "horizontal")
)# 注释放在下面，特别设置


jpeg('../pic/deg_Heatmap.jpg',width = 10,height = 25,units = 'cm',res = 650)
{ht_list=Heatmap(data,name = 'score', #注释名
                col = circlize::colorRamp2(c(0.1, 0.5, 1.0), c("#0571b0", "white", "#f46d43")),
                show_column_names = F,
                show_column_dend = FALSE,
                show_row_dend = F,
                column_title = NULL,
                row_title = NULL,
                show_row_names = F,
                column_gap = unit(5, "mm"),#分割距离
                row_gap = unit(5, "mm"),
                column_split =cn,
                row_split = rn,
                top_annotation = ha,#上方分割注释
                left_annotation = row_ha,
                show_heatmap_legend = FALSE,
                heatmap_legend_param =list(direction = "horizontal"),
                rect_gp = gpar(col = "white", lwd = 1) #格子分割
) # 左边注释
draw(ht_list, heatmap_legend_side = "bottom", 
     annotation_legend_side = "bottom",
     annotation_legend_list = lgd_list)}

dev.off()


