rm(list = ls())  ## 魔幻操作，一键清空~
options(stringsAsFactors = F)

load(file = '../data/RA_cli.rdata')
############ 连续变量两个，作t检验
############ 分类变量，卡方检验
{
tcp <- rp[,c('Age','Age at diagnosis','group')]

# 直方图观察数据
hist(tcp$Age,
     prob=T,
     col="light blue")

hist(tcp$`Age at diagnosis`,
     prob=T,
     col="light blue")

# 批量输出p
ttest_result <- list()

for (i in 1:(length(tcp)-1)){
  # i=1
  a=data.frame(c=tcp[[i]],g=tcp$group)
  a=na.omit(a)
  temp <- t.test(a$c~a$g)
  ttest_result[[i]] <- temp
}
names(ttest_result) <- names(tcp)[1:2]
}

{
  chi_cp <- rp[,-(2:3)]
  dim(chi_cp)
  colnames(chi_cp)
  
  chi_result <- list()
  for (i in 1:11) {
    #i=4
    a <- table(chi_cp[,i],chi_cp$group)
    print(a)
    chi <- chisq.test(a)
    chi_result[[i]] <- chi
  }
  names(chi_result) <- names(chi_cp)
}


