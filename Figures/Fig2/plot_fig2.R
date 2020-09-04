library(ggpubr)
library(ggplot2)
library(cowplot)
library(nlme)
library(plyr)
require(reshape2)
library(gridExtra)
library(cowplot)
library(survival)
library('survminer')
library(scales)
library('dplyr')
library(ggbeeswarm)



cbPalette2<-c("deepskyblue","deepskyblue4")
dat3<-read.table("./protein_expression_to_plot.txt",header=T)
dat3$type<-factor(dat3$type,levels = c("fs_indel_not_expressed","fs_indel_expressed"),ordered = TRUE)
p_prot<-ggplot(data=dat3,aes(x=type, y=norm_prot)) + geom_boxplot(aes(fill = type),outlier.shape = NA)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means()+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+xlab("")+geom_quasirandom(color="grey",size=0.5)

