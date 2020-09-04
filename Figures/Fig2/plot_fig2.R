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

# panel B
dat4<-read.table("./VAF_by_exon_dat.txt",header=T,sep='\t')
dat4$exon_group<-factor(dat4$exon_group,levels = c("first","middle","penultimate_50bp_plus","penultimate_within_50bp","last"),ordered = TRUE)
ggplot(data=dat4,aes(x=exon_group, y=VAF)) + geom_boxplot(aes(fill = exon_group),outlier.shape = NA)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means()+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+xlab("")+geom_quasirandom(color="grey",size=0.5)+ scale_fill_brewer(palette="Blues")


# panel C
cbPalette2<-c("deepskyblue","deepskyblue4")
dat3<-read.table("./protein_expression_to_plot.txt",header=T)
dat3$type<-factor(dat3$type,levels = c("fs_indel_not_expressed","fs_indel_expressed"),ordered = TRUE)
p_prot<-ggplot(data=dat3,aes(x=type, y=norm_prot)) + geom_boxplot(aes(fill = type),outlier.shape = NA)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means()+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+xlab("")+geom_quasirandom(color="grey",size=0.5)

