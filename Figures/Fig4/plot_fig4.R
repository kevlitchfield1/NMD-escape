
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

rm(list=ls(all=TRUE))
my_comparisons <- list( c("no-benefit","benefit"))
cbPalette2 <- c("darkred","darkblue")

# ACT
my_comparisons <- list( c("no_benefit","benefit"))
dat<-read.table("NMD/act_nmd_data.txt",header=T)
dat$response<-factor(dat$response,levels = c("no_benefit","benefit"),ordered = TRUE)
ac_snv_ri<-ggplot(data=dat,aes(x=response, y=TMB,color=response))+geom_boxplot(outlier.shape = NA) +geom_quasirandom(size = 3, shape=18)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means(comparisons = my_comparisons,)+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+ ggtitle("")+xlab("")
ac_fs_ri<-ggplot(data=dat,aes(x=response, y=fs_indels,color=response))+geom_boxplot(outlier.shape = NA) +geom_quasirandom(size = 3, shape=18)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means(comparisons = my_comparisons,)+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+xlab("")
ac_nmd_ri<-ggplot(data=dat,aes(x=response, y=NMD_escape_fs,color=response)) +geom_boxplot(outlier.shape = NA)+geom_quasirandom(size = 3, shape=18)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means(comparisons = my_comparisons,)+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+xlab("")+scale_y_continuous(limits=c(0,3))
plot_grid(ac_snv_ri,ac_fs_ri,ac_nmd_ri,nrow=1)


