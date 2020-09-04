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

# Snyder
dat<-read.table("./cpi_snyder.txt",header=T)
dat$benefit<-factor(dat$benefit,levels = c("no-benefit","benefit"),ordered = TRUE)
cb_snv_sny<-ggplot(data=dat,aes(x=benefit, y=nsSNV,color=benefit))+geom_boxplot()+geom_quasirandom(size = 3, shape=18)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means(comparisons = my_comparisons,)+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+ ggtitle("Snyder et al (n=21)")+xlab("")
cb_fs_sny<-ggplot(data=dat,aes(x=benefit, y=fs_indel,color=benefit))+geom_boxplot()+geom_quasirandom(size = 3, shape=18)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means(comparisons = my_comparisons,)+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+xlab("")
cb_nmd_sny<-ggplot(data=dat,aes(x=benefit, y=nmd_escape,color=benefit))+geom_boxplot() +geom_quasirandom(size = 3, shape=18)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means(comparisons = my_comparisons,)+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+xlab("Anti-CTLA4")+scale_y_continuous(limits=c(0,3))
cb_exsnv_sny<-ggplot(data=dat,aes(x=benefit, y=exp_nsSNV,color=benefit))+geom_boxplot() +geom_quasirandom(size = 3, shape=18)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means(comparisons = my_comparisons,)+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+xlab("")

# VA
dat<-read.table("./cpi_VA.txt",header=T)
dat$benefit<-factor(dat$benefit,levels = c("no-benefit","benefit"),ordered = TRUE)
cb_snv_VA<-ggplot(data=dat,aes(x=benefit, y=nsSNV,color=benefit))+geom_boxplot() +geom_quasirandom(size = 3, shape=18)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means(comparisons = my_comparisons,)+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+ ggtitle("Van Allen et al (n=33)")+xlab("")
cb_fs_VA<-ggplot(data=dat,aes(x=benefit, y=fs_indel,color=benefit))+geom_boxplot() +geom_quasirandom(size = 3, shape=18)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means(comparisons = my_comparisons,)+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+xlab("")
cb_nmd_VA<-ggplot(data=dat,aes(x=benefit, y=nmd_escape,color=benefit))+geom_boxplot() +geom_quasirandom(size = 3, shape=18)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means(comparisons = my_comparisons,)+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+xlab("Anti-CTLA4")+scale_y_continuous(breaks = c(0,2, 4, 6,8))
cb_exsnv_VA<-ggplot(data=dat,aes(x=benefit, y=exp_nsSNV,color=benefit))+geom_boxplot()+geom_quasirandom(size = 3, shape=18)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means(comparisons = my_comparisons,)+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+xlab("")

# Hugo
dat<-read.table("./cpi_hugo.txt",header=T)
dat$benefit<-factor(dat$benefit,levels = c("no-benefit","benefit"),ordered = TRUE)
cb_snv_Hu<-ggplot(data=dat,aes(x=benefit, y=nsSNV,color=benefit))+geom_boxplot() +geom_quasirandom(size = 3, shape=18)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means(comparisons = my_comparisons,)+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+ ggtitle("Hugo et al (n=25)")+xlab("")
cb_fs_Hu<-ggplot(data=dat,aes(x=benefit, y=fs_indel,color=benefit))+geom_boxplot() +geom_quasirandom(size = 3, shape=18)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means(comparisons = my_comparisons,)+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+xlab("")
cb_nmd_Hu<-ggplot(data=dat,aes(x=benefit, y=nmd_escape,color=benefit)) +geom_boxplot()+geom_quasirandom(size = 3, shape=18)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means(comparisons = my_comparisons,)+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+xlab("Anti-PD1")
cb_exsnv_Hu<-ggplot(data=dat,aes(x=benefit, y=exp_nsSNV,color=benefit)) +geom_boxplot()+geom_quasirandom(size = 3, shape=18)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means(comparisons = my_comparisons,)+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+xlab("")

# Riaz
dat<-read.table("./cpi_riaz.txt",header=T)
dat$benefit<-factor(dat$benefit,levels = c("no-benefit","benefit"),ordered = TRUE)
cb_snv_ri<-ggplot(data=dat,aes(x=benefit, y=nsSNV,color=benefit))+geom_boxplot() +geom_quasirandom(size = 3, shape=18)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means(comparisons = my_comparisons,)+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+ ggtitle("Riaz et al (n=26)")+xlab("")
cb_fs_ri<-ggplot(data=dat,aes(x=benefit, y=fs_indel,color=benefit))+geom_boxplot() +geom_quasirandom(size = 3, shape=18)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means(comparisons = my_comparisons,)+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+xlab("")
cb_nmd_ri<-ggplot(data=dat,aes(x=benefit, y=nmd_escape,color=benefit)) +geom_boxplot()+geom_quasirandom(size = 3, shape=18)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means(comparisons = my_comparisons,)+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+xlab("Anti-PD1")+scale_y_continuous(limits=c(0,3))
cb_exsnv_ri<-ggplot(data=dat,aes(x=benefit, y=exp_nsSNV,color=benefit))+geom_boxplot()+geom_quasirandom(size = 3, shape=18)+ theme(legend.position = "none",text = element_text(size=10),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))+stat_compare_means(comparisons = my_comparisons,)+scale_fill_manual(values=cbPalette2)+scale_color_manual(values = cbPalette2)+ theme(plot.margin = unit(c(1,1,1,1), "lines"))+xlab("")

plot_grid(cb_snv_VA,cb_snv_sny,cb_snv_Hu,cb_snv_ri,cb_exsnv_VA,cb_exsnv_sny,cb_exsnv_Hu,cb_exsnv_ri,cb_fs_VA,cb_fs_sny,cb_fs_Hu,cb_fs_ri,cb_nmd_VA,cb_nmd_sny,cb_nmd_Hu,cb_nmd_ri,nrow=4)

ggsave("./combined_cpi_cohorts_not_violin.pdf",dpi=600)
library("CombinePValue")
selfcontained.test(pvalue=c(0.097,0.26,0.18,0.17),weight=NA,p_permu=NA)
selfcontained.test(pvalue=c(0.13,0.29,0.46,0.21),weight=NA,p_permu=NA)
selfcontained.test(pvalue=c(0.056,0.24,0.1,0.46),weight=NA,p_permu=NA)
selfcontained.test(pvalue=c(0.021,0.081,0.065,0.11),weight=NA,p_permu=NA)
