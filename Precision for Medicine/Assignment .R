source("https://bioconductor.org/biocLite.R")
biocLite("NanoStringQCPro")
install.packages("gdata")



install.packages("NanoStringNorm")
require('NanoStringNorm');
data("Precision_for_Medicine");
read.rcc

setwd("/Users/ishwar/Desktop/Assignments/Precision for Medicine/")

help(read.xls.RCC)


# alternatively you can import the dataset
data_file <- file.choose()
path.to.xls.file <- system.file(".", "RCC_files", "Precision_for_Medicine.xls" , package = "NanoStringNorm")
getwd()
NanoString.mRNA <- read.xls.RCC(xls =  = "Precision_for_Medicine.xls", sheet = 1,)

read.markup.RCC( rcc.path = ".", rcc.pattern = "*.RCC|*.rcc", exclude = NULL,  include = NULL, nprobes = -1)

data(NanoString);
path.to.xls.file <- system.file("extdata", "RCC_files", "RCCCollector1_rat_tcdd.xls", package = "NanoStringNorm");
NanoString.mRNA <- read.xls.RCC(x = path.to.xls.file, sheet = 1);
NanoString.mRNA[NanoString.mRNA$Name %in% c('Eef1a1','Gapdh','Hprt1','Ppia','Sdha'),'Code.Class'] <- 'Housekeeping'
NanoString.mRNA.norm <- NanoStringNorm( x = NanoString.mRNA, anno = NA, CodeCount = 'sum', Background = 'mean', SampleContent = 'housekeeping.sum', round.values = FALSE, take.log = FALSE, return.matrix.of.endogenous.probes = TRUE )

Plot.NanoStringNorm( x = NanoString.mRNA.norm,label.best.guess = TRUE,plot.type = c('volcano'), title = FALSE)




#read in all the rcc files in the current directory 
data.raw <- read.markup.RCC()
data.row.mRNA[data.raw$Name %in\% c('Eef1a1','Gapdh','Hprt1','Ppia','Sdha'),'Code.Class'] <- 'Housekeeping';
data.norm <- NanoStringNorm(data.raw)
#This function can be used to normalize mRNA and miRNA expression data from the NanoString platform.

data.norm <- NanoStringNorm(data.raw, anno = NA, CodeCount = 'sum', Background = 'mean', SampleContent = 'housekeeping.sum',round.values = FALSE,take.log = FALSE,return.matrix.of.endogenous.probes = TRUE)
#Plot all data
Plot.NanoStringNorm(x = data.norm,label.best.guess = TRUE,plot.type = 'all')




##############################################################

getwd()
setwd("/Users/ishwar/Desktop/Assignments/Precision for Medicine/")


rccDir <- system.file("RCC", package="case_study_datascientist_data")
rccSet <-  newRccSet(rccFiles = dir(rccDir, full.names=TRUE))







###############################################################
library(NanoStringQCPro)

require('NanoStringNorm')
data("NanoString")
precision_for_medicine_annotation <- read.csv2(file.choose(),header = TRUE,sep=",")

baseline <- subset(precision_for_medicine_annotation,visit == "Baseline")
post_treatment <- subset(precision_for_medicine_annotation, visit =="Post-Treatment")

rccDir <- system.file("RCC", package="case_study_datascientist_data")
rccSet <- newRccSet(rccFiles = dir(rccDir, full.names=TRUE) )

baseline_rcc <- newRccSet(rccFiles =baseline$sampleid)
help("newRccSet")



#########=====####################

precision_for_medicine_annotation <- read.csv2(file.choose(),header = TRUE,sep=",")
baseline <- subset(precision_for_medicine_annotation,visit == "Baseline")
post_treatment <- subset(precision_for_medicine_annotation, visit =="Post-Treatment")

getwd()
setwd("/Users/ishwar/Desktop/Assignments/Precision for Medicine/")

baseline.mRNA <- read.markup.RCC(rcc.path = "case_study_datascientist_data/base_line",rcc.pattern = "*.RCC|*.rcc",exclude = NULL,include = NULL,nprobes = -1)

post_treatment.mRNA <- read.markup.RCC(rcc.path = "case_study_datascientist_data/post_treatment",rcc.pattern = "*.RCC|*.rcc",exclude = NULL,include = NULL,nprobes = -1)
baseline.mRNA

##baseline.mRNA[baseline.mRNA$Name %in% c('Eef1a1','Gapdh','Hprt1','Ppia','Sdha'),'Code.Class'] <- 'Housekeeping'


baseline.mRNA.norm <- NanoStringNorm(
  x = baseline.mRNA,
  anno = NA,
  CodeCount = 'geo.mean',
  Background = 'mean',
  SampleContent = 'housekeeping.geo.mean',
  round.values = TRUE,
  take.log = TRUE,
  return.matrix.of.endogenous.probes = TRUE
)


post_treatment.mRNA.norm <- NanoStringNorm(
  x = post_treatment.mRNA,
  anno = NA,
  CodeCount = 'geo.mean',
  Background = 'mean',
  SampleContent = 'housekeeping.geo.mean',
  round.values = TRUE,
  take.log = TRUE,
  return.matrix.of.endogenous.probes = TRUE
)

#heatmap

# load data
#require(gdata)
#require(vsn)

#install.packages("permute")
#library(permute)

#install.packages("lattice") 
#library(vegan)
#install.packages("NanoStringNormCNV", repos = "http://bpg.oicr.on.ca")
#library(NanoStringNormCNV)
library("nlme")

#
heatmap(t(baseline.mRNA.norm))
heatmap(t(post_treatment.mRNA.norm))


# plot raw NanoString counts
# make sure raw count data frame has gene names for rownames!
#http://bpg.oicr.on.ca/API/NanoStringNormCNV/1.1.0/make.counts.heatmap.html
 baseline.mRNA.formatted <- baseline.mRNA[, -(1:3)];
 rownames(baseline.mRNA.formatted) <- baseline.mRNA$Name;


 install.packages("tidyr")
 library(dplyr)
 library(tidyr)

#Plot baseline and posttreatment 
#baseline
 library(ggplot2)
 
 #CXCL1 values differnce between Baseline/Post-Treatment sample 
 data_CXCL1 <- rbind(data.frame(sample = baseline$subjectid ,time_point = baseline$visit,CXCL1 = baseline.mRNA.norm["CXCL1",]),data.frame(sample= post_treatment$subjectid,time_point = post_treatment$visit,CXCL1 = post_treatment.mRNA.norm["CXCL1",]))
 plot_CXCL1 <- ggplot(data_CXCL1, aes(x=sample,y = CXCL1,fill = time_point)) + geom_bar(stat="identity",position=position_dodge(),colour="black") + labs (x= "Sample Baseline/Post-Treatment", y = "CXCL1") + ggtitle("CXCL1 values differnce between Baseline/Post-Treatment sample")
 
 #MCL1 values differnce between Baseline/Post-Treatment sample 
 data_MCL1 <- rbind(data.frame(sample = baseline$subjectid ,time_point = baseline$visit,MCL1 = baseline.mRNA.norm["MCL1",]),data.frame(sample= post_treatment$subjectid,time_point = post_treatment$visit,MCL1 = post_treatment.mRNA.norm["MCL1",]))
 plot_MCL1 <- ggplot(data_MCL1, aes(x=sample,y = MCL1,fill = time_point)) + geom_bar(stat="identity",position=position_dodge(),colour="black") + labs (x= "Sample Baseline/Post-Treatment", y = "MCL1") + ggtitle("MCL1 values differnce between Baseline/Post-Treatment sample")
 
 grid.arrange(plot_CXCL1, plot_MCL1, ncol=2)
 
 
 
  ggplot(data = data_CXCL1_MCL1, aes(x=sample, y=CXCL1))  + geom_boxplot(aes(fill=time_point),stat = "boxplot") + labs (x= "Sample Baseline/Post-Treatment", y = "CXCL1") + ggtitle("CXCL1 values differnce between Baseline/Post-Treatment sample") + stat_summary(fun.data =min, geom = "boxplot")
 
 install.packages("ggrepel")
 library(ggrepel)
 
 #CXCL1 values differnce between Baseline/Post-Treatment sample 
 data_CXCL1_MCL1 <- rbind(data.frame(sample = baseline$subjectid ,time_point = baseline$visit,CXCL1 = baseline.mRNA.norm["CXCL1",],MCL1 = baseline.mRNA.norm["MCL1",]),data.frame(sample= post_treatment$subjectid,time_point = post_treatment$visit,CXCL1 = post_treatment.mRNA.norm["CXCL1",],MCL1 = post_treatment.mRNA.norm["MCL1",]))
 
 pCXCL1 <- ggplot(data = data_CXCL1_MCL1, aes(x=sample, y=CXCL1))  + geom_boxplot(aes(fill=time_point),stat = "boxplot") 

 pMCL1 <- ggplot(data = data_CXCL1_MCL1, aes(x=sample, y=MCL1))  + geom_boxplot(aes(fill=time_point),stat = "boxplot") 
 
 grid.arrange(pCXCL1, pMCL1, ncol=2)
 
 
 
 
#xyplot(x~baseline.mRNA.norm["MCL1",]+post_treatment.mRNA.norm["MCL1",]) 
#ggplot(mdf, aes(x=c(1,2,3,4,5,6,7,8,9,10), y=baseline.mRNA.norm["MCL1",], colour=variable)) +geom_line() +  theme_bw()
plot.(baseline.mRNA.norm["MCL1",],post_treatment.mRNA.norm["MCL1",], main="MCL1 vs. Car Weight")
#Posttreatment
#plot(baseline.mRNA.norm["CXCL1",],post_treatment.mRNA.norm["CXCL1",])

data_MCL <- rbind(data.frame(sample = baseline$subjectid ,time_point = baseline$visit,CXCL1 = baseline.mRNA.norm["CXCL1",]),data.frame(sample= post_treatment$subjectid,time_point = post_treatment$visit,CXCL1 = post_treatment.mRNA.norm["CXCL1",]))

ggplot(data_MCL, aes(x=sample,y = CXCL1,fill = time_point)) + geom_bar(stat="identity",position=position_dodge(),colour="black") + labs (x= "Sample Baseline/Post-Treatment", y = "CXCL1") + ggtitle("CLX values differnce between Baseline/Post-Treatment sample")

#############
baseline.mRNA
baseline.mRNA.norm <- NanoStringNorm(
  x = baseline.mRNA,
  anno = NA,
  CodeCount = 'geo.mean',
  Background = 'mean',
  SampleContent = 'housekeeping.geo.mean',
  round.values = TRUE,
  take.log = TRUE,
  return.matrix.of.endogenous.probes = FALSE
)


post_treatment.mRNA.norm <- NanoStringNorm(
  x = post_treatment.mRNA,
  anno = NA,
  CodeCount = 'geo.mean',
  Background = 'mean',
  SampleContent = 'housekeeping.geo.mean',
  round.values = TRUE,
  take.log = TRUE,
  return.matrix.of.endogenous.probes = FALSE
)

all_gen_data <- rbind(data.frame(sample = baseline$subjectid[1] ,time_point = baseline$visit[1], gen_data = baseline.mRNA.norm$normalized.data$GSM2055823_01_4353_PD_mRNA, min = baseline.mRNA.norm$normalized.data$GSM2055823_01_4353_PD_mRNA),  
data.frame(sample = post_treatment$subjectid[1] ,time_point = post_treatment$visit[1], gen_data = post_treatment.mRNA.norm$normalized.data$GSM2055824_02_4355_PD_mRNA),  
data.frame(sample = baseline$subjectid[2] ,time_point = baseline$visit[2], gen_data = baseline.mRNA.norm$normalized.data$GSM2055825_03_3366_PD_mRNA), 
data.frame(sample = post_treatment$subjectid[2] ,time_point = post_treatment$visit[2], gen_data = post_treatment.mRNA.norm$normalized.data$GSM2055826_04_4078_PD_mRNA),
data.frame(sample = baseline$subjectid[3] ,time_point = baseline$visit[3], gen_data = baseline.mRNA.norm$normalized.data$GSM2055827_05_4846_PD_mRNA), 
data.frame(sample = post_treatment$subjectid[3] ,time_point = post_treatment$visit[3], gen_data = post_treatment.mRNA.norm$normalized.data$GSM2055828_06_3746_PD_mRNA),
data.frame(sample = baseline$subjectid[4] ,time_point = baseline$visit[4], gen_data = baseline.mRNA.norm$normalized.data$GSM2055829_07_3760_PD_mRNA),
data.frame(sample = post_treatment$subjectid[4] ,time_point = post_treatment$visit[4], gen_data = post_treatment.mRNA.norm$normalized.data$GSM2055830_08_3790_PD_mRNA),
data.frame(sample = baseline$subjectid[5] ,time_point = baseline$visit[5], gen_data = baseline.mRNA.norm$normalized.data$GSM2055831_09_4436_PD_mRNA),
data.frame(sample = post_treatment$subjectid[5] ,time_point = post_treatment$visit[5], gen_data = post_treatment.mRNA.norm$normalized.data$GSM2055832_10_4050_PD_mRNA))

p <- ggplot(data = all_gen_data, aes(x=sample, y=gen_data))  + geom_boxplot(aes(fill=time_point),stat = "boxplot") + qqplot(x=sample, y=gen_data)
p + facet_wrap( ~ sample, scales="free") 



             
             
             baseline.mRNA.norm$normalized.data$GSM2055823_01_4353_PD_mRNA



boxplot(baseline.mRNA$x$CodeClass~baseline.mRNA$x$GSM2055823_01_4353_PD_mRNA)


###############
   

 
  
  pdf('baseline.mRNA.norm.pdf')
  Plot.NanoStringNorm(x = baseline.mRNA.norm,label.best.guess = TRUE,plot.type = 'all')
  dev.off()
  
  pdf('post_treatment.mRNA.norm.pdf')
  Plot.NanoStringNorm(x = post_treatment.mRNA.norm,label.best.guess = TRUE,plot.type = 'all')
  dev.off()
  
  
  
  
  
  
