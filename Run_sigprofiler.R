library(reticulate)
library(data.table)
library(tidyverse)
setwd("/Volumes/Mac HD/Users/simongrund/Desktop/POLK_analysis/")
use_python("/Library/Frameworks/Python.framework/Versions/3.10/bin/python3")
py_config()

#library(devtools)
#install_github("AlexandrovLab/SigProfilerExtractorR")

library(SigProfilerExtractorR)
#install("GRCh37", rsync=FALSE, bash=TRUE)

#Identify samples with POLK loss — only needed to run once, now cp'ed these samples into a separate folder so i can load them
# d = fread("/Volumes/Mac HD/Users/simongrund/Desktop/Fomatted_data_050722.tsv")
# polk = dplyr::filter(d, POLK == "Biallelic LOF")%>%
#   distinct(Sample_ID)
# 
# print(polk$Sample_ID)

#t = fread("/Volumes/Mac HD/Users/simongrund/Desktop/GenomeDK/HartwigMedical/faststorage/Workspaces/Simong/Data/POLK_biallelic_SNV_indels/CPCT02010399T.purple.somatic.vcf", skip = "##")
"/Volumes/Mac HD/Users/simongrund/Desktop/GenomeDK/HartwigMedical/faststorage/Workspaces/Simong/"

data <- "/Volumes/Mac HD/Users/simongrund/Desktop/GenomeDK/HartwigMedical/faststorage/Workspaces/Simong/Data/POLK_biallelic_SNV_indels/" # here you can provide the path of your own data
dat = list.files(data) #These are the 13 samples from HMF with biallelic POLK loss
files = paste(data, as.list(dat), sep = "")
files #Full path to the files

setwd("Documents/Upwork/POLK_Outside_server/Results/")
# sigprofilerextractor("vcf", 
#                         "CPCT02010399T", 
#                         "/Volumes/Mac HD/Users/simongrund/Desktop/GenomeDK/HartwigMedical/faststorage/Workspaces/Simong/Data/POLK_biallelic_SNV_indels/CPCT02010399T.purple.somatic.vcf", 
#                         minimum_signatures=2,
#                         maximum_signatures=3,
#                         nmf_replicates=5,
#                         min_nmf_iterations = 1000,
#                         max_nmf_iterations =100000,
#                         nmf_test_conv = 1000,
#                         nmf_tolerance = 0.00000001)

#Matrix generator
library("SigProfilerMatrixGeneratorR")
input_file = files[[1]]
matrices <- SigProfilerMatrixGeneratorR(
  project = "POLK", 
  genome = "GRCh37", 
  matrix_path = input_file, 
  exome = F, 
  bed_file = NULL, 
  chrom_based = F, 
  plot = T, 
  tsb_stat = T, 
  seqInfo = T, 
  cushion = 100
)
