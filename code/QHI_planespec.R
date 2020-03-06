# 2.3.2020
# Planespec
library(tidyverse)
library(raster)

untar("~/Documents/university work/Dissertation/local/QHI_planespec/ang20190702t205702rfl.tar.gz",list=TRUE)

spec_07 <- read.csv("~/Documents/university work/Dissertation/local/QHI_planespec/ang20190702t205702_rfl_v2v1/ang20190702t205702_corr_v2v1_img.hdr")
spec_07 <- raster("~/Documents/university work/Dissertation/local/QHI_planespec/ang20190702t205702_rfl_v2v1/ang20190702t205702_corr_v2v1_img")
