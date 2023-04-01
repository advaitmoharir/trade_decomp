# ------------------------------------------------------------------------------
# Title: 00_master
# Purpose: Loads required packages and sources all files
# Authors: Advait Moharir
# Status: Ongoing
# Date: 05-03-2023
# ------------------------------------------------------------------------------
# Installing/Loading necessary packages
library(pacman)

p_load(countrycode, knitr, DT, purrr,
       rlang, plm, collapse,stringr, tis, rlang,
       tidyr, tidyverse, dplyr, ggplot2, 
       imfr, ggpubr,ggpubfigs,kableExtra,
       stargazer)

#Setting root directory
here::i_am("decomposition.Rproj")
library(here)

#Running all files

source("2_code/01_prep.R")
source("2_code/02_decomp.R")
source("2_code/03_decomp_goods_services.R")
source("2_code/04_exhibits.R")

#-----------------END-----------------------
