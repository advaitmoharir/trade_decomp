# ------------------------------------------------------------------------------
# Title: 00_master
# Purpose: Loads required packages and sources all files
# Authors: Advait Moharir
# Status: Complete
# Date: 05-03-2023
# ------------------------------------------------------------------------------
# Installing/Loading necessary packages
library(pacman)

p_load(countrycode, knitr, DT, purrr,
       rlang, plm, collapse,stringr, tis, rlang,
       tidyr, tidyverse, dplyr, ggplot2,ggpubr,kableExtra,
       stargazer,devtools,rtf)

# Installing imfr package from github

devtools::install_version("imfr", version="0.1.9")
library(imfr)

#IMPORTANT: Run till line 19 once, then comment out line 18,
# and run the whole file.

#Setting root directory
here::i_am("decomposition.Rproj")
library(here)

#Running all files

source("2_code/01_prep.R")
source("2_code/02_decomp.R")
source("2_code/03_decomp_goods_services.R")
source("2_code/04_exhibits.R")

#-----------------END-----------------------
