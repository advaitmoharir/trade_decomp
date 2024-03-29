# Decomposing India's Trade Ratio: 1980 - 2021

This repository contains the code for the paper "Decomposing India's Trade Ratio: 1980-2021. This paper is co-authored with [Arjun Jayadev](https://azimpremjiuniversity.edu.in/people/arjun-jayadev) and [Josh Mason](https://www.jjay.cuny.edu/faculty/j-w-mason). 

## Repo Structure

The repo consists of the following folders

1. `2_code` : Code required to pull and clean raw data, implement the decomposition and generate figures and tables
2. `3_raw`: Raw data for India's trading partners and other variables.
3. `4_output`: Cleaned csvs generated by code.
4. `5_figures`: Figures and tables in the paper.

## Code

The folder `2_code` consists of the following R scripts:

1. `0_master`: Loads all packages and runs all scripts.
2. `01_prep`: Pulls and cleans raw data for India's trading partners.
3. `02_decomp`: Implements decomposition for trade ratio (goods only).
4. `03_decomp_goods_services`: Implements decomposition for trade ratio (goods and services).
5. `04_exhibits`: Generates figures and tables used in paper.

## Replication

To replicate the results in the paper, follow the steps below

1. Open `decomposition.Rproj` in RStudio.
2. Within the project, open `0_master` from `2_code`.
3. Run the file.

## Software

The scripts are written in R using RStudio 2022.07.2+576 "Spotted Wakerobin" Release. Raw and clean csvs were opened in Microsoft Excel 16.




