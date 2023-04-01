# ------------------------------------------------------------------------------
# Title: Cleaning and preparing data for decomp
# Authors: Advait Moharir
# Status: Ongoing
# Date: 05-03-2023
# ------------------------------------------------------------------------------

# Installing/Loading necessary packages
library(pacman)

p_load(countrycode, knitr, DT, purrr,
       rlang, plm, collapse,stringr, tis, rlang,
       tidyr, tidyverse, dplyr, ggplot2, 
       imfr, ggpubr)

#Setting root directory
here::i_am("decomposition.Rproj")
library(here)

# ------------------------------------------------------------------------------
# SECTION 1: LOADING and CLEANING GDP DATA
# ------------------------------------------------------------------------------

all_nations<-read.csv("3_raw/gdp.csv")

#Getting ISO3 codenames

data(codelist) # from countrycode package
codelist<- codelist %>% 
  select(country.name.en , iso2c, iso3c) %>% 
  filter( !is.na(iso2c))%>%
  rename(country=country.name.en)

#Merge with gdp dataset

gdp<-codelist %>% 
  right_join(all_nations, by="country")

# ------------------------------------------------------------------------------
# SECTION 2: Loading and merging IMF- DOTS data
# ------------------------------------------------------------------------------


#Downloading DOTS dataset
trade<- imf_data(database_id = "DOT" , indicator = c("TXG_FOB_USD") , 
                 country = "IN" , start = 1980, end = 2021,
                 return_raw = TRUE)
trade_data <- trade$CompactData$DataSet$Series%>% # simple df
filter(`@FREQ` == "A")%>% #annual data
mutate(Obs = map(Obs, ~ if(is.list(.x)) as_tibble(.x) else .x))%>%
unnest(Obs)%>%
rename(iso2c=`@COUNTERPART_AREA`, year=`@TIME_PERIOD`, exports=`@OBS_VALUE`)

#Merge with gdp data

gdp$year<-as.character(gdp$year) #for merge
world<-trade_data %>% 
  left_join(gdp, by=c("year", "iso2c"))%>%
  mutate(Obs=NULL)

#Attaching total exports from India to RoW

world$year<-as.numeric(world$year)

total_exp<-world%>%
  group_by(year)%>%
  summarize(total_exports_ind=sum(as.numeric(exports)))

#Drop unnecessary columns
drop.cols <- c(3,4,5,7,8,10)

#Merge back
world<- total_exp %>% 
  right_join(world, by="year")%>%
  select(-all_of(drop.cols))%>%
  mutate(id=group_indices(country)) #country id
  




#DA
world_panel$C<-as.numeric(ifelse(world_panel$year==1980, NA, G(world_panel$cons_final)/100))
world_panel$I<-ifelse(world_panel$year==1980, NA, G(world_panel$gfcf_final)/100)
world_panel$s_C<-ifelse(world_panel$year==1980, NA,lag(world_panel$cons_final_nominal)/(lag(world_panel$cons_final_nominal)+lag(world_panel$gfcf_final_nominal)))
world_panel$s_I<-ifelse(world_panel$year==1980, NA,1-world_panel$s_C)
world_panel$DA_growth<-(world_panel$C*world_panel$s_C)+(world_panel$I*world_panel$s_I)
world_panel$DA_g<-ifelse(world_panel$year==1980, NA, G(world_panel$gdp_usd)/100)
world_panel_goods<-world_panel
#DA
world_panel$DA_level1<-world_panel$gdp_usd
world_panel$DA_level <- world_panel$cons_final + world_panel$gfcf_final
baseDA <- world_panel[world_panel$year==1980, c('country', 'DA_level1')]
names(baseDA)[2] <- 'baseDA'
world_panel <- merge(world_panel, baseDA, by='country')
world_panel$DA_index <- 100 * world_panel$DA_level1 / world_panel$baseDA

#FA
world_panel$exports_by_reporter<-as.numeric(world_panel$exports_by_reporter)
world_panel$w<-ifelse(world_panel$year==1980,NA,lag(world_panel$exports_by_reporter)/lag(world_panel$total_exports))
world_panel$FA_components<-world_panel$w*world_panel$DA_g
fa_growth<-aggregate(FA_components ~ year, data=world_panel,
                     sum)
fa_growth$FA<-(cumprod(fa_growth$FA_components+1))*100
fa_growth$FA<-as.numeric(fa_growth$FA)
fa_growth_goods<-fa_growth

#Opening reporter's data and attaching DA/FA
reporter_data<-world_panel %>% 
  filter(country=="India") %>% select(year,country,cons_final,
                                                                   cons_final_nominal, gfcf_final_nominal, gfcf_final,	
                                                                   nom_exports,nom_exports_dom,nom_imports,nom_imports_dom,real_exports, real_imports, 
                                                                   DA_index,gdp_usd, gdp_dom, gdp_nom_usd)
reporter<-reporter_data %>% left_join(fa_growth, 
                                      by="year")
reporter$FA<-ifelse(reporter$year==1980, 100, reporter$FA)
reporter<-as.data.frame(reporter)

#Decomposition
reporter$TR<-(reporter$nom_exports_dom/reporter$nom_imports_dom)
reporter$RA<-(reporter$FA/reporter$DA_index)
reporter$FMI<-reporter$real_exports/reporter$FA
reporter$DMI<-reporter$real_imports/reporter$DA_index
reporter$RMI<-reporter$FMI/reporter$DMI
reporter$R<-reporter$TR*(reporter$real_imports/reporter$real_exports)
#Logs
reporter$log_RA<-log(reporter$RA)
reporter$log_TR<-log(reporter$TR)
reporter$log_R<-log(reporter$R)
reporter$log_RMI<-log(reporter$RMI)
reporter$log_DMI<-log(reporter$DMI)
reporter$log_FMI<-log(reporter$FMI)
reporter$log_DA<-log(reporter$DA_index)
reporter$log_FA<-log(reporter$FA)
#################FIGURES#################
setwd("C:/Users/hp/Dropbox/Trade Decomposition Data/material for paper")



india<-read.csv("India_forpaper.csv")

#Trade Deficit
india$td<-(india$nom_exports-india$nom_imports)/(india$gdp_nom_usd)


p1<-ggplot(india, aes(x=year))+
  geom_line(aes(y=TR), color="red")+
  labs(x="", y="Trade Ratio")+theme(plot.caption = element_text(hjust = 0, size=12))
ggsave("Figure1A.jpeg", width=6, height=4)

p2<-ggplot(india, aes(x=year))+geom_line(aes(y=-1*td), color="blue")+labs(x="", y="Trade Deficit")+theme(plot.caption = element_text(hjust = 0, size=12))
ggsave("Figure1B.jpeg", width=6, height=4)
plot_grid(p1,p2)



#Terms of Trade
ggplot(india, aes(x=year, y=100*R) )+geom_line(color="dark blue")+labs(x="",y="Terms of Trade")+theme(plot.caption = element_text(hjust = 0, size=12))
ggsave("Figure2.jpeg", width=6, height=4)

contrib<-select(india, year, R, RMI, TR, RA)
contrib%>%pivot_longer(2:5)%>%
  ggplot(aes(x=year,y=log(value), fill=name ))+geom_col()+
  scale_fill_discrete(name="", labels=c("Terms of Trade", "Relative Growth", "Relative Import Intensity", "Trade Ratio"))+theme(plot.caption = element_text(hjust = 0, size=12))+
  labs(x="",y="")
ggsave("Figure3.jpeg", width=8, height=4)

#Reindexing 
reindex1<-select(india, year, R, RMI, TR, RA)
reindex1$ToT<-log(reindex1$R/reindex1$R[1])
reindex1$Exp<-log(reindex1$RA/reindex1$RA[1])
reindex1$Tratio<-log(reindex1$TR/reindex1$TR[1])
reindex1$switch<-log(reindex1$RMI/reindex1$RMI[1])

#Figure 4: Decomposition over time(Goods, Indexed)

reindex1<-reindex1[,-c(2:5)]
reindex1%>%pivot_longer(2:5)%>%
  ggplot(aes(x=year,y=value, color=name ))+geom_line()+
  scale_color_discrete(name="", labels=c("Relative Growth", "Relative Import Intensity", "Terms of Trade", "Trade Ratio"))+theme(plot.caption = element_text(hjust = 0, size=12))+labs(x="",y="")
ggsave("Figure4.jpeg", width=8, height=4)

#Counterfactuals
start.year <- 1
india<-india%>% select(year, log_TR, log_R, log_RMI, log_RA, log_FMI,log_DMI,
                       log_FA, log_DA)
india$year<-as.numeric(india$year)
india.temp <- india[india$year > (start.year-1),]
cf1 <- cf2 <- cf3 <- cf4 <- cf5 <- cf6 <- india.temp$log_TR
n <- length(cf1)

india.change <- india.temp[-1, -1] - india.temp[-n, -1]

for (i in 1:(n-1)){
  cf1[i+1] <- cf1[i] + india.change$log_RMI[i] + india.change$log_RA[i]
  cf2[i+1] <- cf2[i] + india.change$log_R[i]  + india.change$log_RMI[i] 
  cf3[i+1] <- cf3[i] + india.change$log_R[i]  + india.change$log_RA[i]
  cf4[i+1] <- cf4[i] + india.change$log_R[i]
  cf5[i+1] <- cf5[i] + india.change$log_RA[i]
  cf6[i+1] <- cf6[i] + india.change$log_RMI[i] 
}

cf<-as.data.frame(cbind(india$year, india$log_TR,cf1,cf2,cf3,cf4,cf5,cf6))
cf$V1<-c(1980:2018)

#Plotting cf_1
cf_1<-cf%>%select(V1,V2, cf1,cf2,cf3)
cf_1%>%pivot_longer(2:5)%>% 
  ggplot(aes(x=V1,y=exp(value), color=name ))+geom_line()+
  scale_color_discrete(name="", 
                       labels=c("Fixed Terms of Trade", "Fixed Relative Expenditure Growth",
                                "Fixed Relative Import Intensity",
                                "Historical"))+theme(plot.caption = element_text(hjust = 0, size=12))+labs(x="",y="")
ggsave("Figure5.jpeg", width=8, height=4)

#Plotting cf_2
cf_2<-cf%>%select(V1,V2, cf4,cf5,cf6)
cf_2%>%pivot_longer(2:5)%>% 
  ggplot(aes(x=V1,y=exp(value), color=name ))+geom_line()+
  scale_color_discrete(name="", 
                       labels=c("Terms of Trade Only", "Relative Expenditure Growth Only",
                                "Relative Import Intensity Only",
                                "Historical"))+theme(plot.caption = element_text(hjust = 0, size=12))+labs(x="",y="")
ggsave("Figure6.jpeg", width=8, height=4)
#####################DECOMPOSITION TABLES#############

# Table-1 Decomposition over periods (goods)


#Defining decomp and decomp table func

decomp <- function(data, start, end){
  x <- data.frame(ratio=0, terms_of_trade=0, FA=0, DA=0, FMI=0, DMI=0)
  x$ratio <- (data[data$year==end, 'log_TR'] - data[data$year==start, 'log_TR'])*100/(data[data$year==end, 'year']-data[data$year==start, 'year'])
  x$terms_of_trade <- (data[data$year==end, 'log_R'] - data[data$year==start, 'log_R'])*100/(data[data$year==end, 'year']-data[data$year==start, 'year'])
  x$FA <- (data[data$year==end, 'log_FA'] - data[data$year==start, 'log_FA'])*100/(data[data$year==end, 'year']-data[data$year==start, 'year'])
  x$DA <- (-1 * (data[data$year==end, 'log_DA'] - data[data$year==start, 'log_DA']))*100/(data[data$year==end, 'year']-data[data$year==start, 'year'])
  x$FMI <- (data[data$year==end, 'log_FMI'] - data[data$year==start, 'log_FMI'] )*100/(data[data$year==end, 'year']-data[data$year==start, 'year'])
  x$DMI <- (-1 * (data[data$year==end, 'log_DMI'] - data[data$year==start, 'log_DMI'] ))*100/(data[data$year==end, 'year']-data[data$year==start, 'year'])
  return(x)
}

decomp.table <- function(data, starts, ends){
  x <- decomp(india, starts[1], ends[1])
  for (i in 2:length(starts)){
    x <- rbind(x, decomp(india, starts[i], ends[i]))
  }
  x$period <- paste(starts, ends, sep='-')
  x <- x[,c(7, 1:6)]
  return(x)
}

#Decomposition table
starts <- c( 1980, 1991, 1999, 2003, 2012, 2016)
ends <- c(1991, 1999, 2003, 2012, 2016, 2018)
decomposition <- decomp.table(india, starts, ends)
decomposition$terms_of_trade<-
  as.numeric(decomposition$terms_of_trade)
decomposition$ratio<-as.numeric(decomposition$ratio)
#Generating latex code 
decomposition<-decomposition%>%
  mutate(across(2:7, round, 2))
kable(decomposition, "latex")

#Five - Way Decomposition
contrib<-select(india, year, R, RMI, TR, FMI, DMI, FA, DA_index)
contrib%>%pivot_longer(2:8)%>%
  ggplot(aes(x=year,y=log(value), fill=name ))+geom_col()+
  scale_fill_discrete(name="", labels=c("Terms of Trade", "Relative Growth", "Relative Import Intensity", "Trade Ratio"))+theme(plot.caption = element_text(hjust = 0, size=12))+
  labs(x="",y="")
ggsave("Figure3.jpeg", width=8, height=4)

