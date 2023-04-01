#Working Directory
setwd("C:/Users/hp/Dropbox/Trade Decomposition Data/Aggregated Decomposition/Goods plus Services")
#Uploading necessary packages
library(haven)
library(countrycode)
library(DT)
library(plm)
library(dplyr)
library(stringr)
library(splitstackshape)
library(tis)
library(rlang)
library(collapse)
library(plotly)
library(ggplot2)
library(tidyr)
library(imfr)

#Loading both datasets
panel<-read.csv("Panel2.csv")
panel_trunc<-panel%>%filter(year>1994 & year<2013)

services<-read.csv("Linked Series.csv")
services2<-subset(services, partner_iso!="WLD"& 
                    partner_iso!="CZE"& 
                    partner_iso!="AIA"& 
                    partner_iso!= "ABW"& 
                    partner_iso!="ANT"& 
                    partner_iso!="PSE"& 
                    partner_iso!="PAN"& 
                    partner_iso!="KNA"& 
                    partner_iso!="SCG"& 
                    partner_iso!="SWZ"& 
                    partner_iso!="TWN"& 
                    partner_iso!="TZA"& 
                    partner_iso!="ROW"&
                    partner_iso!="XKV"&
                    partner_iso!="CUW"&
                    partner_iso!="SXM"&
                    year!=2019)
services2<-services2[,-4]
services2<-services2%>%filter(year<2013)
#Merging

world_panel<-panel_trunc%>%right_join(services2, by=c("partner_iso", "year"))
world_panel<- pdata.frame(world_panel, index= c("id", "year"))

#DA

world_panel$DA_g<-ifelse(world_panel$year==1995, NA, G(world_panel$gdp_usd)/100)

world_panel$DA_level1<-world_panel$gdp_usd

baseDA <- world_panel[world_panel$year==1995, c('countryarea', 'DA_level1')]
names(baseDA)[2] <- 'baseDA'
world_panel <- merge(world_panel, baseDA, by='countryarea')
world_panel$DA_index <- 100 * world_panel$DA_level1 / world_panel$baseDA
world_panel<-world_panel[order(world_panel$year),]
#Adding goods and services exports
world_panel$exports_by_reporter<-
  world_panel$exports_by_reporter+world_panel$Value

#FA
world_panel$exports_by_reporter<-as.numeric(world_panel$exports_by_reporter)
world_panel$w<-ifelse(world_panel$year==1995,NA,lag(world_panel$exports_by_reporter)/lag(world_panel$total_exports))
world_panel$FA_components<-world_panel$w*world_panel$DA_g
fa_growth<-aggregate(FA_components ~ year, data=world_panel,
                     sum)
fa_growth$FA<- (cumprod(fa_growth$FA_components+1))*100
fa_growth$FA<-as.numeric(fa_growth$FA)


#Opening reporter's data and attaching DA/FA

reporter_data<-world_panel %>% 
  filter(countryarea=="India") %>% select(year,countryarea,cons_final,
                                      cons_final_nominal, gfcf_final_nominal, gfcf_final,	
                                      nom_exports,nom_exports_dom,nom_imports,nom_imports_dom,real_exports, real_imports, 
                                      DA_index,gdp_usd, gdp_dom, gdp_nom_usd)
reporter<-reporter_data %>% left_join(fa_growth, 
                                      by="year")
reporter$FA<-ifelse(reporter$year==1995, 100, reporter$FA)
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

##################FIGURES##############

fig<-plot_ly(reporter, x=~year,
             y=~log_RMI, name="log_RMI", type="scatter", mode="lines",  line = list(shape = "spline", color="black"))
fig<-fig %>% add_trace(y=~log_R, name="log_R",  mode="lines", line = list(shape = "spline", color="blue"))
fig<-fig %>% add_trace(y=~log_RA, name="log_RA",  mode="lines")
fig<-fig %>% add_trace(y=~log_TR, name="log_TR",  mode="lines")
fig

#Re indexed plots



index<-select(reporter, year, R, RMI, TR, RA)
index$ToT<-log(index$R/index$R[1])
index$Exp<-log(index$RA/index$RA[1])
index$Tratio<-log(index$TR/index$TR[1])
index$switch<-log(index$RMI/index$RMI[1])




fig<-plot_ly(index, x=~year,
             y=~Tratio, name="log_TR", type="scatter", mode="lines", line = list(color="black"))
fig<-fig %>% add_trace(y=~ToT, name="log_R",  mode="lines",  line = list(color="blue"))
fig<-fig %>% add_trace(y=~Exp, name="log_RA",  mode="lines",  line = list(color="green"))
fig<-fig %>% add_trace(y=~switch, name="log_RMI",  mode="lines",  line = list(color="red"))
fig <- fig %>% layout(title = "",
                      xaxis = list(title = ""),
                      yaxis = list (title = ""))
fig

#Output

write.csv(reporter, "India_gpluss.csv")


