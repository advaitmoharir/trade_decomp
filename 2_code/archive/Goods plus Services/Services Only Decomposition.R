#Working Directory
setwd("C:/Users/hp/Dropbox/Trade Decomposition Data/Services Data")
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
panel<-read.csv("Panel.csv")
panel_trunc<-panel%>%filter(year>1994)

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
#Merging

world_panel<-panel_trunc%>%right_join(services2, by=c("partner_iso", "year"))
world_panel<- pdata.frame(world_panel, index= c("id", "year"))

#DA

world_panel$C<-as.numeric(ifelse(world_panel$year==1995, NA, G(world_panel$cons_final)/100))
world_panel$I<-ifelse(world_panel$year==1995, NA, G(world_panel$gfcf_final)/100)
world_panel$s_C<-ifelse(world_panel$year==1995, NA,lag(world_panel$cons_final)/(lag(world_panel$cons_final)+lag(world_panel$gfcf_final)))
world_panel$s_I<-ifelse(world_panel$year==1995, NA,1-world_panel$s_C)
world_panel$DA_growth<-(world_panel$C*world_panel$s_C)+(world_panel$I*world_panel$s_I)


world_panel$DA_level <- world_panel$cons_final + world_panel$gfcf_final
baseDA <- world_panel[world_panel$year==1995, c('country', 'DA_level')]
names(baseDA)[2] <- 'baseDA'
world_panel <- merge(world_panel, baseDA, by='country')
world_panel$DA_index <- 100 * world_panel$DA_level / world_panel$baseDA
world_panel<-world_panel[order(world_panel$year),]

#Removing goods

world_panel<-world_panel[,-10]
colnames(world_panel)[18]<-"exports_by_reporter"
#FA

#FA
world_panel$exports_by_reporter<-as.numeric(world_panel$exports_by_reporter)
world_panel$w<-ifelse(world_panel$year==1995,NA,lag(world_panel$exports_by_reporter)/lag(world_panel$total_exports))
world_panel$FA_components<-world_panel$w*world_panel$DA_growth
fa_growth<-aggregate(FA_components ~ year, data=world_panel,
                     sum)
fa_growth$FA<- (cumprod(fa_growth$FA_components+1))*100
fa_growth$FA<-as.numeric(fa_growth$FA)


#Opening reporter's data and attaching DA/FA

reporter_data<-world_panel %>% filter(country=="India") %>% select(year,country,cons_final, gfcf_final,	nom_exports,nom_imports, DA_index)
reporter<-reporter_data %>% left_join(fa_growth, 
                                      by="year")
reporter$FA<-ifelse(reporter$year==1995, 100, reporter$FA)
reporter<-as.data.frame(reporter)
#Attaching real exports and imports
real_trade<-read.csv("Real exports and imports.csv")
real_trade2<-subset(real_trade, year>1994)
real_trade2$year<-c(1:24)
real_trade2$real_exports<-as.numeric(real_trade2$real_exports)
real_trade2$real_imports<-as.numeric(real_trade2$real_imports)
reporter$year<-as.numeric(reporter$year)
reporter<- reporter %>% left_join(real_trade2, by=c("year", "country"))
reporter$year<-c(1995:2018)
#Decomposition
reporter$TR<-(reporter$nom_exports/reporter$nom_imports)
reporter$RA<-(reporter$FA/reporter$DA)
reporter$FMI<-reporter$real_exports/reporter$FA
reporter$DMI<-reporter$real_imports/reporter$DA
reporter$RMI<-reporter$FMI/reporter$DMI
reporter$R<-reporter$TR*(reporter$real_imports/reporter$real_exports)
#Logs
reporter$log_RA<-log(reporter$RA)
reporter$log_TR<-log(reporter$TR)
reporter$log_R<-log(reporter$R)
reporter$log_RMI<-log(reporter$RMI)
reporter$log_DMI<-log(reporter$DMI)
reporter$log_FMI<-log(reporter$FMI)
#Plot
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
