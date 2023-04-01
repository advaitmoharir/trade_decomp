#Working Directory
setwd("C:/Users/hp/Dropbox/Trade Decomposition Data/Aggregated Decomposition/Goods/Data files and code/R")
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
#uploading gdp data for all countries
all_nations<-read_dta("gdp.dta")

#Getting ISO3 codenames
data(codelist)
country_set <- codelist
country_set<- country_set %>% 
  select(country.name.en , iso2c, iso3c) %>% filter( !is.na(iso2c))

#Attaching country codes
colnames(all_nations)[1]<-"country"
colnames(country_set)[1]<-"country"
codes<-read.csv("Country Codes.csv")
codes$partner_iso<-toupper(codes$partner_iso)

#Attaching ISO3 codes to original list
colnames(country_set)[3]<-"partner_iso"
codes<- country_set %>% right_join(codes, by="partner_iso")
codes<- select(codes, country.y, partner_iso, iso2c)
colnames(codes)[1]<-"country"
#Final list of countries with ISO2 and ISO3 codes and Belgium code
gdp<-codes %>% right_join(all_nations, by="country")
gdp$partner_iso<-ifelse(gdp$country=="Belgium", "BEL", gdp$partner_iso)
gdp$iso2c<-ifelse(gdp$country=="Belgium", "BE", gdp$iso2c)

#Downloading DOTS dataset
trade<- imf_data(database_id = "DOT" , indicator = c("TXG_FOB_USD") , 
                 country = "IN" , start = 1980, end = 2018,
                 return_raw = TRUE)
trade_data <- trade$CompactData$DataSet$Series
trade_data <- trade_data %>% 
  filter(`@FREQ` == "A") %>% unnest(Obs) %>% 
  arrange(`@REF_AREA`, `@COUNTERPART_AREA`)  
colnames(trade_data)[4]<-"iso2c"
colnames(trade_data)[7]<-"year"
gdp$year<-as.character(gdp$year)
world<-gdp %>% left_join(trade_data, by=c("year", "iso2c"))
world$Obs<-NULL

#Attaching total exports
reporter_exports<-world %>% filter(country=="India") %>% select(nom_exports, year)
world<- reporter_exports %>% right_join(world, by="year")
colnames(world)[1]<-"total_exports"
colnames(world)[10]<-"nom_exports"
colnames(world)[23]<-"exports_by_reporter"
world<-select(world, year, country, partner_iso, iso2c, 
              cons_final, gfcf_final,gfcf_final_nominal,
              cons_final_nominal,nom_exports, 
              nom_imports,real_exports, real_imports, 
              cons_final_nom_dom, gfcf_final_nom_dom,
              nom_exports_dom,nom_imports_dom,
              exports_by_reporter, total_exports)

#Creating panel data
world$id <- world %>% group_indices(country)
world_panel<- pdata.frame(world, index= c("id", "year"))

#DA
world_panel$C<-as.numeric(ifelse(world_panel$year==1980, NA, G(world_panel$cons_final)/100))
world_panel$I<-ifelse(world_panel$year==1980, NA, G(world_panel$gfcf_final)/100)
world_panel$X<-ifelse(world_panel$year==1980, NA, G(world_panel$real_exports)/100)
world_panel$s_C<-ifelse(world_panel$year==1980, NA,lag(world_panel$cons_final_nom_dom)/(lag(world_panel$cons_final_nom_dom)+lag(world_panel$gfcf_final_nom_dom)+lag(world_panel$nom_exports_dom)))
world_panel$s_I<-ifelse(world_panel$year==1980, NA,lag(world_panel$gfcf_final_nom_dom)/(lag(world_panel$cons_final_nom_dom)+lag(world_panel$gfcf_final_nom_dom)+lag(world_panel$nom_exports_dom)))
world_panel$s_X<-ifelse(world_panel$year==1980, NA,1-(world_panel$s_C+world_panel$s_I))
world_panel$DA_growth<-(world_panel$C*world_panel$s_C)+(world_panel$I*world_panel$s_I)+(world_panel$X*world_panel$s_X)

#DA
world_panel$DA_level <- world_panel$cons_final + world_panel$gfcf_final+world_panel$real_exports
baseDA <- world_panel[world_panel$year==1980, c('country', 'DA_level')]
names(baseDA)[2] <- 'baseDA'
world_panel <- merge(world_panel, baseDA, by='country')
world_panel$DA_index <- 100 * world_panel$DA_level / world_panel$baseDA

#FA
world_panel$exports_by_reporter<-as.numeric(world_panel$exports_by_reporter)
world_panel$w<-ifelse(world_panel$year==1980,NA,lag(world_panel$exports_by_reporter)/lag(world_panel$total_exports))
world_panel$FA_components<-world_panel$w*world_panel$DA_growth
fa_growth<-aggregate(FA_components ~ year, data=world_panel,
                     sum)
fa_growth$FA<- (cumprod(fa_growth$FA_components+1))*100
fa_growth$FA<-as.numeric(fa_growth$FA)


#Opening reporter's data and attaching DA/FA
reporter_data<-world_panel %>% 
  filter(country=="India") %>% select(year,country,cons_final,
                                      cons_final_nominal, gfcf_final_nominal, gfcf_final,	
                                      nom_exports,nom_exports_dom,nom_imports,nom_imports_dom,real_exports, real_imports, 
                                      DA_index)
reporter<-reporter_data %>% left_join(fa_growth, 
                                      by="year")
reporter$FA<-ifelse(reporter$year==1980, 100, reporter$FA)
reporter<-as.data.frame(reporter)

#Decomposition
reporter$TR<-(reporter$nom_exports_dom/reporter$nom_imports_dom)
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
#################FIGURES#################
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


#exporting final data as csv
write.csv(reporter, "India.csv")

# Contribution Plots

contrib<-select(reporter, year, R, RMI, TR, RA)
contrib$RA<-as.numeric(contrib$RA)
contrib$RMI<-as.numeric(contrib$RMI)
contrib%>%pivot_longer(2:5)%>%
  ggplot(aes(x=year,y=log(value), fill=name ))+geom_col()+
  scale_fill_discrete(name="", 
                      labels=c("log_R", "log_RA", "log_RMI", "log_TR"))
+theme(plot.caption = element_text(hjust = 0, size=12))+
  labs(x="",y="",caption="Figure-5:Evolution of components of India's  trade ratio(1980-2018)")

#Counterfactuals
start.year <- 1
india<-reporter%>% select(year, log_TR, log_R, log_RMI, log_RA)
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
cf%>%pivot_longer(2:5)%>% 
  ggplot(aes(x=V1,y=exp(value), color=name ))+geom_line()+
  scale_color_discrete(name="", 
                       labels=c("Countefactual A", "Countefactual B",
                                "Countefactual C",
                                "Historical"))
+theme(plot.caption = element_text(hjust = 0, size=12))+labs(x="",y="",caption="Figure-7:Counterfactual 1")

#Plotting cf_2
cf_2<-cf%>%select(V1,V2, cf4,cf5,cf6)
cf_2%>%pivot_longer(2:5)%>% 
  ggplot(aes(x=V1,y=exp(value), color=name ))+geom_line()+
  scale_color_discrete(name="", 
                       labels=c("Countefactual D", "Countefactual E",
                                "Countefactual F",
                                "Historical"))
+theme(plot.caption = element_text(hjust = 0, size=12))+labs(x="",y="",caption="Figure-8:Counterfactual 2")
