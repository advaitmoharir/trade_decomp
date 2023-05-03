# ------------------------------------------------------------------------------
# Title: 01_prep
# Purpose: Cleaning and preparing data for decomp
# Authors: Advait Moharir
# Status: Ongoing
# Date: 05-03-2023
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# SECTION 1: LOADING and CLEANING GDP DATA
# ------------------------------------------------------------------------------

#Reading nominal and real data
gdp_nom<-read.csv("3_raw/gdp_nom_lcu.csv")
gdp_real<-read.csv("3_raw/gdp_real_lcu.csv")

#Merge both to get countries with BOTH data

all_nations<-gdp_real%>%
  left_join(gdp_nom, by=c("country","year"))



#Getting ISO3 codenames

codelist<-read.csv("3_raw/country_codes.csv")

#Merge with gdp dataset

gdp<-all_nations %>% 
  left_join(codelist, by="country")

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
rename(iso2c=`@COUNTERPART_AREA`, 
       year=`@TIME_PERIOD`, exports=`@OBS_VALUE`)

#Merge with gdp data

gdp$year<-as.character(gdp$year) #for merge
world<-gdp %>% 
  left_join(trade_data, by=c("year", "iso2c"))%>%
  mutate(Obs=NULL, 
         exports=as.numeric(exports),
         exports=coalesce(exports,0)) #replace NA with 0

#Attaching total exports from India to RoW

world$year<-as.numeric(world$year)# convert back to numeric

total_exp<-world%>%
  group_by(year)%>%
  summarize(total_exports_ind=sum(exports))



#Merge back
world<- total_exp %>% 
  left_join(world, by="year")%>%
  select(-c(16,17,18,19,20,22))%>%
  group_by(iso2c)%>%
  mutate(id=cur_group_id())%>%
  mutate_at(c(9:13),as.numeric)%>%
  na.omit()


#Export panel
write.csv(world, "4_output/india_trade_partners.csv")
# -------------------END-----------------------------#

