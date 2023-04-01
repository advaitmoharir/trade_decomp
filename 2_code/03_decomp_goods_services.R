# ------------------------------------------------------------------------------
# Title: 03_decomp_goods_services
#Purpose: Implements trade decomposition, including services
# Authors: Advait Moharir
# Status: Ongoing
# Date: 05-03-2023
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# SECTION 1: Calculating FA and DA
# ------------------------------------------------------------------------------

#options(scipen=100)

#Read panel of trading partners

goods_panel<-read.csv("4_output/india_trade_partners.csv")%>%
  filter(year>1994 & year<2013)

#Read panel of service exports
services<-read.csv("3_raw/service_exports.csv")

# Merge

world_panel<-goods_panel%>%
  left_join(services, by=c("iso3c","year"))%>%
  select(-c(19:25))%>%
  mutate(exports=exports+Value)

#Create Domestic Absorption (DA) variable

world_panel<-world_panel%>%
  arrange(id,year)%>%
  mutate(C=ifelse(year==1995,NA,
                  (cons_real-lag(cons_real))/cons_real),
         I=ifelse(year==1995,NA,
                  (inv_real-lag(inv_real))/inv_real),
         s_C=ifelse(year==1995, NA,
                    lag(cons_nom)/(lag(cons_nom)+lag(inv_nom))),
         s_I=ifelse(year==1995,NA, 1-s_C),
         DA_hat= C*s_C+I*s_I,
         DA_level=cons_real+inv_real) #hat=growth


#DA
baseDA <- world_panel[world_panel$year==1995, c('country', 'DA_level')]
names(baseDA)[2] <- 'baseDA'
world_panel <- merge(world_panel, baseDA, by='country')
world_panel$DA <- 100 * (world_panel$DA_level/ world_panel$baseDA)

#FA_growth
world_panel<-world_panel%>%
  arrange(id,year)%>%
  mutate(w=ifelse(world_panel$year==1995,
                  NA,
                  lag(exports)/lag(total_exports_ind)),
         FA_comp=w*DA_hat)

#Putting fa_growth in seperate dataframe
fa_growth<-aggregate(FA_comp~ year, data=world_panel,
                     sum)
fa_growth$FA<-as.numeric((cumprod(fa_growth$FA_comp+1))*100)

#Extracting India's data and attaching DA/FA
reporter<-world_panel %>% 
  filter(country=="India") %>% 
  left_join(fa_growth,by="year")%>%
  mutate(FA=ifelse(year==1995, 100, FA))%>%
  select(year,country,export_nom,export_real,
         import_nom,import_real,gdp_nom,gdp_real,
         DA,FA)


# ------------------------------------------------------------------------------
# SECTION 2: Implementing decomposition
# ------------------------------------------------------------------------------


reporter<-reporter%>%
  mutate(TR=export_nom/import_nom,
         RA=FA/DA,
         FMI=export_real/FA,
         DMI=import_real/DA,
         RMI=FMI/DMI,
         R=TR*(import_real/export_real))%>%
  mutate_at(c(9:16), log) #take logs for all relevant vars


# exporting output to csv
write.csv(reporter, "4_output/decomp_india_goods_services.csv")

