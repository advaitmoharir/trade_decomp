# ------------------------------------------------------------------------------
# Title: 04_exhibits
#Purpose: Creates and outputs tables and figures
# Authors: Advait Moharir
# Status: Complete
# Date: 05-03-2023
# ------------------------------------------------------------------------------

# Read csvs

india<-read.csv("4_output/decomp_india.csv")
gdp<-read.csv("3_raw/gdp_nom_lcu.csv")
flows<-read.csv("3_raw/fin_flows.csv")
gs<-read.csv("4_output/decomp_india_goods_services.csv")
# ------------------------------------------------------------------------------
# SECTION 1: Figures (Main)
# ------------------------------------------------------------------------------

#Fig1: Scatter of TR against Trade deficit


# Trade Deficit
india<-india%>%
  mutate(td=(export_nom-import_nom)/gdp_nom,
         TR_abs=exp(TR))
#Scatter plot
p1<-ggscatter(india,
          x="td", y="TR_abs", 
          add="reg.line", add.params = 
            list(color="black"),
          xlab="Trade Balance", ylab="Trade Ratio")
ggsave("5_figures/Figure_1.png",dpi=1000, width=8,height=4)

#Fig2: Terms of Trade

p2<-india%>%
  select(year,R)%>%
  mutate(R=R*100)%>%
ggbarplot(x="year", y="R", fill="darkgrey", 
       xlab="", ylab="Terms of Trade (%)")+
  scale_y_continuous(breaks=seq(-40,30,by=10))
  
ggsave("5_figures/Figure_2.png",dpi=1000, width=8, height=4)

#Figure-3: Decomp Bar Graph

contrib<-select(india, year, R, RMI, TR, RA)
p3<-contrib%>%pivot_longer(2:5)%>%
  ggbarplot(x="year", y="value", fill="name",
            xlab="", ylab="", legend.title="")+
  scale_fill_manual(labels=c("Terms of Trade",
                               "Relative Absorption",
                               "Relative Import Intensity",
                               "Trade Ratio"), 
                    values=c("white", "grey80","grey40", "black"))+
  theme(legend.position="bottom")


ggsave("5_figures/Figure_3.png",dpi=1000, width=8, height=4)


#Figure 4: Decomposition over time(Goods, Indexed)

p4<-contrib%>%
  mutate(R=R-R[1],RMI=RMI-RMI[1],TR=TR-TR[1])%>%
  pivot_longer(2:5)%>%
  ggline(x="year", y="value", color="name",
            xlab="", ylab="", legend.title="", linetype="name", 
         plot_type ="l", size=0.7, lineend='round')+
  scale_linetype_manual(values=c(4,2,3,1),labels=c("Terms of Trade",
                                 "Relative Absorption",
                                 "Relative Import Intensity",
                                 "Trade Ratio"))+
  scale_color_manual(values=c("black","black","black","black"),labels=c("Terms of Trade",
                               "Relative Absorption",
                               "Relative Import Intensity",
                               "Trade Ratio"))+
  theme(legend.position="bottom")

ggsave("5_figures/Figure_4.png",dpi=1000, width=8, height=4)

#---------------Counterfactuals--------------------#


start.year <- 1
india<-india%>% 
  select(year, TR,R,RMI,FMI,DMI,FA,DA,RA)

#Create temp df with all values
india.temp <- india[india$year > (start.year-1),]

#6 counterfactual trends
cf1 <- cf2 <- cf3 <- cf4 <- cf5 <- cf6 <- india.temp$TR
n <- length(cf1)

india.change <- india.temp[-1, -1] - india.temp[-n, -1]

for (i in 1:(n-1)){
  cf1[i+1] <- cf1[i] + india.change$RMI[i] + india.change$RA[i]
  cf2[i+1] <- cf2[i] + india.change$R[i]  + india.change$RMI[i] 
  cf3[i+1] <- cf3[i] + india.change$R[i]  + india.change$RA[i]
  cf4[i+1] <- cf4[i] + india.change$R[i]
  cf5[i+1] <- cf5[i] + india.change$RA[i]
  cf6[i+1] <- cf6[i] + india.change$RMI[i] 
}

cf<-as.data.frame(cbind(india$year, india$TR,cf1,cf2,cf3,cf4,cf5,cf6))
cf$V1<-c(1980:2021)

#Figure 5: Counterfactual 1

p5<-cf%>%
select(V1,V2, cf1,cf2,cf3)%>%
pivot_longer(2:5)%>% 
  ggline(x="V1", y="value", color="name",
         xlab="", ylab="", legend.title="", linetype="name", 
         plot_type ="l", size=0.7, lineend='round')+
  scale_linetype_manual(values=c(4,3,2,1), labels=c("Fixed Terms of Trade", 
                                                    "Fixed Relative Expenditure Growth",
                                                    "Fixed Relative Import Intensity",
                                                    "Historical"))+
scale_color_manual(values=c("black","black","black","black"),name="", labels=c("Fixed Terms of Trade", 
              "Fixed Relative Expenditure Growth",
              "Fixed Relative Import Intensity",
              "Historical"))+
  theme(legend.position="bottom")
ggsave("5_figures/Figure_5.png",dpi=1000, width=8, height=4)

# Figure6: Counterfactual 2

p6<-cf%>%
  select(V1,V2, cf4,cf5,cf6)%>%
  pivot_longer(2:5)%>% 
  ggline(x="V1", y="value", color="name",
         xlab="", ylab="", legend.title="", linetype="name", 
         plot_type ="l", size=0.7, lineend='round')+
  scale_linetype_manual(values=c(4,3,2,1), labels=c("Terms of Trade Only", 
                                                    "Relative Expenditure Growth Only",
                                                    "Relative Import Intensity Only",
                                                    "Historical"))+
  scale_color_manual(values=c("black","black","black","black"),name="",
                     labels=c("Terms of Trade Only", 
                              "Relative Expenditure Growth Only",
                              "Relative Import Intensity Only",
                              "Historical"))+
  scale_y_continuous(breaks=seq(-2,1,by=0.5))+
  theme(legend.position="bottom")
ggsave("5_figures/Figure_6.png",dpi=1000, width=8, height=4)

#Figure 7: India's CAB and KAB

p7<-gdp%>%
  filter(country=="India")%>%
  select(year,gdp_nom)%>%
  left_join(flows,by="year")%>%
  mutate(gdp_nom=gdp_nom/10000000, curr=curr/gdp_nom,
         cap=cap/gdp_nom)%>%
  pivot_longer(3:4)%>%
  ggline(x="year", y="value",color="name", 
         legend.title="", xlab="", ylab="", linetype="name", 
         plot_type ="l", size=0.7, lineend='round' )+
  scale_linetype_manual(values=c(1,2), labels=c("Capital Account Balance", 
                                                "Current Account Balance"))+
  scale_color_manual(name="", values=c("black", "black"),
                       labels=c("Capital Account Balance", 
                                "Current Account Balance"))+
  scale_y_continuous(breaks=seq(-0.05,0.1,by=0.025), 
                     labels=scales::percent)+
  theme(legend.position="bottom")
ggsave("5_figures/Figure_7.jpeg", width=8, height=4)

# ------------------------------------------------------------------------------
# SECTION 2: Figures (Appendix)
# ------------------------------------------------------------------------------




#Figure-8: Decomp Bar Graph

a1<-gs%>%
  select(year, R, RMI, TR, RA)%>%
  pivot_longer(2:5)%>%
  ggbarplot(x="year", y="value", fill="name",
            xlab="", ylab="", legend.title="")+
  scale_fill_manual(labels=c("Terms of Trade",
                             "Relative Absorption",
                             "Relative Import Intensity",
                             "Trade Ratio"), 
                    values=c("white", "grey80","grey40", "black"))+
  theme(legend.position="bottom")

ggsave("5_figures/Figure_A1.png",dpi=1000, width=8, height=4)


#Figure A2: Decomposition over time(Goods+Services, Indexed)

a2<-gs%>%
  select(year,R,RMI,TR,RA)%>%
  mutate(R=R-R[1],RMI=RMI-RMI[1],TR=TR-TR[1])%>%
  pivot_longer(2:5)%>%
  ggline(x="year", y="value", color="name",
         xlab="", ylab="", legend.title="", linetype="name", 
         plot_type ="l", size=0.7, lineend='round')+
  scale_linetype_manual(values=c(4,2,3,1),labels=c("Terms of Trade",
                                                   "Relative Absorption",
                                                   "Relative Import Intensity",
                                                   "Trade Ratio"))+
  scale_color_manual(values=c("black","black","black","black"),labels=c("Terms of Trade",
                                                                        "Relative Absorption",
                                                                        "Relative Import Intensity",
                                                                        "Trade Ratio"))+

  scale_y_continuous(breaks=seq(-0.5,0.5,by=0.1))+
  theme(legend.position="bottom")

ggsave("5_figures/Figure_A2.png", width=8, height=4)

#Figure A2: RA/RMI over time (both datasets)

a3<-gs%>%
  rename(RA_gs=RA, RMI_gs=RMI)%>%
  left_join(india, by="year")%>%
  select(year,RA,RMI,RA_gs,RMI_gs)%>%
  pivot_longer(2:5)%>%
  ggline(x="year", y="value", color="name",
         xlab="", ylab="", legend.title="",linetype="name", 
         plot_type ="l", size=0.7, lineend='round')+
  scale_linetype_manual(values=c(1,2,3,4),labels=c("RA (Goods Only)",
                                                   "RA (Goods+Services)",
                                                   "RMI (Goods Only)",
                                                   "RMI (Goods+Services)"))+
  
  scale_color_manual(values=c("black","black","black","black"),
                     labels=c("RA (Goods Only)",
                                "RA (Goods+Services)",
                                "RMI (Goods Only)",
                                "RMI (Goods+Services)"))+
  theme(legend.text=element_text(size=10), legend.position = "bottom")
  
ggsave("5_figures/Figure_A3.jpeg",dpi=1000, width=8, height=4)


# Counterfactuals

start.year <- 1
gs1<-gs%>% 
  select(year, TR,R,RMI,FMI,DMI,FA,DA,RA)

#Create temp df with all values
india.temp <- gs1[gs1$year > (start.year-1),]

#6 counterfactual trends
cf1 <- cf2 <- cf3 <- cf4 <- cf5 <- cf6 <- india.temp$TR
n <- length(cf1)

india.change <- india.temp[-1, -1] - india.temp[-n, -1]

for (i in 1:(n-1)){
  cf1[i+1] <- cf1[i] + india.change$RMI[i] + india.change$RA[i]
  cf2[i+1] <- cf2[i] + india.change$R[i]  + india.change$RMI[i] 
  cf3[i+1] <- cf3[i] + india.change$R[i]  + india.change$RA[i]
  cf4[i+1] <- cf4[i] + india.change$R[i]
  cf5[i+1] <- cf5[i] + india.change$RA[i]
  cf6[i+1] <- cf6[i] + india.change$RMI[i] 
}

cf<-as.data.frame(cbind(gs1$year, gs1$TR,cf1,cf2,cf3,cf4,cf5,cf6))
cf$V1<-c(1995:2012)

#Figure 11: Counterfactual 1

p11<-cf%>%
  select(V1,V2, cf1,cf2,cf3)%>%
  pivot_longer(2:5)%>% 
  ggline(x="V1", y="value", color="name",
         xlab="", ylab="", legend.title="", linetype="name", 
         plot_type ="l", size=0.7, lineend='round')+
  scale_linetype_manual(values=c(4,3,2,1), labels=c("Fixed Terms of Trade", 
                                                    "Fixed Relative Expenditure Growth",
                                                    "Fixed Relative Import Intensity",
                                                    "Historical"))+
  scale_color_manual(values=c("black","black","black","black"),name="", labels=c("Fixed Terms of Trade", 
                                                                                 "Fixed Relative Expenditure Growth",
                                                                                 "Fixed Relative Import Intensity",
                                                                                 "Historical"))+
  theme(legend.position="bottom")
ggsave("5_figures/Figure_A4.png",dpi=1000, width=8, height=4)
# Figure 12: Counterfactual 2

p12<-cf%>%
  select(V1,V2, cf4,cf5,cf6)%>%
  pivot_longer(2:5)%>% 
  ggline(x="V1", y="value", color="name",
         xlab="", ylab="", legend.title="", linetype="name", 
         plot_type ="l", size=0.7, lineend='round')+
  scale_linetype_manual(values=c(4,3,2,1), labels=c("Terms of Trade Only", 
                                                    "Relative Expenditure Growth Only",
                                                    "Relative Import Intensity Only",
                                                    "Historical"))+
  scale_color_manual(values=c("black","black","black","black"),name="",
                     labels=c("Terms of Trade Only", 
                              "Relative Expenditure Growth Only",
                              "Relative Import Intensity Only",
                              "Historical"))+
  theme(legend.position="bottom")
ggsave("5_figures/Figure_A5.png",dpi=1000, width=8, height=4)





# ------------------------------------------------------------------------------
# SECTION 3: Tables
# ------------------------------------------------------------------------------

#Table 1: Summary Stats

sumstat<-india%>%
  select(TR:RA)%>%
  mutate_at(c(1:8),funs(.-lag(.)))%>%
  rename("Trade Ratio"=TR,
         "Foreign Import intensity"=FMI,
         "Domestic Import Intensity"=DMI,
         "Relative Import Intensity"=RMI,
          "Terms of Trade"=R,
         "Domestic Absorption"=DA,
         "Foreign Absorption"=FA,
         "Relative Absorption"=RA)%>%
  slice(-1)%>%
  summarise_each(funs(mean, sd, min, max)) %>%
  
  # Move summary stats to columns
  gather(key, value, everything()) %>% 
  separate(key, into = c("variable", "stat"), sep = "_") %>%
  spread(stat, value) %>%
  
  # Set order of summary statistics 
  select(variable, mean, sd, min, max)%>%
  rename(Mean=mean, SD=sd, Min=min, Max=max)%>%
  mutate_if(is.numeric, ~ . * 100)%>%
  # Round all numeric variables to one decimal point
mutate(across(2:5, round, 2))

#Export as tex

kable(sumstat,
      booktabs=T, col.names = c("Variable",
                                "Mean", "St. Dev",
                                "Min", "Max"), format="latex")%>%
  save_kable("5_figures/sumstat.tex")

# Export as word doc

rtffile <- RTF("5_figures/sumstat.doc")  # this can be an .rtf or a .doc
addParagraph(rtffile, "Table 1. Summary Statistics")
addTable(rtffile, as.data.frame(sumstat))
done(rtffile)

# Table-2 Decomposition over periods (goods)


#Defining decomp and decomp table func

decomp <- function(data, start, end){
  x <- data.frame(ratio=0, terms_of_trade=0, FA=0, DA=0,RA=0, FMI=0, DMI=0,RMI=0)
  x$ratio <- (data[data$year==end, 'TR'] - data[data$year==start, 'TR'])*100/(data[data$year==end, 'year']-data[data$year==start, 'year'])
  x$terms_of_trade <- (data[data$year==end, 'R'] - data[data$year==start, 'R'])*100/(data[data$year==end, 'year']-data[data$year==start, 'year'])
  x$FA <- (data[data$year==end, 'FA'] - data[data$year==start, 'FA'])*100/(data[data$year==end, 'year']-data[data$year==start, 'year'])
  x$DA <- (-1 * (data[data$year==end, 'DA'] - data[data$year==start, 'DA']))*100/(data[data$year==end, 'year']-data[data$year==start, 'year'])
  x$RA <- (data[data$year==end, 'RA'] - data[data$year==start, 'RA'])*100/(data[data$year==end, 'year']-data[data$year==start, 'year'])
  x$FMI <- (data[data$year==end, 'FMI'] - data[data$year==start, 'FMI'] )*100/(data[data$year==end, 'year']-data[data$year==start, 'year'])
  x$DMI <- (-1 * (data[data$year==end, 'DMI'] - data[data$year==start, 'DMI'] ))*100/(data[data$year==end, 'year']-data[data$year==start, 'year'])
  x$RMI <- (data[data$year==end, 'RMI'] - data[data$year==start, 'RMI'])*100/(data[data$year==end, 'year']-data[data$year==start, 'year'])
  return(x)
}

decomp.table <- function(data, starts, ends){
  x <- decomp(india, starts[1], ends[1])
  for (i in 2:length(starts)){
    x <- rbind(x, decomp(india, starts[i], ends[i]))
  }
  x$period <- paste(starts, ends, sep='-')
  #x <- x[,c(7, 1:6)]
  return(x)
}

#Decomposition table
starts <- c( 1980, 1991, 1999, 2003, 2012, 2016)
ends <- c(1991, 1999, 2003, 2012, 2016, 2021)
decomposition <- decomp.table(india, starts, ends)%>%
  select(9,1:8)
decomposition$terms_of_trade<-
  as.numeric(decomposition$terms_of_trade)
decomposition$ratio<-as.numeric(decomposition$ratio)
#Generating latex code

decomposition<-decomposition%>%
  mutate(across(2:9, round, 2))
kable(decomposition, col.names = 
        c("Period","$r$","$p$","$y^{*}$","$y$",
          "$g$","$x$","$m$", "$s$"),format="latex",escape = F,
      booktabs=T)%>%
  save_kable("5_figures/decomp_goods.tex")

#Pull word file for editing

colnames(decomposition)<-c("Period","r","p","y*","y",
                           "g","x","m", "s")

rtffile <- RTF("5_figures/decomp.doc")
addParagraph(rtffile, "Table 2. Annualised period-wise decomposition of India’s merchandise trade ratio")
addTable(rtffile, as.data.frame(decomposition))
done(rtffile)


# Table-3: Average export and import growth

trade<-india%>%select(year, export_real, import_real)

avg<-function(data, start, end){
  y<-data.frame(export_real=0, import_real=0)
  y$export_real<-((data[data$year==end, 'export_real'] - data[data$year==start, 'export_real'])/(data[data$year==start, 'export_real']))/(data[data$year==end, 'year']-data[data$year==start, 'year'])
  y$import_real<-((data[data$year==end, 'import_real'] - data[data$year==start, 'import_real'])/(data[data$year==start, 'import_real']))/(data[data$year==end, 'year']-data[data$year==start, 'year'])
  return(y)
}

avg_tab <- function(data, starts, ends){
  x <- avg(trade, starts[1], ends[1])
  for (i in 2:length(starts)){
    x <- rbind(x, avg(trade, starts[i], ends[i]))
  }
  x$period <- paste(starts, ends, sep='-')
  #x <- x[,c(7, 1:6)]
  return(x)
}

starts <- c( 1980, 1991, 1999, 2003, 2012, 2016)
ends <- c(1991, 1999, 2003, 2012, 2016, 2021)
avg_table <- avg_tab(trade, starts, ends)%>%
  select(3, 1:2)%>%
  mutate(export_real=export_real*100, import_real=import_real*100)%>%
  mutate(across(2:3, round, 2))

#Writing output to tex file

kable(avg_table, col.names = 
        c("Period", "Real Exports", "Real Imports"),format="latex",
      booktabs=T)%>%
  save_kable("5_figures/avg_trade_growth.tex")


# Writing output to word file (for journal)

colnames(avg_table)<-c("Period", "Real Exports", "Real Imports")
rtffile <- RTF("5_figures/avg_table.doc")
addParagraph(rtffile, "Table 3 Periodwise growth of India's exports and imports")
addTable(rtffile, as.data.frame(avg_table))
done(rtffile)




# ------------------------------------------------------------------------------
# SECTION 4: Tables (Appendix)
# ------------------------------------------------------------------------------

# Defining starts and ends for services

starts2<-c(1995,2001,2007)
ends2<-c(2000,2006,2012)

decomp_services<-decomp.table(gs, starts2,ends2)%>%
  select(9,1:8)%>% mutate(across(2:9, round, 2))

#Writing output to tex file

kable(decomp_services, col.names = 
        c("Period","$r$","$p$","$y^{*}$","$y$",
          "$g$","$x$","$m$", "$s$"),format="latex",escape = F,
      booktabs=T)%>%
  save_kable("5_figures/decomp_goods_services.tex")


# Writing output to word file (for journal)

colnames(decomp_services)<-c("Period","r","p","y*","y",
                           "g","x","m", "s")

rtffile <- RTF("5_figures/decomp_services.doc")
addParagraph(rtffile, "Table 2. Annualised period-wise decomposition of India’s trade ratio")
addTable(rtffile, as.data.frame(decomp_services))
done(rtffile)
