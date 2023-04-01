library(readxl)
library(ggplot2)
library(reshape2)
library(readxl)
library(reshape2)
library(viridis)
library(plotly)
library(ggplot2)

#Loading the file

india_1<-read.csv("india_gpluss.csv")

#Defining logs

india_1$logFMI <- log(india_1$FMI)
india_1$logDMI <- log(india_1$DMI)
india_1$logFA <- log(india_1$RA * india_1$DA)
india_1$logDA <- log(india_1$DA)

#Defining decomp function
decomp <- function(data, start, end){
  x <- data.frame(ratio=0, terms_of_trade=0, FA=0, DA=0, FMI=0, DMI=0)
  x$ratio <- data[data$year==end, 'log_TR'] - data[data$year==start, 'log_TR']
  x$terms_of_trade <- data[data$year==end, 'log_R'] - data[data$year==start, 'log_R'] 
  x$FA <- data[data$year==end, 'logFA'] - data[data$year==start, 'logFA'] 
  x$DA <- -1 * (data[data$year==end, 'logDA'] - data[data$year==start, 'logDA'] )
  x$FMI <- data[data$year==end, 'logFMI'] - data[data$year==start, 'logFMI'] 
  x$DMI <- -1 * (data[data$year==end, 'logDMI'] - data[data$year==start, 'logDMI'] )
  return(x)
}

x <- as.data.frame(decomp(india_1, 1995, 2018))

decomp.table <- function(data, starts, ends){
  x <- decomp(india_1, starts[1], ends[1])
  for (i in 2:length(starts)){
    x <- rbind(x, decomp(india_1, starts[i], ends[i]))
  }
  x$period <- paste(starts, ends, sep='-')
  x <- x[,c(7, 1:6)]
  return(x)
}

starts <- c( 1995, 2001, 2006, 2011, 2016)
ends <- c(2000, 2005, 2010, 2015, 2018)
decomposition <- decomp.table(india_1, starts, ends)


#Counterfactuals

# counterfactuals
# 1 is no change in terms of trade
# 2 is no change in income growth
# 3 is no change in expenditure shares
# 4 is terms of trade only
# 5 is income growth only
# 6 is expenditure switching only

start.year <- 1

india_1.temp <- india_1[india_1$year > (start.year-1),]
cf1 <- cf2 <- cf3 <- cf4 <- cf5 <- cf6 <- india_1.temp$log_TR
n <- length(cf1)
india_1.temp[1,9]<-0
india_1.change <- india_1.temp[-1, -1]- india_1.temp[-n, -1]

for (i in 1:(n-1)){
  cf1[i+1] <- cf1[i] + india_1.change$log_RMI[i] + india_1.change$log_RA[i]
  cf2[i+1] <- cf2[i] + india_1.change$log_R[i]  + india_1.change$log_RMI[i] 
  cf3[i+1] <- cf3[i] + india_1.change$log_R[i]  + india_1.change$log_RA[i] 
  cf4[i+1] <- cf4[i] + india_1.change$log_R[i]
  cf5[i+1] <- cf5[i] + india_1.change$log_RA[i]
  cf6[i+1] <- cf6[i] + india_1.change$log_RMI[i] 
}

fig3<-plot_ly(india_1.temp, x=~year, y=~TR, name="Historical", type="scatter", mode="lines", line = list(color="black"))
fig3<-fig3 %>% add_trace(y=~exp(cf1),name="Fixed Terms of Trade",type="scatter", mode="lines", line = list(color="red")) 
fig3<-fig3 %>% add_trace(y=~exp(cf2),name="Fixed Relative Income Growth",type="scatter", mode="lines", line = list(color="blue")) 

fig3<-fig3 %>% add_trace(y=~exp(cf3), name="Fixed Expenditure Share", type="scatter", mode="lines", line = list(color="green")) 
fig3 <- fig3 %>% layout(title = "",
                        xaxis = list(title = ""),
                        yaxis = list (title = "Trade Ratio"))
fig3



fig4<-plot_ly(india.temp, x=~year, y=~TR, name="Historical", type="scatter", mode="lines", line = list(color="black"))
fig4<-fig4 %>% add_trace(y=~exp(cf4),name=" Terms of Trade Only",type="scatter", mode="lines", line = list(color="red")) 
fig4<-fig4 %>% add_trace(y=~exp(cf5),name=" Relative Income Growth Only",type="scatter", mode="lines", line = list(color="blue")) 

fig4<-fig4 %>% add_trace(y=~exp(cf6), name=" Expenditure Share Only", type="scatter", mode="lines", line = list(color="green")) 
fig4 <- fig4 %>% layout(title = "",
                        xaxis = list(title = "", range=c(1980, 2018)),
                        yaxis = list (title = "Trade Ratio"))
fig4
