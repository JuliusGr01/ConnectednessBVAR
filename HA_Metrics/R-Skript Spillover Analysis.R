#######################Bachelorarbeit Julius Gross########################
##########################################################################
###############An analysis of volatility spillovers#######################
#######from Europe to Sub-Saharan Africa and their determinants###########
##########################################################################


# Packages  ---------------------------------------------------------------


#Required packages
'install.packages("Spillover")
install.packages("vars")
install.packages("igraph")
install.packages("PerformanceAnalytics")
install.packages("quantmod")
install.packages("plyr")
install.packages("xts")
install.packages("reshape2")
install.packages("dplyr")
install.packages("psych")
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("stargazer")
install.packages("ggthemes")
install.packages("fred")
install.packages("extrafont")
install.packages("tikzDevice")
install.packages("devtools")
install.packages("ecm")
install.packages("patchwork")
install.packages("cowplot")
install.packages("ConnectednessApproach")
'


devtools::install_github("lukerobert/luketools")

library(ConnectednessApproach)
library(patchwork)
library(luketools)
library(tikzDevice)
library(extrafont)
library(stargazer)
library(readxl)
library(Spillover)
library(vars)
library(plyr)
library(vars)
library(igraph)
library(PerformanceAnalytics)
library(quantmod)
library(plyr)
library(dplyr)
library(reshape2)
library(xts)
library(ggplot2)
library(stringr)
library(psych)
library(tidyverse)
library(zoo)
library(ggpubr)
library(ggthemes)
library(ecm)
library(cowplot)


# FUNCTIONS ---------------------------------------------------------------

#Function for adding recession shading to my plots:
add_rec_shade<-function(st_date,ed_date,shade_color="darkgray")
{
  library(fredr)
  library(ecm)
  library(ggplot2)
  fredr_set_key("d3a5fb2cec9ff9f931c4156077a0d23b") # Here, one would probably have to set an individual key... 
  
  st_date<-as.Date("2014-02-01")
  ed_date<-as.Date("2022-07-01")
  
  recession<-fredr(series_id = "EUROREC",observation_start = as.Date(st_date),observation_end = as.Date(ed_date))
  
  recession$diff<-recession$value-lagpad(recession$value,k=1)
  recession<-recession[!is.na(recession$diff),]
  recession.start<-recession[recession$diff==1,]$date
  recession.end<-recession[recession$diff==(-1),]$date
  
  if(length(recession.start)>length(recession.end))
  {recession.end<-c(recession.end,Sys.Date())}
  if(length(recession.end)>length(recession.start))
  {recession.start<-c(min(recession$date),recession.start)}
  
  recs<-as.data.frame(cbind(recession.start,recession.end))
  recs$recession.start<-as.Date(as.numeric(recs$recession.start),origin=as.Date("1970-01-01"))
  recs$recession.end<-as.Date(recs$recession.end,origin=as.Date("1970-01-01"))
  if(nrow(recs)>0)
  {
    rec_shade<-geom_rect(data=recs, inherit.aes=F, 
                         aes(xmin=recession.start, xmax=recession.end, ymin=-Inf, ymax=+Inf), 
                         fill="pink", alpha=0.3)
    return(rec_shade)
  }
}


# Set-up and volatility computation ---------------------------------------


# 1.1 Importing data ------------------------------------------------------


#The combination of the absolut raw data has taken place in STATA. I now import the dataset created in STATA:

setwd("/Users/juliusgross/Desktop/Bachelorarbeit Julius Gross/R")

price.data.daily <- read_excel("dataset.xlsx")
price.daily <- subset(price.data.daily, select = -c(date1))

#Using the formula for daily volatility from Diebold and Yilmaz (2012):

#Annulized:
price.daily$v.Nigeria <- 100*asinh(sqrt(252*0.361*log(price.daily$Nigeria_H/(price.daily$Nigeria_L))^2))
price.daily$v.Namibia <- 100*asinh(sqrt(252*0.361*log(price.daily$Namibia_H/(price.daily$Namibia_L))^2))
price.daily$v.Kenya <- 100*asinh(sqrt(252*0.361*log(price.daily$Kenya_H/(price.daily$Kenya_L))^2))
price.daily$v.SA <- 100*asinh(sqrt(252*0.361*log(price.daily$SA_H/(price.daily$SA_L))^2))
price.daily$v.EURO <- 100*asinh(sqrt(252*0.361*log(price.daily$EURO_H/(price.daily$EURO_L))^2))

vola.daily <- as.data.frame(price.daily)

#Not annulized and not in %:
price.daily.2 <- price.daily
price.daily.2$v.Nigeria <- asinh(sqrt(0.361*log(price.daily$Nigeria_H/(price.daily$Nigeria_L))^2))
price.daily.2$v.Namibia <- asinh(sqrt(0.361*log(price.daily$Namibia_H/(price.daily$Namibia_L))^2))
price.daily.2$v.Kenya <- asinh(sqrt(0.361*log(price.daily$Kenya_H/(price.daily$Kenya_L))^2))
price.daily.2$v.SA <- asinh(sqrt(0.361*log(price.daily$SA_H/(price.daily$SA_L))^2))
price.daily.2$v.EURO <- asinh(sqrt(0.361*log(price.daily$EURO_H/(price.daily$EURO_L))^2))

vola.daily.2 <- as.data.frame(price.daily.2)

#In the case of Kenya, it becomes visible, that the data stops being valuable for us as of March 2014 because only one price is indicated from that time backwards.
#Thus, I delete the time span for all observations.

daily.vola <- vola.daily[-c(1:529),]

daily.vola <-  subset(daily.vola, select = -c(Nigeria_H, Nigeria_L,Namibia_H, Namibia_L,Kenya_H,Kenya_L,SA_H, SA_L, EURO_H, EURO_L))

daily.vola.NA <- na.omit(daily.vola)

daily.vola.NA <- as.data.frame(daily.vola.NA)
daily.vola.NA.2 <- daily.vola.NA  #Just so that I have a backup without the data variable as the row name
row.names(daily.vola.NA) <- as.Date(daily.vola.NA$Date)
daily.vola.NA$Date <- NULL



#Same for the "not-annulized data set":

daily.vola.2 <- vola.daily.2[-c(1:529),]

daily.vola.2 <-  subset(daily.vola.2, select = -c(Nigeria_H, Nigeria_L,Namibia_H, Namibia_L,Kenya_H,Kenya_L,SA_H, SA_L, EURO_H, EURO_L))

daily.vola.2.na <- na.omit(daily.vola.2)

daily.vola.2.na <- as.data.frame(daily.vola.2.na)

row.names(daily.vola.2.na) <- as.Date(daily.vola.2.na$Date)

daily.vola.2.na$Date <- NULL

# 1.2 Data: Summary Statistics and Visualisation --------------------------


# #TABLE 1: Log annualized volatility summary statistics, 5 stocks: -------

summary(daily.vola.NA)
summary(daily.vola.2.na)

#I use the annualiued standard deviations (daily.vola) to plot and the daily.vola.2.na to create a log volatility summary statistics

#Export to LaTEX format:
'
stargazer(daily.vola.NA)
stargazer(daily.vola.2.na)
'

#Alternatively, not annulized:

describe(daily.vola.NA) #This is with curtosis and with skewness
summarystat.daily.vola.2 <- as.data.frame(describe(daily.vola.2.na))
summarystat.daily.vola.2 <- data.frame(t(summarystat.daily.vola.2[-1]))
stargazer(summarystat.daily.vola.2, summary = FALSE, digits = 6)
summary(daily.vola.2.na)


#Working with daily.vola.NA and plotting each volatility measure over time

#For adding a recession shading, I now specify:

fredr_set_key("d3a5fb2cec9ff9f931c4156077a0d23b") # Here, individual key would have to be entered...
fred.data = fredr(
  series_id = "EUROREC",
  observation_start = as.Date("2014-02-01"),
  observation_end = as.Date("2022-07-01")
)
names(fred.data)[1] <- "Date"


daily.vola.NA.2$Date <- as.Date(daily.vola.NA.2$Date, "%Y-%m-%d")

#I know graph the data for each country:

nigeria.1 = ggplot(daily.vola.NA.2, aes(x = Date, y = v.Nigeria)) +
  geom_line() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(0,120), breaks = seq(0, 120, 20)) +
  labs(title = "(a) Nigeria - NSE All Share",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(0,0,0, 0), "cm")) +
  theme_bw()

'tikz(file = "YYY/Volanigeria.tex", standAlone=T)
nigeria.1
dev.off()'


namibia.1 <- ggplot(subset(daily.vola.NA.2, v.Namibia <=300), aes(x = Date, y = v.Namibia)) +
  geom_line() +
  add_rec_shade(min(fred.data$date),max(fred.data$date)) +
  scale_y_continuous(limits = c(0,120), breaks = seq(0, 120, 20)) +
  labs(title = "(b) Namibia - FTSE NSX Overall",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(0,0,0, 0), "cm"))+
  theme_bw()

'tikz(file = "YYY/Volanamibia.tex", standAlone=T)
namibia.1
dev.off()'


kenya.1 <- ggplot(daily.vola.NA.2, aes(x = Date, y = v.Kenya)) +
  geom_line() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(0,120), breaks = seq(0, 120, 20)) +
  labs(title = "(c) Kenya - FTSE NSE Kenya",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(0,0,0, 0), "cm"))+
  theme_bw()

'tikz(file = "YYY/Volakenya.tex", standAlone=T)
kenya.1
dev.off()'

sa.1 <- ggplot(daily.vola.NA.2, aes(x = Date, y = v.SA)) +
  geom_line() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(0,120), breaks = seq(0, 120, 20)) +
  labs(title = "(d) South Africa - South Afria Top 40",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(0,0,0, 0), "cm"))+
  theme_bw()

'tikz(file = "YYY/Volasa.tex", standAlone=T)
sa.1
dev.off()'

eu.1 <- ggplot(daily.vola.NA.2, aes(x = Date, y = v.EURO)) +
  geom_line() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(0,120), breaks = seq(0, 120, 20)) +
  labs(title = "(e) Europe - EUROSTOXX 50 ",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(0,0,0, 0), "cm"))+
  theme_bw()

'tikz(file = "YYY/Volaeu.tex", standAlone=T)
eu.1
dev.off()'


# Figure 1 - Equity market volatility -------------------------------------

'tikz(file = "YYY/Volaall.tex", standAlone=T)
plot_grid(nigeria.1, namibia.1, kenya.1, sa.1, eu.1, align = "hv", ncol = 1)
dev.off()'


# 2. Computing the spillover measures -------------------------------------

#I make use of the package "ConnectednessApproach"

# 2.1 Optimal Lag-length identification ---------------------------------------

#Before calculating the spillover measures, I first try to figure out the optimal lag length:

vola.var = vars::VAR(daily.vola.NA, p=1, type="none") 
vola.var.2 = vars::VAR(daily.vola.NA, p=2, type="none")
vola.var.3 = vars::VAR(daily.vola.NA, p=3, type="none")
vola.var.4 = vars::VAR(daily.vola.NA, p=4, type="none")
vola.var.5 = vars::VAR(daily.vola.NA, p=5, type="none")
vola.var.6 = vars::VAR(daily.vola.NA, p=6, type="none")
vola.var.7 = vars::VAR(daily.vola.NA, p=7, type="none")
vola.var.8 = vars::VAR(daily.vola.NA, p=8, type="none")
vola.var.9 = vars::VAR(daily.vola.NA, p=9, type="none")
vola.var.10 = vars::VAR(daily.vola.NA, p=10, type="none")
vola.var.11 = vars::VAR(daily.vola.NA, p=11, type="none")
vola.var.12 = vars::VAR(daily.vola.NA, p=12, type="none")
vola.var.13 = vars::VAR(daily.vola.NA, p=13, type="none")
vola.var.14 = vars::VAR(daily.vola.NA, p=14, type="none")

AIC(vola.var)
AIC(vola.var.2)
AIC(vola.var.3)
AIC(vola.var.4)
AIC(vola.var.5)
AIC(vola.var.6)
AIC(vola.var.7)
AIC(vola.var.8)
AIC(vola.var.9)
AIC(vola.var.10)
AIC(vola.var.11)
AIC(vola.var.12)
AIC(vola.var.13)
AIC(vola.var.14)

BIC(vola.var)
BIC(vola.var.2)
BIC(vola.var.3)
BIC(vola.var.4)
BIC(vola.var.5)
BIC(vola.var.6)
BIC(vola.var.7)
BIC(vola.var.8)
BIC(vola.var.9)
BIC(vola.var.10)
BIC(vola.var.11)
BIC(vola.var.12)
BIC(vola.var.13)
BIC(vola.var.14)

#I make use of the BIC and thus in the following measures are calculated for VAR(3) and H=12

# 2.2 Dynamic directional volatility spillover --------------------------

#The goal of this section is to compute directioanl volatility spillover plots.
#The information I am looking for is contained in the "Directional TO others" row and 
#in the "Directional FROM others" column. 

#The package requires a zoo-input
daily.vola.NA.zoo <- as.zoo(daily.vola.NA)

#For VAR(3) and H=12:
#First I estimate the basic FEVD, according to the package instructions:

#Preparation of Graphics and Spillover-measures: Estimation of the whole model, with VAR(3), H=12 and a rolling window of 200 days: 

connectedness.3.12 = ConnectednessApproach(daily.vola.NA.zoo,
                                           nlag = 3,
                                           nfore = 12,
                                           window.size = 200,
                                           corrected = FALSE,
                                           model = c("VAR"),
                                           connectedness = c("Time"),
                                           Connectedness_config = list(TimeConnectedness = list(generalized = TRUE),
                                                                       FrequencyConnectedness = list(partition = c(pi, pi/2, 0), generalized = TRUE,
                                                                                                     scenario = "ABS"))
)



# Table 3: Spillover Table ------------------------------------------------

spillover.table <- connectedness.3.12$TABLE
stargazer(spillover.table, summary=FALSE, digits=2)

#Computing spillover index by hand:
mean(connectedness.3.12$TCI)


# Figure 2 ----------------------------------------------------------------

#Preparation and 2 different versions

data.tci <- as.data.frame(connectedness.3.12$TCI)

'tikz(file = "YYY/TotalvolaspillDY.tex", standAlone=T)
PlotTCI(connectedness.3.12, ylim=c(0,70))
dev.off()'

data.tci$date <- row.names(data.tci)
data.tci <- as.data.frame(data.tci)
data.tci$date <- as.Date(data.tci$date)

tci.3.12 = ggplot(data.tci, aes(x = date, y = TCI)) +
  geom_line() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  labs(title = "",
       x = "Date",
       y = "") 

#Final: Used Figure 2

'tikz(file = "YYY/Totalvolaspillfig2.tex", standAlone=T)
tci.3.12
dev.off()'


#Figure 3
#Preparation:
data.to  <- as.data.frame(connectedness.3.12$TO)
data.to$date <- row.names(data.to)
data.to$date <- as.Date(data.to$date)



to.3.12.Nigeria = ggplot(data.to, aes(x = date, y = v.Nigeria)) +
  geom_line() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(0,130), breaks = seq(0, 120, 20)) +
  labs(title = "(a) Nigeria",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,0,-0.25,1), "cm"))+
  theme_bw()

to.3.12.Namibia = ggplot(data.to, aes(x = date, y = v.Namibia)) +
  geom_line() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(0,130), breaks = seq(0, 120, 20)) +
  labs(title = "(b) Namibia",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,1,-0.25,0), "cm"))+
  theme_bw()

to.3.12.Kenya = ggplot(data.to, aes(x = date, y = v.Kenya)) +
  geom_line() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(0,130), breaks = seq(0, 120, 20)) +
  labs(title = "(c) Kenya",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,0,-0.25,1), "cm"))+
  theme_bw()

to.3.12.SA = ggplot(data.to, aes(x = date, y = v.SA)) +
  geom_line() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(0,130), breaks = seq(0, 120, 20)) +
  labs(title = "(d) South Africa",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,1,-0.25,0), "cm"))+
  theme_bw()

to.3.12.EU = ggplot(data.to, aes(x = date, y = v.EURO)) +
  geom_line() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(0,130), breaks = seq(0, 120, 20)) +
  labs(title = "(e) Europe",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,0,-0.25,1), "cm"))

to.empty = NULL +
  theme(plot.margin=unit(c(-0.25,1,-0.25, 0), "cm"))




# Figure 3 ----------------------------------------------------------------

'tikz(file = "YYY/directional.to.newest.tex", standAlone=T)
to <- to.3.12.Nigeria+ to.3.12.Namibia+to.3.12.Kenya+to.3.12.SA+to.3.12.EU+to.empty + plot_layout(ncol = 2) + theme(panel.spacing = unit(c(-0.5,0-0.5,0), "lines")) + theme_bw() +
  theme(plot.margin = unit(c(0,0,0,0), "lines")) +
  theme(strip.background = element_blank()) +
  theme(plot.background = element_blank()) +
  theme(strip.text = element_blank()) 
dev.off()
'

'tikz(file = "YYY/directional.to.newest.tex", standAlone=T)
grid.draw(to)
dev.off()'


'tikz(file = "YYY/Totalvolaspillfig2.tex", standAlone=T)
tci.3.12
dev.off()'

#Alternatively:
'tikz(file = "YYY/directional.to.tex", standAlone=T)
to.3.12.Nigeria+ to.3.12.Namibia + to.3.12.Kenya + to.3.12.SA + to.3.12.EU + plot_layout(ncol = 2)
dev.off()'


#Figure 4
#Preparation

data.from  <- as.data.frame(connectedness.3.12$FROM)
data.from$date <- row.names(data.from)
data.from$date <- as.Date(data.from$date)

from.3.12.Nigeria = ggplot(data.from, aes(x = date, y = v.Nigeria)) +
  geom_line() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(0,70), breaks = seq(0, 70, 10)) +
  labs(title = "(a) Nigeria",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,0,-0.25,1), "cm"))+
  theme_bw()


from.3.12.Namibia = ggplot(data.from, aes(x = date, y = v.Namibia)) +
  geom_line() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(0,70), breaks = seq(0, 120, 10)) +
  labs(title = "(b) Namibia",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,1,-0.25,0), "cm"))+
  theme_bw()

from.3.12.Kenya = ggplot(data.from, aes(x = date, y = v.Kenya)) +
  geom_line() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(0,70), breaks = seq(0, 70, 10)) +
  labs(title = "(c) Kenya",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,0,-0.25,1), "cm"))+
  theme_bw()

from.3.12.SA = ggplot(data.from, aes(x = date, y = v.SA)) +
  geom_line() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(0,70), breaks = seq(0, 70, 10)) +
  labs(title = "(d) South Africa",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,1,-0.25,0), "cm"))+
  theme_bw()


from.3.12.EU = ggplot(data.from, aes(x = date, y = v.EURO)) +
  geom_line() +
  scale_y_continuous(limits = c(0,70), breaks = seq(0, 70, 10)) +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  labs(title = "(e) Europe",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,0,-0.25,1), "cm"))+
  theme_bw()

from.empty = NULL +
  theme(plot.margin=unit(c(-0.25,1,-0.25, 0), "cm"))


# Figure 4 ----------------------------------------------------------------

from <- from.3.12.Nigeria+ from.3.12.Namibia+from.3.12.Kenya+from.3.12.SA+from.3.12.EU+from.empty + plot_layout(ncol = 2) + theme(panel.spacing = unit(c(-0.5,0-0.5,0), "lines")) + theme_bw() +
  theme(plot.margin = unit(c(0,0,0,0), "lines")) +
  theme(strip.background = element_blank()) +
  theme(plot.background = element_blank()) +
  theme(strip.text = element_blank()) 

'tikz(file = "YYY/directional.from.newest.tex", standAlone=T)
grid.draw(from)
dev.off()'


#Alternatively:
'tikz(file = "YYY/directional.from.tex", standAlone=T)
from.3.12.Nigeria+ from.3.12.Namibia + from.3.12.Kenya + from.3.12.SA + from.3.12.EU + from.empty + plot_layout(ncol = 2)
dev.off()'


#Figure 5: Net-spillovers
#Preparation

data.net  <- as.data.frame(connectedness.3.12$NET)
data.net$date <- row.names(data.net)
data.net$date <- as.Date(data.net$date)

net.3.12.Nigeria = ggplot(data.net, aes(x = date, y = v.Nigeria)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-40,86), breaks = seq(-40, 80, 20)) +
  labs(title = "(a) Nigeria",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,0,-0.25,1), "cm"))+
  theme_bw()

net.3.12.Namibia = ggplot(data.net, aes(x = date, y = v.Namibia)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-40,86), breaks = seq(-40, 80, 20)) +
  labs(title = "(b) Namibia",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,1,-0.25,0), "cm"))+
  theme_bw()

net.3.12.Kenya = ggplot(data.net, aes(x = date, y = v.Kenya)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-40,86), breaks = seq(-40, 80, 20)) +
  labs(title = "(c) Kenya",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,0,-0.25,1), "cm"))+
  theme_bw()

net.3.12.SA = ggplot(data.net, aes(x = date, y = v.SA)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-40,86), breaks = seq(-40, 80, 20)) +
  labs(title = "(d) South Africa",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,1,-0.25,0), "cm"))+
  theme_bw()

net.3.12.EU = ggplot(data.net, aes(x = date, y = v.EURO)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-40,86), breaks = seq(-40, 80, 20)) +
  labs(title = "(e) Europe",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,0,-0.25,1), "cm"))+
  theme_bw()


# Figure 5 ----------------------------------------------------------------

net.3.12 <- net.3.12.Nigeria + net.3.12.Namibia + net.3.12.Kenya + net.3.12.SA + net.3.12.EU + plot_layout(ncol = 2) + theme(panel.spacing = unit(c(-0.5,0,0.5,0), "lines")) + theme_bw() +
  theme(plot.margin = unit(c(0,0,0,0), "lines")) +
  theme(strip.background = element_blank()) +
  theme(plot.background = element_blank()) +
  theme(strip.text = element_blank()) 

'tikz(file = "YYY/net.newest.tex", standAlone=T)
grid.draw(net.3.12)
dev.off()'


#Alternatively:
'tikz(file = "YYY/net.tex", standAlone=T)
net.3.12.Nigeria+ net.3.12.Namibia + net.3.12.Kenya + net.3.12.SA + net.3.12.EU + plot_layout(ncol = 2)
dev.off()'

summary(data.net$v.Nigeria)
summary(data.net$v.Namibia)
summary(data.net$v.Kenya)
summary(data.net$v.SA)
summary(data.net$v.EURO)


#Figure 6: Pairwise Net-spillovers
#Preparation

'tikz(file = "YYY/net.pairwise.tex", standAlone=T)
PlotNPDC(connectedness.3.12)
dev.off()
'

data.net.pairwise  <- as.data.frame(connectedness.3.12$NPDC)

date = as.Date(dimnames(connectedness.3.12$NPDC)[[3]])

data.net.pairwise <- transpose(data.net.pairwise)
data.net$date <- row.names(data.net)
data.net$date <- as.Date(data.net$date)

net.3.12.Nigeria = ggplot(data.net, aes(x = date, y = v.Nigeria)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-62,63), breaks = seq(-60, 60, 10)) +
  labs(title = "(a) Nigeria",
       x = "Date",
       y = "")

net.3.12.Namibia = ggplot(data.net, aes(x = date, y = v.Namibia)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-62,63), breaks = seq(-60, 60, 10)) +
  labs(title = "(b) Namibia",
       x = "Date",
       y = "")

net.3.12.Kenya = ggplot(data.net, aes(x = date, y = v.Kenya)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-62,63), breaks = seq(-40, 60, 10)) +
  labs(title = "(c) Kenya",
       x = "Date",
       y = "")

net.3.12.SA = ggplot(data.net, aes(x = date, y = v.SA)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-62,63), breaks = seq(-40, 60, 10)) +
  labs(title = "(d) South Africa",
       x = "Date",
       y = "")

net.3.12.EU = ggplot(data.net, aes(x = date, y = v.EURO)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-25,86), breaks = seq(-20, 80, 20)) +
  labs(title = "(e) Europe",
       x = "Date",
       y = "")

'tikz(file = "YYY/net.tex", standAlone=T)
net.3.12.Nigeria+ net.3.12.Namibia + net.3.12.Kenya + net.3.12.SA + net.3.12.EU + plot_layout(ncol = 2)
dev.off()
'

# 2.3 Quarterly values for VAR(3) with N=12 ----------------------------------

#Even though is is something that we would be looking for, the format is not practical for further calculations. The format is, at least with my knowledge not changeable.
#Unfortunately, the package I am using does not include a possibility to get a bidirectional spillover table. Thus, I am dependent on another package:



#The panel data is computed with quarterly data. Hence I need quartelized bidrectional spillovers between Europe and each African country.


Pairwise.Connectedness <- connectedness.3.12$PCI
Pairwise.Connectedness.1 <- as.data.frame.table(Pairwise.Connectedness) 

Pairwise.Connectedness.EU.Nigeria <- subset(Pairwise.Connectedness.1, Pairwise.Connectedness.1$Var1 =="v.EURO" & Pairwise.Connectedness.1$Var2 =="v.Nigeria")
Pairwise.Connectedness.EU.Namibia <- subset(Pairwise.Connectedness.1, Pairwise.Connectedness.1$Var1 =="v.EURO" & Pairwise.Connectedness.1$Var2 =="v.Namibia")
Pairwise.Connectedness.EU.Kenya <- subset(Pairwise.Connectedness.1, Pairwise.Connectedness.1$Var1 =="v.EURO" & Pairwise.Connectedness.1$Var2 =="v.Kenya")
Pairwise.Connectedness.EU.SA <- subset(Pairwise.Connectedness.1, Pairwise.Connectedness.1$Var1 =="v.EURO" & Pairwise.Connectedness.1$Var2 =="v.SA")


#Now, we need quarterly data instead of daily data. Thus, I compute the quarterly averages over the daily values:

library(tidyverse)
library(lubridate)

Pairwise.Connectedness.EU.Nigeria$Var3 <- as.Date(Pairwise.Connectedness.EU.Nigeria$Var3)

Pairwise.Quarterly.Connectedness.EU.Nigeria <- Pairwise.Connectedness.EU.Nigeria %>% mutate(year_quarter = as.yearqtr(Var3, with_year = TRUE)) %>%
  group_by(year_quarter) %>%
  summarize(Freq = mean(Freq))

Pairwise.Quarterly.Connectedness.EU.Nigeria$`@REF_AREA` <- "NG"

Pairwise.Quarterly.Connectedness.EU.Nigeria$Quarter <- format(Pairwise.Quarterly.Connectedness.EU.Nigeria$year_quarter, format = "%Y-Q%q")

names(Pairwise.Quarterly.Connectedness.EU.Nigeria)[2] <- "Spillover"
names(Pairwise.Quarterly.Connectedness.EU.Nigeria)[4] <- "@TIME_PERIOD"

Pairwise.Quarterly.Connectedness.EU.Nigeria$year_quarter <- NULL
#Quarterly spillovers for Nigeria


Pairwise.Connectedness.EU.Namibia$Var3 <- as.Date(Pairwise.Connectedness.EU.Namibia$Var3)

Pairwise.Quarterly.Connectedness.EU.Namibia <- Pairwise.Connectedness.EU.Namibia %>% mutate(year_quarter = as.yearqtr(Var3, with_year = TRUE)) %>%
  group_by(year_quarter) %>%
  summarize(Freq = mean(Freq))

Pairwise.Quarterly.Connectedness.EU.Namibia$`@REF_AREA` <- "NA"

Pairwise.Quarterly.Connectedness.EU.Namibia$Quarter <- format(Pairwise.Quarterly.Connectedness.EU.Namibia$year_quarter, format = "%Y-Q%q")

names(Pairwise.Quarterly.Connectedness.EU.Namibia)[2] <- "Spillover"
names(Pairwise.Quarterly.Connectedness.EU.Namibia)[4] <- "@TIME_PERIOD"
Pairwise.Quarterly.Connectedness.EU.Namibia$year_quarter <- NULL
#Quarterly spillovers for Namibia

Pairwise.Connectedness.EU.Kenya$Var3 <- as.Date(Pairwise.Connectedness.EU.Kenya$Var3)

Pairwise.Quarterly.Connectedness.EU.Kenya <- Pairwise.Connectedness.EU.Kenya %>% mutate(year_quarter = as.yearqtr(Var3, with_year = TRUE)) %>%
  group_by(year_quarter) %>%
  summarize(Freq = mean(Freq))

Pairwise.Quarterly.Connectedness.EU.Kenya$`@REF_AREA` <- "KE"

Pairwise.Quarterly.Connectedness.EU.Kenya$Quarter <- format(Pairwise.Quarterly.Connectedness.EU.Kenya$year_quarter, format = "%Y-Q%q")

names(Pairwise.Quarterly.Connectedness.EU.Kenya)[2] <- "Spillover"
names(Pairwise.Quarterly.Connectedness.EU.Kenya)[4] <- "@TIME_PERIOD"
Pairwise.Quarterly.Connectedness.EU.Kenya$year_quarter <- NULL
#Quarterly spillovers for Kenya 

Pairwise.Connectedness.EU.SA$Var3 <- as.Date(Pairwise.Connectedness.EU.SA$Var3)

Pairwise.Quarterly.Connectedness.EU.SA <- Pairwise.Connectedness.EU.SA %>% mutate(year_quarter = as.yearqtr(Var3, with_year = TRUE)) %>%
  group_by(year_quarter) %>%
  summarize(Freq = mean(Freq))

Pairwise.Quarterly.Connectedness.EU.SA$`@REF_AREA` <- "ZA"

Pairwise.Quarterly.Connectedness.EU.SA$Quarter <- format(Pairwise.Quarterly.Connectedness.EU.SA$year_quarter, format = "%Y-Q%q")

names(Pairwise.Quarterly.Connectedness.EU.SA)[2] <- "Spillover"
names(Pairwise.Quarterly.Connectedness.EU.SA)[4] <- "@TIME_PERIOD"
Pairwise.Quarterly.Connectedness.EU.SA$year_quarter <- NULL
#Quarterly spillovers for South Africa


BIDIRECTIONAL.EU.TO.AFRICA.QUARTERLY.3.12 <- rbind(Pairwise.Quarterly.Connectedness.EU.Kenya, Pairwise.Quarterly.Connectedness.EU.Namibia, Pairwise.Quarterly.Connectedness.EU.Nigeria, Pairwise.Quarterly.Connectedness.EU.SA)

#REDO: I ACKNOWLEDGE THAT NET-PAIRWISE SPILLOVERS ARE NOT EXACTLY WHAT I WANT. I RATHER WANT DYNAMIC SPILLOVERS OF EACH DAY INDICATING 
#WHICH AMOUNT OF SPILLOVERS OCCURED FROM EUROPE TO THE SPECIFIC COUNTRY. HOW MUCH SPILLOVERS OCCURED FROM THE AFRICAN COUNTRY TO THE EUROPEAN FINANCIAL MARKET IS NOT OF INTEREST (PORTRAYED IN NET-SPILLOVERS):


melted.CT.EU <- reshape2::melt(connectedness.3.12$CT[1:4,5,,drop=FALSE])
names(melted.CT.EU)[4] <- "Spillover"
names(melted.CT.EU)[3] <- "@TIME_PERIOD"
melted.CT.EU$Var2 <- NULL
names(melted.CT.EU)[1] <- "@REF_AREA"


melted.CT.EU$VERSUCH = ifelse(melted.CT.EU$`@REF_AREA` %in% "v.Kenya" ,"KE",
                              ifelse(melted.CT.EU$`@REF_AREA` %in% "v.Namibia", "NA",
                                     ifelse(melted.CT.EU$`@REF_AREA` %in% "v.SA", "ZA",
                                            ifelse(melted.CT.EU$`@REF_AREA` %in% "v.Nigeria", "NG",0))))


melted.CT.EU$`@REF_AREA` <- NULL
names(melted.CT.EU)[3] <- "@REF_AREA"
melted.CT.EU$`@TIME_PERIOD` <- as.Date(melted.CT.EU$`@TIME_PERIOD`)

Pairwise.Quarterly.Spillovers <- melted.CT.EU %>% mutate(year_quarter = as.yearqtr(`@TIME_PERIOD`, with_year = TRUE)) %>%
  group_by(year_quarter, `@REF_AREA`) %>%
  summarize(Spillover = mean(Spillover))

Pairwise.Quarterly.Spillovers$Quarter <- format(Pairwise.Quarterly.Spillovers$year_quarter, format = "%Y-Q%q")
names(Pairwise.Quarterly.Spillovers)[4] <- "@TIME_PERIOD"
Pairwise.Quarterly.Spillovers$year_quarter <- NULL
BIDIRECTIONAL.EU.TO.AFRICA.QUARTERLY.3.12 <- Pairwise.Quarterly.Spillovers



# 2.4 Preliminary Test: CHECK FOR STATIONARITY --------------------------------------------------


acf.nigeria <- acf(daily.vola.NA$v.Nigeria,lag.max = length(daily.vola.NA$v.Nigeria),
                   xlab = "lag #", ylab = 'ACF',main='Nigeria ', plot = FALSE)

acf.namibia <- acf(daily.vola.NA$v.Namibia,lag.max = length(daily.vola.NA$v.Namibia),
                   xlab = "lag #", ylab = 'ACF',main='Namibia ', plot = FALSE)
acf.namibia <- plot(acf.namibia)

acf.kenya <- acf(daily.vola.NA$v.Kenya,lag.max = length(daily.vola.NA$v.Kenya),
                 xlab = "lag #", ylab = 'ACF',main='Kenya', plot = FALSE)
acf.kenya <- plot(acf.kenya)

acf.sa <- acf(daily.vola.NA$v.SA,lag.max = length(daily.vola.NA$v.SA),
              xlab = "lag #", ylab = 'ACF',main='South Africa', plot = FALSE)
acf.sa <- plot(acf.sa)

acf.eu <- acf(daily.vola.NA$v.EURO,lag.max = length(daily.vola.NA$v.EURO),
              xlab = "lag #", ylab = 'ACF',main='Europe', plot = FALSE)
acf.eu <- plot(acf.eu)

#, width=4.2,height=12

'tikz(file = "YYY/autocorrelation.tex", standAlone=T)
acf.nigeria + acf.namibia + acf.kenya + acf.sa + acf.eu + plot_layout(ncol = 2)
dev.off()'



# 2.5 Pairwise measures -------------------------------------------------------

#Pairwise combinations with Nigeria:

melted_NPDC.Nigeria <- reshape2::melt(connectedness.3.12$NPDC[1,,,drop=FALSE])

NPDC.nigeria.namibia <- melted_NPDC.Nigeria[melted_NPDC.Nigeria$Var2 == "v.Namibia", ]
NPDC.nigeria.namibia$Var3 <- as.Date(NPDC.nigeria.namibia$Var3)

NPDC.nigeria.kenya <- melted_NPDC.Nigeria[melted_NPDC.Nigeria$Var2 == "v.Kenya", ]
NPDC.nigeria.kenya$Var3 <- as.Date(NPDC.nigeria.kenya$Var3)

NPDC.nigeria.sa <- melted_NPDC.Nigeria[melted_NPDC.Nigeria$Var2 == "v.SA", ]
NPDC.nigeria.sa$Var3 <- as.Date(NPDC.nigeria.sa$Var3)

NPDC.nigeria.eu <- melted_NPDC.Nigeria[melted_NPDC.Nigeria$Var2 == "v.EURO", ]
NPDC.nigeria.eu$Var3 <- as.Date(NPDC.nigeria.eu$Var3)



net.pairwise.nigeria.namibia = ggplot(NPDC.nigeria.namibia, aes(x = Var3, y = value)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-30,30), breaks = seq(-30, 30, 10)) +
  labs(title = "(a) Nigeria - Namibia",
       x = "",
       y = "") +
  theme(plot.margin=unit(c(0,0,-0.25,1), "cm"))

net.pairwise.nigeria.kenya = ggplot(NPDC.nigeria.kenya, aes(x = Var3, y = value)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-30,30), breaks = seq(-30, 30, 10)) +
  labs(title = "(b) Nigeria - Kenya",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(0,1,-0.25,0), "cm"))

net.pairwise.nigeria.sa = ggplot(NPDC.nigeria.sa, aes(x = Var3, y = value)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-30,30), breaks = seq(-30, 30, 10)) +
  labs(title = "(c) Nigeria - South Africa",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,0,-0.25,1), "cm"))

net.pairwise.nigeria.eu = ggplot(NPDC.nigeria.eu, aes(x = Var3, y = value)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-35,33.2), breaks = seq(-30, 30, 10)) +
  labs(title = "(d) Nigeria - Europe",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,1,-0.25,0), "cm"))


#Pairwise connections with Namibia

melted_NPDC.Namibia <- reshape2::melt(connectedness.3.12$NPDC[2,,,drop=FALSE])

NPDC.namibia.kenya <- melted_NPDC.Namibia[melted_NPDC.Namibia$Var2 == "v.Kenya", ]
NPDC.namibia.kenya$Var3 <- as.Date(NPDC.namibia.kenya$Var3)

NPDC.namibia.sa <- melted_NPDC.Namibia[melted_NPDC.Namibia$Var2 == "v.SA", ]
NPDC.namibia.sa$Var3 <- as.Date(NPDC.namibia.sa$Var3)

NPDC.namibia.eu <- melted_NPDC.Namibia[melted_NPDC.Namibia$Var2 == "v.EURO", ]
NPDC.namibia.eu$Var3 <- as.Date(NPDC.namibia.eu$Var3)


net.pairwise.namibia.kenya = ggplot(NPDC.namibia.kenya, aes(x = Var3, y = value)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-30,30), breaks = seq(-30, 30, 10)) +
  labs(title = "(e) Namibia - Kenya",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,0,-0.25,1), "cm"))

net.pairwise.namibia.sa = ggplot(NPDC.namibia.sa, aes(x = Var3, y = value)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-30,30), breaks = seq(-30, 30, 10)) +
  labs(title = "(f) Namibia - South Africa",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,1,-0.25,0), "cm"))

net.pairwise.namibia.eu = ggplot(NPDC.namibia.eu, aes(x = Var3, y = value)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-31,32), breaks = seq(-30, 30, 10)) +
  labs(title = "(g) Namibia - Europe",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,0,-0.25,1), "cm"))

#Pairwise connections with Kenya

melted_NPDC.Kenya <- reshape2::melt(connectedness.3.12$NPDC[3,,,drop=FALSE])

NPDC.kenya.sa <- melted_NPDC.Kenya[melted_NPDC.Kenya$Var2 == "v.SA", ]
NPDC.kenya.sa$Var3 <- as.Date(NPDC.kenya.sa$Var3)

NPDC.kenya.eu <- melted_NPDC.Kenya[melted_NPDC.Kenya$Var2 == "v.EURO", ]
NPDC.kenya.eu$Var3 <- as.Date(NPDC.namibia.sa$Var3)


net.pairwise.kenya.sa = ggplot(NPDC.kenya.sa, aes(x = Var3, y = value)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-30,30), breaks = seq(-30, 30, 10)) +
  labs(title = "(h) Kenya - South Africa",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,1,-0.25,0), "cm"))

net.pairwise.kenya.eu= ggplot(NPDC.kenya.eu, aes(x = Var3, y = value)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-30,30), breaks = seq(-30, 30, 10)) +
  labs(title = "(i) Kenya - Europe",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,0,-0.25,1), "cm"))

net.pairwise.kenya.eu
# Pairwise connection with South Africa 

melted_NPDC.SA <- reshape2::melt(connectedness.3.12$NPDC[4,,,drop=FALSE])

NPDC.sa.eu <- melted_NPDC.SA[melted_NPDC.SA$Var2 == "v.EURO", ]
NPDC.sa.eu$Var3 <- as.Date(NPDC.sa.eu$Var3)


net.pairwise.sa.eu = ggplot(NPDC.sa.eu, aes(x = Var3, y = value)) +
  geom_area() +
  add_rec_shade(min(fred.data$date),max(fred.data$date))+
  scale_y_continuous(limits = c(-30,30), breaks = seq(-30, 30, 10)) +
  labs(title = "(j) South Africa - Europe",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,1,-0.25, 0), "cm"))



library(grid)
library(ggpubr)
install.packages("gridExtra")
library(gridExtra)

'tikz(file = "YYY/net.pairwise.grid.tex", standAlone=T)
grid.arrange(net.pairwise.nigeria.namibia,net.pairwise.nigeria.kenya,net.pairwise.nigeria.sa,net.pairwise.nigeria.eu, net.pairwise.namibia.kenya, net.pairwise.namibia.sa, net.pairwise.namibia.eu, net.pairwise.kenya.sa, net.pairwise.kenya.eu,
             net.pairwise.sa.eu, ncol = 2, nrow = 5)
dev.off()
'



'tikz(file = "YYY/networkplot.npdc.tex", standAlone=T)
PlotNetwork(connectedness.3.12, method = "NPDC")
dev.off()
'





# Figure 6 ----------------------------------------------------------------

'tikz(file = "YYY/net.pairwise.tex", standAlone=T)
g <- net.pairwise.nigeria.namibia+net.pairwise.nigeria.kenya+net.pairwise.nigeria.sa+net.pairwise.nigeria.eu+ net.pairwise.namibia.kenya+net.pairwise.namibia.sa+net.pairwise.namibia.eu+net.pairwise.kenya.sa+net.pairwise.kenya.eu+
  net.pairwise.sa.eu+ plot_layout(ncol = 2) + theme(panel.spacing = unit(c(-0.5,0-0.5,0), "lines")) + theme_bw() +
  theme(plot.margin = unit(c(0,0,0,0), "lines")) +
  theme(strip.background = element_blank()) +
  theme(plot.background = element_blank()) +
  theme(strip.text = element_blank()) '



# 3. Robustness-Checks -------------------------------------------------------



# 3.1 Sensitivity of the index to the forecast horizon (Figure 7)--------

#I know compute the spillover estimates in order to get max-min and median values to create Figure 7.
#As I am only interested in the Total spillover measure, I will extract TCI from the list of estimates. This is repeated for several 
#values of nfore = H = forecast horizons..

#Spillover Index with H=5,6,7,8,9,10,11,12,13,14,15
connectedness.5 = ConnectednessApproach(daily.vola.NA.zoo,
                                        nlag = 3,
                                        nfore = 5,
                                        window.size = 200,
                                        corrected = FALSE,
                                        model = c("VAR"),
                                        connectedness = c("Time"),
                                        Connectedness_config = list(TimeConnectedness = list(generalized = TRUE),
                                                                    FrequencyConnectedness = list(partition = c(pi, pi/2, 0), generalized = TRUE,
                                                                                                  scenario = "ABS"))
)
connectedness.5.total.index <- connectedness.5$TCI
connectedness.5.total.index <- as.data.frame(connectedness.5.total.index)


connectedness.6 = ConnectednessApproach(daily.vola.NA.zoo,
                                        nlag = 3,
                                        nfore = 6,
                                        window.size = 200,
                                        corrected = FALSE,
                                        model = c("VAR"),
                                        connectedness = c("Time"),
                                        Connectedness_config = list(TimeConnectedness = list(generalized = TRUE),
                                                                    FrequencyConnectedness = list(partition = c(pi, pi/2, 0), generalized = TRUE,
                                                                                                  scenario = "ABS"))
)
connectedness.6.total.index <- connectedness.6$TCI
connectedness.6.total.index <- as.data.frame(connectedness.6.total.index)


connectedness.7 = ConnectednessApproach(daily.vola.NA.zoo,
                                        nlag = 3,
                                        nfore = 7,
                                        window.size = 200,
                                        corrected = FALSE,
                                        model = c("VAR"),
                                        connectedness = c("Time"),
                                        Connectedness_config = list(TimeConnectedness = list(generalized = TRUE),
                                                                    FrequencyConnectedness = list(partition = c(pi, pi/2, 0), generalized = TRUE,
                                                                                                  scenario = "ABS"))
)
connectedness.7.total.index <- connectedness.7$TCI
connectedness.7.total.index <- as.data.frame(connectedness.7.total.index)


connectedness.8 = ConnectednessApproach(daily.vola.NA.zoo,
                                        nlag = 3,
                                        nfore = 8,
                                        window.size = 200,
                                        corrected = FALSE,
                                        model = c("VAR"),
                                        connectedness = c("Time"),
                                        Connectedness_config = list(TimeConnectedness = list(generalized = TRUE),
                                                                    FrequencyConnectedness = list(partition = c(pi, pi/2, 0), generalized = TRUE,
                                                                                                  scenario = "ABS"))
)
connectedness.8.total.index <- connectedness.8$TCI
connectedness.8.total.index <- as.data.frame(connectedness.8.total.index)


connectedness.9 = ConnectednessApproach(daily.vola.NA.zoo,
                                        nlag = 3,
                                        nfore = 9,
                                        window.size = 200,
                                        corrected = FALSE,
                                        model = c("VAR"),
                                        connectedness = c("Time"),
                                        Connectedness_config = list(TimeConnectedness = list(generalized = TRUE),
                                                                    FrequencyConnectedness = list(partition = c(pi, pi/2, 0), generalized = TRUE,
                                                                                                  scenario = "ABS"))
)
connectedness.9.total.index <- connectedness.9$TCI
connectedness.9.total.index <- as.data.frame(connectedness.9.total.index)


connectedness.10 = ConnectednessApproach(daily.vola.NA.zoo,
                                         nlag = 3,
                                         nfore = 10,
                                         window.size = 200,
                                         corrected = FALSE,
                                         model = c("VAR"),
                                         connectedness = c("Time"),
                                         Connectedness_config = list(TimeConnectedness = list(generalized = TRUE),
                                                                     FrequencyConnectedness = list(partition = c(pi, pi/2, 0), generalized = TRUE,
                                                                                                   scenario = "ABS"))
)

connectedness.10.total.index <- connectedness.10$TCI
connectedness.10.total.index <- as.data.frame(connectedness.10.total.index)

#SPillover Index with p=2
connectedness.11 = ConnectednessApproach(daily.vola.NA.zoo,
                                         nlag = 3,
                                         nfore = 11,
                                         window.size = 200,
                                         corrected = FALSE,
                                         model = c("VAR"),
                                         connectedness = c("Time"),
                                         Connectedness_config = list(TimeConnectedness = list(generalized = TRUE),
                                                                     FrequencyConnectedness = list(partition = c(pi, pi/2, 0), generalized = TRUE,
                                                                                                   scenario = "ABS"))
)
connectedness.11.total.index <- connectedness.11$TCI
connectedness.11.total.index <- as.data.frame(connectedness.11.total.index)


#Spillover Index with p=3
connectedness.12 = ConnectednessApproach(daily.vola.NA.zoo,
                                         nlag = 3,
                                         nfore = 12,
                                         window.size = 200,
                                         corrected = FALSE,
                                         model = c("VAR"),
                                         connectedness = c("Time"),
                                         Connectedness_config = list(TimeConnectedness = list(generalized = TRUE),
                                                                     FrequencyConnectedness = list(partition = c(pi, pi/2, 0), generalized = TRUE,
                                                                                                   scenario = "ABS"))
)

connectedness.12.total.index <- connectedness.12$TCI
connectedness.12.total.index <- as.data.frame(connectedness.12.total.index)


#Spillover Index with p=4
connectedness.13 = ConnectednessApproach(daily.vola.NA.zoo,
                                         nlag = 3,
                                         nfore = 13,
                                         window.size = 200,
                                         corrected = FALSE,
                                         model = c("VAR"),
                                         connectedness = c("Time"),
                                         Connectedness_config = list(TimeConnectedness = list(generalized = TRUE),
                                                                     FrequencyConnectedness = list(partition = c(pi, pi/2, 0), generalized = TRUE,
                                                                                                   scenario = "ABS"))
)
connectedness.13.total.index <- connectedness.13$TCI
connectedness.13.total.index <- as.data.frame(connectedness.13.total.index)



#Spillover Index with p=5
connectedness.14 = ConnectednessApproach(daily.vola.NA.zoo,
                                         nlag = 3,
                                         nfore = 14,
                                         window.size = 200,
                                         corrected = FALSE,
                                         model = c("VAR"),
                                         connectedness = c("Time"),
                                         Connectedness_config = list(TimeConnectedness = list(generalized = TRUE),
                                                                     FrequencyConnectedness = list(partition = c(pi, pi/2, 0), generalized = TRUE,
                                                                                                   scenario = "ABS"))
)
connectedness.14.total.index <- connectedness.14$TCI
connectedness.14.total.index <- as.data.frame(connectedness.14.total.index)

connectedness.15 = ConnectednessApproach(daily.vola.NA.zoo,
                                         nlag = 3,
                                         nfore = 15,
                                         window.size = 200,
                                         corrected = FALSE,
                                         model = c("VAR"),
                                         connectedness = c("Time"),
                                         Connectedness_config = list(TimeConnectedness = list(generalized = TRUE),
                                                                     FrequencyConnectedness = list(partition = c(pi, pi/2, 0), generalized = TRUE,
                                                                                                   scenario = "ABS"))
)
connectedness.15.total.index <- connectedness.15$TCI
connectedness.15.total.index <- as.data.frame(connectedness.15.total.index)


#Add all estimates into one data frame

Sensitivity.H <- connectedness.5.total.index

Sensitivity.H$TCI.2 <- connectedness.6.total.index$TCI
Sensitivity.H$TCI.3 <- connectedness.7.total.index$TCI
Sensitivity.H$TCI.4 <- connectedness.8.total.index$TCI
Sensitivity.H$TCI.5 <- connectedness.9.total.index$TCI
Sensitivity.H$TCI.6 <- connectedness.10.total.index$TCI
Sensitivity.H$TCI.7 <- connectedness.11.total.index$TCI
Sensitivity.H$TCI.8 <- connectedness.12.total.index$TCI
Sensitivity.H$TCI.9 <- connectedness.13.total.index$TCI
Sensitivity.H$TCI.10 <- connectedness.14.total.index$TCI
Sensitivity.H$TCI.11 <- connectedness.15.total.index$TCI

max.min.mean.function.H <- transform(Sensitivity.H, Min = pmin(TCI,TCI.2,TCI.3,TCI.4,TCI.5,TCI.6,TCI.7,TCI.8,TCI.9,TCI.10,TCI.11), Max = pmax(TCI,TCI.2,TCI.3,TCI.4,TCI.5,TCI.6,TCI.7,TCI.8,TCI.9,TCI.10,TCI.11), Median = pmedian(TCI,TCI.2,TCI.3,TCI.4,TCI.5,TCI.6,TCI.7,TCI.8,TCI.9,TCI.10,TCI.11), indx = seq_len(dim(Sensitivity.H)[1]))
max.min.mean.function.H$Date <- row.names(max.min.mean.function.H)
max.min.mean.function.H$Date <- as.Date(max.min.mean.function.H$Date)

ggplot(max.min.mean.function.H) +
  geom_line(aes(Date, Median), group=1) +
  geom_ribbon(aes(x = Date, ymax = Max, ymin = Min), alpha = 0.6, fill = "darkgray")



# 3.2 Sensitivity to forecast horizon H (Figure 8) -----------------------------

sensitivity.H.2 = ggplot(max.min.mean.function.H, aes(x = Date, y = Median)) +
  geom_ribbon(aes(ymin=Min, ymax=Max, x=Date, fill = "(Max,Min)"), alpha = 0.6)+
  geom_line(aes(y=Median, x=Date, colour = "Median")) +
  scale_y_continuous(limits = c(13,65), breaks = seq(20, 60, 10)) +
  theme_bw()+
  labs(title = "",
       x = "",
       y = "") +
  scale_colour_manual("",values="black")+
  scale_fill_manual("",values="grey12")+
  theme(legend.position = "bottom")


'tikz(file = "YYY/sensitivity.H.2.tex", standAlone=T)
sensitivity.H.2
dev.off()'


# 4. Comprehensive Robustness Check ------------------------------------------

#For all three models, VAR, QVAR and TVP-GARCH I know compute the total spillover index wit H=2,5,12 and VAR lags=1,3,5:



# 4.1 VAR ---------------------------------------------------------------------


#2,1  5,1  12,1
#H=2, LAG=1
var.2.1 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 1,
  nfore = 2,
  window.size = 200,
  corrected = FALSE,
  model = c("VAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)))

index.var.2.1 <- mean(var.2.1$TCI)

#H=5, LAG=1
var.5.1 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 1,
  nfore = 5,
  window.size = 200,
  corrected = FALSE,
  model = c("VAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.var.5.1 <- mean(var.5.1$TCI)


#H=12, LAG=1
var.12.1 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 1,
  nfore = 12,
  window.size = 200,
  corrected = FALSE,
  model = c("VAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.var.12.1 <- mean(var.12.1$TCI)

#2,3  5,3  12,3
var.2.3 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 3,
  nfore = 2,
  window.size = 200,
  corrected = FALSE,
  model = c("VAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.var.2.3 <- mean(var.2.3$TCI)


var.5.3 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 3,
  nfore = 5,
  window.size = 200,
  corrected = FALSE,
  model = c("VAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.var.5.3 <- mean(var.5.3$TCI)


var.12.3 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 3,
  nfore = 12,
  window.size = 200,
  corrected = FALSE,
  model = c("VAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.var.12.3 <- mean(var.12.3$TCI)


#    2,5  5,5  12,5
var.2.5 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 5,
  nfore = 2,
  window.size = 200,
  corrected = FALSE,
  model = c("VAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.var.2.5 <- mean(var.2.3$TCI)


var.5.5 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 5,
  nfore = 5,
  window.size = 200,
  corrected = FALSE,
  model = c("VAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.var.5.5 <- mean(var.5.3$TCI)

var.12.5 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 5,
  nfore = 12,
  window.size = 200,
  corrected = FALSE,
  model = c("VAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.var.12.5 <- mean(var.12.3$TCI)


# 4.2 QVAR --------------------------------------------------------------------

#2,1  5,1  12,1
#H=2, LAG=1
qvar.2.1 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 1,
  nfore = 2,
  window.size = 200,
  corrected = FALSE,
  model = c("QVAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.qvar.2.1 <- mean(qvar.2.1$TCI)

#H=5, LAG=1
qvar.5.1 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 1,
  nfore = 5,
  window.size = 200,
  corrected = FALSE,
  model = c("QVAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.qvar.5.1 <- mean(qvar.5.1$TCI)

#H=12, LAG=1
qvar.12.1 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 1,
  nfore = 12,
  window.size = 200,
  corrected = FALSE,
  model = c("QVAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.qvar.12.1 <- mean(qvar.12.1$TCI)

#2,3  5,3  12,3
qvar.2.3 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 3,
  nfore = 2,
  window.size = 200,
  corrected = FALSE,
  model = c("QVAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.qvar.2.3 <- mean(qvar.2.3$TCI)

qvar.5.3 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 3,
  nfore = 5,
  window.size = 200,
  corrected = FALSE,
  model = c("QVAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.qvar.5.3 <- mean(qvar.5.3$TCI)

qvar.12.3 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 3,
  nfore = 12,
  window.size = 200,
  corrected = FALSE,
  model = c("QVAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.qvar.12.3 <- mean(qvar.12.3$TCI)

#    2,5  5,5  12,5
qvar.2.5 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 5,
  nfore = 2,
  window.size = 200,
  corrected = FALSE,
  model = c("QVAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.qvar.2.5 <- mean(qvar.2.5$TCI)


qvar.5.5 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 5,
  nfore = 5,
  window.size = 200,
  corrected = FALSE,
  model = c("QVAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.qvar.5.5 <- mean(qvar.5.5$TCI)


qvar.12.5 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 5,
  nfore = 12,
  window.size = 200,
  corrected = FALSE,
  model = c("QVAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.qvar.12.5 <- mean(qvar.12.5$TCI)




# 4.3 TVP-VAR -----------------------------------------------------------------

##TVP-VAR##

#2,1  5,1  12,1
#H=2, LAG=1
tvp.var.2.1 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 1,
  nfore = 2,
  window.size = 200,
  corrected = FALSE,
  model = c("TVP-VAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.tvp.var.2.1 <- mean(tvp.var.2.1$TCI)

#H=5, LAG=1
tvp.var.5.1 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 1,
  nfore = 5,
  window.size = 200,
  corrected = FALSE,
  model = c("TVP-VAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.tvp.var.5.1 <- mean(tvp.var.5.1$TCI)

#H=12, LAG=1
tvp.var.12.1 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 1,
  nfore = 12,
  window.size = 200,
  corrected = FALSE,
  model = c("TVP-VAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.tvp.var.12.1 <- mean(tvp.var.12.1$TCI)

#2,3  5,3  12,3
tvp.var.2.3 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 3,
  nfore = 2,
  window.size = 200,
  corrected = FALSE,
  model = c("TVP-VAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.tvp.var.2.3 <- mean(tvp.var.2.3$TCI)


tvp.var.5.3 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 3,
  nfore = 5,
  window.size = 200,
  corrected = FALSE,
  model = c("TVP-VAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.tvp.var.5.3 <- mean(tvp.var.5.3$TCI)


tvp.var.12.3 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 3,
  nfore = 12,
  window.size = 200,
  corrected = FALSE,
  model = c("TVP-VAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.tvp.var.12.3 <- mean(tvp.var.12.3$TCI)


#    2,5  5,5  12,5
tvp.var.2.5 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 5,
  nfore = 2,
  window.size = 200,
  corrected = FALSE,
  model = c("TVP-VAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.tvp.var.2.5 <- mean(tvp.var.2.5$TCI)


tvp.var.5.5 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 5,
  nfore = 5,
  window.size = 200,
  corrected = FALSE,
  model = c("TVP-VAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.tvp.var.5.5 <- mean(tvp.var.5.5$TCI)


tvp.var.12.5 <- ConnectednessApproach(
  daily.vola.NA.zoo,
  nlag = 5,
  nfore = 12,
  window.size = 200,
  corrected = FALSE,
  model = c("TVP-VAR"),
  connectedness = c("Time"),
  VAR_config = list(QVAR = list(tau = 0.5), TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99,prior = "BayesPrior", gamma = 0.01)),
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

index.tvp.var.12.5 <- mean(tvp.var.12.5$TCI)


#For all three models, VAR, QVAR and TVP-GARCH I know compute the total spillover index wit H=2,5,12 and VAR lags=1,3,5:

model.robustness <- data.frame(VAR=rep(c(index.var.2.1,  
                                         index.var.2.3,  
                                         index.var.2.5,
                                         index.var.5.1,
                                         index.var.5.3,
                                         index.var.5.5,
                                         index.var.12.1,
                                         index.var.12.3,
                                         index.var.12.5,
                                         index.qvar.2.1,  
                                         index.qvar.2.3, 
                                         index.qvar.2.5,
                                         index.qvar.5.1,
                                         index.qvar.5.3,
                                         index.qvar.5.5,
                                         index.qvar.12.1,
                                         index.qvar.12.3,
                                         index.qvar.12.5,
                                         index.tvp.var.2.1,  
                                         index.tvp.var.2.3,  
                                         index.tvp.var.2.5,
                                         index.tvp.var.5.1,
                                         index.tvp.var.5.3,
                                         index.tvp.var.5.5,
                                         index.tvp.var.12.1,
                                         index.tvp.var.12.3,
                                         index.tvp.var.12.5)))

stargazer(model.robustness, summary=FALSE, digits=1)



# Teil 2: Panel Regression ------------------------------------------------


# API for IMF-Data --------------------------------------------------------


install.packages("magrittr")
install.packages("stringr")
install.packages("janitor")


install_local("/Users/juliusgross/Desktop/Bachelorarbeit Julius Gross/R/imfr_0.1.9.tar")
#Please download the package imfr as indicated in their manual

library(janitor)
library(stringr)
library(magrittr)
library(dplyr)
library(tidyr)
library(quantmod)
library(devtools)
library(imfr)

imf_databases<-imf_ids() 


# 1. EX & IM Data from DOT-database ---------------------------------------

# 1.1 Trade with world ----------------------------------------------------

# 1.1.1 Nigeria, South Africa, Namibia, Europe ----------------------------


DOT_codelist<-imf_codelist(database_id = "DOT")

Africa_EU_ISO2<- c("NG", "ZA", "NA", "B0")

DOT.Africa.EU <- imf_data(database_id = "DOT" , indicator = c("TBG_USD") , country = Africa_EU_ISO2 , start = 2005, end = current_year(), return_raw = TRUE)
DOT.Africa.EU.final <- DOT.Africa.EU$CompactData$DataSet$Series


DOT.Value.of.Trade <- DOT.Africa.EU.final %>% 
  filter(`@FREQ` == "Q" & `@COUNTERPART_AREA` %in% "W00") %>% unnest(Obs) %>% 
  arrange(`@REF_AREA`, `@COUNTERPART_AREA`)

#This gives us the first data set for all relevant variables except Kenya.


# 1.1.2 Kenya -------------------------------------------------------------

Kenya <- c("KE")

DOT.Kenya <- imf_data(database_id = "DOT" , indicator = c("TBG_USD") , country = Kenya , start = 2005, end = current_year(), return_raw = TRUE)
DOT.Kenya.final <- DOT.Kenya$CompactData$DataSet$Series

DOT.Value.of.Trade.Kenya <- DOT.Kenya.final %>% 
  filter(`@FREQ` == "Q" & `@COUNTERPART_AREA` %in% "W00") %>% unnest(Obs) %>% 
  arrange(`@REF_AREA`, `@COUNTERPART_AREA`)



# 1.1.3 NI,ZA,NA,B0,KE  --------------------------------------------------

DOT.Value.of.Trade.final <- rbind(DOT.Value.of.Trade, DOT.Value.of.Trade.Kenya)

#Value of Trade is in US Dollars!

# 1.2 Trade within African countries (Reg2) --------------------------------

Africa_ISO2<- c("NG", "ZA", "NA", "KE")

DOT.Africa <- imf_data(database_id = "DOT" , indicator = c("TBG_USD") , country = Africa_ISO2 , start = 2005, end = current_year(), return_raw = TRUE)
DOT.Africa.final <- DOT.Africa$CompactData$DataSet$Series

DOT.Value.of.Trade.Africa <- DOT.Africa.final %>% 
  filter(`@FREQ` == "Q" & `@COUNTERPART_AREA` %in% Africa_ISO2) %>% unnest(Obs) %>% 
  arrange(`@REF_AREA`, `@COUNTERPART_AREA`)

rm(DOT.Africa, DOT.Africa.final)


# 1.3 Creating IMPEX (EX+IM)/GDP ------------------------------------------


# 1.3.1 Gathering Import and Export data ----------------------------------

DOT_codelist<-imf_codelist(database_id = "DOT")

Africa_EU_ISO2<- c("NG", "ZA", "NA", "B0")

#IMPORT
IMPORT.Africa.EU <- imf_data(database_id = "DOT" , indicator = c("TMG_CIF_USD") , country = Africa_EU_ISO2 , start = 2005, end = current_year(), return_raw = TRUE)
IMPORT.Africa.EU.final <- IMPORT.Africa.EU$CompactData$DataSet$Series


IMPORT <- IMPORT.Africa.EU.final %>% 
  filter(`@FREQ` == "Q" & `@COUNTERPART_AREA` %in% "W00") %>% unnest(Obs) %>% 
  arrange(`@REF_AREA`, `@COUNTERPART_AREA`)


#EXPORT
EXPORT.Africa.EU <- imf_data(database_id = "DOT" , indicator = c("TXG_FOB_USD") , country = Africa_EU_ISO2 , start = 2005, end = current_year(), return_raw = TRUE)
EXPORT.Africa.EU.final <- EXPORT.Africa.EU$CompactData$DataSet$Series


EXPORT <- EXPORT.Africa.EU.final %>% 
  filter(`@FREQ` == "Q" & `@COUNTERPART_AREA` %in% "W00") %>% unnest(Obs) %>% 
  arrange(`@REF_AREA`, `@COUNTERPART_AREA`)

names(IMPORT)[8] <- "IMPORT"
names(EXPORT)[8] <- "EXPORT"

VALUE.OF.TRADE <- merge(EXPORT,IMPORT, by=c("@REF_AREA","@TIME_PERIOD"))
VALUE.OF.TRADE$EXPORT <- as.numeric(VALUE.OF.TRADE$EXPORT)
VALUE.OF.TRADE$IMPORT <- as.numeric(VALUE.OF.TRADE$IMPORT)

VALUE.OF.TRADE$Value.of.trade <- VALUE.OF.TRADE$EXPORT + VALUE.OF.TRADE$IMPORT

#ACHTUNG: Die Daten zu den unterschiedlichen Ländern haben immer noch unterschiedliche Währungen

Kenya <- "KE"

#IMPORT KE
IMPORT.KE <- imf_data(database_id = "DOT" , indicator = c("TMG_CIF_USD") , country = Kenya , start = 2005, end = current_year(), return_raw = TRUE)
IMPORT.KE.final <- IMPORT.KE$CompactData$DataSet$Series


IMPORT.Kenya <- IMPORT.KE.final %>% 
  filter(`@FREQ` == "Q" & `@COUNTERPART_AREA` %in% "W00") %>% unnest(Obs) %>% 
  arrange(`@REF_AREA`, `@COUNTERPART_AREA`)


#EXPORT KE
EXPORT.KE <- imf_data(database_id = "DOT" , indicator = c("TXG_FOB_USD") , country = Kenya , start = 2005, end = current_year(), return_raw = TRUE)
EXPORT.KE.final <- EXPORT.KE$CompactData$DataSet$Series


EXPORT.Kenya <- EXPORT.KE.final %>% 
  filter(`@FREQ` == "Q" & `@COUNTERPART_AREA` %in% "W00") %>% unnest(Obs) %>% 
  arrange(`@REF_AREA`, `@COUNTERPART_AREA`)

names(IMPORT.Kenya)[8] <- "IMPORT"
names(EXPORT.Kenya)[8] <- "EXPORT"

VALUE.OF.TRADE.KENYA <- merge(EXPORT.Kenya,IMPORT.Kenya, by=c("@REF_AREA","@TIME_PERIOD"))
VALUE.OF.TRADE.KENYA$EXPORT <- as.numeric(VALUE.OF.TRADE.KENYA$EXPORT)
VALUE.OF.TRADE.KENYA$IMPORT <- as.numeric(VALUE.OF.TRADE.KENYA$IMPORT)

VALUE.OF.TRADE.KENYA$Value.of.trade <- VALUE.OF.TRADE.KENYA$EXPORT + VALUE.OF.TRADE.KENYA$IMPORT


VALUE.OF.TRADE <- rbind(VALUE.OF.TRADE,VALUE.OF.TRADE.KENYA)

# 1.3.2 Gathering GDP Data ------------------------------------------------

#GDP-values can be found in the IFS (International Financial Statistics):

IFS_codelist<-imf_codelist(database_id = "IFS")

Target_countries <- c("NG", "ZA", "NA", "KE", "B0")

'IFS.Targeted <- imf_data(database_id = "IFS" , indicator = c("") , country = Target_countries , start = 2005, end = current_year(), return_raw = TRUE)
'
IFS.INDICATOR.codes<-imf_codes(codelist = "CL_INDICATOR_IFS")

GDP.Values <- imf_data(database_id = "IFS" , indicator = c("NGDP_NSA_XDC") , country = Target_countries , start = 2005, end = current_year(), return_raw = TRUE)
GDP.final <- GDP.Values$CompactData$DataSet$Series

GDP.final.2 <- GDP.final %>% 
  unnest(Obs)
# Für ZA somit abgeschlossen                          (fast: muss auch noch in USD umgewandelt werden)
#KE nur von 2016 - Q2 2021
# Data import from Central Bank of Kenya


Kenya_QuarterlyGDP <- read.csv("/Users/juliusgross/Desktop/Bachelorarbeit Julius Gross/R/Kenya_QuarterlyGDP.csv", header=FALSE, comment.char="#")

GDP.Kenya <- Kenya_QuarterlyGDP[-c(26:98),]
GDP.Kenya.1 <- GDP.Kenya[-c(1:2),]
GDP.Kenya.2 <- GDP.Kenya.1[-c(3:22),]

GDP.Kenya.2 <- t(GDP.Kenya.2)
GDP.Kenya.2 <- GDP.Kenya.2[-c(1),]
GDP.Kenya.2 <- GDP.Kenya.2[-c(1),]

GDP.Kenya.2 <- as_tibble(GDP.Kenya.2)
names(GDP.Kenya.2)[names(GDP.Kenya.2) == '3'] <- 'Year'
names(GDP.Kenya.2)[names(GDP.Kenya.2) == '4'] <- 'Q'
names(GDP.Kenya.2)[names(GDP.Kenya.2) == '25'] <- 'Obs'
GDP.Kenya.2[53, 1] = "2022"

Kenya.GDP <- as.data.frame(GDP.Kenya.2)

for(i in 2:nrow(Kenya.GDP)) if(Kenya.GDP$Year[i]== "") Kenya.GDP$Year[i] <- Kenya.GDP$Year[i-1]

Kenya.GDP$Quartal <- str_c(Kenya.GDP$Year,"-" ,Kenya.GDP$Q)

#Now: adapt Kenya.GDP to the GDP.final:

Q <- c("Q")

Kenya.GDP$"@FREQ" <- "Q"
Kenya.GDP$"@REF_AREA" <- "KE"
Kenya.GDP$"@INDICATOR" <- "NGDP_NSA_XDC"
Kenya.GDP$"@UNIT_MULT" <- "6"
Kenya.GDP$"@TIME_FORMAT" <- "P3M"
names(Kenya.GDP)[names(Kenya.GDP) == 'Quartal'] <- '@TIME_PERIOD'
names(Kenya.GDP)[names(Kenya.GDP) == 'Obs'] <- '@OBS_VALUE'
Kenya.GDP$Year <- NULL
Kenya.GDP$Q <- NULL

Kenya.GDP <- Kenya.GDP[, c(3, 4, 5, 6, 7, 2, 1)]

Kenya.GDP <- Kenya.GDP[-c(29:50),]

GDP.KE.SA <- rbind(Kenya.GDP, GDP.final.2) 
GDP.KE.SA.ordered <-GDP.KE.SA[order(GDP.KE.SA$`@REF_AREA`, GDP.KE.SA$`@TIME_PERIOD`),]

#Data for Nigeria: from Central Bank of Nigeria (https://www.cbn.gov.ng/rates/NominalGDP.asp)

#Nominal GDP (GDP at current market prices in billion Naira):

GDP.Nigeria <- GDP.KE.SA.ordered$`@TIME_PERIOD`
GDP.Nigeria <- as.data.frame(GDP.Nigeria)
GDP.Nigeria <- GDP.Nigeria[-c(54:122),]
GDP.Nigeria <- GDP.Nigeria[5:53]
GDP.Nigeria <- as.data.frame(GDP.Nigeria)

GDP.OBS.NI <- c("12583.48",	"12934.53",	"14304.44",	"14789.82", "14501.45",	"15054.96"	, "16163.64"	,"17260.35", "16450.36",	"17743.63",	"18521.60",	"18998.34","18295.63",	"19931.02",	"20464.40",	"21401.52" ,"20169.78",	"21734.83"	,"22933.14",	"24205.86", "21041.70",	"22859.15",	"24313.64",	"25930.47", "22235.32",	"23547.47",	"26537.65",	"29169.06", "26028.36",	"27030.25",	"29377.67",	"31275.35", "28438.60",	"30699.57",	"33368.05",	"35230.61", "31824.35",	"35001.88",	"37806.92",	"39577.34", "35647.41",	"34023.20",	"39089.46",	"43564.01", "40014.48", "39123.713", "45113.448", "49276.018", "45317.823") 
GDP.OBS.NI <- as.data.frame(GDP.OBS.NI)

GDP.Nigeria$Obs <- GDP.OBS.NI$GDP.OBS.NI

#Now: adapt GDP.Nigeria to the GDP.final:

GDP.Nigeria$"@FREQ" <- "Q"
GDP.Nigeria$"@REF_AREA" <- "NI"
GDP.Nigeria$"@INDICATOR" <- "NGDP_NSA_XDC"
GDP.Nigeria$"@UNIT_MULT" <- "6"
GDP.Nigeria$"@TIME_FORMAT" <- "P3M"
names(GDP.Nigeria)[names(GDP.Nigeria) == 'GDP.Nigeria'] <- '@TIME_PERIOD'
names(GDP.Nigeria)[names(GDP.Nigeria) == 'Obs'] <- '@OBS_VALUE'
GDP.Nigeria <- as.data.frame(GDP.Nigeria)


GDP.KE.SA.NI <- rbind(GDP.Nigeria, GDP.KE.SA.ordered) 

#Last one that is missing: Namibia

#Data for Namibia (Bank of Namibia: https://services.bon.com.na/iersv1/ExternalReport.aspx?ReportPath=/IERS/sReports_RealQuartelyGDP_million):

# This data is in US Dollar!
GDP_Namibia <- read_excel("GDP Namibia.xlsx")
GDP.Namibia <- GDP_Namibia[-c(1:15),]
GDP.Namibia <- subset(GDP.Namibia, select = -c(`...3`))
GDP.Namibia <- subset(GDP.Namibia, select = -c(`...4`))

names(GDP.Namibia)[names(GDP.Namibia) == "...2"] <- '@OBS_VALUE'
names(GDP.Namibia)[names(GDP.Namibia) == "Macrotrends Data Download"] <- '@TIME_PERIOD'
GDP.Namibia <- GDP.Namibia[-c(1),]
# This data already is in Billions of US $ 

GDP.Namibia$"@FREQ" <- "Q"
GDP.Namibia$"@REF_AREA" <- "NA"
GDP.Namibia$"@INDICATOR" <- "NGDP_NSA_XDC"
GDP.Namibia$"@UNIT_MULT" <- "6"
GDP.Namibia$"@TIME_FORMAT" <- "P3M"

GDP.Namibia <- GDP.Namibia[, c(3, 4, 5, 6, 7, 1, 2)]

Namibia.Time <- strsplit(GDP.Namibia$`@TIME_PERIOD`, split = "-")
Namibia.Time <- as.data.frame(Namibia.Time)

Namibia.Time <- Namibia.Time[c(2,1),]

Namibia.Time$ind <- seq.int(nrow(Namibia.Time))
row.names(Namibia.Time) <- Namibia.Time$ind

Time <- apply(Namibia.Time, -1, paste, collapse="-")
Time <- as.data.frame(Time)
Time <- Time[c(1:49),]

GDP.Namibia$`@TIME_PERIOD` <- Time

GDP.KE.SA.NI.NA <-  rbind(GDP.Namibia, GDP.KE.SA.NI)


# 1.3.2 Creating exchange rate vectors, transforming into Million US Dollars ------------------------------------


#As I want to investigate the spillovers from Europe to several african countries and not vice versa, 
#I don`t nedd the IMPEX for Europe. 
#Thus, for the first indicator (IMPEX) I now only need the information on the exchange rate to receive a
#comparable and valuable IMPEX:
#I will now try to compute an exchange rate vector for each country (NI, SA, KE) (quarterly average):

StartTime <- "2010-01-01"
EndTime <- "2022-03-01"

getSymbols("KESUSD=X", src = "yahoo", from=StartTime, to=EndTime, periodicity = "monthly") 
Exchange.Kenya <- as.data.frame(`KESUSD=X`)

Exchange.Kenya <- Exchange.Kenya %>% mutate(RollAvge= rollapplyr(Exchange.Kenya$`KESUSD=X.Open`, list(-(0:2)), mean, partial = TRUE, fill = NA))

#Creating a sequence so that we get filter out the quarterly (averaged) exchange rates:

Bolean.Quartal <- rep(c(0, 0, 1), times=49)

Exchange.Kenya$Bolean.Quartal <- Bolean.Quartal

Exchange.Kenya$Quarterly.Exchange.Kenya <- Exchange.Kenya$Bolean.Quartal * Exchange.Kenya$RollAvge

Exchange.Kenya <- Exchange.Kenya[,9, drop=FALSE]
#Hier müssen noch die 0 zu NA aber das mache ich wenn ich alle anderen schon hinzugefügt habe




#Now for Nigeria:

getSymbols("NGNUSD=X", src = "yahoo", from=StartTime, to=EndTime, periodicity = "monthly") 
Exchange.Nigeria <- as.data.frame(`NGNUSD=X`)

Exchange.Nigeria <- Exchange.Nigeria %>% mutate(RollAvge= rollapplyr(Exchange.Nigeria$`NGNUSD=X.Open`, list(-(0:2)), mean, partial = TRUE, fill = NA))

#Creating a sequence so that we get filter out the quarterly (averaged) exchange rates:

Bolean.Quartal <- rep(c(0, 0, 1), times=49)

Exchange.Nigeria$Bolean.Quartal <- Bolean.Quartal

Exchange.Nigeria$Quarterly.Exchange.Nigeria <- Exchange.Nigeria$Bolean.Quartal * Exchange.Nigeria$RollAvge

Exchange.Nigeria <- Exchange.Nigeria[,9, drop=FALSE]
#Hier müssen noch die 0 zu NA aber das mache ich wenn ich alle anderen schon hinzugefügt habe


#Now for South Africa:



getSymbols("ZARUSD=X", src = "yahoo", from=StartTime, to=EndTime, periodicity = "monthly") 
Exchange.SA <- as.data.frame(`ZARUSD=X`)

Exchange.SA <- Exchange.SA %>% mutate(RollAvge= rollapplyr(Exchange.SA$`ZARUSD=X.Open`, list(-(0:2)), mean, partial = TRUE, fill = NA))

#Creating a sequence so that we get filter out the quarterly (averaged) exchange rates:

Bolean.Quartal <- rep(c(0, 0, 1), times=49)

Exchange.SA$Bolean.Quartal <- Bolean.Quartal

Exchange.SA$Quarterly.Exchange.SA <- Exchange.SA$Bolean.Quartal * Exchange.SA$RollAvge

Exchange.SA <- Exchange.SA[,9, drop=FALSE]
#Hier müssen noch die 0 zu NA aber das mache ich wenn ich alle anderen schon hinzugefügt habe




# Now for Namibia


getSymbols("NADUSD=X", src = "yahoo", from=StartTime, to=EndTime, periodicity = "monthly") 
Exchange.Namibia <- as.data.frame(`NADUSD=X`)

Exchange.Namibia <- Exchange.Namibia %>% mutate(RollAvge= rollapplyr(Exchange.Namibia$`NADUSD=X.Open`, list(-(0:2)), mean, partial = TRUE, fill = NA))

#Creating a sequence so that we get filter out the quarterly (averaged) exchange rates:

Bolean.Quartal <- rep(c(0, 0, 1), times=49)

Exchange.Namibia$Bolean.Quartal <- Bolean.Quartal

Exchange.Namibia$Quarterly.Exchange.Namibia <- Exchange.Namibia$Bolean.Quartal * Exchange.Namibia$RollAvge

Exchange.Namibia <- Exchange.Namibia[,9, drop=FALSE]
#Hier müssen noch die 0 zu NA aber das mache ich wenn ich alle anderen schon hinzugefügt habe


#Datensatz zusammenfügen

Exchange.Kenya$Quarterly.Exchange.Namibia <- Exchange.Namibia$Quarterly.Exchange.Namibia
Exchange.Kenya$Quarterly.Exchange.SA <- Exchange.SA$Quarterly.Exchange.SA
Exchange.Kenya$Quarterly.Exchange.Nigeria <- Exchange.Nigeria$Quarterly.Exchange.Nigeria



##Jetzt alles auf die gleiche Währung (US Dollar) und auf die gleiche Einheit bringen (in Millions, Billions etc. angehen)
#Dazu: Großen Datensatz erstellen und Exchange Rates nach Land mergen

#Value of Trade and Exchange rates:
Exchange.Rates <- Exchange.Kenya

#Delete rows with obs=0000000 from Exchange.Rates:

Exchange.Rates[Exchange.Rates == 0] <- NA
Exchange.Rates <- na.omit(Exchange.Rates)
Exchange.Rates.SA <- Exchange.Rates[,3, drop=FALSE] 
Exchange.Rates.NA <- Exchange.Rates[,2, drop=FALSE] 
Exchange.Rates.NI <- Exchange.Rates[,4, drop=FALSE]
Exchange.Rates.KE <- Exchange.Rates[,1, drop=FALSE]  

#Now: TimePeriod and country id adding so that it can be merged later

Exchange.Rates.SA$`@TIME_PERIOD` <- GDP.Nigeria$`@TIME_PERIOD`
Exchange.Rates.NA$`@TIME_PERIOD` <- GDP.Nigeria$`@TIME_PERIOD`
Exchange.Rates.NI$`@TIME_PERIOD` <- GDP.Nigeria$`@TIME_PERIOD`
Exchange.Rates.KE$`@TIME_PERIOD` <- GDP.Nigeria$`@TIME_PERIOD`

Exchange.Rates.KE$`@REF_AREA` <- "KE"
Exchange.Rates.NA$`@REF_AREA` <- "NA"
Exchange.Rates.SA$`@REF_AREA`<- "SA"
Exchange.Rates.NI$`@REF_AREA`<- "NI"


#GDP Information:

GDP.KE.SA.NI.NA$`@OBS_VALUE.numeric` <- as.numeric(GDP.KE.SA.NI.NA$`@OBS_VALUE`)
GDP.KE.SA.NI.NA$`@OBS_VALUE.NA.in.millions` <- ifelse(GDP.KE.SA.NI.NA$`@REF_AREA`=="NA",GDP.KE.SA.NI.NA$`@OBS_VALUE.numeric`*1000, NA)
#I created: For Namibia: Quarterly GDP in Million USD
#Extract values of interest into seperate dataset:

GDP.NA <- GDP.KE.SA.NI.NA
GDP.NA$`@FREQ` <- GDP.NA$`@INDICATOR` <- GDP.NA$`@UNIT_MULT` <- GDP.NA$`@TIME_FORMAT` <- GDP.NA$`@OBS_VALUE` <- GDP.NA$`@OBS_VALUE.numeric` <- NULL


#NIGERIAS GDP (in Million USD):
GDP.NI <- merge(GDP.KE.SA.NI.NA,Exchange.Rates.NI,by=c("@REF_AREA","@TIME_PERIOD"))

GDP.NI$`@FREQ` <- GDP.NI$`@OBS_VALUE` <-  GDP.NI$`@INDICATOR` <- GDP.NI$`@UNIT_MULT` <- GDP.NI$`@TIME_FORMAT` <- GDP.NI$`@OBS_VALUE.NA.in.millions` <- NULL

GDP.NI$GDP.in.millio.US <- GDP.NI$`@OBS_VALUE.numeric`*GDP.NI$Quarterly.Exchange.Nigeria

#12583.48 Mrd. Naira sind also 84.24640 Mrd USD

GDP.NI$GDP.in.millio.US.Dollar <- GDP.NI$GDP.in.millio.US*1000
GDP.NI <- GDP.NI[c(1:49),]

#FERTIG: Spalte GDP.in.millionen war das Ziel

GDP.KE <- merge(GDP.KE.SA.NI.NA,Exchange.Rates.KE,by=c("@REF_AREA","@TIME_PERIOD"))

GDP.KE$`@FREQ` <- GDP.KE$`@OBS_VALUE` <-  GDP.KE$`@INDICATOR` <- GDP.KE$`@UNIT_MULT` <- GDP.KE$`@TIME_FORMAT` <- GDP.KE$`@OBS_VALUE.NA.in.millions` <- NULL

GDP.KE$GDP.in.millio.US <- GDP.KE$`@OBS_VALUE.numeric`*GDP.KE$Quarterly.Exchange.Kenya

#FERTIG: Spalte GDP.in.million.US war das Ziel

Exchange.Rates.SA$`@REF_AREA` <- "ZA"
GDP.SA <- merge(GDP.KE.SA.NI.NA,Exchange.Rates.SA,by=c("@REF_AREA","@TIME_PERIOD"))

GDP.SA$`@FREQ` <- GDP.SA$`@OBS_VALUE` <-  GDP.SA$`@INDICATOR` <- GDP.SA$`@UNIT_MULT` <- GDP.SA$`@TIME_FORMAT` <- GDP.SA$`@OBS_VALUE.NA.in.millions` <- NULL

GDP.SA$GDP.in.millio.US <- GDP.SA$`@OBS_VALUE.numeric`*GDP.SA$Quarterly.Exchange.SA

#FETIG: Spalte GDP.in.million.US war das Ziel

#Wieso brauch ich nicht die Werte von EU (IMPEX(EU)):
#Ich habe zwei Regressionen vor: 1) Regression: Spillover von EUROPA auf Afrikanische Länder, also regressiere ich den Vektor:
#(EU->SA ; EU->KE ; EU->NI ; EU->NA) und versuche diesen zu erklären mit dem Trade-Openess-Vektor (TO): (TO(SA), TO(NI), TO(NA), TO(KE))
#2) Regression: Spillover zwischen den einzelnen Afrikanischen Ländern: also Regressiere ich afrikanische Länder spillover auf andere afrikanische Länder und benutze dafür auch nur den 
#eben beschriebene TO-Vektor


# 1.3.3 Creating IMPEX ----------------------------------------------------

#IMPEX = (EX + IM)/GDP 

#EX+IM ist zu finden in: VALUE.OF.TRADE

df <- merge(VALUE.OF.TRADE,GDP.SA, by=c("@REF_AREA","@TIME_PERIOD"))
IMPEX <- df   

IMPEX$`@FREQ.x` <- IMPEX$`@INDICATOR.x` <- IMPEX$`@COUNTERPART_AREA.x` <- IMPEX$`@UNIT_MULT.x` <- IMPEX$`@UNIT_MULT.y` <- IMPEX$`@TIME_FORMAT.x` <- IMPEX$EXPORT <- IMPEX$IMPORT <- IMPEX$`@FREQ.y` <- IMPEX$`@INDICATOR.y` <- IMPEX$`@COUNTERPART_AREA.y` <- NULL
IMPEX$Quarterly.Exchange.SA <- IMPEX$`@OBS_VALUE.numeric` <- IMPEX$`@TIME_FORMAT.y` <- NULL
IMPEX$IMPEX.SA <- IMPEX$Value.of.trade/IMPEX$GDP.in.millio.US
IMPEX$Value.of.trade <- IMPEX$GDP.in.millio.US <- NULL
names(IMPEX)[3] <- "IMPEX"

#IMPEX for SA is created 

df <- merge(VALUE.OF.TRADE,GDP.NA, by=c("@REF_AREA","@TIME_PERIOD"))
df <- df[c(54:102),]

IMPEX.NA <- df
IMPEX.NA$`@FREQ.x` <- IMPEX.NA$`@INDICATOR.x` <- IMPEX.NA$`@COUNTERPART_AREA.x` <- IMPEX.NA$`@UNIT_MULT.x` <- IMPEX.NA$`@UNIT_MULT.y` <- IMPEX.NA$`@TIME_FORMAT.x` <- IMPEX.NA$EXPORT <- IMPEX.NA$IMPORT <- IMPEX.NA$`@FREQ.y` <- IMPEX.NA$`@INDICATOR.y` <- IMPEX.NA$`@COUNTERPART_AREA.y` <- NULL
IMPEX.NA$`@TIME_FORMAT.y` <- NULL

IMPEX.NA$IMPEX <- IMPEX.NA$Value.of.trade/IMPEX.NA$`@OBS_VALUE.NA.in.millions`
IMPEX.NA$Value.of.trade <- IMPEX.NA$`@OBS_VALUE.NA.in.millions` <- IMPEX.NA$IMPEX.NA <- NULL

#IMPEX for NA is created

GDP.NI$`@REF_AREA` <- "NG"

df <- merge(VALUE.OF.TRADE,GDP.NI, by=c("@REF_AREA","@TIME_PERIOD"))

IMPEX.NI <- df
IMPEX.NI$`@FREQ.x` <- IMPEX.NI$`@INDICATOR.x` <- IMPEX.NI$`@COUNTERPART_AREA.x` <- IMPEX.NI$`@UNIT_MULT.x` <- IMPEX.NI$`@UNIT_MULT.y` <- IMPEX.NI$`@TIME_FORMAT.x` <- IMPEX.NI$EXPORT <- IMPEX.NI$IMPORT <- IMPEX.NI$`@FREQ.y` <- IMPEX.NI$`@INDICATOR.y` <- IMPEX.NI$`@COUNTERPART_AREA.y` <- NULL
IMPEX.NI$`@TIME_FORMAT.y` <- NULL
IMPEX.NI$Quarterly.Exchange.Nigeria <- IMPEX.NI$`@OBS_VALUE.numeric` <- IMPEX.NI$GDP.in.millio.US<- NULL

IMPEX.NI$IMPEX <- IMPEX.NI$Value.of.trade/IMPEX.NI$GDP.in.millio.US.Dollar
IMPEX.NI$Value.of.trade <- IMPEX.NI$GDP.in.millio.US.Dollar <- NULL

#IMPEX for NI is created

df <- merge(VALUE.OF.TRADE,GDP.KE, by=c("@REF_AREA","@TIME_PERIOD"))

IMPEX.KE <- df
IMPEX.KE$`@FREQ.x` <- IMPEX.KE$`@INDICATOR.x` <- IMPEX.KE$`@COUNTERPART_AREA.x` <- IMPEX.KE$`@UNIT_MULT.x` <- IMPEX.KE$`@UNIT_MULT.y` <- IMPEX.KE$`@TIME_FORMAT.x` <- IMPEX.KE$EXPORT <- IMPEX.KE$IMPORT <- IMPEX.KE$`@FREQ.y` <- IMPEX.KE$`@INDICATOR.y` <- IMPEX.KE$`@COUNTERPART_AREA.y` <- NULL
IMPEX.KE$`@TIME_FORMAT.y` <- NULL
IMPEX.KE$Quarterly.Exchange.Kenya <- IMPEX.KE$`@OBS_VALUE.numeric` <- NULL

IMPEX.KE$IMPEX <- IMPEX.KE$Value.of.trade/IMPEX.KE$GDP.in.millio.US
IMPEX.KE$Value.of.trade <-  IMPEX.KE$GDP.in.millio.US <- NULL

#Jetzt nur noch alles zusammen in einen Datensatz

IMPEX.ALL <- rbind(IMPEX,IMPEX.NA,IMPEX.KE,IMPEX.NI)


# PART 2 ------------------------------------------------------------------


library(janitor)
library(stringr)
library(magrittr)
library(CoordinateCleaner)
library(dplyr)
library(tidyr)
library(quantmod)
library(devtools)
install_local("/Users/juliusgross/Desktop/Bachelorarbeit Julius Gross/R/imfr_0.1.9.tar")
library(imfr)



# 1. Trade ----------------------------------------------------------------


# 1.1 Trade of each African country with Europe ---------------------------


# 1.1 Import of African Countries from EU --------------------------------------------


Africa_ISO2<- c("NG", "ZA", "NA", "KE")

IMPORT.AfricafromEU <- imf_data(database_id = "DOT" , indicator = c("TMG_CIF_USD") , country = Africa_ISO2 , start = 2010, end = current_year(), return_raw = TRUE)
IMPORT.AfricafromEUfinal <- IMPORT.AfricafromEU$CompactData$DataSet$Series

IMPORTS.OF.AFICA <- IMPORT.AfricafromEUfinal %>% 
  filter(`@FREQ` == "Q" & `@COUNTERPART_AREA` %in% "B0") %>% unnest(Obs) %>% 
  arrange(`@REF_AREA`, `@COUNTERPART_AREA`)



# 1.2 Export from African Countries to EU ---------------------------------


EXPORT.AfricatoEU <- imf_data(database_id = "DOT" , indicator = c("TXG_FOB_USD") , country = Africa_ISO2 , start = 2010, end = current_year(), return_raw = TRUE)
EXPORT.AfricatoEU.final <- EXPORT.AfricatoEU$CompactData$DataSet$Series

EXPORT.OF.AFRICA <- EXPORT.AfricatoEU.final %>% 
  filter(`@FREQ` == "Q" & `@COUNTERPART_AREA` %in% "B0") %>% unnest(Obs) %>% 
  arrange(`@REF_AREA`, `@COUNTERPART_AREA`)


# 1.3 IM+EX ---------------------------------------------------------------

names(IMPORTS.OF.AFICA)[8] <- "IMPORT"
names(EXPORT.OF.AFRICA)[8] <- "EXPORT"

IM_EX <- merge(EXPORT.OF.AFRICA,IMPORTS.OF.AFICA, by=c("@REF_AREA","@TIME_PERIOD"))
IM_EX$EXPORT <- as.numeric(IM_EX$EXPORT)
IM_EX$IMPORT <- as.numeric(IM_EX$IMPORT)

IM_EX$IM_EX <- IM_EX$EXPORT + IM_EX$IMPORT


# 1.4 Creating IMPEX (with GDP) -------------------------------------------

#IMPEX = (EX + IM)/GDP 

#EX+IM ist zu finden in: IM_EX

df <- merge(IM_EX,GDP.SA, by=c("@REF_AREA","@TIME_PERIOD"))
BILATERAL.IMPEX <- df   

BILATERAL.IMPEX$`@FREQ.x` <- BILATERAL.IMPEX$`@INDICATOR.x` <- BILATERAL.IMPEX$`@COUNTERPART_AREA.x` <- BILATERAL.IMPEX$`@UNIT_MULT.x` <- BILATERAL.IMPEX$`@UNIT_MULT.y` <- BILATERAL.IMPEX$`@TIME_FORMAT.x` <- BILATERAL.IMPEX$EXPORT <- BILATERAL.IMPEX$IMPORT <- BILATERAL.IMPEX$`@FREQ.y` <- BILATERAL.IMPEX$`@INDICATOR.y` <- BILATERAL.IMPEX$`@COUNTERPART_AREA.y` <- NULL
BILATERAL.IMPEX$Quarterly.Exchange.SA <- BILATERAL.IMPEX$`@OBS_VALUE.numeric` <- BILATERAL.IMPEX$`@TIME_FORMAT.y` <- NULL
BILATERAL.IMPEX$IMPEX.SA <- BILATERAL.IMPEX$IM_EX/BILATERAL.IMPEX$GDP.in.millio.US
BILATERAL.IMPEX$IM_EX <- BILATERAL.IMPEX$GDP.in.millio.US <- NULL
names(BILATERAL.IMPEX)[3] <- "IMPEX"

#IMPEX for SA is created 

df <- merge(IM_EX,GDP.NA, by=c("@REF_AREA","@TIME_PERIOD"))
df <- df[c(50:98),]

BILATERAL.IMPEX.NA <- df
BILATERAL.IMPEX.NA$`@FREQ.x` <- BILATERAL.IMPEX.NA$`@INDICATOR.x` <- BILATERAL.IMPEX.NA$`@COUNTERPART_AREA.x` <- BILATERAL.IMPEX.NA$`@UNIT_MULT.x` <- BILATERAL.IMPEX.NA$`@UNIT_MULT.y` <- BILATERAL.IMPEX.NA$`@TIME_FORMAT.x` <- BILATERAL.IMPEX.NA$EXPORT <- BILATERAL.IMPEX.NA$IMPORT <- BILATERAL.IMPEX.NA$`@FREQ.y` <- BILATERAL.IMPEX.NA$`@INDICATOR.y` <- BILATERAL.IMPEX.NA$`@COUNTERPART_AREA.y` <- NULL
BILATERAL.IMPEX.NA$`@TIME_FORMAT.y` <- NULL

BILATERAL.IMPEX.NA$IMPEX <- BILATERAL.IMPEX.NA$IM_EX/BILATERAL.IMPEX.NA$`@OBS_VALUE.NA.in.millions`
BILATERAL.IMPEX.NA$IM_EX <- BILATERAL.IMPEX.NA$`@OBS_VALUE.NA.in.millions` <- NULL

#IMPEX for NA is created

GDP.NI$`@REF_AREA` <- "NG"

df <- merge(IM_EX,GDP.NI, by=c("@REF_AREA","@TIME_PERIOD"))

BILATERAL.IMPEX.NI <- df
BILATERAL.IMPEX.NI$`@FREQ.x` <- BILATERAL.IMPEX.NI$`@INDICATOR.x` <- BILATERAL.IMPEX.NI$`@COUNTERPART_AREA.x` <- BILATERAL.IMPEX.NI$`@UNIT_MULT.x` <- BILATERAL.IMPEX.NI$`@UNIT_MULT.y` <- BILATERAL.IMPEX.NI$`@TIME_FORMAT.x` <- BILATERAL.IMPEX.NI$EXPORT <- BILATERAL.IMPEX.NI$IMPORT <- BILATERAL.IMPEX.NI$`@FREQ.y` <- BILATERAL.IMPEX.NI$`@INDICATOR.y` <- BILATERAL.IMPEX.NI$`@COUNTERPART_AREA.y` <- NULL
BILATERAL.IMPEX.NI$`@TIME_FORMAT.y` <- NULL
BILATERAL.IMPEX.NI$Quarterly.Exchange.Nigeria <- BILATERAL.IMPEX.NI$`@OBS_VALUE.numeric` <- BILATERAL.IMPEX.NI$GDP.in.millio.US<- NULL

BILATERAL.IMPEX.NI$IMPEX <- BILATERAL.IMPEX.NI$IM_EX/BILATERAL.IMPEX.NI$GDP.in.millio.US.Dollar
BILATERAL.IMPEX.NI$IM_EX <- BILATERAL.IMPEX.NI$GDP.in.millio.US.Dollar <- NULL

#IMPEX for NI is created

df <- merge(IM_EX,GDP.KE, by=c("@REF_AREA","@TIME_PERIOD"))

BILATERAL.IMPEX.KE <- df
BILATERAL.IMPEX.KE$`@FREQ.x` <- BILATERAL.IMPEX.KE$`@INDICATOR.x` <- BILATERAL.IMPEX.KE$`@COUNTERPART_AREA.x` <- BILATERAL.IMPEX.KE$`@UNIT_MULT.x` <- BILATERAL.IMPEX.KE$`@UNIT_MULT.y` <- BILATERAL.IMPEX.KE$`@TIME_FORMAT.x` <- BILATERAL.IMPEX.KE$EXPORT <- BILATERAL.IMPEX.KE$IMPORT <- BILATERAL.IMPEX.KE$`@FREQ.y` <- BILATERAL.IMPEX.KE$`@INDICATOR.y` <- BILATERAL.IMPEX.KE$`@COUNTERPART_AREA.y` <- NULL
BILATERAL.IMPEX.KE$`@TIME_FORMAT.y` <- NULL
BILATERAL.IMPEX.KE$Quarterly.Exchange.Kenya <- BILATERAL.IMPEX.KE$`@OBS_VALUE.numeric` <- NULL

BILATERAL.IMPEX.KE$IMPEX <- BILATERAL.IMPEX.KE$IM_EX/BILATERAL.IMPEX.KE$GDP.in.millio.US
BILATERAL.IMPEX.KE$IM_EX <-  BILATERAL.IMPEX.KE$GDP.in.millio.US <- NULL

#Jetzt nur noch alles zusammen in einen Datensatz

IMPEX.AFRIKA.EU <- rbind(BILATERAL.IMPEX,BILATERAL.IMPEX.NA,BILATERAL.IMPEX.KE,BILATERAL.IMPEX.NI)
IMPEX.AFRIKA.EU$`@COUNTERPART_AREA` <- "B0"

# 1.4 Creating IMPEX (within Afrika) -------------------------------------

#Instead of creating an bilateral index, I do the same as above, however changing the destination and origin of ex and imports.
#I thus compute, for each african country, one impex that involves all imports and exports to or from sub-saharan african countries, instead
#of computing a bilateral measure.



# 1.4.1 Imports within Africa ---------------------------------------------

Africa_ISO2<- c("NG", "ZA", "NA", "KE")

IMPORT.AfricafromSUB<- imf_data(database_id = "DOT" , indicator = c("TMG_CIF_USD") , country = Africa_ISO2 , start = 2010, end = current_year(), return_raw = TRUE)
IMPORT.AfricafromSUBfinal <- IMPORT.AfricafromSUB$CompactData$DataSet$Series

IMPORTS.OF.AFICA.FROM.SUB <- IMPORT.AfricafromSUBfinal %>% 
  filter(`@FREQ` == "Q" & `@COUNTERPART_AREA` %in% "F6") %>% unnest(Obs) %>% 
  arrange(`@REF_AREA`, `@COUNTERPART_AREA`)


# 1.4.2 Exports within Africa

EXPORT.AfricatoSUB <- imf_data(database_id = "DOT" , indicator = c("TXG_FOB_USD") , country = Africa_ISO2 , start = 2010, end = current_year(), return_raw = TRUE)
EXPORT.AfricatoSUB.final <- EXPORT.AfricatoSUB$CompactData$DataSet$Series

EXPORT.OF.AFRICA.SUB <- EXPORT.AfricatoSUB.final %>% 
  filter(`@FREQ` == "Q" & `@COUNTERPART_AREA` %in% "F6") %>% unnest(Obs) %>% 
  arrange(`@REF_AREA`, `@COUNTERPART_AREA`)


# 1.4.3 IM+EX ---------------------------------------------------------------

names(IMPORTS.OF.AFICA.FROM.SUB)[8] <- "IMPORT"
names(EXPORT.OF.AFRICA.SUB)[8] <- "EXPORT"

IM_EX_SUB <- merge(EXPORT.OF.AFRICA.SUB,IMPORTS.OF.AFICA.FROM.SUB, by=c("@REF_AREA","@TIME_PERIOD"))
IM_EX_SUB$EXPORT <- as.numeric(IM_EX_SUB$EXPORT)
IM_EX_SUB$IMPORT <- as.numeric(IM_EX_SUB$IMPORT)

IM_EX_SUB$IM_EX <- IM_EX_SUB$EXPORT + IM_EX_SUB$IMPORT


# 1.4.4 Creating IMPEX (with GDP) -------------------------------------------

#IMPEX = (EX + IM)/GDP 

#EX+IM ist zu finden in: IM_EX

df <- merge(IM_EX_SUB,GDP.SA, by=c("@REF_AREA","@TIME_PERIOD"))
SUB.IMPEX <- df   

SUB.IMPEX$`@FREQ.x` <- SUB.IMPEX$`@INDICATOR.x` <- SUB.IMPEX$`@COUNTERPART_AREA.x` <- SUB.IMPEX$`@UNIT_MULT.x` <- SUB.IMPEX$`@UNIT_MULT.y` <- SUB.IMPEX$`@TIME_FORMAT.x` <- SUB.IMPEX$EXPORT <- SUB.IMPEX$IMPORT <- SUB.IMPEX$`@FREQ.y` <- SUB.IMPEX$`@INDICATOR.y` <- SUB.IMPEX$`@COUNTERPART_AREA.y` <- NULL
SUB.IMPEX$Quarterly.Exchange.SA <- SUB.IMPEX$`@OBS_VALUE.numeric` <- SUB.IMPEX$`@TIME_FORMAT.y` <- NULL
SUB.IMPEX$IMPEX.SA <- SUB.IMPEX$IM_EX/SUB.IMPEX$GDP.in.millio.US
SUB.IMPEX$IM_EX <- SUB.IMPEX$GDP.in.millio.US <- NULL
names(SUB.IMPEX)[3] <- "IMPEX"

#IMPEX for SA is created 

df <- merge(IM_EX_SUB,GDP.NA, by=c("@REF_AREA","@TIME_PERIOD"))
df <- df[c(50:98),]

SUB.IMPEX.NA <- df
SUB.IMPEX.NA$`@FREQ.x` <- SUB.IMPEX.NA$`@INDICATOR.x` <- SUB.IMPEX.NA$`@COUNTERPART_AREA.x` <- SUB.IMPEX.NA$`@UNIT_MULT.x` <- SUB.IMPEX.NA$`@UNIT_MULT.y` <- SUB.IMPEX.NA$`@TIME_FORMAT.x` <- SUB.IMPEX.NA$EXPORT <- SUB.IMPEX.NA$IMPORT <- SUB.IMPEX.NA$`@FREQ.y` <- SUB.IMPEX.NA$`@INDICATOR.y` <- SUB.IMPEX.NA$`@COUNTERPART_AREA.y` <- NULL
SUB.IMPEX.NA$`@TIME_FORMAT.y` <- NULL

SUB.IMPEX.NA$IMPEX <- SUB.IMPEX.NA$IM_EX/SUB.IMPEX.NA$`@OBS_VALUE.NA.in.millions`
SUB.IMPEX.NA$IM_EX <- SUB.IMPEX.NA$`@OBS_VALUE.NA.in.millions` <- NULL

#IMPEX for NA is created

GDP.NI$`@REF_AREA` <- "NG"

df <- merge(IM_EX_SUB,GDP.NI, by=c("@REF_AREA","@TIME_PERIOD"))

SUB.IMPEX.NI <- df
SUB.IMPEX.NI$`@FREQ.x` <- SUB.IMPEX.NI$`@INDICATOR.x` <- SUB.IMPEX.NI$`@COUNTERPART_AREA.x` <- SUB.IMPEX.NI$`@UNIT_MULT.x` <- SUB.IMPEX.NI$`@UNIT_MULT.y` <- SUB.IMPEX.NI$`@TIME_FORMAT.x` <- SUB.IMPEX.NI$EXPORT <- SUB.IMPEX.NI$IMPORT <- SUB.IMPEX.NI$`@FREQ.y` <- SUB.IMPEX.NI$`@INDICATOR.y` <- SUB.IMPEX.NI$`@COUNTERPART_AREA.y` <- NULL
SUB.IMPEX.NI$`@TIME_FORMAT.y` <- NULL
SUB.IMPEX.NI$Quarterly.Exchange.Nigeria <- SUB.IMPEX.NI$`@OBS_VALUE.numeric` <- SUB.IMPEX.NI$GDP.in.millio.US<- NULL

SUB.IMPEX.NI$IMPEX <- SUB.IMPEX.NI$IM_EX/SUB.IMPEX.NI$GDP.in.millio.US.Dollar
SUB.IMPEX.NI$IM_EX <- SUB.IMPEX.NI$GDP.in.millio.US.Dollar <- NULL

#IMPEX for NI is created

df <- merge(IM_EX_SUB,GDP.KE, by=c("@REF_AREA","@TIME_PERIOD"))

SUB.IMPEX.KE <- df
SUB.IMPEX.KE$`@FREQ.x` <- SUB.IMPEX.KE$`@INDICATOR.x` <- SUB.IMPEX.KE$`@COUNTERPART_AREA.x` <- SUB.IMPEX.KE$`@UNIT_MULT.x` <- SUB.IMPEX.KE$`@UNIT_MULT.y` <- SUB.IMPEX.KE$`@TIME_FORMAT.x` <- SUB.IMPEX.KE$EXPORT <- SUB.IMPEX.KE$IMPORT <- SUB.IMPEX.KE$`@FREQ.y` <- SUB.IMPEX.KE$`@INDICATOR.y` <- SUB.IMPEX.KE$`@COUNTERPART_AREA.y` <- NULL
SUB.IMPEX.KE$`@TIME_FORMAT.y` <- NULL
SUB.IMPEX.KE$Quarterly.Exchange.Kenya <- SUB.IMPEX.KE$`@OBS_VALUE.numeric` <- NULL

SUB.IMPEX.KE$IMPEX <- SUB.IMPEX.KE$IM_EX/SUB.IMPEX.KE$GDP.in.millio.US
SUB.IMPEX.KE$IM_EX <-  SUB.IMPEX.KE$GDP.in.millio.US <- NULL

#Jetzt nur noch alles zusammen in einen Datensatz

IMPEX.AFRIKA.SUB <- rbind(SUB.IMPEX,SUB.IMPEX.NI, SUB.IMPEX.KE, SUB.IMPEX.NA)
IMPEX.AFRIKA.SUB$`@COUNTERPART_AREA` <- "F6"


# 2.  Global Factors ------------------------------------------------------


# 2.1 Global crude oil prices  ---------------------------------------------------

Global.oil <- imf_data(database_id = "PCPS" , indicator = c("POILAPSP") , country = "W00" , start = 2010, end = current_year(), return_raw = TRUE)
Global.oil.final <- Global.oil$CompactData$DataSet$Series

GLOBAL.OIL <- Global.oil.final %>% 
  filter(`@FREQ` == "Q" & `@UNIT_MEASURE` %in% "USD") %>% unnest(Obs) %>% 
  arrange(`@TIME_PERIOD`, `@OBS_VALUE`)

GLOBAL.OIL$`@FREQ` <- GLOBAL.OIL$`@COMMODITY` <- GLOBAL.OIL$`@UNIT_MEASURE` <- GLOBAL.OIL$`@UNIT_MULT` <- GLOBAL.OIL$`@TIME_FORMAT` <- NULL

GLOBAL.OIL.2 <- GLOBAL.OIL
GLOBAL.OIL$`@REF_AREA` <- "KE"
GLOBAL.OIL.3 <- GLOBAL.OIL.2
GLOBAL.OIL.4 <- GLOBAL.OIL.2

GLOBAL.OIL.2$`@REF_AREA` <- "NG"
GLOBAL.OIL.3$`@REF_AREA` <- "ZA"
GLOBAL.OIL.4$`@REF_AREA` <- "NA"

GLOBAL.OIL <- rbind(GLOBAL.OIL, GLOBAL.OIL.2, GLOBAL.OIL.3, GLOBAL.OIL.4)

names(GLOBAL.OIL)[3] <- "Global.Oil"

# 2.2 Global metal prices -------------------------------------------------

Global.metal <- imf_data(database_id = "PCPS" , indicator = c("PMETA") , country = "W00" , start = 2010, end = current_year(), return_raw = TRUE)
Global.metal.final<- Global.metal$CompactData$DataSet$Series

GLOBAL.METAL <- Global.metal.final %>% 
  filter(`@FREQ` == "Q" & `@UNIT_MEASURE` %in% "IX") %>% unnest(Obs) %>% 
  arrange(`@TIME_PERIOD`, `@OBS_VALUE`)


GLOBAL.METAL$`@UNIT_MULT` <- GLOBAL.METAL$`@FREQ` <- GLOBAL.METAL$`@COMMODITY` <-  GLOBAL.METAL$`@TIME_FORMAT` <- NULL
GLOBAL.METAL$`@UNIT_MEASURE` <- NULL

GLOBAL.METAL.2 <- GLOBAL.METAL
GLOBAL.METAL.3 <- GLOBAL.METAL
GLOBAL.METAL.4 <- GLOBAL.METAL

GLOBAL.METAL$`@REF_AREA` <- "KE"
GLOBAL.METAL.2$`@REF_AREA` <- "NG"
GLOBAL.METAL.3$`@REF_AREA` <- "NA"
GLOBAL.METAL.4$`@REF_AREA` <- "ZA"

GLOBAL.METAL <- rbind(GLOBAL.METAL, GLOBAL.METAL.2, GLOBAL.METAL.3, GLOBAL.METAL.4)
names(GLOBAL.METAL)[3] <- "Global.Metal"

# 2.3 EURO-Area Inflation ---------------------------

Euro.infla <- imf_data(database_id = "IFS" , indicator = c("PCPIHA_IX") , country = "U2" , start = 2010, end = current_year(), return_raw = TRUE)
Euro.infla.final<- Euro.infla$CompactData$DataSet$Series

EURO.INFLA <- Euro.infla.final %>% 
  filter(`@FREQ` == "Q") %>% unnest(Obs) %>% 
  arrange(`@TIME_PERIOD`, `@OBS_VALUE`)

EURO.INFLA <- EURO.INFLA[-c(50),]
EURO.INFLA$`@FREQ` <- EURO.INFLA$`@INDICATOR` <- EURO.INFLA$`@UNIT_MULT` <- EURO.INFLA$`@TIME_FORMAT` <- EURO.INFLA$`@REF_AREA` <- EURO.INFLA$`@BASE_YEAR` <- NULL
EURO.INFLA$`@REF_AREA` <- "KE"

EURO.INFLA.2 <- EURO.INFLA
EURO.INFLA.3 <- EURO.INFLA
EURO.INFLA.4 <- EURO.INFLA

EURO.INFLA.2$`@REF_AREA` <- "NA"
EURO.INFLA.3$`@REF_AREA` <- "NG"
EURO.INFLA.4$`@REF_AREA` <- "ZA"

EURO.INFLA <- rbind(EURO.INFLA, EURO.INFLA.2, EURO.INFLA.3, EURO.INFLA.4)

names(EURO.INFLA)[2] <- "Euro.Infla"

# 3. Other variables ------------------------------------------------------


# 3.1 Bilateral Export share  (with EU) -----------------------------------

Exports.to.EU <- imf_data(database_id = "DOT" , indicator = c("TXG_FOB_USD") , country = Africa_ISO2 , start = 2010, end = current_year(), return_raw = TRUE)
Exports.to.EU <- Exports.to.EU$CompactData$DataSet$Series

Exports.to.EU <- Exports.to.EU %>% 
  filter(`@FREQ` == "Q" & `@COUNTERPART_AREA` %in% "B0") %>% unnest(Obs) %>% 
  arrange(`@REF_AREA`, `@COUNTERPART_AREA`)

#I now want to express this as a share of total exports, thus I gather data for total exports (same code just for a different "Counterpart_Area":

Exports.to.World <- imf_data(database_id = "DOT" , indicator = c("TXG_FOB_USD") , country = Africa_ISO2 , start = 2010, end = current_year(), return_raw = TRUE)
Exports.to.World <- Exports.to.World$CompactData$DataSet$Series

Exports.to.World <- Exports.to.World %>% 
  filter(`@FREQ` == "Q" & `@COUNTERPART_AREA` %in% "W00") %>% unnest(Obs) %>% 
  arrange(`@REF_AREA`, `@COUNTERPART_AREA`)

#All I have to do now is to merge those two by Country and Quater and divide 1) to EU by 2) to World:

Export.Share <- merge(Exports.to.EU,Exports.to.World, by=c("@REF_AREA","@TIME_PERIOD"))

Export.Share$`@OBS_VALUE.x` <- as.numeric(Export.Share$`@OBS_VALUE.x`)
Export.Share$`@OBS_VALUE.y` <- as.numeric(Export.Share$`@OBS_VALUE.y`)

Export.Share$Export.Share <- Export.Share$`@OBS_VALUE.x`/Export.Share$`@OBS_VALUE.y` 

Export.Share$`@FREQ.x` <- Export.Share$`@FREQ.x` <- Export.Share$`@OBS_VALUE.y` <- Export.Share$`@TIME_FORMAT.y` <- Export.Share$`@UNIT_MULT.y` <- Export.Share$`@COUNTERPART_AREA.y` <- Export.Share$`@INDICATOR.y` <- Export.Share$`@FREQ.y` <- Export.Share$`@OBS_VALUE.x` <- Export.Share$`@TIME_FORMAT.x` <- Export.Share$`@UNIT_MULT.x` <- Export.Share$`@COUNTERPART_AREA.x` <- NULL
Export.Share$`@INDICATOR.x` <- NULL


# 3.2 Bilaterial portfolio investment / Liabilities -------------------------------------

#Problem: This is bi-annual data:

Liabilities <- imf_data(database_id = "CPIS" , indicator = c("I_L_T_T_T_BP6_DV_USD") , country = Africa_ISO2 , start = 2010, end = current_year(), return_raw = TRUE)
Liabilities <- Liabilities$CompactData$DataSet$Series

Liabilities.final <- Liabilities %>% 
  filter(`@FREQ` == "B" & `@COUNTERPART_AREA` %in% "W00") %>% unnest(Obs) %>% 
  arrange(`@REF_AREA`, `@COUNTERPART_AREA`)

# Not sure yet, whether I want to use it or not...

#Alternative: Balance of Payments

Balance.of.payments <- imf_data(database_id = "IFS" , indicator = c("BFDA_BP6_USD") , country = Africa_ISO2 , start = 2010, end = current_year(), return_raw = TRUE)
Balance.of.payments <- Balance.of.payments$CompactData$DataSet$Series

Balance.of.payments.final <- Balance.of.payments %>% 
  filter(`@FREQ` == "Q") %>% unnest(Obs)

Balance.of.payments.final$`@FREQ` <- Balance.of.payments.final$`@UNIT_MULT` <- Balance.of.payments.final$`@TIME_FORMAT` <- Balance.of.payments.final$`@OBS_STATUS` <- NULL
Balance.of.payments.final$`@INDICATOR` <- NULL
names(Balance.of.payments.final)[3] <- "Financial"

#I use this one in the panel regression:

Balance.of.payments.final.2 <- read_excel("Kenya Balance of Payment.xlsx")
names(Balance.of.payments.final.2)[2] <- "Financial"
Balance.of.payments.final.2$`@REF_AREA` <- "KE"
names(Balance.of.payments.final.2)[1] <- "@TIME_PERIOD"

BALANCE.of.PAYMENTS <- rbind(Balance.of.payments.final, Balance.of.payments.final.2)


# 4.  Panel Regressions ---------------------------------------------------


# 4.1 Preparation Data sets -----------------------------------------------


# 4.1.1 Determinants of Spillovers from EU --------------------------------

#I want to merge the following data sets: Balance.of.PAYMENTS, Export.Share, EURO.INFLA, GLOBAL.METAL, GLOBAL.OIL and IMPEX.AFRIKA.EU:

Panel.Africa.EU <- merge(IMPEX.AFRIKA.EU, GLOBAL.METAL, by=c("@REF_AREA","@TIME_PERIOD"))
Panel.Africa.EU <- merge(Panel.Africa.EU, GLOBAL.OIL, by=c("@REF_AREA","@TIME_PERIOD"))
Panel.Africa.EU <- merge(Panel.Africa.EU, EURO.INFLA, by=c("@REF_AREA","@TIME_PERIOD"))
Panel.Africa.EU <- merge(Panel.Africa.EU, Export.Share, by=c("@REF_AREA","@TIME_PERIOD"))
Panel.Africa.EU <- merge(Panel.Africa.EU, BALANCE.of.PAYMENTS , by=c("@REF_AREA","@TIME_PERIOD"), all = TRUE)


# 4.1.2 Merging Spillover data with determinants --------------------------

#ALTERNATIVELY FOR VAR(3) AND H=12:

PANEL.DATASET.EU.AFRICA.3.12 <- merge(Panel.Africa.EU, BIDIRECTIONAL.EU.TO.AFRICA.QUARTERLY.3.12 , by=c("@REF_AREA","@TIME_PERIOD"), all = TRUE)
PANEL.DATASET.EU.AFRICA.3.12 <- na.omit(PANEL.DATASET.EU.AFRICA.3.12)

# 4.3 Regression for VAR(3) and H=12 --------------------------------------

PANEL.DATASET.EU.AFRICA.3.12$`@COUNTERPART_AREA` <- NULL


#Right now, the dataset is not balanced, because I dont have any observations for Namibia in 2022 (First Quarter). I tried the usual command: make.pbalanced in 
#a multitude of versions but it creates an error message. Thus, I now adjust the panel dataset myself (by adding a row of NA-observations):

#creating new row (dataframe)

NA.2022.Q1.row <- PANEL.DATASET.EU.AFRICA.3.12
NA.2022.Q1.row <- NA.2022.Q1.row[40,]

library(naniar)

NA.2022.Q1.row$IMPEX <- NA.2022.Q1.row$Global.Metal <- NA.2022.Q1.row$Global.Oil <- NA.2022.Q1.row$Euro.Infla <- NA.2022.Q1.row$Export.Share <- NA.2022.Q1.row$Financial <- NA.2022.Q1.row$Spillover <- "0"

NA.2022.Q1.row[NA.2022.Q1.row == 0] <- NA
NA.2022.Q1.row$`@TIME_PERIOD` <- "2022-Q1"

BALANCED.PANEL.DATASET.EU.AFRICA.3.12 <- rbind(PANEL.DATASET.EU.AFRICA.3.12,NA.2022.Q1.row)

BALANCED.PANEL.DATASET.EU.AFRICA.3.12$IMPEX <- as.numeric(BALANCED.PANEL.DATASET.EU.AFRICA.3.12$IMPEX)
BALANCED.PANEL.DATASET.EU.AFRICA.3.12$Global.Metal <- as.numeric(BALANCED.PANEL.DATASET.EU.AFRICA.3.12$Global.Metal)
BALANCED.PANEL.DATASET.EU.AFRICA.3.12$Global.Oil <- as.numeric(BALANCED.PANEL.DATASET.EU.AFRICA.3.12$Global.Oil)
BALANCED.PANEL.DATASET.EU.AFRICA.3.12$Euro.Infla <- as.numeric(BALANCED.PANEL.DATASET.EU.AFRICA.3.12$Euro.Infla)
BALANCED.PANEL.DATASET.EU.AFRICA.3.12$Export.Share <- as.numeric(BALANCED.PANEL.DATASET.EU.AFRICA.3.12$Export.Share)
BALANCED.PANEL.DATASET.EU.AFRICA.3.12$Financial <- as.numeric(BALANCED.PANEL.DATASET.EU.AFRICA.3.12$Financial)
BALANCED.PANEL.DATASET.EU.AFRICA.3.12$Spillover <- as.numeric(BALANCED.PANEL.DATASET.EU.AFRICA.3.12$Spillover)

PANEL.DATASET.EU.AFRICA.3.12$IMPEX <- as.numeric(PANEL.DATASET.EU.AFRICA.3.12$IMPEX)
PANEL.DATASET.EU.AFRICA.3.12$Global.Metal <- as.numeric(PANEL.DATASET.EU.AFRICA.3.12$Global.Metal)
PANEL.DATASET.EU.AFRICA.3.12$Global.Oil <- as.numeric(PANEL.DATASET.EU.AFRICA.3.12$Global.Oil)
PANEL.DATASET.EU.AFRICA.3.12$Euro.Infla <- as.numeric(PANEL.DATASET.EU.AFRICA.3.12$Euro.Infla)
PANEL.DATASET.EU.AFRICA.3.12$Export.Share <- as.numeric(PANEL.DATASET.EU.AFRICA.3.12$Export.Share)
PANEL.DATASET.EU.AFRICA.3.12$Financial <- as.numeric(PANEL.DATASET.EU.AFRICA.3.12$Financial)
PANEL.DATASET.EU.AFRICA.3.12$Spillover <- as.numeric(PANEL.DATASET.EU.AFRICA.3.12$Spillover)



Y <- cbind(PANEL.DATASET.EU.AFRICA.3.12$Spillover)
X <- cbind(PANEL.DATASET.EU.AFRICA.3.12$IMPEX, PANEL.DATASET.EU.AFRICA.3.12$Export.Share, PANEL.DATASET.EU.AFRICA.3.12$Financial, PANEL.DATASET.EU.AFRICA.3.12$Global.Metal, PANEL.DATASET.EU.AFRICA.3.12$Global.Oil, PANEL.DATASET.EU.AFRICA.3.12$Euro.Infla)

'DATA.QTR <- PANEL.DATASET.EU.AFRICA$`@TIME_PERIOD` %>% mutate(QUARTER = as.Date(as.yearqtr(format(PANEL.DATASET.EU.AFRICA$`@TIME_PERIOD`), "%Y-Q%q")))
'
library(zoo)
library(plm)

BALANCED.PANEL.DATASET.EU.AFRICA.3.12$`@TIME_PERIOD2` <- as.yearqtr(format(BALANCED.PANEL.DATASET.EU.AFRICA.3.12$`@TIME_PERIOD`), "%Y-Q%q")
BALANCED.PANEL.DATASET.EU.AFRICA.3.12$`@TIME_PERIOD2` <- as.Date(BALANCED.PANEL.DATASET.EU.AFRICA.3.12$`@TIME_PERIOD2`)

names(BALANCED.PANEL.DATASET.EU.AFRICA.3.12)[1] <- "Country"
names(BALANCED.PANEL.DATASET.EU.AFRICA.3.12)[10] <- "Time"

BALANCED.PANEL.DATASET.EU.AFRICA.3.12 <- pdata.frame(BALANCED.PANEL.DATASET.EU.AFRICA.3.12, index = c("Country", "Time"))
is.pbalanced(BALANCED.PANEL.DATASET.EU.AFRICA.3.12)


PANEL.DATASET.EU.AFRICA.3.12$`@TIME_PERIOD2` <- as.yearqtr(format(PANEL.DATASET.EU.AFRICA.3.12$`@TIME_PERIOD`), "%Y-Q%q")
PANEL.DATASET.EU.AFRICA.3.12$`@TIME_PERIOD2` <- as.Date(PANEL.DATASET.EU.AFRICA.3.12$`@TIME_PERIOD2`)

names(PANEL.DATASET.EU.AFRICA.3.12)[1] <- "Country"
names(PANEL.DATASET.EU.AFRICA.3.12)[10] <- "Time"

PANEL.DATASET.EU.AFRICA.3.12 <- pdata.frame(PANEL.DATASET.EU.AFRICA.3.12, index = c("Country", "Time"))

#REGRESSIONS:

#Fixed effects

fixed <- plm(Y ~ X, data=BALANCED.PANEL.DATASET.EU.AFRICA.3.12, model="within")
summary(fixed)

fixed.1 <- plm(Spillover ~ IMPEX + Export.Share + Financial + Global.Metal + Global.Oil + Euro.Infla, data=BALANCED.PANEL.DATASET.EU.AFRICA.3.12, model="within")
summary(fixed.1)

'random <- plm(Y ~ X, data=PANEL.DATASET.EU.AFRICA.3.12, model="random")
summary(random) ' # Regression doesn`t work with the default estimation approach via swar as the amount of observations of dependet variable is not enough in comparison to regressors`

#I decided to use walhus as the estimation approach as it produces regression results with the biggest adj. R-Squared.

'random1 <- plm(Spillover ~ IMPEX + Export.Share + Financial + Global.Metal + Global.Oil + Euro.Infla, data=PANEL.DATASET.EU.AFRICA.3.12, model="random", random.method = "amemiya")
summary(random1)'

random.1 <- plm(Spillover ~ IMPEX + Export.Share + Financial + Global.Metal + Global.Oil + Euro.Infla, data=BALANCED.PANEL.DATASET.EU.AFRICA.3.12, model="random", random.method = "walhus")
summary(random.1)

'random3 <- plm(Spillover ~ IMPEX + Export.Share + Financial + Global.Metal + Global.Oil + Euro.Infla, data=PANEL.DATASET.EU.AFRICA.3.12, model="random", random.method = "nerlove")
summary(random3)'


#I further use the fixed effects model to investigate robustness of my original results gained through the investigation of determinants 
#with the fixed effects model. For further reasoning, I refer to the thesis. Here, drawing on the informationen provided by Wooldridge (Introductiory Econometrics), I 
#give a more detailed reasoning for why it could be smart to employ a fixed effects model.

fd.1 <- plm(Spillover ~ IMPEX + Export.Share + Financial + Global.Metal + Global.Oil + Euro.Infla, data=BALANCED.PANEL.DATASET.EU.AFRICA.3.12, model="fd")
summary(fd.1)


#Hausman test:

#Which one should I use according to the Hausman test, fixed or random effects:
#Hausman test tests following hypothesis: H0=random effect model is consistent
#H1=fixed effect model is consistent

#Thus, the hausman test shows that the random effect model is adequate to model individual-level effects for volatility spillovers from EU to Africa.

### Further, I will try to estimate the effects during non-crisis times (without Corona), therefore I modify my data set accordingly:
'BALANCED.PANEL.DATASET.EU.AFRICA.3.12$Time2 <- as.Date(BALANCED.PANEL.DATASET.EU.AFRICA.3.12$X.TIME_PERIOD)
'
library(dplyr)

PANEL.DATASET.EU.AFRICA.3.12$Time2 <- as.Date(PANEL.DATASET.EU.AFRICA.3.12$Time)
PANEL.DATASET.EU.AFRICA.3.12.pre.corona <- PANEL.DATASET.EU.AFRICA.3.12 %>%
  select(Country, IMPEX, Global.Oil, Global.Metal, Euro.Infla, Export.Share, Financial, Spillover, Time2) %>%
  filter(Time2 <= "2020-01-01")

#Regression for time prior to Corona:

fixed.2 <- plm(Spillover ~ IMPEX + Export.Share + Financial + Global.Metal + Global.Oil + Euro.Infla, data=PANEL.DATASET.EU.AFRICA.3.12.pre.corona, model="within")
summary(fixed.2)

random.2 <- plm(Spillover ~ IMPEX + Export.Share + Financial + Global.Metal + Global.Oil + Euro.Infla, data=PANEL.DATASET.EU.AFRICA.3.12.pre.corona, model="random", random.method = "walhus")
summary(random.2)

fd.2 <- plm(Spillover ~ IMPEX + Export.Share + Financial + Global.Metal + Global.Oil + Euro.Infla, data=PANEL.DATASET.EU.AFRICA.3.12.pre.corona, model="fd")
summary(fd.2)


'stargazer(fixed.1, random.1, fd.1, fixed.2, random.2, fd.2)'
#Hausman test:

#Which one should I use according to the Hausman test, fixed or random effects:
#Hausman test tests following hypothesis: H0=random effect model is consistent
#H1=fixed effect model is consistent

phtest(fixed.1,random.1)
phtest(fixed.2, random.2)

#Thus, the hausman test shows that the random effect model is adequate to model individual-level effects for volatility spillovers from EU to Africa.

#According to the Hausman test, the H0, the random effect model is consistent, can not be rejected. 
#This indicates that using the random effect model is consistent and thus adequate to model individual-level effects for volatility spillovers
#from Europe to African markets under consideration.


