rm(list = ls())

library(data.table)
library(dplyr)
library(zoo)
#install.packages("zoo")
database1 =read.csv("~/Documents/grad school/Credit Rating Project/database1.csv")
updated_database2 =read.csv("~/Documents/grad school/Credit Rating Project/updated database2.csv")
rating= setDT(database1)
fund= setDT(updated_database2)

rating [ ,date :=as.Date(as.character(datadate),"%Y%m%d")]
fund[ ,date :=as.Date(as.character(datadate),"%Y%m%d")]
require("zoo")
rating [ , yq:=as.yearqtr(date)]
fund [ , yq:=as.yearqtr(date)]
rating [ , CUSIP6:=substr(cusip,1,6)]
fund [ , CUSIP6:=substr(cusip,1,6)]

rating$rating= NA
rating$rating[rating$splticrm=="D"] = 0
rating$rating[rating$splticrm=="C"] = 1
rating$rating[rating$splticrm=="CC"] = 2
rating$rating[rating$splticrm=="CCC−"] = 3
rating$rating[rating$splticrm=="CCC"] = 4
rating$rating[rating$splticrm=="CCC+"] = 5
rating$rating[rating$splticrm=="B−"] = 6
rating$rating[rating$splticrm=="B" ] = 7
rating$rating[rating$splticrm=="B+" ] = 8
rating$rating[rating$splticrm=="BB−"] = 9
rating$rating[rating$splticrm=="BB" ] = 10
rating$rating[rating$splticrm=="BB+"] = 11
rating$rating[rating$splticrm=="BBB−"] = 12
rating$rating[rating$splticrm=="BBB" ] = 13
rating$rating[rating$splticrm=="BBB+"] = 14
rating$rating[rating$splticrm=="A−"] = 15
rating$rating[rating$splticrm=="A" ] = 16
rating$rating[rating$splticrm=="A+"] = 17
rating$rating[rating$splticrm=="AA−"] = 18
rating$rating[rating$splticrm=="AA" ] = 19
rating$rating[rating$splticrm=="AA+"] = 20
rating$rating[rating$splticrm=="AAA" ] = 21
rating=rating[!is.na(rating)]

ra=fund[rating,on=.(CUSIP6,yq)]
csv <- summary(ra)
write.csv(csv,"~/Documents/grad school/Credit Rating Project/sum.ra.csv", row.names = FALSE)

#orderedra <- ra$rating = ordered(ra$rating,levels=0:20)

##modeling
linear1 <- lm(rating ~ atq+ cheq +cshoq , data=ra)
summary(linear1)

linsum <- summary(linear1)
write.csv(linsum,"~/Documents/grad school/Credit Rating Project/linsum.ra.csv", row.names = FALSE)

#install.packages("rms")
library(rms)

ologit<- lrm(rating ~ atq + cheq, data=ra)
print(ologit)

olsum <- summary(ologit)
write.csv(olsum,"~/Documents/grad school/Credit Rating Project/olsum.ra.csv", row.names = FALSE)

oprobit = orm(rating ~ atq+ cheq + dlcq, data = ra, family=probit)
print(oprobit)

opsum <- summary(oprobit)
write.csv(oprobit,"~/Documents/grad school/Credit Rating Project/opsum.ra.csv", row.names = FALSE)


# tab1.1 <- table(ra$fyearq, ra$splticrm)
# print(tab1.1)
# 
# tab2 <- table(ra)
# summary(ra)
# 
# install.packages("gtsummary")
# library(gtsummary)
# head(ra)
# tbl_summary(ra)

## create variables
#interest coverage
ra$ic <- (ra$oiadpq + ra$xintq) / ra$xintq
ra$ic[ra$ic < 0] <- 0  
ra$ic[ra$ic > 100] <- 100  
ra$ic <- cut(ra$ic,
                  breaks=c(0, 5, 10, 20, 100),
                  labels=c('A', 'B', 'C', 'D'))
#operating margin
ra$om <- (ra$oibdpq / ra$saleq)
#Long Term Debt Leverage
ra$ltdl <- (ra$dlttq / ra$atq)
#Total Debt Leverage
ra$tdl <- (ra$dlttq + ra$dlcq) / ra$atq
#NYSE%

#Beta

#Idiosyncratic Risk
ra$
#Dividend Payer
ra$dvpsxq[is.na(ra$dvpsxq)] <- 0
ra$dp <- ra$dvpsxq
ra$dp[ra$dvpsxq > 0] <- 1  
# M/B
ra$marketequity <- (ra$prccq * ra$cshoq)
ra$bookequity <- (ra$seqq - ra$pstkq + ra$txditcq) 
ra$MB <- (ra$atq - ra$bookequity + ra$marketequity) / ra$atq
# R&D
ra$RD <- (ra$xrdy / ra$atq)
# RETA
ra$RETA <- (ra$req / ra$atq)
# CAPEX
ra$capex <- (ra$capxy / ra$atq)
# Cash balances
ra$cb <- (ra$cheq / ra$atq)
#Tangibility
ra$tangibility <- (ra$ppentq / ra$atq)

##summary table
install.packages('vtable')
library(vtable)
data(ra)
st(ra, vars = c('ic','om', 'ltdl','tdl','dp','MB','RD','RETA','capex','cb','tangibility'))
vartable <- vtable(ra,out='return')

summary(ra[c('ic','om', 'ltdl','tdl','dp','MB','RD','RETA','capex','cb','tangibility')])
