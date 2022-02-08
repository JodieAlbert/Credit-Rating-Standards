rm(list = ls())

library(data.table)
library(reshape2)
library(ggplot2)

database1 = read.csv("~/Documents/grad school/Credit Rating Project/database1.csv")
database2 = read.csv("~/Documents/grad school/Credit Rating Project/database2.csv")

sum1 =summary(database1)
sum2 =summary(database2)

write.csv(sum1,'sum1.csv')
write.csv(sum2,'sum2.csv')

