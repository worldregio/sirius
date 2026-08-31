library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(mapsf)
library(FactoMineR)
table(hc$who)
hc <- readRDS("data/hc.RDS")
listmed <- c("BRA_oglobo","CAN_tostar","DEU_frankf","ZAF_times", "EGY_alwafd","CHN_mopost",
             "AUS_sydmor", "KEN_standa","RUS_nezgaz")
listpays <-c("USA","BOL","ITA","NGA","RUS","SYR","IND","PHL","NZL","ARG","CRI","POL","DZA",
             "SEN","ETH","KEN","BRA","CAN","DEU","ZAF","EGY","CHN","AUS","KEN","RUS")
hc2 <- hc %>% filter(who %in% listmed, where1 %in% listpays)
sel <- hc2[,.(Fij=round(sum(news))),.(i=who,j=where1)] 
mod <- glm(data=sel, formula = "Fij~i+j", family="poisson")
sel$Eij<-mod$fitted.values
sel$Dij<-sel$Fij/sel$Eij
mat<- sel %>% select(i,j,Dij) %>% pivot_wider(names_from = "j",values_from = "Dij",)
mat[is.na(mat)]<-0.1
tab<-as.data.frame(mat[,-1])
rownames(tab)<-mat$i
tab
afc <- CA(tab,ncp = inf)
par(mfrow=c(1,2))
plot.CA(afc, axes=c(1,2),cex=0.7)
plot.CA(afc, axes=c(3,4),cex=0.7)
cah<-HCPC(afc,cluster.CA = "columns")
