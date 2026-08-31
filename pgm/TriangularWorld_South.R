## Triangulation

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(mapsf)
library(sf)
library(Ternary)


## Prepare data
hc <- readRDS("data/hc.RDS")
table(hc$who)
summary(hc$when)
mymedia <- c("BRA_oglobo","KEN_standa","IDN_republ")
mymedia<-mymedia[order(mymedia)]
mystates<-substr(mymedia,1,3)

start<-as.Date("2013-01-01")
end<-as.Date("2021-01-01")



hc <- hc %>% filter(when > start, 
                   when < end, 
                   who %in% mymedia,
                   ! where1 %in% mystates)

col<-hc[,.(Fij=(round(sum(tags)))),.(i=where1, j=who)]

tab <- col %>% pivot_wider(names_from = j,values_from = Fij, values_fill = 0)
names(tab)<-c("guest","host1","host2","host3")
tab$host1 <- 100*tab$host1/sum(tab$host1)
tab$host2 <- 100*tab$host2/sum(tab$host2)
tab$host3 <- 100*tab$host3/sum(tab$host3)
tab$tot<-apply(tab[,2:4],1,mean)
tab<-tab[order(-tab$tot),]
sel<-tab %>% filter(tot > 0.1)


par(mfrow=c(1,1), mar=c(0,0,0,0))
TernaryPlot(col = "white",
            axis.col = "gray60",
            alab=paste("O Globo (Brazil)",expression('->')),
            blab=paste("Republika (Indonesia)",expression('->')),
            clab=paste(expression('<-'),"Standard (Kenya)"),            
)


mat<-sel[,2:4]
size<- sqrt(sel$tot/max(sel$tot))
TernaryPoints(mat,col="gray60",pch=20,cex=6*size)
sel$guest2<-sel$guest
sel$guest2[sel$tot < 0.6]<-""
TernaryText(mat,labels=sel$guest2,pos=2,col="black", cex=0.3+size)

