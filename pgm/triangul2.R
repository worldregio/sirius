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
mymedia <- c("ESP_abc","JPN_asahis","CAN_tostar")
mymedia<-mymedia[order(mymedia)]
mystates<-substr(mymedia,1,3)

start<-as.Date("2013-07-01")
end<-as.Date("2015-07-01")



hc <- hc %>% filter(when > start, 
                   when < end, 
                   who %in% mymedia,
                   ! where1 %in% mystates)

col<-hc[,.(Fij=(round(sum(tags)))),.(i=where1, j=who)]

tab <- col %>% pivot_wider(names_from = j,values_from = Fij, values_fill = 0)
names(tab)<-c("guest","host1","host2","host3")
tab$host1 <- round(10000*tab$host1/sum(tab$host1))
tab$host2 <- round(10000*tab$host2/sum(tab$host2))
tab$host3 <- round(10000*tab$host3/sum(tab$host3))
tab$tot<-apply(tab[,2:4],1,sum)
tab<-tab[order(-tab$tot),]
sel<-tab[1:100,]


par(mfrow=c(1,1), mar=c(0,0,3,0))
TernaryPlot(col = "lightyellow",
            axis.col = "gray80",
            lab.col = c("red", "darkgreen", "blue"),
            alab=paste(mymedia[1],expression('->')),
            blab=paste(mymedia[2],expression('->')),
            clab=paste(expression('<-'),mymedia[3]),            
)
title("The world as a  triangle...", cex.main = 1)

cols <- TernaryPointValues(rgb)
ColourTernary(cols,spectrum = NULL)


mat<-sel[,2:4]
size<- sqrt(sel$tot/max(sel$tot))
TernaryPoints(mat,col="white",pch=20,cex=4*size)
TernaryText(mat,labels=sel$guest,pos=2,col="gray80", cex=0.3+size)

