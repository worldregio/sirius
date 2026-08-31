## Linearisation

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(mapsf)
library(sf)
#library(Ternary)


## Prepare data
hc <- readRDS("data/hc.RDS")
table(hc$who)
mymedia <- c("BRA_oglobo","SGP_today")
mymedia<-mymedia[order(mymedia)]
mystates<-substr(mymedia,1,3)

start<-as.Date("2013-07-01")
end<-as.Date("2020-07-01")



hc <- hc %>% filter(when > start, 
                   when < end, 
                   who %in% mymedia,
                   ! where1 %in% mystates)

col<-hc[,.(Fij=(round(sum(tags)))),.(i=where1, j=who)]

tab <- col %>% pivot_wider(names_from = j,values_from = Fij, values_fill = 0)
names(tab)<-c("guest","host1","host2")
tab$host1 <- 1*tab$host1/sum(tab$host1)
tab$host2 <- 1*tab$host2/sum(tab$host2)
#tab$host3 <- round(10000*tab$host3/sum(tab$host3))
tab$tot<-apply(tab[,2:3],1,mean)
tab<-tab[order(-tab$tot),]
tab<-tab[1:100,]
alpha=1
tab$dis= 1/((tab$host1/tab$host2)**(1/alpha)+1)
tab$dis=2*tab$dis-1

ggplot(tab, aes(x=dis, y=tot, label=guest)) + geom_point()+
  scale_y_log10("size") + geom_text_repel() +scale_x_continuous("position",limits =c(-1,1))
