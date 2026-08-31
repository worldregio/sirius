library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(mapsf)
library(FactoMineR)

hc <- readRDS("../sirius/data/hc.RDS") 
hc3<-hc[,.(Fijt=sum(news)),.(i=who, j=where1, t=when)]
geomedia<-as.data.cube(df=hc3,         
                       dim.names = list (media=i,country=j,week=t),
                  var.names = list (articles=Fijt))
summary(geomedia)

x<-geomedia %>%
  select.dim (week,media) %>%
  arrange.elm (week, name) %>%
  arrange.elm(media,name)
t<-x$`week$media`

ggplot(t) +aes(x=week,y=articles,col=media)+geom_line()
