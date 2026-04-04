library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(mapsf)

hc <- readRDS("data/hc.RDS")


p1 <- "MAR"
p2 <- "IRN"

x <- hc %>% filter(where1 %in% c(p1,p2),
                   !where2 %in% c(p1,p2)) %>% 
        group_by(iso3=where2, where1) %>%
             summarise(n=sum(tags)) %>% 
      pivot_wider(names_from = where1, 
                      values_from=n,
                      values_fill = 0) %>% ungroup()
names(x)<-c("iso3","Xi","Xj")

w <- x %>% mutate(pi = round(100*Xi / sum(Xi),2),
                  pj = round(100*Xj / sum(Xj),2),
                  TOT = pi+pj,
                  DIS = (Xi-Xj)/(Xi+Xj) )%>%
           arrange(DIS) 

map<-readRDS("geom/gravitymapV5_pole.RDS")
map2 <- inner_join(map,w)
mypal<-rainbow(30)[1:10]
mf_map(map, type="base")
mf_map(map2, type="choro",var="DIS", add=T,
       breaks=seq(-1,1,0.2),pal=mypal)

         