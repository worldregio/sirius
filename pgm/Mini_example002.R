

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(mapsf)
library(sf)
library(reshape2)


## (A) Prepare data
base <- readRDS("data/hc.RDS")


start<-as.Date("2013-07-01")
end<-as.Date("2020-07-01")

mystates <-c("FRA","DEU","ITA","ESP", "POL","SWE",
             "BRA","ARG","VEN","MEX","CAN","COL")


hc <- base %>% filter(when > start, 
                      when < end, 
                      where1 %in% mystates,
                      where2 %in% mystates)

col<-hc[,.(Fij=(round(sum(tags)))),.(i=where1, j=where2)]


## (B) Anonymize

code <- function(text = "USA") {
  t <- as.character(toupper(encrypt(text,shift=1) ))
  t <- paste0(substr(t,3,3), substr(t,2,2), substr(t,1,1))
  
  return(t)
}

#col$i <- as.character(lapply(col$i,code))
#col$j <- as.character(lapply(col$j, code))


## (C) replace missing by zero

tab<-pivot_wider(col,names_from = i, values_from = Fij, values_fill = 0) %>% as.data.frame()
k<-dim(tab)[2]
col <- pivot_longer(tab,cols=2:k,names_to = "i")  %>% 
          filter(i !=j) %>% 
           arrange(i,j) %>%
          select(i, j, Fij=value) 

col<-data.table(col)
Fi<-col[,.(Fi=sum(Fij)),.(i)]
Fj<-col[,.(Fj=sum(Fij)),.(j)]
Flows<-col %>% left_join(Fi) %>% left_join(Fj)

## (D) Estimate values under random assumption and 


mod <- glm(data=Flows, formula = Fij~i+j, family="poisson")
Flows$Eij <- mod$fitted.values



## (E) Derived matrix of observed and estimates

tmp <- Flows %>% as.data.frame() %>% 
       select(i,j,Fij) %>% pivot_wider(names_from = i, values_from = Fij) %>%
       arrange(j)
obs<- as.matrix(tmp[,-1])
row.names(obs)<-tmp$j
obs


tmp <- Flows %>% as.data.frame() %>% 
  select(i,j,Eij) %>% pivot_wider(names_from = i, values_from = Eij) %>%
  arrange(j)
est<- as.matrix(tmp[,-1])
row.names(est)<-tmp$j
round(est)

## Compute distance under gravity assumption (sqrt(est/obs))
alpha <-0.5
Dist<-(est/obs)**(1/alpha)

round(Dist,1)


#

mod<-smacofSym(delta = Dist)
mod$stress
plot(mod)
