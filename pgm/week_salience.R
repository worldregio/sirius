## Triangulation

library(dplyr)
library(tidyr)
library(data.table)



## Prepare data
hc <- readRDS("data/hc_20260824.RDS")
hc$when<-as.character(hc$when)
# Agregate where2
hc2 <- hc[,.(Fijt=sum(news)),.(i=who, j=where1, t=when)]

# Cast
x<-dcast(hc2, formula = i+j+t~.,drop = F,fill = 0)


  names(x)[4]<-"Fijt"
  
    # Eliminate impossible or same country
    Fijt<-x %>% filter(substr(i,1,3)!=j, !(i=="CHN_mopost" & j=="HKG"))
  
    # add Fit
    
    Fit<-Fijt[,.(Fit=sum(Fijt)),.(i,t)]
  Fijt <- Fijt %>% left_join(Fit)

  
    # add Fjt
    
    Fjt<-Fijt[,.(Fjt=sum(Fijt)),.(j,t)]
  Fijt <- Fijt %>% left_join(Fjt)

  
    # add Fij
    Fij<-Fijt[,.(Fij=sum(Fijt)),.(i,j)]
  Fijt <- Fijt %>% left_join(Fij)


### Filter week >50
hc<-Fijt %>% filter(Fit > 50)


# Compute interactions by week
hc$tag<-hc$Fijt > 0  
int<-hc[,.(flag=as.numeric(Fijt>0), tot=1),.(i,j,t)]


# Summarise flag by media
t1 <- int[,.(x=sum(flag),n=sum(tot)),.(i,j)]
t1$p<-t1$x/t1$n
tab<-dcast(t1,formula = j~i, value.var = "p")
mat<-as.matrix(tab[,-1])
rownames(mat)<-tab$j
mat[is.na(mat)]<-1
x<-apply(mat,1,mean)
mat2<-mat[x>0.01,]

library(FactoMineR)
library(explor)

afc<-CA(mat2,ncp = 20)
cah<-HCPC(afc,cluster.CA = "rows", ncp=7)
