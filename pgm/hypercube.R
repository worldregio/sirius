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
  
    # Eliminate impossible
    Fijt<-x %>% filter(substr(i,1,3)!=j)
  
    # add Fit
    
    Fit<-Fijt[,.(Fit=sum(Fijt)),.(i,t)]
  Fijt <- Fijt %>% left_join(Fit)

  
    # add Fjt
    
    Fjt<-Fijt[,.(Fjt=sum(Fijt)),.(j,t)]
  Fijt <- Fijt %>% left_join(Fjt)

  
    # add Fij
    Fij<-Fijt[,.(Fij=sum(Fijt)),.(i,j)]
  Fijt <- Fijt %>% left_join(Fij)


### Check1 : Salience
hc<-Fijt %>% filter(Fit > 90)
hc$tag<-hc$Fijt > 0  

OKi<-hc[,.(tot=.N, hit = sum(tag)), .(j)]
OKi$sal <-100*OKi$hit/OKi$tot
OKi$rnk<-rank(-OKi$sal)  
plot(OKi$rnk, OKi$sal)

### Check2 : Time focus)

hc<-Fijt %>% filter(Fit > 90, j=="SYR")
hc$tag<-hc$Fijt > 0  
OKjt<-hc[,.(tot=.N, hit = sum(tag)), .(t)]
OKjt$sal <-100*OKjt$hit/OKjt$tot
OKjt$t<-as.Date(OKjt$t)

OKjt <- OKjt %>% arrange(t)
mod<-lm(OKjt$sal~OKjt$t)
summary(mod)
plot(OKjt$t, OKjt$sal, type="h")
abline(mod,col="red")



