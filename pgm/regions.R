library(adespatial)
library(data.table)
library(quanteda)
library(dplyr)
library(tidyr)
library(FactoMineR)
library(explor)

# Load media
media<-"CAN_tostar"
lang="fr"
title_only=TRUE
t1<-as.Date("2013-07-01")
t2<-as.Date("2020-07-01")

myfile<-paste0("../sirius/sirius/",media,"/dt_int.RDS")
dt<-readRDS(myfile)

myfile2<-paste0("../sirius/sirius/",media,"/hc_int.RDS")
hc<-readRDS(myfile2)

# Create Data term matrix
qd<-corpus(dt,text_field = "states")
toks<-tokens(qd)
mat<-dfm(toks,tolower = F) %>%  dfm_group(week) %>% as.matrix()

# Use an extract
sel <- mat[, ]
n<-dim(sel)[1]

# coords 
coo<-matrix(c(1:n, rep(1,n)),n,2)


# 
sim <- dist.ldc(sel, method="hellinger")
dim(sim)

time_reg <- constr.hclust(sim, method="ward.D2", chron=TRUE,
                                coords=coo)
plot(time_reg, k=5, las=1, xlab="week",
     ylab="time", cex=1, lwd=0.1)


class_time <- as.factor(cutree(time_reg,k = 5))
class_time


# Analyze
hc$when5<-as.factor(hc$when)
levels(hc$when5)<-class_time
tab<- hc %>% group_by(when5, where1) %>% summarise(nb=round(sum(news)))
#tot <- hc %>% group_by(when5) %>% summarise(tot=round(sum(news)))
#tab <- tab %>% left_join(tot)%>% mutate(p=100*nb/tot)
res<-pivot_wider(tab,names_from = "when5",values_from = nb,values_fill = 0)


res2 <-as.data.frame(res[,-1])
row.names(res2)<-res$where1

prop<-round(100*prop.table(as.matrix(res2),2),1)

chi<-chisq.test(res2)
w<-round(chi$stdres,1)

## AFC

afc <- CA(res2,graph = F)
plot(afc,cex=0.7,)

## CAH

cah <-HCPC(afc,nb.clust = 5)
