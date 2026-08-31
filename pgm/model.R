library(knitr)

## Global options
options(max.print="75")
opts_chunk$set(echo=FALSE,
               cache=FALSE,
               prompt=FALSE,
               tidy=FALSE,
               comment=NA,
               message=FALSE,
               warning=FALSE)


# Basic packages
library(dplyr)
library(data.table)
library(lubridate)

# Graphic packages
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(visNetwork)

# Spatial packages
library(sf)

# text mining packages
library(quanteda)
library(quanteda.textplots)
library(tidytext)
library(readtext)
library(stringr)
library(readr)


# Check media list
listmedia <- list.files("sirius")
k<-length(listmedia)

# Load first media
media<-listmedia[1]
hc<-readRDS(paste0("sirius/",media,"/hc_int.RDS"))

# Load other media
for (i in 2:k) {
  media<-listmedia[i]
  hc2<-readRDS(paste0("sirius/",media,"/hc_int.RDS"))
  hc<-rbind(hc,hc2)
}
rm(hc2)
class(hc)


hc$test<-(hc$where1=="UKR")

tip<-hc[,.(x=sum(test)>0, y=sum(news)),.(when,who)] %>% filter(y>50)
t<-table(tip$who,tip$x)
round(prop.table(t,1),2)

mod<-glm(data=tip, formula = x~who+as.factor(when), )
tab<-data.frame(x=mod$coefficients,y=names(mod$coefficients))
plot(tab$x[29:391],type="l")

exp(tab$x[2:28])

ggplot(tip, aes(x=when,y=n))+geom_bar(stat="identity")+geom_smooth()


