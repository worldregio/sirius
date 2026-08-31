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

library(tidyr)
x<-hc %>% filter(hc$when ==as.Date("2014-01-27"))

y <- x[,.(tags= sum(news)),.(who,where1)]
z<-pivot_wider(y,names_from = where1,values_from = tags ,values_fill = 0)
w<-z %>% pivot_longer(cols = 2:dim(z)[2]) %>% arrange(who, name)
names(w)<-c("i","j","Fij")
w <- w %>% filter(substr(i,1,3) != j)
pois <- glm(data=w, formula = Fij~i+j)
w$Eij <- pois$fitted.values
w$Chi2ij <-((w$Fij-w$Eij)**2)/w$Eij
sel <- w %>% filter(Fij >4, Chi2ij>3.84)
