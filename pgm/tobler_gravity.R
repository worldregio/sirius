library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(mapsf)


## Prepare data
hc <- readRDS("data/hc.RDS")
hc$sel<-substr(hc$who,1,3)
tab<-hc[,.(Fij=(round(sum(news)))),.(i=sel, j=where1)]
tab2<-pivot_wider(tab,names_from = i, values_from = Fij, values_fill = 0) %>% as.data.frame()

col <- pivot_longer(tab2,cols=2:28,names_to = "i")  %>% 
          filter(i !=j) %>% 
           arrange(i,j) %>%
          select(iso3_i=i, iso3_j=j, Fij=value) 

## Anonymize

code <- function(text = "USA") {
  t <- as.character(toupper(encrypt(text,shift=1) ))
 t <- paste0(substr(t,3,3), substr(t,2,2), substr(t,1,1))
 
 return(t)
}

col$i <- as.character(lapply(col$iso3_i,code))
col$j <- as.character(lapply(col$iso3_j, code))


## mod


mod <- glm(Fij~i+j, family="poisson", data=col)
col$Eij <-mod$fitted.values
col$Dij <- sqrt(col$Eij/col$Fij)     



sel <- col %>% filter(Fij >19)



### Smacoff example
library(smacof)
data("GOPdtm")
grav <- gravity(GOPdtm, lambda=2)
grav$gravdiss
res<-mds(grav$gravdiss)





plot(res)
