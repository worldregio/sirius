

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(mapsf)
library(sf)
library(reshape2)
library(smacof)
library(leaflet)


## (A) Prepare data
base <- readRDS("data/hc.RDS")


start<-as.Date("2013-07-01")
end<-as.Date("2020-07-01")

mystates <-c("PSE","ISR","PRK","KOR","USA","CAN","ZAF","NGA","BRA","ARG",
             "JPN","FRA","DEU","RUS","UKR","TUR","IND","IDN","GBR","AUS","SYR","IRQ",
             "ESP","ITA","VNM","THA","POL","ROU","MEX","COL","CUB","ETH","EGY","SAU", 
             "CHN","TWN","YEM","LBN")


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
est

## Compute distance under gravity assumption (sqrt(est/obs))
alpha <-2
Dist<-(est/obs)**(1/alpha)


Dist<-as.dist(Dist)
Dist[Dist==Inf]<-quantile(Dist,0.95)
round(Dist,1)
#

mod<-smacofSym(delta = Dist,type="ordinal",ndim=3)
mod$stress
plot(mod)
mod$conf

mod3<-smacofSphere(delta=Dist, type='interval',
                   ndim = 3,
                   itmax=10000,
                   penalty = 100)
plot(mod3)
mod3$stress

# extract 3D coordinates
coo<-as.data.frame(mod3$conf)
class(coo)
(coo$D1)**2 + (coo$D2)**2 +(coo$D3)**2

# Visualize geography
geo<-coo
summary(coo)
geo$lon <- 180*geo$D1*3/2
geo$lat <- 90*geo$D2*3/2
geo$names<-row.names(geo)

# Leaflet view
map<-st_as_sf(geo, coords = c("lon","lat"))
map$lon<-geo$lon
map$lat<-geo$lat
leaflet(map) %>% addTiles() %>% addCircleMarkers(label = ~names)



#plotly view
library(plotly)
proj <- 'azimuthal equal area'
g <- list(showframe = TRUE,
          framecolor= toRGB("gray20"),
          coastlinecolor = toRGB("gray20"),
          showland = TRUE,
          landcolor = toRGB("gray50"),
          showcountries = TRUE,
          countrycolor = toRGB("white"),
          countrywidth = 0.2,
          projection = list(type = proj))



p<- plot_geo(map)%>%
  add_markers(x = ~lon,
              y = ~lat,
              #sizes = c(0, 250),
              #size = ~success,
              #             color= ~signif,
              #color = ~index,
              #colors= mycol,
              hoverinfo = ~names
            #  text = ~paste('Location: ',NAME,
            #                '<br /> Total news  : ', round(trial,0),
            #                '<br /> Topic news : ', round(success,0),
            #                '<br /> % observed  : ', round(estimate*100,2),'%',
            #                '<br /> % estimated : ', round(null.value*100,2),'%',
            #               '<br /> Salience : ', round(salience,2),  
            #               '<br /> p.value : ', round(p.value,4)))
            )%>%
  
  layout(geo = g,
         title = title)
p
