library(dt)
library(dplyr)
x<-readRDS("sirius/IDN_republ/hc_int.RDS") %>% 
    filter(where1 %in% c("USA", "SYR","RUS"),
           where2 %in% c("USA","SYR","RUS"),
           when == "2018-04-09")


library(data.table)
library(dplyr)
library(sf)
library(cartogram)
library(mapsf)
media<-"IDN_republ"
hc<-readRDS(paste0("sirius/",media,"/hc_int.RDS")) %>%  
  filter(where1 !=substr(media,1,3),
         when == "2018-04-09")
  
nbnews<-sum(hc$news)
tab<-hc[,.(nb=sum(news)),.(ISO3=where1)] %>% 
  arrange(-nb) %>% 
  mutate(sal = 100*nb/nbnews)
map<-readRDS("geom/world_riate.RDS")
grat<-st_read("geom/grat30.geojson")
map<-st_transform(map,st_crs(grat))
wld<-left_join(map,tab)
wld2 <-wld %>% filter(is.na(sal)==F)

cart<-cartogram_dorling(x=wld2,weight="sal",k = 1)
png(paste0("map_syria.png"),width = 500, height=500)
mf_map(cart, type="base",col=NA)
mf_map(grat, type="base", lty=3,col="gray90", add=T)
mf_map(wld, type = "base",add=T, col="white", lwd=0.2)
mf_map(cart, type="base",col="red", add=T, border="white", lwd=0.4)
mf_label(cart, var = "ISO3",cex=1.4*sqrt(cart$sal/max(cart$sal)), col="white")
mf_layout(title = "",
          credits = "(c) Sirius database - Source : MediaCloud",
          frame=T,
          arrow=F, 
          scale=F)
dev.off()



#### NETWORK

source("media/pgm_network.R")

hc<-readRDS(paste0("sirius/",media,"/hc_int.RDS")) %>%  
  filter(where1 !=substr(media,1,3),
         when == "2018-04-09")


hc<-hc %>% filter(where1 !=substr(who,1,3),
                  where2 !=substr(who,1,3))

hc<-hc_filter(don = hc,
              wgt = "tags",
              where1 = "where1",
              where2 = "where2",
              where1_exc = c("_no_"),
              where2_exc = c("_no_"),
              self = FALSE
)

int <- build_int(don = hc,
                 s1=1,
                 s2=1,
                 n1=1,
                 n2=1,
                 k=0)
int$Fij<-round(int$Fij)

mod<-rand_int(int,
              resid = TRUE,
              diag = FALSE)

network<- geo_network(mod,
                      size = "Fij",
                      minsize = 1,
                      test = "Fij",
                      mintest = 1)
network
myfile<-paste0("networks/",media,"_network.html")
saveRDS(network, myfile)