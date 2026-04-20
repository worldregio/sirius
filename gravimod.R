library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(mapsf)
library(sf)

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


# Filter carto
map<-readRDS("geom/world_riate.RDS")


# Extract name and code
codename<-map %>% st_drop_geometry() %>% select(ISO3, NAMEen)
mystates<-names(table(map$ISO3))


hc <- hc %>% filter(where1 %in% mystates, 
                    where2 %in% mystates,
                    where1 !="PRI",
                    where2 != "PRI")

#hc<-hc %>% filter(substr(who,1,3)!=where1)

df<-hc[,.(Fijt=sum(tags)),.(i=who, j=where1,t=when)]
dfi<-hc[,.(Fit=sum(tags)),.(i=who, t=when)]
dfj<-hc[,.(Fjt=sum(tags)),.(j=where1, t=when)]
df <- df %>% left_join(dfi) %>% left_join(dfj)

df2 <- df %>% filter(Fit>100)



q <-df2[,.(n=.N>0), .(i,t)][,.(nb=sum(n)),.(t)]
tab<-df2[,.(n=.N),.(j,t)]
tab<- tab %>% left_join(q) %>% mutate(score = 100*n/nb)

avg<-tab[,.(avg=mean(score), sd=sd(score)),.(j)] %>% mutate(cv=sd/avg)

ggplot(tab %>% filter(j=="TUR")) +aes(x=t, y=score) + 
  geom_bar(stat="identity",) +
  geom_smooth()



