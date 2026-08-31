

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(mapsf)
library(sf)


## Prepare data
hc <- readRDS("data/hc.RDS")

start<-as.Date("2013-07-01")
end<-as.Date("2015-07-01")



hc <- hc%>% filter(when > start, when < end)

col<-hc[,.(Fij=(round(sum(tags)))),.(i=where1, j=where2)]


## Anonymize

code <- function(text = "USA") {
  t <- as.character(toupper(encrypt(text,shift=1) ))
  t <- paste0(substr(t,3,3), substr(t,2,2), substr(t,1,1))
  
  return(t)
}

#col$i <- as.character(lapply(col$i,code))
#col$j <- as.character(lapply(col$j, code))




tab<-pivot_wider(col,names_from = i, values_from = Fij, values_fill = 0) %>% as.data.frame()
k<-dim(tab)[2]
col <- pivot_longer(tab,cols=2:k,names_to = "i")  %>% 
          filter(i !=j) %>% 
           arrange(i,j) %>%
          select(i, j, Fij=value) 

## Exclude
#excl<-c("USA","CHN","RUS")
#col <- col %>% filter(!i %in% excl, !j %in% excl)

## Add margins
col<-data.table(col)
Fi<-col[,.(Fi=sum(Fij)),.(i)]
Fj<-col[,.(Fj=sum(Fij)),.(j)]
col<-col %>% left_join(Fi) %>% left_join(Fj)

## Filter

sel <- col %>% filter(Fi >9, Fj > 9, Fij>2) %>%
    mutate(Dij = sqrt((Fi*Fj)/Fij)) %>% select(i,j,Dij)



tab<-pivot_wider(sel,names_from = i, values_from = Dij) %>% as.data.frame() %>% arrange(j)









mat<-as.matrix(tab[,-1])
row.names(mat) <- tab$j



## Classical MDS
x<-readRDS("geom/world_riate.RDS")
x$reg<-as.factor(x$SUBREGION_UN)
levels(x$reg) <-c( "Asia-Pacifica",     
                  "Asia-Pacifica"      ,             
                  "Asia-Pacifica"     ,            
                  "Europe"      ,           
                  "America",
                  "Asia-Pacifica"                    ,
                  "Asia-Pacifica"                    ,
                  "N. Africa & Middle East" ,               
                   "America"              , 
                   "Europe"                ,
                  "Asia-Pacifica"               ,     
                  "Asia-Pacifica"          ,
                  "Asia-Pacifica"          ,   
                  "Europe"              ,  
                  "Sub-Saharan Africa"   ,          
                  "N. Africa & Middle East"  ,                 
                  "Europe"  )
x$reg[x$ISO3 %in% c("IRN","PAK","AFG")]<-"N. Africa & Middle East" 

x<-x %>%
  st_drop_geometry() %>% 
select(code=ISO3, reg)

res<-mds(mat,ndim = 2, itmax=1000)

mypal<-c("green4","blue","red","brown","orange")

coo <- as.data.frame(res$conf)
coo$code<-row.names(coo)
coo <- merge(coo, Fi, by.x="code", by.y="i")
coo <-coo %>% left_join(x)


g1<- ggplot(coo) + aes(x=D1, y=D2, label=code, size = Fi,col=reg) + 
  geom_point(show.legend = F) +
  geom_label_repel(size=2,col="black") +
  scale_color_manual(values=mypal) +
  coord_fixed(ratio=1) +
  theme_light() 

g1
#ggsave(plot=g1,filename = "mds_afr.pdf",width = 9,height=7)
