library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(mapsf)
library(sf)
library(RColorBrewer)

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

saveRDS(hc,"data/hc.RDS")


x<- hc %>% group_by(where1) %>% summarise(tags=sum(tags), news=sum(news)) %>%
  mutate(tagnews = tags/news, tagspct=tags/sum(tags), newspct=news/sum(news))


### Heatmap



tab<- hc[,.(n=sum(news)),.(who,when)] %>% arrange(who) %>%
  arrange(when) %>% filter(when < as.Date("2020-01-01"))


years <- as.Date(c("2014-01-01", 
                 "2015-01-01",
                 "2016-01-01",
                 "2017-01-01",
                 "2018-01-01",
                 "2019-01-01",
                 "2020-01-01"))
g<-ggplot(tab) + aes(x=when, y=who, fill=n) + 
                geom_tile() +
                scale_fill_gradient(low = "white",high = "black") +
                geom_vline(xintercept=years, col="white",linetype = 3) +
                scale_x_date(name = "Time division by weeks (starting on monday)",breaks = years) +
                scale_y_discrete(name="Media", limits =rev) +
                ggtitle("Number of foreign news collected by week and media",
                        subtitle = "Source : Sirius database - based on data collected by Mediacloud"
                          ) +
               theme_light()
g
ggsave(plot=g,width = 20,height = 20,units = "cm",filename = "figs/timeline.pdf")

ggsave(plot=g,width = 20,height = 20,units = "cm",filename = "figs/timeline.jpg",dpi = 300)



### Top countries

tab<- hc[,.(n=sum(tags)),.(who,where1)] %>% 
         as.data.frame() %>% group_by(who) %>%
         filter(where1 !=substr(who,1,3))%>%
         mutate(pct = 100*n/sum(n),
                               rnk = rank(-pct)) %>%
          filter(rnk<10) %>%
          arrange(who,rnk) 
 
 tab$pct6<-cut(tab$pct,breaks=c(1,2.5,5,10,20,100))
 mypal<-rev(brewer.pal(name = "RdYlBu",n = 8))[3:8]
 
   g<-ggplot(tab) + aes(x=as.factor(rnk), y=who, fill=pct6) + 
     geom_tile(linewidth = 1) +
     scale_x_discrete(name = "Rank") +
    # scale_fill_brewer(name="RdYlBu")+
     scale_fill_manual(values = mypal)+
    # scale_fill_gradient(low = "orange",high = "lightyellow") +
     geom_text(aes(label = where1),cex=3)+
     scale_y_discrete(name="Media", limits =rev) +
     ggtitle("Most mentionned countries in foreign news by media",
                         subtitle = "Source : Sirius database - based on data collected by Mediacloud"
                 ) +
     theme_light()
 
   g
ggsave(plot=g,width = 20,height = 20,units = "cm",filename = "figs/top10.pdf")
 
ggsave(plot=g,width = 20,height = 20,units = "cm",filename = "figs/top10.jpg",dpi = 300)



### Location map
grat<-readRDS("geom/WORLD30.RDS")
wld<-readRDS("geom/world_riate.RDS")
mapmedia <- readRDS("data/mapmedia.RDS")
wld <- st_transform(map, st_crs(grat))
mapmedia<-st_transform(mapmedia, st_crs(grat))

mf_map(mapmedia, type="base")
mf_map(grat, type="base", lty=3,col="gray98",add=T)
mf_map(wld, type = "typo", var ="SUBREGION_UN" ,
       add=T, alpha=0.3,leg_pos=NA,
       leg_title = "UN subregions")
mf_map(mapmedia, type="base",col="red",add=T)
#mf_label(mapmedia, var = "code",q = 2,
#         overlap = F,
#         col="red",
#         cex=0.6, 
#         halo=T)

mf_layout("Location of the 27 media of the Sirius database",
          credits = "",
          frame=T,
          arrow=F, 
          scale=F)


pdf("figs/locmedia.pdf",width=8, height=8)
mf_map(mapmedia, type="base")
mf_map(grat, type="base", lty=3,col="gray98",add=T)
mf_map(wld, type = "typo", var ="SUBREGION_UN" ,
       add=T, alpha=0.3,leg_pos=NA,
       leg_title = "UN subregions")
mf_map(mapmedia, type="base",col="red",add=T)
mf_label(mapmedia, var = "code",q = 2,
         overlap = F,
         col="red",
         cex=0.6, 
         halo=T)

mf_layout("Location of the 27 media of the Sirius database",
          credits = "",
          frame=T,
          arrow=F, 
          scale=F)
dev.off()


jpeg("figs/locmedia.jpg",width=15, height=15,units = "cm",res=300)
mf_map(mapmedia, type="base")
mf_map(grat, type="base", lty=3,col="gray98",add=T)
mf_map(wld, type = "typo", var ="SUBREGION_UN" ,
       add=T, alpha=0.3,leg_pos=NA,
       leg_title = "UN subregions")
mf_map(mapmedia, type="base",col="red",add=T)
mf_label(mapmedia, var = "code",q = 2,
         overlap = F,
         col="red",
         cex=0.6, 
         halo=T)

mf_layout("Location of the 27 media of the Sirius database",
          credits = "",
          frame=T,
          arrow=F, 
          scale=F)
dev.off()