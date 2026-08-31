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

saveRDS(hc,"data/hc.RDS")


### ggplot2



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
