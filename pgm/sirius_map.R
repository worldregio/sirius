library(sf)
library(dplyr)
library(mapsf)


gra<-st_read("geom/grat30.geojson")
pol<- st_crs(gra)
wld<-readRDS("geom/world_riate.RDS") %>% st_transform(pol)
sirius <-readRDS("geom/sirius.RDS")  %>% st_transform(pol)


png(filename="sirius_location.png",width = 800, height=800)
mf_map(sirius, type="base")
mf_map(gra, type="base",add=T, lty=2, col="white", lwd=0.2, border="black")
mf_map(wld, type="typo" ,
       var="REGION_UN", 
       col=NA, 
       lwd=0.2,
       alpha=0.5,
       leg_title = "UN regions",
       leg_pos = "topleft",
       add=T)
mf_label(sirius, type="base", col = "black", halo=T,bg = "white",lines = T,r = 0.3,
       var="code", overlap = F, cex=0.8,q = 3)
mf_layout(title="", frame=T, arrow=F, scale = F,
          credits = "Sirius database - 2026")
dev.off()
        

