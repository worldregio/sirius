
base<-"/Users/claudegrasland1/Documents/cg/data/sirius/sirius/"

media <- list.files("sirius")




for (i in 1:27) {
  med<-media[i]
  print(med)
  chemin<-paste0(base,med,"/dt_int.RDS")
  x<-readRDS(chemin)
  chemin<-paste0("sirius/",med,"/dt_int.RDS")
  saveRDS(x,chemin)
  chemin<-paste0(base,med,"/hc_int.RDS")
  x<-readRDS(chemin)
  chemin<-paste0("sirius/",med,"/hc_int.RDS")
  saveRDS(x,chemin)
}

