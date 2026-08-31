library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(mapsf)

hc <- readRDS("data/hc.RDS")
guest <- "HUN"

hc$guest <- as.numeric(hc$where1==guest)



df<-hc[,.(n=sum(news),x=sum(guest, na.rm=T)),.(who, when)]
df$ok<-df$x>0
q <- df[,.(n=.N,o=sum(ok)),.(when)] %>% 
  arrange(when) %>% 
  mutate(p=o/n,
         logit = p/(1-p))
plot(q$when,q$p, type="l",col="red")
library(tidyquant)
ggplot(q) +aes(x=when, y=p) + 
            geom_line(col="red") +
            geom_ma(ma_fun = SMA, n = 12, col="blue",lty=1)+
            geom_ma(ma_fun = SMA, n = 52, col="black",lty=1)+  
            scale_y_continuous(limits = c(0,1))


df$ok <-as.numeric(df$x>0)

# Overall probability
mean(df$ok)

# Media probability
tab<-table(df$who, df$ok)
tab2<-prop.table(tab,1)
tab

whospeak <- data.frame(who=rownames(tab2),prob=tab2[,2]) %>% arrange(-prob)
whospeak

# Time probabilitu
tab<-table(df$when, df$ok)
tab2<-prop.table(tab,1)

whenspeak <- data.frame(when=as.Date(rownames(tab2)),prob=tab2[,2]) %>% 
  arrange(when) %>%
  mutate(logit = prob/(1-prob))
plot(whenspeak$when, whenspeak$prob, type="h",  ylim = c(0,1), col="red")


mod <- glm(formula = ok ~ log(n)+who+as.factor(when), family="binomial", data=df)
summary(mod)

coeff<-mod$coefficients
coeff2 <- exp(coeff)/(1+exp(coeff))
plot(coeff2[26:389], type="l")
round(coeff2[1:25],3)
