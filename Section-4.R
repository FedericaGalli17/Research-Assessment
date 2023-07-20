library(tidyverse)
library(latex2exp)
source("script-data-wrangling/funzioni-ispd.R")

ispd_stats <- read_rds("Rdata/ispd_stats.Rds")
ssd <- read_rds("Rdata/ssd.Rds")
ssd_dip <- read_rds("Rdata/ssd-dip.Rds")
dipecc1 <- read_rds("Rdata/dipecc18-22.Rds")
dipecc2 <- read_rds("Rdata/dipecc23-27.Rds")

ispd <- bind_rows(
  purrr::map(
    ispd_stats, ~.$ispd
  )
)
ispd <- ispd %>% filter(NP_d > 50)


names(ssd) <- c("SSD","v_ssd", "NP_ssd", "I_ssd", "percA_ssd", "percB_ssd", "percC_ssd", "percD_ssd", "percE_ssd", "percF_ssd", "percNA_ssd", "mVP_ssd", "sd_ssd", "v_stdA_ssd", "v_stdB_ssd", "v_stdC_ssd", "v_stdD_ssd", "v_stdE_ssd", "v_stdF_ssd", "votoA_ssd", "votoB_ssd", "votoC_ssd", "votoD_ssd", "votoE_ssd", "votoF_ssd", "area")

ssddip<-merge(ssd_dip,ssd,by="SSD")

ssddip$v_std=ssddip$n_A*ssddip$v_stdA_ssd+ssddip$n_B*ssddip$v_stdB_ssd+ssddip$n_C*ssddip$v_stdC_ssd+ssddip$n_D*ssddip$v_stdD_ssd+ssddip$n_E*ssddip$v_stdE_ssd+ssddip$n_F*ssddip$v_stdF_ssd

#number of products by dep
ssddip<-ssddip %>% group_by(dip_key) %>% mutate(np_dip=sum(NP_sd))
#overall score by dep 
ssddip<-ssddip %>% group_by(dip_key) %>% mutate(v_dip=sum(v))
#mean score dep
ssddip<-ssddip %>% group_by(dip_key) %>% mutate(I_dip=v_dip/np_dip)
#standardized score dep
ssddip<-ssddip %>% group_by(dip_key) %>% mutate(v_std_dip=sum(v_std)/sqrt(np_dip))
#ispd poggi
ssddip<-ssddip %>% group_by(dip_key) %>% mutate(ispd=pnorm(v_std_dip)*100)
#ispd semi-integer discretized
ssddip$ispd_d=round(ssddip$ispd)

#ispd by sds and dep
stats <- bind_rows(
  purrr::map(
    ispd_stats, ~.$ispd_ssd
  )
)
stats = subset(stats, select = c("dip_key","SSD","P_inf_sec"))
ssddip<-as_tibble(merge(ssddip,stats,by=c("SSD","dip_key")))
ssddip<-ssddip %>% group_by(dip_key) %>% mutate(ispd_star=sum((NP_sd/np_dip)*P_inf_sec*100))
ssddip$xmediod<-ssddip$v_std_dip/sqrt(ssddip$np_dip)
ssddip$xmediosd<-ssddip$v_std/ssddip$NP_sd
ssddip$dif<-ssddip$ispd_d - ssddip$ispd_star

#R by area and dep
ssddip<-ssddip %>% group_by(area) %>% mutate(I_area = sum(I * NP_sd) / sum(NP_sd))
ssddip<-ssddip %>% group_by(area,dip_key) %>% mutate(I_diparea=sum(I * NP_sd) / sum(NP_sd))
ssddip$r_diparea <- ssddip$I_diparea / ssddip$I_area
ssddip <- ssddip %>% group_by(dip_key) %>% mutate(r_dip=sum(NP_sd*r_diparea)/sum(NP_sd))

#R by sds and dep
ssddip$r_ssddip=ssddip$I/ssddip$I_ssd
ssddip<-ssddip %>% group_by(dip_key) %>% mutate(r_dipstar=sum((NP_sd/np_dip)*r_ssddip))

#IRAS by sds and dep
ssddip$iras_ssddip=ssddip$r_ssddip*(ssddip$NP_sd/ssddip$NP_ssd)
ssddip$totn=sum(ssddip$NP_sd)
ssddip$iras_dip=ssddip$r_dipstar*(ssddip$np_dip/ssddip$totn)

datadip <- ssddip %>% distinct(dip_key,.keep_all = TRUE)
datadip = subset(datadip, select = c("dip_key","np_dip","v_dip","I_dip","xmediod","v_std_dip","ispd","ispd_d","ispd_star","r_dip","r_dipstar","iras_dip"))

soglia <- 50


#Table 4: ISPD computation for a generic department ----
quale1 <- grep("Udine-Scienze degli Alimenti", ssddip$dip_key)
quale2 <- grep("Bologna-Scienze Statistiche “Paolo Fortunati”", ssddip$dip_key)

dip1<-ssddip[quale1,]
dip2<-ssddip[quale2,]

tab1<-select(dip1,c(SSD,NP_sd,v,I,NP_ssd,I_ssd,sd_ssd,v_std,xmediosd,v_std_dip,xmediod,ispd_d,ispd_star,r_dip,r_dipstar,I_dip))
tab2<-select(dip2,c(SSD,NP_sd,v,I,NP_ssd,I_ssd,sd_ssd,v_std,xmediosd,v_std_dip,xmediod,ispd_d,ispd_star,r_dip,r_dipstar,I_dip))

tab1$dip_key<-"DEP-1"
tab1$SSD<-paste0("SDS-",1:nrow(tab1))
tab2$dip_key<-"DEP-2"
tab2$SSD<-paste0("SDS-",4+1:nrow(tab2))

tab1[nrow(tab1) + 1, 1] <- "Sum"
tab1$NP_sd[(nrow(tab1))] <- 
  sum(tab1$NP_sd[1:(nrow(tab1) - 1)])
tab1$NP_ssd[(nrow(tab1))] <- 
  sum(tab1$NP_ssd[1:(nrow(tab2) - 1)])

tab2[nrow(tab2) + 1, 1] <- "Sum"
tab2$NP_sd[(nrow(tab2))] <- 
  sum(tab2$NP_sd[1:(nrow(tab2) - 1)])
tab2$NP_ssd[(nrow(tab2))] <- 
  sum(tab2$NP_ssd[1:(nrow(tab2) - 1)])

names(tab1)<-c("DIP","SDS","P_{sd}","v_{sd}","$overline{v}_{sd}$","P_{s}","$overline{v}_{s}$","$sigma_{s}$","$tilde{v}_{sd}&","$overline{x}_{sd}$","$tilde{v}_{d}$","$overline{x}_{d}$","ISPD","ISPD$^*$","R","R$^*$","I")
names(tab2)<-c("DIP","SDS","P_{sd}","v_{sd}","$overline{v}_{sd}$","P_{s}","$overline{v}_{s}$","$sigma_{s}$","$tilde{v}_{sd}&","$overline{x}_{sd}$","$tilde{v}_{d}$","$overline{x}_{d}$","ISPD","ISPD$^*$","R","R$^*$","I")

tab<-rbind(tab1,tab2)

print(xtable::xtable(tab, digits = 2), 
      include.rownames = F)

#Figure 5: histograms ispd/ispdstar/r/rstar/I ----
p1 <- datadip %>% 
  filter(np_dip > soglia) %>% 
  ggplot(aes(x = ispd_d)) + 
  geom_histogram(binwidth = 1) +
  xlab("ISPD") + ylab("") +
  ggtitle("ISPD") + theme_bw() 

p2 <- datadip %>% 
  filter(np_dip > soglia) %>% 
  ggplot(aes(x = ispd_star)) + 
  geom_histogram(binwidth = 1) +
  xlab("ISPD*") + ylab("") +
  ylim(c(0, 150)) +
  ggtitle("ISPD*") + theme_bw()

p3 <- datadip %>% 
  filter(np_dip > soglia) %>% 
  ggplot(aes(x = r_dipstar)) + 
  geom_histogram(bins = 100) +
  xlab("R*") + ylab("") +
  ylim(c(0, 150)) +
  ggtitle("R*") + theme_bw()

p32 <- datadip %>% 
  filter(np_dip > soglia) %>% 
  ggplot(aes(x = r_dip)) + 
  geom_histogram(bins = 100) +
  xlab("R") + ylab("") +
  ylim(c(0, 150)) +
  ggtitle("R") + theme_bw()

p4 <- datadip %>% 
  filter(np_dip > soglia) %>% 
  ggplot(aes(x = I_dip)) + 
  geom_histogram(binwidth = .01) +
  xlab("I") + ylab("") + xlim(c(0,1)) +
  ylim(c(0, 150)) +
  ggtitle("I") + theme_bw()

gridExtra::grid.arrange(p1,p2,p4,p3,p32, nrow = 2)
p <- gridExtra::arrangeGrob(p1,p2,p4,p3,p32, nrow = 2)
ggsave(file="plots-paper/ispd_vs_I.pdf", p, height = 4, width = 8)


#Figure 6: ispd vs ispdstar ---- 
soglie <- c(-0.1,5,95,100.1)
datadip$classe_ispd <- as.numeric(cut(datadip$ispd_d,soglie))

ps <- datadip %>% 
  filter(np_dip > soglia) %>% 
  filter(ispd > 98 | ispd < 2) %>%
  ggplot() + 
  geom_segment(aes(x = 0, y = ispd_d, xend = 1, 
      yend = ispd_star), linewidth = .1, 
      col = "lightgrey") +
  geom_point(aes(x = 0, y = ispd, col = as.factor(classe_ispd)), size = .5, show.legend = FALSE) + 
  geom_point(aes(x = 1, y = ispd_star, col = as.factor(classe_ispd)), size = .5, show.legend = FALSE) + 
  xlab("") +
  scale_y_continuous("ISPD", sec.axis = sec_axis(~ . * 1, name = "ISPD*"))+
  theme_bw()

ggsave(file="plots-paper/alternativa.pdf", ps, height = 4, width = 6)


#IMPLICATIONS OF FUNDING
#Table 5: ISPD vs project variability ----
#departments of excellence 2018-2022
dipecc1$pispd<-0.7*dipecc1$Ispd
dipecc1$tot<-dipecc1$pispd+dipecc1$F4

varispd1<-var(dipecc1$pispd)
varispd1
varpunteggio1<-var(dipecc1$F4)
varpunteggio1
cov1<-cov(dipecc1$pispd,dipecc1$F4)
cov1
vartot1<-var(dipecc1$tot)
vartot1
check1<-varispd1+varpunteggio1+2*cov1
check1

hist(dipecc1$pispd,xlim=c(0,70))
hist(dipecc1$F4,xlim=c(0,30))
hist(dipecc1$tot,xlim=c(0,100))

plot(dipecc1$pispd, dipecc1$F4)
cov(dipecc1$F4, dipecc1$pispd)

summary(lm(dipecc1$F4~dipecc1$pispd))

#departments of excellence 2023-2027
dipecc2$pispd<-0.7*dipecc2$ispd
dipecc2$tot<-dipecc2$pispd+dipecc2$progetto

varispd2<-var(dipecc2$pispd)
varispd2
varpunteggio2<-var(dipecc2$progetto)
varpunteggio2
cov2<-cov(dipecc2$pispd,dipecc2$progetto)
cov2
vartot2<-var(dipecc2$tot)
vartot2
check2<-varispd2+varpunteggio2+2*cov2
check2

hist(dipecc2$pispd,xlim=c(0,70))
hist(dipecc2$progetto,xlim=c(0,30))
hist(dipecc2$tot,xlim=c(0,100))

#Figure 7: ispd-project departments of excellence ----
p1 <- ggplot(dipecc1, aes(Ispd, F4)) +
  geom_count() + #coord_fixed()+
  theme_bw()+scale_size_area() +
  theme(legend.position = "none") +
  ylab("Project Score") + xlab("ISPD") + ggtitle("2018-2002")
p2 <- ggplot(dipecc2, aes(ispd, progetto)) +
  geom_count() + #coord_fixed()+
  theme_bw()+scale_size_area() +
  theme(legend.position = "none") +
  ylab("Project Score") + xlab("ISPD") + ggtitle("2023-2027")
ggExtra::ggMarginal(p1, type = "histogram")
ggExtra::ggMarginal(p1 + geom_smooth(), type = "histogram")
ggExtra::ggMarginal(p2, type = "histogram")
ggExtra::ggMarginal(p2 + geom_smooth(), type = "histogram")
gridExtra::grid.arrange(p1,p2, nrow = 1)
p <- gridExtra::arrangeGrob(p1,p2, nrow = 1)
ggsave(file="plots-paper/vardipecc.pdf", p, height = 4, width = 8)

#Table 6: ISPD variability 350 dep.----
#vqr 2011-2014
datadip2<-datadip[datadip$np_dip>soglia,]
best350<-rank(datadip$ispd)
mean(datadip$ispd[best350>540])
mean(datadip$ispd_star[best350>540])
mean(datadip$r_dip[best350>540])
mean(datadip$r_dipstar[best350>540])
mean(datadip$I_dip[best350>540])

var(datadip$ispd_d[best350>540])
var(datadip$ispd_star[best350>540])
var(datadip$r_dip[best350>540])
var(datadip$r_dipstar[best350>540])
var(datadip$I_dip[best350>540])

sd(datadip$ispd_d[best350>540])/mean(datadip$ispd_d[best350>540])
sd(datadip$ispd_star[best350>540])/mean(datadip$ispd_star[best350>540])
sd(datadip$r_dip[best350>540])/mean(datadip$r_dip[best350>540])
sd(datadip$r_dipstar[best350>540])/mean(datadip$r_dipstar[best350>540])
sd(datadip$I_dip[best350>540])/mean(datadip$I_dip[best350>540])


#350 dip ecc
grad_17_22 <- readxl::read_xlsx("dati-vqr-11-14/Gradutaoria_DdE_2017-2022.xlsx")
grad_23_27 <- readxl::read_xlsx("dati-vqr-11-14/Gradutaoria_DdE_2023-2027.xlsx")

mean(sort(grad_17_22$ISPD))
mean(sort(grad_23_27$ISPD))

var(sort(grad_17_22$ISPD))
var(sort(grad_23_27$ISPD))

sd(sort(grad_17_22$ISPD))/mean(sort(grad_17_22$ISPD))
sd(sort(grad_23_27$ISPD))/mean(sort(grad_23_27$ISPD))

#Figure 8: histogram dip ecc -----
tb <- tibble(
anno = c(rep("ISPD: 2018-2022", 350), rep("ISPD: 2023-2027", 350)),
 ispd =  c(grad_17_22$ISPD[-(351:352)],
           grad_23_27$ISPD)
)

r<-ggplot(tb, aes(x = ispd)) +
  geom_histogram()+
  facet_grid(~anno)+
  xlab("ISPD") + ylab("") +theme_bw()

ggsave(file="plots-paper/istogrammidipecc.pdf", r, height = 4, width = 8)


r01<-grad_17_22%>% 
  ggplot(aes(x = ISPD)) + 
  geom_histogram() +
  xlab("ISPD") + ylab("") + ylim(c(0,200))+
  ggtitle("2017-2022 top350 ranking: ISPD") + theme_bw()

r02<-grad_23_27%>% 
  ggplot(aes(x = ISPD)) + 
  geom_histogram() +
  xlab("ISPD") + ylab("") + ylim(c(0,200))+
  ggtitle("2017-2022 top350 ranking: ISPD") + theme_bw()

r1<-datadip[best350>540,]%>% 
  ggplot(aes(x = ispd_d)) + 
  geom_histogram(binwidth=1) +
  xlab("ISPD") + ylab("") + ylim(c(0,200))+
  ggtitle("VQR 2011-2014 top350: ISPD") + theme_bw()


r2<-datadip[best350>540,]%>% 
  ggplot(aes(x = ispd_star)) + 
  geom_histogram(binwidth=1) +
  xlab("ISPD*") + ylab("") +
  ylim(c(0, 35)) +
  ggtitle("VQR 2011-2014 top350: ISPD*") + theme_bw()

r3<-datadip[best350>540,]%>% 
  ggplot(aes(x =r_dip)) + 
  geom_histogram(binwidth = .01) +
  xlab("R") + ylab("") +
  ylim(c(0, 35)) +
  ggtitle("VQR 2011-2014 top350: R") + theme_bw()

r4<-datadip[best350>540,]%>% 
  ggplot(aes(x = r_dipstar)) + 
  geom_histogram(binwidth = .01) +
  xlab("R*") + ylab("") + 
  ylim(c(0, 35)) +
  ggtitle("VQR 2011-2014 top350: R*") + theme_bw()

r5<-datadip[best350>540,]%>% 
  ggplot(aes(x = I_dip)) + 
  geom_histogram(binwidth = .01) +
  xlab("I") + ylab("") +
  ylim(c(0, 35)) +
  ggtitle("VQR 2011-2014 top350: I") + theme_bw()


gridExtra::grid.arrange(r01,r02,r1,r2,r3,r4,r5, nrow = 3)
ii <- gridExtra::arrangeGrob(r01,r02,r1,r2,r3,r4,r5, nrow = 3)
ggsave(file="plots-paper/hist350.pdf", ii, height = 8, width = 10)






