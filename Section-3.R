library(tidyverse)
library(latex2exp)
source("script-data-wrangling/funzioni-ispd.R")

ispd_stats <- read_rds("Rdata/ispd_stats.Rds")
ssd <- read_rds("Rdata/ssd.Rds")
ssd_dip <- read_rds("Rdata/ssd-dip.Rds")

ispd <- bind_rows(
  purrr::map(
    ispd_stats, ~.$ispd
  )
)
ispd <- ispd %>% filter(NP_d > 50)

#Figure 3: ISPD MC approximation ----
#middle panel

#plot parameters
global_size <- 16

p1 <- ggplot(ispd, aes(x = ispd_poggi, 
                       y = ispd_poggi_mc)) +
  geom_abline(intercept = 0, slope = 1, 
              lwd = 1, col = "lightgrey") + 
  geom_point(cex = .75) + 
  theme_bw(base_size = global_size) +
  xlab(TeX(r"($ISPD$)")) + ylab(TeX(r"($ISPD_{MC}$)"))

#right panel
p2 <- ggplot(ispd, aes(x = log(NP_d), 
                       y = (ispd_poggi_mc - ispd_poggi))) +
  geom_point(cex = .75) + ylim(c(-1,1)) + theme_bw(base_size = global_size) +
  xlab(TeX(r"($\log(P_d)$)")) + 
  ylab(TeX(r"($ISPD-ISPD_{MC}$)"))

#left panel
quale <- 861
res <- calcola_ispd_mc(
  filter(ssd_dip, dip_key == dip_key[quale]), 
  ssd, n_sims = 500000, return_mc = TRUE)

tres <- tibble(ispd_mc = rowSums(res$dva_ssd) / sqrt(res$ispd$NP_d))

h1 <- tres %>% 
  ggplot(aes(x = ispd_mc)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 80, color="black", fill="lightgrey") +
  stat_function(fun = dnorm, col = 1, 
                linewidth = 1.2, linetype = 1) +
  # geom_vline(xintercept = res$ispd$VS_d, 
  #            linetype = 2, linewidth = 1.1) +
  xlab(TeX(r"($\tilde{v}_{(VD)}$)")) + ylab("") +
  theme_bw(base_size = global_size)

ggsave("plots-paper/gauss-accuracy.pdf", 
       gridExtra::grid.arrange(h1, p1, p2, nrow = 1),
       height = 3.5, width = 11.5)


#other elab
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

#ispd by sds and dip
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

#IRAS per ssd e dip
ssddip$iras_ssddip=ssddip$r_ssddip*(ssddip$NP_sd/ssddip$NP_ssd)
ssddip$totn=sum(ssddip$NP_sd)
ssddip$iras_dip=ssddip$r_dipstar*(ssddip$np_dip/ssddip$totn)

datadip <- ssddip %>% distinct(dip_key,.keep_all = TRUE)
datadip = subset(datadip, select = c("dip_key","np_dip","v_dip","I_dip","xmediod","v_std_dip","ispd","ispd_d","ispd_star","r_dip","r_dipstar","iras_dip"))

soglia <- 50


#Figure 4: ecdf----
datadip <- filter(datadip, np_dip > 49)
datadip$dim <- NA
cuts <- c(80, 150)
appo <- findInterval(datadip$np_dip, cuts) + 1
table(appo)


datadip$dim <- as.factor(appo)
levels(datadip$dim) <- c(paste0("< ", cuts[1]), "medi", paste0("> ", cuts[2]))
datadip$xmediod<-datadip$v_std_dip/datadip$np_dip

sel <- datadip$dim != "2"

t1 <- tibble(
  indicatore = "axbar",
  dim = datadip$dim[sel],
  valore = datadip$xmediod[sel]
)

t2 <- tibble(
  indicatore = "vtilde",
  dim = datadip$dim[sel],
  valore = datadip$v_std_dip[sel]
)

tb <- bind_rows(t1, t2)

lbs <- c(
  `vtilde` = "Standardised score",
  `axbar` = "Average score"
)

pecdf <- tb %>% filter(dim != "medi") %>% 
  ggplot(aes(x = valore, group = dim, color = dim)) +
  stat_ecdf(linewidth = 1) +
  scale_color_manual(values = c("black","grey")) +
  facet_grid(~indicatore, scales = "free_x",
             labeller = as_labeller(lbs)) +
  theme_bw(base_size = global_size)+ xlab("")+
  labs(color = TeX(r"( $P_d$ )")) +
  theme(strip.background = element_rect(
    color="black", fill="white",  linetype="solid",
  ),strip.text.x = element_text(size = 13))+ylab("")
pecdf

ggsave(file="plots-paper/ecdf.pdf", pecdf, height = 4, width = 10)

