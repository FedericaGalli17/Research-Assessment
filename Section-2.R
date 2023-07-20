library(tidyverse)
library(latex2exp)
source("script-data-wrangling/funzioni-ispd.R")

ispd_stats <- read_rds("Rdata/ispd_stats.Rds")
ssd <- read_rds("Rdata/ssd.Rds")
ssd_dip <- read_rds("Rdata/ssd-dip.Rds")

ssd$rank_I <- 369 - rank(ssd$mVP_s)

#Figure 1: caterpillar ----
c1 <- ssd %>% 
  ggplot() +
  geom_segment(aes(x = rank_I, y = (mVP_s + 2 * sd_s / sqrt(NP_s)),
                   xend = rank_I, yend = (mVP_s - 2 * sd_s / sqrt(NP_s))), col = "lightgrey") +
  geom_point(aes(x = rank_I, y = mVP_s), col = 1) +
  geom_hline(aes(yintercept = sum(NP_s * mVP_s) / sum(NP_s)), col = 1, lty = 2) +
  theme_bw() + xlab("") + 
  ylab(TeX(r"( $\bar{v}_s$ )"))
ggsave("plots-paper/caterpillar_ssd.pdf", c1, width = 8, height = 4)


#Figure 2: caterpillar by area ----
c2 <- ssd %>% 
  ggplot() +
  geom_segment(aes(x = rank_I, y = (mVP_s + 2 * sd_s / sqrt(NP_s)),
                   xend = rank_I, yend = (mVP_s - 2 * sd_s / sqrt(NP_s))), col = "lightgrey") +
  geom_point(aes(x = rank_I, y = mVP_s), col = 1) +
  geom_hline(aes(yintercept = sum(NP_s * mVP_s) / sum(NP_s)), col = 1, lty = 2) +
  theme_bw() + xlab("") + ylab(TeX(r"( $\bar{v}_s$ )")) + facet_wrap(~area)
ggsave("plots-paper/caterpillar_ssd_aree.pdf", c2, width = 8, height = 6)


#Table 1: variance decomposition ----
v_bar <- sum(ssd$v) / sum(ssd$NP_s) 
sum(ssd$v) / sum(ssd$NP_s) 
sum(ssd_dip$v) / sum(ssd_dip$NP_sd) 
sum(ssd$I * ssd$NP_s) / sum(ssd$NP_s) 
sum(ssd$mVP_s * ssd$NP_s) / sum(ssd$NP_s) 

var_between <- sum((ssd$mVP_s - v_bar)^2 * ssd$NP_s) / sum(ssd$NP_s)
var_within <- sum(ssd$NP_s * (ssd$sd_s)^2) / sum(ssd$NP_s)
var_tot <- var_between+var_within

c(var_between, var_within) / var_tot
cul <- ssd %>% 
  rowwise(SSD) %>%
  summarise(
    v = sum(c_across(perc_A:perc_F)/100*
              (c_across(voto_A:voto_F) - v_bar)^2),
    NP_s = NP_s,
    sd2 = sd_s^2)

sum(cul$v * cul$NP_s) / sum(cul$NP_s)
var_between + var_within

var_between_area <-group_by(ssd,area) %>% summarise(BV=sum((mVP_s - v_bar)^2 *NP_s) / sum(NP_s))
var_within_area <- group_by(ssd,area) %>% summarise(WV= sum(NP_s * (sd_s)^2) / sum(NP_s))
vmean_area <- group_by(ssd,area) %>% summarise(v= sum(v) / sum(NP_s))
stat_area <- data.frame(area=var_between_area$area,vmean=vmean_area$v,BV=var_between_area$BV,WV=var_within_area$WV)
stat_area$tot <- stat_area$BV+stat_area$WV
stat_area$percBV <- stat_area$BV/stat_area$tot*100
stat_area$percWV <- stat_area$WV/stat_area$tot*100
stat_area$checktot <- stat_area$percBV+stat_area$percWV
print(xtable::xtable(stat_area, digits = 3), 
      include.rownames = F)






