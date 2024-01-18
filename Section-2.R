library(tidyverse)
library(latex2exp)
source("script-data-wrangling/funzioni-ispd.R")

ispd_stats <- read_rds("Rdata/ispd_stats.Rds")
ssd <- read_rds("Rdata/ssd.Rds")
ssd_dip <- read_rds("Rdata/ssd-dip.Rds")

ssd$rank_I <- 370 - rank(ssd$mVP_s)

#plot parameters
global_size <- 16

#Figure 1: caterpillar ----
c1 <- ssd %>% 
  ggplot() +
  geom_segment(aes(x = rank_I, y = (mVP_s + 2 * sd_s / sqrt(NP_s)),
                   xend = rank_I, yend = (mVP_s - 2 * sd_s / sqrt(NP_s))), 
               col = "darkgrey", linewidth = .4) +
  geom_point(aes(x = rank_I, y = mVP_s), cex = .6) +
  geom_hline(aes(yintercept = sum(NP_s * mVP_s) / sum(NP_s)), col = 1, lty = 2) +
  theme_bw(base_size = global_size) + xlab("Rank") + 
  ylab(TeX("$\\bar{v}_s$")) +
  scale_x_continuous(expand = expansion(0.02), breaks = c(1,100,200,300,369))
ggsave("plots-paper/caterpillar_ssd.pdf", c1, width = 8, height = 4)


#Figure 2: caterpillar by area ----
c2 <- ssd %>% 
  ggplot() +
  geom_segment(aes(x = rank_I, y = (mVP_s + 2 * sd_s / sqrt(NP_s)),
                   xend = rank_I, yend = (mVP_s - 2 * sd_s / sqrt(NP_s))), 
               col = "grey", linewidth = .4) +
  geom_point(aes(x = rank_I, y = mVP_s), cex = .6) +
  geom_hline(aes(yintercept = sum(NP_s * mVP_s) / sum(NP_s)), col = 1, lty = 2) +
  theme_bw(base_size = global_size) + xlab("Rank") + ylab(TeX(r"( $\bar{v}_s$ )")) + facet_wrap(~area)+
  scale_x_continuous(expand = expansion(0.02), breaks = c(100,200,300))
ggsave("plots-paper/caterpillar_ssd_aree.pdf", c2, width = 11, height = 8)


#Table 1: variance decomposition ----
v_bar <- sum(ssd$v) / sum(ssd$NP_s) 
var_between <- sum((ssd$mVP_s - v_bar)^2 * ssd$NP_s) / sum(ssd$NP_s)
var_within <- sum(ssd$NP_s * (ssd$sd_s)^2) / sum(ssd$NP_s)
var_tot <- var_between+var_within

var_between_area <-group_by(ssd,area) %>% 
  summarise(BV = sum((mVP_s - v_bar)^2 *NP_s) / sum(NP_s))
var_within_area <- group_by(ssd,area) %>% 
  summarise(WV = sum(NP_s * (sd_s)^2) / sum(NP_s))
vmean_area <- group_by(ssd,area) %>% summarise(v= sum(v) / sum(NP_s))
stat_area <- data.frame(area = var_between_area$area,
                        vmean = vmean_area$v,
                        BV = var_between_area$BV, 
                        WV = var_within_area$WV)
stat_area$tot <- stat_area$BV+stat_area$WV
stat_area$percBV <- stat_area$BV/stat_area$tot*100
stat_area$percWV <- stat_area$WV/stat_area$tot*100
stat_area$checktot <- stat_area$percBV+stat_area$percWV


tab_paper <- stat_area[,
                       c("area", "vmean", "tot", "percBV", "percWV")]
tab_paper[,c("percBV", "percWV")] <- round(tab_paper[,c("percBV", "percWV")],1)
tab_paper$vmean <- round(tab_paper$vmean, 2)
tab_paper$tot <- round(tab_paper$tot, 3)
print(xtable::xtable(tab_paper, digits = c(1,1,2,3,1,1)), 
      include.rownames = F)






