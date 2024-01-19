library(tidyverse)
library(latex2exp)

aree <- c("1", "2", "3", "4", "5", "6", "7", "8a", "8b",
          "9", "10", "11a", "11b", "12", "13a", "13b", "14")
ssd <- list()

# area 1 ----
ssd_a1 <- readxl::read_excel(
  "dati-vqr-15-19/VQR-2015-19-Rapporto_Area_GEV01_tabelle.xlsx",
  sheet = "Tabella 2.7", skip = 6)

ssd_a1 <- bind_cols(SSD_ric = ssd_a1$SSD_ric,
ssd_a1 %>%
  select(-1) %>% 
  mutate_if(is.character,
            str_replace,
            pattern = ",",
            replacement = "\\.") %>%
  mutate_if(is.character,
            as.numeric)  
  ) 

a1 <- ssd_a1 %>% filter(SSD_ric == "Totale")
ssd[[1]] <- ssd_a1 %>% filter(!(SSD_ric == "Totale"))

# area 2 ----
ssd_a2 <- readxl::read_excel(
  "dati-vqr-15-19/VQR-2015-19-Rapporto_Area_GEV02_tabelle.xlsx",
  sheet = "Tabella 2.7", skip = 5)

a2 <- ssd_a2 %>% filter(SSD_ric == "Totale")

ssd[[2]] <- ssd_a2 %>% 
  filter(!(SSD_ric == "Totale"), !is.na(SSD_ric))

# area 3 ----
ssd_a3 <- readxl::read_excel(
  "dati-vqr-15-19/VQR-2015-19_Rapporto_Area_GEV03_tabelle.xlsx",
  sheet = "Tabella 2.7", skip = 1)

a3 <- ssd_a3 %>% filter(SSD_ric == "Totale")

ssd[[3]] <- ssd_a3 %>% 
  filter(!(SSD_ric == "Totale"))

# area 4 ----
ssd_a4 <- readxl::read_excel(
  "dati-vqr-15-19/VQR-2015-19_Rapporto_Area_GEV04_tabelle.xlsx",
  sheet = "Tabella 2.7", skip = 8)

a4 <- ssd_a4 %>% filter(SSD_ric == "Totale")

ssd[[4]] <- ssd_a4 %>% 
  filter(!(SSD_ric == "Totale"))

# area 5 ----
ssd_a5 <- readxl::read_excel(
  "dati-vqr-15-19/VQR-2015-19_Rapporto_Area_GEV05_tabelle.xlsx",
  sheet = "Tabella 2.7", skip = 1)

a5 <- ssd_a5 %>% filter(SSD_ric == "Totale")

ssd[[5]] <- ssd_a5 %>% 
  filter(!(SSD_ric == "Totale"))


# area 6 ----
ssd_a6 <- readxl::read_excel(
  "dati-vqr-15-19/VQR-2015-19_Rapporto_Area_GEV06_tabelle.xlsx",
  sheet = "Tabella 2.2", skip = 8)

a6 <- ssd_a6 %>% filter(SSD_ric == "Totale")

ssd[[6]] <- ssd_a6 %>% 
  filter(!(SSD_ric == "Totale"))

# area 7 ----
ssd_a7 <- readxl::read_excel(
  "dati-vqr-15-19/VQR-2015-19_Rapporto_Area_GEV07_tabelle_EC_21luglio2022.xlsx",
  sheet = "Tabella 2.7", skip = 8)

ssd_a7 <- select(ssd_a7, 1:9)

a7 <- ssd_a7 %>% filter(SSD_ric == "Totale")

ssd[[7]] <- ssd_a7 %>% 
  filter(!(SSD_ric == "Totale"))

# area 8a ----
ssd_a8a <- readxl::read_excel(
  "dati-vqr-15-19/VQR-2015-19_Rapporto_Area_GEV08a_tabelle.xlsx",
  sheet = "Tabella 2.7", skip = 8)

a8a <- ssd_a8a %>% filter(SSD_ric == "Totale")

ssd[[8]] <- ssd_a8a %>% 
  filter(!(SSD_ric == "Totale"))

# area 8b ----
ssd_a8b <- readxl::read_excel(
  "dati-vqr-15-19/VQR-2015-19_Rapporto_Area_GEV08b_tabelle.xlsx",
  sheet = "Tabella 2.7", skip = 8)

a8b <- ssd_a8b %>% filter(SSD_ric == "Totale")

ssd[[9]] <- ssd_a8b %>% 
  filter(!(SSD_ric == "Totale"))

# area 9 ----
ssd_a9 <- readxl::read_excel(
  "dati-vqr-15-19/VQR-2015-19_Rapporto_Area_GEV09_tabelle_EC_21luglio2022.xlsx",
  sheet = "Tabella 2.7", skip = 5)

names(ssd_a9)[1] <- "SSD_ric"
a9 <- ssd_a9 %>% filter(SSD_ric == "Totale")

ssd[[10]] <- ssd_a9 %>% 
  filter(!(SSD_ric == "Totale"))

# area 10 ----
ssd_a10 <- readxl::read_excel(
  "dati-vqr-15-19/VQR-2015-19_Rapporto_Area_GEV10_tabelle.xlsx",
  sheet = "Tabella 2.7", skip = 8)

a10 <- ssd_a10 %>% filter(SSD_ric == "Totale")

ssd[[11]] <- ssd_a10 %>% 
  filter(!(SSD_ric == "Totale"))

# area 11a ----
ssd_a11a <- readxl::read_excel(
  "dati-vqr-15-19/VQR-2015-19_Rapporto_Area_GEV11a_tabelle.xlsx",
  sheet = "Tabella 2.7", skip = 8)

a11a <- ssd_a11a %>% filter(SSD_ric == "Totale")

ssd[[12]] <- ssd_a11a %>% 
  filter(!(SSD_ric == "Totale"))

# area 11a ----
ssd_a11b <- readxl::read_excel(
  "dati-vqr-15-19/VQR-2015-19_Rapporto_Area_GEV11b_tabelle.xlsx",
  sheet = "Tabella 2.7", skip = 8)

a11b <- ssd_a11b %>% filter(SSD_ric == "Totale")

ssd[[13]] <- ssd_a11b %>% 
  filter(!(SSD_ric == "Totale"))

# area 12 ----
ssd_a12 <- readxl::read_excel(
  "dati-vqr-15-19/VQR-2015-19_Rapporto_Area_GEV12_tabelle.xlsx",
  sheet = "Tabella 2.7", skip = 8)

a12 <- ssd_a12 %>% filter(SSD_ric == "Totale")

ssd[[14]] <- ssd_a12 %>% 
  filter(!(SSD_ric == "Totale"))

# area 13a ----
ssd_a13a <- readxl::read_excel(
  "dati-vqr-15-19/VQR-2015-19_Rapporto_Area_GEV13a_tabelle (1).xlsx",
  sheet = "Tabella 2.7", skip = 7)

a13a <- ssd_a13a %>% filter(SSD_ric == "Totale")

ssd[[15]] <- ssd_a13a %>% 
  filter(!(SSD_ric == "Totale"))

# area 13b ----
ssd_a13b <- readxl::read_excel(
  "dati-vqr-15-19/VQR-2015-19_Rapporto_Area_GEV13b_tabelle.xlsx",
  sheet = "Tabella 2.7", skip = 8)

a13b <- ssd_a13b %>% filter(SSD_ric == "Totale")

ssd[[16]] <- ssd_a13b %>% 
  filter(!(SSD_ric == "Totale"))

# area 14 ----
ssd_a14 <- readxl::read_excel(
  "dati-vqr-15-19/VQR-2015-19_Rapporto_Area_GEV14_tabelle.xlsx",
  sheet = "Tabella 2.7", skip = 6)

a14 <- ssd_a14 %>% filter(SSD_ric == "Totale")

ssd[[17]] <- ssd_a14 %>% 
  filter(!(SSD_ric == "Totale"))

names(ssd) <- aree
ssd <- bind_rows(ssd, .id = "area")

names(ssd) <- c("area", "ssd", "tot_p", "NP_s", "I", "perc_A", "perc_B", "perc_C", 
                "perc_D", "perc_E")

voti <- c(1, .8, .5, .2, 0)
ssd$mVP_s <- NA
ssd$sd_s <- NA
for(k in 1:nrow(ssd)){
  ssd$mVP_s[k] <- sum(ssd[k,6:10]/100 * voti)
  ssd$sd_s[k] <- sqrt(sum(ssd[k,6:10]/100 * (voti - ssd$mVP_s[k])^2))
}


names(ssd)
ssd$rank_I <- rank(ssd$mVP_s)


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

ggsave("plots-appendix/ssd_15.pdf", c1, width = 8, height = 4)


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

ggsave("plots-appendix/ssd_aree_15.pdf", c2, width = 11, height = 8)




