library(tidyverse)

area <- "02"
nome_file <- paste0("dati-vqr-11-14/VQR2011-2014_Area",area,"_Tabelle.xlsx")

# Table 2.28: Punteggi ottenuti e distribuzione dei prodotti
ssd <- readxl::read_xlsx(nome_file,
                        sheet = "Tabella 2.28", 
                        skip = 1)

ssd <- filter(ssd, !(SSD_add %in% c("Subtotale", "Totale")))

names(ssd)[1] <- "SSD"
names(ssd)[2] <- "v"
names(ssd)[3] <- "NP_s"
names(ssd)[4] <- "I"
names(ssd)[5:10] <- paste0("perc_", LETTERS[1:6])
ssd$perc_NA <- 0

ssd <- select(ssd,
              SSD, v, NP_s, I, 
              perc_A, perc_B, perc_C, perc_D, perc_E, perc_F, perc_NA)


voto_prod <- c(1, .7, .4, .1, 0, 0)
ssd <-left_join(ssd,  
                     ssd %>%
                       rowwise(SSD) %>% 
                       summarise(
                         mVP_s = sum(c_across(perc_A:perc_F)/100*voto_prod),
                         sd_s = sqrt(sum((c_across(perc_A:perc_F)/100)*(voto_prod-mVP_s)^2))) %>% 
                       ungroup() ,by = "SSD"
)

names(voto_prod) <- paste0("voto_", LETTERS[1:6])
voto_prod_std <- c(1, .7, .4, .1, 0, 0)
names(voto_prod_std) <- paste0("voto_std_", LETTERS[1:6])

voto_ssd <- bind_cols(
  SSD = ssd$SSD,
  map_df(voto_prod_std, ~(.- ssd$mVP_s) / ssd$sd_s),
  map_df(voto_prod, ~.)
)

ssd <- left_join(ssd, voto_ssd, by ="SSD")
ssd$area <- area

nome_out <- paste0("Rdata/",area,"-ssd.Rds")
write_rds(ssd, nome_out)
rm(list = ls())

