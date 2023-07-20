library(tidyverse)

area <- "06"
nome_file <- paste0("dati-vqr-11-14/VQR2011-2014_Area",area,"_Tabelle.xlsx")

# Table 4.3:  Graduatoria dei Dipartimenti piccoli per SSD di afferenza dell'addetto
ssd_dip_p <- readxl::read_xlsx(nome_file,
                           sheet = "Tabella4.3",
                           skip = 2)
names(ssd_dip_p)[1] <- "SSD"
sum(is.na(ssd_dip_p$SSD))

# Table 4.4:  Graduatoria dei Dipartimenti medi per SSD di afferenza dell'addetto
ssd_dip_m <- readxl::read_xlsx(nome_file,
                           sheet = "Tabella4.4",
                           skip = 2)
names(ssd_dip_m)[1] <- "SSD"
sum(is.na(ssd_dip_m$SSD))

# Table 4.5:  Graduatoria dei Dipartimenti grandi per SSD di afferenza dell'addetto
ssd_dip_g <- readxl::read_xlsx(nome_file,
                           sheet = "Tabella4.5",
                           skip = 2)
names(ssd_dip_g)[1] <- "SSD"
sum(is.na(ssd_dip_g$SSD))

mean(names(ssd_dip_p) %in% names(ssd_dip_g))
mean(names(ssd_dip_m) %in% names(ssd_dip_g))

id_nome <- which(!(names(ssd_dip_p) %in% names(ssd_dip_g)))
names(ssd_dip_p)[id_nome] <- names(ssd_dip_g)[id_nome]

ssd_dip <- bind_rows(
  ssd_dip_p, ssd_dip_m, ssd_dip_g
)

rm(ssd_dip_p, ssd_dip_m, ssd_dip_g)

ssd_dip$dip_key <- paste(ssd_dip$Istituzione, 
                         ssd_dip$Dipartimento, 
                     sep = "-")
names(ssd_dip)[6] <- "v"
names(ssd_dip)[7] <- "NP_sd"
names(ssd_dip)[8] <- "I"
names(ssd_dip)[9:14] <- paste0("perc_", LETTERS[1:6])
names(ssd_dip)[15] <- "perc_NA"

ssd_dip <- select(ssd_dip, 
                  SSD, Istituzione, Dipartimento, v, NP_sd, I,
                  perc_A, perc_B, perc_C, perc_D, perc_E, perc_F, perc_NA, dip_key)

ssd_dip$n_A <- round(ssd_dip$NP_sd * ssd_dip$perc_A / 100)
ssd_dip$n_B <- round(ssd_dip$NP_sd * ssd_dip$perc_B / 100)
ssd_dip$n_C <- round(ssd_dip$NP_sd * ssd_dip$perc_C / 100)
ssd_dip$n_D <- round(ssd_dip$NP_sd * ssd_dip$perc_D / 100)
ssd_dip$n_E <- round(ssd_dip$NP_sd * ssd_dip$perc_E / 100)
ssd_dip$n_F <- round(ssd_dip$NP_sd * ssd_dip$perc_F / 100)
ssd_dip$n_NA <- round(ssd_dip$NP_sd * ssd_dip$perc_NA / 100)

ssd_dip %>% 
  rowwise(SSD) %>% 
  summarise(sum(c_across(n_A:n_F)), NP_sd)

sum(is.na(ssd_dip))

nome_out <- paste0("Rdata/", area, "-ssd-dip.Rds")
write_rds(ssd_dip, nome_out)
rm(list = ls())
