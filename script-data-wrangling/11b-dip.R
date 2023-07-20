library(tidyverse)

area <- "11b"
nome_file <- paste0("dati-vqr-11-14/VQR2011-2014_Area",area,"_Tabelle.xlsx")

# Table 4.2:  Graduatoria dei Dipartimenti molto(!) piccoli 
dip_mp <- readxl::read_xlsx(nome_file,
                           sheet = "Tabella 4.2",
                           skip = 3)

# Table 4.3:  Graduatoria dei Dipartimenti piccoli 
dip_p <- readxl::read_xlsx(nome_file,
                           sheet = "Tabella 4.3",
                           skip = 4)

# Table 4.4:  Graduatoria dei Dipartimenti medi 
dip_m <- readxl::read_xlsx(nome_file,
                           sheet = "Tabella4.4",
                           skip = 2)

# Tabella 4.5:  Graduatoria dei Dipartimenti grandi 
dip_g <- readxl::read_xlsx(nome_file,
                           sheet = "Tabella4.5",
                           skip = 3)

mean(names(dip_p) %in% names(dip_g))
mean(names(dip_m) %in% names(dip_g))
mean(names(dip_mp) %in% names(dip_g))


dip <- bind_rows(
  dip_mp, dip_p, dip_m, dip_g
)

rm(dip_mp, dip_p, dip_m, dip_g)

dip$dip_key <- paste(dip$Istituzione, 
                     dip$Dipartimento, 
                     sep = "-")
names(dip)[5] <- "v"
names(dip)[6] <- "n"
names(dip)[7] <- "I"
names(dip)[8:13] <- paste0("perc_", LETTERS[1:6])
names(dip)[14] <- "perc_NA"

dip$n_A <- round(dip$n * dip$perc_A / 100)
dip$n_B <- round(dip$n * dip$perc_B / 100)
dip$n_C <- round(dip$n * dip$perc_C / 100)
dip$n_D <- round(dip$n * dip$perc_D / 100)
dip$n_E <- round(dip$n * dip$perc_E / 100)
dip$n_F <- round(dip$n * dip$perc_F / 100)
dip$n_NA <- round(dip$n * dip$perc_NA / 100)

dip <- select(dip, 
              dip_key, Istituzione, Dipartimento, v, n, I, perc_A,
              perc_B, perc_C, perc_D, perc_E, perc_F, perc_NA,
              n_A, n_B, n_C, n_D, n_E, n_F, n_NA)

pesi_prod <- c(1, .7, .4, .1, 0, 0)

check <- dip %>%
  rowwise(dip_key) %>% 
  summarise(v = sum(c_across(n_A:n_F)*pesi_prod),
            m = sum(c_across(perc_A:perc_F)/100*pesi_prod),
            sd = sqrt(sum((c_across(perc_A:perc_F)/100)*(pesi_prod-m)^2))
  )
summary(check$v - dip$v)
summary((check$v - dip$v) / dip$v)
summary(check$m - dip$I)

nome_out <- paste0("Rdata/", area, "-dip.Rds")
write_rds(dip, nome_out)
rm(list = ls())
