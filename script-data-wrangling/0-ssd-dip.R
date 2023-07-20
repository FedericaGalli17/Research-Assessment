library(tidyverse)

# Classificazione dei prodotti degli SSD area A-F
# utile per calcolare voto medio e sd di SSD da usare nell'ISPD
aree <- c("01", "02", "03", "04", "05", "06", "07", "08a", 
          "08b", "09", "10", "11a", "11b", "12", "13", "14")
nome_script <- paste0(aree, "-ssd-dip.R")
nome_script <- paste0("script-data-wrangling/",nome_script)
purrr::map(nome_script, source)

aree <- c("01", "02", "03", "04", "05", "06", "07", "08a", 
          "08b", "09", "10", "11a", "11b", "12", "13", "14")
ssd_dip <- read_rds("Rdata/01-ssd-dip.Rds")
for(k in 2:length(aree)){
  appo <- read_rds(paste0("Rdata/",aree[k],"-ssd-dip.Rds"))
  ssd_dip <- bind_rows(ssd_dip, appo)
}

write_rds(ssd_dip, file = "Rdata/ssd-dip.Rds")
rm(list = ls())