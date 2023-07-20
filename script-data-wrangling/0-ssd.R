library(tidyverse)

# Classificazione dei prodotti degli SSD area A-F
# utile per calcolare voto medio e sd di SSD da usare nell'ISPD
aree <- c("01", "02", "03", "04", "05", "06", "07", "08a", 
          "08b", "09", "10", "11a", "11b", "12", "13", "14")
nome_script <- paste0(aree, "-ssd.R")
nome_script <- paste0("script-data-wrangling/",nome_script)
purrr::map(nome_script, source)

aree <- c("01", "02", "03", "04", "05", "06", "07", "08a", 
          "08b", "09", "10", "11a", "11b", "12", "13", "14")
ssd <- read_rds("Rdata/01-ssd.Rds")
for(k in 2:length(aree)){
  appo <- read_rds(paste0("Rdata/",aree[k],"-ssd.Rds"))
  ssd <- bind_rows(ssd, appo)
}
ssd <- filter(ssd, SSD != "Totale")

write_rds(ssd, file = "Rdata/ssd.Rds")
rm(list = ls())