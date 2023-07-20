library(tidyverse)
source("script-data-wrangling/funzioni-ispd.R")

ssd <- read_rds("Rdata/ssd.Rds")
ssd_dip <- read_rds("Rdata/ssd-dip.Rds")
ssd <- arrange(ssd, SSD)

dip_keys <- unique(ssd_dip$dip_key)

n_dip <- length(dip_keys)
ispd_stats <- vector("list", n_dip)
names(ispd_stats) <- dip_keys

start <- lubridate::now()
for(k in 1:n_dip){
  ispd_stats[[k]] <- calcola_ispd_mc(
    filter(ssd_dip, dip_key == dip_keys[k]), 
    ssd, n_sims = 500000)
  ispd_stats[[k]]$dip_key <- names(ispd_stats)[[k]]
  print(paste(k, "--done--", date()))
}
end <- lubridate::now()
end - start
write_rds(ispd_stats, "Rdata/ispd_stats.Rds")


