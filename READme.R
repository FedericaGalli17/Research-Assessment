#These files allow to reproduce the analysis performed in the paper 
# "Ranking Deparments based on research quality: a statistical evaluation 
# of the ISPD indicator" written by Federica Galli and Fedele Greco.

#All tables and figures in the paper can be reproduced through the
# codes contained in the "Section-X.R" files.


#The following commands run data wrangling scripts for
# - reading data from Research Area specific reports, VQR 2011-2014
# - Computing ISPD and other stats both by permutation and via
#   Monte Carlo simulation
# - The main output used for further statistical analyses is the rds file
# "Rdata/ispd_stats.Rds"

library(tidyverse)

# department level tables
source("script-data-wrangling/0-dip.R")

# scientific disciplinary sector tables
source("script-data-wrangling/0-ssd.R")

# tables for scientific disciplinary sector by department
source("script-data-wrangling/0-ssd-dip.R")

# computation of ISPD - time consuming because of permutations
source("script-data-wrangling/calcolo-statistiche-ispd.R")




