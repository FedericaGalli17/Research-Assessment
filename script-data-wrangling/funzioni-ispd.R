
calcola_ispd_poggi <- function(dipartimento, settori){
  left_join(
    dipartimento,
  select(settori, SSD, voto_A:voto_F, voto_std_A:voto_std_F), 
  by = "SSD") %>% 
  rowwise(SSD) %>%
  summarise(v = sum(c_across(n_A:n_F)*c_across(voto_A:voto_F)),
            v_std = sum(c_across(n_A:n_F)*c_across(voto_std_A:voto_std_F)),
            n_pr = sum(c_across(n_A:n_F))) %>%
  ungroup() %>% 
  summarise(NP_d = sum(n_pr),
            I = sum(v) / NP_d,
            VS_d = sum(v_std) / sqrt(NP_d),
            ispd_poggi = pnorm(VS_d) * 100,
  )
}

calcola_punteggi_ssd_dip <- function(dipartimento, settori){
  left_join(
    dipartimento,
    select(settori, SSD, voto_A:voto_F, voto_std_A:voto_std_F), 
    by = "SSD") %>% 
    rowwise(SSD) %>%
    summarise(v = sum(c_across(n_A:n_F)*c_across(voto_A:voto_F)),
              v_std = sum(c_across(n_A:n_F)*c_across(voto_std_A:voto_std_F)),
              NP_ds = sum(c_across(n_A:n_F)))
}

calcola_ispd_mc <- function(dipartimento, settori, n_sims = 100000, return_mc = FALSE){
  ispd_poggi <- calcola_ispd_poggi(dipartimento, settori)
  dva_ssd <- matrix(NA, n_sims, nrow(dipartimento))
  for(k in 1:nrow(dipartimento)){
    x <- settori %>% filter(SSD == dipartimento$SSD[k]) %>% 
      select(voto_std_A:voto_std_F) %>% unlist
    pt <- settori %>% filter(SSD == dipartimento$SSD[k]) %>% 
      select(perc_A:perc_F) %>% unlist
    n <- dipartimento$NP_sd[k]
    for(i in 1:n_sims){
      dva_ssd[i,k] <- sum(sample(x = x, size = n, prob = pt, replace = TRUE))
      if(i%%(n_sims/2)==0) print(paste(k,"---",i))
    }
  }
  punteggi_ssd_dip <- calcola_punteggi_ssd_dip(dipartimento, settori)
  punteggi_ssd_dip$P_inf_sec <- colMeans(t(t(dva_ssd) <= punteggi_ssd_dip$v_std))
  ispd_ssd_dip <- sum(punteggi_ssd_dip$P_inf_sec * punteggi_ssd_dip$NP_ds) / sum(punteggi_ssd_dip$NP_ds)
  dva <- rowSums(dva_ssd)
  ispd_poggi$ispd_poggi_mc <-  100 *mean(dva <= ispd_poggi$VS_d * sqrt(ispd_poggi$NP_d))
  ispd_poggi$ispd_ssd_dip <- ispd_ssd_dip *100
  ispd_poggi$settori <- list(unlist(dipartimento$SSD))
  ispd_poggi$dip_key <- dipartimento$dip_key[1]
  punteggi_ssd_dip$dip_key <- dipartimento$dip_key[1]
  if(return_mc){
    return(list(ispd = ispd_poggi, 
                ispd_ssd = punteggi_ssd_dip, dva_ssd = dva_ssd))
  } else {
    return(list(ispd = ispd_poggi, 
                ispd_ssd = punteggi_ssd_dip))
  }
}

