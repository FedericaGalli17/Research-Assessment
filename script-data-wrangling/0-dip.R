library(tidyverse)

# Tabella 2.1 Le università partecipanti alla VQR2
atenei_vqr <- readxl::read_xlsx("dati-vqr-11-14/tabelle_parteprima_VQR2011-2014.xlsx",
                               sheet = "Tabella 2.1", skip = 2)

# Tabella 6.9: Elenco dei dipartimenti delle università elencati in ordine alfabetico per area e università con i valori degli indicatori della qualità media dei prodotti attesi e posizione in graduatoria per ogni area. I parametri v e n rappresentano rispettivamente la valutazione complessiva e il numero di prodotti attesi. Gli indicatori I, R e X rappresentano il voto medio dei prodotti attesi del dipartimento nell’area, il rapporto tra voto medio dell’università e voto medio di area e il rapporto tra la frazione di prodotti eccellenti ed elevati del dipartimento  nell’area e la frazione di prodotti eccellenti ed elevati dell’area. Le colonne Pos. grad. compl. e  Pos. grad. Classe rappresentano rispettivamente la posizione del dipartimento nella graduatoria complessiva di area dei dipartimenti e la posizione nella graduatoria della classe dimensionale di appartenenza.  Le colonne # istituzioni compl. e  # istituzioni classe indicano il numero complessivo dei dipartimenti che hanno presentato prodotti nell’area e il numero dei dipartimenti all’interno della classe dimensionale. Infine, la colonna Classe dimensionale indica la classe dimensionale di appartenenza dell’istituzione (G=Grande, M=Medio, P=Piccolo) . L’indicatore R non tiene conto della diversità delle distribuzioni dei voti fra i settori concorsuali all’interno della stessa area e non è standardizzato, cioè non è diviso per la deviazione standard dell'indice dell'area. 
# In vista della definizione di un “Indicatore standardizzato della performance dipartimentale” richiesta dall’articolo 1, comma 319, della legge di bilancio 2017, l'ANVUR approfondirà nei prossimi mesi sia l’insieme omogeneo appropriato per la normalizzazione sia la metodologia di standardizzazione più appropriata alla valutazione dei dipartimenti cui afferiscano docenti appartenenti ad aree e settori diversi.
dip_vqr <- readxl::read_xlsx(
  "dati-vqr-11-14/tabelle_parteprima_VQR2011-2014.xlsx",
  sheet = "Tab.6.9", 
  skip = 2)

#elimino un po' di roba
names(dip_vqr)
dip_vqr <- select(dip_vqr, -c(7:8,10:14))
names(dip_vqr)

names(dip_vqr)[4] <- "v"
names(dip_vqr)[5] <- "n"
names(dip_vqr)[6] <- "I"


#Atenei nella Tabella 2.1 ma non contenuti nella tabella 6.9 
atenei_vqr %>% 
  filter(!(Abbreviazione %in% unique(dip_vqr$Ateneo)))

#chiave univoca di dipartimento
dip_vqr$dip_key <- paste(dip_vqr$Ateneo, 
                         dip_vqr$Dipartimento, 
                         sep = "-")
length(unique(dip_vqr$dip_key))

#n dipartimenti per ateneo e prodotti attesi
dip_vqr %>% 
  group_by(Ateneo) %>% 
  summarise(ndip = length(unique(dip_key)),
            p_att = sum(n)) %>% 
  arrange(desc(ndip))

#numero di dipartimenti che hanno presentato prodotti nell'area
dip_vqr %>% 
  group_by(Area) %>% 
  summarise(n())

#numero di aree per dipartimento
a <- dip_vqr %>% 
  group_by(dip_key) %>% 
  summarise(n())
#View(a)

write_rds(atenei_vqr, "Rdata/atenei-vqr.Rds")
write_rds(dip_vqr, "Rdata/dip-vqr.Rds")
rm(list = ls())


