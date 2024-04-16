# ------ SCRAPING ------

library(rvest)
library(xml2)
library(dplyr)
# Pacchetti richiesti.

nomi.colonna <- page %>% 
  html_nodes('.DataGrid_defaultDtStyle__soJ6R') %>% 
  html_text()
nomi.colonna <- nomi.colonna[1:40]
# Queste sono le caratteristiche dell'auto delle quali si vuole
# fare scraping. Sono state prese da un URL di prova vedendo che
# la scheda dell'auto era completa.

get_a_car <- function(url) {
  
  page <- read_html(url) 
  # Lettura URL.
  
  brand <- page %>% 
    html_nodes('.StageTitle_boldClassifiedInfo__sQb0l:nth-child(1)') %>% 
    html_text()
  # Marca dell'auto.
  
  model <- page %>%
    html_nodes('.StageTitle_modelVersion__Yof2Z') %>%
    html_text()
  # Modello dell'auto.
  
  city <- page %>% 
    html_nodes('.LocationWithPin_locationItem__tK1m5') %>% 
    html_text()
  # Dove si trova l'auto?
  
  nomi.colonna <- page %>% 
    html_nodes('.DataGrid_defaultDtStyle__soJ6R') %>% 
    html_text()
  nomi.colonna <- c("Marca", "Modello", "Paese",
                    nomi.colonna[1:(length(nomi.colonna)-10)])
  # Nomi degli attributi della scheda tecnica dell'auto.
  
  dettagli <- page %>% 
    html_nodes('.DataGrid_fontBold__RqU01') %>% 
    html_text()
  dettagli <- c(brand, model, city, 
                dettagli[1:(length(dettagli)-10)])
  # Dettagli della scheda tecnica dell'auto.
  
  auto <- rbind(nomi.colonna, dettagli) %>% 
    janitor::row_to_names(1)
  # Dataframe relativo ad un'auto.
  
  optional <- page %>% 
    html_nodes('li') %>% 
    html_text()
  optional <- paste(optional[-c(1:16, (length(optional)-28):length(optional))],
                 collapse = "*")
  auto <- cbind(auto, optional = as.character(optional))
  # Aggiunta degli optional (confort, intrattenimento, sicurezza,
  # extra veri e propri).
  
  return(auto)

}
# Funzione per fare scraping dei dati di una singola auto.

get_links <- function(url){
  page <- read_html(url)
  all_links <- page %>% 
    html_nodes('a') %>% html_attr('href')
  links_for_cars <- all_links[startsWith(all_links, '/annunci/')]
  for (i in 1:length(links_for_cars)) {
    links_for_cars[i] <- paste0('https://www.autoscout24.it',
                                links_for_cars[i])
  }
  return(links_for_cars)
}
# Funzione per prendere tutti i link alle vetture presenti in
# una pagina.


# LINK ALLE PAGINE DELLE AUTO PIU` VENDUTE IN ITALIA NEL
# 2023 (FONTE: ALVOLANTE, https://www.alvolante.it/news/auto-piu-vendute-in-italia-nel-2023-391319)

main <- "https://www.autoscout24.it/lst/"
post <- "?atype=C&cy=I&desc=0&sort=standard&source=homepage_search-mask&ustate=N%2CU"
fiat.panda <- paste(main, "fiat/panda", post, sep = "")
dacia.sandero <- paste(main, "dacia/sandero", post, sep = "")
lancia.y <- paste(main, "lancia/ypsilon", post, sep = "")
toyota.yaris <- paste(main, "toyota/yaris", post, sep = "")
fiat.500 <- paste(main, "fiat/500", post, sep = "")
vw.troc <- paste(main, "volkswagen/t-roc", post, sep = "")
renault.captur <- paste(main, "renault/captur", post, sep = "")
citroen.c3 <- paste(main, "citroen/c3-(tutto)", post, sep = "")
ford.puma <- paste(main, "ford/puma", post, sep = "")
dacia.duster <- paste(main, "dacia/duster", post, sep = "")
jeep.renegade <- paste(main, "jeep/renegade", post, sep = "")
fiat.500x <- paste(main, "fiat/500x", post, sep = "")
renault.clio <- paste(main, "renault/clio", post, sep = "")
peugeot.208 <- paste(main, "peugeot/208", post, sep = "")
peugeot.2008 <- paste(main, "peugeot/2008", post, sep = "")
toyota.yaris.cross <- paste(main, "toyota/yaris-cross", post, sep = "")
opel.corsa <- paste(main, "opel/corsa", post, sep = "")
jeep.avenger <- paste(main, "jeep/avenger", post, sep = "")
jeep.compass <- paste(main, "jeep/compass", post, sep = "")
vw.tcross <- paste(main, "volkswagen/t-cross", post, sep = "")
nissan.qashqai <- paste(main, "nissan/qashqai", post, sep = "")
mg.zs <- paste(main, "mg/zs", post, sep = "")
kia.sportage <- paste(main, "kia/sportage", post, sep = "")
peugeot.3008 <- paste(main, "peugeot/3008", post, sep = "")
alfa.tonale <- paste(main, "alfa-romeo/tonale", post, sep = "")
vw.polo <- paste(main, "volkswagen/polo", post, sep = "")
ford.kuga <- paste(main, "ford/kuga", post, sep = "")
vw.tiguan <- paste(main, "volkswagen/tiguan", post, sep = "")
ford.focus <- paste(main, "ford/focus", post, sep = "")
toyota.aygox <- paste(main, "toyota/aygo-x", post, sep = "")
audi.q3 <- paste(main, "audi/q3", post, sep = "")
bmw.x1 <- paste(main, "bmw/x1", post, sep = "")
hyundai.i10 <- paste(main, "hyundai/i10", post, sep = "")
hyundai.tucson <- paste(main, "hyundai/tucson", post, sep = "")
citroen.c3aircross <- paste(main, "citroen/c3-aircross", post, sep = "")
cupra.formentor <- paste(main, "cupra/formentor", post, sep = "")
mercedes.gla <- paste(main, "mercedes-benz/gla-(tutto)", post, sep = "")
ford.fiesta <- paste(main, "ford/fiesta", post, sep = "")
kia.picanto <- paste(main, "kia/picanto", post, sep = "")
nissan.juke <- paste(main, "nissan/juke", post, sep = "")
audi.a3 <- paste(main, "audi/a3", post, sep = "")
opel.mokka <- paste(main, "opel/mokka", post, sep = "")
suzuki.ignis <- paste(main, "suzuki/ignis", post, sep = "")
fiat.tipo <- paste(main, "fiat/tipo", post, sep = "")
vw.golf <- paste(main, "volkswagen/golf", post, sep = "")
renault.austral <- paste(main, "renault/austral", post, sep = "")
audi.a1 <- paste(main, "audi/a1", post, sep = "")
mini.countryman <- paste(main, "mini/cooper-countryman", post, sep = "")
vw.taigo <- paste(main, "volkswagen/taigo", post, sep = "")
suzuki.vitara <- paste(main, "suzuki/vitara", post, sep = "")
elenco.auto <- c(fiat.panda, dacia.sandero, lancia.y, 
                 toyota.yaris, fiat.500, vw.troc, renault.captur,
                 citroen.c3, ford.puma, dacia.duster, 
                 jeep.renegade, fiat.500x, renault.clio,
                 peugeot.208, peugeot.2008, toyota.yaris.cross,
                 opel.corsa, jeep.avenger, jeep.compass,
                 vw.tcross, nissan.qashqai, mg.zs,
                 kia.sportage, peugeot.3008, alfa.tonale,
                 vw.polo, ford.kuga, vw.tiguan, ford.focus,
                 toyota.aygox, audi.q3, bmw.x1, 
                 hyundai.i10, hyundai.tucson, citroen.c3aircross,
                 cupra.formentor, mercedes.gla, ford.fiesta,
                 kia.picanto, nissan.juke, audi.a3,
                 opel.mokka, suzuki.ignis, fiat.tipo,
                 vw.golf, renault.austral, audi.a1, 
                 mini.countryman, vw.taigo, suzuki.vitara)
# Elenco siti macchine.

all_links <- character(0)
for (i in 1:50) {
  all_links <- c(all_links, 
                 unlist(lapply(elenco.auto[i], get_links)))
  print(i)
  Sys.sleep(5)
}


all_links <- all_links[!all_links %in% 
                         c('https://www.autoscout24.itNA')]
# Rimozione dei link NA.

cars <- data.frame(get_a_car(all_links[1]))
# Prima macchina!

for (i in 2:length(all_links)) {
  auto <- data.frame(get_a_car(all_links[i]))
  cars2 <- bind_rows(cars2, auto)
  print(i)
  Sys.sleep(4)
}
# Si fa una pausa di 4 secondi per ogni auto in modo da non
# inondare il server di richieste e farsi bloccare 
# l'indirizzo IP.
# Un'auto (una Opel Corsa) non era più disponibile al momento
# dell'estrazione.

cars <- cars %>% janitor::clean_names()
cars <- data.frame(cars, row.names = NULL)
head(cars)
cars <- cars[-c(289, 915), ]
cars <- cars %>% 
  filter(!is.na(prezzo_auto))
# Dataframe definitivo. Due righe sono sfasate col resto (lo
# scraping non ha funzionato) e vengono eliminate.
# Si eliminano anche le macchine con prezzo sconosciuto.

write.csv(cars, "macchine_scraping.csv", row.names = F)
# Dati grezzi.


#### ---- Aggiornamento dati auto ---- ####
#* Per evitare di dover riscaricare da capo i dati, si può prendere il dataset precedente
#* Confrontare il numero delle righe con il numero dei link, visto che ad ogni link corrisponde un'auto
#* E si fa ripartire il ciclo di "get_a_car" per questa differenza
#* In tal modo poi è possibile fare solo bind_rows con il dataset precedente grezzo e risalvarlo di nuovo

#carico il dataframe vecchio
temp_dataframe = read.csv("macchine_scraping.csv")
#creo quello con le "novità"
new_cars = data.frame()

#riempo le nuove auto
for (i in dim(temp_dataframe)[1]:length(all_links)) {
  auto <- data.frame(get_a_car(all_links[i]))
  new_cars <- bind_rows(new_cars, auto)
  print(i)
  Sys.sleep(4)
}


# ------ PREZZO SECONDO AUTOSCOUT24 ------ 

# La prima Panda       -> "Superprezzo".
# La seconda Dacia     -> "Ottimo prezzo".
# La quinta Lancia Y   -> "Buon prezzo".
# La terza Yaris       -> "Buon prezzo".
# La prima 500         -> "Superprezzo".
# La quintultima T-Roc -> "Ottimo prezzo".


# ------ PULIZIA DEL DATASET ------

library(tidyverse)
cars <- read.csv(file.choose())
colnames(cars)
# Dataset "cars-raw.csv".

# 1) Eliminazione colonne inutili ------

cars$cambio_cinghia_distribuzione = NULL
cars$disponibilita = NULL
cars$colore_specifico = NULL
cars$revisione = NULL
cars$ultimo_tagliando = NULL
# Troppi valori mancanti.

cars$anticipo = NULL
cars$offerta_n = NULL
cars$importo_da_pagare = NULL
cars$importo_finanziato = NULL
cars$importo_lordo_del_credito = NULL
cars$costo_del_trasferimento = NULL
cars$acconto = NULL
cars$importo_totale_dovuto = NULL
cars$taeg = NULL
cars$taeg_2 = NULL
cars$taeg_ = NULL
cars$intrattenimento___media = NULL
cars$costo_del_trasferimento_ = NULL
cars$costo_di_registrazione = NULL
cars$importo_lordo_del_credito__ = NULL
cars$importo_totale_netto_del_credito = NULL
cars$acconto_1 = NULL
cars$offerta_n_ = NULL
cars$spese = NULL
cars$rata = NULL
cars$rata_finale = NULL
cars$rata_mensile = NULL
cars$durata = NULL
cars$tan_fisso = NULL
# Le colonne relative alla rateizzazione e ai prezzi sono leaker.

cars$comfort = NULL
cars$intrattenimento_media = NULL
cars$extra = NULL
cars$sicurezza =  NULL
cars$versione_per_nazione = NULL
# Gli optional sono nell'apposita colonna.


# 2) Trasformazione nomi dei modelli -------

# panda <- grep("Fiat", cars$marca)[1:19] 
# sandero <- grep("Dacia", cars$marca)[1:19] 
# ypsilon <- grep("Lancia", cars$marca) 
# yaris <- grep("Toyota", cars$marca)[1:19] 
# cinquecento <- grep("Fiat", cars$marca)[20:38] 
# troc <- grep("Volkswagen", cars$marca)[1:18]
# captur <- grep("Renault", cars$marca)[1:19]
# c3 <- grep("Citroen", cars$marca)[1:19]
# puma <- grep("Ford", cars$marca)[1:19]
# duster <- grep("Dacia", cars$marca)[20:38]
# renegade <- grep("Jeep", cars$marca)[1:19]
# cinquecentox <- grep("Fiat", cars$marca)[39:57] 
# clio <- grep("Renault", cars$marca)[20:38]
# p208 <- grep("Peugeot", cars$marca)[1:18]
# p2008 <- grep("Peugeot", cars$marca)[19:37]
# yariscross <- grep("Toyota", cars$marca)[20:36] 
# corsa <- grep("Opel", cars$marca)[1:18] 
# avenger <- grep("Jeep", cars$marca)[20:38] 
# compass <- grep("Jeep", cars$marca)[39:57] 
# tcross <- grep("Volkswagen", cars$marca)[19:56] 
# qashqai <- grep("Nissan", cars$marca)[1:19] 
# zs <- grep("MG", cars$marca) 
# sportage <- grep("Kia", cars$marca)[1:17] 
# p3008 <- grep("Peugeot", cars$marca)[38:56] 
# tonale <- grep("Alfa Romeo", cars$marca) 
# polo <- grep("Volkswagen", cars$marca)[57:75] 
# kuga <- grep("Ford", cars$marca)[20:38] 
# tiguan <- grep("Volkswagen", cars$marca)[76:93] 
# focus <- grep("Ford", cars$marca)[39:57] 
# aygox <- grep("Toyota", cars$marca)[37:55] 
# q3 <- grep("Audi", cars$marca)[1:14] 
# x1 <- grep("BMW", cars$marca) 
# i10 <- grep("Hyundai", cars$marca)[1:19] 
# tucson <- grep("Hyundai", cars$marca)[20:38] 
# c3aircross <- grep("Citroen", cars$marca)[20:36] 
# formentor <- grep("CUPRA", cars$marca) 
# gla <- grep("Mercedes", cars$marca) 
# fiesta <- grep("Ford", cars$marca)[58:76] 
# picanto <- grep("Kia", cars$marca)[18:36] 
# juke <- grep("Nissan", cars$marca)[20:36] 
# a3 <- grep("Audi", cars$marca)[15:33] 
# mokka <- grep("Opel", cars$marca)[19:37] 
# ignis <- grep("Suzuki", cars$marca)[1:18] 
# tipo <- grep("Fiat", cars$marca)[58:76] 
# golf <- grep("Volkswagen", cars$marca)[94:111] 
# austral <- grep("Renault", cars$marca)[39:43] 
# a1 <- grep("Audi", cars$marca)[34:51] 
# countryman <- grep("MINI", cars$marca) 
# taigo <- grep("Volkswagen", cars$marca)[112:129]
# vitara <- grep("Suzuki", cars$marca)[19:37] 
# # Vettori di indici.
# 
# indici <- list(a1, a3, austral, avenger, aygox, c3, c3aircross,
#                captur, cinquecento, cinquecentox, clio, compass,
#                corsa, countryman, duster, fiesta, focus, formentor,
#                gla, golf, i10, ignis, juke, kuga, mokka,
#                p2008, p208, p3008, panda, picanto, polo, puma,
#                q3, qashqai, renegade, sandero, sportage, taigo,
#                tcross, tiguan, tipo, tonale, troc, tucson,
#                vitara, x1, yaris, yariscross, ypsilon, zs)
# labels <- c("A1", "A3", "Austral", "Avenger", "Aygo-X", "C3", "C3 Aircross",
#             "Captur", "500", "500X", "Clio", "Compass",
#             "Corsa", "Countryman", "Duster", "Fiesta", "Focus", "Formentor",
#             "GLA", "Golf", "i10", "Ignis", "Juke", "Kuga", "Mokka",
#             "2008", "208", "3008", "Panda", "Picanto", "Polo", "Puma",
#             "Q3", "Qashqai", "Renegade", "Sandero", "Sportage", "Taigo",
#             "T-Cross", "Tiguan", "Tipo", "Tonale", "T-Roc", "Tucson",
#             "Vitara", "X1", "Yaris", "Yaris Cross", "Ypsilon", "ZS")
# # Conversione in lista dei vettori di indici e labels di ogni modello.
# 
# for (i in 1:length(indici)) {
#   cars$modello[indici[[i]]] <- labels[i]
# }
# # Assegnazione ad ogni macchina del nome del modello.


# 3) Prezzi, chilometraggio, cilindrata -------

cars$prezzo_auto <- gsub("€ ", "", cars$prezzo_auto)
cars$prezzo_auto <- gsub(",-", "", cars$prezzo_auto)
cars$prezzo_auto <- gsub("\\.", "", cars$prezzo_auto)
cars$prezzo_auto <- as.numeric(cars$prezzo_auto)
any(is.na(cars$prezzo_auto))
# I prezzi sono stati convertiti in numerici.

cars$chilometraggio <- gsub(" km", "", cars$chilometraggio)
cars$chilometraggio <- gsub("\\.", "", cars$chilometraggio)
cars$chilometraggio <- as.numeric(cars$chilometraggio)
cars$chilometraggio[is.na(cars$chilometraggio)] <- 0
# Le macchine nuove hanno chilometraggio NA perché è pari a 0.

cars$cilindrata <- gsub(" cm³", "", cars$cilindrata)
cars$cilindrata <- gsub("\\.", "", cars$cilindrata)
cars$cilindrata <- as.numeric(cars$cilindrata)
# Conversione della cilindrata in numeri.


# 4) Anno e anno di produzione -------

cars$anno_prod <- as.numeric(substr(cars$anno, start = 4, stop = 7))
for (i in 1:NROW(cars)) {
  if(is.na(cars$anno_prod[i]) & !is.na(cars$anno_di_produzione[i]))
    cars$anno_prod[i] = cars$anno_di_produzione[i]
}
# Si estrae dall'anno di produzione in formato MM/YYYY l'anno.

cars$anno_prod[is.na(cars$anno_prod)] = "2024"
cars$anno = cars$anno_di_produzione = NULL
cars <- cars %>%
  dplyr::relocate(anno_prod, .before = carrozzeria)
# L'anno di produzione mancante si ha solamente per i veicoli
# nuovi, per il quale si assume che il valore sia pari a 2024.


# 5) Nomi dei paesi ------

find.region <- function(citta){
  citta <- gsub(".*\\b(tresenda|cassina|binasco|seregno|corsico|gallarate|milano|giovanni|cinisello|capriolo|gerola|bulgarograsso|naviglio|lecco|pavia|brescia|lodi|lomellina|monza|bergamo|castiglione|como|cremona|mantova|sondrio|varese|BG|BS|CO|CR|LC|LO|MN|MI|MB|PV|SO|VA)\\b.*", 
                "Lombardia", citta, ignore.case = TRUE)
  citta <- gsub(".*\\b(legnago|ponzano|thiene|treviso|belluno|padova|vicenza|veneto|limena|verona|venezia|monselice|rovigo|TV|BL|PD|VI|VE|VR|RO)\\b.*", 
                "Veneto", citta, ignore.case = TRUE)
  citta <- gsub(".*\\b(ciriè|alba|verolengo|saluzzo|alessandria|marengo|collegno|torinese|asti|biella|cuneo|novara|torino|vercelli|AL|AT|BI|CN|NO|TO|VB|VC)\\b.*", 
                "Piemonte", citta, ignore.case = TRUE)
  citta <- gsub(".*\\b(frosinone|latina|rieti|roma|viterbo|ciampino|FR|LT|RI|RM|VT|ROMA)\\b.*", 
                "Lazio", citta, ignore.case = TRUE)
  citta <- gsub(".*\\b(bologna|panaro|ferrara|forlì|forli|forli-cesena|cesena|modena|parma|piacenza|ravenna|emilia|rimini|BO|FE|FC|MO|PR|PC|RA|RE|RN)\\b.*", 
                "Emilia-Romagna", citta, ignore.case = TRUE)
  citta <- gsub(".*\\b(pietrasanta|firenze|prato|lucca|carrara|fiorentino|grosseto|livorno|pisa|pistoia|siena|arezzo|AR|FI|GR|LI|LU|MS|PI|PT|PO|SI)\\b.*", 
                "Toscana", citta, ignore.case = TRUE)
  citta <- gsub(".*\\b(trento|bolzano|TN|BZ)\\b.*", 
                "Trentino-AA", citta, ignore.case = TRUE)
  citta <- gsub(".*\\b(corciano|terni|perugia|PG|TR)\\b.*", 
                "Umbria", citta, ignore.case = TRUE)
  citta <- gsub(".*\\b(genova|imperia|spezia|savona|GE|IM|SP|SV)\\b.*", 
                "Liguria", citta, ignore.case = TRUE)
  citta <- gsub(".*\\b(Friuli-VG|gorizia|pordenone|trieste|udine|GO|PN|TS|UD)\\b.*", 
                "Friuli-VG", citta, ignore.case = TRUE)
  citta <- gsub(".*\\b(Campania|avellino|benevento|caserta|napoli|salerno|AV|BN|CE|NA|SA)\\b.*", 
                "Campania", citta, ignore.case = TRUE)
  citta <- gsub(".*\\b(catanzaro|CZ|cosenza|CS|crotone|KR|calabria|RC|valentia|VV)\\b.*", 
                "Calabria", citta, ignore.case = TRUE)
  citta <- gsub(".*\\b(bari|barletta|trani|BA|BT|brindisi|foggia|BR|FG|lecce|LE|taranto|TA)\\b.*", 
                "Puglia", citta, ignore.case = TRUE)
  citta <- gsub(".*\\b(ancona|ascoli|fermo|macerata|pesaro|AN|AP|FM|MC|PU)\\b.*", 
                "Marche", citta, ignore.case = TRUE)
  citta <- gsub(".*\\b(aosta|AO)\\b.*", 
                "Valle d'Aosta", citta, ignore.case = TRUE)
  citta <- gsub(".*\\b(palermo|PA)\\b.*", 
                "Sicilia", citta, ignore.case = TRUE)
  citta <- gsub(".*\\b(teramo|pescara)\\b.*", 
                "Abruzzo", citta, ignore.case = TRUE)
  citta <- gsub(".*\\b(potenza)\\b.*", 
                "Basilicata", citta, ignore.case = TRUE)
  return(citta)
}
cars$paese = find.region(cars$paese)
names(cars)[names(cars) == 'paese'] <- 'regione'
# Ora il dataframe contiene la regione e non il paese del rivenditore
# o della concessionaria.

table(cars$regione)
cars$zona_geografica = NA
cars = cars %>% 
  mutate(zona_geografica = case_when(
    regione %in% c("Valle d'Aosta", "Piemonte", "Liguria",
                   "Lombardia") ~ 'Nord Ovest',
    regione %in% c("Veneto", "Trentino-AA", "Friuli-VG") ~ 'Nord Est',
    regione %in% c("Emilia-Romagna", "Toscana", "Umbria",
                   "Marche", "Lazio", "Abruzzo") ~ 'Centro',
    regione %in% c("Campania", "Basilicata", "Calabria",
                   "Sicilia", "Puglia") ~ 'Sud',
  ))
cars <- cars %>%
  dplyr::relocate(zona_geografica, .after = regione)
# Zona geografica.


# 6) Neopatentati e potenza ------

kW <- sub("\\s*kW.*", "", cars$potenza)
cars$kW = kW
cars <- cars %>%
  dplyr::relocate(kW, .before = potenza)
# Indicazione sui kW.

cv <- gsub(".*?\\((.*?)\\).*", "\\1", cars$potenza)
cars$cv <- substr(cv, start = 1, stop = nchar(cv) - 3)
cars <- cars %>%
  dplyr::relocate(cv, .after = kW)
# Indicazione sui cavalli del motore.

cars$potenza = NULL
# Non serve più.

cars$per_neopatentati = ifelse(cars$kW <= 55, "Si", "No")
# Il limite per i neopatentati è 55 kW.
# In questo modo la variabile risulta essere definita
# con certezza sulla base dei kW e si imputano molti NA.


# 7) Peso e carrozzeria ------

peso <- substr(cars$peso_a_vuoto, start = 1,
               stop = nchar(cars$peso_a_vuoto) - 3)
cars$peso_a_vuoto = peso
cars$peso_a_vuoto <- gsub("\\.", "", cars$peso_a_vuoto)
# Pulizia del peso a vuoto.

mediane_per_modello <- cars %>%
  group_by(modello) %>%
  summarise(mediana_peso = median(as.numeric(peso_a_vuoto), na.rm = TRUE))
cars <- left_join(cars, mediane_per_modello, by = "modello")
# Si aggiunge al dataframe una nuova colonna con le mediane dei
# pesi dei modelli.

cars <- cars %>%
  mutate(peso = ifelse(is.na(peso_a_vuoto), mediana_peso, peso_a_vuoto))
cars$peso_a_vuoto = NULL
# I dati mancanti del peso sono stati imputati con la mediana
# fatta per modello (sembra ragionevole).

table(cars$carrozzeria, cars$modello)
# Ad alcuni modelli sono state assegnate più tipologie
# diverse di carrozzeria. Ad esempio, la 500X viene 
# considerata sia come SUV, sia come station wagon.
# Bisogna dunque uniformare il dataset in modo che ad
# ogni modello corrisponda un unico tipo di carrozzeria.

cars$carrozzeria.new = NA
cars$carrozzeria.new = factor(cars$carrozzeria.new,
                              levels = c("Berlina", "City Car", "SUV"))
cars = cars %>% 
  mutate(carrozzeria.new = case_when(
    modello %in% c("A1", "A3", "C3", "Clio", "Countryman",
                   "Fiesta", "Focus", "Golf", 
                   "Tipo", "Yaris") ~ 'Berlina',
    modello %in% c("Aygo-X", "500", "Corsa", "i10",
                   "Ignis", "208", "Panda", "Picanto",
                   "Polo", "Sandero", "Ypsilon") ~ 'City Car',
    modello %in% c("Austral", "Avenger", "C3 Aircross",
                   "Captur", "500X", "Compass", "Duster",
                   "Formentor", "GLA", "Juke", "Kuga",
                   "Mokka", "2008", "3008", "Puma",
                   "Q3", "Qashqai", "Renegade", 
                   "Sportage", "Taigo", "T-Cross",
                   "Tiguan", "Tonale", "T-Roc",
                   "Tucson", "Vitara", "X1", 
                   "Yaris Cross", "ZS") ~ 'SUV'
  ))
cars <- cars %>%
  dplyr::relocate(carrozzeria.new, .before = carrozzeria)
cars$carrozzeria = NULL
names(cars)[names(cars) == 'carrozzeria.new'] <- 'carrozzeria'
# Ora ogni auto ha il corretto tipo di carrozzeria 
# associato.


# 8) Modifica delle variabili rimanenti

table(cars$tagliandi_certificati)
cars$tagliandi_certificati[is.na(cars$tagliandi_certificati)] = "No"
cars$tagliandi_certificati[cars$tagliandi_certificati == "Sì"] = "Si"
# Tagliandi certificati.

table(cars$tipo_di_veicolo)
cars$tipo_di_veicolo[cars$tipo_di_veicolo %in% c("Aziendale", "Dimostrativo", "KM0")] = "Nuovo"
# Tipo di veicolo.

cars$altre_fonti_energetiche[is.na(cars$altre_fonti_energetiche)] = "No"
cars$altre_fonti_energetiche[cars$altre_fonti_energetiche == "Corrente elettrica "] = "Si"
names(cars)[names(cars) == 'altre_fonti_energetiche'] <- 'ibrida'
# Veicolo ibrido o no.

table(cars$tipo_di_cambio)
cars$tipo_di_cambio[cars$tipo_di_cambio == "Semiautomatico"] = "Automatico"
names(cars)[names(cars) == 'tipo_di_cambio'] <- 'cambio'
# Cambio.

table(cars$veicolo_non_fumatori)
cars$veicolo_non_fumatori[is.na(cars$veicolo_non_fumatori)] = "Non dichiarato"
cars$veicolo_non_fumatori[cars$veicolo_non_fumatori == "Sì"] = "Si"
# Veicolo non fumatori.

table(cars$usato_garantito)
cars = cars %>% 
  mutate(usato_garantito = case_when(
    usato_garantito %in% c("0 mesi", "1 mesi") ~ 'No',
    usato_garantito %in% c("6 mesi", "12 mesi", "Sì") ~ '6-12 mesi',
    usato_garantito %in% c("14 mesi", "17 mesi", "18 mesi", "19 mesi",
                           "20 mesi", "22 mesi", "23 mesi") ~ '13-23 mesi',
    usato_garantito %in% c("24 mesi", "36 mesi") ~ '24-36 mesi',
    usato_garantito %in% c("40 mesi", "48 mesi", "60 mesi", "72 mesi", 
                           "74 mesi", "84 mesi") ~ 'Oltre 36 mesi'
  ))
cars$usato_garantito[is.na(cars$usato_garantito)] = "No"
# Per convenzione si assume che la categoria "sì" vada a finire
# nei 12 mesi.

table(cars$colore_finiture_interne)
cars = cars %>% 
  mutate(colore_finiture_interne = case_when(
    colore_finiture_interne %in% c("Altro", "Arancione", "Beige",
                                   "Bianco", "Blu/Azzurro", "Giallo",
                                   "Marrone", "Rosso") ~ 'Colorate',
    colore_finiture_interne %in% c("Grigio") ~ 'Grigie',
    colore_finiture_interne %in% c("Nero") ~ 'Nere'
  ))
names(cars)[names(cars) == 'colore_finiture_interne'] <- 'finiture'
cars$finiture[is.na(cars$finiture)] = "Non specificato"
# Finiture interne.

table(cars$carburante)
cars = cars %>% 
  mutate(carburante = case_when(
    carburante %in% c("Altro", "Benzina", "Benzina (Filtro antiparticolato)",
                      "Benzina 91", "Benzina 91 (Filtro antiparticolato)",
                      "Benzina E10 91", "Super 95", "Super E10 95",
                      "Super Plus 98", "Super Plus E10 98") ~ 'Benzina',
    carburante %in% c("Diesel", "Diesel (Filtro antiparticolato)") ~ 'Gasolio',
    carburante %in% c("Gas di petrolio liquefatto",
                      "Gas naturale", "GPL (Filtro antiparticolato)",
                      "GPL", "Gas di petrolio liquefatto / Benzina 91",
                      "Gas di petrolio liquefatto / Benzina 91 / Super 95 / Super Plus 98",
                      "Gas di petrolio liquefatto / Benzina E10 91") ~ 'GPL',
    carburante %in% c("Metano") ~ 'Metano',
    carburante %in% c("Elettrica") ~ 'Elettrica'
))
# nocarb <- cars %>% filter(is.na(carburante))
# View(nocarb)
cars$carburante[is.na(cars$carburante)] = "Benzina"
# Carburante. Le auto elettriche presenti lo sono al 100% (non ibride).
# Si verifica che tutte le auto con carburante mancante sono ibride.
# Pertanto si imputa a "Benzina" il valore mancante (l'informazione
# sull'ibrido resta nella variabile dedicata).

table(cars$colore)
cars = cars %>% 
  mutate(colore = case_when(
    colore %in% c("Arancione", "Argento", "Beige",
                  "Bronzo", "Blu/Azzurro", "Giallo", "Lilla",
                  "Marrone", "Oro", "Rosso", "Verde") ~ 'Colorata',
    colore %in% c("Bianco") ~ 'Bianca',
    colore %in% c("Nero") ~ 'Nera',
    colore %in% c("Grigio") ~ 'Grigia',
  ))
cars$colore[is.na(cars$colore)] = "Non specificato"
# Colore della macchina.

table(cars$tipo_di_vernice)
cars$tipo_di_vernice[cars$tipo_di_vernice == "Altro"] = "No"
cars$tipo_di_vernice[cars$tipo_di_vernice == "Metallizzato"] = "Si"
cars$tipo_di_vernice[is.na(cars$tipo_di_vernice)] = "No"
names(cars)[names(cars) == 'tipo_di_vernice'] <- 'metallizzata'
# Tipo di vernice.

table(cars$materiale)
cars = cars %>% 
  mutate(materiale = case_when(
    materiale %in% c("Alcantara", "Altro") ~ 'Altro',
    materiale %in% c("Pelle parziale", "Pelle scamosciata",
                     "Pelle totale") ~ 'Pelle',
    materiale %in% c("Stoffa") ~ 'Stoffa'
  ))
cars$materiale[is.na(cars$materiale)] = "Altro"
# Materiale rivestimenti.

cars$emissioni_co_2_8 <- substr(cars$emissioni_co_2_8, 
                                start = 1,
                                stop = nchar(cars$emissioni_co_2_8) - 12)
cars$emissioni_co_2_8 = as.numeric(cars$emissioni_co_2_8)
cars$emissioni_co_2_8[cars$emissioni_co_2_8 < 2] = NA
cars$emissioni_co_2_8[cars$emissioni_co_2_8 > 250] = NA
cars$emissioni_co_wltp_2_8 <- substr(cars$emissioni_co_wltp_2_8 , 
                                     start = 1,
                                     stop = nchar(cars$emissioni_co_wltp_2_8 ) - 12)
cars$emissioni_co_2 <- NULL
cars$emissioni_co_wltp_2_8 = as.numeric(cars$emissioni_co_wltp_2_8)
cars <- cars %>% 
  mutate(emissioni = coalesce(emissioni_co_2_8, emissioni_co_wltp_2_8))
summary(cars$emissioni)
cars$emissioni_co_wltp_2_8 = cars$emissioni_co_2_8 = NULL
# Nuova variabile relativa alle emissioni che unisce i due contributi
# delle variabili emissioni_co_2_8 ed emissioni_co_wltp_2_8.

cars$proprietari = NULL
# Troppi NA e impossibile imputare.

table(cars$classe_emissioni)
cars = cars %>% 
  mutate(classe_emissioni = case_when(
    classe_emissioni %in% c("Euro 2", "Euro 3", "Euro 4") ~ 'Euro 4 o meno',
    classe_emissioni %in% c("Euro 5") ~ 'Euro 5',
    classe_emissioni %in% c("Euro 6", "Euro 6c") ~ 'Euro 6',
    classe_emissioni %in% c("Euro 6d", "Euro 6d-TEMP", "Euro 6e") ~ 'Euro 6 Plus',
  ))
# Classe di emissioni.

cars <- cars %>% 
  mutate(classe_emissioni = case_when(
    is.na(classe_emissioni) & anno_prod %in% c("1990", "1998", "2008", "2009") ~ 'Euro 4 o meno',
    is.na(classe_emissioni) & anno_prod %in% c("2010", "2011", "2012", "2013", "2014") ~ 'Euro 5',
    is.na(classe_emissioni) & anno_prod %in% c("2015", "2016", "2017", "2018", "2018") ~ 'Euro 6',
    is.na(classe_emissioni) & anno_prod %in% c("2019", "2020", "2021", "2022", "2023", "2024") ~ 'Euro 6 Plus',
    TRUE ~ classe_emissioni
  ))
# La classe di emissioni è un dato che si può imputare a partire
# dalla data di costruzione della macchina.

cars$consumo_di_carburante2_8 <- substr(cars$consumo_di_carburante2_8,
                                        start = 1, stop = 3)
cars$consumo_di_carburante_wltp_2_8 <- substr(cars$consumo_di_carburante_wltp_2_8,
                                              start = 1, stop = 3)
cars$consumo_di_carburante2_8 <- gsub(" l", "",
                                      cars$consumo_di_carburante2_8)
cars$consumo_di_carburante_wltp_2_8 <- gsub(" l", "",
                                            cars$consumo_di_carburante_wltp_2_8)
cars$consumo_di_carburante2_8 <- gsub(",", ".",
                                      cars$consumo_di_carburante2_8)
cars$consumo_di_carburante_wltp_2_8 <- gsub(",", ".", 
                                            cars$consumo_di_carburante_wltp_2_8)
cars <- cars %>% 
  mutate(consumi = coalesce(consumo_di_carburante2_8,
                            consumo_di_carburante_wltp_2_8))
cars$consumi = as.numeric(cars$consumi)
cars$consumi[cars$consumi == 0] = NA
cars$consumo_di_carburante2_8 = cars$consumo_di_carburante_wltp_2_8 = NULL
# Costruzione di una nuova variabile relativa ai consumi.


# 10) Da stringa con optional a variabili indicatrici ------

optional <- unlist(strsplit(as.character(cars$optional), "\\*"))
# Lista con optional, intanto si cancellano quelli lunghissimi
# che corrispondono a frasi e non ad optional.

top.optional <- names(sort(table(optional), decreasing = T)[14:58])
# Questi sono gli optional più diffusi (tolti i più banali).

cars$autoradio = NA
cars$isofix = NA
cars$park.distance.control = NA
cars$cruise.control = NA
cars$fendinebbia = NA
cars$bluetooth = NA
cars$volante.in.pelle = NA
cars$climatizzatore = NA
cars$sensori.parcheggio = NA
cars$volante.multifunzione = NA
cars$usb = NA
cars$computer.di.bordo = NA
cars$controllo.pressione.gomme = NA
cars$sedile.posteriore.sdoppiato = NA
cars$cerchi.in.lega = NA
cars$controllo.automatico.trazione = NA
cars$immobilizer = NA
cars$airbag.testa = NA
cars$bracciolo = NA
cars$sensore.luminosita = NA
cars$start.stop = NA
cars$navigatore = NA
cars$autoradio.digitale = NA
cars$frenata.emergenza = NA
cars$touch.screen = NA
cars$chiusura.centralizzata.telecomandata = NA
cars$sensore.pioggia = NA
cars$clima.automatico = NA
cars$mp3 = NA
cars$fari.led = NA
cars$vivavoce = NA
cars$controllo.elettronico.corsia = NA
cars$android.auto = NA
cars$apple.car.play = NA
cars$hill.holder = NA
cars$telecamera.posteriore = NA
cars$cruise.control = NA
cars$vetri.oscurati = NA
cars$sound.system = NA
cars$luci.diurne.led = NA
cars$luci.diurne = NA
cars$riconoscimento.segnali = NA
cars$sensori.parcheggio.anteriori = NA
cars$kit.antipanne = NA
cars$antifurto = NA
# Si creano le variabili corrispondenti agli optional.

start = which(colnames(cars) == "autoradio")
for (i in 1:NROW(cars)) {
  for (j in start:NCOL(cars)) {
    valore = ifelse(grep(top.optional[j-start+1], cars[i, "optional"]) == 0,
                    0, 1) 
    if (!is.na(valore) && length(valore) > 0) {
      cars[i, j] <- 1
    } else {
      cars[i, j] <- 0  
    }
  }
}
cars$optional = NULL
# Adesso vi sono delle variabili indicatrici che indicano se ogni macchina
# ha o no un certo optional.


# --------

library(DataExplorer)
plot_missing(cars)
