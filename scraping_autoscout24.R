# ------ SCRAPING ------

library(rvest)
library(xml2)
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


# ------ PREZZO SECONDO AUTOSCOUT24 ------ 

# La prima Panda       -> "Superprezzo".
# La seconda Dacia     -> "Ottimo prezzo".
# La quinta Lancia Y   -> "Buon prezzo".
# La terza Yaris       -> "Buon prezzo".
# La prima 500         -> "Superprezzo".
# La quintultima T-Roc -> "Ottimo prezzo".


# ------ PULIZIA DEL DATASET ------

cars <- read.csv(file.choose())
colnames(cars)
# Dataset "macchine_scraping.csv".

# 1) Eliminazione colonne inutili ------

cars[, 57:69] = NULL
cars[, 5:11]  = NULL
cars$offerta_n = cars$costo_del_trasferimento = NULL
cars$acconto = NULL
# Le colonne relative alla rateizzazione e ai prezzi sono leaker.

cars$comfort = cars$intrattenimento_media = NULL
cars$extra = cars$sicurezza =  NULL
cars$versione_per_nazione = NULL
# Gli optional sono nell'apposita colonna.


# 2) Trasformazione nomi dei modelli -------

panda <- grep("Fiat", cars$marca)[1:19] #ok
sandero <- grep("Dacia", cars$marca)[1:19] #ok
ypsilon <- grep("Lancia", cars$marca) #ok
yaris <- grep("Toyota", cars$marca)[1:19] #ok
cinquecento <- grep("Fiat", cars$marca)[20:38] #ok
troc <- grep("Volkswagen", cars$marca)[1:18]
captur <- grep("Renault", cars$marca)[1:19]
c3 <- grep("Citroen", cars$marca)[1:19]
puma <- grep("Ford", cars$marca)[1:19]
duster <- grep("Dacia", cars$marca)[20:38]
renegade <- grep("Jeep", cars$marca)[1:19]
cinquecentox <- grep("Fiat", cars$marca)[39:57] #ok
clio <- grep("Renault", cars$marca)[20:38]
p208 <- grep("Peugeot", cars$marca)[1:18]
p2008 <- grep("Peugeot", cars$marca)[19:37]
yariscross <- grep("Toyota", cars$marca)[20:36] #ok
corsa <- grep("Opel", cars$marca)[1:18] #ok
avenger <- grep("Jeep", cars$marca)[20:38] #ok
compass <- grep("Jeep", cars$marca)[39:57] #ok
tcross <- grep("Volkswagen", cars$marca)[19:56] #ok
qashqai <- grep("Nissan", cars$marca)[1:19] #ok
zs <- grep("MG", cars$marca) #ok
sportage <- grep("Kia", cars$marca)[1:17] #ok
p3008 <- grep("Peugeot", cars$marca)[38:56] #ok
tonale <- grep("Alfa Romeo", cars$marca) #ok
polo <- grep("Volkswagen", cars$marca)[57:75] #ok
kuga <- grep("Ford", cars$marca)[20:38] #ok
tiguan <- grep("Volkswagen", cars$marca)[76:93] #ok
focus <- grep("Ford", cars$marca)[39:57] #ok
aygox <- grep("Toyota", cars$marca)[37:55] #ok
q3 <- grep("Audi", cars$marca)[1:14] #ok
x1 <- grep("BMW", cars$marca) #ok
i10 <- grep("Hyundai", cars$marca)[1:19] #ok
tucson <- grep("Hyundai", cars$marca)[20:38] #ok
c3aircross <- grep("Citroen", cars$marca)[20:36] #ok
formentor <- grep("CUPRA", cars$marca) #ok
gla <- grep("Mercedes", cars$marca) #ok
fiesta <- grep("Ford", cars$marca)[58:76] #ok
picanto <- grep("Kia", cars$marca)[18:36] #ok
juke <- grep("Nissan", cars$marca)[20:36] #ok
a3 <- grep("Audi", cars$marca)[15:33] #ok
mokka <- grep("Opel", cars$marca)[19:37] #ok
ignis <- grep("Suzuki", cars$marca)[1:18] #ok
tipo <- grep("Fiat", cars$marca)[58:76] #ok
golf <- grep("Volkswagen", cars$marca)[94:111] #ok
austral <- grep("Renault", cars$marca)[39:43] #ok
a1 <- grep("Audi", cars$marca)[34:51] #ok
countryman <- grep("MINI", cars$marca) #ok
taigo <- grep("Volkswagen", cars$marca)[112:129]# ok
vitara <- grep("Suzuki", cars$marca)[19:37] #ok
# Vettori di indici.

indici <- list(a1, a3, austral, avenger, aygox, c3, c3aircross,
               captur, cinquecento, cinquecentox, clio, compass,
               corsa, countryman, duster, fiesta, focus, formentor,
               gla, golf, i10, ignis, juke, kuga, mokka,
               p2008, p208, p3008, panda, picanto, polo, puma,
               q3, qashqai, renegade, sandero, sportage, taigo,
               tcross, tiguan, tipo, tonale, troc, tucson,
               vitara, x1, yaris, yariscross, ypsilon, zs)
labels <- c("A1", "A3", "Austral", "Avenger", "Aygo-X", "C3", "C3 Aircross",
            "Captur", "500", "500X", "Clio", "Compass",
            "Corsa", "Countryman", "Duster", "Fiesta", "Focus", "Formentor",
            "GLA", "Golf", "i10", "Ignis", "Juke", "Kuga", "Mokka",
            "2008", "208", "3008", "Panda", "Picanto", "Polo", "Puma",
            "Q3", "Qashqai", "Renegade", "Sandero", "Sportage", "Taigo",
            "T-Cross", "Tiguan", "Tipo", "Tonale", "T-Roc", "Tucson",
            "Vitara", "X1", "Yaris", "Yaris Cross", "Ypsilon", "ZS")
# Conversione in lista dei vettori di indici e labels di ogni modello.

for (i in 1:length(indici)) {
  cars$modello[indici[[i]]] <- labels[i]
}
# Assegnazione ad ogni macchina del nome del modello.


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


# 5) Variabili inutili ------

cars$cambio_cinghia_distribuzione = cars$disponibilita = NULL
cars$colore_specifico = NULL

# cars$paese <- gsub(", IT", "", cars$paese)


# 6) Da stringa con optional a variabili indicatrici (DA FARE) ------

optional <- unique(unlist(strsplit(as.character(cars$optional), "\\*")))
# Lista con optional, intanto si cancellano quelli lunghissimi
# che corrispondono a frasi e non ad optional.

contiene_data <- function(acc) {
  if (grepl("\\b\\d{2}/\\d{2}/\\d{4}(\\s.+)?\\b",
            acc)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

parole.da.eliminare <- c("audi" , "bmw" , "peugeot" , "fiat" , "[" ,
                           "dsg" , "garanzia" , "climatronic" , "sanifichiamo" ,
                           "dotazione" , "(" , "+" , "sabato" , "fatta" , 
                           "facebook" , "num" , "volcano" , "''" , 
                           "selec-terrain" , "uconnect" , "canali" , "vie" ,
                           "jbl" , "virtual" , ")" , "]" , "sentry" ,
                           "valutare" , "qualità" , "garantite" , "via" ,
                           "offerta" , "serie" , "simone" , "finanziamenti" ,
                           "/" , "glossy" , "cluster" , "ice" , "60/40" ,
                           "garantiamo" , "prepariamo" , "esperti" , "consegna" ,
                           "velocitaà" , "vw" , "lunedì" , "telefono" , "@" ,
                           "email" , "escluso" , "consegna" , "autosalone" ,
                           "finanziamento" , "condizioni" , "," , "tagliandi" ,
                           "tagliando" , "sanificazione" , "r-line" , "we" ,
                           "dt270rw" , "telaio" , "revisione" , "piaciuta" ,
                           "wheels" , "acoustic" , "active" , "armrest" ,
                           "tailgate" , "blow-by" , "connecteddrive" , "co2" ,
                           "braking" , "eu-" , "satinised" , "tagliandata" ,
                           "x19" , "1.400" , "crdi" , "grandinata" , "vero" ,
                           "sostituito" , "minerale" , "ctr." , "radioricevitore" ,
                           "glonass" , "sx" , "anthracite" , "steptronic" , 
                           "oil" , "lumbar" , "edrive")

contiene_nomi <- function(acc, nomi){
  if (grepl(paste(nomi, collapse = "|"), acc)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

normalize.optional <- function(op){
  op <- op[nchar(op < 50)]
  op <- tolower(op)
  op <- op[!sapply(op, contiene_data)]  
  #op <- op[!sapply(op, contiene_nomi, nomi = parole.da.eliminare)]
  op <- unique(op)
}

optional <- normalize.optional(optional)

optional <- optional[nchar(optional) < 50]
sort(optional)
optional <- optional[-c(1:17)]
optional <- tolower(optional)
optional <- unique(optional)
