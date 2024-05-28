library(arules)
library(arulesViz)
library(dplyr)

setwd("C:/Users/pippo/OneDrive/Documenti/strumenti-statistici-dati-aziendali/autoscout24")
cars <- read.csv("cars-clean-v2-imputed.csv", stringsAsFactors = TRUE)
optional <- cars[, 32:76]
str(optional)
# Lettura dei dati sugli optional.

optional[, colnames(optional)] <- lapply(optional[, colnames(optional)], 
                                         factor)
optional <- as(optional, "transactions")
# La matrice con gli optional viene resa come un oggetto della
# classe "transactions".

f.itemset <- apriori(optional, 
                     parameter = list(supp = 0.5, conf = 0.9, 
                                      target = "rules"))
inspect(f.itemset)
# Ricerca di regole con supporto maggiore di 0.5 e fiducia
# maggiore di 0.9.

inspect(sort(subset(f.itemset, subset = lift > 1), by = "confidence"))
# Si selezionano solo le regole con lift superiore a 1
# e le si ordinano per fiducia decrescente.

f.itemset <- apriori(optional, 
                     parameter = list(supp = 0.5, conf = 0.7, 
                                      target = "rules"))
inspect(f.itemset)
# Ricerca di regole con supporto maggiore di 0.5 e fiducia
# maggiore di 0.9.

inspect(sort(subset(f.itemset, subset = lift > 1.2), by = "confidence"))
# Si selezionano solo le regole con lift superiore a 1
# e le si ordinano per fiducia decrescente.

important_rules <- subset(f.itemset, subset = lift > 3)
plot(important_rules)

plot(important_rules, method = "graph")
plot(important_rules, method = "graph", engine = "interactive")
plot(important_rules, method = "graph", engine = "visNetwork")
