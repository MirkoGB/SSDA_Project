library(tidyverse)
cars <- read.csv(file.choose(), stringsAsFactors = T)
# Lettura dati.

summary(cars$prezzo_auto)
pr.cat <- cut(cars$prezzo_auto,
              breaks = c(0,
                         quantile(cars$prezzo_auto, 1/5),
                         quantile(cars$prezzo_auto, 2/5),
                         quantile(cars$prezzo_auto, 3/5),
                         quantile(cars$prezzo_auto, 4/5),
                         quantile(cars$prezzo_auto, 5/5)),
              labels = c("Molto economica", "Economica", "Prezzo medio",
                         "Costosa", "Molto costosa"))
pr.cat <- factor(pr.cat, ordered = T)
# La variabile prezzo_auto viene resa come fattore ordinato.

cars$chilometraggio <- as.numeric(scale(cars$chilometraggio))
cars$kW = NULL
cars$anno_prod <- cars$anno_prod - 1990
cars$cilindrata <- as.numeric(scale(cars$cilindrata))
cars$peso <- as.numeric(scale(cars$peso))
# La variabile chilometraggio viene riscalata per evitare problemi
# di convergenza.
# La variabile kW viene rimossa poiché può essere considerata come
# una versione riscalata di CV.
# Ai valori della variabile anno_prod viene sottratto l'anno di
# produzione meno recente registrato.

set.seed(44)
mod.full <- MASS::polr(pr.cat ~ ., data = cars[, c(4, 6:75)])
summary(mod.full)
# Modello per logit cumulati con tutte le variabili.

set.seed(44)
mod.null <- MASS::polr(pr.cat ~ 1, data = cars[, c(4, 6:75)])
summary(mod.null)
# Modello per logit cumulati con la sola intercetta.

set.seed(44)
mod.step <- step(mod.null, 
                 scope = formula(mod.full),
                 k = log(nrow(cars)),
                 direction = "forward")
# Forward stepwise.

mod.finale <- MASS::polr(pr.cat ~ anno_prod + cv + chilometraggio + marce +
                           carrozzeria + bracciolo + cilindrata +
                           computer.di.bordo + cerchi.in.lega + 
                           isofix + fendinebbia + mp3 +
                           volante.multifunzione + per_neopatentati + 
                           cilindri + start.stop + peso + hill.holder +
                           consumi + riconoscimento.segnali + 
                           vetri.oscurati + luci.diurne.led,
                         data = cars[, c(4, 6:75)])
summary(mod.finale)
# Modello selezionato dopo la forward stepwise.

xtable::xtable(summary(mod.finale)$coef)
