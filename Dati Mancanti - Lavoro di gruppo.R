#Imputazione dati mancanti
library(dplyr)
library(readr)
library(tidyr)


data = read_csv("cars-clean-v2 (1).csv")

str(data)

#trovare il numero di dati mancanti per ogni variabile

sort(colSums(is.na(data)), decreasing = T)


# Emissioni ---------------------------------------------------------------

#*Idea per le emissioni: classi di emissioni simili avranno emissioni mediane simili
#*Quindi si può ragionevolmente imputare i dati mancanti per le emissioni basandosi sulla sulla classe
#*<<- Assegnazione globale della variabile

emissioni_imputation = function(data){
  table(data$classe_emissioni)
  
  t1 <<- data %>% group_by(classe_emissioni) %>% summarise(median_emissioni = median(emissioni, na.rm = T))
  
  #vediamo il subset dei dati dove abbiamo le variabili "modello auto" "prezzo" "classe emissioni" "emissioni"
  
  t2 <<- data %>% select(modello, prezzo_auto, classe_emissioni, emissioni)
  
  t3 <<- t2 %>%
    group_by(classe_emissioni) %>%
    summarise(mean_emissioni = mean(emissioni, na.rm = T))
  
  #per ogni classe di emissioni fare il grafico di densità
  
  library(ggplot2)
  ggplot(t2, aes(x = emissioni, fill = classe_emissioni)) + 
    geom_density(alpha = 0.5)+
    theme_minimal()
  
  #fare un istogramma, dove per ogni classe di emissioni è un grafico diverso
  
  p = ggplot(t2, aes(x = emissioni)) + 
    geom_histogram(fill = "blue", color = "black", bins = 30) + 
    facet_wrap(~classe_emissioni) + 
    theme_minimal()+
    #inserisci la mediana
    geom_vline(data = t1, aes(xintercept = median_emissioni, color = classe_emissioni), linetype = "dashed")+
    scale_color_manual(values = hcl.colors(4, palette = "Teal"))+
    guides(color = guide_legend(title = "Mediana Classe Emissioni"))+
    #aggiungo la media
    geom_vline(data = t3, aes(xintercept = mean_emissioni, color = classe_emissioni), linetype = "dotted")
  
  print(p)
  return(t3)
}

emissioni_imputation(data)
#la media è molto simile alla mediana -> direi che si possono imputare le emissioni mancanti con la mediana per quella classe di emissioni
#imputazione
data_old = data
data_new = data

data_new$emissioni = ifelse(is.na(data_new$emissioni), t1$median_emissioni[match(data_new$classe_emissioni, t1$classe_emissioni)], data_new$emissioni)
#creo un istogramma per vedere se l'imputazione ha funzionato e come sono cambiati gli istogrammi rispetto a prima

ggplot(data_old, aes(x = emissioni)) + 
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  geom_histogram(data = data_new, aes(x = emissioni), fill = "red", color = "black", bins = 30, alpha = 0.3) +
  facet_wrap(~classe_emissioni) + 
  theme_minimal()

#* Commento sull'imputazione
#* Secondo me si può fare ma non per l'Euro 6 e l'Euro 6 plus che probabilmente richiede un'imputazione diversa
#* Magari possiamo imputare per il modello
#* 
#* 

emissioni_imputation_V2 = function(data){
  table(data$modello)
  
  t1 <<- data %>% group_by(modello) %>% summarise(median_emissioni = median(emissioni, na.rm = T))
  
  #vediamo il subset dei dati dove abbiamo le variabili "modello auto" "prezzo" "classe emissioni" "emissioni"
  
  t2 <<- data %>% select(modello, prezzo_auto, modello, emissioni)
  
  t3 <<- t2 %>%
    group_by(modello) %>%
    summarise(mean_emissioni = mean(emissioni, na.rm = T))
  
  #per ogni classe di emissioni fare il grafico di densità
  
  library(ggplot2)
  ggplot(t2, aes(x = emissioni, fill = modello)) + 
    geom_density(alpha = 0.5)+
    theme_minimal()
  
  #fare un istogramma, dove per ogni classe di emissioni è un grafico diverso
  
  p = ggplot(t2, aes(x = emissioni)) + 
    geom_histogram(fill = "blue", color = "black", bins = 30) + 
    facet_wrap(~modello) + 
    theme_minimal()+
    #inserisci la mediana
    geom_vline(data = t1, aes(xintercept = median_emissioni, color = modello), linetype = "dashed")+
    scale_color_manual(values = hcl.colors(50, palette = "Teal"))+
    guides(color = guide_legend(title = "Mediana Classe Emissioni"))+
    #aggiungo la media
    geom_vline(data = t3, aes(xintercept = mean_emissioni, color = modello), linetype = "dotted")
  
  print(p)
  return(t3)
}
emissioni_imputation_V2(data)

data_old = data
data_new = data

data_new$emissioni = ifelse(is.na(data_new$emissioni), t1$median_emissioni[match(data_new$modello, t1$modello)], data_new$emissioni)

ggplot(data_old, aes(x = emissioni)) + 
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  geom_histogram(data = data_new, aes(x = emissioni), fill = "red", color = "black", bins = 30, alpha = 0.3) +
  facet_wrap(~modello) + 
  theme_minimal()

#* forse fare l'imputazione per modello è meglio
#* 



# Consumi -----------------------------------------------------------------


#* Idea per i consumi, macchine dello stesso tipo consumeranno uguale
#* Quindi creo un grafico di densità dei consmi diviso per modello

t1 = data %>% group_by(modello) %>% summarise(media_consumo = mean(consumi, na.rm = T))


#quanti valori diversi di consumo diversi ci sono per ogni modello di auto?

t2 = data %>% group_by(modello) %>% summarise(n_distinct_consumi = n_distinct(consumi, na.rm = T))

#e qual'è il coefficiente di variazione per ogni modello di auto?

t3 = data %>% group_by(modello) %>% summarise(coefficiente_variazione = sd(consumi, na.rm = T)/mean(consumi, na.rm = T))

#vedo come sono i consumi per ogni modello di auto

t4 = data %>% select(modello, consumi)

ggplot(t4[which(t4$modello == "208"),], aes(x = consumi, fill = modello)) + 
  geom_density(alpha = 0.5)+
  theme_minimal()

#* Ad esempio per la Pegueot 208 quale potrebbe essere un buon metodo di imputazione data la densità

mean_consumo_208 = mean(t4[which(t4$modello == "208"),]$consumi, na.rm = T)
median_consumo_208 = median(t4[which(t4$modello == "208"),]$consumi, na.rm = T)

#la media e la mediana sono simili, quindi si può imputare con la media

#per ogni modello diverso di auto, vorrei imputare i valori con la media

consumi_imputation = function(data){
  t1 <<- data %>% group_by(modello) %>% summarise(mean_consumi = mean(consumi, na.rm = T))
  
  t2 <<- data %>% select(modello, consumi)
  
  ggplot(t2, aes(x = consumi, fill = modello)) + 
    geom_density(alpha = 0.5)+
    theme_minimal()
  
  return(t1)
}

consumi_imputation(data)

data_old = data


data_new$consumi = ifelse(is.na(data_new$consumi), t1$mean_consumi[match(data_new$modello, t1$modello)], data_new$consumi)

ggplot(data_old, aes(x = consumi)) + 
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  geom_histogram(data = data_new, aes(x = consumi), fill = "red", color = "black", bins = 30, alpha = 0.3) +
  facet_wrap(~modello) + 
  theme_minimal()




# Trazione ----------------------------------------------------------------


t1 = data %>% select(trazione, modello, prezzo_auto, chilometraggio)

#* vedere per ogni modello di auto quali sono le trazioni più comuni e qual è il prezzo medio e conta quanti ce ne sono
#* 

t2 = t1 %>% group_by(modello, trazione) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = T), n = n())
t3 = t1 %>% group_by(modello, trazione) %>% summarise(mean_chilometraggio = mean(chilometraggio, na.rm = T), n = n())

#* Una possibile soluzione è quella di basarsi sul prezzo medio per quella trazione e per quel modello di auto
#* Potrebbe essere ragionevole assumere che se gli NA hanno prezzo simile alle "categorie di trazione" allora quei valori mancanti potrebbero essere imputati con quella categoria di trazione stessa

#* Per esempio per la Peugeot 208, se il prezzo medio per la trazione è simile, allora si può imputare con quella trazione

t4 = t1 %>% filter(modello == "208")

#scagliona ogni riga e fai un check se c'è o meno NA sotto la variabile trazione, se esso è presente allora vai a inserire il valor di trazioni mediamente più simile al chilometraggio
which(is.na(t4$trazione))

#prendo valori unici di t4$trazione NA esclusi

unique(t4$trazione[which(!is.na(t4$trazione))])

for(i in which(is.na(t4$trazione))){
  if(length(unique(t4$trazione[which(!is.na(t4$trazione))]) == 1)){
    t4$trazione[i] = unique(t4$trazione[which(!is.na(t4$trazione))])
  } else {
    t4$trazione[i] = t4$trazione[which(!is.na(t4$trazione))][which.min(abs(t3$mean_chilometraggio - t3$mean_chilometraggio[i]))]
  }
}

trazione_imputation = function(data, modello){
  t1 <<- data %>% select(trazione, modello, prezzo_auto, chilometraggio)
  
  t2 <<- t1 %>% group_by(modello, trazione) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = T), n = n())
  t3 <<- t1 %>% group_by(modello, trazione) %>% summarise(mean_chilometraggio = mean(chilometraggio, na.rm = T), n = n())
  
  for(i in which(is.na(t3$trazione))){
    if(length(unique(t3$trazione[which(!is.na(t3$trazione))]) == 1)){
      t3$trazione[i] = unique(t3$trazione[which(!is.na(t3$trazione))])
    } else {
      t3$trazione[i] = t3$trazione[which(!is.na(t3$trazione))][which.min(abs(t3$mean_chilometraggio - t3$mean_chilometraggio[i]))]
    }
  }
  
  return(t3)
}

trazione_imputation(data, "208")

#applio a tutti i modelli

data_old = data

for(modello in unique(data$modello)){
  data_new$trazione = ifelse(data_new$modello == modello & is.na(data_new$trazione), trazione_imputation(data_new, modello)$trazione, data_new$trazione)
}

cbind(data_old$trazione, data_new$trazione)

ggplot(data_old, aes(x = prezzo_auto, y = chilometraggio, color = trazione)) + 
  geom_point() + 
  theme_minimal()+
  geom_point(data = data_new[which(is.na(data_old$trazione)),], mapping = aes(x = prezzo_auto, y = chilometraggio, color = trazione), size = 7,alpha = 0.3)
  

# Cilindri ----------------------------------------------------------------
#sostituisco tutte le lettere NA con il vero NA

data$cilindri = ifelse(data$cilindri == "NA", NA, data$cilindri)
#per ogni riga di data, se il numero di cilindri è compreso fra 0 ed 1 allora lo sostituisco con il numero di cilindri più frequente per quel modello
data$cilindri = ifelse(data$cilindri %in% c("0", "1"), NA, data$cilindri)
data$cilindri = ifelse(data$cilindri %in% c(0,1), NA, data$cilindri)


t1 = data %>% select(cilindri, modello, prezzo_auto, chilometraggio)

# Se il numero di cilindri di un certo modello è pari a 0 a 1 allora non va bene, si modifica il numero di cilindri con il numero di cilindri più frequente per quel modello

t2 = t1 %>% group_by(modello, cilindri) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = T), n = n())

# Numero cilindri fra 0 ed 1 e li sostituisco con il numero di cilindri più frequente per quel modello

t2$numero_cilindri = ifelse(t2$cilindri %in% c(0, 1), names(sort(table(t2$cilindri), decreasing = T)[1]), t2$cilindri)

t3 = t2 %>% select(modello, numero_cilindri, n, mean_prezzo)

#imputa gli NA con il numero di cilindri più frequente per quel modello

t4 = t3 %>% group_by(modello, numero_cilindri) %>% summarise(n = sum(n))

#per ogni modello trova il numero di cilindri più frequente

t5 = t4 %>% group_by(modello) %>% summarise(numero_cilindri = numero_cilindri[which.max(n)])

#imputa i valori mancanti con il numero di cilindri più frequente per quel modello

cilindri_imputation = function(data){
  
  t1 <<- data %>% select(cilindri, modello, prezzo_auto, chilometraggio)
  
  t2 <<- t1 %>% group_by(modello, cilindri) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = T), n = n())
  
  t2$numero_cilindri = ifelse(t2$cilindri %in% c(0, 1), names(sort(table(t2$cilindri), decreasing = T)[1]), t2$cilindri)
  
  t3 <<- t2 %>% select(modello, numero_cilindri, n, mean_prezzo)
  
  t4 <<- t3 %>% group_by(modello, numero_cilindri) %>% summarise(n = sum(n))
  
  t5 <<- t4 %>% group_by(modello) %>% summarise(numero_cilindri = numero_cilindri[which.max(n)])
  
  for(modello in unique(data$modello)){
    data$cilindri = ifelse(data$modello == modello & is.na(data$cilindri), t5$numero_cilindri[match(modello, t5$modello)], data$cilindri)
  }
  
  return(t5)
}

cilindri_imputation(data)

#* Commento sull'imputazione

data_old = data


for(modello in unique(data$modello)){
  data_new$cilindri = ifelse(data_new$modello == modello & is.na(data_new$cilindri), cilindri_imputation(data_new)$numero_cilindri[match(modello, cilindri_imputation(data_new)$modello)], data_new$cilindri)
}

cbind(data_old$cilindri, data_new$cilindri)



# Marce --------------------------------------------------------------------



data$marce = ifelse(data$marce == "NA", NA, data$marce)

# Creazione di t1
t1 <- data %>% 
  select(marce, modello, prezzo_auto, chilometraggio, kW, cilindri)

# Calcolo di t2 e t3 con le medie e il conteggio
t2 <- t1 %>% 
  group_by(modello, marce) %>% 
  summarise(mean_prezzo = mean(prezzo_auto, na.rm = TRUE), n = n())

t3 = t1 %>% 
  group_by(modello, marce) %>% 
  summarise(mean_chilometraggio = mean(chilometraggio, na.rm = TRUE), n = n())

# Calcolo di t4 per determinare la marcia con il prezzo medio più alto per modello
t4 = t2 %>% 
  group_by(modello) %>% 
  summarise(marce = marce[which.max(mean_prezzo)], .groups = 'drop')

# Eliminazione delle righe con NA in t4, utilizzando un'altra marcia disponibile con prezzo alto
t4 = t4 %>% 
  mutate(marce = ifelse(is.na(marce), t2 %>% 
                          filter(modello == first(modello), !is.na(marce)) %>% 
                          summarise(marce = marce[which.max(mean_prezzo)]) %>% 
                          pull(marce), marce))

#creo la funzione marce_imputation

marce_imputation = function(data){
  
  t1 <<- data %>% select(marce, modello, prezzo_auto, chilometraggio, kW, cilindri)
  
  t2 <<- t1 %>% group_by(modello, marce) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = TRUE), n = n())
  
  t3 <<- t1 %>% group_by(modello, marce) %>% summarise(mean_chilometraggio = mean(chilometraggio, na.rm = TRUE), n = n())
  
  t4 <<- t2 %>% group_by(modello) %>% summarise(marce = marce[which.max(mean_prezzo)], .groups = 'drop')
  
  t4 <<- t4 %>% mutate(marce = ifelse(is.na(marce), t2 %>% filter(modello == first(modello), !is.na(marce)) %>% summarise(marce = marce[which.max(mean_prezzo)]) %>% pull(marce), marce))
  
  return(t4)
}


# Imputazione finale nel dataset originale
data_new = data %>% 
  left_join(t4, by = "modello") %>% 
  mutate(marce = ifelse(is.na(marce.x), marce.y, marce.x)) %>%
  select(-marce.y)

# Visualizzazione del risultato
head(data)

cbind(data$marce, data_new$marce)

#* Commento sull'imputazione
#* In pratica abbiamo preso, per ogni modello il numero di marce che costava di più mediamente, e lo abbiamo imputato con quello

# Preparazione dei dati
data_before = data %>% 
  mutate(Stato = "Prima")

data_after = data_new %>% 
  mutate(Stato = "Dopo")

combined_data = bind_rows(data_before, data_after)

# Grafico che mostra i risutaati dell'imputazione
ggplot(combined_data, aes(x = marce, fill = Stato)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Prima" = "steelblue", "Dopo" = "darkred")) +
  ggtitle("Confronto della distribuzione delle marce prima e dopo l'imputazione") +
  theme_minimal() +
  labs(x = "Marce", y = "Frequenza") +
  theme(legend.title = element_blank())



# Cilindrata --------------------------------------------------------------


data$cilindrata = ifelse(data$cilindrata == "NA", NA, data$cilindrata)

#La cilindrata è correlata tanto con peso e cilindri -> meglio cilindri, possiamo optare per un'imputazione per regressione

#faccio quindi una regressione lineare a 2 variabili, usando le osservazioni complete (escludendo quindi i gli NA anche di cilindri (peso non li ha))
#Quindi, o possiamo usare cilindri imputati

# 1. Preparazione dei dati: Rimozione di eventuali NA nelle variabili predittive
data_complete = data %>%
  filter(!is.na(cilindri) & !is.na(peso))

# 2. Creazione del modello di regressione con le osservazioni complete
model = lm(cilindrata ~ cilindri + peso, data = data_complete, na.action = na.exclude)

# 3. Preparazione dei dati per l'imputazione: selezione delle righe con cilindrata NA
data_to_impute = data %>%
  filter(is.na(cilindrata))

# 4. Predizione dei valori mancanti utilizzando il modello creato
predicted_values = predict(model, newdata = data_to_impute)

# 5. Imputazione dei valori predetti nel dataset originale
data_imputed = data %>%
  mutate(cilindrata = ifelse(is.na(cilindrata), predict(model, newdata = .), cilindrata))

data_imputed = data %>%
  mutate(
    cilindrata_original = cilindrata,  # Salva i valori originali
    cilindrata = ifelse(is.na(cilindrata), predict(model, newdata = .), cilindrata),
    Imputato = ifelse(is.na(cilindrata_original), "Imputato", "Originale")
  )

# Grafico
ggplot(data_imputed, aes(x = peso, y = cilindrata, color = Imputato)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("Originale" = "blue", "Imputato" = "red")) +
  labs(
    title = "Confronto Cilindrata vs Peso Prima e Dopo l'Imputazione",
    x = "Peso",
    y = "Cilindrata",
    color = "Stato"
  ) +
  theme_minimal()

##oppure possiamo usare solo il peso come variabile predittiva

# Preparazione dei dati: Rimozione di eventuali NA nella variabile predittiva 'peso'
data_complete = data %>%
  filter(!is.na(peso))

# Creazione del modello di regressione con le osservazioni complete
model = lm(cilindrata ~ peso, data = data_complete, na.action = na.exclude)

# Preparazione dei dati per l'imputazione: selezione delle righe con cilindrata NA
data_to_impute = data %>%
  filter(is.na(cilindrata))

# Predizione dei valori mancanti utilizzando il modello creato
predicted_values = predict(model, newdata = data_to_impute)

# Imputazione dei valori predetti nel dataset originale
data_imputed = data %>%
  mutate(
    cilindrata_original = cilindrata,  # Salva i valori originali
    cilindrata = ifelse(is.na(cilindrata), predict(model, newdata = .), cilindrata),
    Imputato = ifelse(is.na(cilindrata_original), "Imputato", "Originale")
  )

# Grafico che mostra la differenza tra i valori originali e quelli imputati
ggplot(data_imputed, aes(x = peso, y = cilindrata, color = Imputato)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("Originale" = "blue", "Imputato" = "red")) +
  labs(
    title = "Confronto Cilindrata vs Peso Prima e Dopo l'Imputazione",
    x = "Peso",
    y = "Cilindrata",
    color = "Stato"
  ) +
  geom_smooth(method = "loess", se = FALSE)+
  theme_minimal()

###proviamo ad imputare con il loess?

# Preparazione dei dati: Rimozione di eventuali NA nella variabile predittiva 'peso'
data_complete = data %>%
  filter(!is.na(peso) & !is.na(cilindrata))  # Manteniamo solo le righe complete

# Creazione del modello loess con le osservazioni complete
loess_model = loess(cilindrata ~ peso, data = data_complete)

# Preparazione dei dati per l'imputazione: selezione delle righe con cilindrata NA
data_to_impute = data %>%
  filter(is.na(cilindrata))

# Predizione dei valori mancanti utilizzando il modello loess creato
predicted_values = predict(loess_model, newdata = data_to_impute)

# Imputazione dei valori predetti nel dataset originale
data_imputed = data %>%
  mutate(
    cilindrata_original = cilindrata,  # Salva i valori originali
    cilindrata = ifelse(is.na(cilindrata), predict(loess_model, newdata = .), cilindrata),
    Imputato = ifelse(is.na(cilindrata_original), "Imputato", "Originale")
  )

# Grafico che mostra la differenza tra i valori originali e quelli imputati
ggplot(data_imputed, aes(x = peso, y = cilindrata, color = Imputato)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("Originale" = "blue", "Imputato" = "red")) +
  labs(
    title = "Confronto Cilindrata vs Peso Prima e Dopo l'Imputazione con Loess",
    x = "Peso",
    y = "Cilindrata",
    color = "Stato"
  ) +
  theme_minimal()

cbind(data$cilindrata, data_imputed$cilindrata)


# Porte -------------------------------------------------------------------

data$porte = ifelse(data$porte == "NA", NA, data$porte)

t1 = data %>% select(porte, modello, prezzo_auto, chilometraggio)

t2 = t1 %>% group_by(modello, porte) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = T), n = n())

#qui andiamo ad imputare prendendo come riferimento il valore più frequente per ogni singolo modello

t3 = t2 %>% group_by(modello) %>% summarise(porte = porte[which.max(n)])


porte_imputation = function(data){
  
  t1 <<- data %>% select(porte, modello, prezzo_auto, chilometraggio)
  
  t2 <<- t1 %>% group_by(modello, porte) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = T), n = n())
  
  t3 <<- t2 %>% group_by(modello) %>% summarise(porte = porte[which.max(n)])
  
  #t4 <<- t3 %>% group_by(modello) %>% summarise(porte = porte[which.max(n)])
  
  for(modello in unique(data$modello)){
    data$porte = ifelse(data$modello == modello & is.na(data$porte), t3$porte[match(modello, t3$modello)], data$porte)
  }
  
  return(t3)
}

porte_imputation(data)

data_old = data


for(modello in unique(data$modello)){
  data_new$porte = ifelse(data_new$modello == modello & is.na(data_new$porte), porte_imputation(data_new)$porte[match(modello, porte_imputation(data_new)$modello)], data_new$porte)
}

cbind(data_old$porte, data_new$porte)

ggplot(data_old, aes(x = porte)) + 
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  geom_histogram(data = data_new, aes(x = porte), fill = "red", color = "black", bins = 30, alpha = 0.3) +
  theme_minimal()



# Posti -------------------------------------------------------------------

data$posti = ifelse(data$posti == "NA", NA, data$posti)

t1 = data %>% select(posti, modello, prezzo_auto, chilometraggio)

t2 = t1 %>% group_by(modello, posti) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = T), n = n())

t3 = t2 %>% group_by(modello) %>% summarise(posti = posti[which.max(n)])

posti_imputation = function(data){
  
  t1 <<- data %>% select(posti, modello, prezzo_auto, chilometraggio)
  
  t2 <<- t1 %>% group_by(modello, posti) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = T), n = n())
  
  t3 <<- t2 %>% group_by(modello) %>% summarise(posti = posti[which.max(n)])
  
  for(modello in unique(data$modello)){
    data$posti = ifelse(data$modello == modello & is.na(data$posti), t3$posti[match(modello, t3$modello)], data$posti)
  }
  
  return(t3)
}

posti_imputation(data)

data_old = data

for(modello in unique(data$modello)){
  data_new$posti = ifelse(data_new$modello == modello & is.na(data_new$posti), posti_imputation(data_new)$posti[match(modello, posti_imputation(data_new)$modello)], data_new$posti)
}

cbind(data_old$posti, data_new$posti)

ggplot(data_old, aes(x = posti)) + 
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  geom_histogram(data = data_new, aes(x = posti), fill = "red", color = "black", bins = 30, alpha = 0.3) +
  theme_minimal()





# 3 variabili facciamole a mano (ahimè) -----------------------------------

#PER NEOPATENTATI

data$per_neopatentati = ifelse(data$per_neopatentati == "NA", NA, data$per_neopatentati)

t1 = data %>% select(per_neopatentati, modello, prezzo_auto, chilometraggio)

t2 = t1 %>% group_by(modello, per_neopatentati) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = T), n = n())

#Notiamo che ci sono solo 2 modelli che Na, e giustamente andiamo ad imputare con il valore più frequente per quel modello

t3 = t2 %>% group_by(modello) %>% summarise(per_neopatentati = per_neopatentati[which.max(n)])

per_neopatentati_imputation = function(data){
  
  t1 <<- data %>% select(per_neopatentati, modello, prezzo_auto, chilometraggio)
  
  t2 <<- t1 %>% group_by(modello, per_neopatentati) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = T), n = n())
  
  t3 <<- t2 %>% group_by(modello) %>% summarise(per_neopatentati = per_neopatentati[which.max(n)])
  
  for(modello in unique(data$modello)){
    data$per_neopatentati = ifelse(data$modello == modello & is.na(data$per_neopatentati), t3$per_neopatentati[match(modello, t3$modello)], data$per_neopatentati)
  }
  
  return(t3)
}

per_neopatentati_imputation(data)

data_old = data


for(modello in unique(data$modello)){
  data_new$per_neopatentati = ifelse(data_new$modello == modello & is.na(data_new$per_neopatentati), per_neopatentati_imputation(data_new)$per_neopatentati[match(modello, per_neopatentati_imputation(data_new)$modello)], data_new$per_neopatentati)
}

cbind(data_old$per_neopatentati, data_new$per_neopatentati)

#vabbè easy

#kW

data$kW = ifelse(data$kW == "NA", NA, data$kW)

t1 = data %>% select(kW, modello, prezzo_auto, chilometraggio)

t2 = t1 %>% group_by(modello, kW) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = T), n = n())

#essendo Kw con pochi valori mancanti, possiamo imputare attraverso la media per quel modello

t3 = t2 %>% group_by(modello) %>% summarise(kW = mean(kW, na.rm = T))

kW_imputation = function(data){
  
  t1 <<- data %>% select(kW, modello, prezzo_auto, chilometraggio)
  
  t2 <<- t1 %>% group_by(modello, kW) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = T), n = n())
  
  t3 <<- t2 %>% group_by(modello) %>% summarise(kW = mean(kW, na.rm = T))
  
  for(modello in unique(data$modello)){
    data$kW = ifelse(data$modello == modello & is.na(data$kW), t3$kW[match(modello, t3$modello)], data$kW)
  }
  
  return(t3)
}

kW_imputation(data)

data_old = data


for(modello in unique(data$modello)){
  data_new$kW = ifelse(data_new$modello == modello & is.na(data_new$kW), kW_imputation(data_new)$kW[match(modello, kW_imputation(data_new)$modello)], data_new$kW)
}

cbind(data$modello[which(is.na(data_old$kW))],data_old$kW[which(is.na(data_old$kW))], data_new$kW[which(is.na(data_old$kW))])

#cero un ggplot per vedere i dati mancanti come sono stati imputati, selezionando appunto solo quelle righe

ggplot(data = data_new[which(is.na(data_old$kW)),], aes(x = modello, y = kW)) + 
  geom_point(size = 3, color = "#23de45") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  geom_point(data = data_old[which(data$modello %in% data$modello[which(is.na(data_old$kW))]),], color = "red")

#cv

data$cv = ifelse(data$cv == "NA", NA, data$cv)

t1 = data %>% select(cv, modello, prezzo_auto, chilometraggio)

t2 = t1 %>% group_by(modello, cv) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = T), n = n())

#essendo cv con pochi valori mancanti, possiamo imputare attraverso la media per quel modello

t3 = t2 %>% group_by(modello) %>% summarise(cv = mean(cv, na.rm = T))

cv_imputation = function(data){
  
  t1 <<- data %>% select(cv, modello, prezzo_auto, chilometraggio)
  
  t2 <<- t1 %>% group_by(modello, cv) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = T), n = n())
  
  t3 <<- t2 %>% group_by(modello) %>% summarise(cv = mean(cv, na.rm = T))
  
  for(modello in unique(data$modello)){
    data$cv = ifelse(data$modello == modello & is.na(data$cv), t3$cv[match(modello, t3$modello)], data$cv)
  }
  
  return(t3)
}

cv_imputation(data)

data_old = data


for(modello in unique(data$modello)){
  data_new$cv = ifelse(data_new$modello == modello & is.na(data_new$cv), cv_imputation(data_new)$cv[match(modello, cv_imputation(data_new)$modello)], data_new$cv)
}

cbind(data$modello[which(is.na(data_old$cv))],data_old$cv[which(is.na(data_old$cv))], data_new$cv[which(is.na(data_old$cv))])

#creo un ggplot per vedere come sono stati

ggplot(data = data_new[which(is.na(data_old$cv)),], aes(x = modello, y = cv)) + 
  geom_point(size = 3, color = "#23de45") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  geom_point(data = data_old[which(data$modello %in% data$modello[which(is.na(data_old$cv))]),], color = "red")


