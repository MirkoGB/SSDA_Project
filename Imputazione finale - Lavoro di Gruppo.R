source("Funzioni di Imputazione - Lavoro di gruppo.R")

data = read_csv("cars-clean-v2 (1).csv")
sort(colSums(is.na(data)), decreasing = T)

# Verifica dei dati mancanti
missing_summary <- sort(colSums(is.na(data)), decreasing = TRUE)
print(missing_summary)


# Selezione delle colonne con valori mancanti
data_2 <- data %>% select(where(~sum(is.na(.)) > 0))
nomi_var = names(data_2)

data_old = data
data_new = data

#### Qui inizia l'imputazione ------

### Emissioni ----
emissioni_imputation(data)
data_new$emissioni = ifelse(is.na(data_new$emissioni), t1$median_emissioni[match(data_new$modello, t1$modello)], data_new$emissioni)

#creo un istogramma per vedere se l'imputazione ha funzionato e come sono cambiati gli istogrammi rispetto a prima
ggplot(data_old, aes(x = emissioni)) + 
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  geom_histogram(data = data_new, aes(x = emissioni), fill = "red", color = "black", bins = 30, alpha = 0.3) +
  facet_wrap(~classe_emissioni) + 
  theme_minimal()

### Consumi ----
consumi_imputation(data)
data_new$consumi = ifelse(is.na(data_new$consumi), t1$mean_consumi[match(data_new$modello, t1$modello)], data_new$consumi)

ggplot(data_old, aes(x = consumi)) + 
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  geom_histogram(data = data_new, aes(x = consumi), fill = "red", color = "black", bins = 30, alpha = 0.3) +
  facet_wrap(~modello) + 
  theme_minimal()

### Trazione ----

for(modello in unique(data$modello)){
  data_new$trazione = ifelse(data_new$modello == modello & is.na(data_new$trazione), trazione_imputation(data_new, modello)$trazione, data_new$trazione)
}
ggplot(data_old, aes(x = prezzo_auto, y = chilometraggio, color = trazione)) + 
  geom_point() + 
  theme_minimal()+
  geom_point(data = data_new[which(is.na(data_old$trazione)),], mapping = aes(x = prezzo_auto, y = chilometraggio, color = trazione), size = 7,alpha = 0.3)

### Cilindri ----

data$cilindri = ifelse(data$cilindri == "NA", NA, data$cilindri)
#per ogni riga di data, se il numero di cilindri è compreso fra 0 ed 1 allora lo sostituisco con il numero di cilindri più frequente per quel modello
data$cilindri = ifelse(data$cilindri %in% c("0", "1"), NA, data$cilindri)
data$cilindri = ifelse(data$cilindri %in% c(0,1), NA, data$cilindri)

for(modello in unique(data$modello)){
  data_new$cilindri = ifelse(data_new$modello == modello & is.na(data_new$cilindri), cilindri_imputation(data_new)$numero_cilindri[match(modello, cilindri_imputation(data_new)$modello)], data_new$cilindri)
}

cbind(data_old$cilindri, data_new$cilindri)

ggplot(data, aes(x = prezzo_auto, y = chilometraggio, color = as.factor(cilindri))) + 
  geom_point() + 
  theme_minimal()+
  geom_point(data = data_new[which(is.na(data_old$cilindri)),], mapping = aes(x = prezzo_auto, y = chilometraggio, color = cilindri), size = 7,alpha = 0.3)


### Marce ----

data$marce = ifelse(data$marce == "NA", NA, data$marce)

marce_imputation(data)

# Imputazione finale nel dataset originale
data_new2 = data %>% 
  left_join(t4, by = "modello") %>% 
  mutate(marce = ifelse(is.na(marce.x), marce.y, marce.x)) %>%
  select(-marce.y)

# Visualizzazione del risultato
head(data)

cbind(data$marce, data_new2$marce)
data_new$marce = data_new2$marce
#* Commento sull'imputazione
#* In pratica abbiamo preso, per ogni modello il numero di marce che costava di più mediamente, e lo abbiamo imputato con quello

# Preparazione dei dati
data_before = data %>% 
  mutate(Stato = "Prima")

data_after = data_new2 %>% 
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

### Cilindrata ----

data$cilindrata = ifelse(data$cilindrata == "NA", NA, data$cilindrata)

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

data_new$cilindrata = data_imputed$cilindrata

### Porte ---

data$porte = ifelse(data$porte == "NA", NA, data$porte)
porte_imputation(data)
for(modello in unique(data$modello)){
  data_new$porte = ifelse(data_new$modello == modello & is.na(data_new$porte), porte_imputation(data_new)$porte[match(modello, porte_imputation(data_new)$modello)], data_new$porte)
}
cbind(data_old$porte, data_new$porte)

ggplot(data_old, aes(x = porte)) + 
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  geom_histogram(data = data_new, aes(x = porte), fill = "red", color = "black", bins = 30, alpha = 0.3) +
  theme_minimal()

### Posti ---

data$posti = ifelse(data$posti == "NA", NA, data$posti)
posti_imputation(data)
for(modello in unique(data$modello)){
  data_new$posti = ifelse(data_new$modello == modello & is.na(data_new$posti), posti_imputation(data_new)$posti[match(modello, posti_imputation(data_new)$modello)], data_new$posti)
}
cbind(data_old$posti, data_new$posti)

ggplot(data_old, aes(x = posti)) + 
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  geom_histogram(data = data_new, aes(x = posti), fill = "red", color = "black", bins = 30, alpha = 0.3) +
  theme_minimal()

### 3 variabili con 4 NA ----

per_neopatentati_imputation(data)
for(modello in unique(data$modello)){
  data_new$per_neopatentati = ifelse(data_new$modello == modello & is.na(data_new$per_neopatentati), per_neopatentati_imputation(data_new)$per_neopatentati[match(modello, per_neopatentati_imputation(data_new)$modello)], data_new$per_neopatentati)
}

cbind(data_old$per_neopatentati, data_new$per_neopatentati)

#--

kW_imputation(data)
for(modello in unique(data$modello)){
  data_new$kW = ifelse(data_new$modello == modello & is.na(data_new$kW), kW_imputation(data_new)$kW[match(modello, kW_imputation(data_new)$modello)], data_new$kW)
}
cbind(data$modello[which(is.na(data_old$kW))],data_old$kW[which(is.na(data_old$kW))], data_new$kW[which(is.na(data_old$kW))])
ggplot(data = data_new[which(is.na(data_old$kW)),], aes(x = modello, y = kW)) + 
  geom_point(size = 3, color = "#23de45") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  geom_point(data = data_old[which(data$modello %in% data$modello[which(is.na(data_old$kW))]),], color = "red")

#--

cv_imputation(data)
for(modello in unique(data$modello)){
  data_new$cv = ifelse(data_new$modello == modello & is.na(data_new$cv), cv_imputation(data_new)$cv[match(modello, cv_imputation(data_new)$modello)], data_new$cv)
}
cbind(data$modello[which(is.na(data_old$cv))],data_old$cv[which(is.na(data_old$cv))], data_new$cv[which(is.na(data_old$cv))])
ggplot(data = data_new[which(is.na(data_old$cv)),], aes(x = modello, y = cv)) + 
  geom_point(size = 3, color = "#23de45") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  geom_point(data = data_old[which(data$modello %in% data$modello[which(is.na(data_old$cv))]),], color = "red")


#### Controllo finale per vedere di non aver perso nulla

# Verifica dei dati mancanti
missing_summary <- sort(colSums(is.na(data_new)), decreasing = TRUE)
print(missing_summary)


# Salvataggio del dataset con i dati imputati
write_csv(data_new, "cars-clean-v2-imputed.csv")

#elenco di come ho imputato le singole variabili:
