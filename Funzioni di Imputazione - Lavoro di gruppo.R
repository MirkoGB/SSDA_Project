#File source

emissioni_imputation = function(data){
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

consumi_imputation = function(data){
  t1 <<- data %>% group_by(modello) %>% summarise(mean_consumi = mean(consumi, na.rm = T))
  
  t2 <<- data %>% select(modello, consumi)
  
  ggplot(t2, aes(x = consumi, fill = modello)) + 
    geom_density(alpha = 0.5)+
    theme_minimal()
  
  return(t1)
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

per_neopatentati_imputation = function(data){
  
  t1 <<- data %>% select(per_neopatentati, modello, prezzo_auto, chilometraggio)
  
  t2 <<- t1 %>% group_by(modello, per_neopatentati) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = T), n = n())
  
  t3 <<- t2 %>% group_by(modello) %>% summarise(per_neopatentati = per_neopatentati[which.max(n)])
  
  for(modello in unique(data$modello)){
    data$per_neopatentati = ifelse(data$modello == modello & is.na(data$per_neopatentati), t3$per_neopatentati[match(modello, t3$modello)], data$per_neopatentati)
  }
  
  return(t3)
}

kW_imputation = function(data){
  
  t1 <<- data %>% select(kW, modello, prezzo_auto, chilometraggio)
  
  t2 <<- t1 %>% group_by(modello, kW) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = T), n = n())
  
  t3 <<- t2 %>% group_by(modello) %>% summarise(kW = mean(kW, na.rm = T))
  
  for(modello in unique(data$modello)){
    data$kW = ifelse(data$modello == modello & is.na(data$kW), t3$kW[match(modello, t3$modello)], data$kW)
  }
  
  return(t3)
}

cv_imputation = function(data){
  
  t1 <<- data %>% select(cv, modello, prezzo_auto, chilometraggio)
  
  t2 <<- t1 %>% group_by(modello, cv) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = T), n = n())
  
  t3 <<- t2 %>% group_by(modello) %>% summarise(cv = mean(cv, na.rm = T))
  
  for(modello in unique(data$modello)){
    data$cv = ifelse(data$modello == modello & is.na(data$cv), t3$cv[match(modello, t3$modello)], data$cv)
  }
  
  return(t3)
}

marce_imputation = function(data){
  
  t1 <<- data %>% select(marce, modello, prezzo_auto, chilometraggio, kW, cilindri)
  
  t2 <<- t1 %>% group_by(modello, marce) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = TRUE), n = n())
  
  t3 <<- t1 %>% group_by(modello, marce) %>% summarise(mean_chilometraggio = mean(chilometraggio, na.rm = TRUE), n = n())
  
  t4 <<- t2 %>% group_by(modello) %>% summarise(marce = marce[which.max(mean_prezzo)], .groups = 'drop')
  
  t4 <<- t4 %>% mutate(marce = ifelse(is.na(marce), t2 %>% filter(modello == first(modello), !is.na(marce)) %>% summarise(marce = marce[which.max(mean_prezzo)]) %>% pull(marce), marce))
  
  return(t4)
}

posti_imputation = function(data){
  
  t1 <<- data %>% select(posti, modello, prezzo_auto, chilometraggio)
  
  t2 <<- t1 %>% group_by(modello, posti) %>% summarise(mean_prezzo = mean(prezzo_auto, na.rm = T), n = n())
  
  t3 <<- t2 %>% group_by(modello) %>% summarise(posti = posti[which.max(n)])
  
  for(modello in unique(data$modello)){
    data$posti = ifelse(data$modello == modello & is.na(data$posti), t3$posti[match(modello, t3$modello)], data$posti)
  }
  
  return(t3)
}