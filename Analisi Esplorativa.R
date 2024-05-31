setwd("C:/Users/alice/OneDrive - Università degli Studi di Padova/Laurea Magistrale/Secondo Anno/Strumenti Statistici per l'Analisi di Dati Aziendali/Progetto")

library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(sysfonts)
library(showtext)
library(Polychrome)
library(scales)
library(sf)
library(rnaturalearth)
library(latex2exp)

cars = read.csv("cars-clean-v2-imputed.csv")
head(cars)

str(cars)

#Font
font_path = "C:/Users/alice/AppData/Local/Microsoft/Windows/Fonts/cmunrm.ttf"
#font_path = "C:/Users/alice/Downloads/source-sans-pro.semibold.ttf"
font_add("CMUSerif",font_path)
showtext_auto()

#Palette 
palette_function = colorRampPalette(c("#F5F200","#fbfa99","#333333"))

#Grafici esplorativi -----------------------------------------------------
#Prezzo auto per tipologia di carrozzeria
cars %>%
  ggplot(aes(x = as.factor(carrozzeria),y = prezzo_auto,fill = as.factor(carrozzeria))) +
  geom_bar(stat = "summary",fun = "mean",position = "dodge",width = 0.6) +
  labs(x = "Carrozzeria", y = "Prezzo medio auto") +
  scale_fill_manual(values = palette_function(3)) +
  theme_minimal() + 
  theme_minimal() +
  theme(text = element_text(family = "CMUSerif",colour = "black"),
        axis.title = element_text(size = 30,colour = "black"),
        axis.text = element_text(size = 20,colour = "black"),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"))

#Prezzo auto in base alla tipologia di carburante
cars %>%
  ggplot(aes(x = as.factor(carburante),y = prezzo_auto,fill = as.factor(carburante))) +
  geom_bar(stat = "summary",fun = "mean",position = "dodge",width = 0.8) +
  labs(x = "Carburante", y = "Prezzo medio auto") +
  scale_fill_manual(values = palette_function(5)) +
  theme_minimal() +
  theme(text = element_text(family = "CMUSerif",,colour = "black"),
        axis.title = element_text(size = 30,colour = "black"),
        axis.text = element_text(size = 20,colour = "black"),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"))

#Prezzo auto all'aumentare del chilometraggio
cars %>%
  ggplot(aes(y = prezzo_auto,x = chilometraggio)) + 
  geom_point(aes(color = tipo_di_veicolo)) +
  geom_smooth(method = "loess",col = "#F5F200") +
  ylab("Prezzo auto") + 
  xlab("Chilometraggio") + 
  scale_x_continuous(labels = scales::number_format(big.mark = "")) +
  scale_color_manual(values = c("Usato" = "#333333","Nuovo" = "#970015")) +
  labs(color = "Tipo di veicolo") +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  theme_minimal() +
  theme(text = element_text(family = "CMUSerif",,colour = "black"),
        axis.title = element_text(size = 30,,colour = "black"),
        axis.text = element_text(size = 20,colour = "black"),
        legend.text = element_text(size = 30,colour = "black"),
        legend.title = element_text(size = 20,colour = "black"),
        legend.key.size = unit(1,"cm"),
        panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"))

#Optional
optional = matrix(NA,nrow = ncol(cars) - 32,ncol = 2)
for(i in 1:nrow(optional)){
  optional[i,2] = sum(cars[,32 + i])
  optional[i,1] = ifelse(grepl("\\.",colnames(cars)[32 + i]),
                         gsub("\\."," ",colnames(cars)[32 + i]),
                         colnames(cars)[32 + i])
}
optional = as.data.frame(optional)
colnames(optional) = c("optional","count")
optional$count = as.numeric(optional$count)

#Top 10 optional
optional[1:10,] %>%
  ggplot(aes(y = count,x = reorder(optional,count),fill = as.factor(count))) +
  geom_col() +
  scale_fill_manual(values = rev(palette_function(10))) +
  coord_flip() + 
  xlab("Optional") + 
  ylab(" ") +
  ylim(0,1250) +
  theme_minimal() +
  theme(axis.title = element_text(size = 30,colour = "black"),
        axis.text = element_text(size = 20,colour = "black"),
        text = element_text(family = "CMUSerif",colour = "black"),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"))

#Prezzo medio per zona geografica
#Prendo solo le auto di tipo SUV (tipologia di carrozzeria più frequente)
cars_zona = cars[which(cars$carrozzeria == "SUV"),] %>%
  group_by(zona_geografica) %>%
  summarise(prezzo_medio = mean(prezzo_auto,na.rm = TRUE))

cars_region =  data.frame(regione = unique(cars$regione))
cars_region$prezzo_medio = NA

cars_region$prezzo_medio = ifelse(cars_region$regione %in% c("Valle d'Aosta","Liguria","Piemonte","Lombardia"),cars_zona$prezzo_medio[which(cars_zona$zona_geografica == "Nord Ovest")],
                                  ifelse(cars_region$regione %in% c("Trentino-AA","Friuli-VG","Veneto"),cars_zona$prezzo_medio[which(cars_zona$zona_geografica == "Nord Est")],
                                         ifelse(cars_region$regione %in% c("Emilia-Romagna","Toscana","Marche","Umbria","Lazio","Abruzzo"),cars_zona$prezzo_medio[which(cars_zona$zona_geografica == "Centro")],
                                                cars_zona$prezzo_medio[which(cars_zona$zona_geografica == "Sud")])))
cars_region$regione = as.character(cars_region$regione)
cars_region = rbind(cars_region,c("Molise",cars_zona$prezzo_medio[which(cars_zona$zona_geografica == "Sud")]),
                    c("Sardegna",cars_zona$prezzo_medio[which(cars_zona$zona_geografica == "Sud")]))
cars_region$prezzo_medio = as.numeric(cars_region$prezzo_medio)

cars_region$regione[which(cars_region$regione == "Friuli-VG")] = "Friuli-Venezia Giulia"
cars_region$regione[which(cars_region$regione == "Trentino-AA")] = "Trentino-Alto Adige"
colnames(cars_region)[1] = "region"
head(cars_region)

italy = ne_states(country = "italy",returnclass = "sf")
italy$region[which(italy$region == "Sicily")] = "Sicilia"
italy$region[which(italy$region == "Apulia")] = "Puglia"

italy %>%
  group_by(region) %>%
  summarise() %>%
  merge(cars_region,by = "region",all.x = T) %>%
  ggplot() +
  geom_sf(aes(fill = prezzo_medio)) +
  labs(fill = "Prezzo medio") +
  scale_fill_gradientn(colours = c("#fbfa99","#fff476","#F5F200")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        text = element_text(family = "CMUSerif",colour = "black"),
        legend.text = element_text(size = 20,colour = "black"),
        legend.title = element_text(size = 30,colour = "black"),
        legend.key.size = unit(1,"cm"))

#Intervallo minimo-massimo del prezzo per modello e marca
cars$modello = as.character(cars$modello)
cars$marca = as.character(cars$marca)

modelli_df = matrix(NA,nrow = 50,ncol = 4)
for(i in 1:length(unique(cars$modello))){
  modelli_df[i,1] = round(mean(cars$prezzo_auto[which(cars$modello == unique(cars$modello)[i])]),2)
  modelli_df[i,2] = min(cars$prezzo_auto[which(cars$modello == unique(cars$modello)[i])])
  modelli_df[i,3] = max(cars$prezzo_auto[which(cars$modello == unique(cars$modello)[i])])
  modelli_df[i,4] = stringr::str_trim(cars$marca[which(cars$modello == unique(cars$modello)[i])][1])
}
modelli_df = as.data.frame(modelli_df)
colnames(modelli_df) = c("Media","Minimo","Massimo","Marca")
for(i in 1:3){
  modelli_df[,i] = as.numeric(modelli_df[,i])
}
head(modelli_df)

minmax_df = data.frame(modello = unique(cars$modello),
                       mean = modelli_df[,1],
                       min_ci = modelli_df[,2],
                       max_ci = modelli_df[,3],
                       class = modelli_df[,4])

minmax_df %>%
  ggplot(aes(y = reorder(modello,mean),x = mean,
             xmin = min_ci,xmax = max_ci)) +
  geom_linerange(col = "#F5F200",size = 1.5) +
  geom_pointrange(size = 0.6,col = "#333333") +
  xlab("Prezzo medio auto") +
  ylab("Modello") +
  theme_minimal() +
  theme(text = element_text(family = "CMUSerif",colour = "black"),
        axis.text = element_text(size = 15,colour = "black"),
        axis.title = element_text(size = 30,colour = "black"),
        panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) + 
  guides(y.sec = ggh4x::guide_axis_manual(breaks = minmax_df$modello,labels = minmax_df$class,title = "Marca"))

#Creiamo un dataset analogo ma solamente per le marche
marche_df = matrix(NA,nrow = length(unique(cars$marca)),ncol = 4)
for(i in 1:length(unique(cars$marca))){
  marche_df[i,1] = round(mean(cars$prezzo_auto[which(cars$marca == unique(cars$marca)[i])]),2)
  marche_df[i,2] = min(cars$prezzo_auto[which(cars$marca == unique(cars$marca)[i])])
  marche_df[i,3] = max(cars$prezzo_auto[which(cars$marca == unique(cars$marca)[i])])
  marche_df[i,4] = unique(cars$marca)[i]
}
marche_df = as.data.frame(marche_df)
colnames(marche_df) = c("Media","Minimo","Massimo","Marca")

for(i in 1:3){
  marche_df[,i] = as.numeric(marche_df[,i])
}

marche_df %>%
  ggplot(aes(y = reorder(Marca,Media),x = Media,
             xmin = Minimo,xmax = Massimo)) +
  geom_linerange(col = "#F5F200",size = 1.5) +
  geom_pointrange(size = 0.6,col = "#333333") +
  xlab("Prezzo medio auto") +
  ylab("Marca") +
  theme_minimal() +
  theme(text = element_text(family = "CMUSerif",colour = "black"),
        axis.text = element_text(size = 20,colour = "black"),
        axis.title = element_text(size = 30,colour = "black"),
        panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"))

#Correlazione ------------------------------------------------------------
#Trasformo le variabili categoriche in fattori
cars = cars %>%
  mutate_if(is.character,as.factor) %>%
  mutate(across(.cols = where( ~ n_distinct(.) < 6),as.factor))

#Seleziono solo le variabili quantitative
cars_numeric = cars %>%
  select(where(is.numeric))

#Calcolo correlazione
corr = cor(cars_numeric)

cor_melted = melt(corr)

#Heatmap
cor_melted %>%
  ggplot(aes(Var1,Var2,fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "#F5F200",mid = "#fbfa99",low = "#333333",midpoint = 0) +
  theme_minimal() +
  labs(fill = "Correlazione") +
  xlab("") + 
  ylab("") +
  coord_fixed()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,colour = "black"),
        text = element_text(family = "CMUSerif",colour = "black"),
        axis.title = element_text(size = 30,colour = "black"),
        axis.text = element_text(size = 20,colour = "black"),
        legend.text = element_text(size = 20,colour = "black"),
        legend.title = element_text(size = 30,colour = "black"),
        legend.key.size = unit(1,"cm")) 

# Indice di associazione (eta^2) ------------------------------------------
#Funzione per calcolare eta^2
eta2 = function(x,y) {
  m = mean(x,na.rm = TRUE)
  sct = sum((x - m)^2,na.rm = TRUE)
  n = table(y)
  mk = tapply(x,y,mean,na.rm = TRUE)
  sce = sum(n * (mk - m)^2)
  return(ifelse(sct > 0,sce / sct,0))
}

#Seleziono solo le variabili qualitative
var_qualitative = names(select(cars,where(is.factor)))

#Calcolo eta^2
eta2_results = sapply(var_qualitative,function(var) {
  eta2(cars$prezzo_auto,cars[[var]])
})
eta2_df = data.frame(variabile = names(eta2_results),eta2 = eta2_results)

eta2_df %>%
  arrange(desc(eta2)) %>% 
  head(10) %>%
  ggplot(aes(x = reorder(variabile,eta2),y = eta2,fill = eta2)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(high = "#F5F200",mid = "#333333") +
  labs(x = "",y = TeX(sprintf("$\\eta^2$")),fill = TeX(sprintf("$\\eta^2$"))) +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = "CMUSerif",colour = "black"),
        axis.title = element_text(size = 30,colour = "black"),
        axis.text = element_text(size = 20,colour = "black"),
        legend.text = element_text(size = 20,colour = "black"),
        legend.title = element_text(size = 30,hjust = 0.2,colour = "black"),
        legend.key.size = unit(1,"cm"),
        panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"))
