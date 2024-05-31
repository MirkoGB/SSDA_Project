setwd("C:/Users/alice/OneDrive - Università degli Studi di Padova/Laurea Magistrale/Secondo Anno/Strumenti Statistici per l'Analisi di Dati Aziendali/Progetto")

library(tidyverse)
library(readr)
library(caret)
library(gbm)
library(dplyr)

data = read.csv("cars-clean-v2-imputed.csv")

#Gradient Boosting -------------------------------------------------------
#trasformiamo le variabili char in fattori
data = data %>% 
  mutate_if(is.character,as.factor)

data$porte = as.numeric(data$porte)
data$cilindri = as.numeric(data$cilindri)

data$chilometraggio = log(data$chilometraggio + 1)
data$peso = log(data$peso)
data$prezzo_auto = log(data$prezzo_auto)

data_NA = data %>% 
  select_if(~any(is.na(.)))

#Creo un subset del dataset escludendo colonne con qualsiasi NA
data_clean = data %>% 
  select_if(~ !any(is.na(.)))

glimpse(data_clean)

#Split del dataset
#Nota: questa sezione è solo un test per sviluppare la successiva
set.seed(123)
trainIndex = createDataPartition(data_clean$prezzo_auto,p = 0.75,list = FALSE)

#Escludiamo alcune auto da inserire nel test set e di cui disponiamo della valutazione autoscout
to_exlude = c(15,34,55,61,88,103,128,148,155,205,259,317,392,404,423,585,637,642,717,759,1319,1554)
trainIndex = setdiff(trainIndex,to_exlude)
dati.train = data_clean[trainIndex ,] 
dati.test = data_clean[-trainIndex,]

#Variabile risposta: prezzo_auto
#Eliminiamo la variabile modello (crea alcuni problemi)
boost.car = gbm(prezzo_auto ~ . - (modello),data = dati.train,
                distribution = "gaussian",
                n.trees = 5000,
                interaction.depth = 1) #funzione di perdita quadratica

par(mfrow = c(1,1))
#Grafico con gli errori di previsione sull'insieme di stima
plot(boost.car$train.error,type = "l",ylab = "training error")

A = summary(boost.car)

#Grafico per visualizzare l'importanza delle variabili presente dentro l'oggetto A
ggplot(A[A$rel.inf > 1,],aes(x = reorder(var,rel.inf),y = rel.inf,fill = rel.inf)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  theme_minimal() +
  scale_fill_gradient(low = "blue",high = "red") +
  labs(title = "Importanza delle variabili nel modello",
       x = "Variabile",
       y = "Importanza relativa")

yhat.boost = predict(boost.movies,newdata = dati.test,n.trees = 1:5000)
err = apply(yhat.boost,2,function(pred) mean((dati.test$prezzo_auto - pred)^2))
plot(err,type = "l")

#Confronto tra errori (train e test)
plot(boost.movies$train.error,type = "l")
lines(err,type = "l",col = 2)
#Evidenziamo il valore minimo dell'errore di previsione nel test set
best = which.min(err)
abline(v = best,lty = 2,col = 4)

min(err) #minimo errore di previsione
best #numero di iterazioni corrispondente al minimo errore
#alla 237 iterazione ottengono il più piccolo errore di previsione


#Griglia parametri di regolazione ----------------------------------------
#Creo una griglia su cui far variare i parametri del gradient boosting  (n°trees, shrinkage, depth)
?gbm

n_trees = seq(100,1000,by = 100)
shrinkage = seq(0.02,0.1,by = 0.02)
depth = seq(1,5,by = 1)

#Calcolo per ogni combinazione di parametri l'errore di previsione
#Costruzione di un array 3D
err = array(NA,dim = c(length(n_trees),length(shrinkage),length(depth)))

for (i in 1:length(n_trees)) {
  for (j in 1:length(shrinkage)) {
    for (k in 1:length(depth)) {
      boost.movies = gbm(prezzo_auto ~ . - (modello),data = dati.train,
                         distribution = "gaussian",n.trees = n_trees[i],
                         shrinkage = shrinkage[j],interaction.depth = depth[k])
      yhat.boost = predict(boost.movies,newdata = dati.test,n.trees = n_trees[i])
      err[i,j,k] = mean((dati.test$prezzo_auto - yhat.boost)^2)
      print(paste("n.trees =",n_trees[i],"shrinkage =",shrinkage[j],"depth =",depth[k],"err =",err[i,j,k]))
    }
  }
}

#Troviamo l'indice del valore minimo
min_index = which.min(err)

#Ottieniamo le coordinate tridimensionali di questo indice
coordinates = arrayInd(min_index,.dim = dim(err))

#Supponendo che "coordinates" sia l'output di arrayInd() come nel precedente esempio
n_trees_value = n_trees[coordinates[1,1]]
shrinkage_value = shrinkage[coordinates[1,2]]
depth_value = depth[coordinates[1,3]]

print(paste("n_trees:",n_trees_value))
print(paste("shrinkage:",shrinkage_value))
print(paste("depth:",depth_value))

saveRDS(err,"err_array.rds") #5 minimi errori

# Carica l'array di errori
load("err_array.rds")
err = err_array

#Ordiniamo i valori dell'array e ottieniamo gli indici dei 5 valori più bassi
sorted_indices = order(err,decreasing = FALSE)[1:5]

#Ottieni le coordinate tridimensionali di questi indici
coordinates = arrayInd(sorted_indices,.dim = dim(err))

#Ottieniamo i valori corrispondenti
lowest_values = err[sorted_indices]

#Creiamo un dataframe con le coordinate e i valori
lowest_values_df = data.frame(
  n_trees = n_trees[coordinates[,1]],
  shrinkage = shrinkage[coordinates[,2]],
  depth = depth[coordinates[,3]],
  value = lowest_values)
lowest_values_df

#Stimiamo il modello con i valori dei parametri di regolazione selezionati
#Dalle esplicative rimuoviamo modello, regione e kW
library(gbm)
final_model = gbm(prezzo_auto ~ . - modello - regione - kW,data = dati.train,
                  distribution = "gaussian",n.trees = n_trees_value,
                  shrinkage = shrinkage_value,interaction.depth = depth_value)

yhat.boost = predict(final_model,newdata = dati.test,n.trees = n_trees_value)

data_pred_obs = as.data.frame(cbind(yhat.boost,dati.test$prezzo_auto))
colnames(data_pred_obs) = c("Valori_Predetti","Valori_Osservati")
data_pred_obs = data_pred_obs %>%
  mutate(Valori_Predetti = exp(Valori_Predetti),
         Valori_Osservati = exp(Valori_Osservati))

dati.test$yhat.boost = yhat.boost
dati.test = dati.test[,c("modello","marca","prezzo_auto","yhat.boost")]
dati.test = dati.test %>%
  mutate(prezzo_auto = exp(prezzo_auto),
         yhat.boost = exp(yhat.boost))

dati.test$diff_prezzo = dati.test$yhat.boost - dati.test$prezzo_auto
plot(dati.test$diff_prezzo)
abline(h = 0,col = "red")

#Valutazione prevista
dati.test$valutazione = ifelse(dati.test$diff_prezzo >= -500 & dati.test$diff_prezzo <= 500,"Buon Prezzo",
                               ifelse(dati.test$diff_prezzo > 500 & dati.test$diff_prezzo <= 2000,"Ottimo Prezzo",
                                      ifelse(dati.test$diff_prezzo > 2000,"Super Prezzo","Pessimo")))

table(dati.test$valutazione)

#RMSE del modello finale 
rmse = sqrt(mean((dati.test$prezzo_auto - yhat.boost)^2))
rmse

#R^2 del modello finale
cor(data_pred_obs$Valori_Osservati,data_pred_obs$Valori_Predetti)^2

#Mettiamo in un oggetto il valore del RMSE e del R^2
model_gb = data.frame(
  RMSE = rmse,
  R2 = cor(data_pred_obs$Valori_Osservati,data_pred_obs$Valori_Predetti)^2
)

#Cambiamo il nome della riga inserendo il nome del modello
rownames(model_gb) = "Gradient Boosting"

save("model_gb",
     file = "model_gb.RData")

#Residui
data_pred_obs = data_pred_obs %>%
  as.data.frame()
data_pred_obs$Residui = data_pred_obs$Valori_Osservati - data_pred_obs$Valori_Predetti

#Differenza assoluta tra valori predetti e osservati e aggiungila al dataframe
data_pred_obs$Differenza_Assoluta = abs(data_pred_obs$Valori_Osservati - data_pred_obs$Valori_Predetti)

#Calcoliamo la distanza dallo zero per i residui
data_pred_obs$Distanza_Zero = abs(data_pred_obs$Residui)

#Font 
library(sysfonts)
library(showtext)
font_path = "C:/Users/alice/AppData/Local/Microsoft/Windows/Fonts/cmunrm.ttf"
font_add("CMUSerif",font_path)
showtext_auto()

#Palette 
palette_function = colorRampPalette(c("#F5F200","#fbfa99","#333333"))

#Grafico dei residui 
ggplot(data_pred_obs,aes(x = Valori_Predetti,y = Residui)) +
  geom_point(aes(color = Distanza_Zero),size = 3) +
  scale_color_gradient(high = "#F5F200",low = "#333333") +  
  geom_abline(intercept = 0,slope = 0,color = "#970015",linetype = "dashed",size = 1) +
  labs(x = "Valori Predetti",
       y = "Residui") +
  theme_minimal() +
  theme(text = element_text(family = "CMUSerif"),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 30),
        legend.position = "none")

#Grafico per confronto tra valori predetti e osservati 
#evidenziamo la differenza assoluta tra questi
ggplot(data_pred_obs,aes(x = Valori_Predetti,y = Valori_Osservati)) +
  geom_point(aes(color = Differenza_Assoluta),alpha = 0.6,size = 3) +  
  scale_color_gradient(high = "#F5F200",low = "#333333") +  
  geom_abline(intercept = 0,slope = 1,color = "#970015",linetype = "dashed",size = 1) +  
  labs(x = "Valori Predetti",
       y = "Valori Osservati",
       color = "Diff. Assoluta") +
  theme_minimal() +
  theme(legend.position = "right",
        text = element_text(family = "CMUSerif"),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        legend.key.size = unit(1,"cm"))

###########################################################################
cars = read.csv("cars-clean-v2-imputed.csv")

#Creiamo una variabile optional che conta il numero di optional per ogni riga
cars$optional = NA
for(i in 1:nrow(cars)){
  cars$optional[i] = sum(as.numeric(cars[i,32:(ncol(cars)-1)]))
}
cars$optional

#Escludiamo le singole variabili relative agli optional
cars = cars %>% select(-c(32:(ncol(cars) - 1)))

cars = cars %>%
  mutate_if(is.character,as.factor) %>%
  mutate(across(.cols = where( ~ n_distinct(.) < 6),as.factor))

cars$porte = as.numeric(cars$porte)
cars$cilindri = as.numeric(cars$cilindri)

cars$chilometraggio = log(cars$chilometraggio + 1)
cars$peso = log(cars$peso)
cars$prezzo_auto = log(cars$prezzo_auto)

#Split del dataset in training e test set
set.seed(123)
trainIndex = createDataPartition(cars$prezzo_auto,p = 0.75,list = FALSE)
to_exlude = c(34,55,61,88,103,128,148,155,205,259,317,392,404,423,585,637,642,717,759,1319,1554)

trainIndex = trainIndex[-to_exlude]
train = cars[trainIndex,]
test = cars[-trainIndex,]

ABCDE = preProcess(cars)
cars2 = predict(ABCDE,cars)

train2 = cars2[trainIndex,]
test2 = cars2[-trainIndex,]

fitControl = trainControl(method = "CV",number = 10)

#Modello lineare ---------------------------------------------------------
model_lm = train(prezzo_auto ~ . - modello - regione - zona_geografica - kW,
                 data = train,
                 method = "glmStepAIC",
                 trControl = fitControl)
pred_lm = predict(model_lm,test)

ggplot(data = test,aes(x = prezzo_auto,y = pred_lm)) +
  geom_point() +
  geom_abline(intercept = 0,slope = 1,color = "red") +
  labs(title = "Modello lineare",x = "Prezzo reale",y = "Prezzo predetto") +
  theme_minimal()


#LARS --------------------------------------------------------------------
model_lars = train(prezzo_auto ~ . - modello - regione - zona_geografica - kW,
                   data = train,
                   method = "lars",
                   trControl = fitControl)

pred_lars = predict(model_lars,test)

ggplot(data = test,aes(x = prezzo_auto,y = pred_lars)) +
  geom_point() +
  geom_abline(intercept = 0,slope = 1,color = "red") +
  labs(title = "Modello lars",x = "Prezzo reale",y = "Prezzo predetto") +
  theme_minimal()

#Modello ridge -----------------------------------------------------------
model_ridge = train(prezzo_auto ~ . - modello - regione - zona_geografica - kW,
                    data = train,
                    method = "glmnet",
                    trControl = fitControl)
model_ridge
pred_ridge = predict(model_ridge,test)

ggplot(data = test,aes(x = prezzo_auto,y = pred_ridge)) +
  geom_point() +
  geom_abline(intercept = 0,slope = 1,color = "red") +
  labs(title = "Modello ridge",x = "Prezzo reale",y = "Prezzo predetto") +
  theme_minimal()

#Reti neurali ------------------------------------------------------------
model_nn = train(prezzo_auto ~ . - modello - regione - zona_geografica - kW,
                 data = train,
                 method = "nnet",
                 trControl = fitControl,
                 linout = TRUE)
model_nn
pred_nn = predict(model_nn,test)

ggplot(data = test,aes(x = prezzo_auto,y = pred_nn)) +
  geom_point() +
  geom_abline(intercept = 0,slope = 1,color = "red") +
  labs(title = "Modello reti neurali",x = "Prezzo reale",y = "Prezzo predetto") +
  theme_minimal()

#Modello Projection Pursuit ----------------------------------------------
model_ppr = train(prezzo_auto ~ . - modello - regione - zona_geografica - kW,
                  data = train,
                  method = "ppr",
                  trControl = fitControl)
model_ppr
pred_ppr = predict(model_ppr,test)

ggplot(data = test,aes(x = prezzo_auto,y = pred_ppr)) +
  geom_point() +
  geom_abline(intercept = 0,slope = 1,color = "red") +
  labs(title = "Modello Projection Pursuit",x = "Prezzo reale",y = "Prezzo predetto") +
  theme_minimal()


# Modello rvm con kernel lineare -----------------------------------------
model_rvm = train(prezzo_auto ~ . - modello - regione - zona_geografica - kW,
                  data = train2,
                  method = "rvmLinear",
                  trControl = fitControl)
model_rvm
pred_rvm = predict(model_rvm,test2)

ggplot(data = test2,aes(x = prezzo_auto,y = pred_rvm)) +
  geom_point() +
  geom_abline(intercept = 0,slope = 1,color = "red") +
  labs(title = "Modello rvmLinear",x = "Prezzo reale",y = "Prezzo predetto") +
  theme_minimal()

#MARS --------------------------------------------------------------------
model_mars = train(prezzo_auto ~ . - modello - regione - zona_geografica - kW,
                   data = train,
                   method = "earth",
                   trControl = fitControl)
model_mars
pred_mars = predict(model_mars,test)

ggplot(data = test,aes(x = prezzo_auto,y = pred_mars)) +
  geom_point() +
  geom_abline(intercept = 0,slope = 1,color = "red") +
  labs(title = "Modello MARS",x = "Prezzo reale",y = "Prezzo predetto") +
  theme_minimal()

#Random forest -----------------------------------------------------------
model_rf = train(prezzo_auto ~ . - modello - regione - zona_geografica - kW,
                 data = train,
                 method = "ranger",
                 trControl = fitControl)
model_rf
pred_rf = predict(model_rf,test)

ggplot(data = test,aes(x = prezzo_auto,y = pred_rf)) +
  geom_point() +
  geom_abline(intercept = 0,slope = 1,color = "red") +
  labs(title = "Modello Random Forest",x = "Prezzo reale",y = "Prezzo predetto") +
  theme_minimal()

#Salviamo tutti i modelli
save(model_lm,model_lars,model_ridge,model_nn,model_ppr,model_rvm,model_mars,model_rf,
     file = "model_all.RData")

#Errori
#Creiamo una lista con tutti i risultati con i modelli
load("model_all.RData")
models = ls(pattern = "model_")
results = lapply(models,get)
names(results) = models
errors = lapply(results,function(x) {
  x$results
})

#Estraiamo le misture RMSE e R^2 per ogni elemento della lista
errors = lapply(errors,function(x) {
  data.frame(RMSE = x$RMSE,Rsquared = x$Rsquared,model = rownames(x))
})

#Prendiamo il migliore per ogni modello e creiamo un dataframe in cui ogni riga riporta:
# - Nome del modello;
# - RMSE;
# - R^2.
best_models = data.frame(matrix(NA,nrow = length(errors),ncol = 2))
for(i in 1:length(errors)){
  best_models[i,] = errors[[i]][which.min(errors[[i]]$RMSE),][1:2]
}
rownames(best_models) = lapply(results,function(x){
  x$modelInfo$label
})
best_models

#Sostituiamo "glmnet" con "Ridge"
rownames(best_models)[rownames(best_models) == "glmnet"] = "Ridge"

#Rinominiamo le colonne con (RSME, R2)
colnames(best_models) = c("RMSE","R2")

#Uniamo a questo dataframe il Gradient Boosting 
load("model_gb.RData")
model_gb
best_models = rbind(best_models,model_gb)
best_models = best_models[order(best_models$R2,decreasing = T),]

library(xtable)
xtable(best_models,align = "lll")
# Il miglior modello considerando l'indice R^2 è il Gradient Boosting
