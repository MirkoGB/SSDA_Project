library(arules)
library(dplyr)

cars <- read.csv(file.choose())
optional <- cars[, 33:76]
optional[, colnames(optional)] <- lapply(optional[, colnames(optional)], factor)

optional <- as(optional, "transactions")

f.itemset <- apriori(optional, 
                     parameter = list(supp = 0.5, conf = 0.9, 
                                      target = "frequent itemset"))

summary(f.itemset)

inspect(f.itemset[1:10])

rules <- apriori(optional,
                 parameter = list(support = 0.5, confidence = 0.9,
                                  target = "rules"))

summary(rules)

important_rules <- subset(rules, subset = lift > 1)
inspect(important_rules)
