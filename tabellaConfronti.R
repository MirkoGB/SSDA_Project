cars = read.csv(file.choose())

fp <- cars[15, c(1, 2, 5)]
# Riga 15 (Fiat Panda): super prezzo

ds <- cars[34, c(1, 2, 5)]
# Riga 34 (Dacia Sandero): super prezzo 

vt <- cars[103, c(1, 2, 5)]
# Riga 103 (Volkswagen T-Roc): super prezzo

pm <- cars[155, c(1, 2, 5)]
# Riga 155 (Ford Puma): super prezzo

jr <- cars[205, c(1, 2, 5)]
# Riga 205 (Jeep Renegade): super prezzo

pd <- cars[259, c(1, 2, 5)]
# Riga 259 (Peugeot 208) super prezzo 

ty <- cars[61, c(1, 2, 5)]
# Riga 61 (Toyota Yaris Cross): buon prezzo

rc <- cars[128, c(1, 2, 5)]
# Riga 128 (Renault Captur): buon prezzo

fc <- cars[88, c(1, 2, 5)]
# Riga 88 (Fiat 500): ottimo prezzo

cc <- cars[148, c(1, 2, 5)]
# Riga 148 (Citroen C3): ottimo prezzo

ly <- cars[55, c(1, 2, 5)]
# Riga 55 (Lancia Ypsilon): ottimo prezzo 

oc <- cars[317, c(1, 2, 5)]
# Riga 317 (Opel Corsa): ottimo prezzo

nq <- cars[392, c(1, 2, 5)]
# Riga 392 (Nissan Qashqai): ottimo prezzo

mz <- cars[404, c(1, 2, 5)]
# Riga 404 (MG ZS): ottimo prezzo

ks <- cars[423, c(1, 2, 5)]
# Riga 423 (Kia Sportage): ottimo prezzo

hi <- cars[585, c(1, 2, 5)]
# Riga 585 (Hyundai i10): ottimo prezzo

cf <- cars[637, c(1, 2, 5)]
# Riga 637 (Cupra Formentor): ottimo prezzo

mg <- cars[642, c(1, 2, 5)]
# Riga 642 (Mercedes-Benz GLA): ottimo prezzo

aa <- cars[717, c(1, 2, 5)]
# Riga 717 (Audi A3): ottimo prezzo 

si <- cars[759, c(1, 2, 5)]
# Riga 759 (Suzuki Ignis): ottimo prezzo

bx <- cars[1319, c(1, 2, 5)]
# Riga 1319 (BMW X1): ottimo prezzo 

mc <- cars[1543, c(1, 2, 5)]
# Riga 1543 (Mini Countryman): ottimo prezzo

dati <- rbind(rc, ty, aa, bx, cc, cf, fc, hi, ks,
              ly, mg, mz, mc, nq, oc, si, ds, fp,
              pm, jr, pd, vt)
dati$prezzo.previsto = NA
dati$giudizio.as24 = c(rep("Buon prezzo", 2), 
                       rep("Ottimo prezzo", 14),
                       rep("Super prezzo", 6))
dati$giudizio.previsto = NA
print(xtable::xtable(dati), include.rownames = FALSE)
