# tutaj opracujemy wartości NA
install.packages("tidyverse")
install.packages("naniar")

# ładowanie pakietów (środa)
library(naniar)
library(tidyverse)
library(readr)

load("analiza/statystyka_opisowa.Rda")

str(dane3)
glimpse(dane3)

# Analiza opisowa

# dla marka:
table(dane3$marka)
summary(dane3$cena_zl, na.rm = TRUE)

# dla model:
summary(dane3$model, na.rm = TRUE)

# dla cena_zl:
summary(dane3$cena_zl, na.rm = TRUE)

# dla przebieg_w_km:
summary(dane3$przebieg_w_km, na.rm = TRUE)

# dla skrzynia_biegow:
table(dane3$skrzynia_biegow)

# dla poj_silnika:
summary(dane3$poj_silnika)

# dla paliwo:
table(dane3$paliwo)

# dla miasto:
table(dane3$miasto)

# dla wojewodztwo:
table(dane3$wojewodztwo)

# dla rok:
table(dane3$rok)

plot(przebieg_w_km ~ rok, data = dane3)

boxplot(poj_silnika ~ paliwo, data = dane3)

