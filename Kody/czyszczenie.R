#pobranie danych z repozytorium

dane <- read.csv("dane_surowe/samochody_new.csv")
View(dane)

#instalacja pakietów
install.packages("tidyverse")

library(tidyverse)

dane <- dane %>% 
  colnames() <- c("marka", "model", "cena_zl", "przebieg_w_milach", "skrzynia_biegow", "poj_silnika", "paliwo", "miasto", "wojewodztwo", "rok" ) %>%
  arrange(marka, model, cena, przebieg)

View(dane)
