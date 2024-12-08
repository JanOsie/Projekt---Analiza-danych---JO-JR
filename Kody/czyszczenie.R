#pobranie danych z repozytorium

dane <- read.csv("dane_surowe/samochody_new.csv")
View(dane)

#instalacja pakietów
install.packages("tidyverse")

library(tidyverse)

dane <- dane %>% 
  arrange(dane, brand, model)

View(dane)


