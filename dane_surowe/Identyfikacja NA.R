# pobiernie pakietów
install.packages("tidyverse")
install.packages("naniar")

library(naniar)
library(tidyverse)
library(dlookr)
library(editrules)
library(VIM)
library(deducorrect)
library(ISLR) 
# import danych
dane <- read.csv(file="Kody/dane_wyczyszczone.csv")

# zmieniam na tibble

dane <- as_tibble(dane)

# ustawianie na odpowiednie typy

dane$marka <- factor(dane$marka)
dane$model <- factor(dane$model)
dane$cena_zl <- as.integer(dane$cena_zl)
dane$przebieg_w_km <- as.integer(dane$przebieg_w_km)
dane$skrzynia_biegow <- factor(dane$skrzynia_biegow)
dane$poj_silnika <- as.integer(dane$poj_silnika)
dane$paliwo <- factor(dane$paliwo)
dane$miasto <- factor(dane$miasto)
dane$wojewodztwo <- factor(dane$wojewodztwo)
dane$rok <- as.integer(dane$rok)

# łączna liczba brakujących wartość
n_miss(dane)

# NA dla poszczegółnych zmiennych
miss_var_summary(dane)

# liczba wartości NA dla poszczególnych wierszy
dane %>% 
  miss_case_table()

# wykresy zmiennych w których brakuje wartości (malejąco)
vis_miss(dane[, c("rok", "cena_zl", "poj_silnika", "przebieg_w_km")], sort_miss = TRUE)

# współwystępowanie NA między zmiennymi
gg_miss_upset(dane[, c("rok", "cena_zl", "poj_silnika", "przebieg_w_km")], 
              nsets = 4)


ggplot(data = dane, aes(x = rok, y = cena_zl)) + 
  geom_point() +
  geom_miss_point() +
  scale_color_manual(values = c("darkorange","cyan4")) +
  theme_minimal()

# imputacja metodą KNN - metoda K-najbliższych sąsiadów

dane2 <- dane
dane2_knn <- kNN(dane2)
daneknn <- dane2_knn %>% select(where(~ !is.logical(.)))
n_miss(daneknn)

