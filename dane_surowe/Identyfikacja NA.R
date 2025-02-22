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

# zależność występowania NA miedzy zmiennymi dotyczących ceny a rokiem 
ggplot(data = dane, aes(x = rok, y = cena_zl)) + 
  geom_point() +
  geom_miss_point() +
  scale_color_manual(values = c("red","navyblue")) +
  theme_minimal()

# zależność występowania NA miedzy zmiennymi dotyczących roku a pojemności silnika 
ggplot(data = dane, aes(x = rok, y = poj_silnika)) + 
  geom_point() +
  geom_miss_point() +
  scale_color_manual(values = c("orange","green3")) +
  theme_minimal()

# zależność występowania NA miedzy zmiennymi dotyczących roku a przebiegiem 
ggplot(data = dane, aes(x = rok, y = przebieg_w_km)) + 
  geom_point() +
  geom_miss_point() +
  scale_color_manual(values = c("gold","steelblue1")) +
  theme_minimal()

# zależność występowania NA miedzy zmiennymi dotyczących pojemności silnika a ceną 
ggplot(data = dane, aes(x = poj_silnika, y = cena_zl)) + 
  geom_point() +
  geom_miss_point() +
  scale_color_manual(values = c("lightslateblue","sienna1")) +
  theme_minimal()

# zależność występowania NA miedzy zmiennymi dotyczących ceny a przebiegu 
ggplot(data = dane, aes(x = cena_zl, y = przebieg_w_km)) + 
  geom_point() +
  geom_miss_point() +
  scale_color_manual(values = c("magenta4","turquoise4")) +
  theme_minimal()

# zależność występowania NA miedzy zmiennymi dotyczących pojemności silnika a przebiegiem 
ggplot(data = dane, aes(x = poj_silnika, y = przebieg_w_km)) + 
  geom_point() +
  geom_miss_point() +
  scale_color_manual(values = c("hotpink3","forestgreen")) +
  theme_minimal()

# Heatmapa brakujących wartości z podziałem na markę
gg_miss_fct(x = dane, fct = marka)

# Heatmapa brakujących wartości z podziałem na rok
gg_miss_fct(x = dane, fct = rok)

# wykres skumulowana suma brakujących wartości
gg_miss_case_cumsum(dane, breaks = 5000) + theme_bw()

# Procentowy udział brakujących wartości w podziale na typ paliwa
gg_miss_var(dane, facet = paliwo, show_pct = TRUE)

# imputacja metodą KNN - metoda K-najbliższych sąsiadów

dane2 <- dane
dane2_knn <- kNN(dane2)
daneknn <- dane2_knn %>% select(where(~ !is.logical(.)))
n_miss(daneknn)

# wizualizacja imputowanych wartości
dane2_knn %>%
  ggplot() +
  geom_point(mapping = aes(x = rok, 
                           y = cena_zl, 
                           col=cena_zl_imp)) +
  scale_color_manual(values = c("#00800010","red"))
