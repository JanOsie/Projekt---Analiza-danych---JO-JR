# pobiernie pakietów
install.packages("tidyverse")
install.packages("naniar")

library(naniar)
library(tidyverse)

# import danych
dane_NA <- read.csv("dane_surowe/samochody_new.csv")

# łączna liczba brakujących wartość
n_miss(dane_NA)

# NA dla poszczegółnych zmiennych
miss_var_summary(dane_NA)

# liczba wartości NA dla poszczególnych wierszy
dane_NA %>% 
  miss_case_table()

# wykresy zmiennych w których brakuje wartości (malejąco)
vis_miss(dane_NA[, c("year", "price_in_pln", "brand")], sort_miss = TRUE)

# współwystępowanie NA między zmiennymi
gg_miss_upset(dane_NA[, c("year", "price_in_pln", "brand")], 
              nsets = 5)


