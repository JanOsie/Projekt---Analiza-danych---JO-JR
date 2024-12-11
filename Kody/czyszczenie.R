# instalacja i otwieranie pakietów

install.packages("tidyverse")
install.packages("naniar")

library(naniar)
library(tidyverse)

# pobranie danych z repozytorium lokalnego po pull

dane <- read.csv("dane_surowe/samochody_new.csv")

# przekształcenie ramki na tibble

dane <- as_tibble(dane)

#sprawdzenie struktury danych
str(dane)

# sprawdzenie czy przekształciło na tibble

is_tibble(dane)

# zmiana nazw kolumn na polskie

colnames(dane) <- c("marka", "model", "cena_zl", "przebieg_w_milach", "skrzynia_biegow", "poj_silnika", "paliwo", "miasto", "wojewodztwo", "rok" )

# ile NA w całej tabeli(5100), proporcja (0,00557 albo 0,5%)

n_miss(dane)
prop_miss(dane)

# ile danych dla jakiej kolumny i grupy

dane %>%
  group_by(marka) %>%
  miss_var_summary()

# liczba braków dla konkretnej ilości braków w wierszu

dane %>%
  miss_case_table()

# wizualizacja braków
# najwięcej braków mamy dla ferrari (5%) i maserati (5%)

gg_miss_fct(dane, fct = marka)

# zbadanie współwystępowania braków
# braki danych występują prawie wyłącznie pojedyczo
gg_miss_upset(dane, nsets = 10)

# szukanie zależności między zmiennymi

dane_ferrari <- dane %>%
  filter(marka == "ferrari")

#braki danych są rozrzucone dla ferrari i nie wykazują jakiejś zależności
ggplot(data = dane_ferrari, aes(x = rok, y = cena_zl)) +
  geom_point() +
  geom_miss_point() +
  scale_color_manual(values = c("blue","red")) +
  theme_minimal()

# sprawdzenie brakujących danych dla wszytskich modeli (za dużo, trzba ograniczyć model)

ggplot(data = dane, aes(x = rok, y = cena_zl)) +
  geom_point(aes(color = marka, shape = marka), size = 2) +
  theme_minimal()
