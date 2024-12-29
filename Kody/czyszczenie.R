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

# zapisanie prawidłowych wartości w kolumnie paliwo

paliwo_prawidlowe <- c("Benzyna", "Benzyna+LPG", "Diesel", "Hybryda", "Elektryczny", "Benzyna+CNG", "Wodór")

# przefiltrowanie danych, które nie zwierają odpowiednich wartości w kolumnie "paliwo"

paliwo_blednie <- dane %>% 
  filter(!(paliwo %in% paliwo_prawidlowe))

# istnieje 5,503 błędnych obserwacji

View(paliwo_blednie)

# ile z tych obserwacji ma wpisany przebieg (zawierających tekst "km") w kolumnie paliwo

paliwo_blednie_km <- filter(paliwo_blednie, grepl("km", paliwo))

# 4850 pozycji ma przebieg w miejscu paliwa

View(paliwo_blednie_km)

########################## TEST NA PODZBIORZE ##########################

# tworzenie pętli by zamienić paliwo z przebiegiem

dane2 <- head(paliwo_blednie_km)
view(dane2)

# dla każego wiersza w dane2
for (i in 1:nrow(dane2)) {
  # jeśli w kolumnie przebieg_w_milach wartość zawiera coś z paliwo_prawidlowe
  if (dane2$przebieg_w_milach[i] %in% paliwo_prawidlowe) {
    # Zamiana miejscami
    paliwo_km <- dane2$paliwo[i]
    dane2$paliwo[i] <- dane2$przebieg_w_milach[i]
    dane2$przebieg_w_milach[i] <- paliwo_km
  }
}

View(dane2)

###################### OCZYSZCZANIE W CAŁEJ TABELI ######################

# zadziałało na przykładzie :D, teraz z użyciem filtru

dane4 <- dane

for (i in 1:nrow(dane4)) {
  if (grepl("km", dane4$paliwo[i])) {
    if (dane4$przebieg_w_milach[i] %in% paliwo_prawidlowe) {
      paliwo_z_km <- dane4$paliwo[i]
      dane4$paliwo[i] <- dane4$przebieg_w_milach[i]
      dane4$przebieg_w_milach[i] <- paliwo_z_km
    }
  }
}

View(dane4)

#sprawdzenie co zostało 

paliwo_blednie4 <- dane4 %>% 
  filter(!(paliwo %in% paliwo_prawidlowe))

unique(paliwo_blednie4$paliwo)
View(paliwo_blednie4)

# zostało 653 błędnych obserwacji czyli rok i pojemność

###################### OCZYSZCZANIE W CAŁEJ TABELI ######################

# pętla do poprawy cm3

dane5 <- dane4

for (i in 1:nrow(dane5)) {
  if (grepl("cm3", dane5$paliwo[i])) {
    if (dane5$przebieg_w_milach[i] %in% paliwo_prawidlowe) {
      paliwo_z_km <- dane5$paliwo[i]
      dane5$paliwo[i] <- dane5$przebieg_w_milach[i]
      dane5$przebieg_w_milach[i] <- paliwo_z_km
    }
  }
}

View(dane5)

# sprawdzenie co zostało (563 obs)

paliwo_blednie5 <- dane5 %>% 
  filter(!(paliwo %in% paliwo_prawidlowe))

unique(paliwo_blednie5$paliwo)
View(paliwo_blednie5)

# jakie wiersze zawierają pojemność silnika w kolumnie przebieg_w_milach (221 obs)

cm3_w_przebiegu <- dane5[grepl("cm3", dane5$przebieg_w_milach), ]
View(cm3_w_przebiegu)
# gdy zmienna rok jest w poj silnika albo paliwo, to w kolumnie rok została wpisana pojemność silnika

# najpierw poprawiam kolumnę paliwo

########################## TEST NA PODZBIORZE ##########################

cm3_w_przebiegu2 <- cm3_w_przebiegu

for (i in 1:nrow(cm3_w_przebiegu2)) {
  if (cm3_w_przebiegu2$poj_silnika[i] %in% paliwo_prawidlowe) {
    paliwo_z_poj_silnika <- cm3_w_przebiegu2$poj_silnika[i]
    cm3_w_przebiegu2$poj_silnika[i] <- cm3_w_przebiegu2$paliwo[i]
    cm3_w_przebiegu2$paliwo[i] <- paliwo_z_poj_silnika
  }
}

View(cm3_w_przebiegu2)

###################### OCZYSZCZANIE W CAŁEJ TABELI ######################

# zadziałała zamiana więc na głównej tabeli zamieniam

dane6 <- dane5

for (i in 1:nrow(dane6)) {
  if (dane6$poj_silnika[i] %in% paliwo_prawidlowe) {
    paliwo_z_poj_silnika <- dane6$poj_silnika[i]
    dane6$poj_silnika[i] <- dane6$paliwo[i]
    dane6$paliwo[i] <- paliwo_z_poj_silnika
  }
}

# sprawdzam czy zadziałało

unique(dane6$paliwo)
View(paliwo_blednie6)
glimpse(paliwo_blednie6$paliwo)




########################################################################
# ZMIENNA PALIWO ZOSTAŁA OCZYSZCZONA
########################################################################




# sprawdzenie kolumny rok, jakie dane poza rokiem są w kolumnie rok

unique(dane6$rok)

# jest wiele obserwacji z cm3 lub km, sprawdzam czy są one zamienione miejscami

# tworzenie i zapisanie roku jako tekst do późniejszego przefiltrowania
rok_prawidłowe <- c(1990:2025)
rok_prawidłowe <- as.character(rok_prawidłowe)
is.character(rok_prawidłowe)
glimpse(rok_prawidłowe)

# sprawdzenie ile jest obserwacji innych niż rok w kolumnie rok (7566 obs)

rok_blednie6 <- dane6 %>% 
  filter(!(rok %in% rok_prawidłowe))

View(rok_blednie6)

# jakie wiersze zawierają pojemność silnika w kolumnie rok

cm3_w_rok <- rok_blednie6[grepl("cm3", rok_blednie6$rok), ]
View(cm3_w_rok)
# jest 4,399 obserwacji, w których podano rok w poj_silnika a poj_silnia w rok

########################## TEST NA PODZBIORZE ##########################

# pętla do zamiany roku i poj_silnika - próba na cm3_w_rok

cm3_w_rok2 <- cm3_w_rok

for (i in 1:nrow(cm3_w_rok2)) {
  if (grepl("cm3", cm3_w_rok2$rok[i])) {
    if (cm3_w_rok2$poj_silnika[i] %in% rok_prawidłowe) {
      rok_z_cm3 <- cm3_w_rok2$rok[i]
      cm3_w_rok2$rok[i] <- cm3_w_rok2$poj_silnika[i]
      cm3_w_rok2$poj_silnika[i] <- rok_z_cm3
    }
  }
}

View(cm3_w_rok2)

###################### OCZYSZCZANIE W CAŁEJ TABELI ######################

# poprawnie zamieniono, teraz pora na zamiane w najnowszej tabeli "dane6"

dane7 <- dane6

for (i in 1:nrow(dane7)) {
  if (grepl("cm3", dane7$rok[i])) {
    if (dane7$poj_silnika[i] %in% rok_prawidłowe) {
      rok_z_cm3 <- dane7$rok[i]
      dane7$rok[i] <- dane7$poj_silnika[i]
      dane7$poj_silnika[i] <- rok_z_cm3
    }
  }
}

unique(dane7$rok)

# teraz zostały do poprawy wartrości z km, sprawdzam czy wartości kolumn przebiegu i roku są na odwrót

# sprawdzenie ile jest obserwacji innych niż rok w kolumnie rok i nie będące NA (717 obs)

rok_blednie7 <- dane7 %>% 
  filter(!(rok %in% rok_prawidłowe) & !is.na(rok))

View(rok_blednie7)

# wartości z km mają wpisane rok w poj_silnika, przebieg mają też wpisany ten sam albo inny
# zauważyłem, że gdy wpisano paliwo w kol rok, to rok znajduje się w kol przebieg_w_milach (299 obs)

paliwo_w_rok <- rok_blednie7 %>% 
  filter(rok %in% paliwo_prawidlowe)

View(paliwo_w_rok)

########################## TEST NA PODZBIORZE ##########################

# przeniesienie z przebieg (w którym jest rok) do kol rok

# pętla do zamiany roku i poj_silnika - próba na cm3_w_rok

paliwo_w_rok2 <- paliwo_w_rok

for (i in 1:nrow(paliwo_w_rok2)) {
  if (paliwo_w_rok2$rok[i] %in% paliwo_prawidlowe) {
    if (paliwo_w_rok2$przebieg_w_milach[i] %in% rok_prawidłowe) {
      # nie ma potrzeby zapisać paliwa z kol rok, bo są to duplikaty (w kol paliwo już jest wartość)
      paliwo_w_rok2$rok[i] <- paliwo_w_rok2$przebieg_w_milach[i]
      paliwo_w_rok2$przebieg_w_milach[i] <- NA
    }
  }
}

View(paliwo_w_rok2)

###################### OCZYSZCZANIE W CAŁEJ TABELI ######################

# pętla na całej tabeli teraz

dane8 <- dane7

for (i in 1:nrow(dane8)) {
  if (dane8$rok[i] %in% paliwo_prawidlowe) {
    if (dane8$przebieg_w_milach[i] %in% rok_prawidłowe) {
      # nie ma potrzeby zapisać paliwa z kol rok, bo są to duplikaty (w kol paliwo już jest wartość)
      dane8$rok[i] <- dane8$przebieg_w_milach[i]
      dane8$przebieg_w_milach[i] <- NA
    }
  }
}

View(dane8)

# sprawdzam jakie unikatowe wartości zostały w kolumnie rok

unique(dane8$rok)

# został tylko przebieg, który zamieniamy z poj_silnika, a poj_silnika zamieniamy na NA po zmianie

km_w_rok <- dane8 %>% 
  filter(!(rok %in% rok_prawidłowe) & !is.na(rok))

km_w_rok2 <- km_w_rok[grepl("km", km_w_rok$rok), ]

View(km_w_rok2)

km_w_rok3 <- km_w_rok2

for (i in 1:nrow(km_w_rok3)) {
  if (grepl("km", km_w_rok3$rok[i])) {
    if (km_w_rok3$poj_silnika[i] %in% rok_prawidłowe) {
      km_w_rok3$rok[i] <- km_w_rok3$poj_silnika[i]
      km_w_rok3$poj_silnika[i] <- NA
    }
  }
}

View(km_w_rok3)

# poprawiono 418 obs

###################### OCZYSZCZANIE W CAŁEJ TABELI ######################

# poprawa na całej tabeli

dane9 <- dane8

for (i in 1:nrow(dane9)) {
  if (grepl("km", dane9$rok[i])) {
    if (dane9$poj_silnika[i] %in% rok_prawidłowe) {
      dane9$rok[i] <- dane9$poj_silnika[i]
      dane9$poj_silnika[i] <- NA
    }
  }
}

View(dane9)
unique(dane9$rok)





########################################################################
# ZMIENNA ROK ZOSTAŁA OCZYSZCZONA
########################################################################

