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




# sprawdzam co jest w kol skrzynia biegów poza prawidłowymi wartościami

unique(dane9$skrzynia_biegow)

braki_w_skrzyni_biegow <- dane9 %>%
  filter(!c(skrzynia_biegow %in% c("manual", "automatic")))

View(braki_w_skrzyni_biegow)

# wszytskie wartości zostały dobrze wpisane w kolumnie skrzynia_biegow i brak wartości NA





########################################################################
# ZMIENNA SKRZYNIA_BIEGÓW JEST CZYSTA
########################################################################




########################## TEST NA PODZBIORZE ##########################

# sprawdzenie co jest w przebiegu poza przebiegiem

przebieg_błędnie <- dane9[!grepl("km", dane9$przebieg_w_milach), ]
View(przebieg_błędnie)

# przepisanie wartości poj_silnika z kol przebieg do poj_silnika gdy w tej kolumnie nie ma już czego z "cm3" lub "km" (4 obs z rokiem w poj_sil zamieniam na NA)
# jeśli nie ma km w przebiegu oraz km i cm3 w silniku
przebieg_błędnie2 <- przebieg_błędnie

for (i in 1:nrow(przebieg_błędnie2)) {
  if (!grepl("km", przebieg_błędnie2$przebieg_w_milach[i])) {
    if (!grepl("cm3", przebieg_błędnie2$poj_silnika[i])) {
      if (!grepl("km", przebieg_błędnie2$poj_silnika[i])) {
        przebieg_błędnie2$poj_silnika[i] <- przebieg_błędnie2$przebieg_w_milach[i]
        przebieg_błędnie2$przebieg_w_milach[i] <- NA
      }
    }
  }
}

View(przebieg_błędnie2)

# dodatkowo usuwam poj_silnika gdy to już jest wpisane w kol poj_silnika

przebieg_błędnie3 <- przebieg_błędnie2

for (i in 1:nrow(przebieg_błędnie3)) {
  if (grepl("cm3", przebieg_błędnie3$przebieg_w_milach[i])) {
    if (grepl("cm3", przebieg_błędnie3$poj_silnika[i])) {
      przebieg_błędnie3$przebieg_w_milach[i] <- NA
    }
  }
}

View(przebieg_błędnie3)

###################### OCZYSZCZANIE W CAŁEJ TABELI ######################

# przepisanie cm3 do poj_sil gdy tam nie ma
dane10 <- dane9

for (i in 1:nrow(dane10)) {
  if (!grepl("km", dane10$przebieg_w_milach[i])) {
    if (!grepl("cm3", dane10$poj_silnika[i])) {
      if (!grepl("km", dane10$poj_silnika[i])) {
        dane10$poj_silnika[i] <- dane10$przebieg_w_milach[i]
        dane10$przebieg_w_milach[i] <- NA
      }
    }
  }
}

# dodatkowo usuwam poj_silnika gdy to już jest wpisane w kol poj_silnika
dane11 <- dane10

for (i in 1:nrow(dane10)) {
  if (grepl("cm3", dane10$przebieg_w_milach[i])) {
    if (grepl("cm3", dane10$poj_silnika[i])) {
      dane10$przebieg_w_milach[i] <- NA
    }
  }
}

# sprawdzam czy się powiodło, co poza "km" zostało
unique(dane11$przebieg_w_milach)

cm3_w_przebiegu3 <- dane11[!grepl("km", dane11$przebieg_w_milach), ]
view(cm3_w_przebiegu3)

# zostały pojedyńcze lata i paliwa, które usuwam (bo mają już swoje wartości w swoich kol)

dane12 <- dane11

for (i in 1:nrow(dane12)) {
  if (!grepl("km", dane12$przebieg_w_milach[i])) {
    dane12$przebieg_w_milach[i] <- NA
  }
}


# sprawdzam co zostało poza km i NA

cm3_w_przebiegu4 <- dane12 %>% 
  filter(!is.na(dane12$przebieg_w_milach))

view(cm3_w_przebiegu4[!grepl("km", cm3_w_przebiegu4$przebieg_w_milach), ])
# pusta tabela

View(dane12)




########################################################################
# ZMIENNA PRZEBIEG_W_MILACH JEST CZYSTA
########################################################################





# sprawdzam co znajduje się w kol marka

dane13 <- dane12

unique(dane13$marka)

# ile jest NA w kol marka (1100 obs)

marka_NA <- filter(dane13, is.na(marka))

View(marka_NA)

# sprawdzam ile jest obs gdzie marki i modelu nie ma (0 obs)

View(filter(dane13, is.na(marka) & is.na(model)))

# dla sprawniejszej zmiany zmieniam wszytskie nazwy marek i modeli aby były z małej litery

dane13$marka <- tolower(dane13$marka)
dane13$model <- tolower(dane13$model)

View(dane13)

########################## TEST NA PODZBIORZE ##########################

# dodawanie pierwszego słowa z kol model do kol marki jeśli w kol marka jest NA

marka_NA2 <- marka_NA

for (i in 1:nrow(marka_NA2)) {
  if (is.na(marka_NA2$marka[i])) {
    marka_NA2$marka[i] <- word(marka_NA2$model[i], 1) 
  }
}

View(marka_NA2)

unique(marka_NA2$marka)

###################### OCZYSZCZANIE W CAŁEJ TABELI ######################

dane14 <- dane13

for (i in 1:nrow(dane14)) {
  if (is.na(dane14$marka[i])) {
    dane14$marka[i] <- word(dane14$model[i], 1) 
  }
}

unique(dane14$marka)

# alfa romeo, citroen i land rover są zapisane na dwa sposoby, ujednolicam je

dane15 <- dane14

for (i in 1:nrow(dane15)) {
  if (grepl("alfa", dane15$marka[i])) {
    dane15$marka[i] <- "alfa-romeo"
  }
  if (grepl("citroën", dane15$marka[i])) {
    dane15$marka[i] <- "citroen"
  }
  if (grepl("land", dane15$marka[i])) {
    dane15$marka[i] <- "land-rover"
  }
}

# porównuje przed i po (zniknęło tylko NA)

unique(dane12$marka)
unique(dane15$marka)



########################################################################
# ZMIENNA MARKA JEST CZYSTA
########################################################################

glimpse(dane15)

unique(dane15$marka)
View(dane15[!grepl("km", dane15$przebieg_w_milach) & !is.na(dane15$przebieg_w_milach), ])
unique(dane15$skrzynia_biegow)
unique(dane15$paliwo)
unique(dane15$rok)
View(dane15[!grepl("cm3", dane15$poj_silnika) & !is.na(dane15$poj_silnika), ])

# trzeba jeszcze doczyścić poj_silnika z przebiegu i roku (809 obs)

brudy_w_poj_silnika <- dane15[!grepl("cm3", dane15$poj_silnika) & !is.na(dane15$poj_silnika), ]
View(brudy_w_poj_silnika)

########################## TEST NA PODZBIORZE ##########################

brudy_w_poj_silnika2 <- brudy_w_poj_silnika

for (i in 1:nrow(brudy_w_poj_silnika2)) {
  # przenieść km do przebiegu, poźniej usunąć
  if (grepl("km", brudy_w_poj_silnika2$poj_silnika[i]) & is.na(brudy_w_poj_silnika2$przebieg_w_milach[i])) {
    brudy_w_poj_silnika2$przebieg_w_milach[i] <- brudy_w_poj_silnika2$poj_silnika[i]
    brudy_w_poj_silnika2$poj_silnika[i] <- NA
  } 
  # przenieść rok do kol rok, później usunąć
  if (brudy_w_poj_silnika2$poj_silnika[i] %in% rok_prawidłowe & is.na(brudy_w_poj_silnika2$rok[i])) {
    brudy_w_poj_silnika2$rok[i] <- brudy_w_poj_silnika2$poj_silnika[i]
    brudy_w_poj_silnika2$poj_silnika[i] <- NA
  }
  # jeśli rok już jest to usunąć rok gdy jest w kol poj_silnika
  if (brudy_w_poj_silnika2$poj_silnika[i] %in% rok_prawidłowe & !is.na(brudy_w_poj_silnika2$rok[i])) {
    brudy_w_poj_silnika2$poj_silnika[i] <- NA
  }
  if (brudy_w_poj_silnika2$poj_silnika[i] %in% paliwo_prawidlowe) {
    if (is.na(brudy_w_poj_silnika2$paliwo[i])) {
      brudy_w_poj_silnika2$paliwo[i] <- brudy_w_poj_silnika2$poj_silnika[i]
      brudy_w_poj_silnika2$poj_silnika[i] <- NA
    }
    if (brudy_w_poj_silnika2$paliwo[i] %in% paliwo_prawidlowe) {
      brudy_w_poj_silnika2$poj_silnika[i] <- NA
    }
  }
}

View(brudy_w_poj_silnika2)

###################### OCZYSZCZANIE W CAŁEJ TABELI ######################

dane16 <- dane15

for (i in 1:nrow(dane16)) {
  # przenieść km do przebiegu, poźniej usunąć
  if (grepl("km", dane16$poj_silnika[i]) & is.na(dane16$przebieg_w_milach[i])) {
    dane16$przebieg_w_milach[i] <- dane16$poj_silnika[i]
    dane16$poj_silnika[i] <- NA
  } 
  # przenieść rok do kol rok, później usunąć
  if (dane16$poj_silnika[i] %in% rok_prawidłowe & is.na(dane16$rok[i])) {
    dane16$rok[i] <- dane16$poj_silnika[i]
    dane16$poj_silnika[i] <- NA
  }
  # jeśli rok już jest to usunąć rok gdy jest w kol poj_silnika
  if (dane16$poj_silnika[i] %in% rok_prawidłowe & !is.na(dane16$rok[i])) {
    dane16$poj_silnika[i] <- NA
  }
  if (dane16$poj_silnika[i] %in% paliwo_prawidlowe) {
    if (is.na(dane16$paliwo[i])) {
      dane16$paliwo[i] <- dane16$poj_silnika[i]
      dane16$poj_silnika[i] <- NA
    }
    if (dane16$paliwo[i] %in% paliwo_prawidlowe) {
      dane16$poj_silnika[i] <- NA
    }
  }
}

View(dane16)

# sprawdam jeszcze raz czy coś poza cm3 jest w poj_silnika (pusta tabela)

View(dane16[!grepl("cm3", dane16$poj_silnika) & !is.na(dane16$poj_silnika), ])

unique(dane16$marka)
View(dane16[!grepl("km", dane16$przebieg_w_milach) & !is.na(dane16$przebieg_w_milach), ])
unique(dane16$skrzynia_biegow)
unique(dane16$paliwo)
unique(dane16$rok)
View(dane16[!grepl("cm3", dane16$poj_silnika) & !is.na(dane16$poj_silnika), ])

########################################################################
# CAŁA TABELA ZOSTAŁA OCZYSZCZONA
########################################################################

write.csv(dane16,"C:/Users/MSI/Desktop/AG II/Analiza danych/Projekt/Projekt---Analiza-danych/Kody/dane_wyczyszczone_rownamesTRUE.csv", row.names = TRUE)
