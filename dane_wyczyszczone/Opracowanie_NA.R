# tutaj opracujemy wartości NA
install.packages("tidyverse")
install.packages("naniar")

library(naniar)
library(tidyverse)

# pobieram plik
library(readr)
dane_wyczyszczone_rownamesTRUE <- read_csv("Kody/dane_wyczyszczone_rownamesTRUE.csv")
View(dane_wyczyszczone_rownamesTRUE)

# usuwam pierwszą kolumnę

dane <- dane_wyczyszczone_rownamesTRUE[ ,-1]

for (i in 1:nrow(dane)) {
  # przenieść km do przebiegu, poźniej usunąć
  if (grepl("km", dane$rok[i]) & is.na(dane$przebieg_w_milach[i])) {
    dane$przebieg_w_milach[i] <- dane$rok[i]
    dane$rok[i] <- NA
  }
  if (grepl("km", dane$rok[i])) {
    dane$rok[i] <- NA
  }
}

# zmieniam na tibble

dane <- as_tibble(dane)
is_tibble(dane)


# usuwanie km i innych znaków nieliczbowych oraz przekonwertowanie na liczby

dane2 <- dane

dane2$przebieg_w_milach <- dane2$przebieg_w_milach %>%
  gsub("[^0-9]", "",.) %>%
  as.numeric()

View(dane2)
is.numeric(dane2$przebieg_w_milach)

# to samo teraz dla poj_silnika

dane3 <- dane2

dane3$poj_silnika <- dane3$poj_silnika %>%
  gsub(" cm3", "",.) %>%
  gsub("[^0-9]", "",.) %>%
  as.numeric()

View(dane3)
is.numeric(dane3$poj_silnika)

colnames(dane3) <- c("marka", "model", "cena_zl", "przebieg_w_km", "skrzynia_biegow", "poj_silnika", "paliwo", "miasto", "wojewodztwo", "rok" )

# ustawianie na odpowiednie typy

dane3$marka <- factor(dane3$marka)
dane3$model <- factor(dane3$model)
dane3$cena_zl <- as.integer(dane3$cena_zl)
dane3$przebieg_w_km <- as.integer(dane3$przebieg_w_km)
dane3$skrzynia_biegow <- factor(dane3$skrzynia_biegow)
dane3$poj_silnika <- as.integer(dane3$poj_silnika)
dane3$paliwo <- factor(dane3$paliwo)
dane3$miasto <- factor(dane3$miasto)
dane3$wojewodztwo <- factor(dane3$wojewodztwo)
dane3$rok <- as.integer(dane3$rok)


dane3 <- write.csv(dane3,"C:/Users/MSI/Desktop/AG II/Analiza danych/Projekt/Projekt---Analiza-danych/Kody/dane_wyczyszczone.csv", row.names = FALSE)

