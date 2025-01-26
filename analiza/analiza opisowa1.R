# Wczytanie danych
dane <- read.csv("C:/Users/HP/Desktop/analiza danych/dane_wyczyszczone_rownamesTRUE.csv", stringsAsFactors = FALSE)

# Usunięcie zbędnych spacji z nazw kolumn
colnames(dane) <- trimws(colnames(dane))

# Konwersja odpowiednich kolumn na wartości numeryczne
dane$poj_silnika <- as.numeric(gsub("[^0-9]", "", dane$poj_silnika))
dane$przebieg_w_milach <- as.numeric(gsub("[^0-9]", "", dane$przebieg_w_milach))
dane$rok <- as.numeric(dane$rok)

# Ładowanie bibliotek
library(dplyr)
library(ggplot2)
library(ggstatsplot)
library(corrplot)

# 1. Tabele liczności dla każdej zmiennej
tabele <- list(
  marka = dane %>% count(marka, sort = TRUE),
  model = dane %>% count(model, sort = TRUE),
  wojewodztwo = dane %>% count(wojewodztwo, sort = TRUE),
  rok = dane %>% count(rok, sort = TRUE),
  paliwo = dane %>% count(paliwo, sort = TRUE),
  skrzynia_biegow = dane %>% count(skrzynia_biegow, sort = TRUE)
)

# Wyświetlenie tabel liczności
lapply(tabele, print)

# 2. Wizualizacje dla każdej tabeli
for (nazwa in names(tabele)) {
  tabela <- tabele[[nazwa]]
  ggplot(tabela, aes(x = reorder(!!sym(nazwa), n), y = n)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Liczba samochodów wg", nazwa), x = nazwa, y = "Liczba") +
    theme_minimal() +
    ggsave(paste0("wykres_", nazwa, ".png"))
}

# 3. Statystyki opisowe
statystyki_opisowe <- dane %>%
  summarise(
    MinCena = min(cena_zl, na.rm = TRUE),
    SredniaCena = mean(cena_zl, na.rm = TRUE),
    MedianaCena = median(cena_zl, na.rm = TRUE),
    MaxCena = max(cena_zl, na.rm = TRUE),
    OdchylenieCena = sd(cena_zl, na.rm = TRUE),
    IQR_Cena = IQR(cena_zl, na.rm = TRUE),
    MinPrzebieg = min(przebieg_w_milach, na.rm = TRUE),
    SredniPrzebieg = mean(przebieg_w_milach, na.rm = TRUE),
    MaxPrzebieg = max(przebieg_w_milach, na.rm = TRUE),
    MinRok = min(rok, na.rm = TRUE),
    MaxRok = max(rok, na.rm = TRUE)
  )
print(statystyki_opisowe)

# 4. Wyświetlenie raportu i wykresu dla statystyk opisowych
ggplot(statystyki_opisowe, aes(x = names(statystyki_opisowe), y = unlist(statystyki_opisowe))) +
  geom_col(fill = "blue") +
  labs(title = "Podstawowe statystyki opisowe", x = "Statystyka", y = "Wartość") +
  theme_minimal()

# 5. Macierze korelacji dla wszystkich możliwych wariantów
num_cols <- dane %>% select(where(is.numeric)) # Wybierz kolumny numeryczne
cor_matrix <- cor(num_cols, use = "complete.obs")
print(cor_matrix)

# Wizualizacja macierzy korelacji
corrplot(cor_matrix, method = "circle", type = "lower", tl.col = "black", tl.srt = 45)

# 6. Testy statystyczne
# Tabela kontyngencji i test chi-kwadrat
tabela_marka_wojewodztwo <- table(dane$marka, dane$wojewodztwo)
chi_sq_result <- chisq.test(tabela_marka_wojewodztwo)
print(chi_sq_result)

# Testowanie proporcji
prop_table <- prop.table(table(dane$wojewodztwo))
test_proporcji <- chisq.test(table(dane$wojewodztwo))
print(test_proporcji)

# Wizualizacja proporcji
ggplot(as.data.frame(prop_table), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Proporcje samochodów w województwach", x = "Województwo", y = "Proporcja") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7. Regresja liniowa i korelacja
# Regresja liniowa: Cena jako funkcja przebiegu, pojemności silnika i roku produkcji
model <- lm(cena_zl ~ przebieg_w_milach + poj_silnika + rok, data = dane)
summary(model)

# Wizualizacja regresji dla przebiegu
ggplot(dane, aes(x = przebieg_w_milach, y = cena_zl)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Regresja: Cena a przebieg", x = "Przebieg (w milach)", y = "Cena (zł)") +
  theme_minimal()

# Korelacja
correlation <- cor.test(dane$cena_zl, dane$przebieg_w_milach, use = "complete.obs")
print(correlation)

