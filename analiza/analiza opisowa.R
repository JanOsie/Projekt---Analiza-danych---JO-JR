library(utils)
dane <- read.csv("C:/Users/HP/Desktop/analiza danych/dane_wyczyszczone_rownamesTRUE.csv")

# Wczytanie danych
dane <- read.csv("C:/Users/HP/Desktop/analiza danych/dane_wyczyszczone_rownamesTRUE.csv", stringsAsFactors = FALSE)
class(dane)
dane <- as.data.frame(dane)
# Usuwanie spacji z początku i końca nazw kolumn
colnames(dane) <- trimws(colnames(dane)) 
# Identyfkuję problematyczne wiersze
problem_rows <- dane$poj_silnika[is.na(as.numeric(gsub(" cm3", "", gsub(" ", "", dane$poj_silnika))))]
problem_rows <- dane$przebieg_w_milach[is.na(as.numeric(gsub(" cm3", "", gsub(" ", "", dane$przebieg_w_milach))))]
problem_rows

# Konwersuję kolumny do odpowiednich typów-  numeryczne 
dane$poj_silnika <- as.numeric(gsub("[^0-9]", "", dane$poj_silnika))
dane$przebieg_w_milach <- as.numeric(gsub("[^0-9]", "", dane$przebieg_w_milach))
dane$rok <- as.numeric(dane$rok)

# Tabele liczności
library(dplyr)
tabela_marka <- dane %>% count(marka, sort = TRUE)
tabela_model <- dane %>% count(model, sort = TRUE)
tabela_wojewodztwo <- dane %>% count(wojewodztwo, sort = TRUE)
tabela_rok <- dane %>% count(rok, sort = TRUE)
tabela_przebieg <- dane %>% count(przebieg_w_milach, sort = TRUE)
tabela_paliwo <- dane %>% count(paliwo, sort = TRUE)
tabela_miasto <- dane %>% count(paliwo, sort = TRUE)
print(tabela_marka)
print(tabela_model)
print(tabela_wojewodztwo)
print(tabela_rok)
print(tabela_przebieg)
print(tabela_paliwo)
print(tabela_miasto)


library(ggplot2)

# Histogram dla cen
ggplot(dane, aes(x = cena_zl)) + 
  geom_histogram(binwidth = 5000, fill = "blue", color = "black") +
  labs(title = "Histogram cen samochodów", x = "Cena (zł)", y = "Liczba samochodów") +
  theme_minimal()

# Wykres słupkowy dla marek
ggplot(tabela_marka, aes(x = reorder(marka, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Liczba samochodów wg marki", x = "Marka", y = "Liczba samochodów") +
  theme_minimal()

# Wykres dla województw
ggplot(tabela_wojewodztwo, aes(x = reorder(wojewodztwo, n), y = n)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Liczba samochodów wg województwa", x = "Województwo", y = "Liczba samochodów") +
  theme_minimal()

# Analiza heterogeniczności rozkładu zmiennej cena_zl według marki
ggplot(dane, aes(x = marka, y = cena_zl)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  coord_flip() +
  labs(title = "Rozkład cen wg marki", x = "Marka", y = "Cena (zł)") +
  theme_minimal()

# Tabela statystyk opisowych
summary_table <- dane %>% 
  summarise(
    MinCena = min(cena_zl),
    SredniaCena = mean(cena_zl),
    MedianaCena = median(cena_zl),
    MaxCena = max(cena_zl),
    OdchylenieCena = sd(cena_zl), 
    WarCene = var(cena_zl), 
    coeff_var<-sd(cena_zl)/mean(cena_zl),
    coeff_var,
    IQrCena = IQR(cena_zl),
    sxCena<-IQR(cena_zl)/2,  
    coeff_varx_cena<-sx/median(cena_zl),
    coeff_varx_cena,
    quantile(cena_zl,probs=c(0,0.1,0.25,0.5,0.75,0.95,1),na.rm=TRUE),
    MinPrzebieg = min(przebieg_w_milach),
    SredniPrzebieg = mean(przebieg_w_milach, na.rm = TRUE),
    MaxPrzebieg = max(przebieg_w_milach, na.rm = TRUE),
    MinRok = min(rok, na.rm = TRUE),
    MaxRok = max(rok, na.rm = TRUE)
  )
print(summary_table)

# Macierz korelacji
# Wybór kolumn numerycznych
num_dane <- dane %>% 
  select(cena_zl, przebieg_w_milach, poj_silnika, rok)
cor_matrix <- cor(num_dane, use = "complete.obs")
print(cor_matrix)

# Wizualizacja macierzy korelacji
library(corrplot)
corrplot(cor_matrix, method = "circle", type = "lower", tl.col = "black", tl.srt = 45)


# Wczytanie pakietów
install.packages("ggstatsplot")
install.packages("reporttools")
library(ggstatsplot)
library(reporttools)
library(dplyr)
library(ggplot2)


# --- 1. Tabele kontyngencji i testy zależności ---
# Tabela kontyngencji: Marka vs Województwo
tabela_marka_wojewodztwo <- table(dane$marka, dane$wojewodztwo)
tabela_cena_marka <- table(dane$cena_zl, dane$marka)
tabela_cena_model <- table(dane$cena_zl, dane$model)
tabela_cena_prebieg <- table(dane$cena_zl, dane$przebieg_w_milach)
tabela_cena_silnik <- table(dane$cena_zl, dane$poj_silnika)
# Test chi-kwadrat
chi_sq_result <- chisq.test(tabela_marka_wojewodztwo)
print(chi_sq_result)

# Wizualizacja tabeli kontyngencji (ggstatsplot)

# Wybór najpopularniejszych marek (np. top 5)
top_brands <- dane %>%
  count(marka, sort = TRUE) %>%
  top_n(5, n) %>%
  pull(marka)

# Dodanie kategorii "Inne" dla mniej popularnych marek
dane_grouped <- dane %>%
  mutate(marka = ifelse(marka %in% top_brands, marka, "Inne"))

# Wykres kołowy z ggpiestats
ggpiestats(
  data = dane_grouped,
  x = marka,
  title = "Proporcje najpopularniejszych marek samochodów",
  caption = "Kategorie poza top 5 połączono w 'Inne'",
  ggtheme = ggplot2::theme_minimal(base_size = 12), # Czytelny motyw minimalistyczny
  legend.title = "Marki samochodów",
  perc.k = 1,   # Zaokrąglenie procentów do jednego miejsca po przecinku
  palette = "viridis", # Więcej kolorów
  label.position = "outside" # Wyniesienie etykiet poza wykres
)



# Tabele liczności dla wybranych kolumn
library(dplyr)
table_marka <- dane %>% count(marka, sort = TRUE)
table_model <- dane %>% count(model, sort = TRUE)
table_wojewodztwo <- dane %>% count(wojewodztwo, sort = TRUE)
table_rok <- dane %>% count(rok, sort = TRUE)

# Wyświetlanie tabel liczności
print(table_marka)
print(table_model)
print(table_wojewodztwo)
print(table_rok)

# Podstawowe wykresy
library(ggplot2)


# 1. Test t-Studenta: Czy średnie ceny samochodów różnią się między dwiema markami?
marka1 <- "BMW"
marka2 <- "Audi"
ceny_marka1 <- dane$cena_zl[dane$marka == marka1]
ceny_marka2 <- dane$cena_zl[dane$marka == marka2]

t_test <- t.test(ceny_marka1, ceny_marka2, var.equal = FALSE)  # Test z niezależnymi wariancjami
print(t_test)

# 2. Korelacja i regresja liniowa między ceną a przebiegiem

correlation <- cor.test(dane$cena_zl, dane$przebieg_w_milach, use = "complete.obs")
print(correlation)

# Regresja liniowa: Cena jako funkcja przebiegu
model <- lm(cena_zl ~ przebieg_w_milach, data = dane)
summary(model)

# Wykres regresji
library(ggplot2)
ggplot(data, aes(x = przebieg_w_milach, y = cena_zl)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Regresja: Cena a przebieg", x = "Przebieg (w milach)", y = "Cena (zł)") +
  theme_minimal()



# Wczytanie pakietów
library(ggstatsplot)
library(reporttools)
library(dplyr)
library(ggplot2)

# --- 1. Tabele kontyngencji i testy zależności ---
# Tabela kontyngencji: Marka vs Województwo
table_marka_wojewodztwo <- table(dane$marka, dane$wojewodztwo)

# Test chi-kwadrat
chi_sq_result <- chisq.test(table_marka_wojewodztwo)
print(chi_sq_result)

# Wizualizacja tabeli kontyngencji (ggstatsplot)
# Wykres z filtrowaniem top 5 marek i poprawioną osią X
ggstatsplot::ggbarstats(
  data = filtered_data,
  x = marka,
  y = wojewodztwo,
  title = "Zależność między marką (top 5) a województwem",
  xlab = "Marka (top 5)",
  ylab = "Województwo",
  legend.title = "Województwa",
  caption = "Źródło: Dane wyczyszczone",
  ggtheme = ggplot2::theme_minimal(base_size = 12), # Czytelniejszy motyw
  bar.proptest = TRUE,   # Proporcje na wykresie
  bar.color = "black",   # Ramka słupków
  bar.palette = "viridis"
) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1) # Obrót etykiet osi X o 45 stopni
  )


# --- 2. dane# --- 2. Testowanie proporcji ---
# Proporcje samochodów w różnych województwach
prop_table <- prop.table(table(dane$wojewodztwo))
print(prop_table)

# Testowanie proporcji: Czy proporcje samochodów w województwach są równe?
test_proporcji <- chisq.test(table(dane$wojewodztwo))
print(test_proporcji)

# Wizualizacja proporcji
ggplot(as.data.frame(prop_table), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Proporcje samochodów w województwach",
    x = "Województwo",
    y = "Proporcja"
  ) +
  theme_minimal(base_size = 14) + # Powiększenie rozmiaru czcionki na wykresie
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10), # Obrót i powiększenie czcionki osi X
    plot.title = element_text(hjust = 0.5, size = 16) # Wyśrodkowanie i powiększenie tytułu
  )

# --- 3. Testowanie wielu proporcji ---
# Test proporcji dla marek w różnych województwach
# Ensure there are no missing values


# Perform G-Test
contingency_table <- table(dane$marka, dane$wojewodztwo)
test_wielu_proporcji <- DescTools::GTest(contingency_table)
print(test_wielu_proporcji)


# Wizualizacja proporcji (ggstatsplot)
# Top 10 most popular brands
top_10_brands <- brand_counts %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  pull(marka)

# Least 10 popular brands
least_10_brands <- brand_counts %>%
  arrange(n) %>%
  slice_head(n = 10) %>%
  pull(marka)


top_10_data <- dane %>%
  filter(marka %in% top_10_brands)

least_10_data <- dane %>%
  filter(marka %in% least_10_brands)

# Check unique values
if (length(unique(top_10_data$marka)) < 2) {
  stop("Insufficient data for the top 10 brands visualization.")
}
if (length(unique(least_10_data$marka)) < 2) {
  stop("Insufficient data for the least 10 brands visualization.")
}

dane$marka <- as.factor(dane$marka)
#Wykresy
top_10_plot <- ggstatsplot::ggpiestats(
  data = top_10_data,
  x = marka,
  title = "Proporcje 10 najpopularniejszych marek samochodów",
  ggtheme = ggplot2::theme_minimal(),
  legend.title = "Marki samochodów",
  perc.k = 1,
  palette = "viridis",
  label.position = "outside"
)

least_10_plot <- ggstatsplot::ggpiestats(
  data = least_10_data,
  x = marka,
  title = "Proporcje 10 najmniej popularnych marek samochodów",
  ggtheme = ggplot2::theme_minimal(),
  legend.title = "Marki samochodów",
  perc.k = 1,
  palette = "plasma",
  label.position = "outside"
)

library(patchwork)
top_10_plot + least_10_plot

# --- 4. Analiza różnic: ANOVA ---
# Porównanie średnich cen wg marki
anova_result <- aov(cena_zl ~ marka, data = dane)
summary(anova_result)

# Post-hoc Tukey HSD
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Wizualizacja ANOVA (ggstatsplot)
ggbetweenstats(
  data = dane,
  x = marka,
  y = cena_zl,
  title = "Porównanie cen samochodów wg marki",
  outlier.tagging = FALSE, 
  max.overlaps = 100,
  coord.flip = TRUE,
  package = "viridis",       
  palette = "plasma"
)


# --- 5. Badanie zależności: Korelacja ---
# Korelacja między ceną a przebiegiem
correlation <- cor.test(dane$cena_zl, dane$przebieg_w_milach, use = "complete.obs")
print(correlation)

# Wizualizacja korelacji
ggstatsplot::ggscatterstats(
  data = data,
  x = przebieg_w_milach,
  y = cena_zl,
  title = "Korelacja między ceną a przebiegiem"
)

# --- 6. Podsumowanie w raporcie tabelarycznym ---
# Przygotowanie tabeli wyników dla raportu
summary_stats <- reporttools::tableContinuous(
  vars = data[c("cena_zl", "przebieg_w_milach", "poj_silnika")],
  group = data$marka,
  stats = c("mean", "median", "sd", "min", "max")
)

print(summary_stats)
