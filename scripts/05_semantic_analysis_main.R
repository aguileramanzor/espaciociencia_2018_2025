# scripts/05_semantic_analysis_main.R
# Análisis semántico exploratorio del corpus MAIN (2018–2025)
# Entrada: data/processed/espaciociencia_ecosistema_2018_2025_analitico_main.csv

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(tidytext)
library(stopwords)

in_file <- "data/processed/espaciociencia_ecosistema_2018_2025_analitico_main.csv"
df <- readr::read_csv(in_file, show_col_types = FALSE)
df <- df %>%
  mutate(
    texto = str_squish(str_to_lower(replace_na(descripcion, "")))
  )

# 1) Tokenización simple (palabras) desde descripcion (no titulo)
tokens <- df %>%
  transmute(
    id,
    anio,
    texto = str_squish(str_to_lower(descripcion))
  ) %>%
  filter(!is.na(texto), texto != "") %>%
  mutate(texto = str_replace_all(texto, "[^\\p{L}\\p{N}\\s]", " ")) %>%  # limpiar signos
  separate_rows(texto, sep = "\\s+") %>%
  rename(palabra = texto) %>%
  filter(palabra != "", nchar(palabra) >= 3)

# 2) Frecuencias globales (Top 30)
freq_global <- tokens %>%
  count(palabra, sort = TRUE) %>%
  slice_head(n = 30)

print(freq_global, n = 30)

# 3) Frecuencias por año (Top 15 por año, para ver cambios básicos)
freq_year <- tokens %>%
  count(anio, palabra, sort = TRUE) %>%
  group_by(anio) %>%
  slice_max(n, n = 15, with_ties = FALSE) %>%
  ungroup()

print(freq_year, n = 120)

# Stopwords bilingües
stop_es <- stopwords::stopwords("es")
stop_en <- stopwords::stopwords("en")
stop_all <- unique(c(stop_es, stop_en))

# Tokenización limpia
tokens_clean <- df %>%
  select(id, anio, texto) %>%
  tidytext::unnest_tokens(palabra, texto) %>%
  filter(
    !palabra %in% stop_all,
    str_detect(palabra, "[a-záéíóúñ]"),
    nchar(palabra) > 3
  )

# Frecuencias globales (limpias)
freq_global_clean <- tokens_clean %>%
  count(palabra, sort = TRUE)

print(freq_global_clean %>% slice_head(n = 30))

# Frecuencias por año (top 10 por año)
freq_anio_clean <- tokens_clean %>%
  count(anio, palabra, sort = TRUE) %>%
  group_by(anio) %>%
  slice_max(n, n = 10) %>%
  ungroup()

print(freq_anio_clean, n = 40)

