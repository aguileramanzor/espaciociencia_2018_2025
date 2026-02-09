# scripts/02_build_dataset.R
# Construcción del dataset analítico a partir del corpus de descripciones (2018–2025)
# Entrada:  data/processed/espaciociencia_ecosistema_2018_2025_descripciones_clean.csv
# Salida:   data/processed/espaciociencia_ecosistema_2018_2025_analitico.csv

library(dplyr)
library(readr)
library(stringr)

in_file  <- "data/processed/espaciociencia_ecosistema_2018_2025_descripciones_clean.csv"
out_file <- "data/processed/espaciociencia_ecosistema_2018_2025_analitico.csv"

# 1) Cargar corpus limpio
df <- readr::read_csv(in_file, show_col_types = FALSE)

# 2) Tipos mínimos seguros y texto normalizado
df <- df %>%
  mutate(
    id = as.character(id),
    titulo = as.character(titulo),
    descripcion = as.character(descripcion),
    anio = suppressWarnings(as.integer(anio)),
    texto = str_squish(str_to_lower(paste(titulo, descripcion, sep = " ")))
  )

# 3) Variables analíticas básicas (sin interpretación sustantiva)
df <- df %>%
  mutate(
    n_char_desc = if_else(is.na(descripcion), NA_integer_, nchar(descripcion)),
    n_words_desc = if_else(
      is.na(descripcion),
      NA_integer_,
      stringr::str_count(str_squish(descripcion), "\\S+")
    ),
    
    # Banderas QC
    desc_vacia = is.na(descripcion) | str_squish(descripcion) == "",
    desc_corta = !desc_vacia & n_words_desc <= 15,
    # Umbral fijo (conservador) para separar "muy largas"
    desc_muy_larga = !desc_vacia & n_char_desc >= 6000,
    
    # Presencia término (control lógico del corpus)
    tiene_ecosistema_es = str_detect(texto, "\\becosistema(s)?\\b"),
    tiene_ecosistema_en = str_detect(texto, "\\becosystem(s)?\\b"),
    tiene_ecosistema = tiene_ecosistema_es | tiene_ecosistema_en
  )


# 4) Chequeos rápidos (solo consola)
message("Filas: ", nrow(df))
message("IDs únicos: ", dplyr::n_distinct(df$id))
df %>% count(anio, sort = TRUE) %>% print(n = 50)
df %>%
  summarise(
    n_total = n(),
    n_ecosistema = sum(tiene_ecosistema, na.rm = TRUE),
    prop_ecosistema = round(100 * n_ecosistema / n_total, 1)
  ) %>%
  print()

# 5) Guardar dataset analítico
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(df, out_file)

# Subconjuntos para análisis
df_main <- df %>% filter(!desc_vacia, !desc_muy_larga)
df_long <- df %>% filter(desc_muy_larga)

write_csv(df_main, "data/processed/espaciociencia_ecosistema_2018_2025_analitico_main.csv")
write_csv(df_long, "data/processed/espaciociencia_ecosistema_2018_2025_analitico_muy_largas.csv")

message("OK: main guardado en: data/processed/espaciociencia_ecosistema_2018_2025_analitico_main.csv")
message("OK: muy largas guardado en: data/processed/espaciociencia_ecosistema_2018_2025_analitico_muy_largas.csv")
message("Filas main: ", nrow(df_main), " | Filas muy largas: ", nrow(df_long))
message("OK: guardado en: ", out_file)
