# scripts/04_analysis_descriptions.R
# Análisis exploratorio del corpus filtrado (descripciones) 2018–2025
# Insumo: data/processed/espaciociencia_ecosistema_2018_2025_descripciones.csv

library(dplyr)
library(readr)
library(stringr)
library(data.table)

in_file <- "data/processed/espaciociencia_ecosistema_2018_2025_descripciones.csv"
clean_file <- "data/processed/espaciociencia_ecosistema_2018_2025_descripciones_clean.csv"

# Crear versión "clean" solo si no existe (evita re-escritura innecesaria)
if (!file.exists(clean_file)) {
  df_tmp <- data.table::fread(in_file, encoding = "UTF-8", data.table = FALSE)
  readr::write_csv(df_tmp, clean_file)
}

df <- readr::read_csv(clean_file, show_col_types = FALSE)

# Check rápido post-limpieza
message("Filas df: ", nrow(df))
message("IDs únicos: ", dplyr::n_distinct(df$id))
df %>% dplyr::count(anio, sort = TRUE) %>% print(n = 10)

# Forzar tipos mínimos seguros
df <- df %>%
  mutate(
    id = as.character(id),
    titulo = as.character(titulo),
    descripcion = as.character(descripcion),
    anio = suppressWarnings(as.integer(anio))
  )

# Preparar texto (título + descripción) y bandera idioma
df <- df %>%
  mutate(
    texto = str_squish(str_to_lower(paste(titulo, descripcion, sep = " "))),
    tiene_ecosistema_es = str_detect(texto, "\\becosistema(s)?\\b"),
    tiene_ecosistema_en = str_detect(texto, "\\becosystem(s)?\\b"),
    tiene_ecosistema = tiene_ecosistema_es | tiene_ecosistema_en
  )

df %>%
  summarise(
    n_total = n(),
    n_ecosistema = sum(tiene_ecosistema, na.rm = TRUE),
    prop_ecosistema = round(100 * n_ecosistema / n_total, 1)
  ) %>%
  print()

# Tipología preliminar por reglas (descripción)
# natural: vocabulario ecológico/ambiental
# sociotecnico: vocabulario de innovación, emprendimiento, política CTCI
# hibrido: cuando coexisten señales fuertes de ambos

pat_natural <- paste0(
  "\\b(",
  paste(c(
    "biodiversidad","ecologia","ecológico","ecologico","especies","hábitat","habitat",
    "bosque","forestal","marino","marina","acuatico","acuáticos","acuaticos",
    "cuenca","rio","río","humedal","glaciar","clima","climatico","climático",
    "conservacion","conservación","restauracion","restauración",
    "suelo","suelos","flora","fauna","parque","reserva","area protegida","área protegida"
  ), collapse="|"),
  ")\\b"
)

pat_sociotec <- paste0(
  "\\b(",
  paste(c(
    "innovacion","innovación","emprendimiento","emprendedor","startup","spin[- ]?off",
    "transferencia","tecnologica","tecnológica","propiedad intelectual","patente",
    "politica","política","gobernanza","ministerio","ctci","i\\+d","investigacion y desarrollo",
    "ecosistema de innovacion","ecosistema de innovación","ecosistema emprendedor",
    "sector productivo","industria","mercado","valor","cadena de valor","clusters?"
  ), collapse="|"),
  ")\\b"
)

df_tip <- df %>%
  mutate(
    flag_natural = str_detect(texto, pat_natural),
    flag_sociotec = str_detect(texto, pat_sociotec),
    categoria = case_when(
      flag_natural & flag_sociotec ~ "hibrido_ambiguo",
      flag_natural ~ "natural",
      flag_sociotec ~ "sociotecnico",
      TRUE ~ "sin_clasificar"
    )
  )

df_tip %>%
  count(categoria, sort = TRUE) %>%
  mutate(porc = round(100 * n / sum(n), 1)) %>%
  print(n = 50)
