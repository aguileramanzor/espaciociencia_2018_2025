# scripts/03_qc_checks.R
# Control de calidad (QC) del dataset analítico 2018–2025
# Entrada: data/processed/espaciociencia_ecosistema_2018_2025_analitico.csv
# Salidas (logs): outputs/logs/qc_checks.txt

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

in_file <- "data/processed/espaciociencia_ecosistema_2018_2025_analitico.csv"
log_file <- "outputs/logs/qc_checks.txt"

dir.create("outputs/logs", recursive = TRUE, showWarnings = FALSE)

df <- readr::read_csv(in_file, show_col_types = FALSE)

sink(log_file, split = TRUE)

cat("QC | Dataset analítico Espacio Ciencia (2018–2025)\n")
cat("===============================================\n\n")

# 1) Estructura básica
cat("1) Dimensiones\n")
cat("Filas:", nrow(df), "\n")
cat("IDs únicos:", dplyr::n_distinct(df$id), "\n\n")

# 2) Campos clave: NA
cat("2) NA en campos clave\n")
cat("NA titulo:", sum(is.na(df$titulo)), "\n")
cat("NA descripcion:", sum(is.na(df$descripcion)), "\n")
cat("NA anio:", sum(is.na(df$anio)), "\n\n")

# 3) Rango de años
cat("3) Rango de años\n")
cat("Min anio:", min(df$anio, na.rm = TRUE), "\n")
cat("Max anio:", max(df$anio, na.rm = TRUE), "\n\n")

# 4) Duplicados por id
cat("4) Duplicados por id\n")
dups <- df %>% count(id) %>% filter(n > 1)
cat("IDs duplicados:", nrow(dups), "\n\n")

# 5) Descripciones vacías o muy cortas (posible baja calidad semántica)
cat("5) Descripciones vacías / muy cortas\n")
df_qc_desc <- df %>%
  mutate(
    desc_trim = str_squish(replace_na(descripcion, "")),
    desc_nchar = nchar(desc_trim),
    desc_nwords = str_count(desc_trim, "\\S+")
  )

cat("Descripcion vacía (0 chars):", sum(df_qc_desc$desc_nchar == 0), "\n")
cat("Descripcion <= 50 chars:", sum(df_qc_desc$desc_nchar > 0 & df_qc_desc$desc_nchar <= 50), "\n")
cat("Descripcion <= 15 palabras:", sum(df_qc_desc$desc_nwords > 0 & df_qc_desc$desc_nwords <= 15), "\n\n")

# Top 10 más cortas (no vacías)
cat("Top 10 descripciones más cortas (no vacías)\n")
df_qc_desc %>%
  filter(desc_nchar > 0) %>%
  arrange(desc_nchar) %>%
  select(id, anio, titulo, desc_nchar, desc_nwords) %>%
  slice_head(n = 10) %>%
  print(n = 10)
cat("\n")

# 6) Outliers: descripciones muy largas
cat("6) Outliers: descripciones muy largas\n")
q95 <- quantile(df_qc_desc$desc_nchar, 0.95, na.rm = TRUE)
q99 <- quantile(df_qc_desc$desc_nchar, 0.99, na.rm = TRUE)
cat("P95 nchar:", as.integer(q95), "\n")
cat("P99 nchar:", as.integer(q99), "\n\n")

cat("Top 10 descripciones más largas\n")
df_qc_desc %>%
  arrange(desc(desc_nchar)) %>%
  select(id, anio, titulo, desc_nchar, desc_nwords) %>%
  slice_head(n = 10) %>%
  print(n = 10)
cat("\n")

# 7) Distribución por año (control)
cat("7) Distribución por año\n")
df %>% count(anio, sort = TRUE) %>% print(n = 50)
cat("\n")

sink()

message("OK: QC guardado en: ", log_file)
