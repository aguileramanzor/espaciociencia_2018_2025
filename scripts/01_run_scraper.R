# scripts/01_run_scraper.R
# Ejecuta el scraping del corpus 2018–2025 (descripciones, no PDFs)

library(dplyr)
library(readr)

source("R/scrape_helpers.R")

# 1) Recolectar IDs por búsqueda
ids_es <- search_all_ids("ecosistema", limit = 50, pause_sec = 0.2)
ids_en <- search_all_ids("ecosystem",  limit = 50, pause_sec = 0.2)

ids <- unique(c(ids_es, ids_en))
message("IDs únicos totales: ", length(ids))

# 2) Traer detalles por ID (record) y configuración de guardado incremental

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

out_file_raw <- "data/raw/espaciociencia_raw_2018_2025_ecosistema_lookup.csv"
out_file_corpus <- "data/processed/espaciociencia_ecosistema_2018_2025_descripciones.csv"

# 3) Determinar qué IDs ya fueron procesados (checkpoint desde raw)
ids_done <- character(0)
if (file.exists(out_file_raw)) {
  ids_done <- readr::read_csv(out_file_raw, show_col_types = FALSE, col_types = cols(.default = "c"))$id
  ids_done <- unique(ids_done)
}

ids_pendientes <- setdiff(ids, ids_done)

message("IDs ya procesados (raw): ", length(ids_done))
message("IDs pendientes: ", length(ids_pendientes))

# 4) Procesar en bloques
block_size <- 500
bloques <- split(ids_pendientes, ceiling(seq_along(ids_pendientes) / block_size))

append_csv <- function(df, path) {
  if (nrow(df) == 0) return(invisible(NULL))
  if (!file.exists(path)) {
    utils::write.table(df, file = path, sep = ",", row.names = FALSE, col.names = TRUE, append = FALSE, quote = TRUE)
  } else {
    utils::write.table(df, file = path, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE, quote = TRUE)
  }
}

for (b in seq_along(bloques)) {
  ids_b <- bloques[[b]]
  message("=== Bloque ", b, " / ", length(bloques), " | n=", length(ids_b), " ===")
  
  df_raw_b <- purrr::map_dfr(seq_along(ids_b), function(i) {
    if (i %% 50 == 0) message("Avance bloque ", b, ": ", i, " / ", length(ids_b))
    fetch_record_row(ids_b[[i]], pause_sec = 0.5)
  })
  
  # Guardar raw incremental
  append_csv(df_raw_b, out_file_raw)
  
  # Filtrar y guardar corpus incremental
  df_corpus_b <- filter_corpus(df_raw_b, year_min = 2018, year_max = 2025)
  append_csv(df_corpus_b, out_file_corpus)
  
  message("Bloque ", b, " OK | raw: ", nrow(df_raw_b), " | filtrados: ", nrow(df_corpus_b))
}

message("OK: guardado raw en: ", out_file_raw)
message("OK: guardado corpus en: ", out_file_corpus)

