# scripts/07_table_terms_by_cluster.R
# Tabla de términos representativos por cluster (k = 5)
# Insumo: outputs/clustering/clusters_top_terms.csv

library(dplyr)
library(readr)
library(tidyr)
library(stringr)

in_file <- "outputs/clustering/clusters_top_terms.csv"
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

df_terms <- readr::read_csv(in_file, show_col_types = FALSE) %>%
  filter(k == 5) %>%
  mutate(
    palabra = str_replace_all(palabra, "_", " "),
    cluster = as.integer(cluster)
  )

# (Opcional) nombres provisorios definidos por ti
cluster_names <- tibble::tibble(
  cluster = 1:5,
  cluster_nombre = c(
    "Ecosistema en relación a encuadres académico-formativos",
    "Ecosistema en relación a articulaciones socio-territoriales",
    "Ecosistema en relación a configuraciones ambientales",
    "Ecosistema en relación a unidades empírico-materiales",
    "Ecosistema en relación a marcos generales"
  )
)

df_terms <- df_terms %>%
  left_join(cluster_names, by = "cluster") %>%
  arrange(cluster, desc(score))

# Tabla “larga” (para inspección y trazabilidad)
out_long <- "outputs/tables/cluster_terms_k5_long.csv"
readr::write_csv(df_terms, out_long)

# Tabla “ancha” (ideal para pegar como tabla en paper)
df_wide <- df_terms %>%
  group_by(cluster, cluster_nombre) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  select(rank, cluster, cluster_nombre, palabra, score) %>%
  pivot_wider(
    id_cols = rank,
    names_from = cluster,
    values_from = c(palabra, score),
    names_glue = "c{cluster}_{.value}"
  )

out_wide <- "outputs/tables/cluster_terms_k5_wide.csv"
readr::write_csv(df_wide, out_wide)

message("OK: tabla larga -> ", out_long)
message("OK: tabla ancha -> ", out_wide)
