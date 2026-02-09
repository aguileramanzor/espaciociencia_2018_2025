# ================================
# CLUSTERING SEMÁNTICO (GLOBAL)
# ================================

library(tidytext)
library(tidyr)
library(Matrix)
library(stats)

# Usar solo corpus main (excluye descripciones vacías y muy largas)
df_main <- readr::read_csv(
  "data/processed/espaciociencia_ecosistema_2018_2025_analitico_main.csv",
  show_col_types = FALSE
)

# Tokenización
tokens <- df_main %>%
  select(id, anio, descripcion) %>%
  unnest_tokens(palabra, descripcion) %>%
  filter(
    str_detect(palabra, "[a-záéíóúñ]"),
    nchar(palabra) > 2
  )

# Eliminar stopwords ES + EN
# Stopwords en inglés (tidytext)
stop_en <- tidytext::stop_words %>%
  filter(lexicon == "snowball") %>%
  select(word)

# Stopwords básicas en español (lista manual mínima)
stop_es <- tibble::tibble(
  word = c(
    "de","la","que","el","en","y","a","los","del","se","las","por","un","para",
    "con","no","una","su","al","lo","como","más","pero","sus","le","ya","o",
    "este","sí","porque","esta","entre","cuando","muy","sin","sobre","también"
  )
)

tokens <- tokens %>%
  anti_join(stop_en, by = c("palabra" = "word")) %>%
  anti_join(stop_es, by = c("palabra" = "word"))


# Frecuencia término-documento
dtm <- tokens %>%
  count(id, palabra) %>%
  cast_dtm(document = id, term = palabra, value = n)

message("DTM creada: ",
        nrow(dtm), " documentos | ",
        ncol(dtm), " términos")

# ================================
# CO-OCURRENCIAS (GLOBAL) CON "ecosistema(s)/ecosystem"
# ================================

# 1) Definir términos foco
foco <- c("ecosistema", "ecosistemas", "ecosystem", "ecosystems")

# 2) Subconjunto de tokens solo en documentos que contienen el foco
docs_foco <- tokens %>%
  filter(palabra %in% foco) %>%
  distinct(id)

tokens_foco_docs <- tokens %>%
  semi_join(docs_foco, by = "id") %>%
  filter(!palabra %in% foco)

message("Docs con foco: ", nrow(docs_foco))
message("Tokens en docs con foco (sin foco): ", nrow(tokens_foco_docs))

# 3) Co-ocurrencias por documento: contar términos que aparecen junto al foco
cooc <- tokens_foco_docs %>%
  count(palabra, sort = TRUE)

# 4) Top 30 términos co-ocurrentes
cooc_top30 <- cooc %>% slice_head(n = 30)
print(cooc_top30, n = 30)

# ================================
# CO-OCURRENCIAS CON PMI (términos característicos)
# ================================

# Frecuencia global (en todo el corpus) para referencia
freq_all <- tokens %>%
  count(palabra, sort = TRUE)

# Frecuencia en docs con foco
freq_in_foco <- tokens_foco_docs %>%
  count(palabra, sort = TRUE) %>%
  rename(n_foco = n)

# Unir y calcular PMI aproximado:
# PMI ~ log2( P(w|foco) / P(w) )
# donde P(w|foco) = n_foco / total_tokens_foco_docs
# y P(w) = n_all / total_tokens_all

total_foco <- nrow(tokens_foco_docs)
total_all  <- nrow(tokens)

pmi_tbl <- freq_in_foco %>%
  left_join(freq_all %>% rename(n_all = n), by = "palabra") %>%
  mutate(
    p_w_foco = n_foco / total_foco,
    p_w_all  = n_all  / total_all,
    pmi = log2(p_w_foco / p_w_all)
  ) %>%
  filter(n_foco >= 50) %>%            # umbral para estabilidad
  arrange(desc(pmi))

pmi_top30 <- pmi_tbl %>% slice_head(n = 30)
print(pmi_top30, n = 30)

# ================================
# CLUSTERING SEMÁNTICO (TF–IDF + SVD + KMEANS) | AUTÓNOMO
# ================================

library(dplyr)
library(readr)
library(stringr)
library(tidytext)
library(tidyr)
library(tibble)
library(purrr)
library(Matrix)
library(irlba)
library(tm)

# 0) Cargar corpus MAIN (ya filtrado: sin vacíos + sin muy largas)
in_main <- "data/processed/espaciociencia_ecosistema_2018_2025_analitico_main.csv"

df_main <- readr::read_csv(in_main, show_col_types = FALSE) %>%
  mutate(
    id = as.character(id),
    anio = as.integer(anio),
    descripcion = as.character(descripcion),
    texto = str_squish(str_to_lower(descripcion))
  ) %>%
  filter(!is.na(id), !is.na(texto), texto != "")

message("Docs main (post): ", nrow(df_main))

# 1) Tokenización
tokens <- df_main %>%
  select(id, texto) %>%
  tidytext::unnest_tokens(palabra, texto) %>%
  filter(
    str_detect(palabra, "[a-záéíóúñ]"),
    nchar(palabra) > 2
  )

# 2) Stopwords ES + EN (robusto)
stop_es <- tibble(word = tm::stopwords("spanish"))
stop_en <- tibble(word = tm::stopwords("english"))

tokens <- tokens %>%
  anti_join(stop_es, by = c("palabra" = "word")) %>%
  anti_join(stop_en, by = c("palabra" = "word")) %>%
  filter(!palabra %in% c("ecosistema","ecosistemas","ecosystem","ecosystems"))

message("Tokens (limpios): ", nrow(tokens))

# 3) Reducir vocabulario por frecuencia mínima
min_freq <- 50
vocab <- tokens %>%
  count(palabra, sort = TRUE) %>%
  filter(n >= min_freq)

tokens_red <- tokens %>% semi_join(vocab, by = "palabra")

message("Vocabulario (>= ", min_freq, "): ", nrow(vocab))

# 4) TF–IDF (con columnas explícitas para cast_sparse)
tfidf <- tokens_red %>%
  count(id, palabra, sort = FALSE) %>%
  bind_tf_idf(palabra, id, n) %>%
  transmute(
    document = as.character(id),
    term = as.character(palabra),
    value = as.numeric(tf_idf)
  )

# Matriz esparsa docs x términos
X <- tidytext::cast_sparse(tfidf, document, term, value)

message("Matriz TF–IDF: ", nrow(X), " docs | ", ncol(X), " términos")


# 5) SVD (espacio latente)
k_latent <- 50
svd_res <- irlba::irlba(X, nv = k_latent)

X_latent <- svd_res$u %*% diag(svd_res$d)
rownames(X_latent) <- rownames(X)

message("Latente: ", nrow(X_latent), " docs | ", ncol(X_latent), " dims")

# 6) K-means para k = 4..7 (guardar asignaciones)
set.seed(123)
k_vals <- 4:7

clusters_df <- purrr::map_dfr(k_vals, function(k){
  km <- stats::kmeans(X_latent, centers = k, nstart = 25)
  tibble(
    id = rownames(X_latent),
    cluster = as.integer(km$cluster),
    k = as.integer(k)
  )
})

# 7) Términos representativos por cluster (promedio TF–IDF)
#    OJO: 'tfidf' ahora tiene columnas: document, term, value

top_terms_by_cluster <- purrr::map_dfr(k_vals, function(k){
  cl <- clusters_df %>%
    filter(k == !!k) %>%
    transmute(document = as.character(id), cluster = as.integer(cluster))
  
  tfidf %>%
    inner_join(cl, by = "document") %>%
    group_by(cluster, term) %>%
    summarise(score = mean(value), .groups = "drop") %>%
    group_by(cluster) %>%
    slice_max(order_by = score, n = 15, with_ties = FALSE) %>%
    mutate(k = as.integer(k)) %>%
    ungroup()
})

# Renombrar para outputs más claros
top_terms_by_cluster <- top_terms_by_cluster %>%
  rename(palabra = term)


# 8) Guardar outputs
dir.create("outputs/clustering", recursive = TRUE, showWarnings = FALSE)

readr::write_csv(clusters_df, "outputs/clustering/clusters_kmeans_assignments.csv")
readr::write_csv(top_terms_by_cluster, "outputs/clustering/clusters_top_terms.csv")

message("OK: clustering guardado en outputs/clustering/")

# ================================
# ANÁLISIS TEMPORAL POR CLUSTER (k = 5)
# ================================

library(ggplot2)
library(scales)

# 1) Seleccionar k = 5
clusters_k5 <- clusters_df %>%
  filter(k == 5)

# 2) Unir año del documento
clusters_k5 <- clusters_k5 %>%
  left_join(
    df_main %>% select(id, anio),
    by = "id"
  )

# Chequeo rápido
message("Docs con cluster y año: ", nrow(clusters_k5))
print(count(clusters_k5, anio, cluster))

# 3) Distribución por año y cluster (proporciones)
dist_anual <- clusters_k5 %>%
  count(anio, cluster) %>%
  group_by(anio) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# ================================
# VISUALIZACIÓN 1: ÁREA APILADA
# ================================

p_area <- ggplot(dist_anual, aes(x = anio, y = prop, fill = factor(cluster))) +
  geom_area(alpha = 0.85) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Distribución temporal de clusters semánticos (k = 5)",
    x = "Año",
    y = "Proporción de documentos",
    fill = "Cluster"
  ) +
  theme_minimal(base_size = 12)

print(p_area)

# Guardar figura
ggsave(
  "outputs/clustering/cluster_temporal_area_k5.png",
  p_area,
  width = 8,
  height = 5,
  dpi = 300
)

# ================================
# VISUALIZACIÓN 2: LÍNEAS POR CLUSTER
# ================================

p_line <- ggplot(dist_anual, aes(x = anio, y = prop, color = factor(cluster))) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Evolución temporal de clusters semánticos (k = 5)",
    x = "Año",
    y = "Proporción de documentos",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 12)

print(p_line)

# Guardar figura
ggsave(
  "outputs/clustering/cluster_temporal_line_k5.png",
  p_line,
  width = 8,
  height = 5,
  dpi = 300
)

message("OK: análisis temporal y figuras guardadas en outputs/clustering/")