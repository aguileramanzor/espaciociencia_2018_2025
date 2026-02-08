# R/scrape_helpers.R
# Helpers para scraping vía API VuFind (Espacio Ciencia)
# Estrategia:
# 1) Buscar IDs con /api/v1/search (paginación)
# 2) Para cada ID, traer detalle con /api/v1/record
# 3) Filtrar por años 2018–2025 y por presencia ecosistema/ecosystem (en título o descripción)

library(httr2)
library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)

# Construye URL de búsqueda
build_search_url <- function(lookfor, page = 1, limit = 50, lng = "es", includeStaffView = TRUE) {
  base <- "https://espaciociencia.cl/vufind/api/v1/search"
  paste0(
    base,
    "?lookfor=", URLencode(lookfor, reserved = TRUE),
    "&page=", page,
    "&limit=", limit,
    "&includeStaffView=", tolower(as.character(includeStaffView)),
    "&lng=", lng
  )
}

# Descarga JSON desde una URL (con manejo simple de errores)
get_json <- function(url, retries = 6, base_sleep = 0.6) {
  for (i in seq_len(retries)) {
    resp <- tryCatch(
      request(url) |> req_perform(),
      error = function(e) e
    )
    
    # Si fue error de httr2, reintentar
    if (inherits(resp, "error")) {
      Sys.sleep(base_sleep * i)
      next
    }
    
    status <- httr2::resp_status(resp)
    
    # Reintentar en 429/5xx
    if (status == 429 || status >= 500) {
      Sys.sleep(base_sleep * i)
      next
    }
    
    txt <- httr2::resp_body_string(resp)
    return(jsonlite::fromJSON(txt, simplifyVector = TRUE))
  }
  
  # Si falla todo, devolver NULL para que el pipeline no se caiga
  NULL
}


# Extrae IDs desde la respuesta de /search
# Nota: el campo exacto puede variar ("records", "results", etc.). Cubrimos los casos típicos.
extract_ids_from_search <- function(search_json) {
  if (!is.null(search_json$records$id)) return(unique(search_json$records$id))
  if (!is.null(search_json$records$uniqueID)) return(unique(search_json$records$uniqueID))
  if (!is.null(search_json$results$id)) return(unique(search_json$results$id))
  if (!is.null(search_json$items$id)) return(unique(search_json$items$id))
  character(0)
}

# Obtiene IDs iterando páginas hasta que no haya más resultados
search_all_ids <- function(lookfor, limit = 50, max_pages = Inf, pause_sec = 0.2) {
  ids <- character(0)
  page <- 1
  
  repeat {
    if (page > max_pages) break
    
    url <- build_search_url(lookfor = lookfor, page = page, limit = limit)
    sj <- get_json(url)
    batch <- extract_ids_from_search(sj)
    
    if (length(batch) == 0) break
    
    ids <- unique(c(ids, batch))
    message("lookfor='", lookfor, "' | página ", page, " | ids acumulados: ", length(ids))
    
    page <- page + 1
    Sys.sleep(pause_sec)
  }
  
  ids
}

# Construye URL del record
build_record_url <- function(id,
                             includeStaffView = TRUE,
                             includeSimilarItems = FALSE,
                             similarLimit = 0) {
  base <- "https://espaciociencia.cl/vufind/api/v1/record"
  paste0(
    base,
    "?id=", URLencode(id, reserved = TRUE),
    "&includeStaffView=", tolower(as.character(includeStaffView)),
    "&includeSimilarItems=", tolower(as.character(includeSimilarItems)),
    "&similarLimit=", similarLimit
  )
}

# Toma el "primer valor útil" desde vectores/listas
pluck_scalar <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.atomic(x) && length(x) >= 1) return(as.character(x[1]))
  if (is.list(x) && length(x) >= 1) return(as.character(x[[1]]))
  NA_character_
}

# Busca un campo en varias rutas posibles dentro del JSON
pluck_field_any <- function(j, paths) {
  for (p in paths) {
    val <- tryCatch(purrr::pluck(j, !!!p), error = function(e) NULL)
    out <- pluck_scalar(val)
    if (!is.na(out) && nzchar(trimws(out))) return(trimws(out))
  }
  NA_character_
}

parse_record_to_row <- function(record_json, id) {
  
  rec <- record_json$records
  if (is.null(rec) || nrow(rec) == 0) {
    return(tibble::tibble(
      id = id,
      titulo = NA_character_,
      descripcion = NA_character_,
      anio = NA_integer_,
      url_api_record = build_record_url(id)
    ))
  }
  
  # staffViewData es un data.frame (1 fila)
  sv <- rec$staffViewData
  
  titulo <- if ("title" %in% names(sv)) as.character(sv$title[1]) else NA_character_
  descripcion <- if ("description" %in% names(sv)) as.character(sv$description[1]) else NA_character_
  anio_chr <- if ("publishDate" %in% names(sv)) as.character(sv$publishDate[1]) else NA_character_
  
  anio_num <- suppressWarnings(as.integer(stringr::str_extract(anio_chr, "\\d{4}")))
  
  tibble::tibble(
    id = id,
    titulo = ifelse(!is.na(titulo) && nzchar(trimws(titulo)), trimws(titulo), NA_character_),
    descripcion = ifelse(!is.na(descripcion) && nzchar(trimws(descripcion)), trimws(descripcion), NA_character_),
    anio = anio_num,
    url_api_record = build_record_url(id)
  )
}

# Trae y parsea un record por id
fetch_record_row <- function(id, pause_sec = 0.3) {
  url <- build_record_url(id)
  rj <- get_json(url)
  
  Sys.sleep(pause_sec)
  
  if (is.null(rj)) {
    return(tibble::tibble(
      id = id,
      titulo = NA_character_,
      descripcion = NA_character_,
      anio = NA_integer_,
      url_api_record = build_record_url(id)
    ))
  }
  
  parse_record_to_row(rj, id = id)
}


# Filtra por años y por presencia de ecosistema/ecosystem en título o descripción
filter_corpus <- function(df, year_min = 2018, year_max = 2025) {
  df %>%
    filter(!is.na(anio), anio >= year_min, anio <= year_max) %>%
    mutate(
      txt = str_to_lower(paste(titulo, descripcion, sep = " ")),
      contiene_ecosistema = str_detect(txt, "\\becosistema(s)?\\b") | str_detect(txt, "\\becosystem(s)?\\b")
    ) %>%
    filter(contiene_ecosistema) %>%
    select(-txt)
}
