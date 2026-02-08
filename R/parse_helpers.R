# R/parse_helpers.R

#' Extrae la descripción desde una ficha "Detalles" de Espacio Ciencia
#' Estrategia:
#' 1) Intentar extraer desde el cuerpo (div.result-card p)
#' 2) Si no aparece (sitio JS), intentar desde meta tags del <head>
extract_descripcion <- function(html) {
  stopifnot(inherits(html, "xml_document") || inherits(html, "xml_node"))
  
  # 1) Intento en el cuerpo (si el HTML viene completo)
  nodes <- rvest::html_elements(html, "div.result-card p")
  if (length(nodes) > 0) {
    textos <- rvest::html_text2(nodes)
    textos <- trimws(textos)
    textos <- textos[nzchar(textos)]
    if (length(textos) > 0) {
      return(textos[which.max(nchar(textos))])
    }
  }
  
  # 2) Fallback: meta tags (útil cuando el contenido se carga por JS)
  meta_desc <- rvest::html_element(html, "meta[name='description']") |>
    rvest::html_attr("content")
  
  if (!is.na(meta_desc) && nzchar(trimws(meta_desc))) {
    return(trimws(meta_desc))
  }
  
  og_desc <- rvest::html_element(html, "meta[property='og:description']") |>
    rvest::html_attr("content")
  
  if (!is.na(og_desc) && nzchar(trimws(og_desc))) {
    return(trimws(og_desc))
  }
  
  NA_character_
}
