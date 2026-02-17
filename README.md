Este repositorio documenta el proceso de construcción y análisis de un corpus de publicaciones indexadas en la plataforma Espacio Ciencia, recuperadas mediante web scraping bajo criterios de búsqueda asociados al término “ecosistema”.

El objetivo del proyecto fue identificar patrones de uso del concepto en descripciones de publicaciones científicas chilenas entre 2018 y 2025.

El corpus fue construido mediante extracción automatizada de registros desde la plataforma, seguida de: limpieza y normalización de datos,
depuración de registros incompletos, separación entre versiones crudas (raw) y procesadas, generación de un dataset analítico para el estudio semántico.
Todos los procedimientos están contenidos en los scripts del repositorio.

El análisis combina operaciones léxico-estadísticas e interpretación conceptual: tokenización y limpieza de texto, eliminación de stopwords en español e inglés,
cálculo de frecuencias y ponderaciones TF–IDF, reducción dimensional (SVD), agrupamiento mediante K-means (k = 5). El clustering permite identificar agrupamientos estadísticos de documentos según proximidad léxica.

La denominación e interpretación de los clusters no es automática, sino resultado de una operación analítica posterior basada en los términos característicos.

Este repositorio contiene exclusivamente: scripts de extracción, procedimientos de limpieza y construcción de datasets, scripts de análisis, outputs derivados del procesamiento.

No contiene el manuscrito del artículo asociado. El análisis interpretativo completo se desarrolla en el trabajo académico actualmente en preparación.

