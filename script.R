# ==============================================================================
# PROYECTO DE TESIS: ANONIMIZACIÓN Y ANÁLISIS DE DATOS VEHICULARES (2011-2024)
# Autor: Cristian Mendez
# Fecha: 24-10-2025
# Descripción: Script para la consolidación, limpieza, análisis exploratorio y 
#              aplicación de técnicas de anonimización (k-anonimidad, l-diversidad)
#              sobre registros de permisos de circulación de La Serena.
# ==============================================================================

# --- 0. CONFIGURACIÓN INICIAL Y LIBRERÍAS ---

# Librerías necesarias
library(tidyverse)  # Manipulación y visualización
library(readr)      # Lectura de CSV
library(janitor)    # Limpieza de columnas
library(stringr)    # Manejo de texto
library(digest)     # Hashing criptográfico
library(sdcMicro)   # Anonimización estadística (SDC)
library(patchwork)  # Composición de gráficos

# Definir directorios de trabajo
# 'getwd()' asegura que funcione en cualquier PC si se abre desde el proyecto (.Rproj)
DIR_BASE <- getwd()
DIR_DATOS <- file.path(DIR_BASE, "data-raw")
DIR_IMG <- file.path(DIR_BASE, "imagenes") # Carpeta para guardar los gráficos

# Crear carpeta de imágenes si no existe
if (!dir.exists(DIR_IMG)) {
  dir.create(DIR_IMG)
  cat(">>> Carpeta 'imagenes' creada exitosamente.\n")
}


# --- 1. CARGA Y CONSOLIDACIÓN DE DATOS (ETL) ---

# Lista de archivos anuales
archivos_csv <- c(
  "permiso-de-circulacion-2011.csv", "permiso-de-circulacion-2012.csv",
  "permiso-de-circulacion-2013.csv", "permiso-de-circulacion-2014.csv",
  "permiso-de-circulacion-2015.csv", "permiso-de-circulacion-2016.csv",
  "permiso-de-circulacion-2017.csv", "permiso-de-circulacion-2018.csv",
  "permiso-de-circulacion-2019.csv", "permiso-de-circulacion-2020.csv",
  "permiso-de-circulacion-2021.csv", "permiso-de-circulacion-2022.csv",
  "permiso-de-circulacion-2023.csv", "permiso-de-circulacion-2024.csv"
)

# Proceso de lectura y unión
datos_consolidados <- map_dfr(archivos_csv, function(nombre_archivo) {
  ruta_completa <- file.path(DIR_DATOS, nombre_archivo)
  anio_proceso <- as.integer(str_extract(nombre_archivo, "\\d{4}"))
  
  # Lectura con codificación 'latin1' para caracteres especiales
  df <- read_delim(
    ruta_completa, 
    delim = ";", 
    locale = locale(encoding = "latin1"),
    col_types = cols(.default = "c")
  )
  
  df <- df %>% mutate(ANIO_PROCESO = anio_proceso)
  
  # Estandarización: Eliminar columnas extra (ej. 'Comuna' en 2016)
  if ("Comuna" %in% names(df)) {
    df <- df %>% select(-Comuna)
  }
  
  return(df)
})

# Limpieza de tipos de datos
datos_limpios <- datos_consolidados %>%
  clean_names() %>%
  mutate(
    ano_fabricacion = as.integer(ano_fabricacion),
    tasacion = as.numeric(tasacion),
    valor_permiso = as.numeric(valor_permiso),
    cc = as.integer(cc),
    carga = as.numeric(carga),
    asientos = as.integer(asientos)
  )

cat(">>> Datos cargados. Total registros:", nrow(datos_limpios), "\n")


# --- 2. LIMPIEZA Y ESTANDARIZACIÓN DE CATEGORÍAS ---

datos_limpios <- datos_limpios %>%
  mutate(
    # Vehículo: Unificar sinónimos y typos
    vehiculo_limpio = str_to_upper(vehiculo) %>% str_squish(),
    vehiculo_limpio = case_when(
      vehiculo_limpio %in% c("MOTO", "MOTOS", "MOTONETA") ~ "MOTOCICLETA",
      vehiculo_limpio == "CUADRIMOTO" ~ "CUATRIMOTO",
      vehiculo_limpio == "TRICICLO MOTOR" ~ "TRIMOTO",
      TRUE ~ vehiculo_limpio
    ),
    
    # Marca: Estandarizar nombres
    marca_limpia = str_to_upper(marca) %>% str_squish(),
    marca_limpia = case_when(
      marca_limpia == "HAO JUE" ~ "HAOJUE",
      marca_limpia == "KIA MOTORS" ~ "KIA",
      TRUE ~ marca_limpia
    ),
    
    # Combustible: Agrupar variantes
    combustible_limpio = str_to_upper(combustible) %>% str_squish(),
    combustible_limpio = case_when(
      combustible_limpio %in% c("BENC", "GASO") ~ "BENCINA",
      combustible_limpio %in% c("DIES", "DIÉS") ~ "DIESEL",
      combustible_limpio %in% c("HIBR", "HÍBR") ~ "HIBRIDO",
      combustible_limpio %in% c("ELEC", "ELÉC") ~ "ELECTRICO",
      combustible_limpio %in% c("GASN") ~ "GAS",
      combustible_limpio %in% c("BENCINA", "DIESEL", "HIBRIDO", "ELECTRICO", "GAS", "DUAL") ~ combustible_limpio,
      TRUE ~ "NO ESPECIFICADO"
    ),
    
    # Transmisión: Simplificar
    transmision_limpia = str_to_upper(transmision) %>% str_squish(),
    transmision_limpia = case_when(
      transmision_limpia %in% c("MEC", "MECÁNICA") ~ "MECANICA",
      transmision_limpia %in% c("AUT", "AUTOMÁTICA", "AUTOMATIZA", "CVT", "AMT", "DCT") ~ "AUTOMATICA",
      transmision_limpia %in% c("MECANICA", "AUTOMATICA") ~ transmision_limpia,
      TRUE ~ "NO ESPECIFICADO"
    )
  )

# Verificación de integridad
conteo_anual <- table(datos_limpios$anio_proceso)
if (nrow(datos_limpios) == sum(conteo_anual)) {
  cat(">>> Verificación exitosa: Integridad de datos confirmada.\n")
} else {
  warning(">>> Advertencia: Discrepancia en el total de registros.\n")
}


# --- 3. ANÁLISIS EXPLORATORIO DE DATOS (EDA) ---

# 3.1 Evolución del Parque Vehicular
evolucion_parque <- datos_limpios %>%
  group_by(anio_proceso) %>%
  summarise(total_vehiculos_unicos = n_distinct(placa))

grafico_evolucion <- ggplot(evolucion_parque, aes(x = anio_proceso, y = total_vehiculos_unicos)) +
  geom_bar(stat = "identity", fill = "#0072B2", color = "black") +
  geom_text(aes(label = total_vehiculos_unicos), vjust = -0.5, size = 3.5) +
  labs(title = "Evolución del Parque Vehicular en La Serena (2011-2024)", 
       x = "Año", y = "Cantidad") +
  scale_y_continuous(limits = c(0, max(evolucion_parque$total_vehiculos_unicos) * 1.15)) +
  theme_minimal() +
  scale_x_continuous(breaks = 2011:2024)

ggsave("grafico_evolucion_parque.png", plot = grafico_evolucion, path = DIR_IMG, width = 10, height = 6, dpi = 300)


# 3.2 Evolución de Combustibles
evolucion_combustible <- datos_limpios %>%
  group_by(anio_proceso, combustible_limpio) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(anio_proceso) %>%
  mutate(proporcion = n / sum(n)) %>%
  filter(combustible_limpio %in% c("BENCINA", "DIESEL", "ELECTRICO", "HIBRIDO"))

grafico_evolucion_comb <- ggplot(evolucion_combustible, aes(x = anio_proceso, y = proporcion, group = combustible_limpio, color = combustible_limpio)) +
  geom_line(size = 1.5) + geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 2011:2024) +
  labs(title = "Evolución de Tipos de Combustible (2011-2024)", x = "Año", y = "Proporción", color = "Combustible") +
  theme_minimal() + theme(legend.position = "bottom")

ggsave("grafico_evolucion_combustibles.png", plot = grafico_evolucion_comb, path = DIR_IMG, width = 12, height = 7, dpi = 300)


# 3.3 Composición del Parque 2024 (Gráfico Combinado)
datos_2024 <- datos_limpios %>% filter(anio_proceso == 2024)

g_tipos <- datos_2024 %>% count(vehiculo_limpio, sort=T) %>% top_n(10, n) %>%
  ggplot(aes(x=reorder(vehiculo_limpio, n), y=n)) + geom_bar(stat="identity", fill="#0072B2") + coord_flip() + 
  labs(title="A: Top 10 Tipos", x=NULL, y="Cantidad") + theme_minimal()

g_comb <- datos_2024 %>% count(combustible_limpio, sort=T) %>%
  ggplot(aes(x=reorder(combustible_limpio, n), y=n)) + geom_bar(stat="identity", fill="#009E73") + coord_flip() +
  labs(title="B: Combustibles", x=NULL, y="Cantidad") + theme_minimal()

g_marcas <- datos_2024 %>% count(marca_limpia, sort=T) %>% top_n(10, n) %>%
  ggplot(aes(x=reorder(marca_limpia, n), y=n)) + geom_bar(stat="identity", fill="#D55E00") + coord_flip() +
  labs(title="C: Top 10 Marcas", x=NULL, y="Cantidad") + theme_minimal()

grafico_final_combinado <- (g_tipos | g_comb) / g_marcas + 
  plot_annotation(title = 'Composición del Parque Vehicular (2024)')

ggsave("grafico_composicion_2024_combinado.png", plot = grafico_final_combinado, path = DIR_IMG, width = 12, height = 9, dpi = 300)


# --- 4. IMPLEMENTACIÓN DE SEUDONIMIZACIÓN ---

# Aplicación de Hashing para identificadores directos
datos_anonimizados <- datos_limpios %>%
  mutate(
    # Técnica 1: Hashing SHA-256
    placa_hash = sapply(placa, digest, algo = "sha256"),
    
    # Técnica 2: HMAC (Hashing con clave secreta)
    placa_hmac = sapply(placa, hmac,
                        key = "mi_clave_muy_secreta_para_la_tesis",
                        algo = "sha256")
  )

cat(">>> Seudonimización completada.\n")


# --- 5. IMPLEMENTACIÓN DE ANONIMIZACIÓN (SDC) ---

cuasi_ids <- c("ano_fabricacion", "marca_limpia", "vehiculo_limpio", "combustible_limpio")

# 5.1 k-Anonimidad (k=5)
cat(">>> Aplicando k-Anonimidad (k=5)... \n")
sdc_obj_k5_full <- createSdcObj(dat = datos_anonimizados, keyVars = cuasi_ids)
sdc_obj_k5_full <- kAnon(sdc_obj_k5_full, k = 5)
print(sdc_obj_k5_full)

# 5.2 l-Diversidad (l=2)
cat(">>> Evaluando l-Diversidad (l=2) sobre 'tipo_pago'... \n")
sdc_obj_l2_full <- createSdcObj(dat = datos_anonimizados, keyVars = cuasi_ids)
sdc_obj_l2_full <- ldiversity(
  obj = sdc_obj_l2_full,
  ldiv_index = "tipo_pago",
  l = 2
)
print(sdc_obj_l2_full)
print(sdc_obj_l2_full@risk$ldiversity)


# --- 6. EVALUACIÓN DE UTILIDAD (IMPACTO) ---

# Recuperación de datos anonimizados
datos_finales_qis <- extractManipData(sdc_obj_l2_full)

# Método robusto para recuperar año y seudónimo del dataframe original
col_anio <- if("anio_proceso" %in% names(datos_anonimizados)) "anio_proceso" else "ANIO_PROCESO"
datos_finales_qis$anio_proceso <- datos_anonimizados[[col_anio]]
datos_finales_qis$placa_hmac <- datos_anonimizados$placa_hmac

# Datasets comparativos
df_original <- datos_anonimizados %>%
  group_by(anio_proceso = .data[[col_anio]]) %>%
  summarise(conteo = n_distinct(placa_hmac)) %>%
  mutate(tipo = "Original")

df_anonimizado <- datos_finales_qis %>%
  filter(!is.na(ano_fabricacion)) %>% # Filtrar supresiones
  group_by(anio_proceso) %>%
  summarise(conteo = n_distinct(placa_hmac)) %>%
  mutate(tipo = "Anonimizado")

comparacion_utilidad <- bind_rows(df_original, df_anonimizado)

# Gráfico de impacto
grafico_utilidad <- ggplot(comparacion_utilidad, aes(x = anio_proceso, y = conteo, fill = tipo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Impacto de la Anonimización en el Volumen de Datos",
    subtitle = "Comparación antes y después de aplicar k-anonimidad",
    x = "Año", y = "Vehículos Únicos", fill = "Dataset"
  ) +
  scale_fill_manual(values = c("Original" = "#0072B2", "Anonimizado" = "#D55E00")) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("grafico_utilidad_comparacion.png", plot = grafico_utilidad, path = DIR_IMG, width = 10, height = 6, dpi = 300)

cat(">>> Script finalizado exitosamente. Imágenes guardadas en:", DIR_IMG, "\n")