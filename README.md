# Anonimización y Análisis de Datos Vehiculares (2011-2024) 🚗🔒

Este repositorio contiene el código fuente y la documentación del proyecto de tesis para el **Magíster en Estadística**, enfocado en la evaluación de técnicas de protección de privacidad aplicadas a registros administrativos municipales.

## 📄 Descripción del Proyecto

El objetivo principal de este estudio es evaluar el equilibrio (*trade-off*) entre la privacidad de los datos personales y su utilidad estadística. Se utiliza un conjunto de datos longitudinal de permisos de circulación de la comuna de La Serena (Chile) entre los años 2011 y 2024.

El proyecto implementa un flujo de trabajo completo de ciencia de datos:
1.  **ETL:** Carga, consolidación y limpieza de más de 1.3 millones de registros.
2.  **EDA:** Análisis Exploratorio de Datos para identificar tendencias del parque automotriz.
3.  **Privacidad:** Aplicación de técnicas avanzadas de anonimización y seudonimización.
4.  **Evaluación:** Medición del impacto de estas técnicas en la utilidad analítica de los datos.

## 🛠️ Tecnologías y Librerías

El proyecto está desarrollado íntegramente en **R**.

* **Manipulación de Datos:** `tidyverse`, `dplyr`, `readr`, `janitor`, `stringr`.
* **Visualización:** `ggplot2`, `patchwork`.
* **Anonimización y Seguridad:**
    * `digest`: Para técnicas de Hashing y HMAC.
    * `sdcMicro`: Para algoritmos de Control de Revelación Estadística (k-anonimidad, l-diversidad).
* **Reporte:** `Quarto` para la generación de informes dinámicos en PDF.

## 🛡️ Técnicas de Privacidad Implementadas

### Seudonimización (Identificadores Directos)
Se protege la **Placa Patente** utilizando dos métodos para permitir el análisis longitudinal sin exponer el dato real:
* **Hashing Criptográfico (SHA-256)**.
* **HMAC (Hash-based Message Authentication Code)** con clave secreta.

### Anonimización (Cuasi-Identificadores)
Se protegen atributos como *Marca*, *Año de Fabricación*, *Tipo de Vehículo* y *Combustible* mediante:
* **k-Anonimidad (k=5):** Garantizando que cada registro sea indistinguible de al menos otros 4.
* **l-Diversidad (l=2):** Evaluando la diversidad de valores sensibles (*Tipo de Pago*) dentro de los grupos anónimos.

## 📂 Estructura del Repositorio

```text
├── script.R              # Script principal (ETL, EDA, Anonimización, Evaluación)
├── Proyecto Tesis.qmd    # Informe dinámico en Quarto (Código + Texto)
├── Proyecto-Tesis.pdf    # Informe final generado
├── imagenes/             # Gráficos generados por el script
├── .gitignore            # Configuración para excluir datos sensibles
└── README.md             # Documentación del proyecto
