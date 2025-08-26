#### Importación de Datos
install.packages("readr")
install.packages("dplyr")
install.packages("plyr")

library(readr)
library(dplyr)
library(plyr)

# --- NO OCUPADOS ---
no_ocupados_feb <- read_delim("Febrero_2024/CSV/No ocupados.CSV", delim = ";", escape_double = FALSE, trim_ws = TRUE)
no_ocupados_mar <- read_delim("Marzo_2024/CSV/No ocupados.CSV", delim = ";", escape_double = FALSE, trim_ws = TRUE)
no_ocupados_abr <- read_delim("Abril_2024/CSV/No ocupados.CSV", delim = ";", escape_double = FALSE, trim_ws = TRUE)

no_ocupados <- bind_rows(no_ocupados_feb, no_ocupados_mar, no_ocupados_abr)


# --- OTRAS FORMAS DE TRABAJO ---
otras_feb <- read_delim("Febrero_2024/CSV/Otras formas de trabajo.CSV", delim = ";", escape_double = FALSE, trim_ws = TRUE)
otras_mar <- read_delim("Marzo_2024/CSV/Otras formas de trabajo.CSV", delim = ";", escape_double = FALSE, trim_ws = TRUE)
otras_abr <- read_delim("Abril_2024/CSV/Otras formas de trabajo.CSV", delim = ";", escape_double = FALSE, trim_ws = TRUE)

otras_formas_de_trabajo <- bind_rows(otras_feb, otras_mar, otras_abr)


# --- MIGRACION ---
# Forzamos a leer la columna P3374S1 siempre como texto
migracion_feb <- read_delim("Febrero_2024/CSV/Migración.CSV", delim = ";", escape_double = FALSE, trim_ws = TRUE,
                            col_types = cols(P3374S1 = col_character()))
migracion_mar <- read_delim("Marzo_2024/CSV/Migración.CSV", delim = ";", escape_double = FALSE, trim_ws = TRUE,
                            col_types = cols(P3374S1 = col_character()))
migracion_abr <- read_delim("Abril_2024/CSV/Migración.CSV", delim = ";", escape_double = FALSE, trim_ws = TRUE,
                            col_types = cols(P3374S1 = col_character()))

# Convertimos a numérica
migracion_feb$P3374S1 <- as.numeric(migracion_feb$P3374S1)
migracion_mar$P3374S1 <- as.numeric(migracion_mar$P3374S1)
migracion_abr$P3374S1 <- as.numeric(migracion_abr$P3374S1)

# Ahora sí podemos unir
migracion <- bind_rows(migracion_feb, migracion_mar, migracion_abr)


#### Procesamiento y unión de bases ####

base <- plyr::join_all(
  list(
    # Variables de NO OCUPADOS
    no_ocupados %>%
      group_by(DIRECTORIO) %>%
      summarise(
        semanas_busqueda = mean(P7250, na.rm = TRUE),
        salario_esperado = mean(P1806, na.rm = TRUE),
        horas_disponibles = mean(P7260, na.rm = TRUE),
        buscado_como = first(P7280), # variable cualitativa
        .groups = 'drop'
      ),
    
    # Variables de OTRAS FORMAS DE TRABAJO
    otras_formas_de_trabajo %>%
      group_by(DIRECTORIO) %>%
      summarise(
        horas_huerta = mean(P309452, na.rm = TRUE),
        valor_huerta = mean(P309453, na.rm = TRUE),
        horas_cria = mean(P309552, na.rm = TRUE),
        costo_cria = first(P309553), # variable cualitativa
        .groups = 'drop'
      ),
    
    # Variables de MIGRACION
    migracion %>%
      group_by(DIRECTORIO) %>%
      summarise(
        tiempo_exterior = mean(P3375S1, na.rm = TRUE),
        tiempo_trabajo_exterior = mean(P3378S1, na.rm = TRUE),
        anio_salida = mean(P3380, na.rm = TRUE),
        nacionalidad = first(P3374S1), # ahora sí numérica homogénea
        .groups = 'drop'
      )
  ),
  by = 'DIRECTORIO', type = "left", match = "all"
)


#### Análisis descriptivo preliminar ####

summary(base)

# Ejemplo: gráficos rápidos
library(ggplot2)

# Histograma salario esperado
ggplot(base, aes(x = salario_esperado)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "black") + 
  labs(title = "Distribución del salario mínimo esperado")

# Boxplot horas disponibles para trabajar por tipo de búsqueda de trabajo
ggplot(base, aes(x = buscado_como, y = horas_disponibles)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Horas disponibles según tipo de búsqueda de trabajo")
