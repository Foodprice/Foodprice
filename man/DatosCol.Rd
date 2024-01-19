#------------------------------------------------------------------------------------------#
#                 D O C U M E N T A C I Ó N: FUNCIÓN-Datos_dane_col                        #
#-----------------------------------------------------------------------------------------#

\name{DatosCol}
\alias{DatosCol}
\title{Función para procesar datos de precios SIPSA del DANE en Colombia, desde el año 2013 hasta 2023.}
\usage{
DatosCol(
  Mes,
  Año,
  Ciudad,
  Percentil_Abast = NULL,
  Ingreso_Alimentos = NULL,
  data_list_precios = NULL,
  data_list_abas = NULL,
  Margenes = NULL
)
}
\description{
La función Datos_dane_col ofrece la posibilidad de obtener y manejar datos de precios mayoristas SIPSA del Departamento Administrativo Nacional de Estadística (DANE) en Colombia, adecuados para un mes, año y ciudad particulares. Su resultado principal se centra en las estimaciones de los precios minoristas, vinculando los nutrientes correspondientes a cada alimento.}
\examples{

# Ejemplo de uso:

# Obtener datos para Medellín, enero de 2017
DatosCol(Mes = "Enero", Año = 2017, Ciudad = "Medellín")

# Salidas en el ambiente global

kable(head(PRECIOS_SIPSA_Enero_2017))
kable(head(Datos_Insumo_Modelos_2017_Enero_Medellín))

}





\details{
La función realiza una validación exhaustiva de los parámetros ingresados para asegurar su coherencia y luego procesa los datos correspondientes del DANE para estimar los precios minoristas junto con la composición de cada alimento.
}

\value{
Las salidas se mostrarán en el ambiente global de R, su estructura es:

    \item{PRECIOS_SIPSA_Mes_Año:}{Datos de los precios mayoristas de SIPSA descargados desde el DANE en el año y més dado}
    
    \item{Datos_Insumo_Modelos_Año_Mes_Ciudad:}{Data frame de la estimación de los precios minorístas junto con la composición nutricional de cada alimento}

}

\note{
Es importante revisar la documentación para asegurar que los parámetros proporcionados sean correctos y coherentes con los requisitos establecidos.
}
