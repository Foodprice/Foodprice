
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
La función DatosCol ofrece la posibilidad de obtener y manejar datos de precios mayoristas SIPSA del Departamento Administrativo Nacional de Estadística (DANE) en Colombia, adecuados para un mes, año y ciudad particulares. Su resultado principal se centra en las estimaciones de los precios minoristas, vinculando los nutrientes correspondientes a cada alimento.}
\examples{

# Ejemplo de uso:

# Obtener datos para Medellín, enero de 2017
DatosCol(Mes = "Enero", Año = 2017, Ciudad = "Medellín")

# Salidas en el ambiente global

kable(head(PRECIOS_SIPSA_Enero_2017))
kable(head(Datos_Insumo_Modelos_2017_Enero_Medellín))

}

\arguments{

 \code{- PARÁMETROS NECESARIOS} 

    \item{Mes:}{Un texto representando el mes del año (por ejemplo, "Enero").}
    \item{Año:}{Un valor numérico entre 2014 y 2023, indicando el año de interés.}
    \item{Ciudad:}{Un texto que especifica la ciudad de interés en Colombia.}

 \code{- PARÁMETROS OPCIONALES} 

    \item{Percentil_Abast:}{Un número dentro del rango de 0 a 1 que señala el percentil utilizado para filtrar los alimentos con un nivel de abastecimiento inferior a este valor. Por omisión, se establece como NULL.}
    \item{Ingreso_Alimentos:}{Un vector o data frame con 21 columnas, que puede tener un tamaño de 21 si se trata de un vector. Este conjunto de datos actúa como un complemento para los alimentos faltantes en términos de su contenido nutricional. Inicialmente, los análisis nutricionales se llevaron a cabo exclusivamente para la ciudad de Cali, Colombia, lo que resulta en la ausencia de información sobre la composición nutricional de varios alimentos en otras ciudades. La función identifica y muestra los alimentos faltantes en la ciudad proporcionada debido a esta carencia de datos nutricionales específicos para cada ubicación.}
    \item{data_list_precios:}{Una lista que incorpora información de precios. Para su correcto funcionamiento, se espera que el paquete maneje datos descargados directamente desde el DANE, evitando así cualquier procedimiento previo de depuración de datos. Se supone que estos datos se encuentran en un archivo Excel con múltiples hojas, por lo que deben ser cargados como una lista utilizando la librería RIO. Por ejemplo, se puede utilizar la función rio::import_list(Archivo_excel, setclass = "tbl").}
    \item{data_list_abas:}{Una colección de datos vinculados al abastecimiento. Se espera que el proceso de carga para estos datos siga las mismas pautas que se establecen para la carga de la lista de precios, es decir, aplicando las mismas condiciones y procedimientos de carga que se emplean para data_list_precios.}
    \item{Margenes:}{Un conjunto de ocho valores que representan los márgenes de comercialización asociados a cada categoría de alimentos. Estos valores deben seguir un orden específico correspondiente a las siguientes categorías: carnes, frutas, granos y cereales, lácteos y huevos, pescados, productos procesados, tubérculos, raíces y plátanos, verduras y hortalizas.}

}



\details{
La función realiza una validación exhaustiva de los parámetros ingresados para asegurar su coherencia y luego procesa los datos correspondientes del DANE para estimar los precios minoristas junto con la composición de cada alimento.
}

\value{
Las salidas se mostrarán en el ambiente global de R, su estructura es:

    \item{PRECIOS_SIPSA_Mes_Año:}{Datos de los precios mayoristas de SIPSA descargados desde el DANE en el año y més dado}
        \item{SASDA}{xdxd}
        \item{SASDA}{xdxd} 
    \item{Datos_Insumo_Modelos_Año_Mes_Ciudad:}{Data frame de la estimación de los precios minorístas junto con la composición nutricional de cada alimento}

}

\note{
Es importante revisar la documentación para asegurar que los parámetros proporcionados sean correctos y coherentes con los requisitos establecidos.
}

