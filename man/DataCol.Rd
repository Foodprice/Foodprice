\name{DataCol}
\alias{DataCol}
\title{Function to process SIPSA wholesale price data from DANE in Colombia, from 2013 to 2023.}
\usage{
DataCol(
  Month,
  Year,
  City,
  Percentile = NULL,
  Food_income = NULL,
  Price_data_list = NULL,
  Supply_data_list = NULL,
  Margins = NULL
)
}
\description{
The DataCol function provides the possibility to obtain and handle SIPSA wholesale price data from the National Administrative Department of Statistics (DANE) in Colombia, suitable for a specific month, year, and city. Its main result focuses on the estimates of retail prices, linking the corresponding nutrients for each food.

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

La función DataCol proporciona la posibilidad de obtener y manipular datos de precios al por mayor SIPSA del Departamento Administrativo Nacional de Estadística (DANE) en Colombia, adecuados para un mes, año y ciudad específicos. Su resultado principal se centra en las estimaciones de precios al por menor, vinculando los nutrientes correspondientes para cada alimento.

}
\examples{

# Example of usage:
# Ejemplo de uso

# Obtain data for Medellín, January 2017
# Obteniendo datos para medellín, Enero del 2017

X=DataCol(Month = 1, Year = 2017, City = "Medellín")

# Outputs in the global environment
# Salida en el ambiente global de R
kable(head(X))


}

\arguments{

 \code{- REQUIRED PARAMETERS} 

    \item{Month:}{A numerical value representing the month of the year (Range from 1 to 12).}
    \item{Year:}{A numerical value between 2013 and 2023, indicating the year of interest.}
    \item{City:}{A text specifying the city of interest in Colombia.}

 \code{- OPTIONAL PARAMETERS} 

    \item{Percentile:}{A number within the range of 0 to 1 indicating the percentile used to filter foods with a supply level below this value. Defaults to NULL.}
    \item{Food_income:}{A vector or data frame with 25 columns, which can have a size of 25 if it is a vector. This dataset acts as a supplement for missing nutritional information for certain foods. Initially, nutritional analyses were conducted exclusively for the city of Cali, Colombia, resulting in a lack of information on the nutritional composition of various foods in other cities. The function identifies and displays the missing foods in the provided city due to this lack of specific nutritional data for each location.}
    \item{Price_data_list:}{A list incorporating price information. For proper functioning, the package expects data downloaded directly from DANE, avoiding any prior data cleaning procedure. It is assumed that this data is in an Excel file with multiple sheets, so it should be loaded as a list using the RIO library. For example, the rio::import_list(Excel_file, setclass = "tbl") function can be used.}
    \item{Supply_data_list:}{A collection of data linked to supply. The loading process for this data is expected to follow the same guidelines as those established for loading the price list, i.e., applying the same loading conditions and procedures used for data_list_prices.}
    \item{Margins:}{A set of eight values representing the marketing margins associated with each food category. These values must follow a specific order corresponding to the following categories: meats, fruits, grains and cereals, dairy and eggs, fish, processed products, tubers, roots and bananas, vegetables.}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

\code{- PARÁMETROS NECESARIOS} 

    \item{Month:}{Un valor numérico que representa el mes del año (rango de 1 a 12).}
    \item{Year:}{Un valor numérico entre 2014 y 2023, indicando el año de interés.}
    \item{City:}{Un texto que especifica la ciudad de interés en Colombia.}

 \code{- PARÁMETROS OPCIONALES} 

    \item{Percentile:}{Un número dentro del rango de 0 a 1 que indica el percentil utilizado para filtrar alimentos con un nivel de suministro por debajo de este valor. Por defecto, es NULL.}
    \item{Food_income:}{Un vector o marco de datos con 21 columnas, que puede tener un tamaño de 21 si es un vector. Este conjunto de datos actúa como un suplemento para la información nutricional faltante para ciertos alimentos. Inicialmente, se realizaron análisis nutricionales exclusivamente para la ciudad de Cali, Colombia, lo que resultó en una falta de información sobre la composición nutricional de varios alimentos en otras ciudades. La función identifica y muestra los alimentos faltantes en la ciudad proporcionada debido a esta falta de datos nutricionales específicos para cada ubicación.}
    \item{Price_data_list:}{Una lista que incorpora información de precios. Para que funcione correctamente, el paquete espera datos descargados directamente del DANE, evitando cualquier procedimiento previo de limpieza de datos. Se asume que estos datos están en un archivo de Excel con varias hojas, por lo que deben cargarse como una lista utilizando la biblioteca RIO. Por ejemplo, se puede usar la función rio::import_list(Excel_file, setclass = "tbl").}
    \item{Supply_data_list:}{Una colección de datos vinculados al suministro. Se espera que el proceso de carga de estos datos siga las mismas pautas que las establecidas para cargar la lista de precios, es decir, aplicando las mismas condiciones y procedimientos de carga utilizados para data_list_prices.}
    \item{Margins:}{Un conjunto de ocho valores que representan los márgenes de comercialización asociados con cada categoría de alimentos. Estos valores deben seguir un orden específico correspondiente a las siguientes categorías: carnes, frutas, granos y cereales, lácteos y huevos, pescado, productos procesados, tubérculos, raíces y plátanos, verduras.}

}



\details{
The function performs a thorough validation of the entered parameters to ensure their coherence and then processes the corresponding DANE data to estimate retail prices along with the composition of each food.

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

La función realiza una validación exhaustiva de los parámetros ingresados para garantizar su coherencia y luego procesa los datos correspondientes del DANE para estimar los precios al por menor junto con la composición de cada alimento.

}

\value{
The function output is a list containing the estimated retail prices and the data frame downloaded from DANE.

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

La salida de la función es una lista que contiene los precios al por menor estimados y el marco de datos descargado del DANE.
}

\note{

It is important to review the documentation to ensure that the provided parameters are correct and consistent with the established requirements.

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Es importante revisar la documentación para asegurarse de que los parámetros proporcionados sean correctos y consistentes con los requisitos establecidos.
}

