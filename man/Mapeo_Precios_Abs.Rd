
\name{Mapeo_Precios_Abs}
\docType{data}
\alias{Mapeo_Precios_Abs}
\title{(dataset for DataCol function)-Mapeo_Precios_Abs}

\description{

Dataset used to map foods from the Wholesale Price Information System (SIPSA) and supply foods. It is necessary when using the DataCol function to filter by percentile.

----

Conjunto de datos utilizado para mapear los alimentos del Sistema de Información de Precios al por Mayor (SIPSA) y su abastecimiento. Es necesario cuando se utiliza la función DataCol para filtrar por percentil.
}

\usage{
Mapeo_Precios_Abs
}

\format{

Dataset of 146 foods (rows) and 2 variables (columns), namely: "Food" (Food from SIPSA wholesale prices) and "Alimento_Abs" (Corresponding food in supply).

----

Conjunto de datos de 146 alimentos (filas) y 2 variables (columnas), a saber: "Alimento" (Alimento de los precios mayoristas de SIPSA) y "Alimento_Abs" (Alimento correspondiente en el abastecimiento).
}

\examples{

library(Foodprice)
head(Mapeo_Precios_Abs)

}


