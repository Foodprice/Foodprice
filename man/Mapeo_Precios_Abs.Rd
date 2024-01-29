
\name{Mapeo_Precios_Abs}
\alias{Mapeo_Precios_Abs}
\title{Mapeo_Precios_Abs (dataset for DatosCol function)}

\description{

Dataset used to map foods from the Wholesale Price Information System (SIPSA) and supply foods. It is necessary when using the DatosCol function to filter by percentile.
}

\usage{
Mapeo_Precios_Abs
}

\format{
Dataset of 146 foods (rows) and 2 variables (columns), namely: "Alimento" (Food from SIPSA wholesale prices) and "Alimento_Abs" (Corresponding food in supply).
}

\examples{

library(Foodprice)
head(Mapeo_Precios_Abs)

}


