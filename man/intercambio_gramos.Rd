
\name{intercambio_gramos}
\docType{data}
\alias{intercambio_gramos}
\title{(dataset for DataCol function)-intercambio_gramos}

\description{
Dataset that establishes the corresponding exchange for each food. Necessary for mapping outgoing Colombian foods and their exchanges for the CoRD model.

----

Conjunto de datos que establece el intercambio correspondiente para cada alimento. Necesario para mapear los alimentos colombianos salientes de la función 'DataCol' y sus intercambios para el modelo CoRD.

}

\usage{
intercambio_gramos
}

\format{

Dataset of 178 foods (rows) and 6 variables (columns). The columns are: "Cod_TCAC" (Food code according to Colombian Food Composition Table (TCAC)), "Alimentos" (Foods), "Subgrupo_GABAS" (Subgroups of each food according toDietary Guidelines for the Colombian Population Aged 2 Years and Older (GABAS)), "Energia_100g" (Energy contribution of the food per 100 grams), "Energia_Int" (Exchange equivalence), and "Intercambio_g" (Food exchange in grams).

----

Conjunto de datos de 178 alimentos (filas) y 6 variables (columnas). Las columnas son: "Cod_TCAC" (Código del alimento según la Tabla Colombiana de Composición de Alimentos (TCAC)), "Alimentos" , "Subgrupo_GABAS" (Subgrupos de cada alimento según las Guías Alimentarias para la Población Colombiana Mayor de 2 Años (GABAS)), "Energia_100g" (Contribución energética del alimento por cada 100 gramos), "Energia_Int" (Equivalencia de intercambio) e "Intercambio_g" (Intercambio de alimentos en gramos).

}

\examples{

library(Foodprice)
head(intercambio_gramos)

}

\source{

- TCAC: Instituto Colombiano de Bienestar Familiar. (2018).Tabla de Composición de Alimentos Colombianos. Recovered from https://www.icbf.gov.co/system/files/tcac_web.pdf

- GABAS: Instituto Colombiano de Bienestar Familiar. (2020). Guías Alimentarias Basadas en Alimentos para la población colombiana mayor de 2 años. En ICBF/FAO (2da Edic.). Recovered from https://www.minsalud.gov.co/sites/rid/Lists/BibliotecaDigital/RIDE/VS/PP/SNA/guiasalimentariasbasadas-en-alimentos.pdf

}

