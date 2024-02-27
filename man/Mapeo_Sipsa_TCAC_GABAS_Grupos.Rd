
\name{Mapeo_Sipsa_TCAC_GABAS_Grupos}
\docType{data}
\alias{Mapeo_Sipsa_TCAC_GABAS_Grupos}
\title{(dataset for DataCol function)-Mapeo_Sipsa_TCAC_GABAS_Grupos}

\description{
Dataset that maps foods with their energy contributions and nutrients.

----

Conjunto de datos que mapea los alimentos con sus contribuciones energéticas y nutricinales.
}

\usage{
Mapeo_Sipsa_TCAC_GABAS_Grupos
}

\format{
Dataset of 779 foods (rows) and 41 variables (columns). The variables refer to the food along with its group and energy and nutrient contributions. The dataframe is necessary for mapping the nutrient and energy contents of Colombia, which will serve as input to the CoCA and CoRD models.

----

Conjunto de datos de 779 alimentos (filas) y 41 variables (columnas). Las variables hacen referencia al alimento junto con su grupo y las contribuciones energéticas y nutricionales. El conjunto de datos es necesario para mapear los contenidos nutricionales y energeéticos de Colombia, que servirán como entrada a los modelos CoCA y CoRD.
}

\examples{

library(Foodprice)
head(Mapeo_Sipsa_TCAC_GABAS_Grupos)

}

