
\name{Mapeo_Sipsa_TCAC_GABAS_Grupos}
\alias{Mapeo_Sipsa_TCAC_GABAS_Grupos}
\title{Mapeo_Sipsa_TCAC_GABAS_Grupos (dataset for DatosCol function)}

\description{
Dataset that maps foods with their energy contributions and nutrients.
}

\usage{
Mapeo_Sipsa_TCAC_GABAS_Grupos
}

\format{
Dataset of 779 foods (rows) and 41 variables (columns). The variables refer to the food along with its group and energy and nutrient contributions. The dataframe is necessary for mapping the nutrient and energy contents of Colombia, which will serve as input to the CoCA and CoRD models.
}

\examples{

library(Foodprice)
head(Mapeo_Sipsa_TCAC_GABAS_Grupos)

}
