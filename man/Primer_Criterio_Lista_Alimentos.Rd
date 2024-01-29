
\name{Primer_Criterio_Lista_Alimentos}
\alias{Primer_Criterio_Lista_Alimentos}
\title{Primer_Criterio_Lista_Alimentos (dataset for DatosCol function)}

\description{
Dataset for filtering foods with very low caloric content, condiments consumed in small quantities, herbs, spices, or preparations.
}

\usage{
Primer_Criterio_Lista_Alimentos
}

\format{

Dataset of 185 foods (rows) and 2 variables (columns). The variables are "Alimentos" (Foods) and "Cod_TCAC" (Code according to TCAC). Foods to be excluded have the "Cod_TCAC" equal to "EX000".
}

\examples{

library(Foodprice)
head(Primer_Criterio_Lista_Alimentos)

}


