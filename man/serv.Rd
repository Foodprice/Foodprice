
\name{serv}
\docType{data}
\alias{serv}
\title{(Dataset of requirements for Colombia)-serv}

\description{

Dataset containing the exchange requirements for each food subgroup, demographic group, and sex.

----

Conjunto de datos que contiene los requerimientos de intercambio para cada subgrupo de alimentos, grupo demográfico y sexo. 

}

\usage{
serv
}

\format{

Dataset with 220 rows (combinations of food subgroups and demographic groups) and 4 variables: "Age" (Age, demographic group), "Serving" (Serving, exchanges), "subgroup" (Food subgroup), and "Sex" (Sex, gender).

----

Conjunto de datos con 220 filas (combinaciones de subgrupos de alimentos y grupos demográficos) y 4 variables (columnas): "Age" (Edad, grupo demográfico), "Serving" (Porción, intercambios), "subgroup" (Subgrupo de alimentos) y "Sex" (Sexo, género).

}

\examples{

library(Foodprice)
head(serv)

}
