
\name{serv_example}
\alias{serv_example}
\title{(Example dataset)-serv_example}

\description{
Dataset containing exchange requirements for arbitrary foods and subgroups. The exchange values are identical to those expressed in the 'serv' dataset.

----

Conjunto de datos que incluye los requisitos de intercambio para alimentos y subgrupos arbitrarios. Los valores de intercambio son idénticos a los expresados en el conjunto de datos 'serv'.
}

\usage{
serv_example
}

\format{

Dataset with 220 rows (combinations of food subgroups and demographic groups) and 4 variables: "Age" (Age, demographic group), "Serving" (Serving, exchanges), "subgroup" (Food subgroup), and "Sex" (Sex, gender).

----

Conjunto de datos con 220 filas (combinaciones de subgrupos de alimentos y grupos demográficos) y 4 variables (columnas): "Age" (Edad, grupo demográfico), "Serving" (Porción, intercambios), "subgroup" (Subgrupo de alimentos) y "Sex" (Sexo, género).

}

\examples{

library(Foodprice)
head(serv_example)

}

