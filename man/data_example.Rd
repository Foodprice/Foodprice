
\name{data_example}
\docType{data}
\alias{data_example}
\title{(Example dataset)-data_example }

\description{
Dataset obtained through a simulation based on information gathered in September 2022 in the city of Cali, Colombia. This simulation was conducted using marginal copula functions gamma, ensuring the distribution shape and the dependence between price, exchanges, and nutrients columns. The foods, food groups, and subgroups contain arbitrary names. The dataset comprises 96 foods (rows) and 24 variables (columns) corresponding to the prices, exchanges, groups, subgroups, and nutritional contributions of each food.

----

Conjunto de datos obtenido mediante una simulación basada en información recopilada en septiembre de 2022 en la ciudad de Cali, Colombia. Esta simulación se realizó utilizando funciones de cópula marginal gamma, asegurando la forma de distribución y la dependencia entre las columnas de precios, intercambios y nutrientes. Los alimentos, grupos de alimentos y subgrupos contienen nombres arbitrarios. El conjunto de datos comprende 96 alimentos (filas) y 24 variables (columnas) correspondientes a los precios, intercambios, grupos, subgrupos y contribuciones nutricionales de cada alimento.

}



\usage{
data_example
}

\format{

Dataset with 99 foods (rows) and 24 variables (columns). The first columns provide information about the food, including, exchanges, price per exchange, and the energy and nutrient contents for each

----

Conjunto de datos con 99 alimentos (filas) y 24 variables (columnas). Las columnas proporcionan información sobre el alimento, incluyendo intercambios, precio por intercambio y el contenido energético y de nutrientes para cada uno.

}

\examples{

library(Foodprice)
head(data_example)

}

