
\name{data_example}
\alias{data_example}
\title{data_example (Example dataset)}

\description{
Dataset obtained through a simulation based on information gathered in September 2022 in the city of Cali, Colombia. This simulation was conducted using marginal copula functions gamma, ensuring the distribution shape and the dependence between price, exchanges, and nutrients columns. The foods, food groups, and subgroups contain arbitrary names. The dataset comprises 96 foods (rows) and 24 variables (columns) corresponding to the prices, exchanges, groups, subgroups, and nutritional contributions of each food.}

\usage{
data_example
}

\format{

Dataset with 99 foods (rows) and 24 variables (columns). The first columns provide information about the food, including, exchanges, price per exchange, and the energy and nutrient contents for each

}

\examples{

library(Foodprice)
head(data_example)

}

\source{


}