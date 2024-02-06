
\name{serv}
\alias{serv}
\title{serv (Example dataset)}

\description{
Dataset that contains the exchange requirements for each food subgroup, demographic group, and gender. The determination of the set of foods that satisfy, at the lowest cost, the recommendations of groups specified in the Food-based dietary guidelines for the Colombian population (Guías Alimentarias Basadas en Alimentos para la población colombiana [GABAS]) is based on the number of exchanges needed, adjusted according to caloric intake (differentiated by food group) and the EER (differentiated by age group)}

\usage{
serv
}

\format{
Dataset with 220 rows (combinations of food subgroups and demographic groups) and 4 variables: "Age" (Age, demographic group), "Serving" (Portion, exchanges), "Subgroup" (Food subgroup), and "Sex" (Sex).
}

\examples{

library(Foodprice)
head(serv)

}

\source{


}