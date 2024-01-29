
\name{Dri_max}
\alias{Dri_max}
\title{Dri_max (Example dataset)}

\description{

Dataset with energy requirements and maximum requirements for various nutrients in different demographic groups and genders. To establish both minimum and maximum macronutrient requirements by age group, the acceptable macronutrient distribution range (AMDR) was used as a reference value. For micronutrients, the lower limit corresponded to the estimated average requirement (EAR), meeting the nutritional needs of half of all healthy individuals. The upper limit was represented by the tolerable upper intake level (UL), indicating the highest intake level needed to avoid the risk of adverse effects on the health of the majority of the population. For sodium, the adequate intake (AI) was considered the minimum reference value, and the UL was considered the maximum reference value. Furthermore, for nutrients that do not register a UL—namely, riboflavin, thiamine, and vitamin B12—both the EAR and UL were considered.
\usage{
Dri_min
}

\format{
Dataset with 22 demographic groups (rows) and 19 variables (columns). The columns "Sexo" (Gender) and "Edad" (Age) correspond to the genders and demographic groups, respectively; the remaining columns represent the maximum requirements associated with various nutrients.
}

\examples{

library(Foodprice)
head(Dri_max)

}

\source{


}