
\name{Dri_min}
\alias{Dri_min}
\title{Dri_min (Example dataset)}

\description{

Dataset with energy requirements and minimum requirements for various nutrients in different demographic groups and genders. To establish the minimum and maximum macronutrient requirements by age group, the acceptable macronutrient distribution range (AMDR) was used as a reference value. For micronutrients, only the estimated average requirement (EAR) was utilized, meeting the nutritional needs of half of all healthy individuals. The tolerable upper intake level (UL) was not considered for nutrients without an established UL, such as riboflavin, thiamine, and vitamin B12. For sodium, adequate intake (AI) was considered the minimum reference value, and the UL was considered the maximum reference value.}

\usage{
Dri_min
}

\format{
Dataset with 22 demographic groups (rows) and 19 variables (columns). The columns "Sex" (Sex) and "Age" (Age) correspond to the genders and demographic groups, respectively; the remaining columns represent the minimum requirements associated with various nutrients.
}

\examples{

library(Foodprice)
head(Dri_min)

}

\source{


}