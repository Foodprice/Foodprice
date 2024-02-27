
\name{EER_LL}
\docType{data}
\alias{EER_LL}
\title{(Dataset of requirements for Colombia)-EER_LL}

\description{

Dataset containing energy requirements and minimum requirements for various nutrients by demographic groups and gender. To establish the minimum macronutrient requirements per age group, the acceptable macronutrient distribution range (AMDR) was used as a reference value. For micronutrients, only the estimated average requirement (EAR) was utilized, meeting the nutritional needs of half of all healthy individuals. For sodium, adequate intake (AI) was considered the minimum reference value. The energy requirements 'EER' follow the methodology expressed in the dataset of the same name.
----

Conjunto de datos con requerimientos energéticos y requerimientos minimos para varios nutrientes por grupos demográficos y sexo. Para establecer los requisitos mínimos de macronutrientes por grupo de edad, se utilizó el rango de distribución aceptable de macronutrientes (AMDR) como valor de referencia. Para los micronutrientes, solo se utilizó el requerimiento promedio estimado (EAR), que satisface las necesidades nutricionales de la mitad de todos los individuos sanos. Para el sodio, se consideró la ingesta adecuada (AI) como el valor de referencia mínimo. Los requerimientos energéticos 'EER' siguen la metodología expresada en el conjunto de datos del mismo nombre.
}

\usage{
EER_LL
}

\format{

Dataset with 22 demographic groups (rows) and 19 variables (columns). The columns "Sex" (Sex) and "Age" (Age) correspond to the genders and demographic groups, respectively; the remaining columns represent the minimum requirements associated with various nutrients.

----

Conjunto de datos con 22 grupos demográficos (filas) y 19 variables (columnas). Las columnas "Sex" (Sexo) y "Age" (Edad) corresponden a los géneros y grupos demográficos respectivamente; las columnas restantes representan los requisitos mínimos asociados con varios nutrientes.

}

\examples{

library(Foodprice)
head(EER_LL)

}

