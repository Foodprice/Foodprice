
\name{UL}
\alias{UL}
\title{(Dataset of requirements for Colombia.)-UL}

\description{

Dataset with maximum requirements for various nutrients by demographic groups and gender. To establish the maximum macronutrient requirements per age group, the acceptable macronutrient distribution range (AMDR) was used as a reference value. For micronutrients, the upper limit was represented by the tolerable upper intake level (UL), indicating the highest intake level needed to avoid the risk of adverse effects on the health of the majority of the population. For sodium, it was considered as the maximum reference value. Furthermore, for nutrients that do not have a UL, such as riboflavin, thiamine, and vitamin B12, both the EAR and UL were considered.

----

Conjunto de datos con los requerimientos máximos para varios nutrientes por grupos demográficos y sexo. Para establecer los requisitos  máximos de macronutrientes por grupo de edad, se utilizó el rango de distribución aceptable de macronutrientes (AMDR) como valor de referencia. Para los micronutrientes, el límite superior fue representado por el nivel de ingesta superior tolerable (UL), indicando el nivel de ingesta más alto necesario para evitar el riesgo de efectos adversos sobre la salud de la mayoría de la población. Para el sodio, se consideró como el valor de referencia máximo. Además, para los nutrientes que no registran un UL, como la riboflavina, la tiamina y la vitamina B12, se consideraron tanto el EAR como el UL.

}
\usage{
UL
}

\format{

Dataset with 22 demographic groups (rows) and 19 variables (columns). The columns "Sex" (Sex) and "Age" (Age) correspond to the genders and demographic groups, respectively; the remaining columns represent the maximum requirements associated with various nutrients.

----

Conjunto de datos con 22 grupos demográficos (filas) y 19 variables (columnas). Las columnas "Sex" (Sexo) y "Age" (Edad) corresponden a los sexos y grupos demográficos, respectivamente; las columnas restantes representan los requisitos máximos asociados a varios nutrientes.

}

\examples{

library(Foodprice)
head(UL)

}

