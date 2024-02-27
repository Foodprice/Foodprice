\name{EER}
\docType{data}
\alias{EER}
\title{(Dataset of requirements for Colombia)-EER}

\description{
Dataset of national energy requirements by sex and demographic groups. EER was calculated based on information on weight, height, and predominant physical activity for each age group and sex in the region. For the population under 65 years of age, the latter information was extracted from the National Survey of Nutritional Situation (Encuesta Nacional de Situación Nutricional [ENSIN] of 2015);
for the population 65 years of age or older, this information was obtained from the Health, Well-being and Ageing Study (Estudio de Salud, Bienestar y Envejecimiento [SABE]). For the child population, the EER was estimated according to Resolution 3803 of 2016. The EER of adults was estimated by means of the equations described in the Dietary Reference Intakes (IDRs) of the Institute of Medicine. In the case of pregnant and lactating women, the additional caloric intakes for the third trimester of pregnancy and the first six months (postpartum) of exclusive breastfeeding were added to the requirement for a nonpregnant or lactating individual in the same age group. Finally, the weighted average of the energy requirements was computed based on the projections of the National Population and Housing Census to aggregate the age groups. The weights were defined according to the population structure by sex and age.

----

Conjunto de datos de requerimientos energéticos (EER) nacionales por sexo y grupos demográficos. Los EER se calcularon en función de la información sobre peso, altura y actividad física predominante para cada grupo de edad y sexo en la región. Para la población menor de 65 años, esta información se extrajo de la Encuesta Nacional de Situación Nutricional (ENSIN) de 2015; para la población de 65 años o más, esta información se obtuvo del Estudio de Salud, Bienestar y Envejecimiento (SABE). Para la población infantil, los EER se estimaron según la Resolución 3803 de 2016. Los EER de los adultos se estimaron mediante las ecuaciones descritas en las Ingestas Dietéticas de Referencia (IDR) del Instituto de Medicina. En el caso de las mujeres embarazadas y lactantes, las ingesta calóricas adicionales para el tercer trimestre del embarazo y los primeros seis meses (posparto) de lactancia exclusiva se sumaron al requisito para un individuo no embarazado o lactante en el mismo grupo de edad. Finalmente, se calculó el promedio ponderado de los requerimientos energéticos en función de las proyecciones del Censo Nacional de Población y Vivienda para agregar los grupos de edad. Los pesos se definieron según la estructura de la población por sexo y edad.

}

\usage{
EER
}

\format{
Dataset with 22 demographic groups (rows) and 3 variables (columns), namely, "Sex" (0 if male and 1 if female), "Age" (Demographic group), and "Energy" (energy requirement for each gender and demographic group).

----

Conjunto de datos con 22 grupos demográficos (filas) y 3 variables (columnas), a saber, "Sex" (0 si es masculino y 1 si es femenino), "Age" (grupo demográfico) y "Energy" (requerimiento energético para cada sexo y grupo demográfico).
}

\examples{

library(Foodprice)
head(EER)

}

