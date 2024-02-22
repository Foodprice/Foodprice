
\name{EER}
\alias{EER}
\title{(Dataset of requirements for Colombia)-EER}

\description{
Dataset of national energy requirements by sex and demographic groups. EER was calculated based on information on weight, height, and predominant physical activity for each age group and sex in the region. For the population under 65 years of age, the latter information was extracted from the National Survey of Nutritional Situation (Encuesta Nacional de Situación Nutricional [ENSIN] of 2015);
for the population 65 years of age or older, this information was obtained from the Health, Well-being and Ageing Study (Estudio de Salud, Bienestar y Envejecimiento [SABE]). For the child population, the EER was estimated according to Resolution 3803 of 2016. The EER of adults was estimated by means of the equations described in the Dietary Reference Intakes (IDRs) of the Institute of Medicine. In the case of pregnant and lactating women, the additional caloric intakes for the third trimester of pregnancy and the first six months (postpartum) of exclusive breastfeeding were added to the requirement for a nonpregnant or lactating individual in the same age group. Finally, the weighted average of the energy requirements was computed based on the projections of the National Population and Housing Census to aggregate the age groups. The weights were defined according to the population structure by sex and age.
}

\usage{
EER
}

\format{
Dataset with 22 demographic groups (rows) and 3 variables (columns), namely, "Sex" (0 if male and 1 if female), "Age" (Demographic group), and "Energy" (energy requirement for each gender and demographic group).
}

\examples{

library(Foodprice)
head(EER)

}

\source{


}