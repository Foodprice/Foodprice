
\name{TCAC}
\docType{data}
\alias{TCAC}
\title{(dataset for DataCol function)-TCAC}

\description{
Dataset mapping foods and groups from the Colombian Food Composition Table (TCAC) and the Food-Based Dietary Guidelines for the Colombian Population Aged 2 Years and Older (GABAS).

----

Conjunto de datos que mapea los alimentos y grupos de alimentos de la Tabla de Composición de Alimentos Colombiana (TCAC) y las Guías Alimentarias Basadas en Alimentos para la Población Colombiana Mayor de 2 Años (GABAS).

}

\usage{
TCAC
}

\format{
Dataframe of 779 foods (rows) and 5 variables (columns), namely: "Cod_TCAC" (Code of foods for TCAC), "Food" (Food), "Grupo_GABAS" (GABAS Groups), "Subgrupo_GABAS" (Food Subgroups for GABAS), and "Grupo_TCAC" (TCAC Food Group).

----

Conjunto de datos de 779 alimentos (filas) y 5 variables (columnas), a saber: "Cod_TCAC" (Código de alimentos para TCAC), "Alimento" , "Grupo_GABAS" (Grupos GABAS), "Subgrupo_GABAS" (Subgrupos de alimentos para GABAS) y "Grupo_TCAC" (Grupo de alimentos para TCAC).


}

\examples{

library(Foodprice)
head(TCAC)

}

\source{

- TCAC: Instituto Colombiano de Bienestar Familiar. (2018).Tabla de Composición de Alimentos Colombianos. Recovered from https://www.icbf.gov.co/system/files/tcac_web.pdf

- GABAS: Instituto Colombiano de Bienestar Familiar. (2020). Guías Alimentarias Basadas en Alimentos para la población colombiana mayor de 2 años. En ICBF/FAO (2da Edic.). Recovered from https://www.minsalud.gov.co/sites/rid/Lists/BibliotecaDigital/RIDE/VS/PP/SNA/guiasalimentariasbasadas-en-alimentos.pdf

}