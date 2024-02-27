
\name{CoCA}
\docType{package}
\alias{CoCA}
\title{cost day of Caloric Adequacy (CoCA)}
\usage{
CoCA(
data,
EER,
exclude=NULL
)

}
\description{
CoCA computes the minimum daily cost of a caloric adequate diet for an individual according to its estimated energy requirement.

----

La función CoCA calcula el costo diario mínimo de una dieta adecuada en calorías para un individuo, basándose en su requerimiento energético estimado.
}
\examples{

# Example:
# Ejemplo

# Retrieve input data
# Llamando los datos
data(data_example, package = "Foodprice")
data(EER, package = "Foodprice") 

# Applying the model with the sample data
# Aplicando el modelo con los datos de ejemplo
Mod=CoCA(data = data_example, EER=EER_example)

# cost_day Visualization
# visualización del costo estimado
kable(Mod$cost)

}

\arguments{

 \code{- Required Parameters} 

    \item{data:}{Data frame containing the retail price and energy content of local food items, expressed per 100 g edible portion. The data frame must have at least 3 columns with the following names: “Food”, “Price_100g”, and “Energy”.      } 
    \item{EER:}{Data frame containing the Estimated Energy Requirement (EER) for different age and sex groups. The data frame must include 3 columns with the following names: “Age”, “EER”, and “Sex”. “Age” is assumed to be a character string, and “EER” and “Sex” (0 if male, 1 if female) are numeric variables.      }

 
 \code{- Optional Parameters} 

    \item{exclude:}{A vector used for excluding one or more food items before performing the model.}

----

 \code{- Parámetros necesarios} 
 
    \item{data:}{Un conjunto de datos que contiene el precio al por menor y el contenido energético de los alimentos locales, expresados por cada 100 g de porción comestible. El conjunto de datos debe tener al menos tres columnas con los siguientes nombres: "Food", "Price_100g", y "Energy".} 
    \item{EER:}{Un conjunto de datos que contiene el requerimiento energético Estimado (EER) para diferentes grupos de edad y sexo. El conjunto de datos debe incluir 3 columnas con los siguientes nombres: "Age", "EER", y "Sex". Se asume que "Age" es una cadena de texto, mientras que "EER" y "Sex" son variables numéricas, donde "Sex" toma el valor de 0 si se trata de un hombre y 1 si se trata de una mujer.}
 
 \code{- Parámetros Opcionales} 
    \item{exclude:}{Un vector utilizado para excluir uno o más alimentos antes de realizar el modelo.}

}

\details{
The CoCA is defined as the minimum cost of an energy-only diet that provides the calories necessary to meet the EER for an individual. The optimal solution to the linear programming model consists of a diet composed of the food item whose price per kilocalorie is minimal (i.e. the least-cost starchy staple food). (See FoodPrice: An R Package for estimating least-cost diets for further information).
Note that, for the EER parameter, it is not mandatory that the “Sex” column be defined by two categories (0 if male, 1 if female). If only one of the two categories is included, the function computes the CoCA by age groups. In this case specifying the “Sex” column is not needed. 

----

El CoCA se define como el costo mínimo de una dieta solo de energía que proporciona las calorías necesarias para cumplir con el EER para un individuo. La solución óptima al modelo de programación lineal consiste en una dieta compuesta por el alimento cuyo precio por kilocaloría es mínimo (es decir, el alimento básico de menor costo). (Consulte FoodPrice: Un paquete de R para estimar dietas de menor costo para obtener más información).
Tenga en cuenta que, para el parámetro EER, no es obligatorio que la columna "Sex" esté definida. Si solo se cuenta con una de las dos categorías (por ejemplo, si solo se tienen datos de hombres o solo de mujeres), la función calculará el CoCA por grupos de edad en lugar de tener en cuenta el sexo. En este caso, no es necesario especificar la columna "Sex".

}
\value{
The output will be displayed as a list based on the name assigned by the user; its elements are:


\item{cost:}{A data frame containing the cost and composition of the diet, differentiated by age and sex groups. For each demographic group, the data frame displays the food whose price per kilocalorie is minimal (food), the quantity of food expressed in grams (quantity), the cost per day (cost_day) and the cost per 1000 kcal (cost_kcal) for the energy-only diet. (If the food groups were specified in data parameter, the data frame displays the food group to which the food item belongs).    }


\item{p:}{Numeric vector of food item’s retail price expressed per 100 g edible portion (i.e. the objective function coefficients)}


\item{x:}{Vector of food items used in objective function}

\item{energy:}{A data frame containing the constraints of the linear programming model, as entered (i.e. EER by age and sex groups).}

----

La salida se mostrará como una lista basada en el nombre asignado por el usuario; sus elementos son:

\item{cost:}{Un conjunto de datos que contiene el costo y la composición de la dieta, diferenciados por grupos de edad y sexo. Para cada grupo demográfico, el conjunto de datos muestra el alimento cuyo precio por kilocaloría es mínimo (Food), la cantidad de alimento expresada en gramos (quantity), el costo por día (cost_day) y el costo por 1000 kcal (cost_kcal) para la dieta solo de energía. (Si se especificaron los grupos de alimentos en el parámetro data, el conjunto de datos muestra el grupo de alimentos al que pertenece el alimento).}

\item{p:}{Vector numérico del precio al por menor del alimento expresado por cada 100 g de porción comestible (es decir, los coeficientes de la función objetivo).}

\item{x:}{Vector de alimentos utilizados en la función objetivo}

\item{energy:}{Un conjunto de datos que contiene las restricciones del modelo de programación lineal según se ingresaron (es decir, EER por grupos de edad y sexo).}
}


\note{
It is important to review the documentation to ensure that the provided parameters are correct and consistent with the established requirements.

----

Es importante revisar la documentación para asegurarse de que los parámetros proporcionados sean correctos y consistentes con los requisitos establecidos.
}
