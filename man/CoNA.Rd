
\name{CoNA}
\docType{package}
\alias{CoNA}
\title{cost day of Nutrient Adequacy (CoNA)}
\usage{

CoNA(
data,
EER_LL=,
UL,
exclude=NULL

)

}
\description{CoNA computes the minimum daily cost of a nutrient-adequate diet for an individual, according to its dietary reference intakes.

----

LA función CoNA calcula el costo diario mínimo de una dieta adecuada en nutrientes para un individuo según sus ingestas de referencia dietética.
}

\examples{

# Example:
#Ejemplo

# Retrieve input data
# Llamando datos
data(data_example, package = "Foodprice")
data(EER_LL, package = "Foodprice") 
data(UL, package = "Foodprice") 


# Apply the function to the input data
# Uso del modelo
Mod=CoNA(data = data_example,EER_LL=EER_LL_example,UL=UL_example)

# cost_day Visualization
# visualziación del costo
kable(Mod$cost)

}



\arguments{

 \code{- Required Parameters} 

        \item{data:}{Data frame containing the retail price and nutrient composition of local food items, expressed per 100 g edible portion. The data frame must have 3 columns with specific names (“Food”, “Price_100g”, and “Energy”) and K-columns containing food items’ nutrient content for key macro- and micronutrients. }
        \item{EER_LL:}{Data frame containing the Estimated Energy Requirement (EER), as well as the lower limits of macro- and micronutrient intake for different age and sex groups. The data frame must include 3 columns with specific names (“Age”, “Sex”, and “EER”) and K-columns containing the lower limits of the K macro- and micronutrients. (The nutrient names must match the names used in data parameter) } 
       \item{UL:}{Data frame containing the upper limits of macro- and micronutrient intake for different age and sex groups. The data frame must include 2 columns with specific names (“Age”, “Sex”) and K-columns containing the upper limits of the K macro- and micronutrients. (The nutrient names must match the names used in data parameter) } 

 \code{- Optional Parameters} 

        \item{exclude:}{A vector used for excluding one or more food items before performing the model.}

----      

\code{- Parámetros necesarios} 

        \item{data:}{Conjunto de datos que contiene el precio al por menor y la composición de nutrientes de los alimentos locales, expresados por cada 100 g de porción comestible. El conjunto de datos debe tener 3 columnas con nombres específicos (“Food”, “Price_100g”, y “Energy”) y K-columnas conteniendo el contenido de nutrientes de los alimentos para K macro- y micronutrientes clave. }
        \item{EER_LL:}{Conjunto de datos que contiene el Requerimiento Energético Estimado (EER), así como los límites inferiores de la ingesta de macro y micronutrientes para diferentes grupos de edad y sexo. El conjunto de datos debe incluir 3 columnas con nombres específicos (“Age”, “Sex”, y "Energy") y K-columnas conteniendo los límites inferiores de los K macro- y micronutrientes. (Los nombres de los nutrientes deben coincidir con los nombres utilizados en el parámetro data) } 
       \item{UL:}{Conjunto de datos que contiene los límites superiores de la ingesta de macro y micronutrientes para diferentes grupos de edad y sexo. El conjunto de datos debe incluir 2 columnas con nombres específicos (“Age”, “Sex”) y K-columnas conteniendo los límites superiores de los K macro- y micronutrientes. (Los nombres de los nutrientes deben coincidir con los nombres utilizados en el parámetro data y EER_LL) } 

\code{- Parámetros Opcionales} 

        \item{exclude:}{Un vector utilizado para excluir uno o más alimentos antes de realizar el modelo.}

}



\details{
The CoNA is estimated by selecting the set of food items that, at the lowest cost, meets not only the Estimated Energy Requirement (EER), but also the lower and upper limits of macro- and micronutrient intake for an individual. The minimum cost of the nutrient-adequate diet is given by the solution of a linear programming model with 2K + 1 linear constraints. (See FoodPrice: An R Package for estimating least-cost diets for further information).
On the basis of the linear programming model, it is possible to identify limiting nutrients, as well as compute shadow price elasticity (SPE) of each nutrient. By definition, a nutrient is identified as a limiting nutrient if the intake level in optimal diet reaches exactly the lower limit. For the k-th nutrient, the SPE is defined as the percentage change in the objective function evaluated at the optimal basis (optimal CoNA) when the constraint is increased by 1%. (For formal details, see FoodPrice: An R Package for estimating least-cost diets).
Note that, to guarantee data integration, the nutrient names must be consistent across all parameters (data, EER_LL, UL). Additionally, for the EER parameter, it is not mandatory that the “Sex” column be defined by two categories (0 if male, 1 if female). If only one of the two categories is included, the function computes the CoNA by age groups. In this case specifying the “Sex” column is not needed.

----

El CoNA se estima seleccionando el conjunto de alimentos que, al costo más bajo, cumple no solo con el Requerimiento Energético Estimado (EER), sino también con los límites inferiores y superiores de la ingesta de macro y micronutrientes para un individuo. El costo mínimo de la dieta adecuada en nutrientes está dado por la solución de un modelo de programación lineal con 2K + 1 restricciones lineales. (Consulte FoodPrice: Un paquete de R para estimar dietas de menor costo para obtener más información).
Sobre la base del modelo de programación lineal, es posible identificar nutrientes limitantes, así como calcular la elasticidad del precio sombra (SPE) de cada nutriente. Por definición, un nutriente se identifica como nutriente limitante si el nivel de ingesta en la dieta óptima alcanza exactamente el límite inferior. Para el k-ésimo nutriente, la SPE se define como el cambio porcentual en la función objetivo evaluada en la base óptima (CoNA óptimo) cuando la restricción se aumenta en un 1%. (Para detalles formales, consulte FoodPrice: Un paquete de R para estimar dietas de menor costo).
Tenga en cuenta que, para garantizar la integración de datos, los nombres de los nutrientes deben ser consistentes en todos los parámetros (data, EER_LL, UL). Además, para los parámetros  EER_LL y UL, no es obligatorio que la columna "Sex" esté definida. Si solo se cuenta con una de las dos categorías (por ejemplo, si solo se tienen datos de hombres o solo de mujeres), la función calculará el CoRD por grupos de edad en lugar de tener en cuenta el sexo. En este caso, no es necesario especificar la columna "Sex".

}

\value{

The output will be displayed as a list based on the name assigned by the user; its elements are:

    \item{comp:}{A data frame containing the composition of the diet, differentiated by age and sex groups. The data frame displays, for each demographic group, the food items included in the optimal solution (food) and its quantity. (If the food groups were specified in data parameter, the data frame displays the food group to which the food item belongs). }
    
    \item{cost:}{A data frame containing, for each demographic group, the cost per day (cost_day) and the cost per 1000 kcal (cost_kcal) for the nutrient-adequate diet}
    
    \item{limit:}{A data frame with a dichotomous variable indicating whether a nutrient was a limiting nutrient. }

    \item{spe:}{A data frame containing the shadow price (SP) and the shadow price elasticity (SPE) of each constraint. The data frame displays a dichotomous variable (constraints) indicating whether the shadow prices are computed based on the lower limit (min) or the upper limit (max).}

    \item{p:}{Numeric vector of food item’s retail price expressed per 100 g edible portion (i.e. the objective function coefficients)}

    \item{x:}{A vector of food items used in objective function}

    \item{constraints:}{A data frame containing the constraints of the linear programming model, as entered (i.e. EER, lower and upper limits of macro- and micronutrient intake by age and sex groups).}

----

La salida se mostrará como una lista basada en el nombre asignado por el usuario; sus elementos son:

    \item{comp:}{Un conjunto de datos que contiene la composición de la dieta, diferenciada por grupos de edad y sexo. El conjunto de datos muestra, para cada grupo demográfico, los alimentos incluidos en la solución óptima (alimento) y su cantidad. (Si se especificaron los grupos de alimentos en el parámetro data, el conjunto de datos muestra el grupo de alimentos al que pertenece el alimento). }
    
    \item{cost:}{Un conjunto de datos que contiene, para cada grupo demográfico, el costo por día (costo_día) y el costo por 1000 kcal (costo_kcal) para la dieta adecuada en nutrientes}
    
    \item{limit:}{Un conjunto de datos con una variable dicotómica que indica si un nutriente fue un nutriente limitante. (1 si es limitante 0 si no)}

    \item{spe:}{Un conjunto de datos que contiene el precio sombra (SP) y la elasticidad del precio sombra (SPE) de cada restricción. El conjunto de datos muestra una variable dicotómica (restricciones) que indica si los precios sombra se calculan en función del límite inferior (min) o el límite superior (max).}

    \item{p:}{Vector numérico del precio al por menor del alimento expresado por cada 100 g de porción comestible (es decir, los coeficientes de la función objetivo)}

    \item{x:}{Un vector de alimentos utilizados en la función objetivo}

    \item{constraints:}{Un conjunto de datos que contiene las restricciones del modelo de programación lineal según se ingresaron (es decir, Energy, límites inferiores y superiores de la ingesta de macro y micronutrientes por grupos de edad y sexo).}

        
}

\note{
It is important to review the documentation to ensure that the provided parameters are correct and consistent with the established requirements.

----

Es importante revisar la documentación para asegurarse de que los parámetros proporcionados sean correctos y consistentes con los requisitos estable
}
