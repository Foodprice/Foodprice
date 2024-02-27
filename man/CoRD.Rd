\name{CoRD}
\docType{package}
\alias{CoRD}
\title{cost day of Recommended Diet (CoRD)}
\usage{

CoRD(
data,
serv,
diverse,
exclude=NULL
)

}

\description{
CoRD computes the minimum daily cost of a recommended diet or a healthy diet that guarantees among and within groups diversity on the basis of food group recommendations provided by Food-base dietary guidelines (FBDGs).

----

La función CoRD calcula el costo diario mínimo de una dieta recomendada o saludable que garantiza diversidad entre los grupos de alimentos, siguiendo las recomendaciones de grupos de alimentos proporcionadas por las Guías Alimentarias Basadas en Alimentos (GABA, por sus siglas en español).
}

\examples{

# Example:
# Ejemplo

# Retrieve input data
# Cargando datos de ejemplo
data(data_example, package = "Foodprice")
data(serv_example, package = "Foodprice") 
data(diverse_example, package = "Foodprice") 


# Apply the function to the input data
# Uso del modelo
Mod=CoRD(data = data_example,diverse = diverse_example,serv = serv_example)

# cost_day Visualization
# visualización del costo
head(Mod$cost)
}


\arguments{

 \code{- Required Parameters} 

\item{data:}{Data frame containing at least 4 columns with the following names: “Food”, “Group”, “Serving_g”, and “Price_serving”. The “Serving_g” and “Price_serving” columns display the weight in grams per serving and the price per edible serving for each food item. The “Group” column indicates the food group to which the food item belongs (at least 5 food groups are required). }

\item{serv:}{ Data frame having at least the following columns: “Age”, “Sex”, “Serving”, and “Food_group”. The data frame contains, for different age and sex groups, the serving recommendations (Serving) from each food group (Food_group). }


\item{diverse:}{Data frame containing the number of food-items selected from each food group. The data frame must include at least 2 columns with specific names (“Group” and “Number”) }

 \code{- Optional Parameters} 

\item{exclude:}{A vector used for excluding one or more food items before performing the model.}

----


\code{- Parámetros necesarios} 

\item{data:}{Conjunto de datos que contiene al menos 4 columnas con los siguientes nombres: “Food”, “Group”, “Serving_g”, y “Price_serving”. Las columnas “Serving_g” y “Price_serving” muestran el peso en gramos por porción y el precio por porción comestible para cada alimento. La columna “Group” indica el grupo de alimentos al que pertenece el alimento (Se requieres al menos 5 grupos de alimentos). }

\item{serv:}{Conjunto de datos que contiene al menos las siguientes columnas: “Age”, “Sex”, “Serving”, y “Food_group”. El conjunto de datos contiene, para diferentes grupos de edad y sexo, las recomendaciones de porciones (Serving) de cada grupo de alimentos (Group). }

\item{diverse:}{Conjunto de datos que contiene el número de alimentos seleccionados de cada grupo de alimentos. El conjunto de datos debe incluir al menos 2 columnas con nombres específicos (“Group” y “Number”) }

 \code{- Parámetros Opcionales} 

\item{exclude:}{Un vector utilizado para excluir uno o más alimentos antes de realizar el modelo.}

}

\details{

The CoRD is estimated from the set of food items that satisfy, at the lowest cost, the serving recommendations by food group specified in the Food-base dietary guidelines (FBDGs) of a country. The CoRD guarantees the diversity among and within groups in the sense that a specific number of least-cost food items are chosen from each food group. The quantity of each food item in the recommended diet is determined by two conditions: (1) two food items selected from the same food group are included in the same quantity; (2) each food group must satisy the serving recommendations from the FBDGs. (See FoodPrice: An R Package for estimating least-cost diets for further information).
Note that the CoRD can be estimated using not only food groups but also food subgroups. For this purpose, a column called “Subgroups” must be added to each of the above-mentioned data frames (data, serve, and diverse). Additionally, the cost per 1000 kcal for the recommended diet can be estimated by adding a column called “Energy” (to the data set in “Data”) that displays the energy content of each food item. 
For the EER parameter, it is not mandatory that the “Sex” column be defined by two categories (0 if male, 1 if female). If only one of the two categories is included, the function computes the CoRD by age groups. In this case specifying the “Sex” column is not needed.

----

El CoRD se estima a partir del conjunto de alimentos que satisfacen, al costo más bajo, las recomendaciones de porciones por grupo de alimentos especificadas en las Guías Alimentarias Basadas en Alimentos (GABA) de un país. El CoRD garantiza la diversidad entre y dentro de los grupos en el sentido de que se eligen un número específico de alimentos de menor costo de cada grupo de alimentos. La cantidad de cada alimento en la dieta recomendada se determina por dos condiciones: (1) dos alimentos seleccionados del mismo grupo de alimentos se incluyen en la misma cantidad; (2) cada grupo de alimentos debe satisfacer las recomendaciones de porciones de las GABA. (Consulte FoodPrice: Un paquete de R para estimar dietas de menor costo para obtener más información).
Tenga en cuenta que el CoRD se puede estimar utilizando no solo grupos de alimentos sino también subgrupos de alimentos. Para este fin, se debe agregar una columna llamada “Subgroups” a cada uno de los marcos de datos mencionados anteriormente (data, serv, y diverse). Además, el costo por 1000 kcal para la dieta recomendada se puede estimar agregando una columna llamada “Energy” (al conjunto de datos en el parámetro  'data') que muestre el contenido energético de cada alimento. 
Para el parámetro 'serv' no es obligatorio que la columna "Sex" esté definida. Si solo se cuenta con una de las dos categorías (por ejemplo, si solo se tienen datos de hombres o solo de mujeres), la función calculará el CoRD por grupos de edad en lugar de tener en cuenta el sexo. En este caso, no es necesario especificar la columna "Sex".


}


\value{

The output will be displayed as a list based on the name assigned by the user, its elements are:

    \item{cost:}{A data frame containing, for each demographic group, the cost per day (cost_day) for the recommended diet.  (If the “Energy” column were included in “Data”, the cost per 1000 kcal (cost_kcal) will also be displayed).}
    
    \item{serv:}{A data frame containing the food items (differentiated by food group) included in the recommended diet, as well as its number of servings.         }

    \item{comp:}{Data frame with exchange numbers by gender, food, and group..}

    \item{p:}{Numeric vector of the retail price per edible serving of each food item, as entered.}

    \item{x:}{A vector of food items, as entered}
    
    \item{Energy:}{Numeric vector containing the energy content of each food item, as entered in “Energy” column.}

----

Las salida se mostrarán como una lista con base en el nombre asignado por el usuario, sus elementos son:

    \item{cost:}{Un conjunto de datos que contiene, para cada grupo demográfico, el costo por día (cost_day) para la dieta recomendada.  (Si la columna “Energy” se incluye en 'data', también se mostrará el costo por 1000 kcal (cost_kcal)).}
    
    \item{serv:}{Un conjunto de datos que contiene los alimentos (diferenciados por grupo de alimentos) incluidos en la dieta recomendada, así como su número de porciones.}

    \item{comp:}{Un conjunto de datos con números de intercambio por sexo (si lo hay), alimento, y grupo.}

    \item{p:}{Vector numérico del precio al por menor por porción comestible de cada alimento, como se ingresó.}

    \item{x:}{Un vector de alimentos, como se ingresó}
    
    \item{Energy:}{Vector numérico que contiene el contenido energético de cada alimento, tal como se ingresó en la columna “Energy”.}

}

\note{
It is important to review the documentation to ensure that the provided parameters are correct and consistent with the established requirements.

----
Es importante revisar la documentación para asegurarse de que los parámetros proporcionados sean correctos y consistentes con los requisitos estable

}
