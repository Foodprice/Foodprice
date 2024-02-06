
\name{CoRD}
\alias{CoRD}
\title{cost_day of Recommended Diet (CoRD)}
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
}
\examples{

# Example:

# Retrieve input data
data(Datos_Prueba, package = "Foodprice")

data(serv, package = "Foodprice") 
data(diverse, package = "Foodprice") 


# Apply the function to the input data
Mod=CoRD(data = Datos_Prueba,diverse = diverse,serv = serv)

# cost_day Visualization
head(Mod$Costo_CoRD)
}


\arguments{

 \code{- Required Parameters} 

\item{data:}{Data frame containing at least 4 columns with the following names: “Food”, “Group”, “Serving_g”, and “Price_serving”. The “Serving_g” and “Price_serving” columns display the weight in grams per serving and the price per edible serving for each food item. The “Group” column indicates the food group to which the food item belongs (≥ 5 groups). }

\item{serv:}{ Data frame having at least the following columns: “Age”, “Sex”, “Serving”, and “Food_group”. The data frame contains, for different age and sex groups, the serving recommendations (Serving) from each food group (Food_group). }


\item{diverse:}{Data frame containing the number of food-items selected from each food group. The data frame must include at least 2 columns with specific names (“Group” and “Number”) }

 \code{- Optional Parameters} 

\item{exclude:}{A vector used for excluding one or more food items before performing the model.}
}


\details{

The CoRD is estimated from the set of food items that satisfy, at the lowest cost, the serving recommendations by food group specified in the Food-base dietary guidelines (FBDGs) of a country. The CoRD guarantees the diversity among and within groups in the sense that a specific number of least-cost food items are chosen from each food group. The quantity of each food item in the recommended diet is determined by two conditions: (1) two food items selected from the same food group are included in the same quantity; (2) each food group must satisy the serving recommendations from the FBDGs. (See FoodPrice: An R Package for estimating least-cost diets for further information).
Note that the CoRD can be estimated using not only food groups but also food subgroups. For this purpose, a column called “Subgroups” must be added to each of the above-mentioned data frames (data, serve, and diverse). Additionally, the cost per 1000 kcal for the recommended diet can be estimated by adding a column called “Energy” (to the data set in “Data”) that displays the energy content of each food item. 
For the EER parameter, it is not mandatory that the “Sex” column be defined by two categories (0 if male, 1 if female). If only one of the two categories is included, the function computes the CoRD by age groups. In this case specifying the “Sex” column is not needed.


}
\value{

Las salida se mostrarán como una lista con base en el nombre asignado por el usuario, sus elementos son:

    \item{Costo_CORD:}{A data frame containing, for each demographic group, the cost per day (cost_day) for the recommended diet.  (If the “Energy” column were included in “Data”, the cost per 1000 kcal (cost_kcal) will also be displayed).}
    
    \item{Intercambios_CoRD:}{A data frame containing the food items (differentiated by food group) included in the recommended diet, as well as its number of servings.         }

    \item{Precio:}{Numeric vector of the retail price per edible serving of each food item, as entered.}

    \item{Food:}{A vector of food items, as entered}
    
    \item{Energy:}{Numeric vector containing the energy content of each food item, as entered in “Energy” column.}



}

\note{
It is important to review the documentation to ensure that the provided parameters are correct and consistent with the established requirements.
}
