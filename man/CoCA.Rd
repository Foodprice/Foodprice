
\name{CoCA}
\alias{CoCA}
\title{cost_day of Caloric Adequacy (CoCA)}
\usage{
CoCA(
data,
EER,
exclude=NULL
)

}
\description{
CoCA computes the minimum daily cost of a caloric adequate diet for an individual according to its estimated energy requirement.

}
\examples{

# Example:

# Retrieve input data
data(Datos_Prueba, package = "Foodprice")
data(EER, package = "Foodprice") 

# Apply the function to the input data
Mod=CoCA(data = Datos_Prueba, EER=EER)

# cost_day Visualization
kable(Mod$Costo_CoCA)

}

\arguments{

 \code{- Required Parameters} 

    \item{data:}{Data frame containing the retail price and energy content of local food items, expressed per 100 g edible portion. The data frame must have at least 3 columns with the following names: “Food”, “Price_100g”, and “Energy”.      } 
    \item{EER:}{Data frame containing the Estimated Energy Requirement (EER) for different age and sex groups. The data frame must include 3 columns with the following names: “Age”, “EER”, and “Sex”. “Age” is assumed to be a character string, and “EER” and “Sex” (0 if male, 1 if female) are numeric variables.      }

 
 \code{- Optional Parameters} 

    \item{exclude:}{A vector used for excluding one or more food items before performing the model.}
}

\details{
The CoCA is defined as the minimum cost of an energy-only diet that provides the calories necessary to meet the EER for an individual. The optimal solution to the linear programming model consists of a diet composed of the food item whose price per kilocalorie is minimal (i.e. the least-cost starchy staple food). (See FoodPrice: An R Package for estimating least-cost diets for further information).
Note that, for the EER parameter, it is not mandatory that the “Sex” column be defined by two categories (0 if male, 1 if female). If only one of the two categories is included, the function computes the CoCA by age groups. In this case specifying the “Sex” column is not needed. 
}
\value{
The output will be displayed as a list based on the name assigned by the user; its elements are:


\item{Costo_CoCA:}{A data frame containing the cost and composition of the diet, differentiated by age and sex groups. For each demographic group, the data frame displays the food whose price per kilocalorie is minimal (food), the quantity of food expressed in grams (quantity), the cost per day (cost_day) and the cost per 1000 kcal (cost_kcal) for the energy-only diet. (If the food groups were specified in data parameter, the data frame displays the food group to which the food item belongs).    }


\item{Precio:}{Numeric vector of food item’s retail price expressed per 100 g edible portion (i.e. the objective function coefficients)}


\item{Food:}{Vector of food items used in objective function}

\item{EER:}{A data frame containing the constraints of the linear programming model, as entered (i.e. EER by age and sex groups).}
}
\note{
It is important to review the documentation to ensure that the provided parameters are correct and consistent with the established requirements.
}
