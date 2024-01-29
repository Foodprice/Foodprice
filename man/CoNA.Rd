
\name{CoNA}
\alias{CoNA}
\title{Cost of Nutrient Adequacy (CoNA)}
\usage{

CoNA(
Datos_Insumo,
DRI_min=,
DRI_max,
Filtrar_Alimentos=NULL

)

}
\description{CoNA computes the minimum daily cost of a nutrient-adequate diet for an individual, according to its dietary reference intakes.}

\examples{

# Example:

# Retrieve input data
data(Datos_Prueba, package = "Foodprice") 
data(Dri_min, package = "Foodprice") 
data(Dri_max, package = "Foodprice") 


# Apply the function to the input data
Mod=CoNA(Datos_Insumo = Datos_Prueba,DRI_min=Dri_min,DRI_max=Dri_max)

# Cost Visualization
kable(Mod$Costo_CoNA)

}



\arguments{

 \code{- Required Parameters} 

        \item{Datos_Insumo:}{Data frame containing the retail price and nutrient composition of local food items, expressed per 100 g edible portion. The data frame must have 3 columns with specific names (“Food”, “Price_100g”, and “Energy”) and K-columns containing food items’ nutrient content for key macro- and micronutrients. }
        \item{DRI_min:}{Data frame containing the Estimated Energy Requirement (EER), as well as the lower limits of macro- and micronutrient intake for different age and sex groups. The data frame must include 3 columns with specific names (“Age”, “Sex”, and “EER”) and K-columns containing the lower limits of the K macro- and micronutrients. (The nutrient names must match the names used in data parameter) } 
       \item{DRI_max:}{Data frame containing the upper limits of macro- and micronutrient intake for different age and sex groups. The data frame must include 2 columns with specific names (“Age”, “Sex”) and K-columns containing the upper limits of the K macro- and micronutrients. (The nutrient names must match the names used in data parameter) } 

 \code{- Optional Parameters} 

        \item{Filtrar_Alimentos:}{A vector used for excluding one or more food items before performing the model.}

}



\details{
The CoNA is estimated by selecting the set of food items that, at the lowest cost, meets not only the Estimated Energy Requirement (EER), but also the lower and upper limits of macro- and micronutrient intake for an individual. The minimum cost of the nutrient-adequate diet is given by the solution of a linear programming model with 2K + 1 linear constraints. (See FoodPrice: An R Package for estimating least-cost diets for further information).
On the basis of the linear programming model, it is possible to identify limiting nutrients, as well as compute shadow price elasticity (SPE) of each nutrient. By definition, a nutrient is identified as a limiting nutrient if the intake level in optimal diet reaches exactly the lower limit. For the k-th nutrient, the SPE is defined as the percentage change in the objective function evaluated at the optimal basis (optimal CoNA) when the constraint is increased by 1%. (For formal details, see FoodPrice: An R Package for estimating least-cost diets).
Note that, to guarantee data integration, the nutrient names must be consistent across all parameters (data, EER_LL, UL). Additionally, for the EER parameter, it is not mandatory that the “Sex” column be defined by two categories (0 if male, 1 if female). If only one of the two categories is included, the function computes the CoNA by age groups. In this case specifying the “Sex” column is not needed.

}

\value{

The output will be displayed as a list based on the name assigned by the user; its elements are:

    \item{Alimentos_CoNA:}{A data frame containing the composition of the diet, differentiated by age and sex groups. The data frame displays, for each demographic group, the food items included in the optimal solution (food) and its quantity. (If the food groups were specified in data parameter, the data frame displays the food group to which the food item belongs). }
    
    \item{Costo_CoNA:}{A data frame containing, for each demographic group, the cost per day (cost_day) and the cost per 1000 kcal (cost_kcal) for the nutrient-adequate diet}
    
    \item{CoNA_N_Limit:}{A data frame with a dichotomous variable indicating whether a nutrient was a limiting nutrient. }

    \item{CoNA_SP:}{A data frame containing the shadow price (SP) and the shadow price elasticity (SPE) of each constraint. The data frame displays a dichotomous variable (constraints) indicating whether the shadow prices are computed based on the lower limit (min) or the upper limit (max).}

    \item{Precio:}{Numeric vector of food item’s retail price expressed per 100 g edible portion (i.e. the objective function coefficients)}

    \item{Alimento:}{A vector of food items used in objective function}

    \item{Req_entrantes:}{A data frame containing the constraints of the linear programming model, as entered (i.e. EER, lower and upper limits of macro- and micronutrient intake by age and sex groups).}

    
}

\note{
It is important to review the documentation to ensure that the provided parameters are correct and consistent with the established requirements.
}
