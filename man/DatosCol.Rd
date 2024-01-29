\name{DatosCol}
\alias{DatosCol}
\title{Function to process SIPSA wholesale price data from DANE in Colombia, from 2013 to 2023.}
\usage{
DatosCol(
  Mes,
  Año,
  Ciudad,
  Percentil_Abast = NULL,
  Ingreso_Alimentos = NULL,
  data_list_precios = NULL,
  data_list_abas = NULL,
  Margenes = NULL
)
}
\description{
The DatosCol function provides the possibility to obtain and handle SIPSA wholesale price data from the National Administrative Department of Statistics (DANE) in Colombia, suitable for a specific month, year, and city. Its main result focuses on the estimates of retail prices, linking the corresponding nutrients for each food.
}
\examples{

# Example of usage:

# Obtain data for Medellín, January 2017
DatosCol(Month = 1, Year = 2017, City = "Medellín")

# Outputs in the global environment

kable(head(PRICES_SIPSA_January_2017))
kable(head(Data_Input_Models_2017_January_Medellín))

}

\arguments{

 \code{- REQUIRED PARAMETERS} 

    \item{Mes:}{A numerical value representing the month of the year (Range from 1 to 12).}
    \item{Año:}{A numerical value between 2014 and 2023, indicating the year of interest.}
    \item{Ciudad:}{A text specifying the city of interest in Colombia.}

 \code{- OPTIONAL PARAMETERS} 

    \item{Percentil_Abast:}{A number within the range of 0 to 1 indicating the percentile used to filter foods with a supply level below this value. Defaults to NULL.}
    \item{Ingreso_Alimentos:}{A vector or data frame with 21 columns, which can have a size of 21 if it is a vector. This dataset acts as a supplement for missing nutritional information for certain foods. Initially, nutritional analyses were conducted exclusively for the city of Cali, Colombia, resulting in a lack of information on the nutritional composition of various foods in other cities. The function identifies and displays the missing foods in the provided city due to this lack of specific nutritional data for each location.}
    \item{data_list_precios:}{A list incorporating price information. For proper functioning, the package expects data downloaded directly from DANE, avoiding any prior data cleaning procedure. It is assumed that this data is in an Excel file with multiple sheets, so it should be loaded as a list using the RIO library. For example, the rio::import_list(Excel_file, setclass = "tbl") function can be used.}
    \item{data_list_abas:}{A collection of data linked to supply. The loading process for this data is expected to follow the same guidelines as those established for loading the price list, i.e., applying the same loading conditions and procedures used for data_list_prices.}
    \item{Margenes:}{A set of eight values representing the marketing margins associated with each food category. These values must follow a specific order corresponding to the following categories: meats, fruits, grains and cereals, dairy and eggs, fish, processed products, tubers, roots and bananas, vegetables.}

}



\details{
The function performs a thorough validation of the entered parameters to ensure their coherence and then processes the corresponding DANE data to estimate retail prices along with the composition of each food.
}

\value{
The function output is a list containing the estimated retail prices and the data frame downloaded from DANE.
}

\note{
It is important to review the documentation to ensure that the provided parameters are correct and consistent with the established requirements.
}
