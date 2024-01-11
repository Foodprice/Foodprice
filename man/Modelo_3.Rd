#------------------------------------------------------------------------------------------#
#                 D O C U M E N T A C I Ó N: FUNCIÓN-Datos_dane_col                        #
#-----------------------------------------------------------------------------------------#

\name{Modelo_3}
\alias{Modelo_3}
\title{Función para la estimación del costo diario mínimo de una dieta saludable.}
\usage{

Modelo_3(
Datos_Insumo,
Intercambio_Gramos_OP=NULL,
Int_Req_M_OP=NULL,
Int_Req_F_OP=NULL
)

}
\description{
La función Modelo_3 calcula los alimentos para una dieta saludable y su costo mínimo diario. Presenta un parámetro obligatorio y tres opcionales.
}
\examples{

# Ejemplo de uso:

# Obtener datos de ejemplo
data(Datos_Prueba, package = "Foodprice")

# Aplicar la función a los datos de entrada
Modelo_3(Datos_Insumo = Datos_Prueba)

# Salidas en el ambiente global
kable(head(Modelo_3_M))
kable(head(Modelo_3_F))
}


\arguments{

 \code{- PARÁMETROS NECESARIOS}     

  \item{Datos_Insumo:}{El marco de datos debe contener al menos cuatro columnas así nombradas: "Cod_TCAC", "Alimento", "Serving", "Precio_100g_ajust". Se asume es un  DataFrame.}

 \code{- PARÁMETROS OPCIONALES} 

  \item{Intercambio_Gramos_OP:}{DataFrame opcional que debe contener al menos 6 columnas específicas tituladas: "Cod_TCAC", "Alimentos", "Subgrupo_GABAS", "Energia_100g", "Energia_Int", "Intercambio_g". Si no se proporciona, usará los intercambios mapeados para los alimentos de colombia}

  \item{Int_Req_M_OP:}{DataFrame  que puede utilizarse como entrada opcional para los intercambios requeridos en gramos de cada grupo de alimento en hombres de diferentes edades. Debería incluir al menos dos columnas. El paquete asume que existe una primer columna dedicada a los grupos de alimentos y, las siguientes columnas son los grupos de edad.}

  \item{Int_Req_F_OP:}{DataFrame  que puede utilizarse como entrada opcional para los intercambios requeridos en gramos de cada grupo de alimento en mujeres de diferentes edades. Debería incluir al menos dos columnas. El paquete asume que existe una primer columna dedicada a los grupos de alimentos y, las siguientes columnas son los grupos de edad.}
}




\details{
Si los parámetros opcionales no son utilizados, se emplearán por defecto los requerimientos de intercambios establecidos para la región pacífica de Colombia.}

\value{

Las salidas se mostrarán en el ambiente global de R, su estructura es:

    \item{Modelo_3_M:}{Marco de datos con los costos de una dieta saludable para los hombres.}
    
    \item{Modelo_3_F:}{Marco de datos con los costos de una dieta saludable para las mujeres.}

}

\note{
Es importante revisar la documentación para asegurar que los parámetros proporcionados sean correctos y coherentes con los requisitos establecidos.
}
