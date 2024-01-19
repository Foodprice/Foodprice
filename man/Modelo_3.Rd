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





\details{
Si los parámetros opcionales no son utilizados, se emplearán por defecto los requerimientos de intercambios establecidos para la región pacífica de Colombia.}



\note{
Es importante revisar la documentación para asegurar que los parámetros proporcionados sean correctos y coherentes con los requisitos establecidos.
}
