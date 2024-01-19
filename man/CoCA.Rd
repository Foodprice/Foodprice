
\name{CoCA}
\alias{CoCA}
\title{Función para la estimación del costo diario mínimo de una dieta suficiente en energía}
\usage{
CoCA(
Datos_Insumo,
EER,
Filtrar_Alimentos=NULL
)

}
\description{
La función CoCA calcula el alimento y su costo minimo para una dieta  suficiente en energía para el día. Presenta dos parámetros obligatorios y uno opcional.
}
\examples{

# Ejemplo de uso:

# Obtener datos de ejemplo
data(Datos_Prueba, package = "Foodprice") # Datos de insumo
data(DRI_min, package = "Foodprice") # Requerimientos energéticos

# Aplicar la función a los datos de entrada
CoCA(Datos_Insumo = Datos_Prueba, EER=DRI_min)

# Salidas en el ambiente global
kable(Costo_CoCA)

}

\arguments{

 \code{- PARÁMETROS NECESARIOS} 

    \item{Datos_Insumo:}{El marco de datos debe tener al menos tres columnas con estos nombres: "Precio_100g_ajust", "Alimento", "Energia", para asegurar una estimación precisa del modelo.} 
 
 
 \code{- PARÁMETROS OPCIONALES} 

    \item{DRI_M_OP:}{Marco de datos que puede ser utilizado como una entrada opcional para los requerimientos energéticos de hombres de diferentes edades. Debe incluir al menos dos columnas llamadas "Edad" y "Energía". La columna "Edad" debe ser de tipo carácter.}
    \item{DRI_F_OP:}{Marco de datos que puede ser utilizado como una entrada opcional para los requerimientos energéticos de muejeres de diferentes edades y/o grupos gestantes o lactantes. Debe incluir al menos dos columnas llamadas "Edad" y "Energía". La columna "Edad" debe ser de tipo carácter.}

}



\details{
Si los parámetros de requerimientos (DRI_M_OP o DRI_f_OP) no son utilizados, se emplearán por defecto los requerimientos energéticos establecidos para la región pacífica de Colombia.}
\value{
Las salida se mostrará en el ambiente global de R, su estructura es:

    \item{Costo_CoCA:}{Marco de datos con las estimaciones para hombres, que incluye el costo y las porciones del alimento en gramos y 100 gramos.}
    

}

\note{
Es importante revisar la documentación para asegurar que los parámetros proporcionados sean correctos y coherentes con los requisitos establecidos.
}
