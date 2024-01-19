
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

    \item{Datos_Insumo:}{Data frame con precios minorístas ajustado por 100gr a cada alimento; debe tener al menos tres columnas con estos nombres: "Precio_100g_ajust", "Alimento", "Energia", para asegurar una estimación correcta del modelo.} 
    \item{EER:}{Data frame que contiene información sobre los grupos demográficos, el sexo y sus respectivos contenidos energéticos. Este data frame debe incluir las columnas "Edad" , "Energia" y "Sexo". La columna Edad se asume tipo caracter, Energia y Sexo (1=Femenino,0=Masculino) tipo numérico}

 
 \code{- PARÁMETROS OPCIONALES} 

    \item{Filtrar_Alimentos:}{Un vector utilizado como filtro para los alimentos antes de su ingreso al modelo. Este  es útil si se desea excluir uno o varios alimentos específicos del modelo.}



\details{
Nota: No es necesario que ambas codificaciones (1=Femenino, 0=Masculino) estén presentes en la columna "Sexo" de EER. Si su conjunto de datos EER solo incluye un género, aún puede ejecutar el modelo sin problemas. En este caso, simplemente especifique en la columna "Sexo" si corresponde al valor 0 o 1.
}
\value{
Las salida se mostrará en el ambiente global de R, su estructura es:

\item{Costo_CoCA:}{Conjunto de resultados que muestra la asignación de costos asociados al alimento de menor costo capaz de satisfacer los requerimientos energéticos de cada grupo demográfico. Las columnas incluyen: "Alimento" (el alimento seleccionado), "Cantidad_G" (la cantidad necesaria en gramos para satisfacer los requerimientos de energía), "Grupo_demo" (grupo demográfico), "Sexo" (género) y "Costo" (el costo del alimento). Este conjunto proporciona una visión detallada de las elecciones alimenticias óptimas desde el punto de vista económico para cada grupo demográfico.}
}

\note{
Es importante revisar la documentación para asegurar que los parámetros proporcionados sean correctos y coherentes con los requisitos establecidos.
}
