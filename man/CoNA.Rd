
\name{CoNA}
\alias{CoNA}
\title{Función para la estimación del costo diario mínimo de una dieta adecuada en nutrientes.}
\usage{

CoNA(
Datos_Insumo,
DRI_min=,
DRI_max,
Filtrar_Alimentos=NULL

)

}
\description{
La función CoNA calcula los alimentos para una dieta adecuada en nutrientes y su costo mínimo diario. Presenta tres parámetros obligatorios y uno opcional.
}
\examples{

# Ejemplo de uso:

# Obtener datos de ejemplo
# Obtener datos de ejemplo
data(Datos_Prueba, package = "Foodprice") # Datos de insumo
data(Dri_min, package = "Foodprice") # Requerimientos minimos de nutrientes
data(Dri_max, package = "Foodprice") # Requerimientos máximos de nutrientes


# Aplicar la función a los datos de entrada
Mod=CoNA(Datos_Insumo = Datos_Prueba,DRI_min=Dri_min,DRI_max=Dri_max)

# Visualización del costo
kable(Mod$Costo_CoNA)

}



\arguments{

 \code{- PARÁMETROS NECESARIOS} 

        \item{Datos_Insumo:}{Data frame que contiene los precios minoristas ajustados por 100 gramos para cada alimento. Debe incluir al menos las columnas con los siguientes nombres: "Precio_100g_ajust", "Alimento" y "Energia". Además, puede contener columnas adicionales que representan los nutrientes que serán considerados en el modelo. Es crucial que los nombres de estos nutrientes sean consistentes tanto en Datos_Insumo como en DRI_min y DRI_max para garantizar una integración adecuada entre los conjuntos de datos.}
        \item{DRI_min:}{Data Frame de los requerimientos minimos nutricionales. Debe contener mínimo tres columnas así nombradas: "Edad","Energia","Sexo".La columna Edad se asume tipo caracter, Energia y Sexo (1=Femenino,0=Masculino) tipo numérico. Además, Puede contener columnas adicionales que representan los nutrientes que serán considerados en el modelo.} 
       \item{DRI_max:}{Data Frame de los requerimientos maximos nutricionales. Debe contener mínimo tres columnas así nombradas: "Edad","Energia","Sexo".La columna Edad se asume tipo caracter, Energia y Sexo (1=Femenino,0=Masculino) tipo numérico. Además, Puede contener columnas adicionales que representan los nutrientes que serán considerados en el modelo. Si un requerimiento tiene valor "NA", se asume igual a 99999} 

 \code{- PARÁMETROS OPCIONALES} 

        \item{Filtrar_Alimentos:}{Un vector utilizado como filtro para los alimentos antes de su ingreso al modelo. Este  es útil si se desea excluir uno o varios alimentos específicos del modelo.}

}



\details{
NOTA: Es crucial que los nombres los  nutrientes sean consistentes tanto en Datos_Insumo como en DRI_min y DRI_max para garantizar una integración adecuada entre los conjuntos de datos. Además, el vector necesario de "Energia" se asume se encuentra en DRI_min. Además, no es imperativo que ambas codificaciones (1=Femenino, 0=Masculino) estén presentes en la columna "Sexo" de EER. En el caso de que su conjunto de datos EER incluya únicamente un género, aún puede ejecutar el modelo sin problemas. En esta situación, simplemente no es necesario especificar la columna "Sexo".
}

\value{

Las salida se mostrarán como una lista con base en el nombre asignado por el usuario, sus elementos son:

    \item{Alimentos_CoNA:}{Data frame que proporciona detalles esenciales sobre la asignación de alimentos según el modelo, incluyendo el nombre del alimento seleccionado ("Alimento"), la cantidad necesaria en gramos para cumplir con los requisitos de energía y otros nutrientes ("Cantidad_GR"), el grupo demográfico al que se asigna ("Grupo_demo"), y la codificación del género asociado con la asignación ("Sexo"). La columna "Sexo" está codificada, utilizando 0 para representar el género masculino y 1 para el género femenino. Este conjunto de datos ofrece una visión completa de las elecciones alimenticias óptimas en términos de cantidad y tipo de alimentos para satisfacer los requerimientos energéticos y de nutrientes}
    
    \item{Costo_CoNA:}{Data frame que proporciona los costos del modelo. Columnas: "Grupo_demo" identifica el grupo demográfico asociado , mientras que la columna "Sexo" utiliza la codificación 0 para representar el género masculino y 1 para el género femenino . La columna "Costo" refleja el costo diario resultante de la asignación de alimentos para cumplir con los requisitos del modelo. }
    
    \item{CoNA_N_Limit:}{Data frame que proporciona los nutrientes limitantes}

    \item{CoNA_SP:}{Data frame que proporciona los precios sombra estimados}

    \item{Precio:}{Vector de precios de entrada en insumos al modelo}

    \item{Alimento:}{Vector de Alimentos de entrada en insumos al modelo}

    \item{Req_entrantes:}{Data frame con los requerimientos  de nutrientes mínimos y máximos ingresados al modelo (demás del vector de EER)}

    
}

\note{
Es importante revisar la documentación para asegurar que los parámetros proporcionados sean correctos y coherentes con los requisitos establecidos.
}
