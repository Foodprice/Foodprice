
\name{CoRD}
\alias{CoRD}
\title{Función para la estimación del costo diario mínimo de una dieta saludable.}
\usage{

Modelo_3(
Datos_Insumo,
Req_Int,
Cantidad_selec,
Filtrar_Alimentos=NULL
)

}
\description{
La función CoRD calcula los alimentos para una dieta saludable y su costo mínimo diario. Presenta tres parámetros obligatorios y uno opcional.
}
\examples{

# Ejemplo de uso:

# Obtener datos de insumo
data(Datos_Prueba, package = "Foodprice")

data(Req_Int, package = "Foodprice") # obtener los requerimientos de porciones pro grupos de alimento y grupos demográficos
data(Cantidad_selec, package = "Foodprice") # Cantidad de alimentos a selecionar por grupos o subgrupos


# Aplicar la función a los datos de entrada
Mod=CoRD(Datos_Insumo = Datos_Prueba,Cantidad_selec = Cantidad_selec,Req_Int = Req_Int)

# Visualización del costo
head(Mod$Costo_CORD)
}


\arguments{

 \code{- PARÁMETROS NECESARIOS}     

\item{Datos_Insumo:}{El marco de datos debe contener al menos cuatro columnas con los siguientes nombres: "Alimento" (Alimentos), "Intercambios_g" (Intercambio de cada alimento), "Precio_INT" (Precio por Intercambio del alimento), "Grupo"(Grupo de alimentos, se requieren mínimo 5 grupos).
Adicional a lo anterior, si requiere que el modelo calcule los alimentos con base en subgrupos pero que el costo se vea en función de Grupos, añada la columna "Subgrupo" a los datos de insumo. Si también requiere calcular el costo por Mil kilocalorias, añada una columna llamda "Energia" con el aporte energético de cada alimento en los datos de insumo.
 }

  \item{Req_Int:}{ Data frame con los requerimientos de porciones por grupos de alimentos y grupos demográficos. Debe contener al menos tres columnas así nombradas: "Edad" (Grupo demográfico), "Porción" (Porciones de intercambios por grupo), "Grupo" (Grupos de alimentos). Si se requiere que el modelo trabaje con subgrupos, debe existir la columna "subgrupo" con los mimos niveles que los Subgrupos de los datos de insumo (No es necesaio incluir la columna "Grupo" en tal caso).}
  \item{Cantidad_selec:}{Data frame con la cantidad de alimentos a selecionar por cada grupo o subgrupo de alimento. Este debe contener al menos dos columnas así nombradas: "Cantidad", "Grupo". Si desea trabajar con subgrupos, cambie la columna "Grupo" por "Subgrupo".}

 \code{- PARÁMETROS OPCIONALES}

        \item{Filtrar_Alimentos:}{Un vector utilizado como filtro para los alimentos antes de su ingreso al modelo. Este  es útil si se desea excluir uno o varios alimentos específicos del modelo.}
}




\details{
NOTA: Si se requere trabajar con subgrupos, debe existir en los datos de insumo una columna llamada "Subgrupo" además de "Grupo"; Los parámetros Req_Int y Cantidad_selec deberán tener la una columna llamada "Subgrupo" (No es necesario que presenten la columna "Grupo").
}
\value{

Las salida se mostrarán como una lista con base en el nombre asignado por el usuario, sus elementos son:

    \item{Costo_CORD:}{Data frame que proporciona los costos del modelo. Columnas: "Grupo_demo" identifica el grupo demográfico asociado , mientras que la columna "Sexo" utiliza la codificación 0 para representar el género masculino y 1 para el género femenino . La columna "Costo" refleja el costo diario resultante de la asignación de alimentos para cumplir con los requisitos del modelo. Si los datos de insumo tienen la columna "Energia", calculará el costo por Mil kilocalorias por cada grupo demográfico y sexo en la columna llamada: Costo_1000kcal. }
    
    \item{Intercambios_CoRD:}{Data frame que proporciona la cantidad de intercambios por cada Alimento y grupo en cada grupo demográfico y sexo. contiene las columnas:"Alimento", "Grupo", "Cantidad_INT","Edad", "Sexo"}

    \item{Precio:}{Vector de precios de entrada en insumos al modelo}

    \item{Alimento:}{Vector de Alimentos de entrada en insumos al modelo}
    
    \item{Energia:}{Vector aporte energéico de cada alimento en insumos al modelo (si este existe)}



}

\note{
Es importante revisar la documentación para asegurar que los parámetros proporcionados sean correctos y coherentes con los requisitos establecidos.
}
