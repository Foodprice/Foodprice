#------------------------------------------------------------------------------------------#
#                 D O C U M E N T A C I Ó N: FUNCIÓN-Datos_dane_col                        #
#-----------------------------------------------------------------------------------------#

\name{Modelo_2}
\alias{Modelo_2}
\title{Función para la estimación del costo diario mínimo de una dieta adecuada en nutrientes.}
\usage{

Modelo_2(
Datos_Insumo,
DRI_M_OP=NULL,
DRI_F_OP=NULL
)

}
\description{
La función Modelo_2 calcula los alimentos para una dieta adecuada en nutrientes y su costo mínimo diario. Presenta un parámetro obligatorio y dos opcionales.
}
\examples{

# Ejemplo de uso:

# Obtener datos de ejemplo
data(Datos_Prueba, package = "Foodprice")

# Aplicar la función a los datos de entrada
Modelo_2(Datos_Insumo = Datos_Prueba)

# Salidas en el ambiente global
kable(head(Modelo_2_M))
kable(head(Modelo_2_F))
}



\arguments{

 \code{- PARÁMETROS NECESARIOS} 

    \item{Datos_Insumo:}{El marco de datos debe contener al menos estas columnas así nombradas: "Precio_100g_ajust", "Alimento", "Energia", "Proteina", "Lipidos", "Carbohidratos", "VitaminaC", "Folatos", "VitaminaA", "Tiamina", "Riboflavina", "Niacina", "VitaminaB12", "Magnesio", "Fosforo", "Sodio", "Calcio", "Hierro" y "Zinc", para garantizar una estimación precisa del modelo. La columna "Precio_100g_ajust" debe indicar el precio por 100 gramos de cada alimento, "Alimento" identifica el tipo de comida y "Energia" muestra la cantidad de energía que aporta cada alimento. Además, las otras columnas proporcionarán información nutricional detallada como proteínas, lípidos, carbohidratos y diversas vitaminas y minerales para un análisis completo.}

 \code{- PARÁMETROS OPCIONALES} 

    \item{DRI_M_OP:}{DataFrame que puede utilizarse como entrada opcional para los requerimientos nutricionales de hombres de diferentes edades. Debería incluir al menos diecisiete columnas tituladas: "edad", "energia", "proteina_l", "grasa_l", "cho_l", "vit_c_l", "folato_l", "vit_a_l", "tiamina_l", "ribo_l", "niacina_l", "b12_l", "mg_l", "p_l", "na_l", "ca_l", "fe_l", "zinc_l". La columna "edad" debe ser de tipo carácter. El sufijo "_l" hace referencia a los requerimientos mínimos del nutriente.}
    \item{DRI_M_OP:}{DataFrame que puede utilizarse como entrada opcional para los requerimientos nutricionales de mujeres de diferentes edades /o grupos gestantes o lactantes. Debería incluir al menos diecisiete columnas tituladas: "edad", "energia", "proteina_l", "grasa_l", "cho_l", "vit_c_l", "folato_l", "vit_a_l", "tiamina_l", "ribo_l", "niacina_l", "b12_l", "mg_l", "p_l", "na_l", "ca_l", "fe_l", "zinc_l". La columna "edad" debe ser de tipo carácter. El sufijo "_l" hace referencia a los requerimientos mínimos del nutriente.}

}



\details{
Si los parámetros de requerimientos (DRI_M_OP o DRI_f_OP) no son utilizados, se emplearán por defecto los requerimientos nutricionales establecidos para la región pacífica de Colombia.}

\value{

Las salidas se mostrarán en el ambiente global de R, su estructura es:

    \item{Modelo_2_M:}{Marco de datos con las estimaciones para hombres, que incluye el costo y los alimentos para la dieta.}
    
    \item{Modelo_2_F:}{Marco de datos con las estimaciones para muejeres, que incluye el costo y los alimentos para la dieta.}

}

\note{
Es importante revisar la documentación para asegurar que los parámetros proporcionados sean correctos y coherentes con los requisitos establecidos.
}
