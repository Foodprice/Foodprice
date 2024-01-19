#------------------------------------------------------------------------------------------#
#                    SEGUNDA FUNCIÓN: MODELO 2: DIETA ADEC EN NUTRIENTES                  #
#-----------------------------------------------------------------------------------------#


Modelo_2=function(Datos_Insumo,DRI_min,DRI_max,Filtrar_Alimentos=NULL){

#------------------------------------------------------------------------------------------#
#                       PRIMERA ETAPA: VALIDACIÓN DE LIBRERIAS                             #
#-----------------------------------------------------------------------------------------#

Librerias_base = c("readxl","dplyr","ggplot2","reshape2","knitr","haven","foreign","stringi","labelled","tidyr","plyr","tidyverse",
                        "lpSolve","Rglpk","scatterplot3d","reshape","R6","rio","janitor","stringr") # Nombra las librerias necesarias

if (!require("pacman")) install.packages("pacman") # Paquete que simplifica la carga de librerias
pacman::p_load(char = Librerias_base);Librerias_base_print = paste0(paste0("'", Librerias_base, "'"), collapse = ", ") # Instala si es necesario, o en su defecto, sólo llama los paquetes

cat("\n")
print("Se instalaron y cargaron todas la librerias corectamente")
cat("\n")

#------------------------------------------------------------------------------------------#
#         SEGUNDA ETAPA: VALIDACIÓN DE PARÁMETROS OBLIGATORIOS Y OPCIONALES                #
#-----------------------------------------------------------------------------------------#

# Verificar si Datos_Insumo es un data frame
  if (!is.data.frame(Datos_Insumo)) {
    stop("Error: Datos_Insumo no es un data frame.")
  }
  
  # Verificar si tiene al menos 3 columnas
  if (ncol(Datos_Insumo) <= 3) {
    stop("Error: Datos_Insumo debe tener al menos 3 columnas.")
  }

required_columns2 <- c("Precio_100g_ajust", "Alimento", "Energia")

missing_columns2 <- setdiff(required_columns2, colnames(Datos_Insumo))

if (length(missing_columns2) > 0) {
  stop(paste("El modelo 2 requiere las siguientes columnas en los datos de insumo:", paste(missing_columns2, collapse = ", "),". Por favor revise la documentación para conocer el nombre que deben tener las columnas necesarias al segundo modelo"))}

# -------------- Filtrar_Alimentos

 # Validar si Filtrar_Alimentos es distinto de NULL

  if (!is.null(Filtrar_Alimentos)) {
    # Validar si Filtrar_Alimentos es un vector
    if (!is.vector(Filtrar_Alimentos)) {
      stop("El parámetro Filtrar_Alimentos debe ser un vector.")
    }
    
    # Filtrar los alimentos que no están en Filtrar_Alimentos
    Datos_insumo <- Datos_insumo[!(Datos_insumo$Alimento %in% Filtrar_Alimentos), ]
  }



#--------------------------------------------------------------------------------------#
#            TERCERA ETAPA: MODELO FEMENINO- VALIDACIÓN DE NUTRIENTES DE ENTRADA      #
#------------------------------------------------------------------------------------#
categorias_unicas <- unique(DRI_min$Sexo)

if (!1 %in% categorias_unicas) {
  print("No se correrá para el modelo para el sexo femenino ya que la categoría 1 no está presente en la columna sexo del parametro EER.")
  Femenino=FALSE
} else {
  Femenino=TRUE
}


if(Femenino==TRUE) {


DRI_min_F=subset(DRI_min,Sexo==1)

DRI_max_F=subset(DRI_max,Sexo==1);DRI_max_F[is.na(DRI_max_F)] = 999999


# Asignación de vectores
Precio = Datos_Insumo$Precio_100g_ajust;Alimento=Datos_Insumo$Alimento;Edad=DRI_min_F$Edad

# DF de limitaciones en nutrientes y validación de nombres
DRI_min_F_li= DRI_min_F %>% select(-any_of(c("Edad","Energia","Sexo")))
DRI_max_F_li= DRI_max_F %>% select(-any_of(c("Edad","Energia","Sexo")))

if (!identical(names(DRI_min_F_li), names(DRI_max_F_li))) {
  stop("Los datos DRI max y min no tienen los mismos nombres en la columnas.")
}

# selecionar de DRI_min energía
DRI_min_F_li= DRI_min_F %>% select(-any_of(c("Edad","Sexo")))
DRI_max_F_li= DRI_max_F %>% select(-any_of(c("Edad","Energia","Sexo")))


# Exraer los nutrientes de entrada que son distintos a las columnas: ("Cod_TCAC", "Alimento", "Serving", "Precio_100g_ajust")
DF_Nutrientes_ALimentos <- Datos_Insumo %>% select(-any_of(c("Cod_TCAC", "Alimento", "Serving", "Precio_100g_ajust")))

# Identificar las columnas que son iguales de datos insumo y requerimientos y ordenarlas
nombres_comunes <- intersect(names(DF_Nutrientes_ALimentos), names(DRI_min_F_li))

# Ordenar los nombrespara el modelo
DF_Nutrientes_ALimentos <- DF_Nutrientes_ALimentos %>% select(any_of(nombres_comunes));DRI_min_F_li <- DRI_min_F_li %>% select(any_of(nombres_comunes));  DRI_max_F_li <- DRI_max_F_li %>% select(any_of(nombres_comunes))


# Unir los nutrientes de aliemntos en min y max
Sin_EER= DF_Nutrientes_ALimentos %>% select(-Energia)
DF_Nutrientes_ALimentos=cbind(DF_Nutrientes_ALimentos,Sin_EER)


# Matriz de coef de restricción al modelo (ENERGIA y nutrientes)
Coef.Restriq=DF_Nutrientes_ALimentos %>% as.matrix() %>% t()


#signos de las restricciones
constr_signs = c("=", rep(">=", ncol(DRI_min_F_li)-1), rep("<=", length(DRI_max_F_li)))

#Unir los EER, minx y max
Limitaciones=cbind(DRI_min_F_li,DRI_max_F_li)

#--------------------------------------------------------------------------------------#
#            CUARTA ETAPA: MODELO FEMENINO- SOLUCIÓN                                  #
#------------------------------------------------------------------------------------#



#------------------------------Solución:
#DF de la solución de intercambios
Intercambios_CoNA_F <- data.frame(Alimento = character(), Cantidad_GR = numeric(), Grupo_demo = integer(), Sexo = integer())

#DF de la solución de csotos
Costo_CoNA_F <- data.frame(Grupo_demo = integer(), Sexo = integer(), Costo_dia = numeric())
# Modelo
for (i in seq_along(Edad)) { #ciclo para cada edad

CoNA_F = lp(direction = "min",
               objective.in = Precio,
               const.mat = Coef.Restriq,
               const.dir = constr_signs,
               const.rhs =as.vector(unlist(Limitaciones[i, , drop = FALSE])),
               compute.sens = TRUE)

# Guardar estructura de intercambios
costo <- sum(CoNA_F$solution * Precio)
Alimentos_sol <- which(CoNA_F$solution != 0) # ALimento selexionados
cantidades_intercambio <- CoNA_F$solution[Alimentos_sol] # intercambios
    
# Crear un dataframe temporal de la estructura CIAT
temp_df <- data.frame(Alimento = Alimento[Alimentos_sol],
Cantidad_GR = (cantidades_intercambio*100),
Grupo_demo = Edad[i],
Sexo = 1)

# Agregar los resultados al dataframe general
Intercambios_CoNA_F <- merge(Intercambios_CoNA_F, temp_df, all = TRUE)

# Guardar estructura de costo

Costo_dia=sum(CoNA_F$solution * Precio)

# Agregar los resultados al dataframe Costo_CoNA
temp_df <- data.frame(Grupo_demo = Edad[i],
                          Sexo = 1,
                          Costo_dia = sum(CoNA_F$solution * Precio))

# Agregar los resultados al dataframe general
Costo_CoNA_F <- rbind(Costo_CoNA_F, temp_df)



}

}
 else {
   Costo_CoNA_F=NULL
   Intercambios_CoNA_F=NULL

}


#--------------------------------------------------------------------------------------#
#            QUINTA ETAPA: MODELO MASCULINO- VALIDACIÓN DE NUTRIENTES DE ENTRADA      #
#------------------------------------------------------------------------------------#


if (!0 %in% categorias_unicas) {
  print("No se correrá para el modelo para el sexo femenino ya que la categoría 1 no está presente en la columna sexo del parametro EER.")
  Masculino=FALSE
} else {
  Masculino=TRUE
}



if(Masculino==TRUE) {

DRI_min_M=subset(DRI_min,Sexo==0)

DRI_max_M=subset(DRI_max,Sexo==0);DRI_max_M[is.na(DRI_max_M)] = 999999


# Asignación de vectores
Precio = Datos_Insumo$Precio_100g_ajust;Alimento=Datos_Insumo$Alimento;Edad=DRI_min_M$Edad

# DF de limitaciones en nutrientes y validación de nombres
DRI_min_M_li= DRI_min_M %>% select(-any_of(c("Edad","Energia","Sexo")))
DRI_max_M_li= DRI_max_M %>% select(-any_of(c("Edad","Energia","Sexo")))

if (!identical(names(DRI_min_M_li), names(DRI_max_M_li))) {
  stop("Los datos DRI max y min no tienen los mismos nombres en la columnas.")
}

# selecionar de DRI_min energía
DRI_min_M_li= DRI_min_M %>% select(-any_of(c("Edad","Sexo")))
DRI_max_M_li= DRI_max_M %>% select(-any_of(c("Edad","Energia","Sexo")))


# Exraer los nutrientes de entrada que son distintos a las columnas: ("Cod_TCAC", "Alimento", "Serving", "Precio_100g_ajust")
DF_Nutrientes_ALimentos <- Datos_Insumo %>% select(-any_of(c("Cod_TCAC", "Alimento", "Serving", "Precio_100g_ajust")))

# Identificar las columnas que son iguales de datos insumo y requerimientos y ordenarlas
nombres_comunes <- intersect(names(DF_Nutrientes_ALimentos), names(DRI_min_M_li))

# Ordenar los nombrespara el modelo
DF_Nutrientes_ALimentos <- DF_Nutrientes_ALimentos %>% select(any_of(nombres_comunes));DRI_min_M_li <- DRI_min_M_li %>% select(any_of(nombres_comunes));  DRI_max_M_li <- DRI_max_M_li %>% select(any_of(nombres_comunes))


# Unir los nutrientes de aliemntos en min y max
Sin_EER= DF_Nutrientes_ALimentos %>% select(-Energia)
DF_Nutrientes_ALimentos=cbind(DF_Nutrientes_ALimentos,Sin_EER)


# Matriz de coef de restricción al modelo (ENERGIA y nutrientes)
Coef.Restriq=DF_Nutrientes_ALimentos %>% as.matrix() %>% t()


#signos de las restricciones
constr_signs = c("=", rep(">=", ncol(DRI_min_M_li)-1), rep("<=", length(DRI_max_M_li)))

#Unir los EER, minx y max
Limitaciones=cbind(DRI_min_M_li,DRI_max_M_li)

#Correción con mayores de 70
#Limitaciones[nrow(Limitaciones), ] <- (1 - 0.067) * Limitaciones[7, ]

#--------------------------------------------------------------------------------------#
#            SEXTA ETAPA: MODELO MASCULINO- SOLUCIÓN                                  #
#------------------------------------------------------------------------------------#



#------------------------------Solución:
#DF de la solución de intercambios
Intercambios_CoNA_M <- data.frame(Alimento = character(), Cantidad_GR = numeric(), Grupo_demo = integer(), Sexo = integer())

#DF de la solución de csotos
Costo_CoNA_M <- data.frame(Grupo_demo = integer(), Sexo = integer(), Costo_dia = numeric())
# Modelo
for (i in seq_along(Edad)) { #ciclo para cada edad

CoNA_M = lp(direction = "min",
               objective.in = Precio,
               const.mat = Coef.Restriq,
               const.dir = constr_signs,
               const.rhs =as.vector(unlist(Limitaciones[i, , drop = FALSE])),
               compute.sens = TRUE)

# Guardar estructura de intercambios
costo <- sum(CoNA_M$solution * Precio)
Alimentos_sol <- which(CoNA_M$solution != 0) # ALimento selexionados
cantidades_intercambio <- CoNA_M$solution[Alimentos_sol] # intercambios
    
# Crear un dataframe temporal de la estructura CIAT
temp_df <- data.frame(Alimento = Alimento[Alimentos_sol],
Cantidad_GR = (cantidades_intercambio*100),
Grupo_demo = Edad[i],
Sexo = 0)

# Agregar los resultados al dataframe general
Intercambios_CoNA_M <- merge(Intercambios_CoNA_M, temp_df, all = TRUE)

# Guardar estructura de costo

Costo_dia=sum(CoNA_M$solution * Precio)

# Agregar los resultados al dataframe Costo_CoNA
temp_df <- data.frame(Grupo_demo = Edad[i],
                          Sexo = 0,
                          Costo_dia = sum(CoNA_M$solution * Precio))

# Agregar los resultados al dataframe general
Costo_CoNA_M <- rbind(Costo_CoNA_M, temp_df)



}

}
 else {
   Costo_CoNA_M=NULL
   Intercambios_CoNA_M=NULL

}

View(Limitaciones)
View(Intercambios_CoNA_M)


#------------------------------------------------------------------------------------------#
#                       FIN DEL TERCER MÓDULO COMO FUNCIÓN                                 #
#-----------------------------------------------------------------------------------------#

}


library(Foodprice)
Modelo_2(Datos_Insumo=Datos_Prueba,DRI_min=DRI_min,DRI_max=DRI_max)
options(error=recover)


