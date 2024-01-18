#------------------------------------------------------------------------------------------#
#                     SEGUNDA FUNCIÓN: MODELO 1: DIETA SUF EN ENERGÍA                      #
#-----------------------------------------------------------------------------------------#


Modelo_1=function(Datos_Insumo,EER,Filtrar_Alimentos=NULL{

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

# -------------- VERIFICACIÓN DE DATOS DE INSUMO

  # Verificar si Datos_Insumo es un data frame
  if (!is.data.frame(Datos_Insumo)) {
    stop("Datos_Insumo no es un data frame.")
  }
  
  # Verificar si tiene al menos 3 columnas
  if (ncol(Datos_Insumo) < 3) {
    stop("Datos_Insumo debe tener al menos 3 columnas.")
  }
required_columns <- c("Precio_100g_ajust", "Alimento", "Energia")
missing_columns <- setdiff(required_columns, colnames(Datos_Insumo))

if (length(missing_columns) > 0) {
  stop(paste("El modelo 1 requiere las siguientes columnas en los datos de insumo: ", paste(missing_columns, collapse = ", "),". Por favor revise la documentación para conocer el nombre que deben tener las columnas necesarias al primer modelo"))}

#Filtrar azucar
if ("Cod_TCAC" %in% colnames(Datos_Insumo)) {Datos_Insumo = Datos_Insumo %>% filter(!Cod_TCAC %in% c("K003", "K004", "K033","D013"))} 

# -------------- VERIFICACIÓN DE EER


  # Verificar si Datos_Insumo es un data frame
  if (!is.data.frame(EER)) {
    stop("Datos_Insumo no es un data frame.")
  }
  
  # Verificar si tiene al menos 3 columnas
  if (ncol(EER) < 2) {
    stop("Los requerimientos para el modelo 1 deben contener al menos 2 columnas.")
  }
required_columns_E <- c("Edad","Energia")
missing_columns_E <- setdiff(required_columns_E, colnames(EER))

if (length(missing_columns_E) > 0) {
  stop(paste("El modelo 1 requiere las siguientes columnas en los datos de insumo: ", paste(missing_columns_E, collapse = ", "),". Por favor revise la documentación para conocer el nombre que deben tener las columnas necesarias al primer modelo"))}


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

  
#------------------------------------------------------------------------------------------#
#                       TERCERA ETAPA: MODELO 1 MASCULINO                                   #
#-----------------------------------------------------------------------------------------#

#--------------------Preparación:

# Asignación de vectores



# Matriz de contenidos energéticos
A = matrix(as.vector(Datos_Insumo$Energia), ncol = length(alimentos))

edad=DRI_M$Edad
 modelo_1 = data.frame(alimentos)
  modelo_1 = modelo_1 %>% add_row(alimentos = "Costo")
  colnames(modelo_1) = "Alimentos"


#--------------------Solución:

  #signos de las restricciones lineales
  constr_signs = c("=")

  #requerimientos de energia por edad
  dri_edad = list()

  for (i in 1:8) {
    b = vector()
    b = DRI_M[i,2]
    dri_edad = append(dri_edad, b)
    names(dri_edad)[i] = paste0("b_", i)
  }


  #solucion del modelo
  for (i in 1:8) {
    df_1 = data.frame()
    df_2 = data.frame()
    b = dri_edad[[i]]
    opt_sol = lp(direction = "min",
                 objective.in = precios,
                 const.mat = A,
                 const.dir = constr_signs,
                 const.rhs = b,
                 compute.sens = TRUE)
    df_1 = cbind(alimentos, opt_sol$solution)
    colnames(df_1) = c("Alimentos", edad[i])
    df_2 = data.frame("Costo", opt_sol$objval)
    colnames(df_2) = colnames(df_1)
    df_1 = rbind(df_1, df_2)
    modelo_1 = merge(modelo_1,df_1, by = "Alimentos")
    rm(b, df_1, df_2)
  }

  # presentacion de la solucion por grupos etarios

  #eliminar cantidades NA
  modelo_1[modelo_1 == 0] = NA
  modelo_1_res = modelo_1[rowSums(is.na(modelo_1[,2:length(colnames(modelo_1))])) != ncol(modelo_1[,2:length(colnames(modelo_1))]),]

 # Encuentra el nombre del alimento diferente a 'Costo'
alimento_nombre <- modelo_1_res[modelo_1_res$Alimentos != "Costo", "Alimentos"]

# Encuentra la fila del alimento
alimento_fila <- modelo_1_res[modelo_1_res$Alimentos == alimento_nombre, ]

# Multiplica los valores por 100
alimento_fila_100g <- alimento_fila
alimento_fila_100g[-1] <- as.numeric(alimento_fila_100g[-1]) * 100

# Agrega una nueva fila con los valores de alimento * 100
modelo_1_res <- rbind(modelo_1_res, alimento_fila_100g)

# Modifica el nombre del alimento para representar 100g
modelo_1_res[nrow(modelo_1_res), "Alimentos"] <- paste0(alimento_nombre, " (100g)")

assign("Modelo_1_M",modelo_1_res,envir = globalenv())
#------------------------------------------------------------------------------------------#
#                       QUINTA ETAPA: MODELO 1 FEMENINO                                   #
#-----------------------------------------------------------------------------------------#


 #requerimientos de energia por edad
  dri_edad = list()

  for (i in 1:14) {
    b = vector()
    b = DRI_F[i,2]
    dri_edad = append(dri_edad, b)
    names(dri_edad)[i] = paste0("b_", i)
  }



  # base de datos de resultados
  modelo_1 = data.frame(alimentos)
  modelo_1 = modelo_1 %>% add_row(alimentos = "Costo")
  colnames(modelo_1) = "Alimentos"

  edad = DRI_F$Edad

  #solucion del modelo
  for (i in 1:14) {
    df_1 = data.frame()
    df_2 = data.frame()
    b = dri_edad[[i]]
    opt_sol = lp(direction = "min",
                 objective.in = precios,
                 const.mat = A,
                 const.dir = constr_signs,
                 const.rhs = b,
                 compute.sens = TRUE)
    df_1 = cbind(alimentos, opt_sol$solution)
    colnames(df_1) = c("Alimentos", edad[i])
    df_2 = data.frame("Costo", opt_sol$objval)
    colnames(df_2) = colnames(df_1)
    df_1 = rbind(df_1, df_2)
    modelo_1 = merge(modelo_1,df_1, by = "Alimentos")
    rm(b, df_1, df_2)
  }

  # presentacion de la solucion por grupos etarios

  #eliminar cantidades NA
  modelo_1[modelo_1 == 0] = NA
  modelo_1_res = modelo_1[rowSums(is.na(modelo_1[,2:length(colnames(modelo_1))])) != ncol(modelo_1[,2:length(colnames(modelo_1))]),]

 # Encuentra el nombre del alimento diferente a 'Costo'
alimento_nombre <- modelo_1_res[modelo_1_res$Alimentos != "Costo", "Alimentos"]

# Encuentra la fila del alimento
alimento_fila <- modelo_1_res[modelo_1_res$Alimentos == alimento_nombre, ]

# Multiplica los valores por 100
alimento_fila_100g <- alimento_fila
alimento_fila_100g[-1] <- as.numeric(alimento_fila_100g[-1]) * 100

# Agrega una nueva fila con los valores de alimento * 100
modelo_1_res <- rbind(modelo_1_res, alimento_fila_100g)

# Modifica el nombre del alimento para representar 100g
modelo_1_res[nrow(modelo_1_res), "Alimentos"] <- paste0(alimento_nombre, " (100g)")

  assign("Modelo_1_F",modelo_1_res,envir = globalenv())
  if(length(warnings())<100) {print ("Ejecución del modelo 1 correcta") }


#------------------------------------------------------------------------------------------#
#                       FIN DEL SEGUNDO MÓDULO COMO FUNCIÓN                                 #
#-----------------------------------------------------------------------------------------#

}

#-------------------
library(stringdist)

nombres_columnas <- c("Cod_TCAC", "Alimento", "Serving", "Precio_100g_ajust", "Energía", "Proteina",
                      "Carbohidratos", "Lipidos", "Calcio", "Zinc", "Hierro", "Magnesio", "Fosforo",
                      "VitaminaC", "Tiamina", "Riboflavina", "Niacina", "Folatos", "VitaminaB12",
                      "VitaminaA", "Sodio")

# Nombres clave en inglés y español
  nombres_clave <- c("Precio", "Alimento", "Energia")
  

  
# Encontrar la mejor coincidencia para cada nombre clave
mejor_coincidencia <- sapply(nombres_clave, function(clave) agrep(clave, nombres_columnas, ignore.case = TRUE))


coincidencias_validas <- unlist(mejor_coincidencia[sapply(mejor_coincidencia, function(x) length(x) > 0)])















