
#------------------------------------------------------------------------------------------#
#                    CUARTA FUNCIÓN: MODELO 3: DIETA SALUDABLE                             #
#-----------------------------------------------------------------------------------------#


CoRD=function(Datos_Insumo,Req_Int,Cantidad_selec,Filtrar_Alimentos=NULL){


#------------------------------------------------------------------------------------------#
#                       PRIMERA ETAPA: VALIDACIÓN DE LIBRERIAS                             #
#-----------------------------------------------------------------------------------------#

Librerias_base = c("readxl","dplyr","ggplot2","reshape2","knitr","haven","foreign","stringi","labelled","tidyr","plyr","tidyverse",
                        "lpSolve","Rglpk","scatterplot3d","reshape","R6","rio","janitor","stringr") # Nombra las librerias necesarias

if (!require("pacman")) install.packages("pacman") # Paquete que simplifica la carga de librerias
pacman::p_load(char = Librerias_base);Librerias_base_print = paste0(paste0("'", Librerias_base, "'"), collapse = ", ") # Instala si es necesario, o en su defecto, sólo llama los paquetes


# Instala paquetes individualmente si no se han cargado correctamente
paquetes_faltantes <- Librerias_base[!(Librerias_base %in% pacman::p_loaded())]
for (paquete in paquetes_faltantes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}


#cat("\n")
#cat("Se instalaron y cargaron todas la librerias corectamente")
#cat("\n")

#------------------------------------------------------------------------------------------#
#         SEGUNDA ETAPA.1: VALIDACIÓN DE PARÁMETROS OBLIGATORIOS Y OPCIONALES                #
#-----------------------------------------------------------------------------------------#
 #-------------- VERIFICACIÓN DE DATOS DE INSUMO

  # Verificar si Datos_Insumo es un data frame
  if (!is.data.frame(Datos_Insumo)) {
    stop("Datos_Insumo no es un data frame.")
  }
  names(Datos_Insumo)
  # Verificar si tiene al menos 3 columnas
  if (ncol(Datos_Insumo) < 4) {
    stop("Datos_Insumo debe tener al menos 4 columnas.")
  }
required_columns <- c("Precio_100g_ajust" , "Intercambio_EER_gr" ,"Precio_INT","Grupo")
missing_columns <- setdiff(required_columns, colnames(Datos_Insumo))

if (length(missing_columns) > 0) {
  stop(paste("El modelo CoRD requiere las siguientes columnas en los datos de insumo: ", paste(missing_columns, collapse = ", "),". Por favor revise la documentación para conocer el nombre que deben tener las columnas necesarias al primer modelo"))}


# -------------- VERIFICACIÓN DE REQ


  # Verificar si Datos_Insumo es un data frame
  if (!is.data.frame(Req_Int)) {
    stop("Datos_Insumo no es un data frame.")
  }
  
  # Verificar si tiene al menos 3 columnas
  if (ncol(Req_Int) < 3) {
    stop("Los requerimientos para el modelo CoRD deben contener al menos 3 columnas.")
  }

required_columns_E <- c("Grupo"  , "Edad"  ,  "Porción")
missing_columns_E <- setdiff(required_columns_E, colnames(Req_Int))

if (length(missing_columns_E) > 0) {
  stop(paste("El modelo CoRD requiere las siguientes columnas en el parámetro 'Req_Int': ", paste(missing_columns_E, collapse = ", "),". Por favor revise la documentación para conocer el nombre que deben tener las columnas necesarias al primer modelo"))}


# -------------- VERIFICACIÓN DE Cantidad_selec


  # Verificar si Datos_Insumo es un data frame
  if (!is.data.frame(Cantidad_selec)) {
    stop("Cantidad_selec no es un data frame.")
  }
  
  # Verificar si tiene al menos 3 columnas
  if (ncol(Cantidad_selec) < 2) {
    stop("La cantidad de grupos a selecionar para el modelo CoRD deben contener al menos 2 columnas.")
  }

required_columns_E <- c("Grupo","Cantidad")
missing_columns_E <- setdiff(required_columns_E, colnames(Cantidad_selec))

if (length(missing_columns_E) > 0) {
  stop(paste("El modelo CoRD requiere las siguientes columnas en el parámetro 'Cantidad_selec': ", paste(missing_columns_E, collapse = ", "),". Por favor revise la documentación para conocer el nombre que deben tener las columnas necesarias al primer modelo"))}


# -------------- VALIDACIÓN DE Filtrar_Alimentos

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
#                       SEGUNDA ETAPA: FILTRAR RECOMENDACIONES                            #
#-----------------------------------------------------------------------------------------#



#se excluyen los alimentos sin categorías
Datos_Insumo = Datos_Insumo %>% filter(!Grupo %in% "Sin categoría")


# Definir códigos y alimentos a eliminar
codigos_a_eliminar <- c("L017", "D020", "K018", "K018")
alimentos_a_eliminar <- c("Carne de cerdo, espinazo", "Yuca ICA", "Papa Betina", "Papa única")

# Filtrar el dataframe
if ("Cod_TCAC" %in% colnames(Datos_Insumo)){
Datos_Insumo <- Datos_Insumo %>%
  filter(!(Cod_TCAC %in% codigos_a_eliminar) & !(Alimento %in% alimentos_a_eliminar))}

#--------------------------------------------------------------------------------------------------------------------#
#                       TERCER  ETAPA: SELECCIÓN Y VALDIACIÓN DE GRUPOS EN MODELO FEMENINO                      #
#------------------------------------------------------------------------------------------------------------------#

# Verificar si existe la columna "Sexo"

# Extraer sexos disponibles si existe la columna sexo
if ("Sexo" %in% colnames(Req_Int)) {Sexos <- split(Req_Int, Req_Int$Sexo);sexo_nombre=names(Sexos)} else {
   sexo_nombre=0

}

Alimento=Datos_Insumo$ALimento
Precio=Datos_Insumo$Precio_INT
#--------------------------------------------------------#
#               CLICLO PARA CADA SEXO                   #
#-------------------------------------------------------#

for (sexo_nombre in sexo_nombre) { 

if ("Sexo" %in% colnames(Req_Int)) {Req_Int <- Sexos[[sexo_nombre]]}

# Requerimiento y edad
Req_Int_i=Req_Int
Edad=levels(as.factor(Req_Int_i$Edad))


#---------------- VALIDACIÓN Y SELECIÓN DE GRUPOS----------------

# Extraer grupos
Grupos_Insumo=levels(as.factor(Datos_Insumo$Grupo)) # GRupos de insumo
Grupos_Cantidad_Sel <- unique(Cantidad_selec$Grupo) #grupos de cantidad
grupos_req=levels(as.factor(Req_Int_i$Grupo))
#-------------------
# 1. Validar que la cantidad de grupos en el vector sea mayor que 5
if (length(Grupos_Insumo) <= 5) {
  stop("Error: La cantidad de grupos en el vector debe ser mayor que 5.")
}


# 2. Identificar los grupos que son iguales de datos insumo y cantidad a selecionar
Grupos_comunes <- intersect(Grupos_Insumo, Grupos_Cantidad_Sel)

# Grupos no comunes
grupos_faltantes <- union(setdiff(Grupos_Insumo, Grupos_Cantidad_Sel),setdiff(Grupos_Cantidad_Sel,Grupos_Insumo))



if(length(grupos_faltantes)>0){
paste("Cuidado: Hay grupos no comunes entre los grupos de datos insumo y la cantidad a selecionar de estos:",paste(grupos_faltantes,collapse = ", "))
}



if(length(grupos_faltantes)>0){
paste("Se trabajará entonces sólo con los grupos iguales, estos son:",paste(Grupos_comunes,collapse = ", "))
}

# Validar la intersección entre grupos de req y los demás
Grupos_comunes_req <- intersect(Grupos_comunes, grupos_req)

grupos_faltantes_req=union(setdiff(Grupos_comunes, grupos_req),setdiff(grupos_req,Grupos_comunes))

if(length(grupos_faltantes_req)>0){
paste("Cuidado: Hay grupos no comunes entre los grupos de datos insumo, la cantidad a selecionar y los requerimientos, estos son:",paste(grupos_faltantes_req,collapse = ", "))
}

if (length(Grupos_comunes_req)<5){stop("Cuidado: Los grupos comunes entre los requerimientos, Datos de insumo y cantidad a selecionar son muy pocos, deben ser mayor a 5")}

if(length(grupos_faltantes_req)>0){
paste("Se trabajará entonces sólo con los grupos iguales en los tres vectores de grupos, estos son:",paste(Grupos_comunes_req,collapse = ", "))
}

# Cantidad a selcionar, sólo los comunes
Grupos_finales <- subset(Cantidad_selec, Grupo %in% Grupos_comunes_req)

#--------------------------------------------------------------------------------------------------------------------#
#                       CUARTA ETAPA : MODELO      3                                                                #
#------------------------------------------------------------------------------------------------------------------#



Generar_A <- function(n,df) { # n- cantidad alimentos y df- es el dt con Intercambio_EER_gr


# Validar si están las cantidades necesarias
if (nrow(df)<n){
stop(paste("Para el grupo",paste(Grupo_i,collapse = ", ")),paste(" hay menos de " ),paste(n,collapse = ", "),paste(" alimentos."))
}


A <- matrix(0, nrow = n, ncol = n)
for (j in 1:n-1){
A[j,j]=df$Intercambio_EER_gr[j] # Asignar diagonales iguales
A[j,j+1]=-df$Intercambio_EER_gr[j+1] # Asignas diagonal siguiente negativo
}
  A[n,]=1 # Asignar 1 al final
  return(A)
}



# Función para generar entrada B al sistemad de ecuaciones
Generar_B=function(n){

  Ceros=rep(0,n-1)
  B=c(Ceros,Req_i_g$Porción)
  return(B)

}




CoRD_INT <- data.frame()  # DATA DE SALIDA
Edad_CoRD <- c()  # Edad de salida
CoRD_COST <- data.frame() # DF de los costos

# -------------------------------- EXTRACCIONES

# Extraer reque según la edad
for (i in 1:length(Edad)) {
  
  # Extraer req por edad
  E_i=Edad[i]
  Req_i = subset(Req_Int_i, Edad == E_i)
  
  for (j in 1:nrow(Grupos_finales)) {
    
    # Extraer el grupo y la cantidad a seleccionar
    Grupo_i = Grupos_finales$Grupo[j]  # Usar j para el bucle interior
    Cantidad_i = Grupos_finales$Cantidad[j]  # Usar j para el bucle interior
    
    # Extraer y requerimientos datos por grupo
    Datos_grupo_i = subset(Datos_Insumo, Grupo == Grupo_i)
    Datos_grupo_i = Datos_grupo_i[order(Datos_grupo_i$Precio_INT), ]
    Req_i_g = subset(Req_i, Grupo == Grupo_i)
    
    # Dejar columnas útiles en Datos_grupo_i
    if (Cantidad_i == 0) {
      stop("La cantidad de elementos a seleccionar debe ser un entero mayor que cero")
    }
    
    Datos_grupo_i = Datos_grupo_i %>% select(any_of(c("Alimento", "Precio_100g_ajust", "Intercambio_EER_gr", "Precio_INT", "Grupo")))
    Datos_grupo_i = Datos_grupo_i[1:Cantidad_i, ]
    
    # Función para generar entrada A al sistema de ecuaciones
    
    # Generar entrada matrix de coeficientes al sistema
    A = Generar_A(Cantidad_i, Datos_grupo_i)
    
    # generar igualdades del sistema
    B = Generar_B(Cantidad_i)
    
    # ----------------- MODELO FEMENINO-----------------
    
    # Soluciones por INT y gr

  
    Cantidad_INT = solve(A, B)

    Cantidad_g = Cantidad_INT * Datos_grupo_i$Intercambio_EER_gr
    
    # Crear dataframe directamente y agregar las filas a CoRD_INT_F
    CoRD_F = cbind(Datos_grupo_i, Cantidad_INT = Cantidad_INT, Cantidad_g = Cantidad_g,Edad=Edad[i],Sexo=sexo_nombre)
    
    CoRD_INT = rbind(CoRD_INT, CoRD_F)



  }
  
#SALIDA CON EDAD 
#Edad_i=rep(Edad[i],sum(Cantidad_selec$Cantidad));Edad_CoRD=c(Edad_CoRD,Edad_i)

}




#------------------ CÁLCULO DEL COSTO POR EDAD

for (E in Edad) {
  # Filtrar el dataframe por edad
  df_edad <- subset(CoRD_INT, Edad == E)

  # Calcular el costo para la edad actual
  costo_edad <- sum(df_edad$Precio_INT * df_edad$Cantidad_INT)

  # Crear un dataframe temporal para la edad actual
  df_temp <- data.frame(Edad = E, Costo = costo_edad)

  # Agregar el dataframe temporal a costo
  CoRD_COST <- rbind(CoRD_COST, df_temp)
};CoRD_COST$Sexo=as.numeric(sexo_nombre)



# ----------- ESTRUCTURA CIAT PARA INTERCAMBIOS
CoRD_INT= CoRD_INT %>% select(any_of(c("Alimento","Grupo","Cantidad_INT","Edad","Sexo")))

# Asignaciones por sexo
assign(paste("CoRD_", sexo_nombre, sep = ""), CoRD_COST)
assign(paste("Intercambios_CoRD_", sexo_nombre, sep = ""), CoRD_INT)

}

#--------------------------------------------------------#
#        FIND DEL       CLICLO PARA CADA SEXO            #
#-------------------------------------------------------#


# Unir ambos df para cada sexo (si existe)
if ("Sexo" %in% colnames(Req_Int)) {

Costo_CORD=rbind(CoRD_0,CoRD_1)
Intercambios_CoRD=rbind(Intercambios_CoRD_0,Intercambios_CoRD_1)
  
  } else {

  Costo_CORD <- CoRD_0 %>%
  select(-Sexo)
  Intercambios_CoRD<- Intercambios_CoRD__0 %>%
  select(-Sexo)
}


# Asignación en el ambiente global
#assign("Costo_CoRD",Costo_CORD,envir = globalenv()) 
#assign("Intercambios_CoRD",Intercambios_CoRD,envir = globalenv()) 

  #----------------------------#
  #     ASGINACIONES DE LISTA  #
  #----------------------------#
  
  List_CoRD=list(Costo_CORD,Intercambios_CoRD,Precio,Alimento);names(List_CoRD)=c("Costo_CoRD","Intercambios_CoRD","Precio","Alimento")
  
  # retorno
  
  cat("✔ CoRD")
  return(invisible(List_CoRD))

#cat("\n") 
#cat("Ejecución del modelo: 'COSTO DIARIO A UNA DIETA SALUDABLE (CoRD)' correcta")
#cat("\n") 
}


