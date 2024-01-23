

#---------------------------------------------------------------------------------------#
#                            QUINTA ETAPA: TERCER   MODELO FEMENINO                    #
#-------------------------------------------------------------------------------------#


CoRD=function(Datos_Insumo,Req_Int,Cantidad_selec){


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
#         SEGUNDA ETAPA.1: VALIDACIÓN DE PARÁMETROS OBLIGATORIOS Y OPCIONALES                #
#-----------------------------------------------------------------------------------------#








#------------------------------------------------------------------------------------------#
#                       SEGUNDA ETAPA: FILTRAR RECOMENDACIONES                            #
#-----------------------------------------------------------------------------------------#

#se excluyen los alimentos sin categorías
Datos_Insumo = Datos_Insumo %>% filter(!Grupo %in% "Sin categoría")


# Definir códigos y alimentos a eliminar
codigos_a_eliminar <- c("L017", "D020", "K018", "K018")
alimentos_a_eliminar <- c("Carne de cerdo, espinazo", "Yuca ICA", "Papa Betina", "Papa única")

# Filtrar el dataframe
Datos_Insumo <- Datos_Insumo %>%
  filter(!(Cod_TCAC %in% codigos_a_eliminar) & !(Alimento %in% alimentos_a_eliminar))

#--------------------------------------------------------------------------------------------------------------------#
#                       TERCER  ETAPA: SELECCIÓN Y VALDIACIÓN DE GRUPOS EN MODELO FEMENINO                      #
#------------------------------------------------------------------------------------------------------------------#

#VALIDAR SI EXISTE FEMENINO
categorias_unicas <- unique(Req_Int$Sexo)

if (!1 %in% categorias_unicas) {
  print("No se correrá para el modelo para el sexo masculino ya que la categoría 0 no está presente en la columna sexo del parametro Req_Int.")
  Femenino=FALSE
} else {
  Femenino=TRUE
}

if(Femenino==TRUE) {

# Requerimiento y edad
Req_Int_F=subset(Req_Int,Sexo==1)
Edad=levels(as.factor(Req_Int_F$Edad))


#---------------- VALIDACIÓN Y SELECIÓN DE GRUPOS----------------

# Extraer grupos
Grupos_Insumo=levels(as.factor(Datos_Insumo$Grupo)) # GRupos de insumo
Grupos_Cantidad_Sel <- unique(Cantidad_selec$Grupo) #grupos de cantidad
grupos_req=levels(as.factor(Req_Int_F$Grupo))
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
#                       CUARTA ETAPA : MODELO FEMENINO                                                                 #
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




CoRD_INT_F <- data.frame()  # DATA DE SALIDA
Edad_CoRD <- c()  # Edad de salida
CoRD_COST_F <- data.frame() # DF de los costos
# -------------------------------- EXTRACCIONES

# Extraer reque según la edad
for (i in 1:length(Edad)) {
  
  # Extraer req por edad
  E_i=Edad[i]
  Req_i = subset(Req_Int_F, Edad == E_i)
  
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
    CoRD_F = cbind(Datos_grupo_i, Cantidad_INT = Cantidad_INT, Cantidad_g = Cantidad_g)
    
    CoRD_INT_F = rbind(CoRD_INT_F, CoRD_F)



  }
  
#SALIDA CON EDAD 
Edad_i=rep(Edad[i],sum(Cantidad_selec$Cantidad));Edad_CoRD=c(Edad_CoRD,Edad_i)

}


CoRD_INT_F$Edad=Edad_CoRD # Cálculo de edad


#------------------ CÁLCULO DEL COSTO POR EDAD

for (E in Edad) {
  # Filtrar el dataframe por edad
  df_edad <- subset(CoRD_INT_F, Edad == E)

  # Calcular el costo para la edad actual
  costo_edad <- sum(df_edad$Precio_INT * df_edad$Cantidad_INT)

  # Crear un dataframe temporal para la edad actual
  df_temp <- data.frame(Edad = E, Costo = costo_edad)

  # Agregar el dataframe temporal a costo
  CoRD_COST_F <- rbind(CoRD_COST_F, df_temp)
};CoRD_COST_F$Sexo=1



# ----------- ESTRUCTURA CIAT PARA INTERCAMBIOS
CoRD_INT_F$Sexo=1;CoRD_INT_F= CoRD_INT_F %>% select(any_of(c("Alimento","Grupo","Cantidad_INT","Edad","Sexo")))

}else {
CoRD_INT_F=NULL
CoRD_COST_F=NULL

}





if (!0 %in% categorias_unicas) {
  print("No se correrá para el modelo para el sexo masculino ya que la categoría 0 no está presente en la columna sexo del parametro Req_Int.")
  Masculino=FALSE
} else {
  Masculino=TRUE
}

if(Masculino==TRUE) {

#------------------------------------------------------------------------------------------#
#                       TERCERA ETAPA: MODELO   MASCULINO                                   #
#-----------------------------------------------------------------------------------------#

# Requerimiento y edad
Req_Int_M=subset(Req_Int,Sexo==0)
Edad=levels(as.factor(Req_Int_M$Edad))


#---------------- VALIDACIÓN Y SELECIÓN DE GRUPOS----------------

# Extraer grupos
Grupos_Insumo=levels(as.factor(Datos_Insumo$Grupo)) # GRupos de insumo
Grupos_Cantidad_Sel <- unique(Cantidad_selec$Grupo) #grupos de cantidad
grupos_req=levels(as.factor(Req_Int_M$Grupo))
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
#                       CUARTA ETAPA : MODELO FEMENINO                                                                 #
#------------------------------------------------------------------------------------------------------------------#



CoRD_INT_M <- data.frame()  # DATA DE SALIDA
Edad_CoRD <- c()  # Edad de salida
CoRD_COST_M <- data.frame() # DF de los costos
# -------------------------------- EXTRACCIONES

# Extraer reque según la edad
for (i in 1:length(Edad)) {
  
  # Extraer req por edad
  E_i=Edad[i]
  Req_i = subset(Req_Int_M, Edad == E_i)
  
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
    # Generar entrada matrix de coeficientes al sistema
    A = Generar_A(Cantidad_i, Datos_grupo_i)
    
    # generar igualdades del sistema
    B = Generar_B(Cantidad_i)
    
    # ----------------- MODELO FEMENINO-----------------
    
    # Soluciones por INT y gr
    Cantidad_INT = solve(A, B)
    Cantidad_g = Cantidad_INT * Datos_grupo_i$Intercambio_EER_gr
    
    # Crear dataframe directamente y agregar las filas a CoRD_INT_M
    CoRD_M = cbind(Datos_grupo_i, Cantidad_INT = Cantidad_INT, Cantidad_g = Cantidad_g)
    
    CoRD_INT_M = rbind(CoRD_INT_M, CoRD_M)



  }
  
#SALIDA CON EDAD 
Edad_i=rep(Edad[i],sum(Cantidad_selec$Cantidad));Edad_CoRD=c(Edad_CoRD,Edad_i)

}


CoRD_INT_M$Edad=Edad_CoRD # Cálculo de edad


#------------------ CÁLCULO DEL COSTO POR EDAD

for (E in Edad) {
  # Filtrar el dataframe por edad
  df_edad <- subset(CoRD_INT_M, Edad == E)

  # Calcular el costo para la edad actual
  costo_edad <- sum(df_edad$Precio_INT * df_edad$Cantidad_INT)

  # Crear un dataframe temporal para la edad actual
  df_temp <- data.frame(Edad = E, Costo = costo_edad)

  # Agregar el dataframe temporal a costo
  CoRD_COST_M <- rbind(CoRD_COST_M, df_temp)
};CoRD_COST_M$Sexo=0



# ----------- ESTRUCTURA CIAT PARA INTERCAMBIOS
CoRD_INT_M$Sexo=0;CoRD_INT_M= CoRD_INT_M %>% select(any_of(c("Alimento","Grupo","Cantidad_INT","Edad","Sexo")))

} else {
CoRD_INT_M=NULL
CoRD_COST_M=NULL

}

 #Salida en el ambiente global

if (!is.null(CoRD_COST_M) && !is.null(CoRD_COST_F)) {
  # Unir los dataframes usando rbind
  Costo_CoRD <- rbind(CoRD_COST_M, CoRD_COST_F)
  CoRD_INT_M=rbind(CoRD_INT_M,CoRD_INT_F)
} else {
  # Asignar el dataframe que no es NULL a CoCA
  Costo_CoRD <- ifelse(!is.null(CoRD_COST_F), CoRD_COST_F, CoRD_COST_M)
  CoRD_INT_M <- ifelse(!is.null(CoRD_INT_F), CoRD_INT_F, CoRD_INT_M)
}

assign("Costo_CoRD",Costo_CoRD,envir = globalenv());assign("Inter_CoRD",CoRD_INT_M,envir = globalenv())

if(length(warnings())<100) {print ("Ejecución del modelo: 'COSTO DIARIO A UNA DIETA SALUDABLE (CoRD)' correcta") }

}




remove.packages("Foodprice")
devtools::install_github("Foodprice/Foodprice");library(Foodprice)

CoRD(Datos_Insumo=Datos_Prueba,Req_Int=Req_Int,Cantidad_selec=Cantidad_selec)
View(Costo_CoRD)
View(Inter_CoRD)
