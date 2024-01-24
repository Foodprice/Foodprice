#------------------------------------------------------------------------------------------#
#                    SEGUNDA FUNCIÓN: MODELO 2: DIETA ADEC EN NUTRIENTES                  #
#-----------------------------------------------------------------------------------------#


CoNA=function(Datos_Insumo,DRI_min,DRI_max,Filtrar_Alimentos=NULL){

#------------------------------------------------------------------------------------------#
#                       PRIMERA ETAPA: VALIDACIÓN DE LIBRERIAS                             #
#-----------------------------------------------------------------------------------------#

Librerias_base = c("readxl","dplyr","ggplot2","reshape2","knitr","haven","foreign","stringi","labelled","tidyr","plyr","tidyverse",
                        "lpSolve","Rglpk","scatterplot3d","reshape","R6","rio","janitor","stringr") # Nombra las librerias necesarias

if (!require("pacman")) install.packages("pacman") # Paquete que simplifica la carga de librerias
pacman::p_load(char = Librerias_base);Librerias_base_print = paste0(paste0("'", Librerias_base, "'"), collapse = ", ") # Instala si es necesario, o en su defecto, sólo llama los paquetes

#cat("\n")
#cat("Se instalaron y cargaron todas la librerias corectamente")
#cat("\n")

#------------------------------------------------------------------------------------------#
#         SEGUNDA ETAPA: VALIDACIÓN DE PARÁMETROS OBLIGATORIOS Y OPCIONALES                #
#-----------------------------------------------------------------------------------------#

# Función para validar la presencia de columnas en un data frame
validate_columns <- function(df, required_columns, model_name) {
  if (!is.data.frame(df)) {
    stop(paste("Error:", model_name, "no es un data frame."))
  }
  
  if (ncol(df) <= length(required_columns)) {
    stop(paste("Error:", model_name, "debe tener al menos", length(required_columns), "columnas."))
  }
  
  missing_columns <- setdiff(required_columns, colnames(df))
  
  if (length(missing_columns) > 0) {
    stop(paste(model_name, "requiere las siguientes columnas en los datos de insumo:", paste(missing_columns, collapse = ", "), ". Por favor revise la documentación para conocer el nombre que deben tener las columnas necesarias al modelo."))
  }
}

# Validar Datos_Insumo
validate_columns(Datos_Insumo, c("Precio_100g_ajust", "Alimento", "Energia"), "Datos_Insumo")

# Validar DRI_min
validate_columns(DRI_min, c("Edad"), "DRI_min")

# Validar DRI_max
validate_columns(DRI_max, c("Edad"), "DRI_max")


# Verificar Filtrar_Alimentos
if (!is.null(Filtrar_Alimentos)) {
  if (!is.vector(Filtrar_Alimentos)) {
    stop("Error: El parámetro Filtrar_Alimentos debe ser un vector.")
  }
  Datos_Insumo <- Datos_Insumo[!(Datos_Insumo$Alimento %in% Filtrar_Alimentos), ]
}


#--------------------------------------------------------------------------------------#
#                       TERCERA ETAPA: MODELO 2                                        #
#------------------------------------------------------------------------------------#

# Verificar si existe la columna "Sexo"
if ("Sexo" %in% colnames(DRI_min)) {

  # Separar por sexo y obtener los nombres de sexo
  Sexos_min <- split(DRI_min, DRI_min$Sexo)
  Sexos_max <- split(DRI_max, DRI_max$Sexo)

  # Verificar si los vectores son iguales
  if (!identical(names(Sexos_min), names(Sexos_max))) {
    stop("Error: Los sexos en ambos requerimientos no son iguales")
  }

  # Seleccionar el primer nombre de sexo como referencia
  sexo_nombre <- names(Sexos_min)

} else {

  # Si no existe la columna "Sexo", establecer el nombre de sexo como "0"
  sexo_nombre <- "0"
  DRI_min_i <- DRI_min
  DRI_max_i <- DRI_max
}


#--------------------------------------------------------#
#               CLICLO PARA CADA SEXO                   #
#-------------------------------------------------------#
for (sexo_nombre in sexo_nombre) { 


# ------------ PREPARACIÓN DEL MODELO E IDENTI DE NUTRIENTES

if ("Sexo" %in% colnames(DRI_min)){
DRI_min_i <- Sexos_min[[sexo_nombre]]
DRI_max_i <- Sexos_max[[sexo_nombre]];DRI_max_i[is.na(DRI_max_i)] = 999999
}

# Organizar ambos df iguales

DRI_min_i <- arrange(DRI_min_i, Edad)
DRI_max_i <- arrange(DRI_max_i, Edad)

# Asignación de vectores
Precio = Datos_Insumo$Precio_100g_ajust;Alimento=Datos_Insumo$Alimento;Edad=DRI_min_i$Edad

# DF de limitaciones en nutrientes y validación de nombres
DRI_min_li= DRI_min_i %>% select(-any_of(c("Edad","Energia","Sexo")))
DRI_max_li= DRI_max_i %>% select(-any_of(c("Edad","Energia","Sexo")))

if (!identical(names(DRI_min_li), names(DRI_max_li))) {
  stop("Los datos DRI max y min no tienen los mismos nombres en la columnas.")
}

# selecionar de DRI_min energía
DRI_min_li= DRI_min_i %>% select(-any_of(c("Edad","Sexo")))
DRI_max_li= DRI_max_i %>% select(-any_of(c("Edad","Energia","Sexo")))


# Exraer los nutrientes de entrada que son distintos a las columnas: ("Cod_TCAC", "Alimento", "Serving", "Precio_100g_ajust")
DF_Nutrientes_ALimentos <- Datos_Insumo %>% select(-any_of(c("Cod_TCAC", "Alimento", "Serving", "Precio_100g_ajust")))



# Identificar las columnas que son iguales de datos insumo y requerimientos y ordenarlas
nombres_comunes <- intersect(names(DF_Nutrientes_ALimentos), names(DRI_min_li));

# Ordenar los nombrespara el modelo
DF_Nutrientes_ALimentos <- DF_Nutrientes_ALimentos %>% select(any_of(nombres_comunes));DRI_min_li <- DRI_min_li %>% select(any_of(nombres_comunes));  DRI_max_li <- DRI_max_li %>% select(any_of(nombres_comunes))


# Unir los nutrientes de aliemntos en min y max
Sin_EER= DF_Nutrientes_ALimentos %>% select(-Energia)
DF_Nutrientes_ALimentos=cbind(DF_Nutrientes_ALimentos,Sin_EER)


# Matriz de coef de restricción al modelo (ENERGIA y nutrientes)
Coef.Restriq=DF_Nutrientes_ALimentos %>% as.matrix() %>% t()


#signos de las restricciones
constr_signs = c("=", rep(">=", ncol(DRI_min_li)-1), rep("<=", length(DRI_max_li)))

#Unir los EER, minx y max
Limitaciones=cbind(DRI_min_li,DRI_max_li)



#------------------------------ Preparación de datos de resultados:
#DF de la solución de intercambios
Intercambios_CoNA <- data.frame(Alimento = character(), Cantidad_GR = numeric(), Grupo_demo = integer(), Sexo = integer())

#DF de la solución de csotos
Costo_T <- data.frame(Grupo_demo = integer(), Sexo = integer(), Costo_dia = numeric())

# df de nutrientes limitantes
N_limit=data.frame()

# DF de los precios sombra
S_shadow = na.omit(data.frame(Edad = NA,Sexo = NA,Nutrientes = NA,constraint = NA,value_constraint = NA,SP = NA, SPE = NA))
# ------------ -------------------- SOLUCIÓN DEL MODELO

for (i in seq_along(Edad)) { #ciclo para cada edad


CoNA = lp(direction = "min",
               objective.in = Precio,
               const.mat = Coef.Restriq,
               const.dir = constr_signs,
               const.rhs =as.vector(unlist(Limitaciones[i, , drop = FALSE])),
               compute.sens = TRUE)

#--------------------------------------------------------#
#               ETAPA DE ESTRUCTURA PLAZA                #
#-------------------------------------------------------#

# Guardar estructura de intercambios
costo <- sum(CoNA$solution * Precio)
Alimentos_sol <- which(CoNA$solution != 0) # ALimento selexionados
cantidades_intercambio <- CoNA$solution[Alimentos_sol] # intercambios
 
 
# Crear un dataframe temporal de la estructura CIAT
temp_df <- data.frame(Alimento = Alimento[Alimentos_sol],
Cantidad_GR = (cantidades_intercambio*100),
Grupo_demo = Edad[i],
Sexo = as.numeric(sexo_nombre))

# Agregar los resultados al dataframe general
Intercambios_CoNA <- merge(Intercambios_CoNA, temp_df, all = TRUE)

# Guardar estructura de costo

Costo_dia=sum(CoNA$solution * Precio)

# Agregar los resultados al dataframe Costo_CoNA
temp_df <- data.frame(Grupo_demo = Edad[i],
                          Sexo = as.numeric(sexo_nombre),
                          Costo_dia = sum(CoNA$solution * Precio),
                          Costo_1000kcal=(CoNA$objval/as.vector(unlist(Limitaciones[i, , drop = FALSE])[1])*1000))

# Agregar los resultados al dataframe general

Costo_T=rbind(Costo_T, temp_df)

  #------------------------------------#
  #       NUTRIENTES LIMITANTES        #
  #------------------------------------#


  Nutrie_limit <- data.frame(
    Nutrientes = rownames(Coef.Restriq[1:length(names(DRI_min_li)),] %*% as.matrix(CoNA$solution)), # se asumen nutrientes sin EER y sin repetir
    Opt = as.numeric(Coef.Restriq[1:length(names(DRI_min_li)),] %*% as.matrix(CoNA$solution))
  ) %>%  
    mutate(Rest = as.matrix(as.vector(unlist(DRI_min_li[i, , drop = FALSE]))),
           Diff = round(((Opt - Rest) / Rest * 100),2),
           Limiting = ifelse(Diff == 0, 1, 0),
           Edad = Edad[i],
           Sexo = as.numeric(sexo_nombre)) %>% filter(Nutrientes != "Energia") 
  N_limit = rbind(N_limit, Nutrie_limit)

  #------------------------------------#
  #       PRECIOS SOMBRA Y ELAST       #
  #------------------------------------#

 Spe <- data.frame(
    Edad = Edad[i],
    Sexo = as.numeric(sexo_nombre),
    Nutrientes = names(Limitaciones),
    constraint = constr_signs,
    value_constraint = as.vector(unlist(Limitaciones[i, , drop = FALSE])),
    SP = NA,
    SPE = NA
  ) %>%
    mutate(SP = CoNA$duals[1:length(constr_signs)],
           SPE = ((SP/1) * (unlist(Limitaciones[i, , drop = FALSE])/CoNA$objval)) * 100)
  # Añadir a output4
  S_shadow <- rbind(S_shadow, Spe)
  

} #FIN DEL CICLO EN EDAD


# Asignaciones por sexo
assign(paste("CoNA_", sexo_nombre, sep = ""), Costo_T)
assign(paste("Intercambios_CoNA_", sexo_nombre, sep = ""), Intercambios_CoNA)

# asginaciones nutrientes limitantes por sexo
assign(paste("N_limit_", sexo_nombre, sep = ""), N_limit)

# asignaicoens precios sombra por sexo
assign(paste("S_shadow_", sexo_nombre, sep = ""), S_shadow)
#--------------------------------------------------------#
#        FIND DEL       CLICLO PARA CADA SEXO            #
#-------------------------------------------------------#

}


nombres_comunes_sin_energia <- setdiff(nombres_comunes, "Energia")
#cat("\n")
#cat("Los nutrientes a usar en el modelo son:", paste(nombres_comunes_sin_energia, collapse = ", "), "\n")
#cat("\n")

# Unir ambos df para cada sexo (si existe)
if ("Sexo" %in% colnames(DRI_min)) {

Costo_CoNA=rbind(CoNA_0,CoNA_1)
Alimentos_CoNA=rbind(Intercambios_CoNA_0,Intercambios_CoNA_1)
CoNA_N_Limit=rbind(N_limit_0,N_limit_1) 
CoNA_SP=rbind(S_shadow_0,S_shadow_1);CoNA_SP = CoNA_SP %>% filter(constraint == ">=")
CoNA_SP = CoNA_SP[c("Edad", "Sexo", "Nutrientes", "SP", "SPE")]
  } else {

   Costo_CoNA <- CoNA_0 %>%
  select(-Sexo)
  Alimentos_CoNA<- Intercambios_CoNA_0 %>%
  select(-Sexo)
  CoNA_N_Limit<- N_limit_0 %>%
  select(-Sexo)
    CoNA_SP<- S_shadow_0 %>%
  select(-Sexo)

  CoNA_SP = CoNA_SP %>% filter(constraint == ">=")
CoNA_SP = CoNA_SP[c("Edad", "Nutrientes", "SP", "SPE")]
}

#Nutrientes limtantes y precios sombre
CoNA_SP_LM = merge(CoNA_SP, CoNA_N_Limit,by.x = c("Edad", "Sexo", "Nutrientes"),
        by.y= c("Edad", "Sexo", "Nutrientes"))

# Asignación en el ambiente global
#assign("Costo_CoNA",Costo_CoNA,envir = globalenv()) 
#assign("Alimentos_CoNA",Alimentos_CoNA,envir = globalenv()) 
#assign("CoNA_SP_LM",CoNA_SP_LM,envir = globalenv()) 

#------------------------------------------------------------------------------------------#
#                       FIN DEL TERCER MÓDULO COMO FUNCIÓN                               #
#-----------------------------------------------------------------------------------------#
  #----------------------------#
  #     ASGINACIONES DE LISTA  #
  #----------------------------#
  
  List_CoNA=list(Costo_CoNA,Alimentos_CoNA,CoNA_SP_LM);names(List_CoNA)=c("Costo_CoNA","Alimentos_CoNA","CoNA_SP_LM")
  
  # retorno
  
  cat("✔ CoNA")
  return(invisible(List_CoNA))


#cat("\n")
#cat("Ejecución del modelo: 'COSTO DIARIO A UNA DIETA ADECUADA EN NUTRIENTES (CoNA)' correcta") 
#cat("\n")

}

