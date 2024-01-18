#------------------------------------------------------------------------------------------#
#                     SEGUNDA FUNCIÓN: MODELO 1: DIETA SUF EN ENERGÍA                      #
#-----------------------------------------------------------------------------------------#


CoCA=function(Datos_Insumo,EER,Filtrar_Alimentos=NULL){

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

#-------------------- PREPARACIÓN:

# VALIDAR SI EXISTE SEXO =0 EN EER

categorias_unicas <- unique(EER$Sexo)

if (!0 %in% categorias_unicas) {
  print("No se correrá para el modelo para el sexo masculino ya que la categoría 0 no está presente en la columna sexo del parametro EER.")
  Masculino=FALSE
} else {
  Masculino=TRUE
}


if(Masculino==TRUE) {


# Crear df de salida para el modelo 1 másculino

Salida_CoCA_M <- data.frame(Alimentos = Datos_Insumo$Alimento);Salida_CoCA_M <- Salida_CoCA_M %>% add_row(Alimentos = "Costo")

#-------------------- EXTRAER VECTORES DE PARÁEMTROS AL MODELO:

# Extraer df masculino

EER_M=subset(EER,Sexo==0)

# Asignación de vectores
Precio = Datos_Insumo$Precio_100g_ajust;Alimento=Datos_Insumo$Alimento;Edad=EER_M$Edad

# MAtriz de coef de restricción al modelo (ENERGIA)
Coef.Restriq = matrix(as.vector(Datos_Insumo$Energia), ncol = length(Alimento))

# Vector de limitaciones del modelo
Limitaciones=EER_M$Energia

#------------------------------Solución:

# Modelo

for( i in seq_along(Limitaciones)) {#ciclo para cada edad

CoCA = lp(direction = "min",
objective.in = Precio,
const.mat = Coef.Restriq,
const.dir = c("="),
const.rhs = Limitaciones[i],
compute.sens = TRUE)

# Guardar soluciones


# Crear dataframe df_1 con alimentos y la solución óptima para la edad actual
df_1 <- data.frame(Alimentos = Alimento, Valor = CoCA$solution);colnames(df_1) <- c("Alimentos", as.character(Edad[i]))

# Crear dataframe df_2 con el costo asociado para la edad actual
df_2 <- data.frame(Alimentos = "Costo", Valor = CoCA$objval);colnames(df_2) <- colnames(df_1);df_combinado <- rbind(df_1, df_2)

# Agregar la información al dataframe modelo_1
Salida_CoCA_M <- merge(Salida_CoCA_M, df_combinado, by = "Alimentos")

}



# DF sin ceros
DF_o <- Salida_CoCA_M[rowSums(Salida_CoCA_M[, -1]) != 0, ]
Costo=DF_o[DF_o=="Costo",];Costo=as.vector(t(Costo[Costo$Alimentos == "Costo", -1]))


# Indetificando el alimento
df_alimentos <- DF_o[DF_o$Alimentos != "Costo", ]

# Multiplicar los valores por 100
df_alimentos[, -1] <- df_alimentos[, -1] * 100

#ESTRUCTURA CIAT
df_transformado <- df_alimentos %>%
  pivot_longer(cols = -Alimentos, names_to = "Grupo_Etario", values_to = "Cantidad_G") %>%
  mutate(Alimento = Alimentos,
         Sexo = 0
         ) %>%
  select(Alimento, Cantidad_G, Grupo_Etario, Sexo)


CoCA_M=cbind(df_transformado,Costo)

} else {
   CoCA_M=NULL
}



#------------------------------------------------------------------------------------------#
#                       QUINTA ETAPA: MODELO 1 FEMENINO                                   #
#-----------------------------------------------------------------------------------------#




if (!1 %in% categorias_unicas) {
  print("No se correrá para el modelo para el sexo femenino ya que la categoría 1 no está presente en la columna sexo del parametro EER.")
  Femenino=FALSE
} else {
  Femenino=TRUE
}


if(Femenino==TRUE) {


# Crear df de salida para el modelo 1 másculino

Salida_CoCA_F <- data.frame(Alimentos = Datos_Insumo$Alimento);Salida_CoCA_F <- Salida_CoCA_F %>% add_row(Alimentos = "Costo")

#-------------------- EXTRAER VECTORES DE PARÁEMTROS AL MODELO:

# Extraer df Femenino

EER_F=subset(EER,Sexo==1)

# Asignación de vectores
Precio = Datos_Insumo$Precio_100g_ajust;Alimento=Datos_Insumo$Alimento;Edad=EER_F$Edad

# MAtriz de coef de restricción al modelo (ENERGIA)
Coef.Restriq = matrix(as.vector(Datos_Insumo$Energia), ncol = length(Alimento))

# Vector de limitaciones del modelo
Limitaciones=EER_F$Energia

#------------------------------Solución:

# Modelo

for( i in seq_along(Limitaciones)) {#ciclo para cada edad

CoCA = lp(direction = "min",
objective.in = Precio,
const.mat = Coef.Restriq,
const.dir = c("="),
const.rhs = Limitaciones[i],
compute.sens = TRUE)

# Guardar soluciones


# Crear dataframe df_1 con alimentos y la solución óptima para la edad actual
df_1 <- data.frame(Alimentos = Alimento, Valor = CoCA$solution);colnames(df_1) <- c("Alimentos", as.character(Edad[i]))

# Crear dataframe df_2 con el costo asociado para la edad actual
df_2 <- data.frame(Alimentos = "Costo", Valor = CoCA$objval);colnames(df_2) <- colnames(df_1);df_combinado <- rbind(df_1, df_2)

# Agregar la información al dataframe modelo_1
Salida_CoCA_F <- merge(Salida_CoCA_F, df_combinado, by = "Alimentos")

}


# DF sin ceros
DF_o <- Salida_CoCA_F[rowSums(Salida_CoCA_M[, -1]) != 0, ]
Costo=DF_o[DF_o=="Costo",];Costo=as.vector(t(Costo[Costo$Alimentos == "Costo", -1]))


# Indetificando el alimento
df_alimentos <- DF_o[DF_o$Alimentos != "Costo", ]

# Multiplicar los valores por 100
df_alimentos[, -1] <- df_alimentos[, -1] * 100

#ESTRUCTURA CIAT
df_transformado <- df_alimentos %>%
  pivot_longer(cols = -Alimentos, names_to = "Grupo_Etario", values_to = "Cantidad_G") %>%
  mutate(Alimento = Alimentos,
         Sexo = 1
         ) %>%
  select(Alimento, Cantidad_G, Grupo_Etario, Sexo)


CoCA_F=cbind(df_transformado,Costo)

} else {
   CoCA_F=NULL
}
# Salida en el ambiente global

if (!is.null(CoCA_F) && !is.null(CoCA_M)) {
  # Unir los dataframes usando rbind
  CoCA <- rbind(CoCA_F, CoCA_M)
} else {
  # Asignar el dataframe que no es NULL a CoCA
  CoCA <- ifelse(!is.null(CoCA_F), CoCA_F, CoCA_M)
}

assign("CoCA_Result",CoCA,envir = globalenv())

#------------------------------------------------------------------------------------------#
#                       FIN DEL SEGUNDO MÓDULO COMO FUNCIÓN                                 #
#-----------------------------------------------------------------------------------------#

}

