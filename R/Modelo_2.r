#------------------------------------------------------------------------------------------#
#                    SEGUNDA FUNCIÓN: MODELO 2: DIETA ADEC EN NUTRIENTES                  #
#-----------------------------------------------------------------------------------------#


Modelo_2=function(Datos_Insumo,DRI_min,DRI_max){

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
#                   TERCERA ETAPA: CMODELO FEMENINO                                    #
#------------------------------------------------------------------------------------#

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

# DF de limitaciones en nutrientes
DRI_min_F_li= DRI_min_F %>% select(-any_of(c("Edad","Energia","Sexo")))
DRI_max_F_li= DRI_max_F %>% select(-any_of(c("Edad","Energia","Sexo")))

if (!identical(names(DRI_min_F_li), names(DRI_max_F_li))) {
  stop("Los dataframes no tienen las mismas columnas.")
}


DRI_min_F_li= DRI_min_F %>% select(-any_of(c("Edad","Sexo")))


# Exraer los nutrientes de entrada que son distintos a las columnas: ("Cod_TCAC", "Alimento", "Serving", "Precio_100g_ajust")
DF_Nutrientes_ALimentos <- Datos_Insumo %>% select(-any_of(c("Cod_TCAC", "Alimento", "Serving", "Precio_100g_ajust")))

Sin_EER= DF_Nutrientes_ALimentos %>% select(-Energia)
DF_Nutrientes_ALimentos=cbind(DF_Nutrientes_ALimentos,Sin_EER)
names(DF_Nutrientes_ALimentos) <- gsub("\\s", "", names(DF_Nutrientes_ALimentos))


# MAtriz de coef de restricción al modelo (ENERGIA y nutrientes)
Coef.Restriq=DF_Nutrientes_ALimentos %>% as.matrix() %>% t()



#signos de las restricciones
constr_signs = c("=", rep(">=", length(Edad)), rep("<=", length(Edad)))




#------------------------------Solución:

# Modelo

opt_sol = lp(direction = "min",
               objective.in = Precio,
               const.mat = Coef.Restriq,
               const.dir = constr_signs,
               const.rhs =as.vector(unlist(Limitaciones[1, , drop = FALSE])),
               compute.sens = TRUE)
 

#------------------------------------------------------------------------------------------#
#                       FIN DEL TERCER MÓDULO COMO FUNCIÓN                                 #
#-----------------------------------------------------------------------------------------#
}
}








