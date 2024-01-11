#------------------------------------------------------------------------------------------#
#                    SEGUNDA FUNCIÓN: MODELO 2: DIETA ADEC EN NUTRIENTES                  #
#-----------------------------------------------------------------------------------------#


Modelo_2=function(Datos_Insumo,DRI_M_OP=NULL,DRI_F_OP=NULL){

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
  if (ncol(Datos_Insumo) <= 19) {
    stop("Error: Datos_Insumo debe tener al menos 19 columnas.")
  }

required_columns2 <- c("Precio_100g_ajust", "Alimento", "Energia","Proteina", "Lipidos", "Carbohidratos", "VitaminaC", "Folatos", "VitaminaA",
         "Tiamina", "Riboflavina", "Niacina", "VitaminaB12", "Magnesio", "Fosforo", "Sodio",
         "Calcio", "Hierro", "Zinc")

missing_columns2 <- setdiff(required_columns2, colnames(Datos_Insumo))

if (length(missing_columns2) > 0) {
  stop(paste("El modelo 2 requiere las siguientes columnas en los datos de insumo:", paste(missing_columns2, collapse = ", "),". Por favor revise la documentación para conocer el nombre que deben tener las columnas necesarias al segundo modelo"))}

# -------------- VERIFICACIÓN DE DRI_M_OP Y F

  if (!is.null(DRI_M_OP) || !is.null(DRI_F_OP)) {

  # Verificar la existencia de los dataframes DRI_M_OP y DRI_F_OP
 if (!is.null(DRI_M_OP) && !is.data.frame(DRI_M_OP)) {
    stop("DRI_M_OP no es un data frame.")
  }

      if((!is.null(DRI_M_OP) && !ncol(DRI_M_OP)>=17)){ stop("DRI_M_OP no es un data frame de diecisiete columnas.")}

  if (!is.null(DRI_F_OP) && !is.data.frame(DRI_F_OP)) {
    stop("DRI_F_OP no es un data frame de al menos dos columnas.")
    
  }
    if((!is.null(DRI_M_OP) && !ncol(DRI_M_OP)>=17)){ stop("DRI_M_OP no es un data frame de al menos diecisiete columnas.")}


if(!is.null(DRI_M_OP)){

required_columns <- c("edad","energia","proteina_l", "grasa_l","cho_l", "vit_c_l", "folato_l",
          "vit_a_l", "tiamina_l", "ribo_l",
          "niacina_l", "b12_l", "mg_l", "p_l",
          "na_l", "ca_l", "fe_l", "zinc_l")

missing_columns <- setdiff(required_columns, colnames(DRI_M_OP))

if (length(missing_columns) > 0) {
  stop(paste("Los datos de requerimientos del modelo 1  requiere las siguientes columnas: ", paste(missing_columns, collapse = ", "),". Por favor revise la docuentación para conocer el nombre que deben tener las columnas necesarias al primer modelo"))
} 
  }


  if(!is.null(DRI_F_OP)){
required_columns <- c("edad","energia","proteina_l", "grasa_l","cho_l", "vit_c_l", "folato_l",
          "vit_a_l", "tiamina_l", "ribo_l",
          "niacina_l", "b12_l", "mg_l", "p_l",
          "na_l", "ca_l", "fe_l", "zinc_l")

missing_columns <- setdiff(required_columns, colnames(DRI_F_OP))

if (length(missing_columns) > 0) {
  stop(paste("Los datos de requerimientos del modelo 1  requiere las siguientes columnas: ", paste(missing_columns, collapse = ", "),". Por favor revise la docuentación para conocer el nombre que deben tener las columnas necesarias al primer modelo"))
} 
  
  }}

#------------------------------------------------------------------------------------------#
#                               TERCERA ETAPA: CARGA DE REQUERIMIENTOS                    #
#-----------------------------------------------------------------------------------------#

f_x = function(a){
  df = data.frame()
  colnames(a) = c("edad", "energia", "proteina_l", "proteina_u", "grasa_l", "grasa_u",
                  "cho_l", "cho_u", "vit_c_l", "vit_c_u", "folato_l", "folato_u",
                  "vit_a_l", "vit_a_u", "tiamina_l", "tiamina_u", "ribo_l", "ribo_u",
                  "niacina_l", "niacina_u", "b12_l", "b12_u", "mg_l", "mg_u", "p_l", "p_u",
                  "na_l", "na_u", "ca_l", "ca_u", "fe_l", "fe_u", "zinc_l", "zinc_u")
  
  a[is.na(a)] = 999999
  df = a
  return(df)
}

MOD_2 <- new.env()

#------------------------- másculino

if(!is.null(DRI_M_OP)){
DRI_m=DRI_M_OP

} else {


data(DRI_M, package = "Foodprice", envir = MOD_2)
DRI_m = f_x(DRI_M) # El priemro siembre debe ser la edad y la segundo la energía

}


#------------------------- femenino

if(!is.null(DRI_F_OP)){
DRI_f=DRI_F_OP

} else {

data(DRI_F, package = "Foodprice", envir = MOD_2)
DRI_f = f_x(DRI_F) # El priemro siembre debe ser la edad y la segundo la energía

}



#--------------------------------------------------------------------------------------#
#                   CUARTA ETAPA: CMODELO FEMENINO                                    #
#------------------------------------------------------------------------------------#

# vector de precios
precios =Datos_Insumo$Precio_100g_ajust

# nombre alimentos
alimentos=Datos_Insumo$Alimento

# matriz de contenidos nutricionales y energéticos
keep = c("Energia", "Proteina", "Lipidos", "Carbohidratos", "VitaminaC", "Folatos", "VitaminaA",
         "Tiamina", "Riboflavina", "Niacina", "VitaminaB12", "Magnesio", "Fosforo", "Sodio",
         "Calcio", "Hierro", "Zinc")
A = Datos_Insumo[keep] %>% as.matrix() %>% t()
A = rbind(A, A[-1,])

#vector de limites inferiores
lower = c("proteina_l", "grasa_l","cho_l", "vit_c_l", "folato_l",
          "vit_a_l", "tiamina_l", "ribo_l",
          "niacina_l", "b12_l", "mg_l", "p_l",
          "na_l", "ca_l", "fe_l", "zinc_l")

#vector de limites superiores
upper = setdiff(colnames(DRI_f),lower)
upper = upper[!upper %in% c("edad", "energia")]

#signos de las restricciones
constr_signs = c("=", rep(">=", length(lower)), rep("<=", length(upper)))


"-------------------------------------------------------------------"
"-------------------------------------------------------------------"
"-------------------------------------------------------------------"

#requerimientos de energia segun grupos etarios
dri_edad = list()

for (i in 1:14) {
  b = vector()
  b = as.numeric(DRI_f$energia[i])
  dri_edad = append(dri_edad, b)
  names(dri_edad)[i] = paste0("b_",i)
  rm(b)
}

# requerimientos nutricionales minimos (limites inferiores)
lower_list = list()
for (i in 1:14) {
  df = DRI_f
  df = df[lower]
  l = as.numeric(df[i,])
  lower_list[[i]] = l
  names(lower_list)[i] = paste0("lower_", i)
  rm(l , df)
}


# requerimientos nutricionales maximos (limites superiores)

upper_list = list()
for (i in 1:14) {
  df = DRI_f
  df = df[upper]
  u = as.numeric(df[i,])
  upper_list[[i]] = u
  names(upper_list)[i] = paste0("upper_", i)
  rm(u , df)
}

# vector b segun grupo de edad

b_2 = list()
for (k in 1:14) {
  b_2[[k]] = c(dri_edad[[k]], lower_list[[k]], upper_list[[k]])  
}



"-------------------------------------------------------------------"
"-------------------------------------------------------------------"
"-------------------------------------------------------------------"




# base de datos para la presentacion de resultados
modelo_2 = data.frame(alimentos)
modelo_2 = modelo_2 %>% add_row(alimentos = "Costo")
colnames(modelo_2) = "Alimentos"

edad=DRI_f$edad


nutrientes = c("Grupo", "Nutriente", "Proteina", "Lipidos", "Carbohidratos", "VitaminaC",    
               "Folatos" , "VitaminaA" ,    "Tiamina"  ,     "Riboflavina" ,
               "Niacina" , "VitaminaB12" , "Magnesio" , "Fosforo", "Sodio" ,
               "Calcio"  , "Hierro", "Zinc")

const_2 = as.data.frame(matrix(ncol = length(nutrientes)))
colnames(const_2) = nutrientes
const_2  = na.omit(const_2)

#solucion del modelo
for (i in 1:14) {

  
  #resolver el modelo
  
  df_1 = data.frame()
  df_2 = data.frame()
  b = b_2[[i]]
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
  modelo_2 = merge(modelo_2,df_1, by = "Alimentos")
  rm(b, df_1, df_2)
  
  #cumplimiento de las restricciones
  const = A[1:17,]%*%as.matrix(opt_sol$solution)
  const = data.frame(rownames(const), as.numeric(const))
  const = const[-1,]
  const = cbind(const, as.matrix(lower_list[[i]]))
  colnames(const) = c("Nutriente", "Opt", "Rest")
  const$Dif = ((const$Opt - const$Rest)/const$Rest)*100
  const[const<0] <- 0
  
  #redondear
  for (k in 1:nrow(const)) {
    for (l in 2:ncol(const)) {
      const[k,l] = round(as.numeric(const[k,l]), digits = 2)  
    }
  }
  
  colnames(const)[which(colnames(const) == "Dif")] = paste0("Dif", " (%)")
  
  
  const = as.data.frame(t(const))
  colnames(const) = as.character(const[1,])
  const = const[-1,]
  
  const[,ncol(const)+ 1] = rownames(const)
  const[,ncol(const)+ 1] = c(edad[i], edad[i], edad[i])
  
  const = const[,c(ncol(const), (ncol(const)-1),1:(ncol(const)-2))]
  rownames(const) = seq(1, nrow(const), by = 1)
  colnames(const)[1] = "Grupo"
  colnames(const)[2] = "Nutriente"
  
  const_2 = rbind(const_2, const)
  rm(const)
}



#preparacion de resultados en gramos
modelo_2[modelo_2 == 0] = NA
modelo_2_res = modelo_2[rowSums(is.na(modelo_2[,2:length(colnames(modelo_2))])) != ncol(modelo_2[,2:length(colnames(modelo_2))]),]


modelo_2_res = modelo_2_res[-which(modelo_2_res$Alimentos == "Costo"),]


#presentacion de resultados en gramos
for (k in 2:ncol(modelo_2_res)) {
  modelo_2_res[,k] = as.numeric(modelo_2_res[,k])*100
}

modelo_2_res[nrow(modelo_2_res)+1, ] = modelo_2[which(modelo_2$Alimentos == "Costo"),]
modelo_2_res[is.na(modelo_2_res)] = 0 


assign("Modelo_2_F",modelo_2_res,envir = globalenv())

#--------------------------------------------------------------------------------------#
#                   QuUINTA ETAPA: MODELO MÁSCULINO                                   #
#------------------------------------------------------------------------------------#

# requerimientos de enegia por grupos etarios
dri_edad = list()

for (i in 1:8) {
  b = vector()
  b = as.numeric(DRI_m$energia[i])
  dri_edad = append(dri_edad, b)
  names(dri_edad)[i] = paste0("b_",i)
  rm(b)
}

# requerimientos nutricionales minimos (limites inferiores)
lower_list = list()
for (i in 1:8) {
  df = DRI_m
  df = df[lower]
  l = as.numeric(df[i,])
  lower_list[[i]] = l
  names(lower_list)[i] = paste0("lower_", i)
  rm(l , df)
}

# requerimientos nutricionales máximos (limites superiores)
upper_list = list()
for (i in 1:8) {
  df = DRI_m
  df = df[upper]
  u = as.numeric(df[i,])
  upper_list[[i]] = u
  names(upper_list)[i] = paste0("upper_", i)
  rm(u , df)
}

# vector b segun grupo de edad
b_2 = list()
for (k in 1:8) {
  b_2[[k]] = c(dri_edad[[k]], lower_list[[k]], upper_list[[k]])  
}

b_2[[8]] =  (1 - 0.067)*b_2[[7]]

# base de datos para la presentacion de resultados
modelo_2 = data.frame(alimentos)
modelo_2 = modelo_2 %>% add_row(alimentos = "Costo")
colnames(modelo_2) = "Alimentos"

edad=DRI_m$edad


const_2 = as.data.frame(matrix(ncol = length(nutrientes)))
colnames(const_2) = nutrientes
const_2  = na.omit(const_2)


#solucion del modelo
for (i in 1:8) {
  

  #resolver el modelo
  
  df_1 = data.frame()
  df_2 = data.frame()
  b = b_2[[i]]
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
  modelo_2 = merge(modelo_2,df_1, by = "Alimentos")
  rm(b, df_1, df_2)
  
  #cumplimiento de las restricciones
  const = A[1:17,]%*%as.matrix(opt_sol$solution)
  const = data.frame(rownames(const), as.numeric(const))
  const = const[-1,]
  const = cbind(const, as.matrix(lower_list[[i]]))
  colnames(const) = c("Nutriente", "Opt", "Rest")
  const$Dif = ((const$Opt - const$Rest)/const$Rest)*100
  const[const<0] <- 0
  
  #redondear
  for (k in 1:nrow(const)) {
    for (l in 2:ncol(const)) {
      const[k,l] = round(as.numeric(const[k,l]), digits = 2)  
    }
  }
  
  colnames(const)[which(colnames(const) == "Dif")] = paste0("Dif", " (%)")
  
  
  const = as.data.frame(t(const))
  colnames(const) = as.character(const[1,])
  const = const[-1,]
  
  const[,ncol(const)+ 1] = rownames(const)
  const[,ncol(const)+ 1] = c(edad[i], edad[i], edad[i])
  
  const = const[,c(ncol(const), (ncol(const)-1),1:(ncol(const)-2))]
  rownames(const) = seq(1, nrow(const), by = 1)
  colnames(const)[1] = "Grupo"
  colnames(const)[2] = "Nutriente"
  
  const_2 = rbind(const_2, const)
  rm(const)
}



#preparacion de resultados en gramos
modelo_2[modelo_2 == 0] = NA
modelo_2_res = modelo_2[rowSums(is.na(modelo_2[,2:length(colnames(modelo_2))])) != ncol(modelo_2[,2:length(colnames(modelo_2))]),]


modelo_2_res = modelo_2_res[-which(modelo_2_res$Alimentos == "Costo"),]


#presentacion de resultados en gramos
for (k in 2:ncol(modelo_2_res)) {
  modelo_2_res[,k] = as.numeric(modelo_2_res[,k])*100
}

modelo_2_res[nrow(modelo_2_res)+1, ] = modelo_2[which(modelo_2$Alimentos == "Costo"),]
modelo_2_res[is.na(modelo_2_res)] = 0 


assign("Modelo_2_M",modelo_2_res,envir = globalenv())

  if(length(warnings())<100) {print("Ejecución del modelo 2 correcta")} else {cat("Cantidad de errores encontrados:",length(warnings()), "\n")}

}
