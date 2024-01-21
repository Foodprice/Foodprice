#------------------------------------------------------------------------------------------#
#                    SEGUNDA FUNCIÓN: MODELO 2: DIETA ADEC EN NUTRIENTES                  #
#-----------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------#
#                       PRIMERA ETAPA: VALIDACIÓN DE LIBRERIAS                             #
#-----------------------------------------------------------------------------------------#

Librerias_base = c("readxl","dplyr","ggplot2","reshape2","knitr","haven","foreign","stringi","labelled","tidyr","plyr","tidyverse",
                        "lpSolve","Rglpk","scatterplot3d","reshape","R6","rio","janitor","stringr") # Nombra las librerias necesarias

if (!require("pacman")) install.packages("pacman") # Paquete que simplifica la carga de librerias
pacman::p_load(char = Librerias_base);Librerias_base_print = paste0(paste0("'", Librerias_base, "'"), collapse = ", ") # Instala si es necesario, o en su defecto, sólo llama los paquetes



f_gabas_2 = function(a){
  dataset_0 = data.frame()
  a$Grupo_GABAS[which(a$Grupo_GABAS == "AZÚCARES")] = "Azúcares"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "CARNES, HUEVOS, LEGUMINOSAS, FRUTOS SECOS Y SEMILLAS")] = "Carnes, huevos y leguminosas"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS")] = "Cereales y raíces"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "VERDURAS Y FRUTAS")] = "Frutas y verduras"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "GRASAS")] = "Grasas"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "LECHE Y PRODUCTOS LÁCTEOS")] = "Lácteos"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "SIN CATEGORIA")] = "Sin categoría"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "FRUTAS")] = "Frutas"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "VERDURAS")] = "Verduras"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "CEREALES")] = "Cereales"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "RAICES")] = "Raices"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "TUBERCULOS")] = "Tuberculos"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "CARNES")] = "Carnes"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "LEGUMINOSAS")] = "Leguminosas"
  dataset_0 = a
  return(dataset_0)
}


#------------------------- definición de matrices y vectores de coeficientes por medio de funciones--------------

# primero: un alimento

f_A_1 = function(a){
  A_0 = data.frame()
  A_0 = as.matrix(c(1))
  return(A_0)
}

f_b_1 = function(a){
  b = int_req_m_x[which(int_req_m_x$Grupo_GABAS == levels(as.factor(df_x$Grupo_GABAS))),2]
  return(b)
}

# segundo: dos alimentos
  f_A_2 = function(a){
    a = df_x
    A_0 = data.frame()
    vec_x = c(a$Intercambio_g[1], -a$Intercambio_g[2])
    vec_y = rep(1,2)
    A_0 = rbind(vec_x, vec_y)
    return(A_0)
  }

  f_b_2 = function(a){
    b1 = 0
    b2 = int_req_m_x[which(int_req_m_x$Grupo_GABAS == levels(as.factor(df_x$Grupo_GABAS))),2]
    b = c(b1, b2)
    return(b)
  }

# tercero: tres alimentos
  f_A_3 = function(a){
    a = df_x
    A_0 = data.frame()
    vec_x = c(a$Intercambio_g[1], -a$Intercambio_g[2],0)
    vec_y = c(0, a$Intercambio_g[2], -a$Intercambio_g[3])
    vec_z = rep(1,3)
    A_0 = rbind(vec_x, vec_y, vec_z)
    return(A_0)
  }

  f_b_3 = function(a){
    b1 = 0
    b2 = 0
    b3 = int_req_m_x[which(int_req_m_x$Grupo_GABAS == levels(as.factor(df_x$Grupo_GABAS))),2]
    b = c(b1, b2, b3)
    return(b)
  }

# cuarto: cuatro alimentos

  f_A_4 = function(a){
    a = df_x
    A_0 = data.frame()
    vec_x = c(a$Intercambio_g[1], -a$Intercambio_g[2], 0 , 0)
    vec_y = c(0, a$Intercambio_g[2], -a$Intercambio_g[3], 0)
    vec_z = c(0, 0, a$Intercambio_g[3], -a$Intercambio_g[4])
    vec_w = rep(1,4)
    A_0 = rbind(vec_x, vec_y, vec_z, vec_w)
    return(A_0)
  }

  f_b_4 = function(a){
    b1 = 0
    b2 = 0
    b3 = 0
    b4 = int_req_m_x[which(int_req_m_x$Grupo_GABAS == levels(as.factor(df_x$Grupo_GABAS))),2]
    b = c(b1, b2, b3, b4)
    return(b)
  }

remove.packages("Foodprice")
devtools::install_github("Foodprice/Foodprice");library(Foodprice)

dataset_m3

  # primero: se excluyen los alimentos sin categorías
  dataset_m3 = dataset_m3 %>% filter(!Grupo_GABAS %in% "Sin categoría")

  # segundo: se excluyen los alimentos que no están recomendados por GABAS

colnames(exclusion_3er_modelo)=c("Alimento","Cod_TCAC")
  dataset_m3 = dataset_m3 %>% filter(!Cod_TCAC %in% levels(as.factor(exclusion_3er_modelo$Cod_TCAC)))

exclusion_ad_hoc = c("Carne de cerdo, espinazo", "Yuca ICA", "Papa Betina",
                       "Papa única")

dataset_m3 = dataset_m3 %>% filter(!Alimento %in% exclusion_ad_hoc)


#---------------------------------------------------------------------------------------#
#                            QUINTA ETAPA: TERCER   MODELO FEMENINO                    #
#-------------------------------------------------------------------------------------#

# base de datos de recepción
modelo_3_dieta = data.frame(dataset_m3[c("Grupo_GABAS", "Alimento")])

edad = colnames(int_req_f)[-1]

modelo_3_costo = as.data.frame(matrix(ncol = 1))
colnames(modelo_3_costo) = c("Grupo")
modelo_3_costo$Grupo[1] = "Costo"


modelo_3_dieta_g = modelo_3_dieta
modelo_3_dieta_int = modelo_3_dieta


## Solución del    MOD3      ##

colnames(cantidad_alimentos_seleccionar) = c("Grupo_GABAS", "Cantidad")

# recodificar cantidad a seleccionar
cantidad_alimentos_seleccionar$Grupo_GABAS[which(cantidad_alimentos_seleccionar$Grupo_GABAS == "Azúcares")] = "Azúcares"
cantidad_alimentos_seleccionar$Grupo_GABAS[which(cantidad_alimentos_seleccionar$Grupo_GABAS == "Carnes, huevos y leguminosas")] = "Carnes, huevos y leguminosas"
cantidad_alimentos_seleccionar$Grupo_GABAS[which(cantidad_alimentos_seleccionar$Grupo_GABAS == "Cereales, Raíces, Tubérculos y Plátanos")] = "Cereales y raíces"
cantidad_alimentos_seleccionar$Grupo_GABAS[which(cantidad_alimentos_seleccionar$Grupo_GABAS == "Frutas y verduras")] = "Frutas y verduras"
cantidad_alimentos_seleccionar$Grupo_GABAS[which(cantidad_alimentos_seleccionar$Grupo_GABAS == "Grasas")] = "Grasas"
cantidad_alimentos_seleccionar$Grupo_GABAS[which(cantidad_alimentos_seleccionar$Grupo_GABAS == "Leche y productos lácteos")] = "Lácteos"

# incluir la cantidad a seleccionar de frutas y verduras
frutas_verduras_cantidad = data.frame(Grupo_GABAS = c("Verduras", "Frutas"),
                                      Cantidad = c(2,2))
cantidad_alimentos_seleccionar = rbind(cantidad_alimentos_seleccionar, frutas_verduras_cantidad)

# incluir la cantidad a seleccionar de cereales, raíces y tubérculos
cereales_cantidad = data.frame(Grupo_GABAS = c("Tuberculos", "Raices",
                                                      "Cereales"),
                                      Cantidad = c(1,1,1))
cantidad_alimentos_seleccionar = rbind(cantidad_alimentos_seleccionar, cereales_cantidad)

# incluir la cantidad a seleccionar de carnes y leguminosas
carnes_cantidad = data.frame(Grupo_GABAS = c("Carnes", "Leguminosas"),
                                      Cantidad = c(1,1))
cantidad_alimentos_seleccionar = rbind(cantidad_alimentos_seleccionar, carnes_cantidad)

# construcción de subsets por grupos de alimentos
gabas = levels(as.factor(dataset_m3$Grupo_GABAS))
datos_grupos = list()
length(datos_grupos) = length(gabas)

gabas = setdiff(gabas, c("Carnes, huevos y leguminosas", "Cereales y raíces"))

for (k in 1:length(gabas)) {
  df = data.frame()
  df = dataset_m3 %>% filter(Grupo_GABAS %in% gabas[k])
  datos_grupos[[k]] = df
  names(datos_grupos)[k] = paste0("Grupo_",gabas[k])
  rm(df)
}

# bucle general para obtener las cantidades de los alimentos
for (i in 1:length(edad)) {
  # restricciones
  int_req_m_x = int_req_f[,c(1,1+i)]
  colnames(int_req_m_x) = c("Grupo_GABAS", "Intercambio")
  int_req_m_x = f_gabas_2(int_req_m_x)

  df_solution = as.data.frame(matrix(ncol = (ncol(dataset_m3)+2)))
  colnames(df_solution) = c(colnames(dataset_m3), "sol_int", "solution_g")
  df_solution = na.omit(df_solution)



  #frutas
  df = datos_grupos[["Grupo_Frutas"]]
  q = cantidad_alimentos_seleccionar[which(cantidad_alimentos_seleccionar$Grupo_GABAS == levels(as.factor(df$Grupo_GABAS))),2]
  q = as.numeric(levels(as.factor(q$Cantidad)))
  df = df[order(df$Precio_per_int),]
  df_x = df[1:q,]

  A = f_A_2(df_x)
  b = f_b_2(df_x)
  df_x$sol_int = solve(A, b)
  df_x$solution_g = df_x$sol_int*df_x$Intercambio_g
  df_solution = rbind(df_solution, df_x)

  #verduras
  df = datos_grupos[["Grupo_Verduras"]]
  q = cantidad_alimentos_seleccionar[which(cantidad_alimentos_seleccionar$Grupo_GABAS == levels(as.factor(df$Grupo_GABAS))),2]
  q = as.numeric(levels(as.factor(q$Cantidad)))
  df = df[order(df$Precio_per_int),]
  df_x = df[1:q,]

  A = f_A_2(df_x)
  b = f_b_2(df_x)
  df_x$sol_int = solve(A, b)
  df_x$solution_g = df_x$sol_int*df_x$Intercambio_g
  df_solution = rbind(df_solution, df_x)



  # grasas, lacteos y azucares
  for (k in c(1,2,3,5,6,7,8,9)) {
    df = datos_grupos[[k]]
    q = cantidad_alimentos_seleccionar[which(cantidad_alimentos_seleccionar$Grupo_GABAS == levels(as.factor(df$Grupo_GABAS))),2]
    q = as.numeric(levels(as.factor(q$Cantidad)))
    df = df[order(df$Precio_per_int),]
    df_x = df[1:q,]

    A = f_A_1(df_x)
    b = f_b_1(df_x)
    df_x$sol_int = solve(A, b)
    df_x$solution_g = df_x$sol_int*df_x$Intercambio_g
    df_solution = rbind(df_solution, df_x)
  }



  costo_df = as.data.frame(matrix(ncol = 2, nrow = 1))
  colnames(costo_df) = c("Grupo", edad[i])
  costo_df$Grupo = "Costo"
  costo_df[,2] = sum(df_solution$Precio_per_int*df_solution$sol_int)

  modelo_3_costo = merge(modelo_3_costo, costo_df, by = "Grupo")



  modelo_3_sol_g = df_solution[c("Alimento", "solution_g")]
  modelo_3_sol_int = df_solution[c("Alimento", "sol_int")]

  colnames(modelo_3_sol_g) = c("Alimento", edad[i])
  colnames(modelo_3_sol_int) = c("Alimento", edad[i])


  modelo_3_dieta_g = merge(modelo_3_dieta_g, modelo_3_sol_g, by = "Alimento")
  modelo_3_dieta_int = merge(modelo_3_dieta_int, modelo_3_sol_int, by = "Alimento")

}


# Organizar los resultados
modelo_3_dieta_g = modelo_3_dieta_g[,c(2,1,3:16)]
modelo_3_dieta_g = modelo_3_dieta_g[order(modelo_3_dieta_g$Grupo_GABAS),]


modelo_3_dieta_int = modelo_3_dieta_int[,c(2,1,3:16)]
modelo_3_dieta_int = modelo_3_dieta_int[order(modelo_3_dieta_int$Grupo_GABAS),]



