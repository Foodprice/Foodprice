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
  if (ncol(EER) < 3) {
    stop("Los requerimientos para el modelo 1 deben contener al menos 3 columnas.")
  }
  required_columns_E <- c("Edad","Energia")
  missing_columns_E <- setdiff(required_columns_E, colnames(EER))
  
  if (length(missing_columns_E) > 0) {
    stop(paste("El modelo 1 requiere las siguientes columnas en los datos de insumo: ", paste(missing_columns_E, collapse = ", "),". Por favor revise la documentación para conocer el nombre que deben tener las columnas necesarias al primer modelo"))}
  
  
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
  #                       TERCERA ETAPA: MODELO 1                                           #
  #-----------------------------------------------------------------------------------------#
  
  #Si no existe la columna sexo
  
  # Extraer sexos disponibles si existe la columna sexo
  if ("Sexo" %in% colnames(EER)) {Sexos <- split(EER, EER$Sexo);sexo_nombre=names(Sexos)} else {
    sexo_nombre=0
    
  }
  
  
  #--------------------------------------------------------#
  #               CLICLO PARA CADA SEXO                   #
  #-------------------------------------------------------#
  for (sexo_nombre in sexo_nombre) { 
    
    
    
    Salida_CoCA <- data.frame(Alimentos = Datos_Insumo$Alimento);Salida_CoCA <- Salida_CoCA %>% add_row(Alimentos = "Costo") # Define el df de salida
    
    # REquerimientos por sexo
    if ("Sexo" %in% colnames(EER)) {EER_S <- Sexos[[sexo_nombre]]}
    
    
    # ---Asignación de vectores al modelo
    Precio = Datos_Insumo$Precio_100g_ajust;Alimento=Datos_Insumo$Alimento;Edad=EER$Edad
    
    # MAtriz de coef de restricción al modelo (ENERGIA)
    Coef.Restriq = matrix(as.vector(Datos_Insumo$Energia), ncol = length(Alimento))
    
    # Vector de limitaciones del modelo
    Limitaciones=EER_S$Energia

    
    #------------------------------Solución del modelo:
    
    # Modelo
    
    for( i in seq_along(Limitaciones)) {#ciclo para cada edad
      
      CoCA = lp(direction = "min",
                objective.in = Precio,
                const.mat = Coef.Restriq,
                const.dir = c("="),
                const.rhs = Limitaciones[i],
                compute.sens = TRUE)
      
      #------------------------ POR EDAD Guardar soluciones
      
      
      # Crear dataframe df_1 con alimentos y la solución óptima para la edad actual
      df_1 <- data.frame(Alimentos = Alimento, Valor = CoCA$solution);colnames(df_1) <- c("Alimentos", as.character(Edad[i]))
      
      # Crear dataframe df_2 con el costo asociado para la edad actual
      df_2 <- data.frame(Alimentos = "Costo", Valor = CoCA$objval);colnames(df_2) <- colnames(df_1);df_combinado <- rbind(df_1, df_2)
      
      # Agregar la información al dataframe modelo_1
      Salida_CoCA <- merge(Salida_CoCA, df_combinado, by = "Alimentos")



    }  #Fin del ciclio del modelo por edad
    
    
    #--------------------------------------------------------#
    #               ETAPA DE ESTRUCTURA PLAZA                #
    #-------------------------------------------------------#
    
    #DF sin ceros
    DF_o <- Salida_CoCA[rowSums(Salida_CoCA[, -which(names(Salida_CoCA) %in% c("Alimentos"))]) != 0, ]


    Costo=DF_o[DF_o=="Costo",];Costo=as.vector(t(Costo[Costo$Alimentos == "Costo", -1]))
    Costo_1000kcal=(Costo/Limitaciones)*1000
    # Identificar grupo si existe la columna en insumos

    if ("Grupo" %in% colnames(Datos_Insumo)) {Grupo_sex=na.omit(as.vector(Datos_Insumo$Grupo[match(Alimento, DF_o$Alimentos)]))[1]}



    # Indetificando el alimento
    df_alimentos <- DF_o[DF_o$Alimentos != "Costo", ]
    
    # Multiplicar los valores por 100
    df_alimentos[, -1] <- df_alimentos[, -1] * 100
    
    #ESTRUCTURA CIAT
df_transformado <- df_alimentos %>%
  pivot_longer(cols = -Alimentos, names_to = "Grupo_demo", values_to = "Cantidad_G") %>%
  mutate(
    Alimento = Alimentos,
    Sexo = as.numeric(sexo_nombre),
    Grupo = if ("Grupo" %in% colnames(Datos_Insumo)) Grupo_sex else NA
  ) %>%
  select(Alimento, Cantidad_G, Grupo_demo, Sexo, Grupo)

  df_transformado_limpio <- df_transformado %>%
  select(-where(~all(is.na(.))))
    
    
    assign(paste("CoCA_", sexo_nombre, sep = ""), cbind(df_transformado_limpio, Costo,Costo_1000kcal))
    
    
    #--------------------------------------------------------#
    #        FIND DEL       CLICLO PARA CADA SEXO            #
    #-------------------------------------------------------#
    
    
  }
  # Unir ambos df para cada sexo (si existe)
  if ("Sexo" %in% colnames(EER)) {Costo_CoCA=rbind(CoCA_1,CoCA_0)} else {
    Costo_CoCA <- CoCA_0 %>%
      select(-Sexo)
  }
  
  #----------------------------#
  #     ASGINACIONES DE LISTA  #
  #----------------------------#
  
  List_CoCA=list(Costo_CoCA,Precio,Alimento,EER);names(List_CoCA)=c("Costo_CoCA","Precio","Alimento","EER")
  
  # retorno
  
cat("(✓) CoCA: Costo diario promedio por cada 1000 kilocalorías es", mean(Costo_CoCA$Costo_1000kcal)) 

 return(invisible(List_CoCA))
  
  #------------------------------------------------------------------------------------------#
  #                       FIN DEL SEGUNDO MÓDULO COMO FUNCIÓN                               #
  #-----------------------------------------------------------------------------------------#

}
