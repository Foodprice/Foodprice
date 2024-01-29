#------------------------------------------------------------------------------------------#
#         PRIMERA FUNCIÓN: CARGA Y DEPURACIÓN DATOS DE COLOMBIA, DANE                      # 
#-----------------------------------------------------------------------------------------#


DatosCol<- function(Mes, Año, Ciudad, Percentil_Abast = NULL, Ingreso_Alimentos = NULL, data_list_precios = NULL, data_list_abas = NULL, Margenes=NULL) {
  
  
  #------------------------------------------------------------------------------------------#
  #         PRIMERA ETAPA: VALIDACIÓN DE PARÁMETROS OBLIGATORIOS Y OPCIONALES                # ✔ SIMPLIFICADA Y ASEGURADA
  #-----------------------------------------------------------------------------------------#
  
#Función para validar parámetros
validar_parametros <- function(parametro, tipo, rango = NULL) {
  if (missing(parametro)) {
    stop("Falta el parámetro ", deparse(substitute(parametro)))
  }
  
  if (!is.null(tipo)) {
    tipo_funcion <- switch(tipo,
                           "numeric" = is.numeric,
                           "character" = is.character,
                           "list" = is.list,
                           "vector" = function(x) is.vector(x) || is.data.frame(x),
                           "default" = function(x) FALSE)
    
    if (!tipo_funcion(parametro)) {
      stop(paste(deparse(substitute(parametro)), " debe ser de tipo ", tipo))
    }
  }
  
  if (!is.null(rango) && !is.infinite(rango[1]) && !is.infinite(rango[2])) {
    if (parametro < rango[1] || parametro > rango[2]) {
      stop(paste(deparse(substitute(parametro)), " debe estar en el rango ", rango[1], " - ", rango[2]))
    }
  }
}

#-----------------------Verificaciones de paraḿetros obligatorios

# Verificación de Mes
validar_parametros(Mes, "numeric", c(1, 12))
# Verificación de Año
validar_parametros(Año, "numeric", c(2013, 2023))
# Verificación de Ciudad
validar_parametros(Ciudad, "character")

#-----------------------Verificaciones de paraḿetros opcionales

if (!is.null(Percentil_Abast)) {
  validar_parametros(Percentil_Abast, "numeric", c(0, 1))
  
  if (!is.null(data_list_abas)) {
    validar_parametros(data_list_abas, "list")
  }
} else {
  if (!is.null(data_list_abas)) {
    stop("Si se proporciona data_list_abas, Percentil_Abast debe estar presente.")
  }
}
# Verificación de Ingreso_Alimentos si es proporcionado
if (!is.null(Ingreso_Alimentos)) {validar_parametros(Ingreso_Alimentos, "vector", c(21, 21))}
# Verificación de data_list_precios
if (!is.null(data_list_precios)) {validar_parametros(data_list_precios, "list")}
# Verificación de margenes
if (!is.null(data_list_precios)) {validar_parametros(Margenes, "vector", c(8, 8))}

  #------------------------------------------------------------------------------------------#
  #                       SEGUNDA ETAPA: VALIDACIÓN DE LIBRERIAS                             # ✔ SIMPLIFICADA Y ASEGURADA
  #-----------------------------------------------------------------------------------------#
  
  Librerias_base = c("readxl","dplyr","ggplot2","reshape2","knitr","haven","foreign","stringi","labelled","tidyr","plyr","tidyverse",
                     "lpSolve","Rglpk","scatterplot3d","reshape","R6","rio","janitor","lubridate","stringr") # Nombra las librerias necesarias
  
  if (!require("pacman")) install.packages("pacman") # Paquete que simplifica la carga de librerias
  pacman::p_load(char = Librerias_base,character.only = TRUE);Librerias_base_print = paste0(paste0("'", Librerias_base, "'"), collapse = ", ") # Instala si es necesario, o en su defecto, sólo llama los paquetes


# Instala paquetes individualmente si no se han cargado correctamente
paquetes_faltantes <- Librerias_base[!(Librerias_base %in% pacman::p_loaded())]
for (paquete in paquetes_faltantes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
    library(paquete, character.only = TRUE)
  }
}


  #cat("\n")
  #print("Se instalaron y cargaron todas la librerias corectamente")
  #cat("\n")
   

  #------------------------------------------------------------------------------------------#
  #                   TERCERA ETAPA: CARGA DE DATOS DESDE EL DANE (COL)                      # ✔ SIMPLIFICADA Y ASEGURADA
  #-----------------------------------------------------------------------------------------#


options(rio.column_names = FALSE)  # No mostrar nombres de columnas al importar como lista
options(timeout = 1000)  # Tiempo de espera alto


# Función para descargar y cargar datos desde el DANE
cargar_datos_dane <- function(tipo, año, env) {
  temp_dir <- tempdir()
  archivo_excel <- file.path(temp_dir, paste0("archivo_", tipo, "_", año, ".xlsx"))
  nombre_data <- paste0("data_list_", tipo, "_", año, "_ev")

  if (!exists(nombre_data, envir = env)) {
    url_excel <- switch(
      tipo,
      "precios" = {
        if (año > 2022) {
          sprintf("https://www.dane.gov.co/files/operaciones/SIPSA/anex-SIPSA-SerieHistoricaMayorista-%d.xlsx", año)
        } else if (año > 2017) {
          sprintf("https://www.dane.gov.co/files/investigaciones/agropecuario/sipsa/series-historicas/series-historicas-precios-mayoristas-%d.xlsx", año)
        } else {
          "https://www.dane.gov.co/files/investigaciones/agropecuario/sipsa/series-historicas/series-historicas-precios-mayoristas.xlsx"
        }
      },
      "abastecimiento" = {
        if (año > 2022) {
          sprintf("https://www.dane.gov.co/files/operaciones/SIPSA/anex-SIPSAbastecimiento-Microdatos-%d.xlsx", año)
        } else {
          sprintf("https://www.dane.gov.co/files/investigaciones/agropecuario/sipsa/series-historicas/microdato-abastecimiento-%d.xlsx", año)
        }
      },
      stop("Tipo de datos no reconocido.")
    )

    if (!file.exists(archivo_excel)) {
      download.file(url_excel, archivo_excel, mode = "wb", timeout = 444)
      suppressMessages(assign(nombre_data, rio::import_list(archivo_excel, setclass = "tbl"), envir = env))
    } else {
      suppressMessages(assign(nombre_data, rio::import_list(archivo_excel, setclass = "tbl"), envir = env))
    }
  }

  return(get(nombre_data, envir = env))
}


# Función para crear o reutilizar un entorno
crear_o_reusar_entorno <- function(nombre_entorno) {
  if (!exists(nombre_entorno, envir = globalenv())) {
    assign(nombre_entorno, new.env(parent = emptyenv()), envir = globalenv())
  }
  return(get(nombre_entorno, envir = globalenv()))
}

# Crear o reutilizar entornos para precios y abastecimiento
data_list_precios_ev_nuevo <- crear_o_reusar_entorno("data_list_precios_ev")
data_list_abast_ev_nuevo <- crear_o_reusar_entorno("data_list_abast_ev")



# Carga de precios mayoristas
if (is.null(data_list_precios)) {
  data_list_precios = cargar_datos_dane("precios", Año, data_list_precios_ev_nuevo)
}

# Carga de datos de abastecimiento
if (!is.null(Percentil_Abast) && is.null(data_list_abas)) {
  data_list_abas = cargar_datos_dane("abastecimiento", Año, data_list_abast_ev_nuevo)
} else {
  data_list_abas = NULL
}


  #------------------------------------------------------------------------------------------#
  #                       CUARTA ETAPA: DEPURACIÓN DE LOS DATOS                              #  ✔ SIMPLIFICADA Y ASEGURADA
  #-----------------------------------------------------------------------------------------#
  
  
  #------------------ IDENTIFICACIÓN DE MES, FECHAS Y SEMESTRES ------------------------------
  
  
  Nombres_Meses = c("Enero","Febrero","Marzo","Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre","Octubre","Noviembre","Diciembre")
  Mes_Num=Mes
  Mes=Nombres_Meses[Mes]  
  Semestres=c("I_Semestre","II_Semestre")
  
  Enero = seq(from = as.Date(paste(Año,"1","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(Año,"1","31", sep = "-"),format = "%Y-%m-%d"), by =1)
  Febrero = seq(from = as.Date(paste(Año,"2","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(Año,"2","28", sep = "-"),format = "%Y-%m-%d"), by =1)
  Marzo = seq(from = as.Date(paste(Año,"3","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(Año,"3","31", sep = "-"),format = "%Y-%m-%d"), by =1)
  Abril = seq(from = as.Date(paste(Año,"4","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(Año,"4","30", sep = "-"),format = "%Y-%m-%d"), by =1)
  Mayo = seq(from = as.Date(paste(Año,"4","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(Año,"5","31", sep = "-"),format = "%Y-%m-%d"), by =1)
  Junio = seq(from = as.Date(paste(Año,"6","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(Año,"6","30", sep = "-"),format = "%Y-%m-%d"), by =1)
  Julio = seq(from = as.Date(paste(Año,"7","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(Año,"7","31", sep = "-"),format = "%Y-%m-%d"), by =1)
  Agosto = seq(from = as.Date(paste(Año,"8","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(Año,"8","31", sep = "-"),format = "%Y-%m-%d"), by =1)
  Septiembre = seq(from = as.Date(paste(Año,"9","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(Año,"9","30", sep = "-"),format = "%Y-%m-%d"), by =1)
  Octubre = seq(from = as.Date(paste(Año,"10","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(Año,"10","31", sep = "-"),format = "%Y-%m-%d"), by =1)
  Noviembre = seq(from = as.Date(paste(Año,"11","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(Año,"11","30", sep = "-"),format = "%Y-%m-%d"), by =1)
  Diciembre = seq(from = as.Date(paste(Año,"12","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(Año,"12","30", sep = "-"),format = "%Y-%m-%d"), by =1)
  
  Semestre_I = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio")
  Semestre_II = c("Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  
  Lista_Mes=list(Enero,Febrero,Marzo,Abril, Mayo, Junio, Julio, Agosto, Septiembre,Octubre,Noviembre,Diciembre);names(Lista_Mes)=Nombres_Meses
  Lista_Semestres = list(Semestre_I, Semestre_II);names(Lista_Semestres)=c("I_Semestre","II_Semestre")
  Fecha=Lista_Mes[[Mes]]
  
  # -------------------------------------------------
  
  # ------------DFECHAS
  
  
  
  convertir_fechas_vector <- function(fechas) { #Función para validar el tipo de formato de entrada
    formato_fecha <- ifelse(grepl("/", fechas[1]), "%d/%m/%y", "%d") # Comprueba el formato
    
    if (formato_fecha == "%d/%m/%y") {
      fechas_convertidas <- as.Date(fechas, format = "%d/%m/%y", na.rm = TRUE)
    } else {
      fechas_numericas <- as.numeric(fechas)
      fechas_convertidas <- as.Date(fechas_numericas, origin = "1899-12-30")
    }
    
    return(fechas_convertidas)
  }
  

  
  #------------------ IDENTIFICACIÓN DE MES EN PRECIOS SIPSA   ------------------------------
  
  #S---elecionando el año según la estructura de datos
  
# Función para depurar y filtrar datos
depurar_y_filtrar <- function(data, mes_num) {
  data <- data[rowSums(is.na(data)) / ncol(data) < 0.5, colSums(is.na(data)) / nrow(data) < 0.5 ]
  data <- data[-1,]
  colnames(data) <- c("Fecha", "Grupo", "Alimento", "Mercado", "Precio_kg")
  data$Precio_kg <- as.numeric(data$Precio_kg)
  data$Fecha <- convertir_fechas_vector(data$Fecha)
  return(data[month(data$Fecha) >= mes_num,])
}



# Selección del año según la estructura de datos
if (Año >= 2019) {
  Meses <- Nombres_Meses[1:length(data_list_precios) - 1]
  posicion_mes <- which(Meses %in% Mes)
  
  if (length(posicion_mes) == 0) {
    stop("El mes solicitado aún no está presente en los datos abiertos de precios SIPSA.")
  }
  
  Data_Sipsa_Precios <- depurar_y_filtrar(data_list_precios[[posicion_mes + 1]], Mes_Num)
}

if (Año == 2018 || Año < 2018) {
  Año_selec <- ifelse(Año == 2018, 2, which(Año == 2013:2017) + 1)
  Data_Sipsa_Precios <- depurar_y_filtrar(data_list_precios[[Año_selec]], Mes_Num)
}

  

# assign(paste("PRECIOS_SIPSA", Mes, Año, sep = "_"),Data_Sipsa_Precios,envir = globalenv())
  
  
  
  # -- Establece la ciudad de interés
  
  
  ciudades_colombia <- c("Bogotá", "Medellín", "Cali", "Barranquilla", "Cartagena", "Cúcuta", "Bucaramanga", 
                         "Pereira", "Santa Marta", "Ibagué", "Pasto", "Manizales", "Neiva", "Soledad", "Villavicencio", "Valledupar", 
                         "Armenia", "Soacha", "Itagüí", "Sincelejo", "Popayán", "Floridablanca", "Palmira", "Buenaventura", 
                         "Barrancabermeja", "Dosquebradas", "Tuluá", "Envigado", "Cartago", "Maicao", "Florencia", "Girardot", 
                         "Sogamoso", "Buga", "Tunja", "Girón", "Mocoa", "Ipiales", "Facatativá", "Yopal", "Tumaco", "Riohacha", 
                         "Quibdó", "Turbo", "Magangué", "Apartadó", "Montería", "Arauca", "Mitu", "Puerto Carreño", "San Andrés")
  
  asociar_ciudad_mercado <- function(ciudad, df) { # Función del enguaje para asginar parámetro ciduad a algunas de las 51 de COL
    opciones_mercado <- unique(df$Mercado[grep(ciudad, df$Mercado, ignore.case = TRUE)])
    
    if (length(opciones_mercado) == 0) {
      stop("Error:No se encontraron opciones de mercado para la ciudad especificada. Por favor, verifique su ortografía o escriba una ciudad disponible.")
      opciones_mercado=NULL
    } else {
      return(opciones_mercado)
    }
  }
  
  
  asociar_ciudad_entrada_usuario <- function(entrada_usuario, lista_ciudades, df) { # Función para asginar a la ciudad identificada los mercados pertinentes
    
    similitudes <- sapply(lista_ciudades, function(ciudad) stringdist::stringdist(entrada_usuario, ciudad, method = "jw"))
    # Encontramos la ciudad más cercana en términos de texto a la entrada del usuario
    ciudad_mas_cercana <- lista_ciudades[which.min(similitudes)]
    # Llamamos a la función asociar_ciudad_mercado con la ciudad encontrada
    opciones_ciudad <- asociar_ciudad_mercado(ciudad_mas_cercana, df)
    return(opciones_ciudad)
  }
  
  
  Mercados_ciudad=asociar_ciudad_entrada_usuario(Ciudad,ciudades_colombia,Data_Sipsa_Precios)
  
  
  if(!is.null(Mercados_ciudad)) {
    Data_Sipsa_Precios = Data_Sipsa_Precios %>% filter(Mercado %in% Mercados_ciudad)
  } else {cat("Error,",Ciudad," aún no está en los datos públicos de precios SIPSA",sep="")}
  
  
  #-- Crea el vector de alimentos con base en la data de SIPSA precios mayoristas
  Alimentos_Sipsa_Precios = levels(as.factor(Data_Sipsa_Precios$Alimento))
  
  # Base de datos de recepción para el precio único de cada alimento
  Data_Sipsa_Precios_Unicos = data.frame(Alimentos_Sipsa_Precios, Precio_kg = NA);colnames(Data_Sipsa_Precios_Unicos)=c("Alimento","Precio_kg")
  
  # El precio único de cada alimento corresponde al precio promedio entre los 5 mercados
  
  for (i in 1:length(Alimentos_Sipsa_Precios)) {
    Media_Precios = data.frame()
    Media_Precios = Data_Sipsa_Precios[Data_Sipsa_Precios$Alimento == Alimentos_Sipsa_Precios[i],]
    Data_Sipsa_Precios_Unicos$Precio_kg[i] = mean(Media_Precios$Precio_kg)
    rm(Media_Precios)
  }
  
  #------------------ IDENTIFICACIÓN DE MES EN ABASTECIMIENTO    ------------------------------
  
  
  if (!is.null(Percentil_Abast)){
    
    if(Año >=2022){
      
      Data_Sipsa_Abas=(data_list_abas[[as.integer(which(sapply(Lista_Semestres, function(x) Mes %in% x)))+2]]) # Se extraen los meses disponibles con base en la data dada
    }
    
    else  {
      
      Data_Sipsa_Abas=(data_list_abas[[as.integer(which(sapply(Lista_Semestres, function(x) Mes %in% x)))+1]]) # Se extraen los meses disponibles con base en la data dada
      
    }
    
    if (ncol(Data_Sipsa_Abas)<9){stop("Error: En los datos del DANE no existe información de la fecha para este més, por favor omita los datos de abastecimiento.")}
    
    
    colnames(Data_Sipsa_Abas) = c("Ciudad_Mercado", "Fecha","Cod_Dep", "Cod_Mun", "Dep_Proc", "Mun_Proc","Grupo", "Alimento", "Cantidad_KG")
    Data_Sipsa_Abas <- Data_Sipsa_Abas[rowSums(is.na(Data_Sipsa_Abas)) / ncol(Data_Sipsa_Abas) < 0.5, colSums(is.na(Data_Sipsa_Abas)) / nrow(Data_Sipsa_Abas) < 0.5 ]
    Data_Sipsa_Abas=Data_Sipsa_Abas[-1,]
    Data_Sipsa_Abas$Cantidad_KG=as.numeric(Data_Sipsa_Abas$Cantidad_KG)
    
    
    Data_Sipsa_Abas$Fecha <- convertir_fechas_vector(Data_Sipsa_Abas$Fecha)
    
    
    # -- Seleciona la ciudad de interés- ABSATECIMIENTO
    
    
    asociar_ciudad_mercado_Abast <- function(ciudad, df) {
      opciones_mercado <- unique(df$Ciudad_Mercado[grep(ciudad, df$Ciudad_Mercado, ignore.case = TRUE)])
      
      if (length(opciones_mercado) == 0) {
        print("Error: No se encontraron opciones de mercado para la ciudad especificada en abastecimiento. No es posible filtrar en la ciudad digitada.")
        opciones_mercado=NULL
      } else {
        return(opciones_mercado)
      }
    }
    
    
    asociar_ciudad_entrada_usuario_Abas <- function(entrada_usuario, lista_ciudades, df) {
      
      similitudes <- sapply(lista_ciudades, function(ciudad) stringdist::stringdist(entrada_usuario, ciudad, method = "jw"))
      # Encontramos la ciudad más cercana en términos de texto a la entrada del usuario
      ciudad_mas_cercana <- lista_ciudades[which.min(similitudes)]
      # Llamamos a la función asociar_ciudad_mercado con la ciudad encontrada
      opciones_ciudad <- asociar_ciudad_mercado_Abast(ciudad_mas_cercana, df)
      return(opciones_ciudad)
    }
    
    
    Mercados_ciudad_Abas=asociar_ciudad_entrada_usuario_Abas(Ciudad,ciudades_colombia,Data_Sipsa_Abas)
    
    
    
    if(!is.null(Mercados_ciudad)) {
      
      Data_Sipsa_Abas = Data_Sipsa_Abas %>% filter(Ciudad_Mercado %in% Mercados_ciudad_Abas) } else 
      {cat("Error,",Ciudad," aún no está en los datos públicos de abastecimiento SIPSA",sep="")}
    
    
    # ---------------------------cargar ciudades atuomáticamente
    
    # Crear una lista para almacenar los resultados de cada mercado
    resultados_mercados <- list()
    
    # Iterar sobre cada mercado en Mercados_ciudad_Abas
    for (mercado in Mercados_ciudad_Abas) {
      data_mercado <- subset(Data_Sipsa_Abas, Ciudad_Mercado == mercado)
      Alimentos_Mercado_Abas <- levels(as.factor(data_mercado$Alimento))
      
      Data_Mercado_Unico <- data.frame(Alimentos_Mercado_Abas, Total_Mercado = NA)
      colnames(Data_Mercado_Unico) <- c("Alimento_abs", paste0("Total_", gsub(", ", "_", mercado)))
      
      for (i in 1:length(Alimentos_Mercado_Abas)) {
        Datos_Alimento <- subset(data_mercado, Alimento == Alimentos_Mercado_Abas[i])
        Datos_Alimento$Cantidad_KG <- as.numeric(Datos_Alimento$Cantidad_KG)  # Convertir a tipo numérico
        
        Data_Mercado_Unico[i, paste0("Total_", gsub(", ", "_", mercado))] <- sum(Datos_Alimento$Cantidad_KG, na.rm = TRUE)
      }
      resultados_mercados[[mercado]] <- Data_Mercado_Unico
    }
    
    
    Data_Sipsa_Abas_Unicos <- Reduce(function(x, y) merge(x, y, by = "Alimento_abs", all = TRUE), resultados_mercados);Data_Sipsa_Abas_Unicos[is.na(Data_Sipsa_Abas_Unicos)] <- 0
    
    
    # Obtener las columnas numéricas para calcular la suma por fila
    columnas_numericas <- sapply(Data_Sipsa_Abas_Unicos, is.numeric)
    
    # Calcular la suma por fila en las columnas numéricas
    Data_Sipsa_Abas_Unicos$Total <- rowSums(Data_Sipsa_Abas_Unicos[columnas_numericas], na.rm = TRUE)
    
    Data_Sipsa_Abas_Unicos=Data_Sipsa_Abas_Unicos[,c("Alimento_abs","Total")]
    
    #----# Salida: Data_Sipsa_Abas_Unicos #----#
    
  } else {Data_Sipsa_Abas_Unicos=NULL}
  
  #------------------------------------------------------#
  #                       CARGA DE MAPEOS: Datos insumo  #  ✔ SIMPLIFICADA Y ASEGURADA
  #------------------------------------------------------#
  
  # LA CARGA DE DATOS NO SE MUESTRA EN EL AMBIENTE GLOBAL
  
  
  # Crear un nuevo ambiente local para sólo los datos
  datos_env <- new.env()
  
  # Cargar los datos en el ambiente local
  data(Primer_Criterio_Lista_Alimentos, package = "Foodprice", envir = datos_env)
  
  
  # SIPSA (precios mayoristas-abastecimiento)
  data(Mapeo_Precios_Abs, package = "Foodprice",envir=datos_env)
  
  
  
  # SIPSA-TCAC (Códigos de sipsa a  Composición de Alimentos Colombianos)
  
  data(Mapeo_Sipsa_TCAC, package = "Foodprice",envir=datos_env);colnames(Mapeo_Sipsa_TCAC) = c("Alimento", "Codigo_TCAC")
  Mapeo_Sipsa_TCAC1=Mapeo_Sipsa_TCAC
  Mapeo_Sipsa_TCAC = Mapeo_Sipsa_TCAC %>% filter(Codigo_TCAC %in% setdiff(levels(as.factor(Mapeo_Sipsa_TCAC$Codigo_TCAC)), "EX000"))
  
  # TCAC-GABAS (TCAC con Guías Alimentarias y SIN composición )
  data(Mapeo_Sipsa_TCAC_GABAS_Grupos, package = "Foodprice",envir=datos_env)
  Variables_Necesarias = c("codigo", "Nombre del Alimento","Grupos  GABAS", "Subgrupos  GABAS",  "Grupo TCAC");Mapeo_Sipsa_TCAC_GABAS_Grupos = Mapeo_Sipsa_TCAC_GABAS_Grupos[Variables_Necesarias]
  colnames(Mapeo_Sipsa_TCAC_GABAS_Grupos) = c("Cod_TCAC", "Alimento", "Grupo_GABAS", "Subgrupo_GABAS", "Grupo_TCAC")
  
  
  #--------               -------#
  #    Criterios de exclusión    #
  #-----                  -------#
  
  data(Primer_Criterio_Lista_Alimentos, package = "Foodprice",envir=datos_env)
  
  
  #--------               -------#
  #    Composición nutricional   #
  #-----                  -------#
  
  
  data(Mapeo_Sipsa_TCAC_Carga_2, package = "Foodprice",envir=datos_env)
  
  
#------------------------------------------------------#
  #                       seleción de datos              #  ✔ SIMPLIFICADA Y ASEGURADA
  #------------------------------------------------------#
  
  Micro_Macro_Nutrientes_Necesarios = c("codigo", "Nombre del Alimento", "% de parte comestible", "Factor de conversión", "Energia (Kcal)", "Proteina (g)", "Carbohidratos Totales (g)", "Lipidos (g)", "Calcio (mg)",
                                        "Zinc (mg)", "Hierro (mg)", "Magnesio (mg)", "Fosforo (mg)", "Vitamina C (mg)", "Tiamina (mg)", "Riboflavina (mg)",
                                        "Niacina (mg)", "Folatos (mcg)", "Vitamina B12 (mcg)", "Vitamina A (ER)", "Sodio (mg)", "Micr sin inf (por alimento)")
  
  Sipsa_TCAC=Mapeo_Sipsa_TCAC_Carga_2[Micro_Macro_Nutrientes_Necesarios];colnames(Sipsa_TCAC)[1] = "Cod_TCAC";colnames(Sipsa_TCAC)[2] = "Alimento_TCAC"
  
  Data_abs_precios_Sipsa=Data_Sipsa_Precios_Unicos

  if (!is.null(Percentil_Abast)){
    # Asignación del valor de abastecimiento en cada caso
    Data_abs_precios_Sipsa = merge(Data_Sipsa_Precios_Unicos, Mapeo_Precios_Abs, by = "Alimento", all.x = TRUE)
    Data_abs_precios_Sipsa = merge(Data_Sipsa_Abas_Unicos, Data_abs_precios_Sipsa,by = "Alimento_abs", all.x = TRUE)
    # Selección de las variables de interés
    Data_abs_precios_Sipsa = Data_abs_precios_Sipsa[c("Alimento", "Precio_kg", "Total")]
    Data_abs_precios_Sipsa = Data_abs_precios_Sipsa[order(Data_abs_precios_Sipsa$Alimento),]
    colnames(Data_abs_precios_Sipsa) = c("Alimento",paste0("Precio_kg_", Mes), paste0("Total_", Mes))
  }
  else {
    # Selección de las variables de interés
    Data_abs_precios_Sipsa = Data_abs_precios_Sipsa[c("Alimento", "Precio_kg")]
    Data_abs_precios_Sipsa = Data_abs_precios_Sipsa[order(Data_abs_precios_Sipsa$Alimento),]
    colnames(Data_abs_precios_Sipsa) = c("Alimento",paste0("Precio_kg_", Mes))
  }
  
  
  #------------------------------------------------------#
  #                       criterios de exclusión         #  ✔ SIMPLIFICADA Y ASEGURADA
  #------------------------------------------------------#
  
  if (!is.null(Percentil_Abast)){
    
    Data_abs_precios_Sipsa_ABS=Data_abs_precios_Sipsa[,c("Alimento",paste0("Total_",Mes))]
    
    
    
    Alimentos_Exclu = c("Aceite vegetal mezcla", "Huevo rojo A", "Huevo rojo AA","Huevo rojo extra", "Leche pasteurizada (1.1 L)", "Queso doble crema",
                        "Queso cuajada", "Queso Caquetá", "Pollo entero con visceras","Lomitos de atún en lata", "Galletas saladas", "Sardinas en lata","Chocolate amargo")
    
    Alimentos_Inclu = setdiff(Data_abs_precios_Sipsa_ABS$Alimento, Alimentos_Exclu)
    criterio_2 = Data_abs_precios_Sipsa_ABS %>% filter(Alimento %in% Alimentos_Inclu)
    
    # Eliminar niveles NA de abastecimiento (Flujos de carga nulos)
    criterio_2 = criterio_2 %>% drop_na(paste0("Total_",Mes))
    
    # Calcular cuantiles
    quant = quantile(criterio_2[,2],probs = Percentil_Abast, na.rm = TRUE)
    
    # Eliminar los alimentos cuyo flujo de carga está abajo del percentil 25
    criterio_2 = criterio_2[criterio_2[,2] < quant,]
    
    
    #  Primer  criterio de exclusión: Nutrición    #
    
    
    Alimentos_Exclu_Criterio_1 = Primer_Criterio_Lista_Alimentos[Primer_Criterio_Lista_Alimentos$`COD. TCAC` == "EX000","Alimento"]
    
    
    # Lista depurada con base en los dos criterios #
    
    
    # Abastecimiento nulo
    Alimentos_NA = Data_abs_precios_Sipsa_ABS %>% filter(is.na(get(paste0("Total_", Mes))))
    
    # Construir el vector con la totalidad de alimentos excluidos
    # (criterio 1, criterio 2, flujo de carga nulo y exclusiones ad hoc)
    
    Alimentos_Excluidos_Criterio_1 = Alimentos_Exclu_Criterio_1["Alimento"]
    Alimentos_Exclu_Criterio_2 = criterio_2$Alimento
    Alimentos_Excluidos_Na = Alimentos_NA$Alimento
    Alimentos_Excluidos = c(Alimentos_Excluidos_Criterio_1, Alimentos_Exclu_Criterio_2, Alimentos_Excluidos_Na,"Queso Caquetá")
    
    # Exclusión de los alimentos y construcción de la lista definitiva
    Lista_Alimentos_Definitiva = Data_abs_precios_Sipsa_ABS %>% filter(Alimento %in% setdiff(levels(as.factor(Data_abs_precios_Sipsa_ABS$Alimento)), Alimentos_Excluidos))
    
  }
  else {
    
    
    #--------                               -------#
    #  Primer  criterio de exclusión: Nutrición    # 
    #-----                                  -------#
    
    Alimentos_Exclu_Criterio_1 = Primer_Criterio_Lista_Alimentos[Primer_Criterio_Lista_Alimentos$`COD. TCAC` == "EX000","Alimento"]
    
    #--------                               -------#
    # Lista depurada con base en los dos criterios #
    #-----                                  -------#
    
    # Construir el vector con la totalidad de alimentos excluidos
    # (criterio 1, criterio 2, flujo de carga nulo y exclusiones ad hoc)
    
    Alimentos_Excluidos_Criterio_1 = Alimentos_Exclu_Criterio_1["Alimento"]
    Alimentos_Excluidos = c(Alimentos_Excluidos_Criterio_1,"Queso Caquetá")
    
    # Exclusión de los alimentos y construcción de la lista definitiva
    Lista_Alimentos_Definitiva = Data_abs_precios_Sipsa %>% filter(Alimento %in% setdiff(levels(as.factor(Data_abs_precios_Sipsa$Alimento)), Alimentos_Excluidos))
    
    
  }
  
  # ---------------------------#
  #   Marginalización          #    ✔ SIMPLIFICADA Y ASEGURADA
  #---------------------------#
  
  
  #-- Construcción data por grupos (SIPSA)
  
  Grupos_Alimentos_Sipsa = Data_Sipsa_Precios[c("Alimento", "Grupo")];Grupos_Alimentos_Sipsa = Grupos_Alimentos_Sipsa[!duplicated(Grupos_Alimentos_Sipsa), ]
  Precios_Grupos_SIPSA = merge(Data_Sipsa_Precios_Unicos, Grupos_Alimentos_Sipsa,by = "Alimento", all.x = TRUE, all.y = FALSE, no.dups = TRUE)
  Precios_Grupos_SIPSA$Grupo <- toupper(Precios_Grupos_SIPSA$Grupo)

  
  #--------                    -------#
  #  Margenes de comercialziación     #  ✔ SIMPLIFICADA Y ASEGURADA
  #-----                       -------#

# Margenes de grupos en general para COL
categorias <- c("CARNES", "FRUTAS", "GRANOS Y CEREALES", "LACTEOS Y HUEVOS", 
                "PESCADOS", "PROCESADOS", "TUBERCULOS, RAICES Y PLATANOS", "VERDURAS Y HORTALIZAS")

 if (!is.null(Margenes)) {valores=Margenes} else {valores <- c(4.925515, 32.154734, 21.770773, 26.226295, 17.150887, 6.884347, 76.380988, 54.096494)}
Df_grupos_marg=data.frame(Grupos=categorias,Valor=valores);grupos_margenes <- levels(as.factor(Precios_Grupos_SIPSA$Grupo))

# Encontrar índices de los grupos en el dataframe
indices <- match(grupos_margenes, Df_grupos_marg$Grupos)
# Asociar los valores correspondientes a los grupos
valores_asociados <- Df_grupos_marg$Valor[indices]

Margenes_Historicos <- data.frame(Grupo = grupos_margenes, margen_medio=valores_asociados);colnames(Margenes_Historicos)=c("Grupo", "margen_medio")

  
  # -----------------------------------------------------------------#
  #                 Estimación precios minoristas                    #
  #------------------------------------------------------------------#
  
    
  precios_mayoristas_grupos_margenes <- merge(Precios_Grupos_SIPSA,
                                                Margenes_Historicos[c("Grupo", "margen_medio")],
                                                by = "Grupo", all.x = TRUE)
  precios_mayoristas_grupos_margenes$Precio_minorista_kg <- precios_mayoristas_grupos_margenes$Precio_kg * (1 + precios_mayoristas_grupos_margenes$margen_medio/100)
    
    

  
  
  Estimación_Precios_Minoristas <- precios_mayoristas_grupos_margenes %>%
    filter(Alimento %in% Lista_Alimentos_Definitiva$Alimento) # exclusión criterios 1 y 2
  
  
  #--------                    -------  #
  # Mapeo: Precios con contenidos nutri #
  #-----                       -------  #

precios_kg <- Estimación_Precios_Minoristas[c("Alimento", "Precio_minorista_kg")]
  colnames(Mapeo_Sipsa_TCAC) <- c("Alimento", "Cod_TCAC")
  
  dataset_sim <- merge(precios_kg, Mapeo_Sipsa_TCAC, by = "Alimento", all.x = TRUE)
  dataset_sim <- merge(dataset_sim, Sipsa_TCAC, by = "Cod_TCAC", all.x = TRUE)
  
  # Subset de alimentos con unidades de medida no comparables
  
  # Subset 2: alimentos en unidades de medida distintas a 100g
  dataset_sim_2 <- dataset_sim %>%
    filter(Alimento %in% c("Aceite vegetal mezcla", "Huevo rojo A", "Huevo rojo AA","Huevo rojo extra"))
  
  # Subset 1: alimentos en 100g
  dataset_sim_1 <- dataset_sim %>%
    filter(Alimento %in% setdiff(levels(as.factor(dataset_sim$Alimento)), c("Aceite vegetal mezcla","Huevo rojo A", "Huevo rojo AA","Huevo rojo extra")))
  
  # Subset 1
  dataset_sim_1$Serving <- rep(100, nrow(dataset_sim_1))
  dataset_sim_1$Precio_100g <- dataset_sim_1$Precio_minorista_kg/10
  
  # Subset 2
  
  aux_dataset <- data.frame(matrix(nrow = 4, ncol = 3))
  colnames(aux_dataset) <- c("Alimento", "Serving", "Factor_gramos")
  
  aux_dataset$Alimento <- c("Aceite vegetal mezcla", "Huevo rojo A", "Huevo rojo AA", "Huevo rojo extra")
  
  aux_dataset$Serving <- c("1 Litro", "1 Unidad", "1 Unidad", "1 Unidad")
  
  aux_dataset$Factor_gramos <- c(100/920, 100/50, 100/60, 100/67)
  
  dataset_sim_2 <- merge(dataset_sim_2, aux_dataset, by = "Alimento", all.x = TRUE)
  
  dataset_sim_2$Precio_100g <- dataset_sim_2$Precio_minorista_kg * dataset_sim_2$Factor_gramos
  
  dataset_sim_2 <- dataset_sim_2[colnames(dataset_sim_1)]
  
  # Base de datos de precios (combinación de ambos subsets)
  dataset_sim <- rbind(dataset_sim_1, dataset_sim_2)
  dataset_sim$Serving <- rep(100, nrow(dataset_sim))
  
  #--------                    -------  #
  #     Cálculo de precios ajustados    #
  #-----                       -------  #
  
  # omitir los alimentos sin dato sobre la parte comestible
  dataset_sim$`Lipidos (g)` = as.numeric(dataset_sim$`Lipidos (g)`)
  
  dataset_sim["Factor de conversión"][dataset_sim["Factor de conversión"] == "SD"] = NA
  dataset_sim = dataset_sim[!is.na(dataset_sim$`Factor de conversión`),]
  dataset_sim$`Factor de conversión` = as.numeric(dataset_sim$`Factor de conversión`)
  
  # calculo del precio ajustado
  dataset_sim$Precio_100g_ajust = dataset_sim$Precio_100g*dataset_sim$`Factor de conversión`
  
  
  #--------                    -------  #
  #     Composición nutricional         #
  #-----                       -------  #
  
  # reemplazar los valores NA de los macronutrientes por 0
  macro = c("Proteina (g)", "Carbohidratos Totales (g)", "Lipidos (g)")
  for (k in 1:length(macro)) {
    dataset_sim[macro[k]][is.na(dataset_sim[macro[k]])] = "0"
  }
  
  #informaci?n faltante sobre micronutrientes
  dataset_sim$`Micr sin inf (por alimento)` = rowSums(is.na(dataset_sim))
  
  dataset_sim = dataset_sim %>% mutate(Int = cut(dataset_sim$`Micr sin inf (por alimento)`, c(c(1,4, 7), Inf),
                                                 right = FALSE))
  dataset_sim$Int = as.character(dataset_sim$Int);dataset_sim["Int"][is.na(dataset_sim["Int"])] = "0";dataset_sim %>% count("Int")
  
  # por simplicidad, om?tase los alimentos con informaci?n faltante
  
  dataset_sim = dataset_sim[complete.cases(dataset_sim), ]
  
  drop = c("Precio_minorista_kg", "Alimento_TCAC", "Precio_100g", "% de parte comestible", "Factor de conversión",
           "Micr sin inf (por alimento)", "Int");dataset_sim = dataset_sim[setdiff(colnames(dataset_sim), drop)]
  
  dataset_sim = dataset_sim[c(1,2, 20, 21, 3:19)];dataset_sim = dataset_sim[order(dataset_sim$Alimento),];dataset_sim[1,3]="1 Litro";Datos_Insumo_Modelos=dataset_sim
  
  Datos_Insumo_Modelos[1,3]=100
  
  colnames(Datos_Insumo_Modelos)=c("Cod_TCAC", "Alimento", "Serving", "Precio_100g_ajust",  "Energia","Proteina","Carbohidratos","Lipidos",  "Calcio",  "Zinc", "Hierro", "Magnesio","Fosforo","VitaminaC", "Tiamina", "Riboflavina","Niacina", "Folatos", "VitaminaB12", "VitaminaA","Sodio")
  #--------------------------------------------------- Salida principal 2 ----------------------------- Datos_Insumo_Modelos ----------------------------------------#
 
  
  # -----------------------------------------------------------------#
  #                         Alimentos faltantes                      #
  #------------------------------------------------------------------#
  
  
  if (!is.null(Ingreso_Alimentos)) {
    alimentos_faltantes <- Alimentos_Sipsa_Precios[!(Alimentos_Sipsa_Precios %in% Mapeo_Sipsa_TCAC1$Alimento)]
    
    if (is.data.frame(Ingreso_Alimentos)) {
      # Si es un data frame, buscar los alimentos en la columna 'Alimento'
      alimentos_encontrados <- Ingreso_Alimentos$Alimento[Ingreso_Alimentos$Alimento %in% alimentos_faltantes]
    } else {
      # Si es un vector, buscar los alimentos directamente
      alimentos_encontrados <- Ingreso_Alimentos[Ingreso_Alimentos %in% alimentos_faltantes]
    }
    
    alimentos_faltantes <- alimentos_faltantes[!(alimentos_faltantes %in% alimentos_encontrados)]
    alimentos_a_eliminar <- c("Ajo importado", "cilantro", "linaza molida", "jengibre", "tomillo", "perejil liso", "crespo", "Ajo","Acelga")
    
    # Eliminar alimentos específicos del vector
    alimentos_faltantes <- alimentos_faltantes[!grepl(paste(alimentos_a_eliminar, collapse = "|"), alimentos_faltantes, ignore.case = TRUE)]
    
    alimentos_faltantes <- alimentos_faltantes[!(alimentos_faltantes %in% alimentos_encontrados)]
    
    Datos_Insumo_Modelos <- rbind(Ingreso_Alimentos, Datos_Insumo_Modelos)
  } else {
    alimentos_faltantes <- Alimentos_Sipsa_Precios[!(Alimentos_Sipsa_Precios %in% Mapeo_Sipsa_TCAC1$Alimento)]
    # Palabras a eliminar
    alimentos_a_eliminar <- c("Ajo importado", "cilantro", "linaza molida", "jengibre", "tomillo", "perejil liso", "crespo", "Ajo","Acelga")
    
    # Eliminar alimentos específicos del vector
    alimentos_faltantes <- alimentos_faltantes[!grepl(paste(alimentos_a_eliminar, collapse = "|"), alimentos_faltantes, ignore.case = TRUE)]
    
  }
  
  
  
  mensaje <- paste("En la ciudad de", Ciudad, "del año", Año, "y mes", Mes, ", se omitieron los siguientes alimentos por falta de información nutricional " , length(alimentos_faltantes) ," :", paste(alimentos_faltantes, collapse = ", "), ". Si conoce la información de estos, utilice el parámetro opcional llamado 'Ingreso_Alimentos' para ingresarlos")
  #---- alimentos por falta de nutrición cat(mensaje)
  
  
  
  
  # -----------------------------------------------------------------#
  #               MAPEOS Y GRUPOS PARA EL MODELO CORD                #
  #------------------------------------------------------------------#
  
  
  
  # Función de recodificación de subgrupos
  f_gabas_1 <- function(a) {
    a$Grupo_GABAS[a$Grupo_GABAS == "AZUCARES"] <- "Azúcares"
    a$Grupo_GABAS[a$Grupo_GABAS == "CARNES, HUEVOS, LEGUMINOSAS SECAS, FRUTOS SECOS Y SEMILLAS"] <- "Carnes, huevos y leguminosas"
    a$Grupo_GABAS[a$Grupo_GABAS == "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS"] <- "Cereales y raíces"
    a$Grupo_GABAS[a$Grupo_GABAS == "FRUTAS Y VERDURAS"] <- "Frutas y verduras"
    a$Grupo_GABAS[a$Grupo_GABAS == "GRASAS"] <- "Grasas"
    a$Grupo_GABAS[a$Grupo_GABAS == "LECHE Y PRODUCTOS LACTEOS"] <- "Lácteos"
    a$Grupo_GABAS[a$Grupo_GABAS == "SIN CATEGORIA"] <- "Sin categoría"
    return(a)
  }
  
  # Mapear con base en COD TCAC los alimentos y subgrupos además de mapear los intercambios de EER
  Datos_MOD3 <- merge(Datos_Insumo_Modelos, intercambio_gramos[c("Cod_TCAC", "Intercambio_g")], by = "Cod_TCAC", all.x = TRUE) %>%
    distinct()  # Quitar duplicados
  
  # Crear una nueva columna con el precio por intercambio unitario
  Datos_MOD3 <- Datos_MOD3 %>%
    mutate(Precio_INT = (Precio_100g_ajust/as.numeric(Serving)) * Intercambio_g)
  
  # Recuperar grupos y subgrupos GABAS
  Datos_MOD3 <- merge(Datos_MOD3, TCAC[c("Cod_TCAC", "Grupo_GABAS", "Subgrupo_GABAS")], by = "Cod_TCAC") %>%
    f_gabas_1()
  
  # Extraer subgrupos y pasarlos a grupos
  Datos_MOD3 <- Datos_MOD3 %>%
    mutate(Subgrupo_GABAS = case_when(
      Subgrupo_GABAS == "FRUTAS" ~ "Frutas",
      Subgrupo_GABAS == "VERDURAS" ~ "Verduras",
      Subgrupo_GABAS == "CARNES MAGRAS CRUDAS" ~ "Carnes",
      Subgrupo_GABAS == "LEGUMINOSAS COCIDAS Y MEZCLAS VEGETALES COCIDAS" ~ "Leguminosas",
      Subgrupo_GABAS == "TUBÉRCULOS" ~ "Tuberculos",
      Subgrupo_GABAS == "RAÍCES" ~ "Raices",
      Subgrupo_GABAS == "CEREALES" ~ "Cereales",
      TRUE ~ Grupo_GABAS
    ));
   # Datos_MOD3 <- Datos_MOD3[, -which(names(Datos_MOD3) == "Subgrupo_GABAS")]
  
  colnames(Datos_MOD3)=c("Cod_TCAC", "Alimento", "Serving", "Precio_100g_ajust",  "Energia","Proteina","Carbohidratos","Lipidos",  "Calcio",  "Zinc", "Hierro", "Magnesio","Fosforo","VitaminaC", "Tiamina", "Riboflavina","Niacina", "Folatos", "VitaminaB12", "VitaminaA","Sodio","Intercambios_g","Precio_INT","Grupo","Subgrupo")
  
  # Orden de salida
  
  Datos_MOD3 <- Datos_MOD3 %>%
    select(Cod_TCAC, Alimento, Serving, Precio_100g_ajust,Intercambios_g, Precio_INT, Grupo,Subgrupo, Energia:VitaminaB12,
           VitaminaA, Sodio)
  
  
  #------------------------------------------------------------------------------------------#
  #                       ASGINACIÓN EN EL ENTORNO GLOBAL                                   #
  #-----------------------------------------------------------------------------------------#
  
  #assign(paste0("Datos_",Año,"_",Mes_Num,"_",Ciudad),Datos_MOD3,envir = globalenv())
  
  print(paste("✔",Ciudad,"_" ,Año,"_" ,Mes))
  
  #cat("\n")
  # if(length(warnings())<100) {cat("Depuración de datos exitosa", "\n")} else {cat("Cantidad de errores encontrados:",length(warnings()), "\n")}
  #cat("\n")
  
  
  #------------------------------------------------------------------------------------------#
  #                       FIN DEL PRIMER MÓDULO COMO FUNCIÓN                                 #
  #-----------------------------------------------------------------------------------------#
  
  
  return(invisible(Datos_MOD3))
  
  
}

