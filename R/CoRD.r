

#---------------------------------------------------------------------------------------#
#                            QUINTA ETAPA: TERCER   MODELO FEMENINO                    #
#-------------------------------------------------------------------------------------#


remove.packages("Foodprice")
devtools::install_github("Foodprice/Foodprice");library(Foodprice)
DatosCol(Mes="Enero",Año=2015,Ciudad="Cali")

Datos_Insumo=Datos_Insumo_Modelos_2015_Enero_Cali
 
#se excluyen los alimentos sin categorías
  Datos_Insumo = Datos_Insumo %>% filter(!Grupo %in% "Sin categoría")


# Definir códigos y alimentos a eliminar
codigos_a_eliminar <- c("L017", "D020", "K018", "K018")
alimentos_a_eliminar <- c("Carne de cerdo, espinazo", "Yuca ICA", "Papa Betina", "Papa única")

# Filtrar el dataframe
Datos_Insumo_filtrado <- Datos_Insumo %>%
  filter(!(Cod_TCAC %in% codigos_a_eliminar) & !(Alimento %in% alimentos_a_eliminar))

#---------------------------------------------------------------------------------------#
#                            QUINTA ETAPA: TERCER   MODELO FEMENINO                    #
#-------------------------------------------------------------------------------------#

# Requerimiento y edad
Req_Int_F=subset(Req_Int,Sexo==1)
Edad =levels(as.factor(Req_Int_F$Edad))

#---------------- VALIDACIÓN Y SELECIÓN DE GRUPOS----------------


# Extraer grupos
Grupos_Insumo=levels(as.factor(Datos_Insumo$Grupo)) # GRupos de insumo
Grupos_Cantidad_Sel <- unique(cantidad_alimentos_seleccionar$Grupo_GABAS) #grupos de cantidad
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



if(length(grupos_faltantes>0)){
paste("Cuidado: Hay grupos no comunes entre los grupos de datos insumo y la cantidad a selecionar de estos:",paste(grupos_faltantes,collapse = ", "))
}
# Ordenar los nombrespara el modelo
DF_Nutrientes_ALimentos <- DF_Nutrientes_ALimentos %>% select(any_of(nombres_comunes))


if(length(grupos_faltantes>0)){
paste("Se trabajará entonces sólo con los grupos iguales, estos son:",paste(Grupos_comunes,collapse = ", "))
}

# Validar la intersección entre grupos de req y los demás
Grupos_comunes_req <- intersect(Grupos_comunes, grupos_req)

grupos_faltantes_req=union(setdiff(Grupos_comunes, grupos_req),setdiff(grupos_req,Grupos_comunes))

if(length(grupos_faltantes_req>0)){
paste("Cuidado: Hay grupos no comunes entre los grupos de datos insumo, la cantidad a selecionar y los requerimientos, estos son:",paste(grupos_faltantes,collapse = ", "))
}


if(length(grupos_faltantes_req>0)){
paste("Se trabajará entonces sólo con los grupos iguales en los tres vectores de grupos, estos son:",paste(Grupos_comunes,collapse = ", "))
}

# ---------------- Finalziación de validación de grupos



#Eliminar
Grupos_comunes=setdiff(Grupos_comunes, c("Carnes, huevos y leguminosas", "Cereales y raíces"))


# Cantidad a selcionar, sólo los comunes

Selec_comunes <- subset(cantidad_alimentos_seleccionar, Grupo_GABAS %in% Grupos_comunes)


save(cantidad_alimentos_seleccionar,file="Cantidad_selec.rda")






