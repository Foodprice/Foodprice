# Foodprice

<p align="center">
<a name="top" href="#"> <img src="https://media2.giphy.com/media/rGlAZysKBcjRCkAX7S/giphy.gif" alt="mf-dots" height="40%" width="60%"/> </a>

# 

### :computer: **Introducción:**

En el presente repositorio encontrará la versión 1.0.0 del paquete Foodprice, el cúal alberga cuatro funciones:

- **Datos_dane_col:** La función Datos_dane_col ofrece la posibilidad de obtener y manejar datos de precios mayoristas SIPSA del Departamento Administrativo Nacional de Estadística (DANE) en Colombia, adecuados para un mes, año y ciudad particulares. Su resultado principal se centra en las estimaciones de los precios minoristas, vinculando los nutrientes correspondientes a cada alimento.

- **Modelo_1:** Calcula el alimento y su costo minimo para una dieta que garantice suficiente energía en el día. Requiere un parámetro obligatorio y dos opcionales.
- **Modelo_2:** calcula los alimentos para una dieta adecuada en nutrientes y su costo mínimo diario. Requiere un parámetro obligatorio y dos opcionales.
- **Modelo_3:** calcula los alimentos para una dieta saludable y su costo mínimo diario. Requiere un parámetro obligatorio y tres opcionales.
#

### :wrench: **Instrucciones de instalación y uso:**

**1. Instalación y carga**
            
Instale y cargue el paquete alojado en el presente repositorio ejecutando en R:            

```
devtools::install_github("Foodprice/Foodprice");library(Foodprice)

```

**2. Información**

Para conocer la información, el uso y las condiciones de funcionamiento de cada función use en R la función "help" o "??" para acceder a la documentación:

```
help(Datos_dane_col)

# ó

??Datos_dane_col

```
#

### :page_with_curl: **Actualizaciónes**

Aún no se han encontrado actualizaciones

#        

La versión actual del paquete es:  1.0.0
