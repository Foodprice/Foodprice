# Foodprice

<p align="center">
<a name="top" href="#"> <img src="https://media2.giphy.com/media/rGlAZysKBcjRCkAX7S/giphy.gif" alt="mf-dots" height="40%" width="60%"/> </a>

# 

### :computer: **Introducción:**

En el presente repositorio encontrará la versión 0.0.1 del paquete Foodprice, el cúal alberga cinco módulos:

- **Librerias:** Módulo encargado de instalar y llamar las librerias necesesarias al paquete.
  
- **Módulo_1:** Módulo encargado de descargar y cargar los datos necesarios desde la página del DANE (Precios mayorístas de sipsa y el abastecimiento en caso de ser requerido); Además se encarga de la depuración y estructuración de dichos datos, la estimación de precios minorístas con base en los márgenes de comercialziación y el mapeo con los nutrientes. 

- **Módulo_2 :** Módulo encargado de la carga de los requerimientos para los tres modelos.
  
- **Módulo_3:** Salida del primer modelo: Dieta suficiente en energía.

- **Módulo_4:** Salida del segundo modelo: Dieta adecuada en nutrientes.

- **Módulo_5:** Salida del tercer modelo: Dieta saludable.

#

### :wrench: **Instrucciones de instalación y uso:**

**1. Instalación**
            
Instale y cargue el paquete alojado en el presente repositorio ejecutando en R:            

```
devtools::install_github("JuanArchis/Foodprice");library(Foodprice)

```
#


**2. Argumentos del objeto**

El objeto cuenta con dos tipos de parámetros: necesarios y opcionales, los necesarios son:

- **Mes:** Mes o muestra de estudio, debe estar en formato texto, por ejemplo: "Julio"
- **Año:** Año de estudio,su formato debe ser númerico; por ejemplo, 2022.
- **Ciudad:** Ciudad de estudio, debe estar en formato texto, por ejemplo: "Cali".

Si no se especifícan el objeto no funcionará. Por otra parte, los opcionales son:

- **Margenes:** Es un vector de tamaño 8 que contiene los márgenes de comercialización de cada grupo de alimento con los cuales se estimarán los precios minorístas. Si no se espécifica este parámetro, los márgenes serán los promedios estimados con mercar.
- **Data_Model:** Es un data frame el cual servirá como insumo a los tres modelos, si no se especifíca los modelos se calcularán con los datos estimados del módulo 1.
- **Percentil_Abast:** Valor entre 0 y 1 el cual indica el percentil de exlución de los alimentos con poca comercialización. Si no es pecifica no carga ni depura los datos de abastecimiento del DANE.

#
  
**3. Instrucciones de uso**

El paquete es una clase, por lo cual se debe crear una asginación que cuente con la función "Paquete_Foodprice_ver_0_0_1" y sus argumentos de entrada. Lo anterior permite crear varias asignaciones indicando diferente ciudades, año y mes de estudio.

#

**4.1 Crear asignación**
            
Creando una asginación que contendrá la depuración del objeto. En ella se llama la clase y se indican los argumentos de esta; por ejemplo, sea la asginación "Prueba1":  
            
```
Prueba1=Foodprice::Paquete_Foodprice_ver_0_0_1$new(Mes="Septiembre",Año=2022,Ciudad="Cali", Margenes =c(4.925515,32.154734,21.770773,26.226295,17.150887,6.884347,76.380988,54.096494))
```
            
**4.2 Cargar módulos**

```
Prueba1$Librerias()
Prueba1$Módulo_1()
Prueba1$Módulo_2()
Prueba1$Módulo_3()
Prueba1$Módulo_4()
Prueba1$Módulo_5()

```
#

## :bangbang: Aspectos a tener en cuenta

- Falta generalizar el módulo 1 para las demás ciudades de sipsa (Incluye la opción de ingresar los alimentos faltantes en la ciudad establecida).
- Flata generalziar la estructura de depuración para los años diferentes al 2022.
- Falta como entrada opcional los requerimientos a los tres modelos.


#
            
En la versión actual sólo se encuentra disponible la ciudad  de Cali y el Año 2022.
