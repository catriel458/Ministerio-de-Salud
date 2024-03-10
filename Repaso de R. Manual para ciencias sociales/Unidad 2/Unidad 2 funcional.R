#data wrangling: proceso de "domar" los datos. Ordenamiento y procesamiento de los datos para poder usarlos como nosotros queramos luego.
# tecnicas de wangling:
library(tidyverse)
atencion_ciudadano<-read.csv("http://bitsandbricks.github.io/data/gcba_suaci_barrios.csv")
#analizamos brevemente el dataset
dim(atencion_ciudadano) # 57431 filas y 5 columnas o variables (periodo, rubro, tipo_prestacion, barrio,total)
head(atencion_ciudadano) # 6 primeras filas
names(atencion_ciudadano) # nombre de las variables.
# usamos funcion str que nos indica la ESTRUCTURA de dataframe que no conocemos
str(atencion_ciudadano) # VARIABLES TIPO INT: trabajan con numeros enteros (integers) restp de variables son factores, reciben el nombre de factores (usan datos chr de caracter)
#str nos dice que estamos trabajando con un dataframe (se puede usar para cualquier objeto de R)
# las variables categoricas reciben el nombre de factores.
# si no se pone nada la funciÃ³n READ.CSV interpreta a todas las variables o columnas que tiene texto como factores.
#para avisarle que no lo haga hay que usar un nuevo parametro: stringsAsFactors de la siguiente manera
# "mis_datos"<-read.csv("archivo con mis datos, stringsAsFactors = FALSE)
#En general es buena idea evitar que los campos de texto se asuman como factores, pero en Ã©ste caso estÃ¡ bien: todas las columnas de texto, en efecto, contienen variables categÃ³ricas.
#una variable es categÃ³rica cuando es razonable considerar que se elige entre un conjunto finito de variables posibles; por ejemplo, los barrios de Buenos Aires son un conjunto finito y predeterminado.
#funciÃ³n summary.
summary(atencion_ciudadano) # nos da un resumen de las variables

#Las categorias posibles para un factor son llamadas niveles (levels). Para ver todos los niveles del factor BARRIO, es decir todos los barrios representados en la columna con la variable BARRIO, podemos usar la funcion levels()

levels(atencion_ciudadano$BARRIO)
atencion_ciudadano$BARRIO #veo los datos (las filas) de esa columna

#¿PORQUE SALE NULL?
# consultar sobre lo de los levels: permite decir al programa que la columna es una de factores o niveles, despues nos permite ordenar el orden de los factores.Le asignas un valor por ejemplo un numero a una variable (muy bueno=5, bueno=4)= permite transformar a las variables en categorias son factores. No los toma como caracteres sino como niveles dentro de la vairable. 
 

# CRUZANDO VARIABLE: OPERACION JOIN combinar tablas relacionadas entre si por una o mas variables en comandos. left_join(). la funciÃ³n toma como parametros 2 dataframes (que a fin y al cabo son tablas) y busca las variables que tengan el mismo nombre y usandolas como referencia completa la primera de ellas, la de la izquierda, con los datos nuevos que aporta la segunda. left_join() devuelve un dataframe nuevo con los datos combinados.

#descargamos nuevo dataframe con barrios y comunas
barrios_comunas<-read.csv("http://bitsandbricks.github.io/data/barrios_comunas.csv")
atención_ciudadano<-left_join(atencion_ciudadano,barrios_comunas)
# la tabla de la derecha agrega las comunas que corresponden a cada barrio de la tabla de la izquierda. Es un cruce de variables donde hay dos tablas que tienen una variable en comun (en este caso barrios) y se agregan los datos de otra variable de la tabla para cada fila de la variable comun. 
# forma de combinar datos de distintos dataset con varibles en comun, te agrega mas info y te genera una tabla mas completa.

atencion_ciudadano<-left_join(atencion_ciudadano,barrios_comunas)
rm(atención_ciudadano) # tengo una de mas asi que la borro.
head(atencion_ciudadano) # (en esta nueva tabla se le asigno la columna "comuna" que le asigna un numero de comuna a cada barrio)

# mas funciones right_join(), inner_join(), full_join, anti_join()

# Guardar dataframe nuevo: usar comando write.csv(atencion_ciudadano, "atencion ciudadano", row.names = FALSE)

write.csv(atencion_ciudadano, "atencion ciudadano", row.names = FALSE) # write.csv (parametro que vamos a guardar, "nombre del archivo", row.names = false)
# row.names = FALSE sirve para evitar que R le agregue una columna al principio con números consecutivos (1, 2, 3, y así), cosa que quizás fue útil alguna vez pero en general no necesitamos.

# para leer los datos de nuevo usamos
read.csv("atencion_ciudadano.csv")

# comando getwd: nos permite saber donde guardamos los dataset (.csv) si queremos abrirlos despues.

getwd()

#Funciones para transformación de datos:

# 1) select() seleccionar o elegir columnas (variables) por su nombre (seleccionamos de todo el dataset solo las variables que nos interesan, un dataset puede tener cientos de variables y solo necesitamos algunas)

# ejemplo: primero vemos los nombres de las variables
names(atencion_ciudadano)
# A) SELECCION DE VARIABLES: ahora solo queremos seleccionar las variables "Periodo" y "total"

# "nuevo dataset"<-select(dataset, variable 1, variable 2)
selección<-select(atencion_ciudadano,PERIODO,total)
names(selección) # nombres variable
dim(selección) # numero filas y columnas
head(selección) # primeras 6 

# B) SELECCIÓN POR CONTIGUIDAD: Seleccionamos todas las columnas o variables que van de RUBRO A BARRIO. se usa el "a:b" (indica que va de a "a" a  "b" )
1:4
selección<-select(atencion_ciudadano,RUBRO:BARRIO)
names(selección)
head(selección)
dim(selección)

# C) SELECCIÓN POR OMISIÓN: Si nos interesa todo el contenido del dataset salvo una variable o columna particular. usamos el "-"

selección<-select(atencion_ciudadano,-RUBRO) # seleccionamos todas las columnas salvo el RUBRO

head(selección)

# podemos omitir varias variables o columnas: select(dataset, - (variable 1, varable 2)) seleccionamos al dataset menos v1,v2,...vi

selección<-select(atencion_ciudadano,-(PERIODO:total))
head(selección)
selección<-select(atencion_ciudadano,-(TIPO_PRESTACION:COMUNA))
head(selección)

 # 2) filter(): lo usamos para filtrar datos de interes, para seleccionar objetos u observaciones que cumplen determinada condición.

# filter(dataset, condición 1 , condicion 2, condicion n) si ponemos == usamos " ".
#Ejemplos:

villa_urquiza<-filter(atencion_ciudadano, BARRIO == "VILLA URQUIZA") # de todos los datos seleccionamos solo a los que corresponden al BARRIO villa urquiza

dim(villa_urquiza)
names(villa_urquiza)
head(villa_urquiza)
# ahora seleccione aquellos totales mayores a 50
villa_urquiza<-filter(villa_urquiza, total>=50)
head(villa_urquiza)
# ahora seleccionamos solo ciber
villa_urquiza<-filter(villa_urquiza, RUBRO == "CIBER")


head(villa_urquiza)
# no hay observaciones resulta que no hay ciberes con mas de 50 denuncias
#sigamos jugando
villa_urquiza<-filter(atencion_ciudadano, BARRIO == "VILLA URQUIZA")
villa_urquiza<-filter(villa_urquiza, RUBRO == "SANEAMIENTO URBANO", total>=100)
#en este caso le pedimos al sistema 2 condiciones para seleccionar: que sea del rubro saneaminto y que el total de denuncias (total) sea mayor o igual a 100
#seleccionamos registros que corresponden a RETIRO del periodo enero 2014 (201401), cuyo total este entre 0 y 50 y sea denuncia)
retiro<-filter(atencion_ciudadano, BARRIO == "RETIRO", TIPO_PRESTACION == "DENUNCIA",total<=50)
head(retiro)
balvanera<-filter(atencion_ciudadano, BARRIO == "BALVANERA", RUBRO == "	
ACTOS DE CORRUPCION", TIPO_PRESTACION == "DENUNCIA",total>=70)
rm(balvanera)
# y asi uno le da las condiciones que va queriendo. Es una herramienta realmente util.

#OPERADORES LOGICOS:

#seleccionamos rubro salud

seleccion<-filter(atencion_ciudadano, RUBRO == "SALUD", PERIODO == "20108")

# ahora queremos ver intentar:

seleccion<-filter(atencion_ciudadano, BARRIO == "RETIRO", BARRIO == "PALERMO") # la "," asume "y"
head(seleccion)
#nos da 0 resultados (un vacio),en ningun registro el barrio es retiro y palermo. Ninguna observacion cumple con las condiciones.

# Para obtener registros ocurridos en RETIRO O PALERMO usamos operador logico "|" que significa "Ó".

seleccion<-filter(atencion_ciudadano, BARRIO == "RETIRO" | BARRIO == "PALERMO" )
head(seleccion)

# logica de conjuntos o logica  booleana

# a & b     a y b
# a | b     a ó b
# a & !b    a, y no b
# !a & b    no a, y b
# !(a & b)  no (a y b) 

#Un ejemplo de a & !b, filas en las que el tipo de prestación sea "TRAMITE", y en las que el rubro no sea "REGISTRO CIVIL"

seleccion<-filter(atencion_ciudadano, TIPO_PRESTACION == "TRAMITE"  & ! RUBRO == "REGISTRO CIVIL")

#todas las filas excepto las de tipo "DENUNCIA", y rubro "SEGURIDAD E HIGIENE"

seleccion<-filter(atencion_ciudadano, !(TIPO_PRESTACION == "DENUNCIA" & RUBRO == "SEGURIDAD E HIGIENE"))

head(seleccion)

#Ordenar filas con arrange: cambia el orden en el cual aparecen las filas de un dataframe .

ordenado<-arrange(atencion_ciudadano, total) 
head(ordenado)
ordenado<-arrange(atencion_ciudadano, COMUNA)
head(ordenado)
ordenado<-arrange(atencion_ciudadano, total, COMUNA)
head(ordenado)
ordenado<- arrange(atencion_ciudadano, total, BARRIO)
head(ordenado)

# si no aclaramos el orden es siempre ascendente (de menor a mayor), si queremos lo contrario usamos desc()

ordenado<-arrange(atencion_ciudadano,desc(total))
head(ordenado)

# VALORES FALTANTES: NA (No disponible o not avalaible). La mayoria de las operaciones con estos valores resultan indefinidas, hay que tener cuidado.

10>NA

#¿10 es mayor a un valor desconocido? la respuesta es NA (No disponible) nadie lo sabe.

#¿NA es un valor desconocido igual a otro valor desconocido?

NA==NA

#la respuesta es NA , tampoco lo sabe.

# podemos preguntar a R si un valor es deconocido con la función is.na()

is.na(NA) # TRUE , efectivamente NA es un valor deconocido.

# filter() ignora las filas que contienen NA's en la variable que usa para filtrar. arrange() muestra las filas con NA's en el campo por el que ordena, pero todas al final.

# Agregar nuevas variables con mutate: cuando queremos agregar nuevas columnas a nuestro dataset usamos mutate(dataset, nombre nueva columna == datos nueva columna)
# con la función data.frame creamos nuevo dataset.

circulos<- data.frame(nombre = c("Círculo 1", "Círculo 2", "Círculo 3"),
                       tamaño = c("Pequeño", "Mediano", "Grande"),
                       radio  = c(1, 3, 5))
#count: cuenta los valores de 1 o mas variables o columnas



#agregamos nueva columna con el area de cada circulo con mutate

circulos1<-mutate(circulos, area = 3,1416 * radio^2)
count(circulos,radio)
count(circulos,tamaño)

circulos1

#Usando mutate(), definimos la columna "area", indicando que su contenido será el valor de la columna "radio" en cada registro puesto en la fórmula del área de un círculo. Los operadores aritméticos (+, -, *, /, ^) son con frecuencia útiles para usar en conjunto con mutate().

#queremos agregar una nueva variable con el mes y el año de cada registro. La columna periodo tiene valores del tipo 201301. usamos la variable substr() que extrae porciones de una variable de texto. usamos mutate para agregar 2 variables nuevas

atencion_ciudadano<-mutate(atencion_ciudadano,
    AÑO = substr(PERIODO, 1,4),
    MES = substr(PERIODO, 5,6))
head(atencion_ciudadano)

#Extraer sumarios con summarise(): summarise o resumir toma un dataframe completo y lo resume en una sola fila de acuerdo a la operacion que le indiquemos

summarise(atencion_ciudadano,promedio = mean(total)) # por ejemplo el promedio de la columna total
mean(atencion_ciudadano$total) # tambien se puede sacar asi.

# summarise se usa con la funcion group_by que cambia la unidad de analisis del dataframe completo a grupos individuales.
#Usar summarise() sobre un dataframe al que antes agrupamos con group_by resulta en resúmenes "por grupo"
agrupado<-group_by(atencion_ciudadano, AÑO) #primero lo agrupamos por año
summarise(agrupado, promedio = mean(total)) # ahora al año le aplicamos una operacion y nos queda año y promedio, resume el grupo que le dijimos en una sola fila definida por una operacion que nosotros le damos. se puede usar para muchas columnas con diferentes operaciones.

# podemos agrupar por varias columnas  generando mas subgrupos por ejemplo promedios por año y mes.

agrupado<-group_by(atencion_ciudadano, AÑO,MES)
summarise(agrupado, promedio = mean(total)) # es del total porque queremos agrupar sobre el promedio del total de denuncias.

# agrupamos año, mes y barrio.
agrupado<-group_by(atencion_ciudadano, AÑO,MES,BARRIO)
sumario<-summarise(agrupado, mean(total))
head(sumario)

# Con summarise() podemos usar cualquier función que tome una lista de valores y devuelva un sólo restado. Para empezar, algunas de las que más podrían ayudarnos son:

#mean()`: Obtiene el promedio de los valores
#sum()`: Obtiene la suma
#min()`: Obtiene el valor más bajo
#max()`: Obtiene el valor más alto

#ejemplo totamos agrupamos año, mes, barrio segun el maximo de los datos (del numero de denuncias)
agrupado<-group_by(atencion_ciudadano, AÑO,MES,BARRIO)
sumario<-summarise(agrupado, maximo = max(total))
head(sumario)

# EL OPERADOR PIPE %>%: Se pronuncia "paip". Es un simbolo que relaciona dos entidades. Une las operaciones.
# es como decir "Y LUEGO PASA".

# Ejemplo:

filtrado<-filter(atencion_ciudadano, AÑO == "2014") # Filtramos los datos para quedarnos con los registros del 2014.
agrupado<-group_by(filtrado, BARRIO) # agrupamos por barrio
sumario<-summarise(agrupado, suma = sum(total)) #hacemos un sumario, creando una variable resumen que contiene la suma de los registros para cada barrio
ordenado<-arrange(sumario, desc(sum)) #ordenamos de forma descendente las suma de las denuncias
head(ordenado,5) # mostramos las primeras 5 sumas (si ponemos un 5 head nos muestra los primeros 5)
# funciona el problema es que creamos muchas variables que despues nunca vamos a usar, ademas requiere que vayamos nombrando las variables, tarea que es lenta y engorrosa.

#El pipe, %>%, permite encadenar operaciones, conectando el resultado de una como el dato de entrada de la siguiente. La misma secuencia que realizamos antes puede resolverse con pipes, quedando así:

atencion_ciudadano %>% 
        filter(AÑO == 2014) %>%
         group_by(BARRIO) %>%
          summarise(suma = sum(total)) %>%
           arrange(desc(suma)) %>%
            head(5)

# es mucho mas facil  y rapido escribirlo.

# El uso de pipes permite concentrarse en las operaciones de transformación, y no en lo que está siendo transformado en cada paso. Esto hace al código mucho más sencillo de leer e interpretar. En el ejemplo con pipe, sólo tuvimos que nombrar un dataframe con el cual trabajar un única vez, al principio.

# Detrás de escena, x %>% f(y) se transforma en f(x, y). Por eso:

filter(atencion_ciudadano, AÑO == 2014) #equivale a:
atencion_ciudadano %>% filter(AÑO == 2014)

# atencion_ciudadano %>% filter(AÑO == 2014)


# Trabajar con pipes es una de las ventajas que hacen de R un lenguaje muy expresivo y cómodo para manipular datos, y a partir de aquí lo usaremos de forma habitual.

#  Las técnicas para examinar un dataframe, como sumamry() nos permiten entender de forma rápida con que clase de variables vamos a trabajar. Los cinco verbos de manipulación que aprendimos, usados en conjunto, brindan una enorme capacidad para adaptar el formato de los datos a nuestras necesidades. Y el operador pipe nos ayuda a escribir nuestro código de forma sucinta y fácil de interpretar.
# A medida que vayamos progresando en nuestra familiaridad con las funciones -y agregando técnicas nuevas- vamos a ser capaces de procesar grandes cantidades de datos con soltura. Y obtener en pocos minutos lo que de otra forma, sin herramientas computacionales, tardaría días o sería inviable por lo tedioso.

#EJERCICIO:
# 1) removemos todo lo que trabajamos antes.
rm(list=ls())
library("tidyverse")
# 2) importamos dataset (consumo de electricidad en los hogares)
# 3) operamos con select y filter.
Consumo<-select(DatosT,Consumo) #seleccionames variable de interes.
filtrado<-filter(DatosT, Consumo >= 200) #seleccionamos las filas de interes, en este caso hogares con consumo mayor a 200.
head(filtrado)
rm(Electro)# la teniamos de mas.
# 4) ordenamos dataset con arrange.
ordenado<-arrange(DatosT, desc(Consumo))
head(ordenado) #ordena el consumo en forma descendente (de mayor a menor ordena los valores numericos)
# 5) creamos columnas nuevas con mutate:
# creo nueva variable o columna:
convivientes<-c(1,0,2,3,4,6,8,0,1,3,4,6,6,8,0,1,9,4,5,1) # vector que dice las personas que viven en el hogar
nuevos_datos<-mutate(DatosT, personas_casa = convivientes) # creamos nueva columna agregando al vector anterior
head(nuevos_datos)
# 6) resumen del dataset con group_by y summarise
agrupado<-group_by(nuevos_datos, Hogar,Consumo,personas_casa)
resumen<-summarise(agrupado, mean(personas_casa))
head(resumen)

#EJERCICIO 2: Hacemos lo mismo pero con el dataset del principio (no estoy encontrando datasets buenos para repasar)
# 1)
atencion_ciudadano<-read.csv("http://bitsandbricks.github.io/data/gcba_suaci_barrios.csv")
# 2) seleccionamos con select: 
seleccion<-select(atencion_ciudadano,PERIODO,RUBRO,BARRIO,total)
# 3)filtramos con filter
filtrado<-filter(atencion_ciudadano,PERIODO == "201308", BARRIO == "CHACARITA" | BARRIO == "PALERMO", total <= 100)
head(filtrado)
rm(seleccion)
rm(selección)
# 4) ordenamos variables.
ordenado<-arrange(filtrado, PERIODO, BARRIO, desc(total))
head(ordenado)
# 5) agrupamos
agrupado<- group_by(ordenado,PERIODO,BARRIO,RUBRO)
sumado<- summarise(agrupado, promedio = mean(total))
head(sumado)

# Hacer todo el proceso usando un unico comando con el conector %>%
 
atencion_ciudadano %>% 
    select(PERIODO,RUBRO,BARRIO,total) %>%
      filter(PERIODO == "201308", BARRIO == "CHACARITA" | BARRIO == "PALERMO", total <= 100 ) %>%
       arrange(PERIODO,BARRIO,desc(total)) %>%
        group_by(PERIODO,BARRIO,RUBRO) %>%
         summarise(promedio = mean(total)) %>%
         head()
