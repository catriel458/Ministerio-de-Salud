# 1)instalar paquetes
installed.packages("tidyverse")
# 2) activar los paquetes
library("tidyverse")
# 3)cargar archivo .CSV (donde estan los datos)
mortalidad <- read.csv('https://bitsandbricks.github.io/data/mortalidad_infantil_caba_2016.csv')
# 4) Operaciones basicas con dataset
 mortalidad
 dim(mortalidad) #cantidad de filas y columnas
 names(mortalidad)#nombre de las columnas (variables)
 head(mortalidad)# visi贸n rapida de los datos(muestra las 6 primeras filas)
# 5) graficos:
ggplot(mortalidad) + #selecci贸n del dataset que utilizaremos para el grafico (si ponemos esto el grafico sale en blanco hay que usar mas condiciones)
geom_col(aes(x=factor(Comuna),y=Tasa2016)) #el "+" sirve para ir agregando mas capas a nuestro grafico.

# 6) viendo los diferntes tipos de graficos.
ggplot(mortalidad) + geom_point(aes(x=factor(Comuna),y=Tasa2016)) # grafico de dispersi贸n(de puntos)
ggplot(mortalidad) + geom_polygon(aes(x=factor(Comuna),y=Tasa2016)) # no se ve el grafico.(debe ser para otro tipo de datos)
ggplot(mortalidad) + geom_text(aes(x=factor(Comuna),y=Tasa2016)) # no se ve el  grafico (debe ser para otro tipo de datos)
 #Cada funci髇  toma como parametro un conjunto de definiciones estadisticas que le indican una variable a graficar (mortalidad en nuestro caso), como (color, tama駉, etc) y d贸nde (posici贸n x, posici贸n y del eje). Estos par谩metros van siempre dentro de una funci贸n auxiliar, aes(). En nuestro ejemplo, geom_col(aes(x = factor(Comuna), y = Tasa2016))鈥�.

# 7) MAPAS:
# El paquete "Sf" brinda herramientas que permiten crear, manipular y combinar archivos con datos espaciales para producir mapas que pueden ser simples o en extremo sofisticados.
install.packages("sf")
# 8) activamos el paquete:
library(sf)
# 9) cargamos archivo geolocalisado (formato geojson). Definimos nuevo objeto o dataset como "comunas" (para definir un nuevo objeto se usa <-)
comunas<-st_read('https://bitsandbricks.github.io/data/CABA_comunas.geojson')
dim(comunas)
names(comunas)
head(comunas)
# 驴que datos sacamos de estos comandos?
# A) el dataset tiene 15 filas y 5 columnas (posiblemente las variables)
# B) el dataset tiene 5 variables o columnas: barrios, perimetro, area, comunas, geometry
# c) se muestran las primeras 6 ilas del data


# 10) generar mapas de cordenadas:

ggplot(comunas) + geom_sf()

# 11) podemos agrerar una leyenda que identifique cada comuna con su numero

ggplot(comunas) + geom_sf(aes(fill = comunas))

# Dentro de la funci贸n aes (significa estetica)   se usa el parametro "fill"(Relleno en ingles) para pedirle a ggplot que llene cada poligono con un color distinto  de acuerdo al campo "comunas".

# 12) vamos a clasificar que comunas pertenecen al norte y cuales al sur  usando como frontera la avenida rivadavia.

Rivadia<-st_read('https://bitsandbricks.github.io/data/avenida_rivadavia.geojson') # dibujamos la Avenida Rivadavia
ggplot(comunas) + geom_sf(aes(fill = comunas)) + geom_sf(data = Rivadia , color = "red")
# tenemos repetida la variable. Borramos una con el comando rm.

rm(ivadia)

# 13) Dividimos a ojo las comunas norte y sur y con eso definimos nuevas variables.

# sur: 1, 3, 4, 5, 7, 8 y 9
# norte: 2,5,6,10,11,12,13,14

# Queremos determinar si una comuna es norte o su,agregamos nueva columna.Primero definimos si las columnas son norte o sur y hacemos un vector. Definimos nueva variable.

nueva_columna<- c("Sur", "Norte", "Sur", "Sur", "Sur", "Norte", "Sur", "Sur", 
                  "Sur", "Norte", "Norte", "Norte", "Norte", "Norte", "Norte")
# las funciones dim, head y names son para dataset y no para variables. Para ver la variable solo escribimos su nombre.
nueva_columna

# 14) Agregamos una nueva columna al dataset comunas, nos da info sobre si es norte o sur.
comunas<-mutate(comunas, ubicacion = nueva_columna) # comando "Mutate" (dataset, nombre de la nueva columna (en este caso ubicaci贸n = variable o nueva columna)
comunas

# 15) verificamos con los comandos conocidos:
head(comunas) # primeras 6 filas
dim(comunas)  # numero de filas y columnas
names(comunas) # nombre de las columnas

rm(nueva_columna) # (la teniamos de mas)

# 16) Graficamos el nuevo dataset comunas que tiene la columna de ubicaci贸n. Donde dice fill rellenamos con la variable que queremos en este caso ubicacion
ggplot(comunas) + geom_sf(aes(fill = ubicacion)) + geom_sf(data = Rivadia, color = "blue")

# 17) ahora agregamos la nueva columna "ubicaci髇" al dataset inicial de mortalidad
mortalidad<-mutate(mortalidad, ubicaci髇 = nueva_columna)

head(mortalidad)

# el dataset mortalidad que creamos remplaza al anterior.
# 18) creamos un grafico que marca con color el nivel de mortalidad.
ggplot(comunas) + geom_sf(aes(fill = mortalidad$Tasa2016)) + scale_fill_distiller(palette = "spectral")

# 19) Creamos grafico de barras para visualizar mejor la info: geom_col para grafico de barras

ggplot(mortalidad) + geom_col(aes(x = Comuna, y = Tasa2016, fill = ubicaci髇))

# con el ggplot elegimos el data set, luego usamos geom_col para crear un grafico de barras donde X sean las comunas y y sea la tasa, despues rellanamos con fill segun si la comuna es norte o sur

# 20) ahora queremos quedarnos solo con los datos de una columna, no usar todo el dataset entero, usamos comando filter definimos nuevos objetos.

comunas_sur<-filter(mortalidad, ubicaci髇 == "Sur")

dim(comunas_sur)
names(comunas_sur)
head(comunas_sur)

comunas_norte<-filter(mortalidad, ubicaci髇 == "Norte")

# 21) calculamos los promedios 
promedio_sur<-mean(comunas_sur$Tasa2016)
promedio_norte<-mean(comunas_norte$Tasa2016)

relaci髇_mortalidad_sur_norte<- promedio_sur/promedio_norte

# CONCLUSI覰 FINAL: hay dos veces mas mortalidad el sur que en el norte. 

# En el a帽o 2016, la tasa de mortalidad infantil en todo los barrios del sur es m谩s alta que en cualquier de los del norte.

# Para los nacidos en 2016 de padres que viven en el sur de la ciudad, la posibilidad de morir antes del primer a帽o es, en promedio, el doble que la de aquellos con padres que residen al norte.

