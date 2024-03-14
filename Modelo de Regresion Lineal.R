#install.packages("devtools") # solo la primera vez
#devtools::install_github("dgonxalex80/paqueteMODELOS", force =TRUE)

## Librerías a utilizar

suppressWarnings({
  suppressMessages(  library(paqueteMODELOS))
  suppressMessages(library(tidyverse))
  suppressMessages(library(mice))
  suppressMessages(library(naniar))
  suppressMessages(library(sf))
  suppressMessages(library(plotly))
  suppressMessages(library(GGally))
  suppressMessages(library(ggcorrplot))
  suppressMessages(library(performance))
  suppressMessages(library(ggspatial))
  suppressMessages(library(nortest))
  suppressMessages(library(car))
  suppressMessages(library(equatiomatic))
  suppressMessages(library(leaflet))
})

# Exploración base de datos

data("vivienda")
str(vivienda)

# Datos Faltantes
#Forma 1
md.pattern(vivienda, rotate.names = TRUE)
title(main = "Matriz de Datos faltantes", sub = "Valores faltantes en el conjunto de datos")
#Forma 2
gg_miss_var(vivienda) + labs(x="Variables", y = "Datos NA")

# Poner la columna ID como index
vivienda <- vivienda[complete.cases(vivienda$id), ]
vivienda<-textshape::column_to_rownames(vivienda, loc = 1)

# 1. Base de datos de solo las ofertas de la base1: casas, de la zona norte de la ciudad.
vivienda_casas <- filter(vivienda, tipo == "Casa", zona == "Zona Norte")
head(vivienda_casas, 3) %>%
  kable()

#Mapa de las casa en la zona

datos_shapefile <- st_read("C:/Users/juanr/Downloads/Mapa/mazanascali.shp") # Leer el shapefile con datos geoespaciales desde un archivo
vivienda_casas <- st_as_sf(vivienda_casas, coords = c("longitud", "latitud"), crs = st_crs(datos_shapefile)) # Convertir los datos de vivienda a un objeto sf con coordenadas especificadas y realizar buffer
datos_shapefile <- st_as_sf(datos_shapefile)
vivienda_casas <- st_buffer(vivienda_casas, dist = 0.01)# el buffer se realiza para que las variables  (Longitud y Latitud) de ambas bases de datos sean poligonos
datos_combinados <- st_join(datos_shapefile, vivienda_casas) # Realizar una operación de unión espacial entre los datos del shapefile y los datos de vivienda
colores_pasteles <- c("#217937", "#02031a", "#b10c43", "#e84b2c", "#34baab") # colores
ggplot() +
  geom_sf(data = datos_combinados, aes(fill = zona), color = "#faf9f4", size = 0.2) +
  scale_fill_manual(values = colores_pasteles) +  # Asignar la paleta de colores
  ggtitle("Distribución de casas en la zona Norte") +
  theme_minimal() +
  labs(fill = "Zona") +
  guides(fill = guide_legend(na.value = "transparent")) +
  theme(legend.key = element_rect(color = "#faf9f4"))+
  annotation_scale()+
  annotation_north_arrow(location='tr')

## Relación entre el precio y el área construida
plot_area_construida <- plot_ly(data = vivienda_casas, x = ~areaconst, y = ~preciom, type = "scatter", mode = "markers") %>%
  add_trace(name = "Precio de la Casa") %>%
  layout(title = "Relación entre Precio de la Casa y Área Construida",
         xaxis = list(title = "Área Construida (m²)"),
         yaxis = list(title = "Precio de la Casa")) %>%
  hide_legend()

plot_area_construida

## Relación entre el precio y el estrato.

# Tabla de frecuencias precio y estrato
summarytools::freq(vivienda_casas$estrato, plain.ascii = FALSE, style = "rmarkdown")

# Gráfico de cajas 
colores_pasteles <- c("#fea304", "#909320", "#37192c", "#125a44")# Definir una paleta de colores pasteles
vivienda_casas$estrato <- factor(vivienda_casas$estrato) # Convertir la variable estrato en factor
# Crear el gráfico de cajas con ggplot
p <- ggplot(vivienda_casas, aes(x = estrato, y = preciom, fill = estrato)) +
  geom_boxplot() +
  labs(x = "Estrato", y = "Precio de la Vivienda", title = "Relación entre Precio de la Casa y Estrato") +
  scale_fill_manual(values = colores_pasteles) +  # Asignar la paleta de colores pasteles
  theme(axis.text.x = element_text(angle = 90))
# Convertir el gráfico a plotly
plotly::ggplotly(p)



## Relación entre el precio y número de baños.

#Tabla de frecuencias
summarytools::freq(vivienda_casas$banios, plain.ascii = FALSE, style = "rmarkdown")
# Gráfico de dispersión
plot_area_construida <- plot_ly(data = vivienda_casas, x = ~banios, y = ~preciom, type = "scatter", mode = "markers") %>%
  add_trace(name = "Precio de la Casa") %>%
  layout(title = "Relación entre Precio de la Casa y baños",
         xaxis = list(title = "Cantidad de  baños"),
         yaxis = list(title = "Precio de la Casa")) %>%
  hide_legend()
plot_area_construida

## Relación entre el precio y número de habitaciones.

#Tabla de frecuencias
summarytools::freq(vivienda_casas$habitaciones, plain.ascii = FALSE, style = "rmarkdown")
# Gráfico de dispersión.
plot_area_construida <- plot_ly(data = vivienda_casas, x = ~habitaciones, y = ~preciom, type = "scatter", mode = "markers") %>%
  add_trace(name = "Precio de la Casa") %>%
  layout(title = "Relación entre Precio de la Casa y cantidad de habitaciones",
         xaxis = list(title = "Cantidad de habitaciones"),
         yaxis = list(title = "Precio de la Casa")) %>%
  style(marker = list(color = "#02031a")) %>%
  hide_legend()
plot_area_construida

## Relación entre el precio y zona.
#Tabla de frecuencias.
summarytools::freq(vivienda_casas$zona, plain.ascii = FALSE, style = "rmarkdown")
# Gráfico de caja
p<-ggplot(vivienda_casas,aes(x=zona,y=preciom, fill=zona))+
  geom_boxplot()+
  labs(x="Zona",y="Precio de la Vivienda",title="Relación entre Precio de la Casa y Zona") + theme(axis.text.x = element_text(angle = 90))
plotly::ggplotly(p)
## Matriz de Correlación
#Filtar base de datos
vivienda_2 <- select(vivienda_casas, estrato, preciom, areaconst,banios, habitaciones)
# Convertir todas las columnas a numérico
vivienda_2 <- lapply(vivienda_2, as.numeric)
# Eliminar filas con NA después de la conversión
vivienda_2 <- na.omit(vivienda_2)
# Crear un nuevo marco de datos
vivienda_2 <- as.data.frame(vivienda_2)
# Calcular la matriz de correlación
Casa_cor <- cor(vivienda_2)
# Crear el gráfico de correlación
ggcorrplot(Casa_cor, type = "upper", lab = TRUE) +
  labs(title = "Mapa de Correlación - Casa") +
  theme(plot.title = element_text(hjust = 0.5))

# 3. Estimación del modelo.

Vivienda_casa_21<- filter(vivienda, tipo == "Casa" & zona == "Zona Norte")
vivienda_3 <- select(Vivienda_casa_21, estrato, preciom, areaconst,banios, habitaciones, parqueaderos)
mod_multi_1 <- lm(preciom~areaconst+estrato+habitaciones+parqueaderos+banios, data = vivienda_3)
summary(mod_multi_1)
# Ecuación del modelo con los estimadores.
extract_eq(mod_multi_1, use_coefs = TRUE)

# 4. validación de supuestos del modelo
check_model(mod_multi_1) ## Este codigo muestra cada uno de los supuestos del modelo.
##Prueba de normalidad en los residuales
ad.test(mod_multi_1$residuals)
#Prueba de homocedasticidad.
lmtest::bptest(mod_multi_1)
#Análisis de Inflación de Varianza (VIF):
vif(mod_multi_1)
#Prueba de no autocorrelación de errores.
lmtest::dwtest(mod_multi_1)

# 5. Modelo con las características de la primera solicitud.

# Crear un nuevo conjunto de datos con las características de la primera solicitud
nueva_solicitud <- data.frame(
  areaconst = 200,
  parqueaderos = 1,
  banios = 2,
  habitaciones = 4,
  estrato = 4 
)

nueva_solicitud_1 <- data.frame(
  areaconst = 200,
  parqueaderos = 1,
  banios = 2,
  habitaciones = 4,
  estrato = 5
)
# Realizar la predicción con el modelo identificado
prediccion_precio <- predict(mod_multi_1, newdata = nueva_solicitud)
prediccion_precio_1 <- predict(mod_multi_1, newdata = nueva_solicitud_1)
# Imprimir la predicción
cat("El precio de una vivienda con estas características pero con estrato 4 en la zona Zorte es de:", prediccion_precio, "\n")
cat("El precio de una vivienda con estas características pero con estrato 5 en la zona Zorte es de:", prediccion_precio_1, "\n")


# 6. Potenciales ofertas.

ofertas_potenciales_estrat4 <- filter(vivienda,
                                      estrato %in% c(4, 5),
                                      areaconst >= 200,
                                      parqueaderos >= 1,
                                      banios >= 2,
                                      habitaciones == 4,
                                      zona == 'Zona Norte',
                                      tipo =='Casa',
                                      preciom <= 350)
head(ofertas_potenciales_estrat4, 5 )%>%
  kable()

# Crear mapa con marcadores de casas
map <- leaflet() %>%
  addTiles() %>% 
  addMarkers(
    data = ofertas_potenciales_estrat4,
    lng = ~longitud,
    lat = ~latitud,
    icon = leaflet::makeIcon(iconUrl = "https://images.vexels.com/media/users/3/142690/isolated/preview/42d5c05736bc309247375267ec45aa22-logotipo-de-lupa-inmobiliaria.png", iconWidth = 38, iconHeight = 38), # Icono de casa roja
    popup = paste("Precio: $", ofertas_potenciales_estrat4$preciom,"<br>",
                  "Área Construida: ", ofertas_potenciales_estrat4$areaconst,"m²","<br>",
                  "Estrato: ", ofertas_potenciales_estrat4$estrato,"<br>",
                  "Cantidad de baños: ", ofertas_potenciales_estrat4$banios,"<br>",
                  "Cantidad de habitaciones: ", ofertas_potenciales_estrat4$habitaciones,"<br>",
                  "Cantidad de Parqueaderos: ", ofertas_potenciales_estrat4$parqueaderos
    )
  )
# Visualizar el mapa
map

### A continuación, se realizan los mismos pasos anteriores pero con filtrando la base de datos con la nueva solicitud (2).

## 1. Base de datos de solo las ofertas de la base2: apartamentos, de la zona sur de la ciudad.

vivienda_apartamentos <- filter(vivienda, tipo == "Apartamento", zona == "Zona Sur")
head(vivienda_apartamentos, 3) %>%
  kable()

# Mapa Zona sur apartamentos

datos_shapefile <- st_read("C:/Users/juanr/Downloads/Mapa/mazanascali.shp") # Leer el shapefile con datos geoespaciales desde un archivo
vivienda_apartamentos <- st_as_sf(vivienda_apartamentos, coords = c("longitud", "latitud"), crs = st_crs(datos_shapefile)) # Convertir los datos de vivienda a un objeto sf con coordenadas especificadas y realizar buffer
datos_shapefile <- st_as_sf(datos_shapefile)
vivienda_apartamentos <- st_buffer(vivienda_apartamentos, dist = 0.01)# el buffer se realiza para que las variables  (Longitud y Latitud) de ambas bases de datos sean poligonos
datos_combinados <- st_join(datos_shapefile, vivienda_apartamentos)# Realizar una operación de unión espacial entre los datos del shapefile y los datos de vivienda
colores_pasteles <- c("#e84b2c","#217937", "#02031a", "#b10c43", "#34baab")
ggplot() +
  geom_sf(data = datos_combinados, aes(fill = zona), color = "#faf9f4", size = 0.2) +
  scale_fill_manual(values = colores_pasteles) +  # Asignar la paleta de colores
  ggtitle("Distribución de Apratamentos en la Zona Sur") +
  theme_minimal() +
  labs(fill = "Zona") +
  guides(fill = guide_legend(na.value = "transparent")) +
  theme(legend.key = element_rect(color = "#faf9f4"))+
  annotation_scale()+
  annotation_north_arrow(location='tr')
### Relación entre el precio y el área construida

# Graficar la relación entre el precio y el área construida
plot_area_construida <- plot_ly(data = vivienda_apartamentos, x = ~areaconst, y = ~preciom, type = "scatter", mode = "markers") %>%
  add_trace(name = "Precio de la Casa") %>%
  layout(title = "Relación entre Precio del Apartamento y Área Construida",
         xaxis = list(title = "Área Construida (m²)"),
         yaxis = list(title = "Precio  Apartamentos")) %>%
  hide_legend()
plot_area_construida

### Relación entre el precio y el estrato.
summarytools::freq(vivienda_apartamentos$estrato, plain.ascii = FALSE, style = "rmarkdown")

colores_pasteles <- c("#fea304", "#909320", "#37192c", "#125a44") # Definir una paleta de colores pasteles
# Convertir la variable estrato en factor
vivienda_apartamentos$estrato <- factor(vivienda_apartamentos$estrato)
# Crear el gráfico de cajas con ggplot
p <- ggplot(vivienda_apartamentos, aes(x = estrato, y = preciom, fill = estrato)) +
  geom_boxplot() +
  labs(x = "Estrato", y = "Precio de la Vivienda", title = "Relación entre Precio de Apartamentos y Estrato") +
  scale_fill_manual(values = colores_pasteles) +  # Asignar la paleta de colores pasteles
  theme(axis.text.x = element_text(angle = 90))
# Convertir el gráfico a plotly
plotly::ggplotly(p)

### Relación entre el precio y número de baños.
summarytools::freq(vivienda_apartamentos$banios, plain.ascii = FALSE, style = "rmarkdown")
#Gráfico de  dispersión
plot_area_construida <- plot_ly(data = vivienda_apartamentos, x = ~banios, y = ~preciom, type = "scatter", mode = "markers") %>%
  add_trace(name = "Precio de la Casa") %>%
  layout(title = "Relación entre Precio de los apartamentos y baños",
         xaxis = list(title = "Cantidad de  baños"),
         yaxis = list(title = "Precio del Apartamento")) %>%
  hide_legend()
plot_area_construida
### Relación entre el precio y número de habitaciones.
summarytools::freq(vivienda_apartamentos$habitaciones, plain.ascii = FALSE, style = "rmarkdown")
# Gráfico de dispersión
plot_area_construida <- plot_ly(data = vivienda_apartamentos, x = ~habitaciones, y = ~preciom, type = "scatter", mode = "markers") %>%
  add_trace(name = "Precio del Apartamento") %>%
  layout(title = "Relación entre Precio del Apartamento y cantidad de habitaciones",
         xaxis = list(title = "Cantidad de habitaciones"),
         yaxis = list(title = "Precio del Apartamento")) %>%
  style(marker = list(color = "#02031a")) %>%
  hide_legend()
plot_area_construida
### Relación entre el precio y zona.
# Tabla de frecuencia
summarytools::freq(vivienda_apartamentos$zona, plain.ascii = FALSE, style = "rmarkdown")
#Fráfico de cajas
p<-ggplot(vivienda_apartamentos,aes(x=zona,y=preciom, fill=zona))+
  geom_boxplot()+
  labs(x="Zona",y="Precio del Apartamento",title="Relación entre Precio del Apartamento y Zona") + theme(axis.text.x = element_text(angle = 90))
plotly::ggplotly(p)
### Matriz de Correlación
Apartamento_2 <- select(vivienda_apartamentos, estrato, preciom, areaconst, banios, habitaciones)
# Convertir todas las columnas a numérico
Apartamento_2 <- lapply(Apartamento_2, as.numeric)
# Eliminar filas con NA después de la conversión
Apartamento_2 <- na.omit(Apartamento_2)
# Crear un nuevo marco de datos
Apartamento_2 <- as.data.frame(Apartamento_2)
# Calcular la matriz de correlación
Aparatmento <- cor(Apartamento_2)
# Crear el gráfico de correlación
ggcorrplot(Aparatmento, type = "upper", lab = TRUE) +
  labs(title = "Mapa de Correlación - Apartamento") +
  theme(plot.title = element_text(hjust = 0.5))
## 3. Estimación del modelo.
Vivienda_aparta_21<- filter(vivienda, tipo == "Apartamento" & zona == "Zona Sur")
Vivienda_aparta_21 <- select(Vivienda_aparta_21, estrato, preciom, areaconst,banios, habitaciones, parqueaderos)
mod_multi_1_aparta <- lm(preciom~areaconst+estrato+habitaciones+parqueaderos+banios, data = Vivienda_aparta_21)
summary(mod_multi_1_aparta)
# modelo con los estimadores.
extract_eq(mod_multi_1_aparta, use_coefs = TRUE)

## 4. Validación de los supuestos.
check_model(mod_multi_1_aparta)

#Prueba de normalidad en los residuales
ad.test(mod_multi_1_aparta$residuals)
#Prueba de homocedasticidad.
lmtest::bptest(mod_multi_1_aparta)
# Análisis de Inflación de Varianza (VIF):
vif(mod_multi_1_aparta)
#Prueba de no autocorrelación de errores.
lmtest::dwtest(mod_multi_1_aparta)


## 5. Modelo con las características de la segunda solicitud.


# Crear un nuevo conjunto de datos con las características de la primera solicitud
nueva_solicitud_aparta <- data.frame(
  areaconst = 300,
  parqueaderos = 3,
  banios = 3,
  habitaciones = 5,
  estrato = 5
)


nueva_solicitud_1_aparta <- data.frame(
  areaconst = 300,
  parqueaderos = 3,
  banios = 3,
  habitaciones = 5,
  estrato = 6
)
# Realizar la predicción con el modelo identificado
prediccion_precio_aparta <- predict(mod_multi_1, newdata = nueva_solicitud_aparta)
prediccion_precio_1_aparta <- predict(mod_multi_1, newdata = nueva_solicitud_1_aparta)
# Imprimir la predicción
cat("El precio de un apartamento con estas características pero con estrato 5 en la zona Sur es de:", prediccion_precio_aparta, "\n")
cat("El precio de un apartamento con estas características pero con estrato 6 en la zona Sur es de:", prediccion_precio_1_aparta, "\n")


## 6. Potenciales ofertas.

Apartamento_potenciales_5 <- filter(vivienda,
                                    estrato %in% c(5, 6),
                                    areaconst >= 300,
                                    parqueaderos >= 3,
                                    banios >= 3,
                                    habitaciones >= 5,
                                    zona == 'Zona Sur',
                                    tipo =='Apartamento',
                                    preciom <= 850)
head(Apartamento_potenciales_5, 3)%>%
  kable()

# Mapa potenciales ofertas:

map <- leaflet() %>%
  addTiles() %>% 
  addMarkers(
    data = Apartamento_potenciales_5,
    lng = ~longitud,
    lat = ~latitud,
    icon = leaflet::makeIcon(iconUrl = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTTBmOOSJSKXNNJi99I_lLTGqlEJpYr2y2Plg", iconWidth = 38, iconHeight = 60), # Icono de casa roja
    popup = paste("Precio: $", Apartamento_potenciales_5$preciom, "<br>",
                  "Cantidad de baños:", Apartamento_potenciales_5$banios, "<br>",
                  "Cantidad de habitaciones", Apartamento_potenciales_5$habitaciones,"<br>",
                  "Área Construida m²", Apartamento_potenciales_5$areaconst,"<br>",
                  "Estrato", Apartamento_potenciales_5$estrato,"<br>",
                  "Cantidad de Parqueaderos", Apartamento_potenciales_5$parqueaderos
    )
  )

# Visualizar el mapa
map





