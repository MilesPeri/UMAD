download.file(url="https://catalogodatos.gub.uy/dataset/46e30f24-c2a1-451d-9258-39e2960215ca/resource/c97f04a9-f19c-4277-b4a9-32791c039124/download/productos.csv",
              destfile = "productos.csv",
              method="auto")

library(readr)
library(tidyverse)
productos <- read_delim("productos.csv", 
                        delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                        trim_ws = TRUE)
download.file(url="https://catalogodatos.gub.uy/dataset/46e30f24-c2a1-451d-9258-39e2960215ca/resource/35e0e3e3-a4df-45c9-8e1c-334b0c118ca3/download/establecimiento.csv",
              destfile = "establecimientos.csv",
              method="auto")

establecimientos <- read_delim("establecimientos.csv", 
                        delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                        trim_ws = TRUE)

options(timeout = 180)
download.file(url="https://catalogodatos.gub.uy/dataset/46e30f24-c2a1-451d-9258-39e2960215ca/resource/48ec1ad7-4713-42fb-bf1d-902942dcdc09/download/precios_2023.csv",
              destfile = "precios23.csv",
              method="auto")
download.file(url="https://catalogodatos.gub.uy/dataset/92b19a76-31d0-4b60-b8ae-8fe3d672d1d1/resource/c77704af-fd9a-42b2-9a65-c62c2161100b/download/precios_2016.csv",
              destfile = "precios16.csv",
              method="auto")
download.file(url="https://catalogodatos.gub.uy/dataset/92b19a76-31d0-4b60-b8ae-8fe3d672d1d1/resource/b18eccb9-21a3-405b-a43d-0796f80a7bf2/download/precios_2017.csv",
              destfile = "precios17.csv",
              method="auto")
download.file(url="https://catalogodatos.gub.uy/dataset/92b19a76-31d0-4b60-b8ae-8fe3d672d1d1/resource/e9ec9d26-b25c-4716-9019-02b16b82e1d5/download/precios_2018.csv",
              destfile = "precios18.csv",
              method="auto")
download.file(url="https://catalogodatos.gub.uy/dataset/92b19a76-31d0-4b60-b8ae-8fe3d672d1d1/resource/cc74984b-e57c-4f0a-90d3-bd64292b8743/download/precios_2019.csv",
              destfile = "precios19.csv",
              method="auto")
download.file(url="https://catalogodatos.gub.uy/dataset/92b19a76-31d0-4b60-b8ae-8fe3d672d1d1/resource/42995bf6-da53-4236-87b0-53d0e0d77911/download/precios_2020.csv",
              destfile = "precios20.csv",
              method="auto")
download.file(url="https://catalogodatos.gub.uy/dataset/92b19a76-31d0-4b60-b8ae-8fe3d672d1d1/resource/0598e527-29d7-4a69-8e0d-86078d9aea0b/download/precios_2021.csv",
              destfile = "precios21.csv",
              method="auto")
download.file(url="https://catalogodatos.gub.uy/dataset/92b19a76-31d0-4b60-b8ae-8fe3d672d1d1/resource/0dd77d6e-9c7d-4fdf-bc46-2f86a90d635e/download/precios_2022.csv",
              destfile = "precios22.csv",
              method="auto")

precios16 <- read_csv("precios16.csv", col_names = FALSE,
                      locale = locale(encoding = "WINDOWS-1252"), 
                      trim_ws = TRUE)
precios17 <- read_csv("precios17.csv", col_names = FALSE,
                      locale = locale(encoding = "WINDOWS-1252"), 
                      trim_ws = TRUE)
precios18 <- read_csv("precios18.csv", col_names = FALSE,
                      locale = locale(encoding = "WINDOWS-1252"), 
                      trim_ws = TRUE)
precios19 <- read_csv("precios19.csv", col_names = FALSE,
                      locale = locale(encoding = "WINDOWS-1252"), 
                      trim_ws = TRUE)
precios20 <- read_csv("precios20.csv", col_names = FALSE,
                      locale = locale(encoding = "WINDOWS-1252"), 
                      trim_ws = TRUE)
precios21 <- read_csv("precios21.csv", col_names = FALSE,
                      locale = locale(encoding = "WINDOWS-1252"), 
                      trim_ws = TRUE)
precios22 <- read_csv("precios22.csv", col_names = FALSE,
                      locale = locale(encoding = "WINDOWS-1252"), 
                      trim_ws = TRUE)
precios23 <- read_csv("precios23.csv", col_names = FALSE,
                               locale = locale(encoding = "WINDOWS-1252"), 
                               trim_ws = TRUE)
precios_a <- bind_rows(precios16,precios17)
names(precios_a) <- c("Establecimiento(id.establecimiento)","Presentacion_Producto(id.producto)","Precio", "Fecha", "Oferta") 

download.file(url="https://catalogodatos.gub.uy/dataset/46e30f24-c2a1-451d-9258-39e2960215ca/resource/bf7f793b-5f03-4d7b-b155-fa8af49280b3/download/metadatos_precios.csv",
              destfile = "encabezados.csv",
              method="auto")
encabezados <- read_csv("encabezados.csv")
precios_b <- bind_rows(precios18,precios19, precios20, precios21,precios22,precios23)
names(precios_b) <- encabezados %>% pull(nombreDeAtributo)
precios_b <- precios_b %>% select(names(precios_a))
gc()
precios <- bind_rows(precios_a,precios_b)
rm(precios_a,precios_b)

library(lubridate)                                 
precios <- precios %>% mutate(Fecha=floor_date(Fecha, unit = "month"))
precios_agr <- precios %>% group_by(`Establecimiento(id.establecimiento)`,`Presentacion_Producto(id.producto)`,Fecha,Oferta) %>% 
  summarise(Precio_mes=mean(Precio)) %>% ungroup()

tabla_barrios <- establecimientos %>% 
  filter(depto=="Montevideo") %>% 
  group_by(barrio) %>% count(ccz) %>% 
  filter(!is.na(ccz)) %>% arrange(-n) %>% 
  slice_head(n=1) %>% ungroup() %>% 
  transmute(barrio, ccz_est=ccz)

establecimientos <- establecimientos %>% left_join(tabla_barrios)

establecimientos <- establecimientos %>% mutate(ccz=case_when(is.na(ccz)~ccz_est,
                                                              TRUE~ccz))
establecimientos <- establecimientos %>% select(-ccz_est)

precios_reg <- precios_agr %>% filter(Oferta==0) %>% group_by(`Establecimiento(id.establecimiento)`,`Presentacion_Producto(id.producto)`) %>% 
  arrange(`Establecimiento(id.establecimiento)`, `Presentacion_Producto(id.producto)`,Fecha) %>%
  mutate(Precio_ant=lag(Precio_mes), relativo=Precio_mes/Precio_ant) %>% 
  ungroup()

precios_reg <- precios_reg %>% mutate(relativo=if_else(Fecha==ymd("2016-01-01"),1,relativo))
precios_reg <- precios_reg %>% mutate(relativo=na_if(relativo,0))
precios_reg <- precios_reg %>% mutate(relativo=na_if(relativo,Inf))
precios_reg <- precios_reg %>% mutate(relativo=na_if(relativo,NaN))
precios_reg <- precios_reg %>% drop_na(relativo)

precios_reg <- precios_reg %>% left_join(establecimientos, by=c("Establecimiento(id.establecimiento)"="id.establecimientos"))
precios_reg <- precios_reg %>% mutate(Precio_ant=na_if(Precio_ant,9999))
precios_reg <- precios_reg %>% drop_na(Precio_ant)

relativos_dpto <- precios_reg %>% group_by(Fecha, depto) %>% summarise(prom_relat=exp(mean(log(relativo))))
library(ggplot2)
relativos_dpto %>% group_by(depto) %>% mutate(indice=100*cumprod(prom_relat)) %>% 
  ggplot(aes(x=Fecha,y=indice))+geom_line()+
    facet_wrap(depto~.)
relativos_mont <- precios_reg %>% filter(depto=="Montevideo") %>% group_by(Fecha,ccz) %>% 
  summarise(prom_relat=exp(mean(log(relativo))))
relativos_mont %>% group_by(ccz) %>% mutate(indice=100*cumprod(prom_relat)) %>% 
  ggplot(aes(x=Fecha,y=indice))+geom_line()+
  facet_wrap(ccz~.)

relativos_cadena <- precios_reg %>% group_by(Fecha,cadena) %>% summarise(prom_relat=exp(mean(log(relativo))))
relativos_cadena %>% group_by(cadena) %>% mutate(indice=100*cumprod(prom_relat)) %>% 
  ggplot(aes(x=Fecha,y=indice))+geom_line()+
  facet_wrap(cadena~.)

precios_no_reg <- precios_agr  %>% group_by(`Establecimiento(id.establecimiento)`,`Presentacion_Producto(id.producto)`) %>% 
  arrange(`Establecimiento(id.establecimiento)`, `Presentacion_Producto(id.producto)`,Fecha) %>%
  mutate(Precio_ant=lag(Precio_mes), relativo=Precio_mes/Precio_ant) %>% 
  ungroup()

precios_no_reg <- precios_no_reg %>% mutate(relativo=if_else(Fecha==ymd("2016-01-01"),1,relativo))
precios_no_reg <- precios_no_reg %>% mutate(relativo=na_if(relativo,0))
precios_no_reg <- precios_no_reg %>% mutate(relativo=na_if(relativo,Inf))
precios_no_reg <- precios_no_reg %>% mutate(relativo=na_if(relativo,NaN))
precios_no_reg <- precios_no_reg %>% drop_na(relativo)

precios_no_reg <- precios_no_reg %>% left_join(establecimientos, by=c("Establecimiento(id.establecimiento)"="id.establecimientos"))
precios_no_reg <- precios_no_reg %>% mutate(Precio_ant=na_if(Precio_ant,9999))
precios_no_reg <- precios_no_reg %>% drop_na(Precio_ant)


relativos_ofta_dpto <- precios_no_reg %>% group_by(Fecha, depto) %>% summarise(prom_relat=exp(mean(log(relativo))))
relativos_ofta_dpto %>% group_by(depto) %>% mutate(indice=100*cumprod(prom_relat)) %>% 
  ggplot(aes(x=Fecha,y=indice))+geom_line()+
  facet_wrap(depto~.)

# Creo una clasificación por productos
# Carne, pollo, pescado y huevos
# Hamburguesas y embutidos
# Pulpa de tomate, dulces y enlatados
# Harina y Arroz
# Aceite
# Panificados y pastas
# Azucar, sal, café, té, yerba, cocoa, mayonesa
# Lacteos
# Frutas y verduras
# Aguas y refrescos
# Cerveza y vino
# Aseo personal
# Productos de limpieza
# Artículos escolares

productos <- productos %>% mutate(Clasificacion=case_when(id.producto%in% c(13,14,29,30,85:87,114:117,128:132,139)~"Carne, pollo, pescado y huevos",
                                                          id.producto%in% c(37:39,58:60,68:70,97:101)~"Hamburguesas y embutidos",
                                                          id.producto%in% c(21:23,48:51,111:113,133:135)~"Pulpa de tomate, dulces y enlatados",
                                                          id.producto%in% c(15:20,71:78,358,360,362:364)~"Harina y Arroz",
                                                          id.producto%in% c(1:9,359,361,365)~"Aceite",
                                                          id.producto%in% c(52:57,61:62,118:121)~"Panificados y pastas",
                                                          id.producto%in% c(24:28,40:41,108:110,140:145,149:151)~"Azucar, sal, café, té, yerba, cocoa, mayonesa",
                                                          id.producto%in% c(79:81,102:107,136:138,152:154)~"Lacteos",
                                                          id.producto%in% c(225:297)~"Frutas y verduras",
                                                          id.producto%in% c(10:12,63:67,366:370)~"Aguas y refrescos",
                                                          id.producto%in% c(31:33,146:148)~"Cerveza y vino",
                                                          id.producto%in% c(34:36,42:44,88:90,122:127,308:357)~"Aseo personal",
                                                          id.producto%in% c(45:47,82:84,91:96)~"Productos de limpieza",
                                                          id.producto%in% c(155:224,298:307)~"Artículos escolares"
                                                      
                                                            
                                                          ))


precios_reg <- precios_reg %>% left_join(productos,by=c("Presentacion_Producto(id.producto)"="id.producto"))

relativos_clase <- precios_reg %>% group_by(Fecha,Clasificacion) %>% summarise(prom_relat=exp(mean(log(relativo))))
relativos_clase %>% group_by(Clasificacion) %>% mutate(indice=100*cumprod(prom_relat)) %>% 
  ggplot(aes(x=Fecha,y=indice))+geom_line()+
  facet_wrap(Clasificacion~.)
