setwd("C:/Users/João Bazzo/Dropbox/Infraestrutura Cicloviária no Estado do Paraná/Joao/")
require(sf)
require(stringr)
require(openxlsx)
require(tibble)
require(readxl)
rm(list=ls())
mun <- c("ANTONINA","CURITIBA","GUARAPUAVA","LONDRINA","PARANAGUÁ")
mun1 <- c("ANTONINA","CURITIBA","GUARAPUAVA","LONDRINA","PARANAGUA")
#cwb <- read_sf("E:/Documents/CICLO/Base Cartografica/EIXO_RUA_SIRGAS/EIXO_RUA.shp")
# roads
roads <- data.frame("municipio"=c("antonina","cwb","guarapuava","londrina","paranagua1"),"km"=0)
for(i in 1:5){
  aux <- read_sf(paste0("roads/shp/",roads$municipio[i],".shp"))
  vec <- c("path","service","footway",
           "steps","cycleway","bridleway","track")
  aux <- aux[-which(aux$fclass%in%vec),]
  roads$km[i] <- as.numeric(sum(st_length(aux)/1000))
}

roads
# idh
idh_legenda <- read.xlsx("E:/Documents/CICLO/Base Cartografica/atlas2013_dadosbrutos_pt.xlsx")
idh <- read.xlsx("E:/Documents/CICLO/Base Cartografica/atlas2013_dadosbrutos_pt.xlsx",sheet=2)
idh <- idh[which(idh$ANO=="2010"),]
idh <- idh[which(str_to_upper(idh$Município)%in%mun),c("Município","IDHM")]
# pop
pop <- read_xls(path = "dados/dados_ibge/UF_Municipio_2018.xls",sheet=2,skip=1)
pop <- pop[which(str_to_upper(pop$`NOME DO MUNICÍPIO`)%in%mun),c("NOME DO MUNICÍPIO","POPULAÇÃO ESTIMADA")]
# --------------------------
# cities areas
break()
cit <- read_sf("roads/shp/cities_final.shp")
cit <- cit[order(cit$nome),]
cit <- data.frame("cities"=mun,"area"=as.numeric(st_area(cit)/1000000))
#cit <- cit[order(cit$cities),]
cit
#cit <- cit[c(2,1,5,3,4),]
# frota
veh_2018 <- read.xlsx("dados/denatran/Frota_Munic_Dezembro_2018.xlsx",sheet = 1,startRow = 4)
veh_2018 <- veh_2018[which(veh_2018$MUNICIPIO%in%mun1),c("MUNICIPIO","AUTOMOVEL")]
# ciclovias
ciclovias <- read.table("C:/Users/João Bazzo/Dropbox/Infraestrutura Cicloviária no Estado do Paraná/Malhas Cicloviárias (Kmz) e Extensão/Malhas Cicloviárias/Extensão das Malhas.txt")
ciclovias <- ciclovias[,-c(2,4)]
colnames(ciclovias) <- c("Município","km")
ciclovias$Município <- str_to_upper(ciclovias$Município)
ciclovias <- ciclovias[order(ciclovias$Município),]
# malha viaria
#malha_crop <- st_crop(x = cit)
# Density
final <- data.frame("MUNICIPIO"=mun,
                    "População"=pop$`POPULAÇÃO ESTIMADA`,
                    "Vias"=roads$km,
                    "Ciclovias"=ciclovias$km,
                    "ciclovias_vias"=round(100*ciclovias$km/roads$km,2),
                    "Frota"=veh_2018$AUTOMOVEL,
                    "Taxa de motorização"=round(veh_2018$AUTOMOVEL/as.numeric(pop$`POPULAÇÃO ESTIMADA`),2),
                    "IDH"=idh$IDHM,
                    "Área (km2)"=cit$area,
                    "Densidade demográfica hab./km^2"=round(as.numeric(pop$`POPULAÇÃO ESTIMADA`)/cit$area,2))

View(final[,c(1,3,4,5)])
break()
t(final)
