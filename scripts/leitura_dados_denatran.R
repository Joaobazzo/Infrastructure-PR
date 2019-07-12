require(openxlsx)
require(gdata)
require(stringr)
require(gridExtra)
require(ggplot2)
require(extrafont)
rm(list=ls())
setwd("C:/Users/João Bazzo/Dropbox/Infraestrutura Cicloviária no Estado do Paraná/Joao/dados/denatran/")
# -----------
# municipios
mun <- c("ANTONINA","CURITIBA","GUARAPUAVA","LONDRINA","PARANAGUA")
# -----------
veh_2001 <- read.xlsx("Frota2001/Frota Tipo-Munic 2001/Frota Tipo-Munic 2001.xlsx",startRow = 3)
veh_2001 <- veh_2001[which(veh_2001$Municípios%in%mun),]
# 2002
veh_2002 <- read.xlsx("Frota2002/Frota Tipo-Munic 2002/Frota Tipo-Munic 2002.xlsx",startRow = 3)
veh_2002 <- veh_2002[which(veh_2002$Municípios%in%mun),]
# 2003
veh_2003 <- read.xlsx("Frota2003/Frota Tipo-Munic 2003/Frota_Mun_Dez_03t.xlsx",startRow = 1)
veh_2003 <- veh_2003[which(veh_2003$MUNICÍPIO%in%mun),]
# 2004
veh_2004 <- read.xlsx("Frota2004/Frota Munic 012004 Internet/Frota Munic 122004 Internet.xlsx",startRow = 2)
veh_2004 <- veh_2004[which(veh_2004$MUNICÍPIO%in%mun),]
# 2005
veh_2005 <- read.xlsx("Frota2005/Frota Tipo Munic 2005/Frota Munic 122005 Internet.xlsx",startRow = 3)
veh_2005 <- veh_2005[which(veh_2005$MUNICÍPIO%in%mun),]
# 2006
veh_2006 <- read.xlsx("Frota2006/Frota Tipo Munic 2006/Frota Munic DEZ2006 Internet.xlsx",sheet = 2,startRow = 3)
veh_2006 <- veh_2006[which(veh_2006$MUNICÍPIO%in%mun),]
# 2007
veh_2007 <- read.xlsx("Frota2007/Frota Tipo Munic 2007/Frota Munic Dez2007 Internet.xlsx",sheet = 2,startRow = 3)
veh_2007 <- veh_2007[which(veh_2007$MUNICÍPIO%in%mun),]
# 2008
veh_2008 <- read.xlsx("Frota2008/Frota Tipo Munic 2008/Frota Munic Dez2008.xlsx",sheet = 2,startRow = 3)
veh_2008 <- veh_2008[which(veh_2008$MUNICÍPIO%in%mun),]
# 2009
veh_2009 <- read.xlsx("FROTA_2009/Frota Munic 2009/Frota Munic Dez2009.xlsx",sheet = 2,startRow = 3)
veh_2009 <- veh_2009[which(veh_2009$MUNICIPIO%in%mun),]
# 2010
veh_2010 <- read.xlsx("FROTA_2010/Frota_Municipios/Frota Munic DEZ2010.xlsx",sheet = 2,startRow = 3)
veh_2010 <- veh_2010[which(veh_2010$MUNICIPIO%in%mun),]
# 2011
veh_2011 <- read.xlsx("FROTA_2011/Frota Munic. 2011/Frota Munic. DEZ.2011.xlsx",sheet = 2,startRow = 4)
veh_2011 <- veh_2011[which(veh_2011$MUNICIPIO%in%mun),]
# 2012
veh_2012 <- read.xlsx("FROTA_2012/Frota Munic. 2012/Frota Munic.DEZ.2012.xlsx",sheet = 2,startRow = 4)
veh_2012 <- veh_2012[which(veh_2012$MUNICIPIO%in%mun),]
# 2013
veh_2013 <- read.xlsx("FrotaMunic2013_dezembro/Frota Munic.DEZ.2013.xlsx",sheet = 2,startRow = 4)
veh_2013 <- veh_2013[which(veh_2013$MUNICIPIO%in%mun),]
# 2014
veh_2014 <- read.xlsx("Frota_Por_Municipio_e_Tipo_DEZ_2014/Frota Munic.DEZ.2014.xlsx",sheet = 2,startRow = 4)
veh_2014 <- veh_2014[which(veh_2014$MUNICIPIO%in%mun),]
# 2015
veh_2015 <- read.xlsx("Frota_por_Municipio_e_Tipo-DEZ_15/Frota_por_Municipio_e_Tipo-DEZ_15.xlsx",sheet = 2,startRow = 4)
veh_2015 <- veh_2015[which(veh_2015$MUNICIPIO%in%mun),]
# 2016
veh_2016 <- read.xlsx("Frota_por_Municipio_e_Tipo-DEZ_16.xlsx",sheet = 1,startRow = 4)
veh_2016 <- veh_2016[which(veh_2016$MUNICIPIO%in%mun),]
# 2017
veh_2017 <- read.xlsx("Frota_Munic_Dezembro_2017.xlsx",sheet = 1,startRow = 3)
veh_2017 <- veh_2017[which(veh_2017$MUNICIPIO%in%mun),]
# 2018
veh_2018 <- read.xlsx("Frota_Munic_Dezembro_2018.xlsx",sheet = 1,startRow = 4)
veh_2018 <- veh_2018[which(veh_2018$MUNICIPIO%in%mun),]
# -----
df_plot <- data.frame("Cidade"=rep(mun,length(2001:2018)),
                      "Ano"=rep(2001:2018,each=5),
                      "Total"=c(veh_2001$AUTOMÓVEL,veh_2002$AUTOMÓVEL,veh_2003$AUTOMÓVEL,
                                veh_2004$AUTOMÓVEL,veh_2005$AUTOMÓVEL,veh_2006$AUTOMÓVEL,
                                veh_2007$AUTOMÓVEL,veh_2008$AUTOMÓVEL,
                                veh_2009$AUTOMOVEL,veh_2010$AUTOMOVEL,veh_2011$AUTOMOVEL,
                                veh_2012$AUTOMOVEL,veh_2013$AUTOMOVEL,veh_2014$AUTOMOVEL,
                                veh_2015$AUTOMOVEL,veh_2016$AUTOMOVEL,veh_2017$AUTOMOVEL,
                                veh_2018$AUTOMOVEL),
                      "Auto"="Automóvel")
break()
write.csv(df_plot,"C:/Users/João Bazzo/Dropbox/Infraestrutura Cicloviária no Estado do Paraná/Joao/pre_processed_data/frota_municipio_2001-2018.csv")
#df_plot$Ano <- factor(df_plot$Ano,2001:2018)
# --- plot 1
