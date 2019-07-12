require(readxl)
require(tibble)
require(stringr)
require(gridExtra)
require(ggplot2)
require(extrafont)
setwd("C:/Users/João Bazzo/Dropbox/Infraestrutura Cicloviária no Estado do Paraná/Joao/dados/")
rm(list=ls())
mun <-c("ANTONINA","CURITIBA","GUARAPUAVA","LONDRINA","PARANAGUÁ")
# -----------
# reading IBGE population file
# -----------
mylist <- list.files(path = "dados_ibge",pattern = ".xls",all.files = T,full.names = T,recursive = T)
mylist <- mylist[-c(1:21)]
# planilhas ibge - 2000
# -----------
wb1 = read_xls(path = mylist[1],skip=2)
colnames(wb1) <- c("Estado","1","2","Municipio","Populacao","3")
wb1$Municipio <- str_to_upper(wb1$Municipio)
wb1 <- wb1[which(wb1$Municipio%in%mun),]
# planilhas ibge - 2001
# -----------
wb2 = read_xls(path = mylist[2],skip=2)
head(wb2)
#colnames(wb2) <- c("Estado","1","2","Municipio","Populacao","3")
wb2$Municipio <- str_to_upper(wb2$MUNICÍPIOS)
wb2 <- wb2[which(wb2$Municipio%in%mun),]
# planilhas ibge - 2002
# -----------
wb3 = read_xls(path = mylist[3],skip=2)
head(wb3)
#colnames(wb2) <- c("Estado","1","2","Municipio","Populacao","3")
wb3$Municipio <- str_to_upper(wb3$MUNICÍPIOS)
wb3 <- wb3[which(wb3$Municipio%in%mun),]
# planilhas ibge - 2003
# -----------
wb4 = read_xls(path = mylist[4],skip=2)
head(wb4)
#colnames(wb2) <- c("Estado","1","2","Municipio","Populacao","3")
wb4$Municipio <- str_to_upper(wb4$MUNICÍPIOS)
wb4 <- wb4[which(wb4$Municipio%in%mun),]
# planilhas ibge - 2004
# -----------
wb5 = read_xls(path = mylist[5],skip=2)
head(wb5)
#colnames(wb2) <- c("Estado","1","2","Municipio","Populacao","3")
wb5$Municipio <- str_to_upper(wb5$MUNICÍPIOS)
wb5 <- wb5[which(wb5$Municipio%in%mun),]
# planilhas ibge - 2005
# -----------
wb6 = read_xls(path = mylist[6],skip=2)
head(wb6)
#colnames(wb2) <- c("Estado","1","2","Municipio","Populacao","3")
wb6$Municipio <- str_to_upper(wb6$MUNICÍPIOS)
wb6 <- wb6[which(wb6$Municipio%in%mun),]
# planilhas ibge - 2006
# -----------
wb7 = read_xls(path = mylist[7],skip=2)
head(wb7)
#colnames(wb2) <- c("Estado","1","2","Municipio","Populacao","3")
wb7$Municipio <- str_to_upper(wb7$MUNICÍPIOS)
wb7 <- wb7[which(wb7$Municipio%in%mun),]
# planilhas ibge - 2008
# -----------
wb8 = read_xls(path = mylist[8],skip=3)
head(wb8)
#colnames(wb2) <- c("Estado","1","2","Municipio","Populacao","3")
wb8$Municipio <- str_to_upper(wb8$X__2)
wb8 <- wb8[which(wb8$Municipio%in%mun),]
# planilhas ibge 2006-2008 (2007)
# ------------
wb_2007 <- data.frame("MUNICIPIO"=mun,
                      "POPULACAO"=0.5*(wb7$`POP-2006`+
                                         as.numeric(wb8$`POPULAÇÃO EM`)))

wb_2007
# planilhas ibge - 2009
# -----------
wb9 = read_xls(path = mylist[9],skip=2)
head(wb9)
#colnames(wb2) <- c("Estado","1","2","Municipio","Populacao","3")
wb9$Municipio <- str_to_upper(wb9$X__2)
wb9 <- wb9[which(wb9$Municipio%in%mun),]
# dados ibge - 2010
# -----------
wbcenso2010 <- read.xlsx("dados_ibge/parana/Tabela 4.21.1.1 - Copy.xlsx")
wbcenso2010$Abatiá <- str_to_upper(wbcenso2010$Abatiá)
head(wbcenso2010)


wbcenso2010 <- wbcenso2010[which(wbcenso2010[,1]%in%mun),]
wbcenso2010a <- data.frame("Cidade"=mun,"Pop"=0)
for(i in 1:5){
  aux <- wbcenso2010[which(wbcenso2010[,1]%in%mun[i]),]
  ind <- which.max(aux[,2])
  wbcenso2010a$Pop[i] <- aux[ind,2]
}
# planilhas ibge - 2011
# -----------
wb10 = read_xls(path = mylist[10],skip=2)
head(wb10)
#colnames(wb2) <- c("Estado","1","2","Municipio","Populacao","3")
wb10$Municipio <- str_to_upper(wb10$`NOME DO MUNICÍPIO`)
wb10 <- wb10[which(wb10$Municipio%in%mun),]
# planilhas ibge - 2012
# -----------
wb11 = read_xls(path = mylist[11],sheet = 2,skip=1)
head(wb11)
#colnames(wb2) <- c("Estado","1","2","Municipio","Populacao","3")
wb11$Municipio <- str_to_upper(wb11$`NOME DO MUNICÍPIO`)
wb11 <- wb11[which(wb11$Municipio%in%mun),]
# planilhas ibge - 2013
# -----------
wb12 = read_xls(path = mylist[12],sheet=2,skip=1)
head(wb12)
#colnames(wb2) <- c("Estado","1","2","Municipio","Populacao","3")
wb12$Municipio <- str_to_upper(wb12$`NOME DO MUNICÍPIO`)
wb12 <- wb12[which(wb12$Municipio%in%mun),]
# planilhas ibge - 2014
# -----------
wb13 = read_xls(path = mylist[13],sheet=2,skip=1)
head(wb13)
#colnames(wb2) <- c("Estado","1","2","Municipio","Populacao","3")
wb13$Municipio <- str_to_upper(wb13$`NOME DO MUNICÍPIO`)
wb13 <- wb13[which(wb13$Municipio%in%mun),]
# planilhas ibge - 2015
# -----------
wb14 = read_xls(path = mylist[14],sheet=2,skip=1)
head(wb14)
#colnames(wb2) <- c("Estado","1","2","Municipio","Populacao","3")
wb14$Municipio <- str_to_upper(wb14$`NOME DO MUNICÍPIO`)
wb14 <- wb14[which(wb14$Municipio%in%mun),]
# planilhas ibge - 2016
# -----------
wb15 = read_xls(path = mylist[15],sheet=2,skip=1)
head(wb15)
#colnames(wb2) <- c("Estado","1","2","Municipio","Populacao","3")
wb15$Municipio <- str_to_upper(wb15$`NOME DO MUNICÍPIO`)
wb15 <- wb15[which(wb15$Municipio%in%mun),]
# planilhas ibge - 2017
# -----------
wb16 = read_xls(path = mylist[16],sheet=2,skip=1)
head(wb16)
#colnames(wb2) <- c("Estado","1","2","Municipio","Populacao","3")
wb16$Municipio <- str_to_upper(wb16$`NOME DO MUNICÍPIO`)
wb16 <- wb16[which(wb16$Municipio%in%mun),]
# planilhas ibge - 2018
# -----------
wb17 = read_xls(path = mylist[17],sheet=2,skip=1)
head(wb17)
#colnames(wb2) <- c("Estado","1","2","Municipio","Populacao","3")
wb17$Municipio <- str_to_upper(wb17$`NOME DO MUNICÍPIO`)
wb17 <- wb17[which(wb17$Municipio%in%mun),]
#
# -----
df_plot1 <- data.frame("Cidade"=rep(mun,length(2001:2018)),
                      "Ano"=rep(2001:2018,each=5),
                      "Total"=c(wb2$`POP-2001`,wb3$`POP-2002`,
                                wb4$`POP-2003`,wb5$`POP-2004`,
                                wb6$`POP-2005`,wb7$`POP-2006`,
                                wb_2007$POPULACAO,wb8$`POPULAÇÃO EM`,
                                as.numeric(wb9$POPULAÇÃO),
                                as.numeric(wbcenso2010a$Pop),
                                wb10$`POPULAÇÃO ESTIMADA`,
                                wb11$`POPULAÇÃO ESTIMADA`,wb12$`POPULAÇÃO ESTIMADA`,
                                wb13$`POPULAÇÃO ESTIMADA`,wb14$`POPULAÇÃO ESTIMADA`,
                                wb15$`POPULAÇÃO ESTIMADA`,wb16$`POPULAÇÃO ESTIMADA`,
                                wb17$`POPULAÇÃO ESTIMADA`),
                      "Pop"="Populacao")
break()
write.csv(df_plot1,"C:/Users/João Bazzo/Dropbox/Infraestrutura Cicloviária no Estado do Paraná/Joao/pre_processed_data/pop_ibge_municipio_2001-2018.csv")
