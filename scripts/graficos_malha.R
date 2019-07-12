# -----------------
#
# grafico - crescimento da malha
#
# -----------------
require(openxlsx)
require(ggplot2)
require(gridExtra)
rm(list=ls())
setwd("C:/Users/João Bazzo/Dropbox/Infraestrutura Cicloviária no Estado do Paraná/Recentes - Mauricio/")
Curitiba <- read.xlsx("Crescimento da Malha.xlsx",sheet = 1,startRow = 2)
Londrina <- read.xlsx("Crescimento da Malha.xlsx",sheet = 2,startRow = 2)
Maringa <- read.xlsx("Crescimento da Malha.xlsx",sheet = 3,startRow = 2)
# data.frame
df <- data.frame("Cidade"=c(rep("Curitiba",dim(Curitiba)[1]),
                            rep("Londrina",dim(Londrina)[1]),
                            rep("Maringa",dim(Maringa)[1])),
                 "Ano"=c(Curitiba$Ano,Londrina$Ano,Maringa$Ano),
                 "Malha"=c(Curitiba$`Malha.(km)`,Londrina$`Malha.(m)`/1000,
                           Maringa$`Malha.(km)`))
df$Ano <- factor(df$Ano,unique(df$Ano))
# plot
p <- ggplot(data = df[df$Cidade=="Curitiba",],aes(x=Ano,y=Malha,group=1)) + 
  geom_point(aes(x=Ano,y=Malha,color = Cidade),col=brewer.pal(n = 8, name = "Set2")[1])+
  geom_line(aes(x=Ano,y=Malha,color = Cidade),col=brewer.pal(n = 8, name = "Set2")[1])+
  #geom_line(aes(x=Ano,y=Malha,color = Cidade))+
  facet_grid(Cidade ~ ., scales = "free_y")+
  ylab("Malha cicloviária (km)")+xlab(NULL)+
  #scale_fill_discrete(guide=FALSE)+
  labs(color="none")+
  guides(color=guide_legend(ncol=2))+
  theme(legend.title=element_blank(),legend.background = element_rect(fill="white"),
        legend.position=c(0.155,0.8250),text=element_text(size=10, family="Arial"))+
  theme(axis.text.x = element_text(angle=0,size=10.2,vjust=0.75))
p
df1 <- df[df$Cidade=="Londrina",];df1$Ano <- factor(df1$Ano,unique(df1$Ano))
p1 <- ggplot(data = df1,aes(x=Ano,y=Malha,group=1)) + 
  geom_point(aes(x=Ano,y=Malha,color = Cidade),col=brewer.pal(n = 8, name = "Set2")[2])+
  geom_line(aes(x=Ano,y=Malha,color = Cidade),col=brewer.pal(n = 8, name = "Set2")[2])+
  #geom_line(aes(x=Ano,y=Malha,color = Cidade))+
  facet_grid(Cidade ~ ., scales = "free_y")+
  ylab("Malha cicloviária (km)")+xlab(NULL)+
  #scale_fill_discrete(guide=FALSE)+
  labs(color="none")+
  guides(color=guide_legend(ncol=2))+
  theme(legend.title=element_blank(),legend.background = element_rect(fill="white"),
        legend.position=c(0.155,0.8250),text=element_text(size=10, family="Arial"))+
  theme(axis.text.x = element_text(angle=45,size=10.2,vjust=0.75))
p1
df1 <- df[df$Cidade=="Maringa",];df1$Ano <- factor(df1$Ano,unique(df1$Ano))
df1$Cidade <- "Maringá"
p2 <- ggplot(data = df1,aes(x=Ano,y=Malha,group=1)) + 
  geom_point(aes(x=Ano,y=Malha,color = Cidade),col=brewer.pal(n = 8, name = "Set2")[4])+
  geom_line(aes(x=Ano,y=Malha,color = Cidade),col=brewer.pal(n = 8, name = "Set2")[4])+
  #geom_line(aes(x=Ano,y=Malha,color = Cidade))+
  facet_grid(Cidade ~ ., scales = "free_y")+
  ylab("Malha cicloviária (km)")+xlab(NULL)+
  #scale_fill_discrete(guide=FALSE)+
  labs(color="none")+
  guides(color=guide_legend(ncol=2))+
  theme(legend.title=element_blank(),legend.background = element_rect(fill="white"),
        legend.position=c(0.155,0.8250),text=element_text(size=10, family="Arial"))+
  theme(axis.text.x = element_text(angle=0,size=10.2,vjust=0.75))
p2

pf1 <- grid.arrange(p,p1,p2,ncol=3)
ggsave(filename = "C:/Users/João Bazzo/Dropbox/Infraestrutura Cicloviária no Estado do Paraná/Joao/graficos/malha_cicloviaria1.jpg",
       width = 36,height = 8,units = "cm",scale = 0.85,dpi = 300,plot = pf1)

pf <- grid.arrange(p,p1,p2,ncol=1)
ggsave(filename = "C:/Users/João Bazzo/Dropbox/Infraestrutura Cicloviária no Estado do Paraná/Joao/graficos/malha_cicloviaria.jpg",
       width = 16,height = 24,units = "cm",scale = 0.85,dpi = 300,plot = pf)
