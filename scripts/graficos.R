
setwd("C:/Users/João Bazzo/Dropbox/Infraestrutura Cicloviária no Estado do Paraná/Joao/")
rm(list=ls()) #
require(ggplot2)
require(stringr)

df_plot <- read.csv("pre_processed_data/frota_municipio_2001-2018.csv")
df_plot1 <- read.csv("pre_processed_data/pop_ibge_municipio_2001-2018.csv")
#df_plot1$Cidade <- str_replace_all(df_plot1$Cidade,"MARINGÁ","MARINGA")
df_plot1$Cidade <- str_replace_all(df_plot1$Cidade,"PARANAGUÁ","PARANAGUA")
mun <- c("ANTONINA","CURITIBA","GUARAPUAVA","LONDRINA","PARANAGUA")
# ---
# merge data
df_plot$tx <- 0
for(i in 1:length(mun)){
  ind <- which(df_plot$Cidade==mun[i])
  ind1 <- which(df_plot1$Cidade==mun[i])
  df_plot$tx[ind] <- df_plot$Total[ind]/df_plot1$Total[ind1]
}
df_plot$Cidade <- str_replace_all(df_plot$Cidade,mun[2],"Curitiba")
df_plot$Cidade <- str_replace_all(df_plot$Cidade,mun[3],"Guarapuava")
df_plot$Cidade <- str_replace_all(df_plot$Cidade,mun[4],"Londrina")
df_plot$Cidade <- str_replace_all(df_plot$Cidade,mun[1],"Antonina")
df_plot$Cidade <- str_replace_all(df_plot$Cidade,mun[5],"Paranaguá")
# ---
break()
p <- ggplot(data = df_plot,aes(x=factor(Ano),y=tx,color = Cidade)) + 
  geom_point(aes(x=Ano,y=tx,color = Cidade))+
  geom_line(aes(x=Ano,y=tx,color = Cidade))+
  facet_grid(Cidade ~ ., scales = "free_y")+ylab("Taxa de motorização (automóveis/hab.)")+xlab(NULL)+
  #scale_fill_discrete(guide=FALSE)+
  labs(color="Município")+
  theme(legend.position="none",text=element_text(size=12, family="Arial"))+
  theme(axis.text.x = element_text(angle=45,size=10.2,vjust=0.75))+
  scale_x_continuous(limits=c(2001,2018),breaks = 2001:2018,name = "Ano")
p
ggsave(filename = "C:/Users/João Bazzo/Dropbox/Infraestrutura Cicloviária no Estado do Paraná/Joao/graficos/modelo_tx_1.jpg",
       width = 15,height = 20,units = "cm",scale = 1.2,dpi = 300,plot = p)
break()
# ---
mun1 <- c("Curitiba","Londrina","Guarapuava","Paranaguá","Antonina")
df_plot$Cidade <- factor(df_plot$Cidade,mun1)
p <- ggplot(data = df_plot,aes(x=factor(Ano),y=tx,color = Cidade)) + 
  geom_point(aes(x=Ano,y=tx,color = Cidade))+
  geom_line(aes(x=Ano,y=tx,color = Cidade))+
  #facet_grid(Cidade ~ ., scales = "free_y")+
  ylab("Taxa de motorização (automóveis/hab.)")+xlab(NULL)+
  #scale_fill_discrete(guide=FALSE)+
  labs(color="none")+
  guides(color=guide_legend(ncol=2))+
  theme(legend.title=element_blank(),legend.background = element_rect(fill="white"),
        legend.position=c(0.155,0.8250),text=element_text(size=10, family="Arial"))+
  theme(axis.text.x = element_text(angle=45,size=10.2,vjust=0.75))+
  scale_x_continuous(limits=c(2001,2018),breaks = 2001:2018,name = "Ano")
p
ggsave(filename = "C:/Users/João Bazzo/Dropbox/Infraestrutura Cicloviária no Estado do Paraná/Joao/graficos/modelo_11.jpg",
       width = 15,height = 8,units = "cm",scale = 1.2,dpi = 300,plot = p)
break()
#----
p <- ggplot(data = df_plot,aes(x=factor(Ano),y=Total/1000)) + 
  geom_point(aes(x=Ano,y=Total/1000,color = Cidade))+
  geom_line(aes(x=Ano,y=Total/1000,color = Cidade))+
  facet_grid(Cidade ~ ., scales = "free_y")+ylab("Frota de automóveis")+xlab(NULL)+
  #scale_fill_discrete(guide=FALSE)+
  labs(color="Município")+
  theme(legend.position="none",text=element_text(size=12, family="Arial"))+
  theme(axis.text.x = element_text(angle=45,size=10.2,vjust=0.75))+
  scale_x_continuous(limits=c(2001,2018),breaks = 2001:2018,name = "Ano")
p
ggsave(filename = "C:/Users/João Bazzo/Dropbox/Infraestrutura Cicloviária no Estado do Paraná/Joao/graficos/modelo_1.jpg",
       width = 15,height = 20,units = "cm",scale = 1.2,dpi = 300,plot = p)
# --- plot 111
#df_plot$Ano1 <- factor(df_plot$Ano,2001:2018)
p111 <- ggplot(data = df_plot,aes(x=Ano,y=tx)) + 
  geom_bar(stat = "identity",color="black",width=0.85, alpha=.8)+
  facet_grid(Cidade ~ ., scales = "free_y")+ylab("Taxa de motorização (automóveis/hab.)")+xlab(NULL)+
  scale_fill_discrete("Set1",guide=FALSE)+
  labs(color="Município")+
  theme(legend.position="none",text=element_text(size=12, family="Arial"))+
  theme(axis.text.x = element_text(angle=45,size=10.2,vjust=0.75))+
  scale_x_continuous(limits=c(2000.5,2018.5),expand = c(0.05,0.1),breaks = 2001:2018,name = "Ano")#+
#scale_x_continuous()
p111
ggsave(filename = "C:/Users/João Bazzo/Dropbox/Infraestrutura Cicloviária no Estado do Paraná/Joao/graficos/modelo_tx_12.jpg",
       width = 15,height = 20,units = "cm",scale = 1.2,dpi = 300,plot = p111)
# --- plot 2
#df_plot$Ano <- factor(df_plot$Ano,2001:2018)
p1 <- ggplot(df_plot[df_plot$Cidade==mun[1],], aes(x=Ano,y=tx))+ggtitle(mun[1])+
  geom_bar(stat="identity",colour="black",fill="lightskyblue2", width=0.85, alpha=.8)+
  labs(x=NULL,y="Taxa de motorização (autos/hab.)")+geom_text(aes(label=round(tx,1)),
                                                    fontface=1,angle=45, size=3.5,vjust=-0.42,hjust=-0.20)+
  scale_y_continuous(limits=c(0,1.3*max(df_plot[df_plot$Cidade==mun[1],"tx"])))+
  theme(plot.title = element_text(colour="black", face="bold",family="Arial", size=10,hjust = c(0.5)))+
  theme(axis.text.x = element_text(angle=45,size=10.2,vjust=0.75))+
  scale_x_discrete(expand = c(0.05,0.1))

p2 <- ggplot(df_plot[df_plot$Cidade==mun[2],], aes(x=Ano,y=tx))+ggtitle(mun[2])+
  geom_bar(stat="identity",colour="black",fill="lightskyblue2", width=0.85, alpha=.8)+
  labs(x=NULL,y=NULL)+geom_text(aes(label=round(tx,1)),
                                fontface=1,angle=45, size=3.5,vjust=-0.42,hjust=-0.20)+
  scale_y_continuous(limits=c(0,1.3*max(df_plot[df_plot$Cidade==mun[2],"tx"])))+
  theme(plot.title = element_text(colour="black", face="bold",family="Arial", size=10,hjust = c(0.5)))+
  theme(axis.text.x = element_text(angle=45,size=10.2,vjust=0.75))+
  scale_x_discrete(expand = c(0.05,0.05))

p3 <- ggplot(df_plot[df_plot$Cidade==mun[3],], aes(x=Ano,y=tx))+ggtitle(mun[3])+
  geom_bar(stat="identity",colour="black",fill="lightskyblue2", width=0.85, alpha=.8)+
  labs(x=NULL,y="Taxa de motorização (autos/hab.)")+geom_text(aes(label=round(tx,1)),
                                                    fontface=1,angle=45, size=3.5,vjust=-0.42,hjust=-0.20)+
  scale_y_continuous(limits=c(0,1.3*max(df_plot[df_plot$Cidade==mun[3],"tx"])))+
  theme(plot.title = element_text(colour="black", face="bold",family="Arial", size=10,hjust = c(0.5)))+
  theme(axis.text.x = element_text(angle=45,size=10.2,vjust=0.75))+
  scale_x_discrete(expand = c(0.05,0.05))
#
p4 <- ggplot(df_plot[df_plot$Cidade==mun[4],], aes(x=Ano,y=tx))+ggtitle(mun[4])+
  geom_bar(stat="identity",colour="black",fill="lightskyblue2", width=0.85, alpha=.8)+
  labs(x=NULL,y=NULL)+geom_text(aes(label=round(tx,1)),
                                fontface=1,angle=45, size=3.5,vjust=-0.42,hjust=-0.20)+
  scale_y_continuous(limits=c(0,1.3*max(df_plot[df_plot$Cidade==mun[4],"tx"])))+
  theme(plot.title = element_text(colour="black", face="bold",family="Arial", size=10,hjust = c(0.5)))+
  theme(axis.text.x = element_text(angle=45,size=10.2,vjust=0.75))+
  scale_x_discrete(expand = c(0.05,0.05))

p5 <- ggplot(df_plot[df_plot$Cidade==mun[5],], aes(x=Ano,y=tx))+ggtitle(mun[5])+
  geom_bar(stat="identity",colour="black",fill="lightskyblue2", width=0.85, alpha=.8)+
  labs(x=NULL,y="Taxa de motorização (autos/hab.)")+geom_text(aes(label=round(tx,1)),
                                                    fontface=1,angle=45, size=3.5,vjust=-0.42,hjust=-0.20)+
  scale_y_continuous(limits=c(0,1.3*max(df_plot[df_plot$Cidade==mun[5],"tx"])))+
  theme(plot.title = element_text(colour="black", face="bold",family="Arial", size=10,hjust = c(0.5)))+
  theme(axis.text.x = element_text(angle=45,size=10.2,vjust=0.75))+
  scale_x_discrete(expand = c(0.05,0.05))

pf <- grid.arrange(p1,p2,p3,p4,p5,ncol=2)
ggsave(filename = "C:/Users/João Bazzo/Dropbox/Infraestrutura Cicloviária no Estado do Paraná/Joao/graficos/modelo_tx_2.jpg",
       width = 27.5,height = 20,units = "cm",scale = 1.2,dpi = 300,plot = pf)
# --- plot 3
p11 <- ggplot(df_plot[df_plot$Cidade==mun[1],], aes(x=Ano,y=tx))+ggtitle(mun[1])+
  geom_point(aes(x=Ano,y=tx,color = Cidade))+
  geom_line(aes(x=as.numeric(Ano),y=tx,color = Cidade))+
  labs(x=NULL,y="Taxa de motorização (autos/hab.)")+#geom_text(aes(label=round(tx,1)),
  #         fontface=1,angle=45, size=3.5,vjust=-0.42,hjust=-0.20)+
  #scale_y_continuous(limits=c(0,1.3*max(df_plot[df_plot$Cidade==mun[1],"tx"])))+
  theme(legend.position="none",plot.title = element_text(colour="black", face="bold",family="Arial", size=10,hjust = c(0.5)))+
  theme(axis.text.x = element_text(angle=45,size=10.2,vjust=0.75))+
  scale_color_discrete("Set1")

#p11
p22 <- ggplot(df_plot[df_plot$Cidade==mun[2],], aes(x=Ano,y=tx))+ggtitle(mun[2])+
  geom_point(aes(x=Ano,y=tx,color = Cidade))+
  geom_line(aes(x=as.numeric(Ano),y=tx,color = Cidade))+
  labs(x=NULL,y=NULL)+#geom_text(aes(label=round(tx,1)),
  #         fontface=1,angle=45, size=3.5,vjust=-0.42,hjust=-0.20)+
  #scale_y_continuous(limits=c(0,1.3*max(df_plot[df_plot$Cidade==mun[1],"tx"])))+
  theme(legend.position="none",plot.title = element_text(colour="black", face="bold",family="Arial", size=10,hjust = c(0.5)))+
  theme(axis.text.x = element_text(angle=45,size=10.2,vjust=0.75))

p33 <- ggplot(df_plot[df_plot$Cidade==mun[3],], aes(x=Ano,y=tx))+ggtitle(mun[3])+
  geom_point(aes(x=Ano,y=tx,color = Cidade))+
  geom_line(aes(x=as.numeric(Ano),y=tx,color = Cidade))+
  labs(x=NULL,y="Taxa de motorização (autos/hab.)")+#geom_text(aes(label=round(tx,1)),
  #         fontface=1,angle=45, size=3.5,vjust=-0.42,hjust=-0.20)+
  #scale_y_continuous(limits=c(0,1.3*max(df_plot[df_plot$Cidade==mun[3],"tx"])))+
  theme(legend.position="none",plot.title = element_text(colour="black", face="bold",family="Arial", size=10,hjust = c(0.5)))+
  theme(axis.text.x = element_text(angle=45,size=10.2,vjust=0.75))

#p3
p44 <- ggplot(df_plot[df_plot$Cidade==mun[4],], aes(x=Ano,y=tx))+ggtitle(mun[4])+
  geom_point(aes(x=Ano,y=tx,color = Cidade))+
  geom_line(aes(x=as.numeric(Ano),y=tx,color = Cidade))+
  labs(x=NULL,y=NULL)+#geom_text(aes(label=round(tx,1)),
  #         fontface=1,angle=45, size=3.5,vjust=-0.42,hjust=-0.20)+
  #scale_y_continuous(limits=c(0,1.3*max(df_plot[df_plot$Cidade==mun[4],"tx"])))+
  theme(legend.position="none",plot.title = element_text(colour="black", face="bold",family="Arial", size=10,hjust = c(0.5)))+
  theme(axis.text.x = element_text(angle=45,size=10.2,vjust=0.75))

p55 <- ggplot(df_plot[df_plot$Cidade==mun[5],], aes(x=Ano,y=tx))+ggtitle(mun[5])+
  geom_point(aes(x=Ano,y=tx,color = Cidade))+
  geom_line(aes(x=as.numeric(Ano),y=tx,color = Cidade))+
  labs(x=NULL,y="Taxa de motorização (autos/hab.)")+#geom_text(aes(label=round(tx,1)),
  #         fontface=1,angle=45, size=3.5,vjust=-0.42,hjust=-0.20)+
  #scale_y_continuous(limits=c(0,1.3*max(df_plot[df_plot$Cidade==mun[5],"tx"])))+
  theme(legend.position="none",plot.title = element_text(colour="black", face="bold",family="Arial", size=10,hjust = c(0.5)))+
  theme(axis.text.x = element_text(angle=45,size=10.2,vjust=0.75))

pff <- grid.arrange(p11,p22,p33,p44,p55,ncol=2)
ggsave(filename = "C:/Users/João Bazzo/Dropbox/Infraestrutura Cicloviária no Estado do Paraná/Joao/graficos/modelo_tx_3.jpg",
       width = 27.5,height = 20,units = "cm",scale = 1.2,dpi = 300,plot = pff)




