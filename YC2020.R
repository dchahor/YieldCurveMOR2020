library(readxl)
library(gganimate)
library(ggplot2)
library(ggrepel)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(reshape2)

#Lire le fichier Yield Curve via read_excel
yc=read_excel("~/YieldCurve2020.xlsx",sheet = "yc",col_types = c("date",rep("numeric",9)))

# definition d'une nouvelle variable mat qui va contenir les attributs 13s, 36s ..., pour une meilleure visualisation sur ggplot
yc=melt(yc,id.vars = 'date', variable.name = 'mat')

#Confirmation du format as.Date de la variable Date
yc$date=as.Date(yc$date,format = "%d-%m-%y")
yc=yc[order(yc$date),]

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#Ajout des evenements qu'a connu 2020 (Baisse du taux, levee a l'international) est definir le temps d'apparition et disparition de chaque element
dimyc=dim(yc)[1]
yc$events=rep(NA,dimyc)
yc[yc$date>="2020-03-17" & yc$date<='2020-04-05',]$events="Baisse du taux directeur de 25pbs"
yc[yc$date>="2020-06-15" & yc$date<='2020-07-06',]$events="Baisse du taux directeur de 50pbs"
yc[yc$date>="2020-09-23" & yc$date<='2020-10-06',]$events="Levée de 1bn EUR à l'international"
yc[yc$date>="2020-12-09" & yc$date<='2020-12-26',]$events="Levée de 3bn USD à l'international"

#Definir le taux directeur sur le graphe et les evenement de baisse de 2,25% à 2% puis à 1,50%
yc$dir=0.0225
yc[yc$date>="2020-03-17" & yc$date<='2020-06-15',]$dir=0.02
yc[yc$date>="2020-06-16",]$dir=0.015

#Definition de l'animation par un plot ggplot 
p=ggplot(data=yc,aes(x=mat,y=value),group=mat)+
  geom_line(aes(group=1),size=1.1,colour="#0F95D7")+
  geom_line(aes(y=dir,group=1),linetype="dashed",size=1.1,col="#FF2700")+
  geom_text(aes(y = dir,x=8,label=paste("Taux Direct.",as.factor(dir*100),"%")),vjust = -1,hjust=0.8,family="Decima X",col="#FF2700",check_overlap = TRUE,size=4)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L),breaks = scales::pretty_breaks(n = 7))+
  theme_fivethirtyeight()+
  geom_text(aes(family="Atlas Grotesk",x=8, y=0.05 , label = as.factor(firstup(months(date)))), alpha = 0.8,  col = "gray", size = 8,check_overlap = TRUE)+
  geom_text(aes(family="Atlas Grotesk",x=6, y=0.046 , label = as.factor(events)), alpha = 0.5,  col = "black", size = 4,check_overlap = TRUE,hjust=0.5)+
  labs(title="Courbe des Taux Maroc en 2020",x="",y="",caption="© Daoud Chahor")+
  theme(axis.title = element_text(family = "Atlas Grotesk"), axis.text=element_text(family = "Decima X"),plot.caption=element_text(family = "Decima X"),legend.text = element_text(size = 10))
p=p+geom_text(data=yc[yc$mat=="5A",],aes(family="Decima X",x=mat,y=value,label=sprintf("%0.2f",value*100)),vjust=-1.5,colour="#810F7C")
p=p+geom_point(data=yc[yc$mat=="5A",],aes(x=mat,y=value),colour = "#810F7C", size = 2)
 
# Ajouter l'animation au plot existant, states=date definit que la base de l'animation est la variable date 
anim <- p +     transition_states(states = date,transition_length = 2)

#Rendering de l'animation et output av (cad format video .mp4)
animate(anim, fps = 60, duration = 70,rewind = FALSE,renderer = av_renderer())

