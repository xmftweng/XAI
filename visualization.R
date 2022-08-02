rm(list = ls())
library(ggplot2)


## 
name=c("#FF9933","#00BB00")
mydata<-data.frame(country=c("China","USA"),
                   OFR_FSI=c(0.0063856857427574184,0.0045209613287376382))
mydata$Cut<-factor(mydata$country,levels=mydata$Cut)
ggplot(data=mydata,aes(country,OFR_FSI))+
  geom_bar(stat="identity",width = 0.5,colour="black",
           size=0.25,fill=name,alpha=1)
  # size=0.25,fill="#FC4E07",alpha=1)



# 雷达图
# 中美1-雷达图
coord_radar <- function (theta = "x", start = 0, direction = 1) 
{  theta <- match.arg(theta, c("x", "y"))
r <- if (theta == "x") 
  "y"
else "x"
ggproto("CoordRadar", CoordPolar, theta = theta, r = r, start = start, 
        direction = sign(direction),
        is_linear = function(coord) TRUE)}

label_data<-data.frame(
  car=c("Day1" , "Day2" ,"Day3" ,  "Day4" , "Day5","Day6","Day7" ),
  id=c(1:7) ,
  China=c(0.013496247,	0.006324252,	0.016019138,
          0.011544235,	0.010568208,	0.032245403,
          0.034449311
),
  USA=c(0.021468172,	0.009343116,	0.007510358,
        0.008815353,	0.008232973,	0.022923594,
        0.047530163
)
)

AddRow<-c(NA,nrow(label_data)+1,label_data[1,ncol(label_data)-1],label_data[1,ncol(label_data)])
mydata<-rbind(label_data,AddRow)

myAngle<- 360- 360 * (label_data$id-1) /nrow(label_data)  

mydata<-melt(mydata,id=c("car", "id"))

ggplot(data=mydata,aes(x=id, y=value,group=variable,fill=variable)) + 
  geom_polygon(colour="black",alpha=0.1)+
  geom_point(size=2,shape=21,color = 'black')+
  coord_radar()+
  #coord_polar() +
  scale_x_continuous(breaks =label_data$id,labels=label_data$car)+
  theme_bw() +
  ylim(0,0.05)+
  theme(axis.text.x=element_text(size = 11,colour="black",angle = myAngle),
        axis.title=element_text(size=15,face="plain",color="black"),
        axis.text = element_text(size=12,face="plain",color="black"),
        panel.grid.major = element_line(color="grey80"),
        axis.line = element_line(color="black"),
        axis.ticks =  element_line(color="black"))

