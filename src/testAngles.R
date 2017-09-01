library(ggplot2)

awaToVect <- function(angle, len){
  # x, y
  c(cos(deg2rad(angle))*len, sin(deg2rad(angle))*len)
  
  
}

awa <- 320
aws <- 10
sog <- 5

awvect <- awaToVect(awa,aws)
twvect <- awaToVect(awa,aws) - c(sog, 0)



twangle <- rad2deg(atan2(twvect[2], twvect[1]))
twlength <- sqrt(abs(twvect[1])^2+abs(twvect[2])^2)


ggplot()+
  geom_point(aes(x=awvect[1], y=awvect[2]), color="red")+
  geom_point(aes(x=twvect[1], y=twvect[2]), color="green")+
  geom_spoke(aes(x=0, y=0, angle=deg2rad(awa), radius=aws, color="AW"), arrow=arrow(), color="red")+
  geom_spoke(aes(x=sog, y=0, angle=deg2rad(twangle), radius=twlength, color="TW"), arrow=arrow(), color="green")+
  geom_spoke(aes(x=0, y=0, angle=0, radius=sog), arrow=arrow(), color="blue")
