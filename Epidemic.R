
#Epidemic Models
#-Deterministic model (DCM).
#-Individual contact model (ICM).
#-Network models

# Susceptible Unfectious Susceptible (SIS)

#Instalar el modelo epidimiologico.
#install.packages("EpiModel",dependencies=TRUE)

#Cargar las libreria.
library(EpiModel)

#Creacion de la red neuronal vacia. 
nw<-network.initialize(n=1000, directed = FALSE)

#Definir los atributos de los nodos
nw<-set.vertex.attribute(nw, "risk",rep(0:1,each=500))

#Grafica Inicial
plot(nw)

#Colorea los nodos por el atributo "risk"
myrisk<-get.vertex.attribute(nw,"risk")

myrisk

plot(nw,vertex.col=myrisk+4)

#El modelo especifica como las personas en la poblacion forman y disuelve 
#relaciones a traves del tiempo.


#Prediccion de formacion de parejas.
formation<-~edges +nodefactor("risk")+nodematch("risk")+concurrent
target.stats<-c(250,375,225,100)
#Estos valores objetivo representan los valores esperados de la estadistica de la
# para cada momento en el tiempo de la red dynamica.

#50 muestas de tiempo con una probabilidad de 2% en cada muestra de que la relacion se disuelva. 

coef.diss<-dissolution_coefs(dissolution=~offset(edges),duration = 50)
coef.diss

#Funcion netest, invoca funciones de estimacion para los coeficientes de formacion y disolucion

est1<-netest(nw,formation,target.stats,coef.diss)

dx<-netdx(est1,nsims = 10, nsteps = 1000)

dx
plot(dx)

par(mfrow=c(1,2))
plot(dx,type="duration")
abline(v=200,col=2)
plot(dx,type="dissolution")

init<-init.net(i.num=50)

param<-param.net(inf.prob = 0.1,act.rate = 5, rec.rate=0.02)
param

control<-control.net(type="SIS",nsteps = 500,nsims = 10, epi.by = "risk")

sim1<-netsim(est1,param,init,control)
summary(sim1, at=500)

mySimData<-as.data.frame(sim1)

par(mfrow=c(1,1))

plot(sim1)

plot(sim1,y = c("si.flow","is.flow"),leg=TRUE)
plot(sim1,y = c("i.num.risk0","i.num.risk1"),leg=TRUE)


par(mfrow=c(1,2),mar=c(0,0,1,0))

plot(sim1,type="network",at=1,sims="mean",col.status = TRUE, main="Prevalence at t1")
plot(sim1,type="network",at=500,sims="mean",col.status = TRUE, main="Prevalence at 500")

