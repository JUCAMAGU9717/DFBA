library(NlcOptim)
library(readxl)
#finite elements
n=2
h=10/n
#initial conditions
cin=c(10.8,0.4,0.21,0.001)
dato=c()
flux=c()
tiempo=0
#Residuals
R=c(1,-5.999966,2.999991,-5.999966,0,4.99997,-5.727473,10.163951,0,1.163985,2.000002,-9.163974,0,-0.163978,0.727487,5.000004)
MR=matrix(R,ncol=4,byrow=FALSE)
lagone=c(-0.99999,1.66667,-1.33334,1.66667)
  #objective function
  objfun=function(x){-((1/h)*(0.27791*x[14] + 0.44444*x[15] + 0.27778*x[16])+ (1/h)*(0.27791*x[38] + 0.44444*x[39] + 0.27778*x[40])+h*x[24]+h*x[48])}
  #constraint function 
  confun=function(x){ 
    f=NULL
    #first element
    #glucose
    f=rbind(f,x[1]-cin[1])
    f=rbind(f,MR[2,1]*x[1]+MR[2,2]*x[2]+MR[2,3]*x[3]+MR[2,4]*x[4]-x[21]*x[14]*h)
    f=rbind(f,MR[3,1]*x[1]+MR[3,2]*x[2]+MR[3,3]*x[3]+MR[3,4]*x[4]-x[21]*x[15]*h)
    f=rbind(f,MR[4,1]*x[1]+MR[4,2]*x[2]+MR[4,3]*x[3]+MR[4,4]*x[4]-x[21]*x[16]*h)
    #Acetate
    f=rbind(f,x[5]-cin[2])
    f=rbind(f,MR[2,1]*x[5]+MR[2,2]*x[6]+MR[2,3]*x[7]+MR[2,4]*x[8]-x[22]*x[14]*h)
    f=rbind(f,MR[3,1]*x[5]+MR[3,2]*x[6]+MR[3,3]*x[7]+MR[3,4]*x[8]-x[22]*x[15]*h)
    f=rbind(f,MR[4,1]*x[5]+MR[4,2]*x[6]+MR[4,3]*x[7]+MR[4,4]*x[8]-x[22]*x[16]*h)
    #Oxygen
    f=rbind(f,x[9]-cin[3])
    f=rbind(f,MR[2,1]*x[9]+MR[2,2]*x[10]+MR[2,3]*x[11]+MR[2,4]*x[12]-x[23]*x[14]*h)
    f=rbind(f,MR[3,1]*x[9]+MR[3,2]*x[10]+MR[3,3]*x[11]+MR[3,4]*x[12]-x[23]*x[15]*h)
    f=rbind(f,MR[4,1]*x[9]+MR[4,2]*x[10]+MR[4,3]*x[11]+MR[4,4]*x[12]-x[23]*x[16]*h)
    #Biomass
    f=rbind(f,x[13]-cin[4])
    f=rbind(f,MR[2,1]*x[13]+MR[2,2]*x[14]+MR[2,3]*x[15]+MR[2,4]*x[16]-x[24]*x[14]*h)
    f=rbind(f,MR[3,1]*x[13]+MR[3,2]*x[14]+MR[3,3]*x[15]+MR[3,4]*x[16]-x[24]*x[15]*h)
    f=rbind(f,MR[4,1]*x[13]+MR[4,2]*x[14]+MR[4,3]*x[15]+MR[4,4]*x[16]-x[24]*x[16]*h)
    #continuity conditions
    f=rbind(f,-x[25]+lagone[1]*x[1]+lagone[2]*x[2]+lagone[3]*x[3]+lagone[4]*x[4])
    f=rbind(f,-x[29]+lagone[1]*x[5]+lagone[2]*x[6]+lagone[3]*x[7]+lagone[4]*x[8])
    f=rbind(f,-x[33]+lagone[1]*x[9]+lagone[2]*x[10]+lagone[3]*x[11]+lagone[4]*x[12])
    f=rbind(f,-x[37]+lagone[1]*x[13]+lagone[2]*x[14]+lagone[3]*x[15]+lagone[4]*x[16])
    #second element
    #glucose
    f=rbind(f,MR[2,1]*x[25]+MR[2,2]*x[26]+MR[2,3]*x[27]+MR[2,4]*x[28]-x[45]*x[38]*h)
    f=rbind(f,MR[3,1]*x[25]+MR[3,2]*x[26]+MR[3,3]*x[27]+MR[3,4]*x[28]-x[45]*x[39]*h)
    f=rbind(f,MR[4,1]*x[25]+MR[4,2]*x[26]+MR[4,3]*x[27]+MR[4,4]*x[28]-x[45]*x[40]*h)
    #Acetate
    f=rbind(f,MR[2,1]*x[29]+MR[2,2]*x[30]+MR[2,3]*x[31]+MR[2,4]*x[32]-x[46]*x[38]*h)
    f=rbind(f,MR[3,1]*x[29]+MR[3,2]*x[30]+MR[3,3]*x[31]+MR[3,4]*x[32]-x[46]*x[39]*h)
    f=rbind(f,MR[4,1]*x[29]+MR[4,2]*x[30]+MR[4,3]*x[31]+MR[4,4]*x[32]-x[46]*x[40]*h)
    #Oxygen
    f=rbind(f,MR[2,1]*x[33]+MR[2,2]*x[34]+MR[2,3]*x[35]+MR[2,4]*x[36]-x[47]*x[38]*h)
    f=rbind(f,MR[3,1]*x[33]+MR[3,2]*x[34]+MR[3,3]*x[35]+MR[3,4]*x[36]-x[47]*x[39]*h)
    f=rbind(f,MR[4,1]*x[33]+MR[4,2]*x[34]+MR[4,3]*x[35]+MR[4,4]*x[36]-x[47]*x[40]*h)
    #Biomass
    f=rbind(f,MR[2,1]*x[37]+MR[2,2]*x[38]+MR[2,3]*x[39]+MR[2,4]*x[40]-x[48]*x[38]*h)
    f=rbind(f,MR[3,1]*x[37]+MR[3,2]*x[38]+MR[3,3]*x[39]+MR[3,4]*x[40]-x[48]*x[39]*h)
    f=rbind(f,MR[4,1]*x[37]+MR[4,2]*x[38]+MR[4,3]*x[39]+MR[4,4]*x[40]-x[48]*x[40]*h)
    #continuity conditions
    f=rbind(f,-x[49]+lagone[1]*x[25]+lagone[2]*x[26]+lagone[3]*x[27]+lagone[4]*x[28])
    f=rbind(f,-x[50]+lagone[1]*x[29]+lagone[2]*x[30]+lagone[3]*x[31]+lagone[4]*x[32])
    f=rbind(f,-x[51]+lagone[1]*x[33]+lagone[2]*x[34]+lagone[3]*x[35]+lagone[4]*x[36])
    f=rbind(f,-x[52]+lagone[1]*x[37]+lagone[2]*x[38]+lagone[3]*x[39]+lagone[4]*x[40])
    g=NULL
    
    return(list(ceq=f,c=g)) 
  }
  #linear constraints
  Aeq=read_excel("C:/Users/USER/Documents/DFBA/RED/mahadevan/mahadevan_dospasos.xlsx",sheet=1,col_names=F)
  Aeq=as.matrix(Aeq)
  b=numeric(nrow(Aeq))
  #initial approximation
  x0=numeric(ncol(Aeq))
  #lower bound
  lb=c(numeric(16),0,0,0,0,-15,-15,-1.5,0,numeric(16),0,0,0,0,-15,-15,-1.5,0,numeric(4))
  #upper bound
  ub=c(rep(150,ncol(Aeq)))
  S=solnl(x0,objfun=objfun,confun=confun,lb=lb,ub=ub,Aeq=Aeq,Beq = b)
  s1=S[1]
  #Time
  t1=tiempo
  t2=tiempo + h*0.112702
  t3=tiempo + h*0.5
  t4=tiempo + h*0.887298
  t5=tiempo + h
  t6=t5 + h*0.112702
  t7=t5 + h*0.5
  t8=t5 + h*0.887298
  t9=t5 + h
  #Results
  dato=c(dato,t1,s1$par[1],s1$par[5],s1$par[9],s1$par[13],t2,s1$par[2],s1$par[6],s1$par[10],s1$par[14],t3,s1$par[3],s1$par[7],s1$par[11],s1$par[15],t4,s1$par[4],s1$par[8],s1$par[12],s1$par[16],t5,s1$par[25],s1$par[29],s1$par[33],s1$par[37],
         t6,s1$par[26],s1$par[30],s1$par[34],s1$par[38],t7,s1$par[27],s1$par[31],s1$par[35],s1$par[39],t8,s1$par[28],s1$par[32],s1$par[36],s1$par[40],t9,s1$par[49],s1$par[50],s1$par[51],s1$par[52])
  data=matrix(dato,ncol = 5,byrow = TRUE)
  flux=c(flux,s1$par[17],s1$par[18],s1$par[19],s1$par[20],s1$par[21],s1$par[22],s1$par[23],s1$par[24],
         s1$par[41],s1$par[42],s1$par[43],s1$par[44],s1$par[45],s1$par[46],s1$par[47],s1$par[48])
  flujo=matrix(flux,ncol=8,byrow=TRUE)
 
#-----------------------------Concentrations Graphs-----------------------------
t=data[,1]
glc=data[,2]
ac=data[,3]
o2=data[,4]
x=data[,5]

plot(t,glc,col="black",xlab="Time (h)",ylab="Glucose (mM)", type ="l")
plot(t,ac,col="red",xlab="Time (h)",ylab="Acetate (mM)", type ="l")
plot(t,o2,col="blue",xlab="Time (h)",ylab="Oxygen (mM)", type ="l")
plot(t,x,col="orange",xlab="Time (h)",ylab="Biomass (g/L)", type ="l")

#--------------------------------Fluxes Graphs---------------------------------------------
#v2
a <- function(x){ 
  ifelse(( x < 5),0.1049,ifelse((5<x & x<10),0.014, ifelse((x>10),0, NA))) 
} 
plot(a,xlim=c(0,10), ylim = c(0, 0.15), col = "gray",xlab="Time (h)",ylab="mmol/gDW*h") 
#v4
a <- function(x){ 
  ifelse(( x < 5),0.5259,ifelse((5<x & x<10),0.6072, ifelse((x>10),0, NA))) 
} 
plot(a,xlim=c(0,10), ylim = c(0, 0.8), col = "brown",xlab="Time (h)",ylab="mmol/gDW*h") 
#b1
a <- function(x){ 
  ifelse(( x < 5),-11.1070,ifelse((5<x & x<10),-11.8106, ifelse((x>10),0, NA))) 
} 
plot(a,xlim=c(0,10), ylim = c(0,-12), col = "black",xlab="Time (h)",ylab="mmol/gDW*h") 
#b2
a <- function(x){ 
  ifelse(( x < 5),6.3747,ifelse((5<x & x<10),7.3601, ifelse((x>10),0, NA))) 
} 
plot(a,xlim=c(0,10), ylim = c(0,8), col = "red",xlab="Time (h)",ylab="mmol/gDW*h") 
#b3
a <- function(x){ 
  ifelse(( x < 5),-1.3558,ifelse((5<x & x<10),-0.1813, ifelse((x>10),0, NA))) 
} 
plot(a,xlim=c(0,10), ylim = c(0,-2), col = "blue",xlab="Time (h)",ylab="mmol/gDW*h") 
#b4
a <- function(x){ 
  ifelse(( x < 5),0.6309,ifelse((5<x & x<10),0.6213, ifelse((x>10),0, NA))) 
} 
plot(a,xlim=c(0,10), ylim = c(0,0.8), col = "orange",xlab="Time (h)",ylab="mmol/gDW*h") 


