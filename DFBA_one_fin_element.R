library(NlcOptim)
library(readxl)
#finite elements
n=1
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
  objfun=function(x){-((1/h)*(0.27791*x[14] + 0.44444*x[15] + 0.27778*x[16])+h*x[24])}
  #constraint function 
  confun=function(x){ 
    f=NULL
    #glucose
    f=rbind(f,x[1]-cin[1])
    f=rbind(f,MR[2,1]*x[1]+MR[2,2]*x[2]+MR[2,3]*x[3]+MR[2,4]*x[4]-x[21]*x[14]*h)
    f=rbind(f,MR[3,1]*x[1]+MR[3,2]*x[2]+MR[3,3]*x[3]+MR[3,4]*x[4]-x[21]*x[15]*h)
    f=rbind(f,MR[4,1]*x[1]+MR[4,2]*x[2]+MR[4,3]*x[3]+MR[4,4]*x[4]-x[21]*x[16]*h)
    #acetate
    f=rbind(f,x[5]-cin[2])
    f=rbind(f,MR[2,1]*x[5]+MR[2,2]*x[6]+MR[2,3]*x[7]+MR[2,4]*x[8]-x[22]*x[14]*h)
    f=rbind(f,MR[3,1]*x[5]+MR[3,2]*x[6]+MR[3,3]*x[7]+MR[3,4]*x[8]-x[22]*x[15]*h)
    f=rbind(f,MR[4,1]*x[5]+MR[4,2]*x[6]+MR[4,3]*x[7]+MR[4,4]*x[8]-x[22]*x[16]*h)
    #oxygen
    f=rbind(f,x[9]-cin[3])
    f=rbind(f,MR[2,1]*x[9]+MR[2,2]*x[10]+MR[2,3]*x[11]+MR[2,4]*x[12]-x[23]*x[14]*h)
    f=rbind(f,MR[3,1]*x[9]+MR[3,2]*x[10]+MR[3,3]*x[11]+MR[3,4]*x[12]-x[23]*x[15]*h)
    f=rbind(f,MR[4,1]*x[9]+MR[4,2]*x[10]+MR[4,3]*x[11]+MR[4,4]*x[12]-x[23]*x[16]*h)
    #Biomass
    f=rbind(f,x[13]-cin[4])
    f=rbind(f,MR[2,1]*x[13]+MR[2,2]*x[14]+MR[2,3]*x[15]+MR[2,4]*x[16]-x[24]*x[14]*h)
    f=rbind(f,MR[3,1]*x[13]+MR[3,2]*x[14]+MR[3,3]*x[15]+MR[3,4]*x[16]-x[24]*x[15]*h)
    f=rbind(f,MR[4,1]*x[13]+MR[4,2]*x[14]+MR[4,3]*x[15]+MR[4,4]*x[16]-x[24]*x[16]*h)
    #continuity constraints
    f=rbind(f,-x[25]+lagone[1]*x[1]+lagone[2]*x[2]+lagone[3]*x[3]+lagone[4]*x[4])
    f=rbind(f,-x[26]+lagone[1]*x[5]+lagone[2]*x[6]+lagone[3]*x[7]+lagone[4]*x[8])
    f=rbind(f,-x[27]+lagone[1]*x[9]+lagone[2]*x[10]+lagone[3]*x[11]+lagone[4]*x[12])
    f=rbind(f,-x[28]+lagone[1]*x[13]+lagone[2]*x[14]+lagone[3]*x[15]+lagone[4]*x[16])
    g=NULL
    return(list(ceq=f,c=g)) 
  }
  #linear constraints
  Aeq=read_excel("C:/Users/USER/Documents/DFBA/RED/mahadevan/ejemahadevan.xlsx",sheet=3,col_names=F)
  Aeq=as.matrix(Aeq)
  b=numeric(nrow(Aeq))
  #initial approximation
  x0=numeric(ncol(Aeq))
  #lower bound
  lb=c(numeric(16),0,0,0,0,-15,-15,-1.5,0,numeric(4))
  #upper bound
  ub=c(rep(150,28))
  S=solnl(x0,objfun=objfun,confun=confun,lb=lb,ub=ub,Aeq=Aeq,Beq = b)
  s1=S[1]
  #Time
  t1=tiempo
  t2=tiempo + h*0.112702
  t3=tiempo + h*0.5
  t4=tiempo + h*0.887298
  t5=tiempo + h
  #Results
  dato=c(dato,t1,s1$par[1],s1$par[5],s1$par[9],s1$par[13],t2,s1$par[2],s1$par[6],s1$par[10],s1$par[14],t3,s1$par[3],s1$par[7],s1$par[11],s1$par[15],t4,s1$par[4],s1$par[8],s1$par[12],s1$par[16],t5,s1$par[25],s1$par[26],s1$par[27],s1$par[28])
  data=matrix(dato,ncol = 5,byrow = TRUE)
  flux=c(flux,s1$par[17],s1$par[18],s1$par[19],s1$par[20],s1$par[21],s1$par[22],s1$par[23],s1$par[24])
  flujo=matrix(flux,ncol=8,byrow=TRUE)




