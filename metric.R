attach(All_data_clean_final)
#thrickness<-c(Right_ACgG_anterior_cingulate_gyrus_thickness, )
roi<-All_data_clean_final
str(roi)
thickness<-c()
for (i in 1:312){
  if (i%%3==0)
  {thickness<-c(thickness, roi[i])}
}  
thickness<-data.frame(thickness)
write.table(thickness, file = "thickness", row.names = F, sep =" ")
tau<-c()
start<-1
step<-3
n<-seq(start, 312, by=step)
for (i in n){
  
  tau<-c(tau,roi[i])
}
tau<-data.frame(tau)
attach (thickness)
attach(tau)
print(log(unlist(tau[1])))

library('MASS')
m<-rlm(unlist(thickness[104])~unlist(tau[104]))
resid(m)
resid(rlm(Left_TTG_transverse_temporal_gyrus_thickness ~ log(Left_TTG_transverse_temporal_gyrus_tau)))
print(mod<-rlm(unlist(thickness[104])~(log10(unlist(tau[104])))))
print(resid(mod))
l<-matrix(0, nrow=258, ncol=104)
for (i in 1:104){
  print(i)
  filename=i
  mod<-rlm(unlist(thickness[i])~(log10(unlist(tau[i]))), psi=psi.bisquare)
  l[,i]<-(resid(mod))
  #write.table(scale(resid(mod)), paste(i, ".text", sep =" "), row.names = F)
}

re<-data.frame(l)

bin<-matrix(0,nrow=258, ncol=104)
for (i in 1:104){
  for (num in 1:258){
    if (re[num,i]< -1.5*(sd(re[,i]))){
      bin[num,i]=-1
    }
    else if (re[num,i]>1.5*(sd(re[,i]))){
      bin[num,i]=1
    }
    else {bin[num,i]=0}
  }
}
b<-matrix(0, nrow=258, ncol=208)
for (i in 1:104){
  for (num in 1:258){
    if (bin[num, i]==1){
      b[num, 2*i-1]<-1
      b[num, 2*i]<-0}
    else if (bin[num, i]==-1){
      b[num, 2*i-1]<-0
      b[num, 2*i]<-1}
    else{ b[num, 2*i-1]<-0
    b[num, 2*i]<-0}
  }
}

binaa<-data.frame(b)
