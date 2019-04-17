library(parallel) 

setwd("C:/Users/meetr/Desktop/Fall 2018/R Analytics INSY 5392/project2")
d<-read.csv("final_data.csv")

#d<-d1[sample(nrow(d1), 15000), ]


#t = d$t
#number_of_Cases = d$number_of_Cases
#number_of_escalation = d$number_of_escalation
#number_of_response_missed = d$number_of_response_missed
#number_of_parts_used = d$number_of_parts_used
#number_of_response_missed = d$number_of_response_missed

ID_Alone <- d$SAID
#n=100
n <- length(ID_Alone)
time.start<-proc.time()[3] 

cl<-makeCluster(3) 

# x<-matrix(runif(1000000),ncol=5) 

clusterExport(cl=cl,ls(),envir=.GlobalEnv) 

ffff<-function(par){
  a<-par[1]
  b<-par[2]
  beta<-par[3]
  re<-par[4]
  rm<-par[5]
  rp<-par[6]
  rrm<-par[7]  
  
  
  
  y<-rep(0,n)
  z<-rep(0,n)
  p<-rep(0,n)
  x<-rep(0,n)
  
  NN<- exp((-1)*beta*d$t)*log(1+d$number_of_Cases)*(1+re*log(1+d$number_of_escalation)
                                                  +rm*log(1+d$number_of_single_visit_missed)
                                                  +rp*log(1+d$number_of_parts_used)
                                                  +rrm*log(1+d$number_of_response_missed))
  y<- NN
  #print(y)
  #z<- a+b* y
  
  for (j in 1:n) {
    z[j]<-a+b* y[j]
    p[j]<- 1/(1+exp((-1)*z[j]))
    x[j]<- d$status[j]
  }
  
  
  pp<-rep(0,n)
  for(k in 1:n) { if(x[k]==1) pp[k]<- p[k] else pp[k]<- 1-p[k]}
  logproductpp<- sum(log(pp))
  return (-logproductpp)
}
ans<-parSapply(cl=cl, 1:1000,ffff) 

MLE<-nlminb(c(0,0,0,0,0,0,0),objective=ffff, control=list(eval.max=1000, iter.max=1500))
MLE


ffff1<-function(par){
  
  b<-par[1]
  beta<-par[2]
  re<-par[3]
  rm<-par[4]
  rp<-par[5]
  rrm<-par[6]  
  
  
  
  y<-rep(0,n)
  z<-rep(0,n)
  p<-rep(0,n)
  x<-rep(0,n)
  
  NN<- exp((-1)*beta*d$t)*log(1+d$number_of_Cases)*(1+re*log(1+d$number_of_escalation)
                                                    +rm*log(1+d$number_of_single_visit_missed)
                                                    +rp*log(1+d$number_of_parts_used)
                                                    +rrm*log(1+d$number_of_response_missed))
  y<- NN
  #print(y)
  #z<- a+b* y
  
  for (j in 1:n) {
    z[j]<-b* y[j]
    p[j]<- 1/(1+exp((-1)*z[j]))
    x[j]<- d$status[j]
  }
  
  
  pp<-rep(0,n)
  for(k in 1:n) { if(x[k]==1) pp[k]<- p[k] else pp[k]<- 1-p[k]}
  logproductpp<- sum(log(pp))
  return (-logproductpp)
}
ans<-parSapply(cl=cl, 1:1000,ffff1) 

MLE_a<-nlminb(c(0,0,0,0,0,0),objective=ffff1, control=list(eval.max=1000, iter.max=1500))
MLE_a

p_a<- 1-pchisq(2*(MLE_a$objective-MLE$objective),1)
p_a


ffff2<-function(par){
  a<-par[1]
  
  beta<-par[2]
  re<-par[3]
  rm<-par[4]
  rp<-par[5]
  rrm<-par[6]  
  
  
  
  y<-rep(0,n)
  z<-rep(0,n)
  p<-rep(0,n)
  x<-rep(0,n)
  
  NN<- exp((-1)*beta*d$t)*log(1+d$number_of_Cases)*(1+re*log(1+d$number_of_escalation)
                                                    +rm*log(1+d$number_of_single_visit_missed)
                                                    +rp*log(1+d$number_of_parts_used)
                                                    +rrm*log(1+d$number_of_response_missed))
  y<- NN
  #print(y)
  #z<- a+b* y
  
  for (j in 1:n) {
    z[j]<-a
    p[j]<- 1/(1+exp((-1)*z[j]))
    x[j]<- d$status[j]
  }
  
  
  pp<-rep(0,n)
  for(k in 1:n) { if(x[k]==1) pp[k]<- p[k] else pp[k]<- 1-p[k]}
  logproductpp<- sum(log(pp))
  return (-logproductpp)
}
ans<-parSapply(cl=cl, 1:1000,ffff2) 

MLE_b<-nlminb(c(0,0,0,0,0,0),objective=ffff2, control=list(eval.max=1000, iter.max=1500))
MLE_b

p_b<- 1-pchisq(2*(MLE_b$objective-MLE$objective),1)
p_b

ffff3<-function(par){
  a<-par[1]
  b<-par[2]
  re<-par[3]
  rm<-par[4]
  rp<-par[5]
  rrm<-par[6]  
  
  
  
  y<-rep(0,n)
  z<-rep(0,n)
  p<-rep(0,n)
  x<-rep(0,n)
  
  NN<- exp((-1)*d$t)*log(1+d$number_of_Cases)*(1+re*log(1+d$number_of_escalation)
                                                    +rm*log(1+d$number_of_single_visit_missed)
                                                    +rp*log(1+d$number_of_parts_used)
                                                    +rrm*log(1+d$number_of_response_missed))
  y<- NN
  #print(y)
  #z<- a+b* y
  
  for (j in 1:n) {
    z[j]<-a+b* y[j]
    p[j]<- 1/(1+exp((-1)*z[j]))
    x[j]<- d$status[j]
  }
  
  
  pp<-rep(0,n)
  for(k in 1:n) { if(x[k]==1) pp[k]<- p[k] else pp[k]<- 1-p[k]}
  logproductpp<- sum(log(pp))
  return (-logproductpp)
}
ans<-parSapply(cl=cl, 1:1000,ffff3) 

MLE_beta<-nlminb(c(0,0,0,0,0,0),objective=ffff3, control=list(eval.max=1000, iter.max=1500))
MLE_beta
p_beta<- 1-pchisq(2*(MLE_beta$objective-MLE$objective),1)
p_beta


  ffff4<-function(par){
    a<-par[1]
    b<-par[2]
    beta<-par[3]
   
    rm<-par[4]
    rp<-par[5]
    rrm<-par[6]  
    
    
    
    y<-rep(0,n)
    z<-rep(0,n)
    p<-rep(0,n)
    x<-rep(0,n)
    
    NN<- exp((-1)*beta*d$t)*log(1+d$number_of_Cases)*(1+(0)*log(1+d$number_of_escalation)
                                                      +rm*log(1+d$number_of_single_visit_missed)
                                                      +rp*log(1+d$number_of_parts_used)
                                                      +rrm*log(1+d$number_of_response_missed))
    y<- NN
    #print(y)
    #z<- a+b* y
    
    for (j in 1:n) {
      z[j]<-a+b* y[j]
      p[j]<- 1/(1+exp((-1)*z[j]))
      x[j]<- d$status[j]
    }
    
    
    pp<-rep(0,n)
    for(k in 1:n) { if(x[k]==1) pp[k]<- p[k] else pp[k]<- 1-p[k]}
    logproductpp<- sum(log(pp))
    return (-logproductpp)
  }
  ans<-parSapply(cl=cl, 1:1000,ffff4) 
  
  MLE_re<-nlminb(c(0,0,0,0,0,0),objective=ffff4, control=list(eval.max=1000, iter.max=1500))
  MLE_re
  p_re<- 1-pchisq(2*(MLE_re$objective-MLE$objective),1)
  p_re


ffff5<-function(par){
  a<-par[1]
  b<-par[2]
  beta<-par[3]
  re<-par[4]
  
  rp<-par[5]
  rrm<-par[6]  
  
  
  
  y<-rep(0,n)
  z<-rep(0,n)
  p<-rep(0,n)
  x<-rep(0,n)
  
  NN<- exp((-1)*beta*d$t)*log(1+d$number_of_Cases)*(1+re*log(1+d$number_of_escalation)
                                                    +0*log(1+d$number_of_single_visit_missed)
                                                    +rp*log(1+d$number_of_parts_used)
                                                    +rrm*log(1+d$number_of_response_missed))
  y<- NN
  #print(y)
  #z<- a+b* y
  
  for (j in 1:n) {
    z[j]<-a+b* y[j]
    p[j]<- 1/(1+exp((-1)*z[j]))
    x[j]<- d$status[j]
  }
  
  
  pp<-rep(0,n)
  for(k in 1:n) { if(x[k]==1) pp[k]<- p[k] else pp[k]<- 1-p[k]}
  logproductpp<- sum(log(pp))
  return (-logproductpp)
}
ans<-parSapply(cl=cl, 1:1000,ffff5) 

MLE_rm<-nlminb(c(0,0,0,0,0,0),objective=ffff5, control=list(eval.max=1000, iter.max=1500))
MLE_rm
p_rm<- 1-pchisq(2*(MLE_rm$objective-MLE$objective),1)
p_rm

ffff6<-function(par){
  a<-par[1]
  b<-par[2]
  beta<-par[3]
  re<-par[4]
  rm<-par[5]
  
  rrm<-par[6]  
  
  
  
  y<-rep(0,n)
  z<-rep(0,n)
  p<-rep(0,n)
  x<-rep(0,n)
  
  NN<- exp((-1)*beta*d$t)*log(1+d$number_of_Cases)*(1+re*log(1+d$number_of_escalation)
                                                    +rm*log(1+d$number_of_single_visit_missed)
                                                    +0*log(1+d$number_of_parts_used)
                                                    +rrm*log(1+d$number_of_response_missed))
  y<- NN
  #print(y)
  #z<- a+b* y
  
  for (j in 1:n) {
    z[j]<-a+b* y[j]
    p[j]<- 1/(1+exp((-1)*z[j]))
    x[j]<- d$status[j]
  }
  
  
  pp<-rep(0,n)
  for(k in 1:n) { if(x[k]==1) pp[k]<- p[k] else pp[k]<- 1-p[k]}
  logproductpp<- sum(log(pp))
  return (-logproductpp)
}
ans<-parSapply(cl=cl, 1:1000,ffff6) 

MLE_rp<-nlminb(c(0,0,0,0,0,0),objective=ffff6, control=list(eval.max=1000, iter.max=1500))
MLE_rp
p_rp<- 1-pchisq(2*(MLE_rp$objective-MLE$objective),1)
p_rp


ffff7<-function(par){
  a<-par[1]
  b<-par[2]
  beta<-par[3]
  re<-par[4]
  rm<-par[5]
  rp<-par[6]
   
  
  
  
  y<-rep(0,n)
  z<-rep(0,n)
  p<-rep(0,n)
  x<-rep(0,n)
  
  NN<- exp((-1)*beta*d$t)*log(1+d$number_of_Cases)*(1+re*log(1+d$number_of_escalation)
                                                    +rm*log(1+d$number_of_single_visit_missed)
                                                    +rp*log(1+d$number_of_parts_used)
                                                    +(0)*log(1+d$number_of_response_missed))
  y<- NN
  #print(y)
  #z<- a+b* y
  
  for (j in 1:n) {
    z[j]<-a+b* y[j]
    p[j]<- 1/(1+exp((-1)*z[j]))
    x[j]<- d$status[j]
  }
  
  
  pp<-rep(0,n)
  for(k in 1:n) { if(x[k]==1) pp[k]<- p[k] else pp[k]<- 1-p[k]}
  logproductpp<- sum(log(pp))
  return (-logproductpp)
}
ans<-parSapply(cl=cl, 1:1000,ffff7) 

MLE_rrm<-nlminb(c(0,0,0,0,0,0),objective=ffff7, control=list(eval.max=1000, iter.max=1500))
MLE_rrm
p_rrm<- 1-pchisq(2*(MLE_rrm$objective-MLE$objective),1)
p_rrm

MLE
MLE_a
MLE_b
MLE_beta
MLE_re
MLE_rm
MLE_rp
MLE_rrm

p_a
p_b
p_beta
p_re
p_rm
p_rp
p_rrm