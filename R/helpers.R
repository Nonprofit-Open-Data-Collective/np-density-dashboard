
quant.cut<-function(var,x,df){
xvec<-vector()
for (i in 1:x){
  xvec[i]<-i/x
}

qs<-c(min(df[[var]],na.rm=T), quantile(df[[var]],xvec,na.rm=T))

df[['new']]=x+1 #initialize variable


for (i in 1:(x)){
  df[['new']]<-ifelse(df[[var]]<qs[i+1] & df[[var]]>=qs[i],
                         c(1:length(qs))[i],
                         ifelse(df[[var]]==qs[qs==max(qs)],x,df[['new']]))
}

return(df[['new']])
}


