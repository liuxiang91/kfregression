
data=read.csv(paste(getwd(),'/kfregression/forReg.csv',sep = ""),header=FALSE)

names(data)=c('MD','IOP','PSD','MDvel','IOPvel','PSDvel','MDacc','IOPacc','PSDacc','Baseline_MD','Baseline_IOP','Baseline_PSD','Age','Race','Sex','SphEq', 'CCT','AxLen','DiskHem','Prog')
fullModel <- glm(Prog~0+MD+IOP+PSD+MDvel+IOPvel+PSDvel+MDacc+IOPacc+PSDacc+Baseline_MD+Baseline_IOP+Baseline_PSD+Age+Race+Sex, data=data, family=binomial(link="logit"))
summary(fullModel)
model=step(fullModel,direction="both")
summary(model)
c=list();
c[[1]]=coef(model)
c[[2]]=rep(0,16)
names(c[[2]])=c('(Intercept)','MD','IOP','PSD','MDvel','IOPvel','PSDvel','MDacc','IOPacc','PSDacc','Baseline_MD','Baseline_IOP','Baseline_PSD','Age','Race','Sex')
out=tapply(unlist(c), names(unlist(c)), sum)
out1=out[c('(Intercept)','MD','IOP','PSD','MDvel','IOPvel','PSDvel','MDacc','IOPacc','PSDacc','Baseline_MD','Baseline_IOP','Baseline_PSD','Age','Race','Sex')]
write.csv(out1,file='regCoeff.csv')

