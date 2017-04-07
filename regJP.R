data=read.csv(paste(getwd(),'/kfregression/forReg.csv',sep = ""),header=FALSE)
names(data)=c('ID','MD','IOP','PSD','MDvel','IOPvel','PSDvel','MDacc','IOPacc','PSDacc','Baseline_MD','Baseline_IOP','Baseline_PSD','Age','Race','Sex','SphEq', 'CCT','AxLen','DiskHem','Prog')
fullModel <- glm(Prog~MD+IOP+PSD+MDvel+IOPvel+PSDvel+MDacc+IOPacc+PSDacc+Baseline_MD+Baseline_IOP+Baseline_PSD+Age+Race+Sex+SphEq+CCT+AxLen+DiskHem, data=data, family=binomial(link="logit"))
summary(fullModel)
model=step(fullModel,direction="both")
summary(model)
c=list();
c[[1]]=coef(model)
c[[2]]=rep(0,16+4)
names(c[[2]])=c('(Intercept)','MD','IOP','PSD','MDvel','IOPvel','PSDvel','MDacc','IOPacc','PSDacc','Baseline_MD','Baseline_IOP','Baseline_PSD','Age','Race','Sex','SphEq',	'CCT',	'AxLen',	'DiskHem')
out=tapply(unlist(c), names(unlist(c)), sum)
out1=out[c('(Intercept)','MD','IOP','PSD','MDvel','IOPvel','PSDvel','MDacc','IOPacc','PSDacc','Baseline_MD','Baseline_IOP','Baseline_PSD','Age','Race','Sex','SphEq',	'CCT',	'AxLen',	'DiskHem')]
write.csv(out1, file=paste(getwd(),'/kfregression/regCoeff.csv',sep = ""))

