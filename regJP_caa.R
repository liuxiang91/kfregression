# read data and assign variable names
data=read.csv(paste(getwd(),'/kfregression/forReg.csv',sep = ""),header=FALSE)

names(data)=c('MD','IOP','PSD','MDvel','IOPvel','PSDvel','MDacc','IOPacc','PSDacc','Baseline_MD','Baseline_IOP','Baseline_PSD','Age','Race','Sex','SphEq', 'CCT','AxLen','DiskHem','Prog')

# FIXED EFFECT MODEL
# fit logistic regression model ignoring clustering
# and perform stepwise selection
fullModel <- glm(Prog~MD+IOP+PSD+MDvel+IOPvel+PSDvel+MDacc+IOPacc+PSDacc+Baseline_MD+Baseline_IOP+Baseline_PSD+Age+
								 #Race+ # Race is constant in these data
								 Sex+SphEq+CCT+AxLen+DiskHem, data=data, family=binomial(link="logit"))
# Note the warning
# Warning message: glm.fit: fitted probabilities numerically 0 or 1 occurred 
# this is often a sign of over fitting (too many variables)
summary(fullModel)

model=step(fullModel, direction="both")
summary(model)
# note that the resulting model has acceleration terms without velocity terms
# this is difficult to interpret and/or justify.

#c=list();
#c[[1]]=coef(model)
#c[[2]]=rep(0,16+4)
#names(c[[2]])=c('(Intercept)','MD','IOP','PSD','MDvel','IOPvel','PSDvel','MDacc','IOPacc','PSDacc','Baseline_MD','Baseline_IOP','Baseline_PSD','Age','Race','Sex','SphEq',	'CCT',	'AxLen',	'DiskHem')
#out=tapply(unlist(c), names(unlist(c)), sum)
#out1=out[c('(Intercept)','MD','IOP','PSD','MDvel','IOPvel','PSDvel','MDacc','IOPacc','PSDacc','Baseline_MD','Baseline_IOP','Baseline_PSD','Age','Race','Sex','SphEq',	'CCT',	'AxLen',	'DiskHem')]
#write.csv(out1,file='regCoeff.csv')

# rewrite ending
# This allows the fullModel to be changed without having to edit vector of names 3 times
# no crypic use of tapply and unlist and sum
out <- rep(0, length(coef(fullModel))) # create output vector of 0s
names(out) <- names(coef(fullModel)) # add names
out[names(coef(model))] <- coef(model) # add values from reduced model

# output regression coefficients
#write.csv(out, file='regCoeff.csv')

# MIXED EFFECT MODEL
# investigate stepwise selection with glmm
# this is all slower than the fixed effect version above.

# install.packages("lme4") # run once if you've never used before
library("lme4")
# install.packages("LMERConvenienceFunctions") # run once if you've never used before
library("LMERConvenienceFunctions")

# add pretend IDs to data
# presumably you have this data elsewhere
data$ID <- rep(seq(625), each=2)[-1250]


mod <- glmer(Prog~MD+IOP+PSD+MDvel+IOPvel+PSDvel+MDacc+IOPacc+PSDacc+Baseline_MD+Baseline_IOP+Baseline_PSD+Age+
						 	#Race+
						 	Sex+SphEq+CCT+AxLen+DiskHem + (1|ID), data=data, family=binomial(link="logit"), nAGQ=5)
# convergence warning, as with fixed effect model above
summary(mod)

# same model with variables on similar scales
# mod <- glmer(Prog ~ MD + IOP + PSD +
# 						 	I(10*MDvel) + I(10*IOPvel) + I(10*PSDvel) +
# 						 	I(100*MDacc) + I(100*IOPacc) + I(100*PSDacc) +
# 						 	Baseline_MD + Baseline_IOP + Baseline_PSD +
# 						 	I(Age/1000) + #Race+
# 						 	Sex+SphEq+CCT+AxLen+DiskHem + (1|ID),
# 						 data=data, family=binomial(link="logit"), nAGQ=5)
# convergence warning, as above
# rescaling does not improve convergence in this case

# step(mod) # this does not work
#drop1(mod) # this could be used instead as part of a loop; it drops 1 variable at a time and reports the AIC for each

# An automated version I've never used before
system.time(mod2 <- bfFixefLMER_t.fnc(mod, method = "AIC", threshold = 2))
summary(mod2)


out2 <- rep(0, length(fixef(mod))) # create output vector of 0s
names(out2) <- names(fixef(mod)) # add names
out2[names(fixef(mod2))] <- fixef(mod2) # add values from reduced model

# output regression coefficients
#write.csv(out2, file='regCoeff2.csv')

# Compare two fits
# fulls
cbind(coef(fullModel), fixef(mod)) # coefficiencts nearly identical
# with actual IDs, there might be more difference between the coefficients (but not likely much).
# reduced
cbind(out, out2) # coefficiencts not too different
# with actual IDs, the coefficients might differ and their standard errors.
# That information might lead to consistently smaller models:
# Correlated data can decrease the effective sample size, which (all else equal) increases p-values
# and therefore smaller "stepwise-selected" models.

# probably not worth the added computation time?


# GENERALIZED ESTIMATING APPROACH
# install.packages("geepack")
library("geepack")

system.time(
	gmod <- geeglm(Prog~MD+IOP+PSD+MDvel+IOPvel+PSDvel+MDacc+IOPacc+PSDacc+Baseline_MD+Baseline_IOP+Baseline_PSD+Age+
								 	#Race+
								 	Sex+SphEq+CCT+AxLen+DiskHem, data=data, family=binomial(link="logit"), id=ID, corstr="independence")
)
#	gmod <- geeglm(Prog~MD+IOP+PSD+MDvel+IOPvel+PSDvel+MDacc+IOPacc+PSDacc+Baseline_MD+Baseline_IOP+Baseline_PSD+Age+
#								 	#Race+
#								 	Sex+SphEq+CCT+AxLen+DiskHem, data=data, family=binomial(link="logit"), id=ID, corstr="exchangeable"))
summary(gmod)

# compare 3 models
cbind(coef(fullModel), fixef(mod), coef(gmod)) # nearly identical
# fulls
cbind(coef(fullModel), fixef(mod), coef(gmod)) # nearly identical
# with actual IDs, there might be more difference between the coefficients (but not likely much).

# step(gmod) # nope
drop1(gmod) # nope (well. OK. I hacked it.)

# this is not well checked.  I just changed a few calls from AIC to QIC
drop1.geeglm <- function (object, scope, scale = 0, test = c("none", "Chisq"), 
    k = 2, trace = FALSE, ...) 
{
	require("MuMIn")
    tl <- attr(terms(object), "term.labels")
    if (missing(scope)) 
        scope <- drop.scope(object)
    else {
        if (!is.character(scope)) 
            scope <- attr(terms(update.formula(object, scope)), 
                "term.labels")
        if (!all(match(scope, tl, 0L) > 0L)) 
            stop("scope is not a subset of term labels")
    }
    ns <- length(scope)
    ans <- matrix(nrow = ns + 1L, ncol = 2L, dimnames = list(c("<none>", 
        scope), c("df", "QIC"))) # changed label
    #ans[1, ] <- extractAIC(object, scale, k = k, ...)
    ans[1, ] <- c(length(coef(object)), QIC(object))
    n0 <- nobs(object, use.fallback = TRUE)
    env <- environment(formula(object))
    for (i in seq_len(ns)) {
        tt <- scope[i]
        if (trace > 1) {
            cat("trying -", tt, "\n", sep = "")
            flush.console()
        }
        nfit <- update(object, as.formula(paste("~ . -", tt)), 
            evaluate = FALSE)
        nfit <- eval(nfit, envir = env)
        #ans[i + 1, ] <- extractAIC(nfit, scale, k = k, ...)
        ans[i + 1, ] <- c(length(coef(nfit)), QIC(nfit))
        nnew <- nobs(nfit, use.fallback = TRUE)
        if (all(is.finite(c(n0, nnew))) && nnew != n0) 
            stop("number of rows in use has changed: remove missing values?")
    }
    dfs <- ans[1L, 1L] - ans[, 1L]
    dfs[1L] <- NA
    aod <- data.frame(Df = dfs, QIC = ans[, 2])
    test <- match.arg(test)
    if (test == "Chisq") {
        dev <- ans[, 2L] - k * ans[, 1L]
        dev <- dev - dev[1L]
        dev[1L] <- NA
        nas <- !is.na(dev)
        P <- dev
        P[nas] <- safe_pchisq(dev[nas], dfs[nas], lower.tail = FALSE)
        aod[, c("LRT", "Pr(>Chi)")] <- list(dev, P)
    }
    head <- c("Single term deletions", "\nModel:", deparse(formula(object)), 
        if (scale > 0) paste("\nscale: ", format(scale), "\n"))
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
}


drop1(gmod) # nope (well. OK. I hacked it.)

# backward selection
gmod2 <- gmod
qicdf <- drop1(gmod2) 
while((rw <- which.min(qicdf$QIC)) > 1) {
	cat(sprintf("Dropping %s\n", row.names(qicdf)[rw]))
	gmod2 <- update(gmod2, as.formula(paste("~ . -", row.names(qicdf)[rw])))
	qicdf <- drop1(gmod2)
}


out3 <- rep(0, length(coef(gmod))) # create output vector of 0s
names(out3) <- names(coef(gmod)) # add names
out3[names(coef(gmod2))] <- coef(gmod2) # add values from reduced model


# output regression coefficients
#write.csv(out3, file='regCoeff3.csv')

# reduced
cbind(out, out2, out3) # coefficiencts not too different.  Some different selection.
# with actual IDs, the coefficients might differ and their standard errors.
# That information might lead to consistently smaller models:
# Correlated data can decrease the effective sample size, which (all else equal) increases p-values
# and therefore smaller "stepwise-selected" models.





# stop here



# install.packages("MuMIn") # run once if you've not used before
library("MuMIn")
op <- options(na.action = "na.fail")

# This does best subset selection (not stepwise) and is very time consuming
#
# Note the author's warning
# Users should keep in mind the hazards that a "thoughtless approach" of evaluating all possible models poses.
# Although this procedure is in certain cases useful and justified, it may result in selecting a spurious
# "best" model, due to the model selection bias.
#
#"Let the computer find out" is a poor strategy and usually reflects the fact that the researcher did not
# bother to think clearly about the problem of interest and its scientific setting (Burnham and Anderson, 2002).
#
# VERY LONG (~1 day)
#system.time(allgmods <- dredge(gmod, beta = "sd", rank = "QIC", evaluate=TRUE, trace=1))
gmod2a <- get.models(allgmods, 1)[[1]]
summary(gmod2a)

coef(gmod2a)

out4 <- rep(0, length(coef(gmoda))) # create output vector of 0s
names(out4) <- names(coef(gmoda)) # add names
out4[names(coef(gmod2a))] <- coef(gmod2a) # add values from reduced model


# output regression coefficients
#write.csv(out4, file='regCoeff4.csv')

cbind(out, out2, out3, out4)

options(op)
