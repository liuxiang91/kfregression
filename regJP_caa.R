# read data and assign variable names
data=read.csv('forReg.csv',header=FALSE)
names(data)=c('ID','MD','IOP','PSD','MDvel','IOPvel','PSDvel','MDacc','IOPacc','PSDacc','Baseline_MD','Baseline_IOP','Baseline_PSD','Age','Race','Sex','SphEq', 'CCT','AxLen','DiskHem','Prog')

# GENERALIZED ESTIMATING APPROACH
# install.packages("geepack")
library("geepack")
library("MuMIn")

system.time(
    gmod <- geeglm(Prog~MD+IOP+PSD+MDvel+IOPvel+PSDvel+MDacc+IOPacc+PSDacc+Baseline_MD+Baseline_IOP+Baseline_PSD+Age+
                                    #Race+
                                    Sex+SphEq+CCT+AxLen+DiskHem, data=data, family=binomial(link="logit"), id=ID, corstr="independence")
)
#   gmod <- geeglm(Prog~MD+IOP+PSD+MDvel+IOPvel+PSDvel+MDacc+IOPacc+PSDacc+Baseline_MD+Baseline_IOP+Baseline_PSD+Age+
#                                   #Race+
#                                   Sex+SphEq+CCT+AxLen+DiskHem, data=data, family=binomial(link="logit"), id=ID, corstr="exchangeable"))
summary(gmod)


# step(gmod) # nope
#drop1(gmod) # nope (well. OK. I hacked it.)

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


#drop1(gmod) # nope (well. OK. I hacked it.)

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
write.csv(out3, file='regCoeffJP.csv')

