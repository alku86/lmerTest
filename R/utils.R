getY <- function(model)
{
    return(getME(model, "y"))
}
getX <- function(model)
{
    return(getME(model, "X"))
}
getZt <- function(model)
{
    Ztlist <- getME(model, "Ztlist")
    return(do.call(rBind,Ztlist))
}
getST <- function(model)
{
    return(getME(model, "ST"))
}


##########################################################################
# Create rho environmental variable of mixed model ####################### 
##########################################################################
rhoInit <- function(rho, model, change.contr = TRUE)
{
  ## creating rho environment that will contain info about model
  ## create an empty environment
  if(change.contr){
    resSAS <- changeContrastsToSAS(model)
    rho$model <- resSAS$model
    rho$contr <- resSAS$l.lmerTest.private.contrast
  }
  else
    rho$model <- model
  rho$data <- model.frame(rho$model) 
  rho$y <- getY(rho$model) #model@y                   
  rho$X <- getX(rho$model) #model@X
  chol(rho$XtX <- crossprod(rho$X))  # check for full column rank
  
  rho$REML <-  getREML(rho$model)
  if(class(rho$model) == "lmerMod") 
    rho$s <- summary(rho$model)
  
  rho$fixEffs <- fixef(rho$model)
  rho$sigma <- sigma(rho$model)
  rho$vlist <- sapply(rho$model@cnms, length)
  
  
  ## get the optima
  #pp <- model@pp$copy()  
  #opt <- Cv_to_Vv(pp$theta, n = vlist, s = rho$sigma)
  #rho$opt <- opt
  rho$thopt <- getME(rho$model, "theta")
  #rho$param <- as.data.frame(VarCorr(model))[, "sdcor"]
  #rho$vars <- Cv_to_Vv(rho$thopt, n = rho$vlist, 
  #             s = rho$sigma)
  return(rho)  
}

       
############################################################################
#get formula for model 
############################################################################
getFormula <- function(model, withRand=TRUE)
{
  fmodel <- formula(model)  
 
  if(withRand)
    return(fmodel)
  
  fm <- paste(fmodel)
  fmodel.red <- paste(fm[2],fm[1], 
                        paste(fm[3], 
                              paste(unlist(lapply(names(.fixedrand(model)$randeffs),
                                                  function(x) paste("(",x, ")"))), 
                                    collapse = " - "), 
                              sep = "-"))
  return(update(fmodel, fmodel.red))
}



emptyAnovaLsmeansTAB <- function()
{
  result <- NULL
  anova.table <-  matrix(ncol=5,nrow=0)
  colnames(anova.table) <- c("Estimate", "Standard Error", "DF", "F-value", 
                             "p-value")
  anova.table <- as.data.frame(anova.table)
  result$TAB.fixed <- anova.table
  lsmeans.summ <-  matrix(ncol=7,nrow=0)
  colnames(lsmeans.summ) <- c("Estimate", "Standard Error", "DF", "t-value", 
                              "Lower CI", "Upper CI", "p-value")
  lsmeans.summ <- as.data.frame(lsmeans.summ)
  result$TAB.lsmeans <- lsmeans.summ
  return(result)
}


## save results for fixed effects for model with only fixed effects
saveResultsFixModel <- function(result, model, type = 3)
{
  if(type==3)
    result$anova.table <- drop1(model, test="F")
  else{
    result$anova.table <- anova(model)
  }
  result$model <- model
  lsmeans.summ <-  matrix(ncol=7,nrow=0)
  colnames(lsmeans.summ) <- c("Estimate","Standard Error", "DF", "t-value", 
                              "Lower CI", "Upper CI", "p-value")
  result$lsmeans.table <- lsmeans.summ
  result$diffs.lsmeans.table <- lsmeans.summ
  return(result)
}



getREML <- function(model)
{
  if(inherits(model,"merMod"))
    return(getME(model, "is_REML"))

}

#update model
updateModel <- function(model, mf.final, reml.lmerTest.private, 
                        l.lmerTest.private.contrast, 
                        devFunOnly.lmerTest.private = FALSE)
{
  if(!mf.final == as.formula(paste(".~.")))
  {
    inds <-  names(l.lmerTest.private.contrast) %in% attr(terms(as.formula(mf.final)), 
                                                          "term.labels")
     #update contrast l.lmerTest.private.contrast
    l.lmerTest.private.contrast <- l.lmerTest.private.contrast[inds]
  }
 
 #data.update.lmerTest.private <- model.frame(model)  
 nfit <- update(object=model, formula.=mf.final, REML=reml.lmerTest.private ,
                contrasts=l.lmerTest.private.contrast, 
                devFunOnly = devFunOnly.lmerTest.private, 
 #               data = data.update.lmerTest.private,
                evaluate=FALSE)
 env <- environment(formula(model))
 assign("l.lmerTest.private.contrast", l.lmerTest.private.contrast, envir=env)
 assign("reml.lmerTest.private", reml.lmerTest.private, envir=env)
 assign("devFunOnly.lmerTest.private", devFunOnly.lmerTest.private, envir=env)
 #assign("data.update.lmerTest.private", data.update.lmerTest.private, envir=env)
 nfit <- eval(nfit, envir = env) 
 return(nfit)   
}

checkNameDDF <- function(ddf){
  ddfs <- c("Satterthwaite", "Kenward-Roger")
  ind.ddf <- pmatch(tolower(ddf), tolower(ddfs))
  if(is.na(ind.ddf))  
    stop('Parameter ddf is wrongly specified')  
  else
    ddf <- ddfs[ind.ddf]
  ddf
}


