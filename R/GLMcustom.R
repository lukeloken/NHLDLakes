

#### Functions to pull out and plot glm objects ####
glm2lm<-function(glm){
  df<-glm[[1]]$model
  equation<-paste0(names(df)[1], '~', paste(names(df)[2:ncol(df)], collapse='+'))
  lm<-lm(equation, data=df)
  return(lm)
}


glmplot<-function (glm, ...){
  y_obs<-glm[[1]]$model[,1]
  y_pred<-predict(glm$BestModel, glm[[1]]$model)
  plotlim<-range(c(y_obs, y_pred), na.rm=T)
  plot(y_obs, y_pred, xlim=plotlim, ylim=plotlim,...)
}

extractp<-function(lm) {
  rSquared <- summary(lm)$r.squared
  pVal <- anova(lm)$'Pr(>F)'[1]
  v <- c(pVal, rSquared)
  names(v) <- c('pVal', 'rSquared')
  return(v)
}
