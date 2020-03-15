eucl.distance <- function(pars, prob, prev, inc)
{
  prevalence1 <- function(x){
    num <- x[2]
    den <- sum(x[1:10])
    return(num/den)
  }
  
  incidence1 <- function(i, x, newCasesMean1){
    num <- newCasesMean1[i]
    den <- sum(x[i, c(1:10)])
    num/den
  }
  
  prob2 <- prob
  k <- 1
  for (i in 1:dim(prob)[1])
  {
    for (j in 2:dim(prob)[2])
    {
      if (prob[i, j] == 0 | prob[i, j] == 1 | is.nan(pars[k]))
      {
        prob2[i, j] <- prob[i, j]
      }else{
        prob2[i, j] <- pars[k]
        k <- k + 1
      }
    }
  }
  prob2 <- cbind(prob2[, 1], t(apply(prob2[, 2:dim(prob2)[2]], 1, function(x)(x/(sum(x))))))
  prob2 <- as.data.frame(prob2)
  prob2[, 1] <- prob[, 1]
  
  for (i in 1:dim(prob2)[1])
  {
    for (j in 2:dim(prob2)[2])
    {
      if (is.nan(prob2[i, j])) prob2[i, j] <- prob[i, j]
    }
  }
  res <- try(simCohort(probs1=prob2, Nsim = 5, figoSymProb = c(.11, .23, .66, .9), ### Probability to develop symptoms in FIGO states
                       vaccinePrice.md = 0, vaccinePrice.nmd = 0, vaccinePrice.i = 0, screenPrice.md = 0, screenPrice.nmd = 0, screenPrice.i = 0,
                       costCoefs.md = c(0, 0, 248.2, 1461.0, 1461.0, 5417.3, 12136.3, 22583.6, 33222.5, 0, 0, 0),
                       costCoefs.nmd = c(0, 0, 79.5, 189.6, 189.6, 214.0, 214.0, 214.0, 214.0, 0, 0, 0),
                       costCoefs.i = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                       vaccineCov = 0, utilityCoefs = c(1, 1, 0.987, 0.87, 0.87, 0.76, 0.67, 0.67, 0.67, 0.938, 0, 0), 
                       screenProbs = c(0, 0, 1, 1, 1, 0.9688, 0.9066, 0.7064, 0.3986, 0, 0, 0), ndoses = 0), silent = TRUE)
  if (class(res) == "try-error")
  {
    return(1e5)
  }
  tmp <- lapply(as.list(unique(res$age)), function(i, dat){
    aux <- res[res$age == i, ]
    apply(aux[, 1:(ncol(aux)-2)], 2, mean)
  }, dat = res)
  meanDat1 <- as.data.frame(t(as.matrix(as.data.frame(tmp))))
  rownames(meanDat1) <- NULL
  totPrev1 <- apply(meanDat1, 1, prevalence1)
  
  newCasesMean1 <- apply(attr(res, "newCases"), 2, mean)
  
  names(newCasesMean1) <- 1:length(newCasesMean1)
  
  totInc1 <- unlist(lapply(as.list(1:nrow(meanDat1)), incidence1,
                           x = meanDat1, newCasesMean1 = newCasesMean1))
  
  age  <- as.numeric(substr(prob[1, 1], 1, 2)):as.numeric(substr(prob[dim(prob)[1], 1], 4, 5))
  ages <- as.numeric(substr(prev[1, 1], 1, 2)):as.numeric(substr(prev[dim(prev)[1], 1], 4, 5))
  HNObsPrev <- approx(prev[, 2], n=length(ages))$y 
  
  tmp1 <- NULL
  tmp2 <- NULL
  ageGroupPrev1_pr <- rep(NA, length(unique(unique(prob[, 1]))))
  currentAgeGroup  <- 1
  i <- 1
  ageGroups <- unique(prob[, 1])
  for(year in age){
    if(year > as.numeric(substr(ageGroups[currentAgeGroup], 4, 5))){
      ageGroupPrev1_pr[currentAgeGroup] <- sum(tmp1)/sum(tmp2)
      currentAgeGroup <- currentAgeGroup + 1
      tmp1 <- NULL
      tmp2 <- NULL
    }
    tmp1 <- c(tmp1, meanDat1[i, 2])
    tmp2 <- c(tmp2, sum(meanDat1[i, 1:(dim(meanDat1)[2]-2)]))
    i <- i + 1
  }
  ageGroupPrev1_pr[length(unique(prob[, 1]))] <- sum(tmp1)/sum(tmp2)
  
  ageGroupNames <- unique(prob[, 1])
  
  tmp <- NULL
  tmp2 <- NULL
  ageGroupInc1 <- rep(NA, length(unique(unique(prob[, 1]))))
  currentAgeGroup <- 1
  i <- 1
  den <- sum(meanDat1[1, c(1:10)])
  for(year in age){
    if(year > as.numeric(substr(ageGroups[currentAgeGroup], 4, 5))){
      ageGroupInc1[currentAgeGroup] <- sum(tmp)/sum(tmp2)
      currentAgeGroup <- currentAgeGroup + 1
      tmp <- NULL
      tmp2 <- NULL
    }
    tmp <- c(tmp, newCasesMean1[i])
    tmp2 <- c(tmp2, sum(meanDat1[i, 1:(dim(meanDat1)[2]-2)]))
    i <- i + 1
  }
  ageGroupInc1[length(unique(prob[, 1]))] <- sum(tmp)/sum(tmp2)
  ageGroupInc1 <- ageGroupInc1*100000
  
  f <- which(ageGroups==as.character(prev[1, 1]))
  l <- which(ageGroups==as.character(prev[dim(prev)[1], 1]))
  diff.matrix <- cbind(prev, ageGroupPrev1_pr[f:l], inc, ageGroupInc1[f:l])
  desv.prev <- ifelse(diff.matrix[, 2]!=0, abs(diff.matrix[, 2] - diff.matrix[, 3])/diff.matrix[, 2], 0)
  desv.inc  <- ifelse(diff.matrix[, 5]!=0, abs(diff.matrix[, 5] - diff.matrix[, 6])/diff.matrix[, 5], 0)
  eucl.dist <- desv.prev + desv.inc
  return(sum(eucl.dist, na.rm=T))
}
