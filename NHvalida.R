NHvalida <- function(probs, prev, inc, mort=NULL, N=500, keep=20, p.change)
{
  withProgress(message="", value=0, {
  p.change <- p.change/100
  if (N < keep) stop("Number of matrices to keep should be lower than the number of simulations")
  if (keep < 2) stop("The minimum is to perform 2 simulations")
  if (!all(prev[, 1] == inc[, 1])) stop("Age groups in reference prevalences/incidences should be equal")
  
  probs.def   <- vector("list", keep)
  prevalences <- vector("list", keep)
  incidences  <- vector("list", keep)
  mortality   <- vector("list", keep)
  dists       <- vector()
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
  
  n <- 1
  while(n <= N)
  {  
    incProgress(1/N, detail = paste("Simulation: ", n))
    probs.prova <- matrix(nrow=nrow(probs), ncol=ncol(probs))
    probs.prova[, 1] <- probs[, 1]
    for (i in 1:nrow(probs))
    {
      for (j in 2:ncol(probs))
      {
        if (probs[i, j] != 0 & probs[i, j] != 1)
        {
          probs.prova[i, j] <- sample(rpert(5000, min=max(as.numeric(probs[i, j])*(1-p.change), 0), mode=as.numeric(probs[i, j]),
                                            max=min(c(as.numeric(probs[i, j])*(1+p.change), 1))), 1)
        }else{
          probs.prova[i, j] <- probs[i, j]
        }
      }
    }
    
    ### Standardize all rows to sum 1
    probs.prova <- cbind(probs.prova[, 1], t(apply(probs.prova[, 2:dim(probs.prova)[2]], 1, function(x)(x/(sum(x))))))
    probs.prova <- as.data.frame(probs.prova)
    probs.prova[, 1] <- probs[, 1]
    res <- simCohort(probs1=probs.prova, Nsim = 5,
                     figoSymProb = c(.11, .23, .66, .9), ### Probability to develop symptoms in FIGO states
                     vaccinePrice.md = 0, vaccinePrice.nmd = 0, vaccinePrice.i = 0, screenPrice.md = 0, screenPrice.nmd = 0, screenPrice.i = 0,
                     costCoefs.md = c(0, 0, 248.2, 1461.0, 1461.0, 5417.3, 12136.3, 22583.6, 33222.5, 0, 0, 0),
                     costCoefs.nmd = c(0, 0, 79.5, 189.6, 189.6, 214.0, 214.0, 214.0, 214.0, 0, 0, 0),
                     costCoefs.i = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                     vaccineCov = 0,
                     utilityCoefs = c(1, 1, 0.987, 0.87, 0.87, 0.76, 0.67, 0.67, 0.67, 0.938, 0, 0), 
                     screenProbs = c(0, 0, 1, 1, 1, 0.9688, 0.9066, 0.7064, 0.3986, 0, 0, 0), ndoses = 0)
    
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
    
    CCdeathsMean1 <- apply(attr(res, "CCdeaths"), 2, mean)
    
    age  <- as.numeric(substr(probs[1, 1], 1, 2)):as.numeric(substr(probs[dim(probs)[1], 1], 4, 5))
    ages <- as.numeric(substr(prev[1, 1], 1, 2)):as.numeric(substr(prev[dim(prev)[1], 1], 4, 5))
    HNObsPrev <- approx(prev[, 2], n=length(ages))$y 
    
    tmp1 <- NULL
    tmp2 <- NULL
    ageGroupPrev1_pr <- rep(NA, length(unique(probs[, 1])))
    
    currentAgeGroup <- 1
    i <- 1
    ageGroups <- unique(probs[, 1])
    veamos<-as.numeric(substr(ageGroups, 4, 5))
    
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
    ageGroupPrev1_pr[length(unique(probs[, 1]))] <- sum(tmp1)/sum(tmp2)
    ageGroupNames <- unique(probs[, 1])
    
    tmp <- NULL
    tmp2 <- NULL
    ageGroupInc1 <- rep(NA, length(unique(probs[, 1])))
    currentAgeGroup <- 1
    i <- 1
    den <- sum(meanDat1[1, c(1:5)])
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
    ageGroupInc1[length(unique(probs[, 1]))] <- sum(tmp)/sum(tmp2)
    ageGroupInc1 <- ageGroupInc1*100000
    
    if (!is.null(mort))
    {
      incCCdeath1 <- rep(NA, length(unique(probs[, 1])))
      offset <- as.numeric(substr(probs[1, 1], 1, 2))-1
      for(i in 1:length(ageGroups)){
        tmp <- (as.numeric(substr(ageGroups[i], 1, 2))-offset):(as.numeric(substr(ageGroups[i], 4, 5))-offset)
        num <- mean(CCdeathsMean1[tmp])
        surv <- meanDat1[(as.numeric(substr(ageGroups[i], 1, 2))-offset):(as.numeric(substr(ageGroups[i], 4, 5))-offset), 1:10]
        den <- mean(apply(surv, 1, sum))
        incCCdeath1[i] <- num/den
      }
      incCCdeath1<-incCCdeath1*100000
    }
    
    f <- which(ageGroups==as.character(prev[1, 1]))
    l <- which(ageGroups==as.character(prev[dim(prev)[1], 1]))
    diff.matrix <- cbind(prev, ageGroupPrev1_pr[f:l], inc, ageGroupInc1[f:l])
    eucl.dist <- vapply(1:dim(diff.matrix)[1], function(i){desv.prev <- ifelse(diff.matrix[i, 2]!=0, abs(as.numeric(diff.matrix[i, 2]) - as.numeric(diff.matrix[i, 3]))/as.numeric(diff.matrix[i, 2]), 0)
                           desv.inc  <- ifelse(diff.matrix[i, 5]!=0, abs(as.numeric(diff.matrix[i, 5]) - as.numeric(diff.matrix[i, 6]))/as.numeric(diff.matrix[i, 5]), 0)
    return(desv.prev + desv.inc)}, FUN.VALUE = 1)
    if (n <= keep)
    {
      if(is.null(probs.def[[n]]))
      {
        probs.def[[n]]   <- probs.prova
        dists[n]         <- sum(eucl.dist)
        prevalences[[n]] <- ageGroupPrev1_pr
        incidences[[n]]  <- ageGroupInc1
        if (!is.null(mort)) mortality[[n]] <- incCCdeath1
      }
    }else{
      if (sum(eucl.dist) < max(dists, na.rm=T))
      {
        probs.def[[which(dists==max(dists, na.rm=T))[1]]] <- probs.prova
        prevalences[[which(dists==max(dists, na.rm=T))[1]]] <- ageGroupPrev1_pr
        incidences[[which(dists==max(dists, na.rm=T))[1]]] <- ageGroupInc1
        if (!is.null(mort)) mortality[[which(dists==max(dists, na.rm=T))[1]]] <- incCCdeath1
        dists[which(dists==max(dists, na.rm=T))[1]] <- sum(eucl.dist)
      }
    }
    n <- n + 1
  }
  })
  #### plot the graph with prevalences from selected matrices vs real prevalence
  if (!is.null(mort)) par(las = 2, mfrow = c(3, 1))
  if (is.null(mort))  par(las = 2, mfrow = c(2, 1))
  maxim <- max(sapply(prevalences, max), prev[, 2])
  plot(1:length(prevalences[[1]]), prevalences[[1]], pch = 20, axes = FALSE,
       xlab = "", ylab = "Prevalence", main = "HPV prevalence per age group",
       ylim = c(0, maxim+0.05), col="grey")
  lines(1:length(prevalences[[1]]), prevalences[[1]], col="grey", lwd = 2)
  for (i in 2:length(prevalences)) ### length(prevalences) should be equal to 'keep' parameter
  {
    lines(1:length(prevalences[[i]]), prevalences[[i]], pch=20, type="p", col="grey")
    lines(1:length(prevalences[[i]]), prevalences[[i]], lwd=2, col="grey")
  }
  lines(f:l, prev[, 2], col = "black", type = "p", pch = 20)
  lines(f:l, prev[, 2], col = "black", lwd = 2)
  axis(2)
  axis(1, labels = ageGroupNames, at = 1:length(ageGroupPrev1_pr))
  legend("topright", c("Model simulations", "Observed prevalence"),
         pch = 20, col = c("grey", "black"), lwd = 2, bty = "n")
  maxim <- max(sapply(incidences, max), inc[, 2])
  plot(1:length(incidences[[1]]), incidences[[1]], pch = 20, axes = FALSE,
       xlab = "", ylab = "Incidence rate (x 100,000)", main = "CC incidence per age group",
       ylim = c(0, maxim+0.05), col="grey")
  lines(1:length(incidences[[1]]), incidences[[1]], lwd = 2, col="grey")
  for (i in 2:length(incidences)) ### length(incidences) should be equal to 'keep' parameter
  {
    lines(1:length(incidences[[i]]), incidences[[i]], pch=20, type="p", col="grey")
    lines(1:length(incidences[[i]]), incidences[[i]], lwd=2, col="grey")
  }
  lines(f:l, inc[, 2], col = "black", type = "p", pch = 20)
  lines(f:l, inc[, 2], col = "black", lwd = 2)
  axis(2)
  axis(1, labels = ageGroupNames, at = 1:length(ageGroupInc1))
  legend("topleft", c("Model simulations", "Observed incidence"),
         pch = 20, col = c("grey", "black"), lwd = 2, bty = "n")
  if (!is.null(mort))
  {
    maxim <- max(sapply(mortality, max), mort[, 2])
    plot(1:length(mortality[[1]]), mortality[[1]], pch = 20, axes = FALSE,
         xlab = "", ylab = "Mortality rate (x 100,000)", main = "CC mortality per age group",
         ylim = c(0, maxim+0.05), col="grey")
    lines(1:length(mortality[[1]]), mortality[[1]], col="grey", lwd = 2)
    for (i in 2:length(mortality)) ### length(mortality) should be equal to 'keep' parameter
    {
      lines(1:length(mortality[[i]]), mortality[[i]], pch=20, type="p", col="grey")
      lines(1:length(mortality[[i]]), mortality[[i]], lwd=2, col="grey")
    }
    lines(f:l, mort[, 2], col = "black", type = "p", pch = 20)
    lines(f:l, mort[, 2], col = "black", lwd = 2)
    axis(2)
    axis(1, labels = ageGroupNames, at = 1:length(incCCdeath1))
    legend("topleft", c("Model simulations", "Observed mortality"),
           pch = 20, col = c("grey", "black"), lwd = 2, bty = "n")
  }
  return(probs.def)
}