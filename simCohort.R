simCohort <- function(probs1, probs2 = NULL, probs3 = NULL, stopYear = 85, stepTime = 1,
                      M = 1e5, Nsim = 500, propDiffStrain = 0.70, propNoHPV = 0.0,
                      screening = FALSE, screenCoverage = NULL, screenSensi = NULL, screenProbs = NULL,### vaccinePrice is cost per dose, number of doses in ndoses
                      figoSymProb, vaccinePrice.md, vaccinePrice.nmd, vaccinePrice.i, screenPrice.md, screenPrice.nmd, screenPrice.i, 
                      vaccineCov, costCoefs.md, costCoefs.nmd, ### Direct medical; Direct non medical 
                      costCoefs.i, utilityCoefs, screenPeriod = 3, seed = 1234, dnaScreening = FALSE,  ### Indirect costs
                      dnaScSensi = NULL, dnaScCost.md = NULL, dnaScCost.nmd = NULL, dnaScCost.i = NULL, papScSensi = NULL, dnaScAgeGroups = NULL, 
                      newScreenPrice.md, newScreenPrice.nmd, newScreenPrice.i, newScreenCoverage, ScreenType = 0, OpScreen = NULL, ndoses=0, 
                      via = FALSE, via.Sensi = NULL, viaCoverage = NULL, viaProbs = NULL, viaPeriod = 0, via.md=0, via.nmd=0, via.i=0) ### VIA 
  ### ScreenType = 0 -> No screening
  ### ScreenType = 1 -> Organized screening
  ### ScreenType = 2 -> Opportunistic screening, needs file with proportions (OpScreen)
{
  ll <- as.list(match.call())[-1]     ## 
  myfor <- formals(simCohort)         ## formals with default arguments
  for (v in names(myfor)){
    if (!(v %in% names(ll)))
      ll <- append(ll, myfor[v])      ## if arg is missing I add it
  }
  if(!is.null(seed)) 
  {
    set.seed(seed)
  }else{
    set.seed(1234)
  }
  if (via == TRUE & is.null(via.Sensi)) stop("If VIA scenario is selected, VIA sensitivity should be provided")
  if ((vaccinePrice.md > 0 | vaccinePrice.nmd > 0 | vaccinePrice.i > 0) & ndoses == 0) stop("Number of vaccine doses should be at least 1")
  if (vaccinePrice.md == 0 & vaccinePrice.nmd == 0 & vaccinePrice.i == 0 & ndoses > 0) stop("Vaccine price per dose should be more than 0")
  if (screening == FALSE & ScreenType != 0) stop("If no screening, 'ScreenType' should be 0")
  if (screening == TRUE  & ScreenType == 0) stop("If screening, 'ScreenType' should be 1 (organized) or 2 (opportunistic)")
  if (ScreenType == 2)
  {
    if (is.null(OpScreen)) stop("File with oportunistic screening details needed")
    if (sum(OpScreen[, 2]) != 100) stop("Wrong proportions in oportunistic screening details file")
  }
  totNumSteps <-(stopYear-as.numeric(substr(probs1[, 1], 1, 2))[1])/stepTime
  ageGroups <- unique(data.frame(Start=as.numeric(substr(probs1[, 1], 1, 2)), End=as.numeric(substr(probs1[, 1], 4, 5))))
  probs1 <- split(probs1[, 2:dim(probs1)[2]], probs1[, 1])
  if (!is.null(probs2)) probs2 <- split(probs2[, 2:dim(probs2)[2]], probs2[, 1])
  if (!is.null(probs3)) probs3 <- split(probs3[, 2:dim(probs3)[2]], probs3[, 1])
  K <- length(probs1)    ### Age groups
  P <- ncol(probs1[[1]]) ### Health states
  res <- list()
  func <- function(sim){
    if(!is.null(probs2)){
      if(is.null(probs3))
        stop("If 'probs2' is provided 'probs3' must also be provided")
      propNormal <- 1 - propDiffStrain - propNoHPV
      probs <- lapply(1:length(probs1), function(i, probs1, probs2, probs3, prop1, prop2, prop3)
        prop1*probs1[[i]] + prop2*probs2[[i]] + prop3*probs3[[i]],
        probs1 = probs1, probs2 = probs2, probs3 = probs3,
        prop1 = propNormal, prop2 = propDiffStrain,
        prop3 = propNoHPV)
    }
    else
      probs <- probs1
    cumProbs <- lapply(probs, function(x)
      t(apply(x, 1, cumsum)))
    hpvProbK <- lapply(probs2, function(x) unlist(x[1, ]))
    hpvProbKCum <- lapply(hpvProbK, cumsum)
    if(screening)
      if(is.null(screenCoverage) || is.null(screenSensi) || is.null(screenProbs))
        stop("If screening scenario is selected 'screenCoverage', 'screenSensi' and 'screenProbs' must be provided")
    if(dnaScreening){
      if(!screening)
        stop("'dnaScreening = TRUE' is only available when 'screening = TRUE'")
      if(is.null(dnaScSensi)||is.null(dnaScCost.md)||is.null(dnaScCost.nmd)||is.null(dnaScCost.i)||is.null(papScSensi)||
         is.null(dnaScAgeGroups))
        stop("'dnaScSensi', 'dnaScCost.md', 'dnaScCost.nmd', 'dnaScCost.i', 'papScSensi' and 'dnaScAgeGroups' must be provided when 'dnaScreening = TRUE'")
    }
    if(via)
    {
      if (is.null(via.md) || is.null(via.nmd) || is.null(via.i)) stop("'via.md', 'via.nmd' and 'via.i' must be provided when 'via = TRUE'")
    }
    stageNi <- rep(0, P)
    names(stageNi) <- names(probs[[1]])
    stageN <- rep(list(stageNi), totNumSteps)
    #stageN[[1]][1] <- M # Assign all individuals to the 'Well' stage initially
    attr(stageN[[1]], "newCases")         <- 0 # No new cases in the first step!
    attr(stageN[[1]], "newCin1Cases")     <- 0
    attr(stageN[[1]], "newCin2Cases")     <- 0
    attr(stageN[[1]], "newCin3Cases")     <- 0 
    attr(stageN[[1]], "newSurvivalCases") <- 0
    attr(stageN[[1]], "CCdeaths")         <- 0
    attr(stageN[[1]], "Otherdeaths")      <- 0
    attr(stageN[[1]], "utility")          <- M
    attr(stageN[[1]], "probs_k")          <- rep(0, 9)
    attr(stageN[[1]], "probs_u")          <- rep(0, 9)
    attr(stageN[[1]], "nCito")            <- 0
    attr(stageN[[1]], "nVPH")             <- 0
    stageN[[1]][1]                        <- attr(stageN[[1]], "probs_u")[1] <- M
    if(!is.null(probs2) && !is.null(probs3)) # Assign to first step the vaccine cost in vaccination scenario
    {
      attr(stageN[[1]], "md_cost")  <- vaccineCov*vaccinePrice.md*M*ndoses
      attr(stageN[[1]], "nmd_cost") <- vaccineCov*vaccinePrice.nmd*M*ndoses
      attr(stageN[[1]], "i_cost")   <- vaccineCov*vaccinePrice.i*M*ndoses
    }else{
      attr(stageN[[1]], "md_cost")  <- 0
      attr(stageN[[1]], "nmd_cost") <- 0
      attr(stageN[[1]], "i_cost")   <- 0
    }
    currentAge <- ageGroups[1,1]
    currentAgeGroup <- 1 # Age group
    screenCount <- 1
    viaCount    <- 1
    for(i in 1:(totNumSteps-1)){ # Step loop
      if(currentAge > ageGroups$End[currentAgeGroup])
        currentAgeGroup <- currentAgeGroup + 1 # Change the age group if pertinent
      J <- which(stageN[[i]] > 0)
      stageN[[i+1]] <- stageN[[i]] # Initialize next step
      attr(stageN[[i+1]], "nCito") <- 0
      attr(stageN[[i+1]], "nVPH")  <- 0
      attr(stageN[[i+1]], "probs_k") <- attr(stageN[[i]], "probs_k")
      attr(stageN[[i+1]], "probs_u") <- attr(stageN[[i]], "probs_u")
      newSurvivalCases <- 0
      cost.md  <- 0
      cost.nmd <- 0
      cost.i   <- 0
      for(j in J){ # Stage loop
        if(j%in%1:9){ # CIN or FIGO
          nU <- attr(stageN[[i]], "probs_u")[j]
          nK <- attr(stageN[[i]], "probs_k")[j]
          pU <- runif(nU)
          pK <- runif(nK)
          qU <- findInterval(pU, unlist(cumProbs[[currentAgeGroup]][j,])) + 1
          qK <- findInterval(pK,
                             unlist(cumProbs[[currentAgeGroup]][j,])) + 1
          outU <- sum(qU != j)
          outK <- sum(qK != j)
          attr(stageN[[i+1]], "probs_u")[j] <-
            attr(stageN[[i+1]], "probs_u")[j] - outU
          attr(stageN[[i+1]], "probs_k")[j] <-
            attr(stageN[[i+1]], "probs_k")[j] - outK
          if(j == 2)
            attr(stageN[[i+1]], "newCin1Cases") <-
            sum(qU == 3) + sum(qK == 3)
          if(j == 3)
            attr(stageN[[i+1]], "newCin2Cases") <-
            sum(qU == 4) + sum(qK == 4)
          if(j == 4)
            attr(stageN[[i+1]], "newCin3Cases") <-
            sum(qU == 5) + sum(qK == 5)
          if(j == 5) # Store the number of subjects arriving to FIGO.I from CIN3 (new-cases)
            attr(stageN[[i+1]], "newCases") <- sum(qU == 6) + sum(qK == 6)
          for(l in union(unique(qU), unique(qK))){
            if(l != j){
              if(l%in%10:12)
                stageN[[i+1]][l] <- stageN[[i+1]][l] + sum(qU==l) + sum(qK==l)
              else{
                attr(stageN[[i+1]], "probs_u")[l] <- 
                  attr(stageN[[i+1]], "probs_u")[l] + sum(qU==l)
                attr(stageN[[i+1]], "probs_k")[l] <- 
                  attr(stageN[[i+1]], "probs_k")[l] + sum(qK==l)
              }
            }
          }
          if((screening)&&(screenCount%%screenPeriod == 0) && ScreenType == 1){## ORGANIZED SCREENING
            pU <- runif(attr(stageN[[i+1]], "probs_u")[j])
            pK <- runif(attr(stageN[[i+1]], "probs_k")[j])
            nScreenedU <- sum(pU < screenCoverage[currentAgeGroup])
            nScreenedK <- sum(pK < screenCoverage[currentAgeGroup])
            if(dnaScreening && currentAgeGroup%in%dnaScAgeGroups){
              cost.md <- cost.md + nScreenedU*dnaScCost.md
              cost.nmd <- cost.nmd + nScreenedU*dnaScCost.nmd
              cost.i <- cost.i + nScreenedU*dnaScCost.i
              p2 <- runif(nScreenedU)
              attr(stageN[[i+1]], "nVPH") <- ifelse(!is.null(attr(stageN[[i+1]], "nVPH")), attr(stageN[[i+1]], "nVPH")+nScreenedU+nScreenedK,
                                                    nScreenedU+nScreenedK)
              nDetected <- sum(p2 < dnaScSensi[j])
              attr(stageN[[i+1]], "nCito") <- ifelse(!is.null(attr(stageN[[i+1]], "nCito")), attr(stageN[[i+1]], "nCito")+nDetected,
                                                     nDetected)
              p22 <- runif(nDetected)
              nPapDetected <- sum(p22 < papScSensi[j])
              cost.md <- cost.md + nDetected*screenPrice.md
              cost.nmd <- cost.nmd + nDetected*screenPrice.nmd
              cost.i <- cost.i + nDetected*screenPrice.i
              cost.md <- cost.md + nPapDetected*costCoefs.md[j]
              cost.nmd <- cost.nmd + nPapDetected*costCoefs.nmd[j]
              cost.i <- cost.i + nPapDetected*costCoefs.i[j]
              p3 <- runif(nPapDetected)
              nRecovered <- sum(p3 < screenProbs[j])
              attr(stageN[[i+1]], "probs_u")[j] <- 
                attr(stageN[[i+1]], "probs_u")[j] - nPapDetected
              attr(stageN[[i+1]], "probs_k")[j] <- 
                attr(stageN[[i+1]], "probs_k")[j] + nPapDetected - nRecovered
              if(j <= 5)
                attr(stageN[[i+1]], "probs_u")[1] <-
                attr(stageN[[i+1]], "probs_u")[1] + nRecovered
              else{
                stageN[[i+1]][10] <- stageN[[i+1]][10] + nRecovered
                newSurvivalCases <- newSurvivalCases + nRecovered
              }
            }
            else{
              cost.md <- cost.md + nScreenedU*screenPrice.md
              cost.nmd <- cost.nmd + nScreenedU*screenPrice.nmd
              cost.i <- cost.i + nScreenedU*screenPrice.i
              p2 <- runif(nScreenedU)
              attr(stageN[[i+1]], "nVPH") <- 0
              attr(stageN[[i+1]], "nCito") <- ifelse(!is.null(attr(stageN[[i+1]], "nCito")), attr(stageN[[i+1]], "nCito")+nScreenedU+nScreenedK,
                                                     nScreenedU+nScreenedK)
              nDetected <- sum(p2 < screenSensi[j])
              cost.md <- cost.md + nDetected*costCoefs.md[j]
              cost.nmd <- cost.nmd + nDetected*costCoefs.nmd[j]
              cost.i <- cost.i + nDetected*costCoefs.i[j]
              p3 <- runif(nDetected)
              nRecovered <- sum(p3 < screenProbs[j])
              attr(stageN[[i+1]], "probs_u")[j] <- 
                attr(stageN[[i+1]], "probs_u")[j] - nDetected
              attr(stageN[[i+1]], "probs_k")[j] <- 
                attr(stageN[[i+1]], "probs_k")[j] + nDetected - nRecovered
              if(j <= 5)
                attr(stageN[[i+1]], "probs_u")[1] <-
                attr(stageN[[i+1]], "probs_u")[1] + nRecovered
              else{
                stageN[[i+1]][10] <- stageN[[i+1]][10] + nRecovered
                newSurvivalCases <- newSurvivalCases + nRecovered
              }
            }
            cost.md <- cost.md + nScreenedK*newScreenPrice.md
            cost.nmd <- cost.nmd + nScreenedK*newScreenPrice.nmd
            cost.i <- cost.i + nScreenedK*newScreenPrice.i
          }
          if((screening) && ScreenType == 2){## OPPORTUNISTIC SCREENING
            for (k in 1:nrow(OpScreen))
            {
              screenPeriod <- OpScreen[k, 1]
              if (screenCount%%screenPeriod == 0 & OpScreen[k, 2] != 0) 
              {
                pU <- runif(attr(stageN[[i+1]], "probs_u")[j])
                pK <- runif(attr(stageN[[i+1]], "probs_k")[j])
                nScreenedU <- sum(pU < screenCoverage[currentAgeGroup])*OpScreen[k, 2]/100
                nScreenedK <- sum(pK < screenCoverage[currentAgeGroup])*OpScreen[k, 2]/100
                if(dnaScreening && currentAgeGroup%in%dnaScAgeGroups){
                  cost.md <- cost.md + nScreenedU*dnaScCost.md
                  cost.nmd <- cost.nmd + nScreenedU*dnaScCost.nmd
                  cost.i <- cost.i + nScreenedU*dnaScCost.i
                  p2 <- runif(nScreenedU)
                  nDetected <- sum(p2 < dnaScSensi[j])
                  attr(stageN[[i+1]], "nVPH") <- ifelse(!is.null(attr(stageN[[i+1]], "nVPH")), attr(stageN[[i+1]], "nVPH")+nScreenedU+nScreenedK,
                                                        nScreenedU+nScreenedK)
                  attr(stageN[[i+1]], "nCito") <- ifelse(!is.null(attr(stageN[[i+1]], "nCito")), attr(stageN[[i+1]], "nCito")+nDetected,
                                                         nDetected)
                  p22 <- runif(nDetected)
                  nPapDetected <- sum(p22 < papScSensi[j])
                  cost.md <- cost.md + nDetected*screenPrice.md
                  cost.nmd <- cost.nmd + nDetected*screenPrice.nmd
                  cost.i <- cost.i + nDetected*screenPrice.i
                  cost.md <- cost.md + nPapDetected*costCoefs.md[j]
                  cost.nmd <- cost.nmd + nPapDetected*costCoefs.nmd[j]
                  cost.i <- cost.i + nPapDetected*costCoefs.i[j]
                  p3 <- runif(nPapDetected)
                  nRecovered <- sum(p3 < screenProbs[j])
                  attr(stageN[[i+1]], "probs_u")[j] <- 
                    attr(stageN[[i+1]], "probs_u")[j] - nPapDetected
                  attr(stageN[[i+1]], "probs_k")[j] <- 
                    attr(stageN[[i+1]], "probs_k")[j] + nPapDetected - nRecovered
                  if(j <= 5)
                    attr(stageN[[i+1]], "probs_u")[1] <-
                    attr(stageN[[i+1]], "probs_u")[1] + nRecovered
                  else{
                    stageN[[i+1]][10] <- stageN[[i+1]][10] + nRecovered
                    newSurvivalCases <- newSurvivalCases + nRecovered
                  }
                }
                else{
                  cost.md <- cost.md + nScreenedU*screenPrice.md
                  cost.nmd <- cost.nmd + nScreenedU*screenPrice.nmd
                  cost.i <- cost.i + nScreenedU*screenPrice.i
                  p2 <- runif(nScreenedU)
                  attr(stageN[[i+1]], "nCito") <- ifelse(!is.null(attr(stageN[[i+1]], "nCito")), attr(stageN[[i+1]], "nCito")+nScreenedU+nScreenedK,
                                                         +nScreenedU+nScreenedK)
                  attr(stageN[[i+1]], "nVPH") <- 0
                  nDetected <- sum(p2 < screenSensi[j])
                  cost.md <- cost.md + nDetected*costCoefs.md[j]
                  cost.nmd <- cost.nmd + nDetected*costCoefs.nmd[j]
                  cost.i <- cost.i + nDetected*costCoefs.i[j]
                  p3 <- runif(nDetected)
                  nRecovered <- sum(p3 < screenProbs[j])
                  attr(stageN[[i+1]], "probs_u")[j] <- 
                    attr(stageN[[i+1]], "probs_u")[j] - nDetected
                  attr(stageN[[i+1]], "probs_k")[j] <- 
                    attr(stageN[[i+1]], "probs_k")[j] + nDetected - nRecovered
                  if(j <= 5)
                    attr(stageN[[i+1]], "probs_u")[1] <-
                    attr(stageN[[i+1]], "probs_u")[1] + nRecovered
                  else{
                    stageN[[i+1]][10] <- stageN[[i+1]][10] + nRecovered
                    newSurvivalCases <- newSurvivalCases + nRecovered
                  }
                }
                cost.md <- cost.md + nScreenedK*newScreenPrice.md
                cost.nmd <- cost.nmd + nScreenedK*newScreenPrice.nmd
                cost.i <- cost.i + nScreenedK*newScreenPrice.i
              }
            }
          }
          if((via)&&(viaCount%%viaPeriod == 0)){## VIA
            pU <- runif(attr(stageN[[i+1]], "probs_u")[j])
            pK <- runif(attr(stageN[[i+1]], "probs_k")[j])
            nScreenedVIAU <- sum(pU < viaCoverage[currentAgeGroup])
            nScreenedVIAK <- sum(pK < viaCoverage[currentAgeGroup])
            cost.md <- cost.md + nScreenedVIAU*via.md
            cost.nmd <- cost.nmd + nScreenedVIAU*via.nmd
            cost.i <- cost.i + nScreenedVIAU*via.i
            p2 <- runif(nScreenedVIAU)
            nDetected <- sum(p2 < via.Sensi[j])
            cost.md <- cost.md + nDetected*costCoefs.md[j]
            cost.nmd <- cost.nmd + nDetected*costCoefs.nmd[j]
            cost.i <- cost.i + nDetected*costCoefs.i[j]
            p3 <- runif(nDetected)
            nRecovered <- sum(p3 < viaProbs[j])
            attr(stageN[[i+1]], "probs_u")[j] <- 
              attr(stageN[[i+1]], "probs_u")[j] - nDetected
            attr(stageN[[i+1]], "probs_k")[j] <- 
              attr(stageN[[i+1]], "probs_k")[j] + nDetected - nRecovered
            if(j <= 5)
              attr(stageN[[i+1]], "probs_u")[1] <-
              attr(stageN[[i+1]], "probs_u")[1] + nRecovered
            else{
              stageN[[i+1]][10] <- stageN[[i+1]][10] + nRecovered
              newSurvivalCases <- newSurvivalCases + nRecovered
            }
          }
          ## GENERATE SYMPTOMS (COST COMPUTATIONS)
          if(j%in%6:9){
            cost.md <- cost.md + costCoefs.md[j]*attr(stageN[[i+1]], "probs_k")[j]
            cost.nmd <- cost.nmd + costCoefs.nmd[j]*attr(stageN[[i+1]], "probs_k")[j]
            cost.i <- cost.i + costCoefs.i[j]*attr(stageN[[i+1]], "probs_k")[j]
            p <- runif(attr(stageN[[i+1]], "probs_u")[j])
            nSymp <- sum(p < figoSymProb[j - 5])
            cost.md <- cost.md + nSymp*costCoefs.md[j]
            cost.nmd <- cost.nmd + nSymp*costCoefs.nmd[j]
            cost.i <- cost.i + nSymp*costCoefs.i[j]
            p <- runif(nSymp)
            nCured <- sum(p < screenProbs[j])
            attr(stageN[[i+1]], "probs_u")[j] <-
              attr(stageN[[i+1]], "probs_u")[j] - nSymp
            attr(stageN[[i+1]], "probs_k")[j] <-
              attr(stageN[[i+1]], "probs_k")[j] + nSymp - nCured
            stageN[[i+1]][10] <- stageN[[i+1]][10] + nCured
            newSurvivalCases <- newSurvivalCases + nCured
          }
        }
        else{
          p <- runif(stageN[[i]][j]) # Generate Uniform(0,1) random numbers for each individual at the stage
          q <- findInterval(p, unlist(cumProbs[[currentAgeGroup]][j,])) + 1 # Assign the generated numbers to the cumulative probabilities
          out <- sum(q != j) # Number of individuals leaving the stage
          stageN[[i+1]][j] <- stageN[[i+1]][j] - out # Subtract the number of individuals leaving the stage
          for(l in unique(q)){ # Loop for assigning the leaving individuals to their new stage
            if(l!=j){
              if(l%in%3:9)
                attr(stageN[[i+1]], "probs_u")[l-2] <-
                  attr(stageN[[i+1]], "probs_u")[l-2] + sum(q == l)
              else
                stageN[[i+1]][l] <- stageN[[i+1]][l] + sum(q == l)
            }
          }
          if(j != 10)
            newSurvivalCases <- newSurvivalCases + sum(q == 10)
          if((screening) && (screenCount%%screenPeriod == 0) && (j == 10) && ScreenType==1){
            p <- runif(stageN[[i+1]][j])
            nScreened <- sum(p < screenCoverage[currentAgeGroup])
            if(dnaScreening && currentAgeGroup%in%dnaScAgeGroups){
              cost.md <- cost.md + nScreened*dnaScCost.md
              cost.nmd <- cost.nmd + nScreened*dnaScCost.nmd
              cost.i <- cost.i + nScreened*dnaScCost.i
              cost.md <- cost.md + nScreened*screenPrice.md
              cost.nmd <- cost.nmd + nScreened*screenPrice.nmd
              cost.i <- cost.i + nScreened*screenPrice.i
              attr(stageN[[i+1]], "CCdeaths") <- stageN[[i+1]][11] - stageN[[i]][11]
              names(attr(stageN[[i+1]], "CCdeaths")) <- NULL
              attr(stageN[[i+1]], "Otherdeaths") <- stageN[[i+1]][12] - stageN[[i]][12]
              names(attr(stageN[[i+1]], "Otherdeaths")) <- NULL
            }
          }
          if((screening) && (j == 10) && ScreenType==2){
            for (k in 1:nrow(OpScreen))
            {
              screenPeriod <- OpScreen[k, 1]
              if (screenCount%%screenPeriod == 0 & OpScreen[k, 2] != 0) 
              {
                p <- runif(stageN[[i+1]][j])
                nScreened <- sum(p < screenCoverage[currentAgeGroup])*OpScreen[k, 2]/100
                if(dnaScreening && currentAgeGroup%in%dnaScAgeGroups){
                  cost.md <- cost.md + nScreened*dnaScCost.md
                  cost.nmd <- cost.nmd + nScreened*dnaScCost.nmd
                  cost.i <- cost.i + nScreened*dnaScCost.i
                  cost.md <- cost.md + nScreened*screenPrice.md
                  cost.nmd <- cost.nmd + nScreened*screenPrice.nmd
                  cost.i <- cost.i + nScreened*screenPrice.i
                  attr(stageN[[i+1]], "CCdeaths") <- stageN[[i+1]][11] - stageN[[i]][11]
                  names(attr(stageN[[i+1]], "CCdeaths")) <- NULL
                  attr(stageN[[i+1]], "Otherdeaths") <- stageN[[i+1]][12] - stageN[[i]][12]
                  names(attr(stageN[[i+1]], "Otherdeaths")) <- NULL
                }
              }
            }
          }
        }
      }
      for(j in 1:9)
        stageN[[i+1]][j] <- attr(stageN[[i+1]], "probs_u")[j] + 
        attr(stageN[[i+1]], "probs_k")[j]
      attr(stageN[[i+1]], "newSurvivalCases") <- newSurvivalCases
      attr(stageN[[i+1]], "md_cost") <- cost.md
      attr(stageN[[i+1]], "nmd_cost") <- cost.nmd
      attr(stageN[[i+1]], "i_cost") <- cost.i
      names(stageN)[[i]] <- currentAge
      currentAge <- currentAge + stepTime
      attr(stageN[[i+1]], "utility") <- utilityCoefs%*%stageN[[i+1]]
      attr(stageN[[i+1]], "CCdeaths") <- stageN[[i+1]][11] - stageN[[i]][11]
      names(attr(stageN[[i+1]], "CCdeaths")) <- NULL
      attr(stageN[[i+1]], "Otherdeaths") <- stageN[[i+1]][12] - stageN[[i]][12]
      names(attr(stageN[[i+1]], "Otherdeaths")) <- NULL
      screenCount <- screenCount + 1
      viaCount    <- viaCount + 1
    }
    names(stageN)[[length(stageN)]] <- currentAge
    return(stageN)
  }
  res <- lapply(seq(1,Nsim,1), func)
  listDat <- lapply(res, function(x){
    y <- as.data.frame(t(as.matrix(as.data.frame(x))))
    aux <- unlist(lapply(x, function(x) attr(x, "newCases")))
    attr(y, "newCases") <- aux
    aux <- unlist(lapply(x, function(x) attr(x, "newCin1Cases")))
    attr(y, "newCin1Cases") <- aux
    aux <- unlist(lapply(x, function(x) attr(x, "newCin2Cases")))
    attr(y, "newCin2Cases") <- aux
    aux <- unlist(lapply(x, function(x) attr(x, "newCin3Cases")))
    attr(y, "newCin3Cases") <- aux
    aux2 <- unlist(lapply(x, function(x) attr(x, "newSurvivalCases")))
    attr(y, "newSurvivalCases") <- aux2
    aux <- unlist(lapply(x, function(x) attr(x, "md_cost")))
    attr(y, "md_cost") <- aux
    aux <- unlist(lapply(x, function(x) attr(x, "nmd_cost")))
    attr(y, "nmd_cost") <- aux
    aux <- unlist(lapply(x, function(x) attr(x, "i_cost")))
    attr(y, "i_cost") <- aux
    aux <- unlist(lapply(x, function(x) attr(x, "utility")))
    attr(y, "utility") <- aux
    aux <- unlist(lapply(x, function(x) attr(x, "CCdeaths")))
    attr(y, "CCdeaths") <- aux
    aux <- unlist(lapply(x, function(x) attr(x, "Otherdeaths")))
    attr(y, "Otherdeaths") <- aux
    aux <- unlist(lapply(x, function(x) attr(x, "nCito")))
    attr(y, "nCito") <- aux
    aux <- unlist(lapply(x, function(x) attr(x, "nVPH")))
    attr(y, "nVPH") <- aux
    rownames(y) <- NULL
    y
  })
  fullDat <- data.frame()
  attr(fullDat, "newCases")         <- data.frame()
  attr(fullDat, "newCin1Cases")     <- data.frame()
  attr(fullDat, "newCin2Cases")     <- data.frame()
  attr(fullDat, "newCin3Cases")     <- data.frame()
  attr(fullDat, "newSurvivalCases") <- data.frame()
  attr(fullDat, "md_cost")          <- data.frame()
  attr(fullDat, "nmd_cost")         <- data.frame()
  attr(fullDat, "i_cost")           <- data.frame()
  attr(fullDat, "utility")          <- data.frame()
  attr(fullDat, "CCdeaths")         <- data.frame()
  attr(fullDat, "Otherdeaths")      <- data.frame()
  attr(fullDat, "nCito")            <- data.frame()
  attr(fullDat, "nVPH")             <- data.frame()
  for(i in 1:length(listDat)){
    fullDat <- rbind(fullDat, cbind(listDat[[i]], sim=i))
    attr(fullDat, "newCases") <- rbind(attr(fullDat, "newCases"),
                                       attr(listDat[[i]], "newCases"))
    attr(fullDat, "newCin1Cases") <- rbind(attr(fullDat, "newCin1Cases"),
                                           attr(listDat[[i]],"newCin1Cases"))
    attr(fullDat, "newCin2Cases") <- rbind(attr(fullDat, "newCin2Cases"),
                                           attr(listDat[[i]],"newCin2Cases"))
    attr(fullDat, "newCin3Cases") <- rbind(attr(fullDat, "newCin3Cases"),
                                           attr(listDat[[i]],"newCin3Cases"))
    attr(fullDat, "newSurvivalCases") <- rbind(attr(fullDat, "newSurvivalCases"),
                                               attr(listDat[[i]], "newSurvivalCases"))
    attr(fullDat, "md_cost") <- rbind(attr(fullDat, "md_cost"),
                                      attr(listDat[[i]], "md_cost"))
    attr(fullDat, "nmd_cost") <- rbind(attr(fullDat, "nmd_cost"),
                                       attr(listDat[[i]], "nmd_cost"))
    attr(fullDat, "i_cost") <- rbind(attr(fullDat, "i_cost"),
                                     attr(listDat[[i]], "i_cost"))
    attr(fullDat, "utility") <- rbind(attr(fullDat, "utility"),
                                      attr(listDat[[i]], "utility"))
    attr(fullDat, "CCdeaths") <- rbind(attr(fullDat, "CCdeaths"),
                                       attr(listDat[[i]], "CCdeaths"))
    attr(fullDat, "Otherdeaths") <- rbind(attr(fullDat, "Otherdeaths"),
                                          attr(listDat[[i]], "Otherdeaths"))
    attr(fullDat, "nCito") <- rbind(attr(fullDat, "nCito"),
                                    attr(listDat[[i]], "nCito")) 
    attr(fullDat, "nVPH") <- rbind(attr(fullDat, "nVPH"),
                                   attr(listDat[[i]], "nVPH")) 
  }
  
  fullDat$age <- ageGroups[1, 1]:(stopYear-1)
  rownames(attr(fullDat, "newCases")) <- paste0("sim", 1:Nsim)
  newCases1 <- attr(fullDat, "newCases")
  rownames(attr(fullDat, "newCin1Cases")) <- paste0("sim", 1:Nsim)
  newCin1Cases1 <- attr(fullDat, "newCin1Cases")
  rownames(attr(fullDat, "newCin2Cases")) <- paste0("sim", 1:Nsim)
  newCin2Cases1 <- attr(fullDat, "newCin2Cases")
  rownames(attr(fullDat, "newCin3Cases")) <- paste0("sim", 1:Nsim)
  newCin3Cases1 <- attr(fullDat, "newCin3Cases")
  rownames(attr(fullDat, "newSurvivalCases")) <- paste0("sim", 1:Nsim)
  newSurvivalCases1 <- attr(fullDat, "newSurvivalCases")
  rownames(attr(fullDat, "md_cost")) <- paste0("sim", 1:Nsim)
  mdcost1 <- attr(fullDat, "md_cost")
  rownames(attr(fullDat, "nmd_cost")) <- paste0("sim", 1:Nsim)
  nmdcost1 <- attr(fullDat, "nmd_cost")
  rownames(attr(fullDat, "i_cost")) <- paste0("sim", 1:Nsim)
  icost1 <- attr(fullDat, "i_cost")
  rownames(attr(fullDat, "utility")) <- paste0("sim", 1:Nsim)
  utility1 <- attr(fullDat, "utility")
  rownames(attr(fullDat, "CCdeaths")) <- paste0("sim", 1:Nsim)
  CCdeaths1 <- attr(fullDat, "CCdeaths")
  rownames(attr(fullDat, "Otherdeaths")) <- paste0("sim", 1:Nsim)
  Otherdeaths1 <- attr(fullDat, "Otherdeaths")
  rownames(attr(fullDat, "nCito")) <- paste0("sim", 1:Nsim)
  nCito1 <- attr(fullDat, "nCito")
  rownames(attr(fullDat, "nVPH")) <- paste0("sim", 1:Nsim)
  nVPH1 <- attr(fullDat, "nVPH")
  attr(fullDat, "Call") <- ll
  return(fullDat)
}