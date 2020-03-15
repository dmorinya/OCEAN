library(gdata)
library(mc2d)
library(shinyjs)
library(xlsx)
library(shiny)
library(xtable)
library(readxl) 
library(WGCNA)
library(gridExtra)
library(gtable)
library(data.table)
library(shinyalert)

options(repos = c(BiocManager::repositories(), "https://cran.r-project.org/"))

source("simCohort.R", encoding="utf-8")
source("NHvalida.R", encoding="utf-8")
source("eucl.distance.R", encoding="utf-8")

#8-9/5 - Incorporaci? de que l'usuari descarregui els resultats de
#cost-efectivitat (restringit a un o dos escenaris)
#9-10/5 - Incorporaci? de noves estrat?gies de prevenci? (vacunes)
#11-12/5 - Incorporaci? altres tipus de c?ncers anogenital / Ampliaci? n?m.
#d'escenaris disponibles

##QALYS

qalysstrategy <- function(scenario, disc.rate=0)
{
  res <- as.data.frame(apply(attr(scenario, "utility"), 2, mean))
  colnames(res) <- "QALY"
  res$QALY <- res$QALY/(1+disc.rate/100)^(as.numeric(rownames(res))-as.numeric(rownames(res))[1])
  qalsrt <- as.double(mean(sum(res)))
  return(qalsrt)
}

qalysperson <- function(scenario, disc.rate=0)
{
  res <- as.data.frame(apply(attr(scenario, "utility"), 2, mean))
  colnames(res) <- "QALY"
  res$QALY <- res$QALY/(1+disc.rate/100)^(as.numeric(rownames(res))-as.numeric(rownames(res))[1])
  qalp <-as.double(mean(sum(res))/scenario[1, 1])
  return(qalp)
}

###COSTS

medstrat <- function(scenario, disc.rate=0)
{
  mdres  <- as.data.frame(apply(attr(scenario, "md_cost"), 2, mean))
  colnames(mdres)  <- "cost"
  mdres$cost <- mdres$cost/(1+disc.rate/100)^(as.numeric(rownames(mdres))-as.numeric(rownames(mdres))[1])
  meds <-as.double(mean(sum(mdres)))
  return(meds)
}

medpers <- function(scenario, disc.rate=0)
{
  mdres  <- as.data.frame(apply(attr(scenario, "md_cost"), 2, mean))
  colnames(mdres)  <- "cost"
  mdres$cost <- mdres$cost/(1+disc.rate/100)^(as.numeric(rownames(mdres))-as.numeric(rownames(mdres))[1])
  medp <- as.double(mean(sum(mdres))/scenario[1, 1])
  return(medp)
}

nomedstrat <- function(scenario, disc.rate=0)
{
  nmdres <- as.data.frame(apply(attr(scenario, "nmd_cost"), 2, mean))
  colnames(nmdres) <- "cost"
  nmdres$cost <- nmdres$cost/(1+disc.rate/100)^(as.numeric(rownames(nmdres))-as.numeric(rownames(nmdres))[1])
  nomeds <- as.double(mean(sum(nmdres)))
  return(nomeds)
}

nomedpers <- function(scenario, disc.rate=0)
{
  nmdres <- as.data.frame(apply(attr(scenario, "nmd_cost"), 2, mean))
  colnames(nmdres) <- "cost"
  nmdres$cost <- nmdres$cost/(1+disc.rate/100)^(as.numeric(rownames(nmdres))-as.numeric(rownames(nmdres))[1])
  nomedp <- as.double(mean(sum(nmdres))/scenario[1, 1])
  return(nomedp)
}

indstrat <- function(scenario, disc.rate=0)
{
  ires   <- as.data.frame(apply(attr(scenario, "i_cost"), 2, mean))
  colnames(ires)   <- "cost"
  ires$cost <- ires$cost/(1+disc.rate/100)^(as.numeric(rownames(ires))-as.numeric(rownames(ires))[1])
  inds <- as.double(mean(sum(ires)))
  return(inds)
}

indpers <- function(scenario, disc.rate=0)
{
  ires   <- as.data.frame(apply(attr(scenario, "i_cost"), 2, mean))
  colnames(ires)   <- "cost"
  ires$cost <- ires$cost/(1+disc.rate/100)^(as.numeric(rownames(ires))-as.numeric(rownames(ires))[1])
  indp <-  as.double(mean(sum(ires))/scenario[1, 1])
  return(indp)
}


totalstrat <- function(scenario, disc.rate=0)
{
  mdres  <- as.data.frame(apply(attr(scenario, "md_cost"), 2, mean))
  nmdres <- as.data.frame(apply(attr(scenario, "nmd_cost"), 2, mean))
  ires   <- as.data.frame(apply(attr(scenario, "i_cost"), 2, mean))
  res    <- mdres + nmdres + ires
  colnames(res)    <- "cost"
  res$cost  <- res$cost/(1+disc.rate/100)^(as.numeric(rownames(res))-as.numeric(rownames(res))[1])
  totals <- as.double(mean(sum(res)))
  return(totals)
}


totalpers <- function(scenario, disc.rate=0)
{
  mdres  <- as.data.frame(apply(attr(scenario, "md_cost"), 2, mean))
  nmdres <- as.data.frame(apply(attr(scenario, "nmd_cost"), 2, mean))
  ires   <- as.data.frame(apply(attr(scenario, "i_cost"), 2, mean))
  res    <- mdres + nmdres + ires
  colnames(res)    <- "cost"
  res$cost  <- res$cost/((1+disc.rate/100)^(as.numeric(rownames(res))-as.numeric(rownames(res))[1]))
  totalp <- as.double(mean(sum(res))/scenario[1, 1])
  return(totalp)
}



##LIFE EXPECTANCI


lemodif <- function(scenario, res,  disc.rate=0)
{
  res2 <- as.data.frame(apply(attr(scenario, "utility"), 2, mean))
  res3 <- as.double(res/(1+disc.rate/100)^(as.numeric(rownames(res2))[1]+0.5))
  return(res3)
}


ylsstrat <- function(scenario1, scenario2, res1, res2, disc.rate = 0)
{
  if (scenario1[1, 1] != scenario2[1, 1]) stop("Different cohort sizes")
  res3 <- lemodif(scenario1, res1,  disc.rate)
  res4 <- lemodif(scenario2, res2, disc.rate)
  res <- res4 - res3
  ylss <- as.double(res*scenario1[1, 1])
  return(ylss)
  
}


ylspers <- function(scenario1, scenario2, res1, res2, disc.rate = 0)
{
  if (scenario1[1, 1] != scenario2[1, 1]) stop("Different cohort sizes")
  res3 <- lemodif(scenario1, res1, disc.rate)
  res4 <- lemodif(scenario2, res2, disc.rate)
  res <- res4 - res3
  ylsp <- as.double(res)
  return(ylsp)
}








