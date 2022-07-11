fPvar <- function(votes, positions, method = "euclidean") {
  positions <- positions * 2  #this function wants a range of 2
  distances <- as.matrix(dist(positions, method = method))
  votes_mat <- votes %o% votes
  diag(votes_mat)[diag(votes_mat) > 1] <- diag(votes_mat)[diag(votes_mat) > 1] - 1
  Pvar <- Hmisc::wtd.var(as.numeric(distances), as.numeric(votes_mat))
  return(Pvar)
}
fPvar <- cmpfun(fPvar)

fPV <- function(votes, positions, method = "euclidean") {
  shares <- votes/sum(votes, na.rm = TRUE)
  pbar <- rep(NA, NCOL(positions))
  pbar <- as.numeric(t(shares) %*% positions)  #center of mass / mean position
  
  # distances to mean
  if (method != "sq") {
    if (NCOL(positions) == 1) {
      distances <- as.matrix(stats::dist(c(pbar, positions), method = method))[, 1][-1]
    } else {
      distances <- as.matrix(stats::dist(rbind(pbar, positions), method = method))[, 1][-1]
    }
  }
  # if (method=='sq') {distances <- ??}
  
  # defining the constant
  if (method == "euclidean") {
    k <- 2/sqrt(NCOL(positions))
  }
  if (method == "manhattan") {
    k <- 2/NCOL(positions)
  }
  if (method == "sq") {
    k <- 1
  }
  PV <- k * sum(shares * distances)
  return(PV)
}
fPV <- cmpfun(fPV)

fPER <- function(alpha = 1, votes, positions, method = "euclidean") {
  positions <- positions
  distances <- as.matrix(stats::dist(positions, method = method))
  shares <- votes/sum(votes, na.rm = TRUE)
  sharesi <- shares^(1 + alpha)
  sharesj <- shares
  ER <- as.numeric(sharesi %*% distances %*% sharesj)
  return(ER)
}

fPER <- cmpfun(fPER)

#----

library(purrr)
library(future)
library(furrr)

load("./data/processed/20220708polling_df")

js_df <- x
rm(x)

positions_df <- read_csv2("./data/kieskompas_positie_partijen.csv")  #change to your own file!!

js_df <- ungroup(js_df)

js_df$Pvar <- rep(NA, nrow(js_df))
js_df$PER <- rep(NA, nrow(js_df))
js_df$PV <- rep(NA, nrow(js_df))

# I will use dimensions as above, but you will need to tweak of course.
positions <- (cbind(positions_df$x, positions_df$y) + 2)/4  #to range 0-1


parties <- c("BIJ1", "PvdD", "GL", "SP", "PvdA", "DENK", "Volt", "D66", "CU", "PLUS50",
             "PVV", "CDA", "BBB", "SGP", "VVD", "JA21", "FvD")

parties

#DOIT

library(rbenchmark)

benchmark("standard" = {
  for (i in 1:nrow(js_df)) {
    votes <- c(js_df$BIJ1[i], js_df$PvdD[i], js_df$GL[i], js_df$SP[i], js_df$PvdA[i], js_df$DENK[i],
               js_df$Volt[i], js_df$D66[i], js_df$CU[i], js_df$PLUS50[i], js_df$PVV[i], js_df$CDA[i], js_df$BBB[i],
               js_df$SGP[i], js_df$VVD[i], js_df$JA21[i], js_df$FvD[i])
    js_df$Pvar[i] <- fPvar(votes = votes, positions = positions)
    js_df$PER[i] <- fPER(votes = votes, positions = positions)
    js_df$PV[i] <- fPV(votes = votes, positions = positions)
  }
},
  "rowwise" = {
  js_df %>% 
    rowwise() %>% 
    mutate(Pvar = fPvar(votes = c(BIJ1, PvdD, GL, SP, PvdA, DENK, Volt, D66, CU, PLUS50, PVV, CDA, BBB, SGP, VVD, JA21, FvD), positions = positions)) %>% 
    mutate(PER = fPER(votes = c(BIJ1, PvdD, GL, SP, PvdA, DENK, Volt, D66, CU, PLUS50, PVV, CDA, BBB, SGP, VVD, JA21, FvD), positions = positions)) %>% 
    mutate(PV = fPV(votes = c(BIJ1, PvdD, GL, SP, PvdA, DENK, Volt, D66, CU, PLUS50, PVV, CDA, BBB, SGP, VVD, JA21, FvD), positions = positions)) %>% 
    select(Pvar, PER, PV)
},
"rowwise_c_across" = {
  js_df %>% 
    select(Adres, all_of(parties)) %>% 
    rowwise() %>% 
    mutate(Pvar = list(fPvar(votes = c_across(BIJ1:FvD),positions = !!positions))) %>% 
    mutate(PER = list(fPER(votes = c_across(BIJ1:FvD),positions = !!positions))) %>% 
    mutate(PV = list(fPV(votes = c_across(BIJ1:FvD),positions = !!positions))) %>% 
    unnest(c(Pvar, PER, PV)) %>% 
    select(Pvar, PER, PV)
},
"nested_and_map" = {
  js_df %>% 
    mutate(id = row_number()) %>% 
    group_by(id) %>% 
    summarise(parties_result = list(c(BIJ1, PvdD, GL, SP, PvdA, DENK, Volt, D66, CU, PLUS50, PVV, CDA, BBB, SGP, VVD, JA21, FvD))) %>% 
    mutate(Pvar = map_dbl(parties_result, function(x) fPvar(votes = x, positions = !!positions))) %>% 
    mutate(PER = map_dbl(parties_result, function(x) fPER(votes = x, positions = !!positions))) %>% 
    mutate(PV = map_dbl(parties_result, function(x) fPV(votes = x, positions = !!positions))) %>% 
    select(Pvar, PER, PV)
},
"nested_and_furrr" = {
  plan(multisession, workers = availableCores() - 1)
  
  js_df %>% 
    mutate(id = row_number()) %>% 
    group_by(id) %>% 
    summarise(parties_result = list(c(BIJ1, PvdD, GL, SP, PvdA, DENK, Volt, D66, CU, PLUS50, PVV, CDA, BBB, SGP, VVD, JA21, FvD))) %>% 
    mutate(Pvar = future_map_dbl(parties_result, function(x) fPvar(votes = x, positions = !!positions))) %>% 
    mutate(PER = future_map_dbl(parties_result, function(x) fPER(votes = x, positions = !!positions))) %>% 
    mutate(PV = future_map_dbl(parties_result, function(x) fPV(votes = x, positions = !!positions))) %>% 
    select(Pvar, PER, PV)
},
replications = 3,
columns = c("test", "elapsed"))









