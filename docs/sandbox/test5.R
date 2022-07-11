fsave <- function(x, file, location = "./data/processed/", ...) {
  if (!dir.exists(location))
    dir.create(location)
  datename <- substr(gsub("[:-]", "", Sys.time()), 1, 8)
  totalname <- paste(location, datename, file, sep = "")
  print(paste("SAVED: ", totalname, sep = ""))
  save(x, file = totalname)
}

fpackage.check <- function(packages) {
  lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
}

colorize <- function(x, color) {
  sprintf("<span style='color: %s;'>%s</span>", color, x)
}

packages = c("tidyverse", "sf", "ggplot2", "ggimage", "ggmap", "compiler", "Hmisc", "stats")

fpackage.check(packages)

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


# let us define a Moran's I function (heavily based on Moran.I of package ape) you can toggle
# rowstandardization
fMoranI <- function(x, y = NULL, weight, scaled = FALSE, na.rm = FALSE, alternative = "two.sided", rowstandardize = TRUE) {
  if (is.null(y)) {
    y <- x
  }
  
  if (dim(weight)[1] != dim(weight)[2])
    stop("'weight' must be a square matrix")
  nx <- length(x)
  ny <- length(y)
  if (dim(weight)[1] != nx | dim(weight)[1] != ny)
    stop("'weight' must have as many rows as observations in 'x' (and 'y', for the bivariate case) ")
  ei <- -1/(nx - 1)
  nas <- is.na(x) | is.na(y)
  if (any(nas)) {
    if (na.rm) {
      x <- x[!nas]
      y <- y[!nas]
      nx <- length(x)
      weight <- weight[!nas, !nas]
    } else {
      warning("'x' and/or 'y' have missing values: maybe you wanted to set na.rm = TRUE?")
      return(list(observed = NA, expected = ei, sd = NA, p.value = NA))
    }
  }
  if (rowstandardize) {
    ROWSUM <- rowSums(weight)
    ROWSUM[ROWSUM == 0] <- 1
    weight <- weight/ROWSUM
  }
  s <- sum(weight)
  mx <- mean(x)
  sx <- x - mx
  my <- mean(y)
  sy <- y - my
  v <- sum(sx^2)
  cv <- sum(weight * sx %o% sy)
  obs <- (nx/s) * (cv/v)
  cv_loc <- rowSums(weight * sx %o% sy)
  obs_loc <- (nx/s) * (cv_loc/v)
  if (scaled) {
    i.max <- (nx/s) * (sd(rowSums(weight) * sx)/sqrt(v/(nx - 1)))
    obs <- obs/i.max
    obs_loc <- obs_loc/i.max
  }
  S1 <- 0.5 * sum((weight + t(weight))^2)
  S2 <- sum((apply(weight, 1, sum) + apply(weight, 2, sum))^2)
  s.sq <- s^2
  k <- (sum(sx^4)/nx)/(v/nx)^2
  sdi <- sqrt((nx * ((nx^2 - 3 * nx + 3) * S1 - nx * S2 + 3 * s.sq) - k * (nx * (nx - 1) * S1 - 2 *
                                                                             nx * S2 + 6 * s.sq))/((nx - 1) * (nx - 2) * (nx - 3) * s.sq) - 1/((nx - 1)^2))
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  pv <- pnorm(obs, mean = ei, sd = sdi)
  if (alternative == "two.sided")
    pv <- if (obs <= ei)
      2 * pv else 2 * (1 - pv)
  if (alternative == "greater")
    pv <- 1 - pv
  list(observed = obs, expected = ei, sd = sdi, p.value = pv, observed_locals = obs_loc)
  
  
}
fMoranI <- cmpfun(fMoranI)

# Moran's I for aggregated
# data_____________________________________________________________________
fMoranIdens <- function(x, y = NULL, weight, dens = NULL) {
  # Adapted from Anselin (1995, eq. 7, 10, 11)
  # https://onlinelibrary.wiley.com/doi/epdf/10.1111/j.1538-4632.1995.tb00338.x dens: the
  # proportion of individuals in each cell over the district population if individual level data
  # dens is.null and N is simply length of input if we have aggregate data then N should be total
  # population size (or actually just a large number)
  if (is.null(y)) {
    y <- x
  }
  N <- length(x)
  if (is.null(dens)) {
    dens <- rep(1/N, times = N)
  }
  
  # correct scaling of opinions for densities #this is really inefficient, should use weighted
  # var from hmsci
  v1dens_ind <- rep(x, times = (dens * N))
  v1dens <- (x - mean(v1dens_ind))/sd(v1dens_ind)
  v2dens_ind <- rep(y, times = (dens * N))
  v2dens <- (y - mean(v2dens_ind))/sd(v2dens_ind)
  
  # (density) weighted proximity matrix
  w <- weight
  wdens <- t(dens * t(w))
  wdens <- wdens/rowSums(wdens)
  
  # density and proximity weighted locals
  localI <- (v1dens * wdens %*% v2dens)  #formula 7
  
  # correct the normalization constants
  m2 <- sum(v1dens^2 * dens)
  S0 <- N  #we know the weight matrix for the individual level should add up to N
  ydens <- S0 * m2
  globalI <- sum(localI * dens * N)/ydens  # formula 10/11
  
  return(list(globalI = globalI, localI = as.numeric(localI)))
}
fMoranIdens <- cmpfun(fMoranIdens)

positions_df <- load("./data/processed/20220708positions_data.RData")

# I'm droppihng cases I have NA because unmatched names

merged %>% 
  drop_na() 

p <- ggplot(merged, aes(pop_immigration, dpes_sentence_death, label=party)) +
  geom_point() +
  geom_text(hjust=0, vjust=0) +
#  geom_image(aes(image = image)) + 
#  xlim(-2, 2) + 
#  ylim(-2, 2) + xlab("left <---------> right") +
#  ylab("conservative <---------> progressive") + 
  theme(aspect.ratio = 1)

p
