# helpers

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

# packages

packages = c("tidyverse", "compiler", "Hmisc", "stats")
fpackage.check(packages)

# Variance in the pairwise opinion differences

fPvar <- function(votes, positions, method = "euclidean") {
  positions <- positions * 2  #this function wants a range of 2 for each attitude
  distances <- as.matrix(dist(positions, method = method))
  votes_mat <- votes %o% votes
  diag(votes_mat)[diag(votes_mat) > 0] <- diag(votes_mat)[diag(votes_mat) > 1] - 1  #we do not want to include distance to yourself, thus i cannot by j in the dyad_ij. 
  Pvar <- Hmisc::wtd.var(as.numeric(distances), as.numeric(votes_mat))/NCOL(positions)  #we normalize for number of opinions
  return(Pvar)
}

fPvar <- cmpfun(fPvar)


# Distance to the center


fPV <- function(votes, positions, method = "euclidean") {
  shares <- votes/sum(votes, na.rm = TRUE)
  pbar <- rep(NA, NCOL(positions))
  pbar <- as.numeric(t(shares) %*% as.matrix(positions))  #center of mass / mean position
  
  # distances to mean
  if (method != "sq") {
    if (NCOL(positions) == 1) {
      distances <- as.matrix(stats::dist(c(pbar, positions), method = method))[, 1][-1]
    } else {
      distances <- as.matrix(stats::dist(rbind(pbar, positions), method = method))[, 1][-1]
    }
  }
  
  # function resolved
  
  if (method == "sq") {
    
    distmatrix <- rbind(pbar, positions)
    
    distances = matrix(NA, nrow(distmatrix), ncol(distmatrix))
    
    for (i in 1:nrow(distmatrix)) {
      for (j in 1:nrow(distmatrix))
      {
        distances[i,]= (distmatrix[i,] - distmatrix[j,])^2
      }
    }
  }
  
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


x <- c(0, 0.5, 1, 0, 0.5, 1)
y <- c(0, 0.5, 1, 1, 0.5, 0)

positions <- data.frame(x, y)

votes1 <- c(100, 100, 100, 100, 100, 100)
votes2 <- c(100, 0, 100, 0, 0, 0)
votes3 <- c(0, 0, 100, 0, 0, 100)

# weighted

Hmisc::wtd.var(positions$x * 2, votes1)

fPvar(votes = votes1, positions = positions[, 1])
fPV(votes = votes1, positions = positions[, 1])
fPER(votes = votes1, positions = positions[, 1])

votes1
positions[, 1]
