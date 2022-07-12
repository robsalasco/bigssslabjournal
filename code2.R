group <- c(1,1,2,2,3,4,4)
y <- c(12,3,4,7,2,2,1)
x <- c(3,2,2,3,1,1,2)

# distance matrix (needs to be a matrix object)
distance_m <- as.matrix(dist(data.frame(x,y), diag = TRUE,  upper = TRUE))

# mean distance
ptt <- mean(distance_m)

# for each group a for loop
mprox <- rep(NA,length(unique(group)))
pi <- rep(NA,length(unique(group)))

i = 1
for (i in 1:length(unique(group))) {
  mprox[i] <- mean(distance_m[group==i, group==i], na.rm = TRUE)
  pi[i] <- sum(distance_m[group==i, group==i])/length(unique(group))
}

sum(mprox*pi) / ptt

