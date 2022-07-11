set.seed(567732)
g1 <- sample(20:400, 10)  #counts group 1
g2 <- sample(20:400, 10)  #counts group 2
popcounts <- data.frame(g1, g2)
distances <- matrix(sample(20:400, 100), nrow = 10, ncol = 10)
distances[lower.tri(distances)] <- (t(distances)[lower.tri(distances)])
weights <- exp(-distances/100)
diag(weights) <- 0
rm(list = c("g1", "g2", "distances"))
weights


as.matrix(weights) %*% as.matrix(popcounts)

colSums(as.matrix(weights) %*% as.matrix(popcounts))


