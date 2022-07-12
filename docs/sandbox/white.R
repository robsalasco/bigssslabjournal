group <- c(1,1,2,2,3,4,4)
y <- c(12,3,4,7,2,2,1)
x <- c(3,2,2,3,1,1,2)

# distance matrix (needs to be a matrix object)
distance <- as.matrix(dist(data.frame(x,y), diag = TRUE,  upper = TRUE))

# mean distance
ptt <- mean(distance)
distance[group==1, group==unique(group)[i]]

# for each group a for loop
i = 1

vector_values <- rep(NA,length(unique(group)))
pi <- rep(NA,length(unique(group)))

for (i in 1:length(unique(group))) {
  subs <- unique(group)[i]
  m <- mean(distance[group==subs, group==subs])
  vector_values[i] <- m
  print(pi[i]) 
  pi[i] <- nrow(distance[group==subs, group==subs])
}

sum(vector_values*pi) / ptt

