X <- matrix(c(2, 1, 1, 1, 2, 1, 3, 3, 1, 1, 0, -1), ncol = 4)
m1hat <- matrix(c(2, 2), ncol = 1)
m2hat <- matrix(c(1, 0), ncol = 1)
M <- matrix(c(rep(m1hat, 3), m2hat), ncol = 4)

outerProduct <- function(i, Xy.matrix, m1, m2){
  if(Xy.matrix[3, i] == 1){m <- m1} else {m <- m2}
  v <- Xy.matrix[1:2, i] - m
  v %*% t(v)
}

outer.products <- sapply(1:dim(X)[2], outerProduct, Xy.matrix = X,
                         m1 = m1hat, m2 = m2hat)

onehat <- rep(1, 4)

Sw <- matrix(outer.products %*% onehat, ncol = 2)



Z <- X[-3, ] - M
A <- matrix(rep(Z[, 1], 2), ncol = 2)
for(i in 2:4){
  A <- cbind(A, cbind(Z[, i], Z[, i]))
}

B <- matrix(rep(t(Z)[1, ], 2), nrow = 2, byrow = TRUE)
for(i in 2:4){
  B <- cbind(B, rbind(t(Z)[i, ], t(Z)[i, ]))
}

V <- A * B
