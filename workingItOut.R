X <- matrix(c(2, 1, 1, 1, 2, 1, 3, 3, 1, 1, 0, -1), ncol = 4)
m1hat <- matrix(c(2, 2), ncol = 1)
m2hat <- matrix(c(1, 0), ncol = 1)
M <- matrix(c(rep(m1hat, 3), m2hat), ncol = 4)

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
