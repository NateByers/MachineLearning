library(diagram)
par(mar = c(1, 1, 1, 1))
openplotmat()

#########################################################################################
# first argument of coordinates() specifies the number of elements in each row
elpos <- coordinates (c(1, rep(2, 4)))
# create data.frame say from what position to where
from <- rep(1:7, each = 2)
to <- c(2:3, rep(4:5, 2), rep(6:7, 2), rep(8:9, 2))
arrpos <- matrix(ncol = 2, nrow = length(from))

# draw arrows
for(i in 1:length(from)){
  arrpos[i, ] <- straightarrow(to = elpos[to[i], ], from = elpos[from[i], ],
                lwd = 2, arr.pos = 0.6, arr.length = 0.5, endhead = FALSE)
}

# draw circles
flip <- rep(c("T", "H"), each = 4)
pi <- rep(c("B", "F"), 4)
textellipse(elpos[1,], .05, lab = "start", shadow.size = 0)
for(i in 1:(length(flip))) {
  textellipse(elpos[i + 1,], 0.05, lab = paste(flip[i], pi[i], sep = ", "))
}

#############Label with probabilities
par(mar = c(1, 1, 1, 1))
openplotmat()

# draw arrows
for(i in 1:length(from)){
  straightarrow(to = elpos[to[i], ], from = elpos[from[i], ],
                               lwd = 2, arr.pos = 0.6, arr.length = 0.5, endhead = FALSE)
}

# draw circles
prob <- sapply(1:length(flip), function(i){
  state <- paste(flip[i], pi[i])
  if(state == "T B"){
    .25
  }else if(state == "H B"){
    .75
  }else{
    .5
  }
})
textellipse(elpos[1,], .05, lab = "start", shadow.size = 0)
for(i in 1:(length(flip))) {
  textellipse(elpos[i + 1,], 0.05, lab = prob[i])
}

pi_prob <- rep(c(.7, .3, .3, .7), 3)

# draw transition probabilities
for(i in 1:12){
  if((i %% 2) == 0){
    text(arrpos[i + 2, 1] - 0.05, arrpos[i + 2, 2], pi_prob[i])
  }else{
    text(arrpos[i + 2, 1] + 0.05, arrpos[i + 2, 2], pi_prob[i])
  }
  
}

# draw heaviest edge
openplotmat()
edge <- c(2, 6, 9, 11)
# draw arrows
for(i in 1:length(from)){
  if(i %in% edge){col <- "red"}else{col <- "black"}
  straightarrow(to = elpos[to[i], ], from = elpos[from[i], ], lcol = col,
                lwd = 2, arr.pos = 0.6, arr.length = 0.5, endhead = FALSE)
}

textellipse(elpos[1,], .05, lab = "start", shadow.size = 0)
for(i in 1:(length(flip))) {
  textellipse(elpos[i + 1,], 0.05, lab = paste(flip[i], pi[i], sep = ", "))
}





#########################################################################################


elpos <- coordinates (c(1, 1, 2, 4))
fromto <- matrix(ncol = 2, byrow = TRUE,
                    data = c(1, 2, 2, 3, 2, 4, 4, 7, 4, 8))
nr <- nrow(fromto)
arrpos <- matrix(ncol = 2, nrow = nr)
for (i in 1:nr)
  # i = 2
   arrpos[i, ] <- straightarrow (to = elpos[fromto[i, 2], ],
                                   from = elpos[fromto[i, 1], ],
                                   lwd = 2, arr.pos = 0.6, arr.length = 0.5)
 textellipse(elpos[1,], 0.1, lab = "start", box.col = "green",
               shadow.col = "darkgreen", shadow.size = 0.005, cex = 1.5)
 textrect (elpos[2,], 0.15, 0.05,lab = "found term?", box.col = "blue",
             shadow.col = "darkblue", shadow.size = 0.005, cex = 1.5)
 textrect (elpos[4,], 0.15, 0.05,lab = "related?", box.col = "blue",
            shadow.col = "darkblue", shadow.size = 0.005, cex = 1.5)
 textellipse(elpos[3,], 0.1, 0.1, lab = c("other","term"), box.col = "orange",
               shadow.col = "red", shadow.size = 0.005, cex = 1.5)
 textellipse(elpos[3,], 0.1, 0.1, lab = c("other","term"), box.col = "orange",
               shadow.col = "red", shadow.size = 0.005, cex = 1.5)
 textellipse(elpos[7,], 0.1, 0.1, lab = c("make","a link"),box.col = "orange",
               shadow.col = "red", shadow.size = 0.005, cex = 1.5)
 textellipse(elpos[8,], 0.1, 0.1, lab = c("new","article"),box.col = "orange",
               shadow.col = "red", shadow.size = 0.005, cex = 1.5)

   dd <- c(0.0, 0.025)
 text(arrpos[2, 1] + 0.05, arrpos[2, 2], "yes")
 text(arrpos[3, 1] - 0.05, arrpos[3, 2], "no")
 text(arrpos[4, 1] + 0.05, arrpos[4, 2] + 0.05, "yes")
 text(arrpos[5, 1] - 0.05, arrpos[5, 2] + 0.05, "no")
 #
  
 rx <- 0.1
 ry <- 0.05
 pos <- coordinates(c(1, 1, 1, 1, 1, 1, 1,1 ), mx = -0.2)
 textdiamond(mid = pos[1,], radx = rx, rady = ry, lab = LETTERS[1],
               cex = 2, shadow.col = "lightblue")
 textellipse(mid = pos[2,], radx = rx, rady = ry, lab = LETTERS[2],
               cex = 2, shadow.col = "blue")
 texthexa(mid = pos[3,], radx = rx, rady = ry, lab = LETTERS[3],
            cex = 2, shadow.col = "darkblue")
 textmulti(mid = pos[4,], nr = 7, radx = rx, rady = ry, lab = LETTERS[4],
             cex = 2, shadow.col = "red")
 textrect(mid = pos[5,], radx = rx, rady = ry, lab = LETTERS[5],
            cex = 2, shadow.col = "darkred")
 textround(mid = pos[6,], radx = rx, rady = ry, lab = LETTERS[6],
             cex = 2, shadow.col = "black")
 textparallel(mid = pos[7,], radx = rx, rady = ry, lab = LETTERS[7],
                cex = 2, theta = 40, shadow.col = "black")
 textempty(mid = pos[8,], lab = LETTERS[8], cex = 2, box.col = "yellow")
 pos[ ,1] <- pos[ ,1] + 0.5
 text(pos[ ,1],pos[ ,2], c("textdiamond", "textellipse", "texthexa",
                             "textmulti", "textrect", "textround",
                             "textparallel", "textempty"))


##################################################################################################

rm(X)
#############you must install the package HMM
#### dishonestCasino()
require(HMM)
nSim = 2000
States = c("Fair", "Unfair")
Symbols = 1:6
transProbs = matrix(c(0.99, 0.01, 0.02, 0.98), c(length(States),
                                                 length(States)), byrow = TRUE)
emissionProbs = matrix(c(rep(1/6, 6), c(rep(0.1, 5), 0.5)),
                       c(length(States), length(Symbols)), byrow = TRUE)
hmm = initHMM(States, Symbols, transProbs = transProbs, emissionProbs = emissionProbs)
sim = simHMM(hmm, nSim)
vit = viterbi(hmm, sim$observation)
f = forward(hmm, sim$observation)
b = backward(hmm, sim$observation)
i <- f[1, nSim]
j <- f[2, nSim]
probObservations = (i + log(1 + exp(j - i)))
posterior = exp((f + b) - probObservations)
x = list(hmm = hmm, sim = sim, vit = vit, posterior = posterior)
##Plotting simulated throws at top
mn = "Fair and unfair die"
xlb = "Throw nr."
ylb = ""

plot(x$sim$observation, ylim = c(-7.5, 6), pch = 3, main = mn,
     xlab = xlb, ylab = ylb, bty = "n", yaxt = "n")
axis(2, at = 1:6)
#######Simulated, which die was used (truth)####################
text(0, -1.2, adj = 0, cex = 0.8, col = "black", "True: green = fair die")
for (i in 1:nSim) {
  if (x$sim$states[i] == "Fair")
    rect(i, -1, i + 1, 0, col = "green", border = NA)
  else rect(i, -1, i + 1, 0, col = "red", border = NA)
}
########Most probable path (viterbi)#######################
text(0, -3.2, adj = 0, cex = 0.8, col = "black", "Most probable path")
for (i in 1:nSim) {
  if (x$vit[i] == "Fair")
    rect(i, -3, i + 1, -2, col = "green", border = NA)
  else rect(i, -3, i + 1, -2, col = "red", border = NA)
}
##################Differences:
text(0, -5.2, adj = 0, cex = 0.8, col = "black", "Difference")
differing = !(x$sim$states == x$vit)
for (i in 1:nSim) {
  if (differing[i])
    rect(i, -5, i + 1, -4, col = rgb(0.3, 0.3, 0.3),
         border = NA)
  else rect(i, -5, i + 1, -4, col = rgb(0.9, 0.9, 0.9),
            border = NA)
}

#################Posterior-probability:#########################
points(x$posterior[2, ] - 3, type = "l")
###############Difference with classification by posterior-probability:############
text(0, -7.2, adj = 0, cex = 0.8, col = "black", "Difference by posterior-probability")
differing = !(x$sim$states == x$vit)
for (i in 1:nSim) {
  if (posterior[1, i] > 0.5) {
    if (x$sim$states[i] == "Fair")
      rect(i, -7, i + 1, -6, col = rgb(0.9, 0.9, 0.9),
           border = NA)
    else rect(i, -7, i + 1, -6, col = rgb(0.3, 0.3, 0.3),
              border = NA)
  }
  else {
    if (x$sim$states[i] == "Unfair")
      rect(i, -7, i + 1, -6, col = rgb(0.9, 0.9, 0.9),
           border = NA)
    else rect(i, -7, i + 1, -6, col = rgb(0.3, 0.3, 0.3),
              border = NA)
  }
}


# Initialise HMM
hmm = initHMM(c("A","B"), c("L","R"), transProbs=matrix(c(.6,.4,.4,.6),2),
              emissionProbs=matrix(c(.6,.4,.4,.6),2))
print(hmm)
# Sequence of observations
observations = c("L","L","R","R")
# Calculate Viterbi path
viterbi = viterbi(hmm,observations)
print(viterbi)



#########################################################


trans <- matrix(c())

prcomp(USArrests)  # inappropriate
prcomp(USArrests, scale = TRUE)
prcomp(~ Murder + Assault + Rape, data = USArrests, scale = TRUE)
plot(prcomp(USArrests))
summary(prcomp(USArrests, scale = TRUE))
biplot(prcomp(USArrests, scale = TRUE))


