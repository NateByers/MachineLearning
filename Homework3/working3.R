x <- c(1, 0, 1, 3, 4, 5)
y <- c(1, 1, 0, 3, 4, 4)
seed1 <- c(3, 3)
seed2 <- c(5, 4)

plot(x = x, y = y)
points(c(seed1[1], seed2[1]),
       c(seed1[2], seed2[2]),
       col = c("red", "blue"),
       pch = 19)

group.df <- data.frame(x, y, group = NA,
                       centr_x = NA, centr_y = NA)
group.df[group.df$x == 3 & 
           group.df$y == 3, 
         c("group", "centr_x", "centr_y")] <- c(1, 3, 3)

group.df[group.df$x == 5 & 
           group.df$y == 4,
         c("group", "centr_x", "centr_y")] <- c(2, 5, 4)



manDist <- function(x, y, centr_x, centr_y){
  abs(centr_x - x) + 
    abs(centr_y - y)
}

group.df$dist1 <- unlist(mapply(manDist, group.df$x,
                       group.df$y,
                       MoreArgs = list(
                         centr_x = seed1[1],
                         centr_y = seed1[2])))

group.df$dist2 <- unlist(mapply(manDist, group.df$x,
                                group.df$y,
                                MoreArgs = list(
                                  centr_x = seed2[1],
                                  centr_y = seed2[2])))

group.df$group <- apply(group.df, 1, function(row){
  if(row["dist1"] == 
       min(row[c("dist1", "dist2")])){
    1
  }else{2}
})

group.df$change <- NA



reGroup <- function(df){
  center_x1 <- mean(df[df$group == 1, "x"])
  center_y1 <- mean(df[df$group == 1, "y"])
  center_x2 <- mean(df[df$group == 2, "x"])
  center_y2 <- mean(df[df$group == 2, "y"])
  df[df$group == 1, "centr_x"] <- center_x1
  df[df$group == 1, "centr_y"] <- center_y1
  df[df$group == 2, "centr_x"] <- center_x2
  df[df$group == 2, "centr_y"] <- center_y2
  df$dist1 <- unlist(mapply(manDist, df$x,
                            df$y,
                            MoreArgs = list(
                              centr_x = center_x1,
                              centr_y = center_y1)))
  df$dist2 <- unlist(mapply(manDist, df$x,
                            df$y,
                            MoreArgs = list(
                              centr_x = center_x2,
                              centr_y = center_y2)))
  g.initial <- df$group
  g <- apply(df, 1, function(row){
    if(row["dist1"] != row["dist2"]){
      if(row["dist1"] == 
           min(row[c("dist1", "dist2")])){
        1
      }else{2}
    }else{0}
  })
  df$group <- g
  df$change <- sapply(1:length(g), function(i){
    !(g[i] == g.initial[i])
  })
  df
}

group.df2 <- reGroup(group.df)

plotGroups <- function(df){
  group.cols <- df$group
  group.cols[group.cols == 1] <- "red"
  group.cols[group.cols == 2] <- "blue"
  group.cols[group.cols == 0] <- "black"
  plot(df$x, df$y, pch = 19,
       col = group.cols)
}

plotGroups(group.df2)

group.df3 <- reGroup(group.df2)

plotGroups(group.df3)


###################################################################################
theta <- 0.1
calcA <- function(theta, x = 12000){
  (x * theta)/((1/4) + theta)
}
calcT <- function(theta, y = 13000){
  (((1/4) - theta) * y)/((3/4)-theta)
}
calcTheta <- function(a, t){
  a/(4 * ( t+ a))
}
  
em.df <- data.frame(n = 0, a = NA, t = NA, theta = theta)
theta.diff <- theta
while(theta.diff> 0.00001){
  n.last = em.df[nrow(em.df), "n"]
  theta.old = em.df[em.df$n == n.last, "theta"]
  a.new = calcA(theta.old)
  t.new = calcT(theta.old)
  theta.new = calcTheta(a.new, t.new)
  n.new = n.last + 1
  em.new.df = data.frame(n = n.new, a = a.new,
                          t = t.new, theta = theta.new)
  em.df <<- rbind(em.df, em.new.df) 
  theta.diff <<- abs(theta.new - theta.old)
  if(n.new > 1000) break
}

######################################################################################

age <- factor(c(0, 0, 1, 1, 1, 0)) # 0 = <25, 1 = >25
income <- factor(c(0, 0, 1, 0, 0, 1)) # 0 = >50K, 1 = <50K
gender <- factor(c(0, 1, 1, 1, 0, 0)) # 0 = M, 1 = F
risk <- factor(c(0, 0, 0, 1, 1, 0)) # 0 = High, 1 = Low



income.split <- split(risk, income)

sum(as.numeric(levels(income.split[["0"]]))[income.split[["0"]]])/length(income.split[["0"]])

sum(as.numeric(levels(income.split[["1"]]))[age.split[["1"]]])/length(income.split[["1"]])

calcEntropy <- function(split.factor, outcome){
  # split.factor = income
  # outcome = risk
  split.outcome <- split(outcome, split.factor)
  outcome.list <- sapply(split.outcome, function(x){
    sum(as.numeric(levels(x))[x])/length(x)
  }
  )
  sapply(outcome.list,
  function(y) {
    entropy = -y * log(y, base = 2)
  if(is.nan(entropy)){
    0
  }else{
    entropy
  }
  }
  )
}


####################################################################################################################
age <- factor(c(0, 0, 1, 1, 1, 0), labels = c("<25", ">25")) # 0 = <25, 1 = >25
income <- factor(c(0, 0, 1, 0, 0, 1), labels = c(">50k", "<50k")) # 0 = >50K, 1 = <50K
gender <- factor(c(0, 1, 1, 1, 0, 0), labels = c("M", "F")) # 0 = M, 1 = F
risk <- factor(c(0, 0, 0, 1, 1, 0), labels = c("High", "Low")) # 0 = High, 1 = Low

df <- data.frame(age, income, gender, risk)

library(dplyr)

calcEntropy <- function(split.factor, outcome){
  # split.factor = age
  # outcome = risk
  # get proportions
  proportions = sapply(split(split.factor, split.factor),
                       function(level, total){length(level)/total},
                       total = length(split.factor))
  if(identical(split.factor, outcome)){
    sum(sapply(proportions, function(x) -x * log(x, 2)))
  }else{
  # put in data frame, group by factor and outcome,
  # get total counts
  df = data.frame(split.factor, outcome, I = 1)
  df = group_by(df, split.factor, outcome)
  df.sum = summarize(df, count = sum(I))
  # get the levels of the split.factor and subset
  split.levels = levels(split.factor)
  df.split1 = filter(df.sum, split.factor == split.levels[1])
  # entropy of outcome after split, level 1
  entropy1 = sapply(df.split1$count, function(x, total) {x/total},
                    total = sum(df.split1$count))
  entropy1 = sum(sapply(entropy1, function(x) -x * log(x, 2)))
  # entropy of outcome after split, level 1
  df.split2 = filter(df.sum, split.factor == split.levels[2])
  entropy2 = sapply(df.split2$count, function(x, total) {x/total},
                    total = sum(df.split2$count))
  entropy2 = sum(sapply(entropy2, function(x) -x * log(x, 2)))
  # sum entropies proportionally
  proportions[[1]] * entropy1 + proportions[[2]] * entropy2
  }
}

calcEntropy(gender, risk)


entropy.list <- lapply(df, calcEntropy, outcome = df$risk)

information.gain <- entropy.list[[4]] - unlist(entropy.list[1:3]) 

df.age.split <- lapply(levels(age), 
                       function(i) df[df$age == i, 
                                      c("income", "gender", "risk")])

df.age.split


df2 <- df.age.split[[2]]

entropy.list2 <- lapply(df2, calcEntropy, 
                        outcome = df2$risk)

information.gain2 <- entropy.list[[3]] - unlist(entropy.list2[1:2])

df.income.split <- lapply(levels(income),
                          function(i) df2[df2$income == i,
                                          c("gender", "risk")])
