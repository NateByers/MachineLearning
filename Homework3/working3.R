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

myK_Cluster <- function(df){
  df2 <- reGroup(df)
  n <- 1
  while()
}
