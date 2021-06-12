library(abind)
results1 = NULL
for(name.dat in sapply(1:20, FUN = function(x) paste0("autocov_var1_n50_", x, ".RData"))){
  results1 = abind(results1, get(load(name.dat)), along = 1)
}

results2 = NULL
for(name.dat in sapply(1:20, FUN = function(x) paste0("makeup_autocov_var1_n100_", x, ".RData"))){
  results2 = abind(results2, get(load(name.dat)), along = 1)
}

dim(results1)
dim(results2)

results1[,,7,,,,] <- results2[,,1,,,,]

boxplot(results1[,1,,1,1,1,1])
boxplot(results1[,1,,1,2,1,1])

error = results1[,1,,1,4,1,1]
apply(error, MARGIN = 2, sum)/apply(error, MARGIN = 2, sum)[1]
save(results1, file = "autocov_var1_n100.rda")


load("autocov_var1_n100.rda")

error = results1[,1,,1,1,1,1]
round(apply(error, MARGIN = 2, sum)[c(2,4,5,6,7)]/apply(error, MARGIN = 2, sum)[1], digits = 2)

# Table
table1 <- rbind(
cbind(sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,1,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,1,1,1,x], MARGIN = 2, sum)[1], digits = 2)}),
      sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,2,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,1,2,1,x], MARGIN = 2, sum)[1], digits = 2)}),
      sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,3,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,1,3,1,x], MARGIN = 2, sum)[1], digits = 2)}),
      sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,4,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,1,4,1,x], MARGIN = 2, sum)[1], digits = 2)})),
cbind(sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,1,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,2,1,1,x], MARGIN = 2, sum)[1], digits = 2)}),
      sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,2,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,2,2,1,x], MARGIN = 2, sum)[1], digits = 2)}),
      sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,3,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,2,3,1,x], MARGIN = 2, sum)[1], digits = 2)}),
      sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,4,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,2,4,1,x], MARGIN = 2, sum)[1], digits = 2)})),
cbind(sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,1,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,3,1,1,x], MARGIN = 2, sum)[1], digits = 2)}),
      sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,2,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,3,2,1,x], MARGIN = 2, sum)[1], digits = 2)}),
      sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,3,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,3,3,1,x], MARGIN = 2, sum)[1], digits = 2)}),
      sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,4,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,3,4,1,x], MARGIN = 2, sum)[1], digits = 2)})))

table2 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,1,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,1,,1,1,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,2,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,1,,1,2,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,3,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,1,,1,3,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,4,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,1,,1,4,2,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,1,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,1,,2,1,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,2,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,1,,2,2,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,3,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,1,,2,3,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,4,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,1,,2,4,2,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,1,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,1,,3,1,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,2,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,1,,3,2,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,3,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,1,,3,3,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,4,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,1,,3,4,2,x], MARGIN = 2, sum)[1], digits = 2)})))

table3 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,1,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,1,1,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,2,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,1,2,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,3,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,1,3,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,4,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,1,4,3,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,1,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,2,1,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,2,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,2,2,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,3,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,2,3,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,4,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,2,4,3,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,1,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,3,1,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,2,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,3,2,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,3,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,3,3,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,4,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,1,,3,4,3,x], MARGIN = 2, sum)[1], digits = 2)})))
save(table1, table2, table3, file = "VAR1_n100_lag0.rda")


table4 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,1,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,1,1,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,2,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,1,2,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,3,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,1,3,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,4,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,1,4,1,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,1,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,2,1,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,2,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,2,2,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,3,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,2,3,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,4,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,2,4,1,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,1,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,3,1,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,2,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,3,2,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,3,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,3,3,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,4,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,3,4,1,x], MARGIN = 2, sum)[1], digits = 2)})))

table5 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,1,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,2,,1,1,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,2,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,2,,1,2,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,3,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,2,,1,3,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,4,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,2,,1,4,2,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,1,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,2,,2,1,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,2,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,2,,2,2,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,3,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,2,,2,3,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,4,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,2,,2,4,2,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,1,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,2,,3,1,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,2,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,2,,3,2,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,3,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,2,,3,3,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,4,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,2,,3,4,2,x], MARGIN = 2, sum)[1], digits = 2)})))

table6 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,1,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,1,1,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,2,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,1,2,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,3,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,1,3,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,4,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,1,4,3,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,1,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,2,1,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,2,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,2,2,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,3,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,2,3,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,4,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,2,4,3,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,1,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,3,1,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,2,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,3,2,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,3,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,3,3,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,4,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,2,,3,4,3,x], MARGIN = 2, sum)[1], digits = 2)})))
save(table4, table5, table6, file = "VAR1_n100_lag1.rda")


table7 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,1,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,1,1,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,2,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,1,2,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,3,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,1,3,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,4,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,1,4,1,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,1,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,2,1,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,2,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,2,2,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,3,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,2,3,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,4,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,2,4,1,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,1,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,3,1,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,2,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,3,2,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,3,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,3,3,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,4,1,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,3,4,1,x], MARGIN = 2, sum)[1], digits = 2)})))

table8 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,1,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,3,,1,1,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,2,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,3,,1,2,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,3,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,3,,1,3,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,4,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,3,,1,4,2,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,1,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,3,,2,1,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,2,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,3,,2,2,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,3,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,3,,2,3,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,4,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,3,,2,4,2,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,1,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,3,,3,1,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,2,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,3,,3,2,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,3,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,3,,3,3,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,4,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results1[,3,,3,4,2,x], MARGIN = 2, sum)[1], digits = 2)})))

table9 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,1,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,1,1,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,2,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,1,2,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,3,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,1,3,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,4,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,1,4,3,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,1,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,2,1,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,2,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,2,2,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,3,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,2,3,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,4,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,2,4,3,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,1,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,3,1,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,2,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,3,2,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,3,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,3,3,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,4,3,x], MARGIN = 2, sum)[c(4,5,6,7)]/apply(results1[,3,,3,4,3,x], MARGIN = 2, sum)[1], digits = 2)})))
save(table7, table8, table9, file = "VAR1_n100_lag2.rda")


# Table of absult error
table1 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,1,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,2,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,3,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,4,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,1,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,2,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,3,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,4,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,1,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,2,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,3,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,4,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)})))

table2 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,1,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,2,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,3,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,4,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,1,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,2,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,3,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,4,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,1,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,2,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,3,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,4,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)})))

table3 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,1,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,2,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,3,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,1,4,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,1,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,2,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,3,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,2,4,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,1,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,2,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,3,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,1,,3,4,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)})))
save(table1, table2, table3, file = "VAR1_n100_lag0_abs.rda")


table4 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,1,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,2,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,3,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,4,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,1,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,2,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,3,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,4,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,1,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,2,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,3,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,4,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)})))

table5 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,1,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,2,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,3,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,4,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,1,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,2,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,3,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,4,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,1,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,2,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,3,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,4,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)})))

table6 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,1,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,2,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,3,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,1,4,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,1,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,2,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,3,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,2,4,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,1,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,2,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,3,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,2,,3,4,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)})))
save(table4, table5, table6, file = "VAR1_n100_lag1_abs.rda")


table7 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,1,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,2,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,3,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,4,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,1,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,2,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,3,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,4,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,1,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,2,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,3,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,4,1,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)})))
table8 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,1,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,2,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,3,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,4,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,1,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,2,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,3,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,4,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,1,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,2,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,3,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,4,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)})))

table9 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,1,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,2,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,3,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,1,4,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,1,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,2,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,3,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,2,4,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,1,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,2,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,3,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results1[,3,,3,4,3,x], MARGIN = 2, mean)[c(1,4,5,6,7)], digits = 2)})))
save(table7, table8, table9, file = "VAR1_n100_lag2_abs.rda")



### AutoCov_Linear_Process
library(abind)
results = NULL
for(name.dat in sapply(1:20, FUN = function(x) paste0("autocov_LP_n100_", x, ".RData"))){
  results = abind(results, get(load(name.dat)), along = 1)
}
dim(results)


boxplot(results[,1,,1,1,1])
boxplot(results[,1,,1,2,1])

error = results[,1,,1,1,1]
apply(error, MARGIN = 2, sum)/apply(error, MARGIN = 2, sum)[1]
save(results, file = "autocov_LP_n100.rda")


load("autocov_LP_n100.rda")
# Table
table1 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results[,1,,1,1,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,1,,1,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,1,,1,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,1,,1,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,1,,1,3,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,1,,1,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,1,,1,4,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,1,,1,4,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results[,1,,2,1,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,1,,2,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,1,,2,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,1,,2,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,1,,2,3,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,1,,2,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,1,,2,4,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,1,,2,4,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results[,1,,3,1,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,1,,3,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,1,,3,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,1,,3,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,1,,3,3,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,1,,3,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,1,,3,4,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,1,,3,4,x], MARGIN = 2, sum)[1], digits = 2)})))

save(table1,file = "LP_n100_lag0.rda")


table2 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results[,2,,1,1,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,2,,1,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,2,,1,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,2,,1,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,2,,1,3,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,2,,1,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,2,,1,4,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,2,,1,4,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results[,2,,2,1,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,2,,2,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,2,,2,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,2,,2,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,2,,2,3,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,2,,2,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,2,,2,4,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,2,,2,4,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results[,2,,3,1,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,2,,3,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,2,,3,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,2,,3,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,2,,3,3,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,2,,3,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,2,,3,4,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,2,,3,4,x], MARGIN = 2, sum)[1], digits = 2)})))
save(table2, file = "LP_n100_lag1.rda")


table3 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results[,3,,1,1,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,3,,1,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,3,,1,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,3,,1,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,3,,1,3,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,3,,1,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,3,,1,4,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,3,,1,4,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results[,3,,2,1,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,3,,2,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,3,,2,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,3,,2,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,3,,2,3,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,3,,2,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,3,,2,4,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,3,,2,4,x], MARGIN = 2, sum)[1], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results[,3,,3,1,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,3,,3,1,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,3,,3,2,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,3,,3,2,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,3,,3,3,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,3,,3,3,x], MARGIN = 2, sum)[1], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,3,,3,4,x], MARGIN = 2, sum)[c(3,5,6,7)]/apply(results[,3,,3,4,x], MARGIN = 2, sum)[1], digits = 2)})))
save(table3, file = "LP_n100_lag2.rda")



# Table of absolute errors
table1 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results[,1,,1,1,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,1,,1,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,1,,1,3,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,1,,1,4,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results[,1,,2,1,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,1,,2,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,1,,2,3,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,1,,2,4,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results[,1,,3,1,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,1,,3,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,1,,3,3,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,1,,3,4,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)})))

save(table1,file = "LP_n100_lag0_abs.rda")


table2 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results[,2,,1,1,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,2,,1,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,2,,1,3,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,2,,1,4,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results[,2,,2,1,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,2,,2,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,2,,2,3,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,2,,2,4,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results[,2,,3,1,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,2,,3,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,2,,3,3,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,2,,3,4,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)})))
save(table2, file = "LP_n100_lag1_abs.rda")


table3 <- rbind(
  cbind(sapply(1:3, FUN = function(x){round(apply(results[,3,,1,1,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,3,,1,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,3,,1,3,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,3,,1,4,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results[,3,,2,1,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,3,,2,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,3,,2,3,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,3,,2,4,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)})),
  cbind(sapply(1:3, FUN = function(x){round(apply(results[,3,,3,1,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,3,,3,2,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,3,,3,3,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)}),
        sapply(1:3, FUN = function(x){round(apply(results[,3,,3,4,x], MARGIN = 2, mean)[c(1,3,5,6,7)], digits = 2)})))
save(table3, file = "LP_n100_lag2_abs.rda")
