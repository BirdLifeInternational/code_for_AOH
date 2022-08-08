# calculate all possible combinations of files and decide how to treat them when creating
# the resident, breeding and non-breeding AOH maps
b <- t(combn(1:5, 2))
res <- NULL
for(i in 1:nrow(b)){
  res[i] <- paste0(b[i,1], b[i,2])
}

b <- t(combn(1:5, 3))
res3 <- NULL
for(i in 1:nrow(b)){
  res3[i] <- paste0(b[i,1], b[i,2], b[i, 3])
}


b <- t(combn(1:5, 4))
res4 <- NULL
for(i in 1:nrow(b)){
  res4[i] <- paste0(b[i,1], b[i,2], b[i, 3], b[i, 4])
}

b <- t(combn(1:5, 5))
res5 <- NULL
for(i in 1:nrow(b)){
  res5[i] <- paste0(b[i,1], b[i,2], b[i, 3], b[i, 4], b[i, 5])
}
seas <- data.frame(combinations = c(1:5, res, res3, res4, res5))
seas$resident <- seas$combinations
seas$breeding <- seas$non_breeding <- NULL
seas$comb_without_4 <- str_remove(seas$combinations, "4")
seas$breeding <- str_remove(seas$comb_without_4, "3")
seas$non_breeding <- str_remove(seas$comb_without_4, "2")
seas$validation_migrant <- seas$comb_without_4 
seas <- seas[, c("combinations", "resident", "breeding", "non_breeding", "validation_migrant")]
rm(b, i, res, res3, res4, res5)
