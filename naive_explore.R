n1 <- read.csv("./data/markus/naive1_full_preds.csv", stringsAsFactors = F)
n2 <- read.csv("./data/markus/naive2_full_preds.csv", stringsAsFactors = F)

load("./data/naive_forecast.RData")


eq_markus <- sapply(1:nrow(n1), function(i) {
  all(n1[i, ] == n2[i, ], na.rm = T)
})
sum(eq_markus)


eq_naive1 <- sapply(1:nrow(n1), function(i) {
  all(n1[i, ] == naive_forecast$Naive[i, ], na.rm = T)
})
sum(eq_naive1)


eq_naive2 <- sapply(1:nrow(n1), function(i) {
  all(n2[i, ] == naive_forecast$Naive2[i, ], na.rm = T)
})
sum(eq_naive2)
