# smoking_results

library(BCEA)
data(Smoking)

m <- bcea(eff, cost, ref = 4, Kmax = 500, interventions = treats)
ec = data.frame("e" = m$e, "c" = m$c)
ec = ec[matrix(1:ncol(ec), nrow = 2, byrow = T) |> c()]

ec |> write.csv(file = "./data/smoking_results.csv", row.names = FALSE)
