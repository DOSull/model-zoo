# Tallies number of calues greater than x in a vector and returns that as a vector of same length
cluster.size.list <- function(sizes)
{
 gte.sizes <- vector(length=length(sizes))
 for (i in 1:length(sizes))
 {
   gte.sizes[i] <- sum(sizes >= sizes[i])
 }
 gte.sizes
}  