#Birthday simulation
#What is the probability NONE of the N people in a room sharing a birthday

N <- 0
samples <- 0;
prob.nodub <-0;

for(j in 10:100){
    counting.no.dups <- 0;
    test <- for(i in 1:1000){
    bdays <- sample(seq(as.Date('1990/01/01'), as.Date('1990/12/31'), by="day"), N, replace=T)
    dups <- duplicated(bdays, incomparables = FALSE)
    ch <-length(which(dups == TRUE))
    if(ch==0){
      counting.no.dups <- counting.no.dups +1
    }
    fine <- (counting.no.dups/1000)
    }
 print(N)
 print(fine)
 N <- N + 1
 samples[[j]] <- N
 prob.nodub[[j]] <- fine

}

plot(samples,prob.nodub, main="Birthday Simulation", xlab = "Sample Size", ylab = "Probability of no Duplicates", las =1)


