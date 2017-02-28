# Random Birthday Simulation
# What is the probability that NONE of the N people in a room shares a birthday?

N <- 0
samples <- 0;
prob.nodub <-0;

for(j in 10:100){
    counting.no.dups <- 0;
    test <- for(i in 1:1000){
    bdays <- sample(seq(as.Date('1990/01/01'), as.Date('1990/12/31'), by="day"), N, replace=T)
    dups <- duplicated(bdays, incomparables = FALSE) #returns duplicates
    ch <-length(which(dups == TRUE)) #returns number of duplicates
    if(ch==0){ #if there are no dupicates the variable is incremented by one
      counting.no.dups <- counting.no.dups +1
    }
    fine <- (counting.no.dups/1000) 
    }
 print(N)
 print(fine)
 N <- N + 1
 samples[[j]] <- N #variables are stored consecutively
 prob.nodub[[j]] <- fine

}

plot(samples,prob.nodub, main="Birthday Simulation", xlab = "Sample Size", ylab = "Probability of no Duplicates", las =1,ylim = 0:1)




