

#Random
sim <- function(){

	payout <- c(0, 0, 1, 3, 7, 14, 28, 49)
	cards <- c(c(1:24), 25 + c(1:24))
	#midpoint at 25

	#Can pick at most 7 so no reason to sample 12
	picked <- sample(cards, 7)

	k <- 1
	#Flip 7 cards
	for(i in 1:7){

		u <- runif(1)
		#Pick above or below at random
		if(((u > 0.5) * (picked[i] > 25) + (u < 0.5) * (picked[i] < 25)) == 0){break}
		#Keep going if win, otherwise stops at current value of k
		k <- k + 1

	}

	return(payout[k])

}


simulations <- replicate(10^6, sim())
mean(simulations) - 1



#Optimal
sim <- function(){

	payout <- c(0, 0, 1, 3, 7, 14, 28, 49)
	cards <- c(c(1:24), 25 + c(1:24))
	#midpoint at 25

	#Can pick at most 7 so no reason to sample 12
	picked <- sample(cards, 7)

	n_above <- 0
	n_below <- 0

	k <- 1
	#Flip 7 cards
	for(i in 1:7){

		if(n_above >= n_below){

			if(picked[i] < 25){break}
			n_below <- n_below + 1

		} else {

			if(picked[i] > 25){break}
			n_above <- n_above + 1

		}

		#Keep going if win, otherwise stops at current value of k
		k <- k + 1

	}

	return(payout[k])

}


simulations <- replicate(10^6, sim())
mean(simulations) - 1


#Closed form for optimal
payout <- c(0, 0, 1, 3, 7, 14, 28, 49)
p_win <- rep(0, 7)

#Card in deck minus the 7s
n <- 48 
#Cards above (or below, whatever)
m <- 24

#Draws
for(i in 1:7){

	p_win[i] <- m/n 
	n <- n - 1
	
	if(i %% 2 == 0){

		m <- m - 1

	}

}

p_lose <- 1 - p_win

#Probability of being alive at time t
#We are alive at time 0 
S <- c(1, cumprod(p_win))

#Probability of run ending at time t is
#P(alive) * P(dying)
p_end <- S 
p_end[1:7]  <- p_end[1:7] * p_lose

#Sanity check
sum(p_end) == 1

p_end %*% payout - 1
