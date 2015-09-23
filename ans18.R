myTrainData <- read.table("q18train.txt")
myTestData <- read.table("q18test.txt")

myPLA18 <- function (mySeed) {
	set.seed(mySeed)
	w <- c(rep(0,5))
	pocket_w <- w
	pocketEin <- 500
	updateCnt <- 0
	halt<-FALSE
	while (updateCnt < 50 && !halt) {
		myseq <- sample(1:500,500)
		for (i in myseq) {
			x <- c(1,myTrainData[i,1],myTrainData[i,2],myTrainData[i,3],myTrainData[i,4])
			y <- myTrainData[i,5]
			wx = sign( w %*% x)
			if ( wx == 0 ) wx <- -1
			if ( wx != y ) {
				updateCnt <- updateCnt + 1
				w <- w + y * x
				wEin <- countEin(w)
				if ( wEin < pocketEin ) {
					pocket_w <- w			# update pocket
					pocketEin <- wEin
					halt <- wEin==0
				}
				break
			}
		}
	}
	return(pocket_w)
}

countEin <- function( w ) {
	cnt <- 0
	for (i in 1:500) {
		x <- c(1,myTrainData[i,1],myTrainData[i,2],myTrainData[i,3],myTrainData[i,4])
		y <- myTrainData[i,5]
		wx = sign( w %*% x )
		if ( wx == 0 ) wx <- -1
		if ( wx != y ) cnt<-cnt+1
	}
	return(cnt)
}

countEval <- function( w ) {
	cnt <- 0
	for (i in 1:500) {
		x <- c(1,myTestData[i,1],myTestData[i,2],myTestData[i,3],myTestData[i,4])
		y <- myTestData[i,5]
		wx = sign( w %*% x )
		if ( wx == 0 ) wx <- -1
		if ( wx != y ) cnt<-cnt+1
	}
	return(cnt)
}

runN <-function( n ) {
	err_val <- vector("numeric",length=n)
	for (i in 1:n) {
		err_val[i] <- countEval(myPLA18(i))
		print(c(i, err_val[i]))
	}
	mean(err_val)/500
}