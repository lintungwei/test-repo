myPLA18 <- function (s) {
        myTrainData <- read.table("q18train.txt")
        myTestData <- read.table("q18test.txt")

        Eout <- c(rep(0,s))        
	for (j in 1:s) {
	        print(j)
	        w <- c(0,0,0,0,0)
	        pocketEin <- 500  # pretent all fail
	        pocket_w <- w
	        set.seed(j)
	        updateCnt <- 0
	        while (updateCnt < 50) {
	                updated <- FALSE
	                for (i in sample(500)) {
	                        x <- c(1,as.numeric(myTrainData[i,1:4]))
	                        y <- myTrainData[i,5]
	                        wx <- sign( as.numeric(w %*% x))
	                        if ( wx == 0 ) wx <- -1
	                        if ( wx != y ) {
	                                w <- w + y * x
	                                updateCnt <- updateCnt + 1
	                                updated <- TRUE
	                                wErr <- countErr(w, myTrainData)
	                                if ( wErr < pocketEin) {
                                                pocket_w <- w
                                                pocketEin <- wErr
	                                }
	                                break
	                        }
	                }
	                if (!updated) break   # halt condition
	        }
	        Eout[j] <- countErr(pocket_w, myTestData)
	}
        return(Eout)
}

countErr <- function(w, data) {
        cnt <- 0
        for (i in 1:500) {
                x <- c(1, as.numeric(data[i,1:4]))
                y <- data[i,5]
                wx <- sign( as.numeric(w %*% x) )
                if ( wx == 0 ) wx <- -1
                if ( wx != y ) cnt<-cnt+1
        }
        return(cnt)
}
