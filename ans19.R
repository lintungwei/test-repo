myPLA19 <- function (s) {
        myTrainData <- read.table("q18train.txt")
        myTestData <- read.table("q18test.txt")
        
        Eout <- c(rep(0,s))        
        for (j in 1:s) {
                print(j)
                w <- c(rep(0,5))
                pocketEin <- nrow(myTrainData)  # pretent all fail
                pocket_w <- w
                set.seed(j)
                updateCnt <- 0
                while (updateCnt < 50) {
                        updated <- FALSE
                        for (i in sample(nrow(myTrainData))) {
                                x <- c(1,as.numeric(myTrainData[i,1:4]))
                                y <- myTrainData[i,5]
                                wx <- sign( as.numeric(w %*% x))
                                if ( wx == 0 ) wx <- -1
                                if ( wx != y ) {
                                        w <- w + y * x
                                        updateCnt <- updateCnt + 1
                                        updated <- TRUE
                                        pocket_w <- w
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
        for (i in 1:nrow(data)) {
                x <- c(1, as.numeric(data[i,1:4]))
                y <- data[i,5]
                wx <- sign( as.numeric(w %*% x) )
                if ( wx == 0 ) wx <- -1
                if ( wx != y ) cnt<-cnt+1
        }
        return(cnt)
}
