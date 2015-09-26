myPLA17 <- function (s) {
        mydata <- read.table("q15.txt")
	update_cnt <- c(rep(0,s))
	for (j in 1:s) {
	        print(j)
	        w <- c(rep(0,5))
	        set.seed(j)
	        halt<-FALSE
	        while (!halt) {
        	        updated <- FALSE
        		for (i in sample(nrow(mydata))) {
        			x <- c(1,as.numeric(mydata[i,1:4]))
        			y <- mydata[i,5]
        			wx <- sign( as.numeric(w %*% x))
        			if ( wx == 0 ) wx <- -1
        			if ( wx != y ) {
        				w <- w + 0.5 * y * x
        				update_cnt[j] <- update_cnt[j] + 1
        				updated <- TRUE
        				break
        			}
        		}
        		if (!updated) halt<- TRUE
	        }
	        print(w)
	} 
	return(update_cnt)
}

