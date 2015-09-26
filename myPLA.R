myPLA <- function () {
        mydata <- read.table("q15.txt")
        w <- c(rep(0,5))
	update_cnt <- 0
	halt<-FALSE
	while (!halt) {
	        updated <- FALSE
		for (i in 1:nrow(mydata)) {
			x <- c(1,as.numeric(mydata[i,1:4]))
			y <- mydata[i,5]
			wx <- sign( as.numeric(w %*% x))
			if ( wx == 0 ) wx <- -1
			if ( wx != y ) {
				w <- w + y * x
				update_cnt <- update_cnt + 1
				message("update w ")
				print(w)
				updated <- TRUE
				break
			}
		}
		if (!updated) halt<- TRUE
	}
	return(update_cnt)
}

