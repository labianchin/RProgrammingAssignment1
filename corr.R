corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
 
	files <- lapply(list.files(directory, '*.csv'), function(f) sprintf("%s/%s", directory, f))
	mydata <- do.call("rbind", lapply(files, read.csv))

	compdata <- mydata[ complete.cases(mydata),  ]

	count_id <- function(id) {
		d <- subset(compdata, ID %in% id)
		nrow(d)
	}
	ids <- unique(compdata$ID)
	valid_ids <- Filter(function(id) count_id(id) >= threshold, ids)

	id_cor <- function(id) {
		d <- subset(compdata, ID %in% id)
		cor(d[, 'nitrate'], d[, 'sulfate'])		
	}
	cors <- lapply(valid_ids, id_cor)

	unlist(cors)

}
