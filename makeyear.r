makeyear <- function(season){
#data.file <- paste("all", season, ".csv", sep="")
data.file <- paste0("all", season, ".csv")
data <- read.csv(data.file, header=FALSE)
#data <- read.csv(paste0("./download.folder/unzipped/",data.file), header=FALSE)
#fields <- read.csv("http://www-math.bgsu.edu/~albert/baseball/fields.csv")
#fields <- read.csv("/home/jaosborn/research/sports/mlb/fields.csv")
fields <- read.csv("fields.csv")
names(data) <- fields[, "Header"]
data}
