library(ggplot2)
print(getwd())
setwd("~/R")
series <- read.csv("series_data.csv", header= TRUE, sep = "," )
print("DATASET")
print(series)
print("Type of dataset")
print(class(series))
print(mode(series))
print(typeof(series))

obs = nrow(series)
var = ncol(series)
print("no of observations")
print(obs)
print("no of variables")
print(var)
attach(series)

print("Info about series data")
print(summary(series))


print(unique(series$Certificate))
print("Number of shows by Certificate")
agg_count <- aggregate(IMDB_Rating ~ (Certificate), series, function(x) which.max(table(x)))
print(agg_count[-1,])

pie(agg_count$IMDB_Rating, c("Rating Not Available", "12+", "13", "13+","15", "15+", "16", "16+", "18", "18+", "7","7+","A","All", "Not Rated", "PG","R","U","UA"), main ="Different Certificates by Count", col = c("purple", "red", "blue","yellow","pink","brown","orange","light blue","coral","maroon","grey","dark green","navy","orchid"))

barplot(agg_count[-1,]$IMDB_Rating,
        width = 1, names.arg = agg_count[-1,]$Certificate,
        horiz = FALSE, xlab = "Certificate", ylab = "Number of Web Series", axes = TRUE,
        col = c("pink", "purple"), ylim = c(0, 40)
)


print("Information about the IMBD ratings")
print(summary(series$IMDB_Rating))



print(" Average rating by Certificate")
agg_rating <- aggregate(IMDB_Rating ~ (Certificate), series, mean)
print(agg_rating[-1,])
barplot(agg_rating[-1,]$IMDB_Rating,
        width = 1, names.arg = agg_rating[-1,]$Certificate,
        horiz = FALSE, xlab = "Certificate", ylab = "Average IMDB Rating", axes = TRUE,
        col = c("pink", "purple"), ylim = c(0, 10)
)

print("Different range of Ratings")
n <- c()
print("Okayish")
n[1] <- length(series$IMDB_Rating[series$IMDB_Rating<7.0])
print(n[1])
print("One time watch")
n[2] <- length(series$IMDB_Rating[(series$IMDB_Rating >= 7.0) & (series$IMDB_Rating < 9.0)])
print(n[2])
print("Good series")
n[3] <- length(series$IMDB_Rating[series$IMDB_Rating >= 9.0])#print(n[3])
pie(n, c("okayish", "one time watch", "good series"), main = "Different range of ratings", col = c("purple", "red", "blue"))
