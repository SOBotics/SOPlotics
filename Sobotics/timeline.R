library(timevis)

data <- read.csv('Moderation_Bot_Data.csv')
data[,2:3] <- format(as.Date(as.matrix(data[,2:3]), " %b %d %Y"),"%Y-%m-%d")

timevis_data <- data.frame(
id      = 1:nrow(data),
content = data$Bot,
start   = c(data$Introduced)
)

timevis(timevis_data)