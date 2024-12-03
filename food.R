#음식점 csv
setwd("C:/R_WD/r_project")
food_csv <- read.csv("food_data.csv")

SD_food <- subset(food_csv,SIGNGU_NM=="성동구",select = c("FOOD_FCLTY_NM"))

data <- na.omit(SD_food)
data_table <- table(data)
data_table <- data_table[-1]

sum_data <- sum(data_table)
sum_data

for(i in 1:length(data_table)){
  data_per[i] <- (data_table[i]/sum_data)*100
}

pie(data_table,main="음식점 소비기반 트렌드",col = rainbow(length(data_table)),labels = (paste(round(data_per,1),"%")))

legend_text <- names(data_table)
legend(1.2,1.2,cex=0.6,legend_text,fill=rainbow(length(data_table)))

