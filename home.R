setwd("C:/R_WD/r_project")

#6월데이터
distance_csv_6 <- read.csv("20240629_distance.csv" , header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")

week_distance_avg_6 <- sum(distance_csv_6["week"])/nrow(distance_csv_6)
weekEnd_distance_avg_6 <- sum(distance_csv_6["weekend"])/nrow(distance_csv_6)

increase_per_6 <- (weekEnd_distance_avg_6-week_distance_avg_6)/week_distance_avg_6*100





#7월데이터
distance_csv_7 <- read.csv("20240729_distance.csv" , header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")

week_distance_avg_7 <- sum(distance_csv_7["week"])/nrow(distance_csv_7)
weekEnd_distance_avg_7 <- sum(distance_csv_7["weekend"])/nrow(distance_csv_7)

increase_per_7 <- (weekEnd_distance_avg_7-week_distance_avg_7)/week_distance_avg_7*100





#8월데이터
distance_csv_8 <- read.csv("20240829_distance.csv" , header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")

#names(distance_csv_8[1]) <- "week"

week_distance_avg_8 <- sum(distance_csv_8[1])/nrow(distance_csv_8)
weekEnd_distance_avg_8 <- sum(distance_csv_8["weekend"])/nrow(distance_csv_8)

increase_per_8 <- (weekEnd_distance_avg_8-week_distance_avg_8)/week_distance_avg_8*100





#9월데이터
distance_csv_9 <-read.csv("20240929_distance.csv" , header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")

#평균 값
week_distance_avg_9 <- sum(distance_csv_9["week"])/nrow(distance_csv_9)
weekEnd_distance_avg_9 <- sum(distance_csv_9["weekend"])/nrow(distance_csv_9)

increase_per_9 <- (weekEnd_distance_avg_9-week_distance_avg_9)/week_distance_avg_9*100



#출력
cat("평일 평균 이동 횟수 : ",week_distance_avg_6,"\n휴일 평균 이동 횟수: ",weekEnd_distance_avg_6,"\n",increase_per_6,"% 감소",sep = "")
cat("평일 평균 이동 횟수 : ",week_distance_avg_7,"\n휴일 평균 이동 횟수: ",weekEnd_distance_avg_7,"\n",increase_per_7,"% 감소",sep = "")
cat("평일 평균 이동 횟수 : ",week_distance_avg_8,"\n휴일 평균 이동 횟수: ",weekEnd_distance_avg_8,"\n",increase_per_8,"% 감소",sep = "")
cat("평일 평균 이동 횟수 : ",week_distance_avg_9,"\n휴일 평균 이동 횟수: ",weekEnd_distance_avg_9,"\n",increase_per_9,"% 감소",sep = "")



#월별 증감 result_avg에 저장
result_avg <- c(increase_per_6,increase_per_7,increase_per_8,increase_per_9)
names(result_avg) <- c(6,7,8,9)


#선그래프
plot(result_avg,main="월별 감소 비율",axes=F,lwd=5,xlab="월",ylab="평일/휴일 이동 횟수 감소 비율(%)",type="o",col="red")
axis(1, at=1:4, lab=names(result_avg),las=1)
axis(2,at=-65:-59,las=1)
abline(h=seq(-100, 0, by=0.5),v=c(0:10),lty=2)


