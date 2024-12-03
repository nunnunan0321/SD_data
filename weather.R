# XML 설치 및 로드
#install.packages("XML")
#library(XML)

#http://apis.data.go.kr/1360000/VilageFcstInfoService_2.0/getUltraSrtFcst?serviceKey=인증키&numOfRows=10&pageNo=1&base_date=20210628&base_time=0630&nx=55&ny=127


#공공데이터 포털 key
key <- "QNJCgYEZ1AkFuQn%2FoHKSNF51%2BvF4knES1SPykk7WiPtfSS%2BvwSOzdDshFObBa1uge3sEtRvPXCeefYCX8%2BgjEA%3D%3D"

#메인주소
url <- "http://apis.data.go.kr/1360000/"

#서비스명(기상청 초단기예보 서비스)
service <- "VilageFcstInfoService_2.0/getUltraSrtFcst"

first_url <- paste(url,service,"?serviceKey=",key,sep = "")

#한 페이지 결과 수 
num_of_rows <- "100"
num_of_rows <- paste("numOfRows=",num_of_rows,sep = "")

#페이지 번호
pageNo <- "1"
pageNo <- paste("pageNo=",pageNo,sep = "")

#예보 발표 일자(YYYYMMDD)
base_date <- "20241122"
base_date <- paste("base_date=",base_date,sep = "")

#발표 시각(HH24MI)
base_time <- "0700"
start_time<- as.numeric(base_time)
base_time <- paste("base_time=",base_time,sep = "")

#예보지점 x좌표
nx <- "61"
nx <- paste("nx=", nx, sep = "")

#예보지점 y좌표
ny <- "127"
ny <- paste("ny=", ny, sep = "")

second_url <- paste("&", num_of_rows, pageNo, base_date, base_time, nx, ny, sep = "&")

#전체주소
weather_api_url <- paste(first_url, second_url, sep = "")

weather_xml <- xmlParse(weather_api_url)  #날씨데이터

#<item>태그 데이터 추출
weather_item <- getNodeSet(weather_xml, "//item")

#dataframe변환
weather_api_data <- xmlToDataFrame(weather_item,stringsAsFactors = F)

#하늘상태SKY,강수형태PTY,기온T1H,습도REH,풍속WSD
SKY_data <- subset(weather_api_data,category=="SKY")
PTY_data <- subset(weather_api_data,category=="PTY")
T1H_data <- subset(weather_api_data,category=="T1H")
REH_data <- subset(weather_api_data,category=="REH")
WSD_data <- subset(weather_api_data,category=="WSD")

#필요한 값만 저장(하늘,강수, 기온, 습도, 풍속)
weather_data <- rbind(SKY_data,PTY_data,T1H_data,REH_data,WSD_data)

#딕셔너리 타입으로 키에 따른 날씨 저장
SKY_dict <- list("1" = "맑음", "3" = "구름많음", "4" = "흐림")
PTY_dict <- list("0"="강수없음","1"="비","2"="비/눈","3"="눈","5"="빗방울","6"="빗방울눈날림(진눈깨비)","7"="눈날림")

#올림해서 start_fcstTime에 저장 ex. 730->800(7시반을8시로)
start_fcstTime <- ((start_time%/%100)+1)*100

#시작시간부터 1000(1시간)씩 더해줘서 예보 시간 모두 저장
fcstTime_list <- c(0,0,0,0,0,0)
for(i in 1:length(fcstTime_list)){
  fcstTime_list[i] <- start_fcstTime+(100*(i-1))
}

#예보시간 문자로 변환(ex.800->0800)
ch_fcstTime_list <- c("a","a","a","a","a","a")
for(i in 1:length(ch_fcstTime_list)){
  if(fcstTime_list[i]<1000){
    ch_fcstTime_list[i] <- as.character(fcstTime_list[i])
    ch_fcstTime_list[i] <- paste("0",ch_fcstTime_list[i],sep = "")
  }else{
    ch_fcstTime_list[i] <- as.character(fcstTime_list[i])
  }
}

print_time <- c("","","","","","")

#콘솔출력
for(i in 1:length(ch_fcstTime_list)){
  now_print <- subset(weather_data,fcstTime==ch_fcstTime_list[i],select = c('fcstTime','category','fcstValue'))
  
  
  #print(now_print)
  
  cat("시간: ")
  print_time[i] <- paste(substr(ch_fcstTime_list[i],1,2),substr(ch_fcstTime_list[i],3,4),sep = ":")
  print(print_time[i])
  
  dict_key<- subset(now_print,category=="SKY")["fcstValue"]
  dict_key <- as.character(dict_key)  
  cat("날씨: ")
  print(as.character(SKY_dict[dict_key]))
  
  dict_key<- subset(now_print,category=="PTY")["fcstValue"]
  dict_key <- as.character(dict_key)
  cat("강수형태: ")
  print(as.character(PTY_dict[dict_key]))
  
  temp<- subset(now_print,category=="T1H")["fcstValue"]
  cat("온도: ")
  print(paste(as.character(temp),"°C",sep = ""))

  hum<- subset(now_print,category=="REH")["fcstValue"]
  cat("습도: ")
  print(paste(as.character(hum),"%",sep = ""))
  
  wind <- subset(now_print,category=="WSD")["fcstValue"]
  cat("풍속: ")
  wind <- as.numeric(wind)
  if(wind<=3){
    cat("바람 약함\n")  
  }else if(wind<=8){
    cat("바람 약간 강함\n")
  }else if(wind<=13){
    cat("바람 강함\n")
  }else{
    cat("바람 많이 강함\n")
  }
  cat("------------------\n")
}



#선그래프
#시간에 따른 온도/습도

#온도그래프
temp_data_toTime <- T1H_data["fcstValue"]
temp_data_vector <- unlist(temp_data_toTime,use.names = F)

plot(temp_data_vector,axes=F,main="시간에 따른 기온 변화 예측",lwd=5,xlab="시간",ylab="기온(°C)",type="o",col="red")

axis(1,at=1:6,labels = print_time,las=1)
axis(2,las=1)

abline(h=c(-10:40),v=c(1:6),lty=2)


#습도
hum_data_toTime <- REH_data["fcstValue"]
hum_data_vector <- unlist(hum_data_toTime,use.names = F)

plot(hum_data_vector,axes=F,main="시간에 따른 습도 변화 예측",lwd=5,xlab="시간",ylab="습도(%)",type="o",col="blue")

axis(1,at=1:6,labels = print_time,las=1)
axis(2,las=1)

abline(h=c(0:100),v=c(1:6),lty=2)


