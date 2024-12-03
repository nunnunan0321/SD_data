# XML 설치 및 로드
#install.packages("XML")
library(XML)
library(ggmap)

#api_url을 통해 웹에 접속하여 XML 파일 다운로드 및 파싱
url <- "http://openapi.seoul.go.kr:8088"
key <- "6a4a617a526e756e38306c6970484e"
type <- "xml"

start_index <- "1"
end_index <- "1000"

#성동구 문화 축제 데이터
SD_service <- "SDListPublicReservationCulture"  

#서울시 문화 축제 데이터
seoul_service <- "culturalEventInfo"  

#메인주소(url)+유저 key값+출력형식(type)+서비스명(service)+시작index+끝index
#http://openapi.seoul.go.kr:8088/key/xml/SDListPublicReservationCulture/1/5/

#paste 함수로 전체 주소 저장
SD_api_url <- paste(url,key,type,SD_service,start_Index,end_index,sep = "/")  #성동구 데이터
seoul_api_url <- paste(url,key,type,seoul_service,start_Index,end_index,sep = "/")  #서울시 데이터

#xmlParse함수는 인수를 xml 문서로 분석, xml 값 반환
SD_xml <- xmlParse(SD_api_url)  #성동구데이터
seoul_xml <- xmlParse(seoul_api_url)  #서울시데이터

#<row>태그 데이터 추출
SD_row <- getNodeSet(SD_xml,"//row")
seoul_row <- getNodeSet(seoul_xml,"//row")

#row 데이터를 dataFrame으로 변경
SD_api_data <- xmlToDataFrame(SD_row,stringsAsFactors = F)
seoul_api_data <- xmlToDataFrame(seoul_row,stringsAsFactors = F)


#진행중인 성동구 데이터 저장
accept_SD_api <- subset(SD_api_data,SVCSTATNM=="접수중",select = c('SVCNM','PLACENM'))
accept_seoul_api <- subset(seoul_api_data,GUNAME=="성동구",select = c("TITLE","PLACE"))


#장소명 저장
SD_place_name <- accept_SD_api["PLACENM"]
seoul_place_name <- accept_seoul_api["PLACE"]

SD_place_name_un<- unlist(SD_place_name,use.names = F)
seoul_place_name_un <- unlist(seoul_place_name,use.names = F)

place_result <- c(SD_place_name_un,seoul_place_name_un)

#list_place<- SD_place_name <- seoul_place_name
#unique(list_place)


#프로그램명 저장
SD_pro_name <- accept_SD_api["SVCNM"]   #list형태 프로그램 이름 저장
seoul_pro_name <- accept_seoul_api["TITLE"]

SD_pro_name_un <- unlist(SD_pro_name,use.names = F)
seoul_pro_name_un <- unlist(seoul_pro_name,use.names = F)

SD_pro_name[(nrow(SD_pro_name)+1),]=seoul_pro_name
pro_name_list <- SD_pro_name


#지도api
register_google(key='AIzaSyCLM-YtFvRHfgiEeE9kmtQc_KgUlPan6s8')


#검색 안 되는 장소값 바꿔주기
place_result[which(place_result=="매봉산숲속도서관 프로그램실")] <- "매봉산숲속도서관"
place_result[which(place_result=="언더스탠드 에비뉴 아트스탠드")] <- "언더스탠드애비뉴"

gc <- geocode(enc2utf8(place_result)) #xy값
gc <- na.omit(gc)   #결측치 제거

#SD.name 지도 중심으로
SD.name <- "응봉체육공원"
sdg <- geocode(enc2utf8(SD.name))    
cen <- colMeans(as.matrix(sdg))   # 경도위도를 숫자로 

map <- get_googlemap(center=cen,
                     size=c(640,640),
                     zoom=14,
                     maptype='roadmap')
gmap<-ggmap(map)      
gmap # 지도 화면에 보이기

#산도포
gmap+geom_point(data = data.frame(gc),
                aes(color=rainbow(nrow(gc))),
                size = 10,alpha=0.2) + theme(legend.position = "none")



place_unique<- unique(as.list(place_result))
length(place_result)
place_unique
