# 라이브러리 불러오기 
# tidyverse : 텍스트 전처리  
# rvest : html 크롤링
library(tidyverse)
library(rvest)

######## 크롤링할 HTML 코드  ####################

scrap_amazon <- function(ASIN, page_num) {
  
  url_reviews <- paste0("https://www.amazon.com/Echo-Dot-Heather-Gray-Auto/product-reviews/",
                        ASIN, "/?pageNumber=", page_num)
  doc <- read_html(url_reviews)
  
  # Review Date
  doc %>%
    html_nodes("[data-hook='review-date']")%>%
    html_text() -> Data
  
  # Review Title
  doc %>%
    html_nodes("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']")%>%
    html_text() -> Title
  
  # Review Text
  doc %>%
    html_nodes("[class='a-size-base review-text review-text-content']")%>%
    html_text() -> Review
  
  # Number of Stars in Review
  doc %>%
    html_nodes("[data-hook='review-star-rating']")%>%
    html_text() -> Rating
  
  # Return a tibble
  tibble(Data, Title, Review, Rating, Page = page_num)%>%
    return()
}


######## page별 크롤링 시작  ####################

# Product name = Echo-Dot-Heather-Gray-Auto
# ASIN = B082TJT44G
# ASIN : 아마존이 만든 10자리의 고유 식별 번호 
review_all <- vector("list", length = 10)


# 스크랩 시작
for (i in 1:10) {
  review_all[[i]] <- scrap_amazon(ASIN = "B082TJT44G", page_num = i)
}
  
# review_all 내용을 rbind를 이용하여 한줄씩 리뷰를 저장 
amazon <- do.call(rbind, review_all)

######## 텍스트 전처리 ####################

# Rating에서 ".0 out of 5 stars" 지우기 
# 점수만 확인 
amazon$Rating <- gsub(".0 out of 5 stars", "", amazon$Rating)


# Data에서 국가 및 날짜 나누기 
# “ on”과 “on ”을 기준으로 문장을 1,2로 나누기
# Country와 Date 생성    
amazon$Country <- strsplit(amazon$Data, " on")[[1]][1]
amazon$Country <- gsub("Reviewed in ", "", amazon$Country)
amazon$Date <- strsplit(amazon$Data, "on ")[[1]][2]

# Data 속성 지우기 
amazon <- amazon[,-1]


# Pattern을 이용하여 newline을 의미하는 “\n” 지우고, 
# 두 칸 white space 없애기
amazon$Title <- gsub(pattern="\\n", "", amazon$Title)
amazon$Review <- gsub(pattern="\\n", "", amazon$Review)
amazon$Title <- gsub("  ", "", amazon$Title)
amazon$Review <- gsub("  ", "", amazon$Review)

# 속성 중요도에 따라 reorder  
amazon <- amazon[,c(6, 5, 3, 1, 2, 4)]


### Write.csv ######################################

# CSV 저장하기 
write.csv(amazon, file = "Alexa_review.csv", row.names = FALSE)
