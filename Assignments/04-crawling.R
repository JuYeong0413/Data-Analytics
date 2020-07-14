##########################################
# 과목명 : 데이터 애널리틱스
# 과제명 : 4 – LSA
# 이름 : 이주영
# 학번 : 2016111540
# 학과 : 경영학부                        
##########################################



############################################################
############################################################
# (문제1) 아마존닷컴 상품평 크롤링 (2019년 이후 50건만 추출)
############################################################
############################################################
library(tidyverse)
library(rvest)

scrap_amazon <- function(ASIN, page_num) {
  
  url_reviews <- paste0("https://www.amazon.com/HP-Backlit-Monitor-T3M88AA-ABA/product-reviews/",
                        ASIN, "/?sortBy=recent&pageNumber=", page_num)
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

review_all <- vector("list", length = 10)

for (i in 1:5) {
  review_all[[i]] <- scrap_amazon(ASIN = "B01F6V704G", page_num = i)
}

amazon <- do.call(rbind, review_all)


amazon$Rating <- gsub(".0 out of 5 stars", "", amazon$Rating)

amazon$Country <- strsplit(amazon$Data, " on")[[1]][1]
amazon$Country <- gsub("Reviewed in ", "", amazon$Country)
amazon$Date <- strsplit(amazon$Data, "on ")[[1]][2]

amazon <- amazon[,-1]

amazon$Title <- gsub(pattern="\\n", "", amazon$Title)
amazon$Review <- gsub(pattern="\\n", "", amazon$Review)
amazon$Title <- gsub("  ", "", amazon$Title)
amazon$Review <- gsub("  ", "", amazon$Review)

amazon <- amazon[,c(6, 5, 3, 1, 2, 4)]

write.csv(amazon, file = "HW#4(이주영).csv", row.names = FALSE)

############################################################
############################################################ 
# (문제2) TDM 만들기
############################################################
############################################################
library(tm)
library(topicmodels)

# 텍스트 전처리 
monitor <- iconv(enc2utf8(amazon$Review), sub="bytes")
monitor <- gsub("[\U4E00-\U9FFF\U3000-\U303F]", "", monitor)
monitor <- iconv(monitor, from="latin1", to="ASCII", sub="")
corpus <- VCorpus(VectorSource(monitor))
Sys.setlocale(category="LC_ALL", locale = "us")

# TDM 생성 (TDM 생성 조건은 PDF 확인)
tdm <- TermDocumentMatrix(corpus,
                          control = list(removePunctuation = T,
                                         stopwords = "SMART",
                                         stripwhitespace = T,
                                         tolower = T,
                                         removeNumbers = T,
                                         wordLengths = c(3, 6),
                                         stemming = F,
                                         weighting = weightTfIdf))


############################################################
############################################################
# (문제3) LSA 생성
############################################################
############################################################
library(lsa)

# as.textmatrix 이름을 text_m 로 생성
text_m <- as.textmatrix(as.matrix(tdm))

# LSA 모델명을 lsa_m 으로 생성 (LSA의 dimension : 5차원)
lsa_m <- lsa(text_m, dim = 5)

# lsa_m$dk, lsa_m$tk, lsa_m$sk 생성
lsa_m$dk
lsa_m$tk
lsa_m$sk
