##########################################
# 과목명 : 데이터 애널리틱스
# 과제명 : 2 – K군집분석
# 이름 : 이주영
# 학번 : 2016111540
# 학과 : 경영학부                        
##########################################

# 문제 1 : 데이터 읽기
teens <- read.csv("snsdata.csv")

# 문제 2 : interests 벡터 생성
interests <- teens[8:32]
interests_z <- as.data.frame(lapply(interests, scale))

# 문제 3 : kmeans를 이용한 벡터 생성
set.seed(79)
teen_clusters <- kmeans(interests_z, 6)

# 문제 4 : 데이터 확인
teens$cluster <- teen_clusters$cluster
teens[3:6,
      c("cluster", "dance", "music", "shopping")]






# 30줄 이상 작성하여 제출하는 경우, 감점이 들어갑니다.
