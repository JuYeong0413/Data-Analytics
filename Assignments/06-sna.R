##########################################
# 과목명 : 데이터 애널리틱스
# 과제명 : 6 – SNA
# 이름 : 이주영
# 학번 : 2016111540
# 학과 : 경영학부                        
##########################################
rm(list = ls()) # object remove
##########################################

####################################################
# (1) 아마존 데이터셋을 데이터 프레임으로 변환
####################################################

# 라이브러리 불러오기 
library(igraph)

# 아마존 데이터 세트 읽기(head/tail 작성하지 말 것)
amazon <- read.table("amazon.txt", header = FALSE)

# sn 데이터를 그래프 형식의 데이터 프레임으로 변환 
amazon.df <- graph.data.frame(amazon, directed = FALSE)

#####################################################
# (2) 아이템ID 777 에 대한 데이터 셋 연결
#####################################################

# ID 777번과 연결된 사용자들의 데이터셋 연결
am1 <- subset(amazon, amazon$V1==777)

# am1.df로 그래프 데이터셋 지정
am1.df <- graph.data.frame(am1, directed = FALSE)

####################################################
# (3) 시각화 출력
####################################################

# ID 777만의 아이템 연결망 출력
plot(am1.df)

# amazon.df에 대한 분포 시각화 출력
plot(degree(amazon.df), xlab="아이템 번호", ylab="연결 정도", type='h')

# 연결정도에 대한 분포
amazon.df.dist <- degree.distribution(amazon.df)
plot(amazon.df.dist, xlab="연결 정도", ylab="확률")

####################################################
# (4) 집중도 계산
####################################################

# Degree centralization 
degree(amazon.df, normalized = TRUE)
degree_tmax <- centralization.degree.tmax(amazon.df)
centralization.degree(amazon.df, normalized = FALSE)$centralization / degree_tmax

# closeness centralization 
closeness(amazon.df, normalized = TRUE)
closeness_tmax <- centralization.closeness.tmax(amazon.df)
centralization.closeness(amazon.df, normalized = FALSE)$centralization / closeness_tmax

# betweenness centralization
betweenness(amazon.df, normalized = TRUE)
betweenness_tmax <- centralization.betweenness.tmax(amazon.df)
centralization.betweenness(amazon.df, normalized = FALSE)$centralization / betweenness_tmax
