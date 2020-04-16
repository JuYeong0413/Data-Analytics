# nrow : 2행 나누기
m1 <- matrix(c('a', 'b', 'c', 'd'), nrow = 2)
m1

# ncol : 2열 나누기
m2 <- matrix(c('a', 'b', 'c', 'd'), ncol = 2)
m2

# nrow : 3행 나누기
m3 <- matrix(c('a', 'b', 'c', 'd', 'e', 'f'), nrow = 3)
m3

# ncol : 3열 나누기
m4 <- matrix(c('a', 'b', 'c', 'd', 'e', 'f'), ncol = 3)
m4

m4[1,1] # m4변수 1행 1열 출력
m4[2,3] # m4변수 2행 3열 출력

m3[1,] # m3변수 1행 전체 출력
m3[,1] # m3변수 1열 전체 출력

# 지금의 변수들을 저장
save(m1, m2, m3, m4, file = "myData.RData")

# 데이터 변수 출력
load("myData.RData")

# R데이터를 CSV저장하고 불러오기
write.csv(people,
          file="first.csv")

# "first.csv" 불러오기
first <- read.csv(file = "first.csv")
first <- read.csv(file = "first.csv", stringsAsFactors = FALSE)
first <- first[,-1]

# read.csv의 header 비교
first <- read.csv(file = "first.csv", stringsAsFactors = FALSE,
                  header = TRUE)
first <- read.csv(file = "first.csv", stringsAsFactors = FALSE,
                  header = FALSE)
