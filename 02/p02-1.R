# 벡터 만들기
x <- c(80, 85, 70)
x


## 산술연산자
# 더하기
2+3

# 빼기
10-3

# 곱하기
3*4

# 나누기
8/2

# 거듭제곱
2^3

# 나머지
10%%3

# 몫
10%/%3


## 비교연산자
x <- 3

# 작음
x < 10

# 이하
x <= 10

# 큼
x > 10

# 이상
x >= 10

# 같음
x == 10

# 같지않음
x != 10


## 벡터의 원소
x <- c(1, 2, 3, 4, 5)

# 두번째 원소
x[2]

# 1, 3, 5번째 원소
x[c(1, 3, 5)]

# 2, 4번째 원소
x[c(2, 4)]

# 원소의 값이 2보다 큰 값들만 출력
x[x > 2]

# 원소의 값이 2이상이고 4이하인 값들만 출력
x[x >= 2 & x <= 4]

# 2번째 원소의 값을 20으로 수정
x[2] <- 20

# 3, 4번째 원소들의 값을 모두 15로 수정
x[c(3, 4)] <- 15

# 15이하인 원소 값들을 모두 10으로 수정
x[x <= 15] <- 10


## 함수 이용 벡터 연산
x <- c(seq(1:10))

# x 벡터의 원소 값들의 평균
mean(x)

# x 벡터의 원소 값들의 분산
var(x)

# x 벡터의 원소 값들의 표준편차
sd(x)

# x 벡터의 원소 값들의 제곱근
sqrt(x)

# x 벡터의 원소 값들의 개수
length(X)

# x 벡터의 원소 값들의 절대값
abs(x)


## 배열
# 1차원 배열
# 1행 3열 내용물 " 1:3 "
x <- array(1:3, dim = c(3))
x

# 2차원 배열
# 2행 3열 내용물 " 1:6 "
x <- array(1:6, dim = c(2, 3))
x
View(x)

# 1행 3열 값
x[1,3]

# 3열의 모든 값들
x[,3]

# 3열을 제외한 모든 열의 값들
x[,-3]

# 1행 2열의 값을 20으로 수정
x[1,2] <- 20


## 행렬
x <- matrix(1:6, nrow = 2)
x


## 리스트
# '홍길동'을 표현하는 리스트
x <- list("홍길동",
          "2016001",
          20,
          c("IT융합", "데이터관리")
)
x

y <- list("성명" = "홍길동",
          "학번" = 2016001,
          "나이" = 20,
          "수강과목" = c("IT융합", "데이터관리")
      )
y


## 데이터 프레임 만들기
# 두 객체에 해당하는 데이터 프레임
x <- data.frame(성명 = c("홍길동", "손오공"),
                나이 = c(20, 30),
                주소 = c("서울", "부산")
     )
x

# 데이터 프레임에서 열(학과) 추가
x <- cbind(x,
           학과 = c("전산학", "경영학")
     )
x

# 데이터 프레임에서 행(장발장) 추가
x <- rbind(x,
           data.frame(성명 = c("장발장"),
                      나이 = c(25),
                      주소 = c("파리"),
                      학과 = c("전산학")
           )
     )
x

# 3행 2열 값
x[3,2]

# 3행의 모든 값들
x[3,]

# 3열의 모든 값들
x[,3]

# 2행을 제외한 모든 열의 값들
x[-2,]

# "성명" 요소
x[1]
x["성명"]

# "성명" 요소값
x$성명
x[["성명"]]
x[[1]]

# 1열 요소값에서 두번재 값
x[[1]][2]
