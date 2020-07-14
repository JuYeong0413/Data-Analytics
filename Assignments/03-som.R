##########################################
# 과목명 : 데이터 애널리틱스
# 과제명 : 3 – 자기조직화
# 이름 : 이주영
# 학번 : 2016111540
# 학과 : 경영학부                        
##########################################

# 과제 1 -  라이브러리 생성 및 데이터셋 생성 
library(kohonen)
data(wines)

# 과제 2 - somgrid
data_train_matrix <- as.matrix(scale(wines))
wine_grid <- somgrid(xdim=13, ydim=13, topo="hexagonal")

# 과제 3 - 1차원 som 생성
set.seed(31)
wine_model <- som(data_train_matrix,
                  grid=wine_grid,
                  rlen=1000,
                  alpha=c(0.3,0.01),
                  keep.data=TRUE)

# 과제 4 - plot 생성 
plot(wine_model, type="codes", main="이주영(2016111540)")

# 과제 5 - 군집 결정
wine_cluster <- cutree(hclust(dist(getCodes(wine_model))), 4)
