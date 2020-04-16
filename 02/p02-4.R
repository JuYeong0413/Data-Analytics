usedcars <- read.csv("usedcars.csv", stringsAsFactors = FALSE)
usedcars
str(usedcars)

summary(usedcars$year)
summary(usedcars[c("price", "mileage")])

(36000 + 44000 + 56000) / 3
mean(c(36000, 44000, 56000))

median(c(3600, 44000, 56000))

range(usedcars$price)
diff(range(usedcars$price))

IQR(usedcars$price)

quantile(usedcars$price)

quantile(usedcars$price, probs = c(0.01, 0.99))

quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20))

boxplot(usedcars$price,
        main = "Boxplot of Used Car Prices",
        ylab="Price ($)")

boxplot(usedcars$mileage,
        main = "Boxplot of Used Car Mileage",
        ylab="Odometer (mi.)")

hist(usedcars$price,
     main = "Histogram of Used Car Prices",
     xlab="Price ($)")

hist(usedcars$mileage,
     main = "Histogram of Used Car Mileage",
     xlab="Odometer (mi.)")

var(usedcars$price)
sd(usedcars$price)
var(usedcars$mileage)
sd(usedcars$mileage)

table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

mt <- table(usedcars$model)
prop.table(mt)

rmt <- prop.table(mt)
rmt <- prop.table(mt) * 100
round(rmt, digits = 1)

plot(x = usedcars$mileage, y = usedcars$price,
     main = "Scatterplot of Price vs. Mileage",
     xlab = "Used Car Odometer (mi.)",
     ylab = "Used Car Price ($)")

install.packages("gmodels") # gmodels 패키지 설치
library(gmodels) # gmodels 불러오기

usedcars$conservative <- usedcars$color %in% c("Black", "Gray",
                                               "Silver", "White")
table(usedcars$conservative)

CrossTable(x = usedcars$model, y = usedcars$conservative)

getCircleArea <- function(r) {
  area = 3.14*r^2
  return(area)
}

getCircleArea(3)
getCircleArea(11)
