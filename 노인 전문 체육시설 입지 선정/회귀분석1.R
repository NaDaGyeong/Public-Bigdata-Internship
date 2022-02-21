rm(list=ls())
data <- read.csv('C:/Users/user/bigdata/변수_final.csv')

# summary
head(data)
str(data)
summary(data)
par(mfrow=c(2,2))
plot(노인시설 ~ 공공체육시설.개수, data=data)
plot(노인시설 ~ 병원, data=data)
plot(노인시설 ~ 상권, data=data)
plot(노인시설 ~ 도시공원, data=data)
plot(노인시설 ~ 버스정류장, data=data)
plot(노인시설 ~ 공공기관, data=data)
plot(노인시설 ~ 총.인구, data=data)
attach(data)

# model fitting
m1 <- lm(노인시설 ~ 공공체육시설.개수 + 병원 + 상권 + 도시공원 + 버스정류장 + 공공기관 + 총.인구)
summary(m1)

# model selection
data1 <- data[,2:13]
cor(data1,method='pearson')

m2 <- lm(노인시설 ~ 병원 + 상권 + 도시공원 + 버스정류장 + 공공기관 + 총.인구) # 공공체육시설은 뺌
summary(m2)

fit3 <- step(m2, direction = "both")
smfit3 <- summary(fit3)
smfit3


# model diagnostics

attach(data)
n <- nrow(data)
X <- model.matrix(fit3)
p <- ncol(X)
y.hat <- predict(fit3)
res <- fit3$residuals
sigma.hat <- smfit3$sigma
ri <- rstandard(fit3)
par(mfrow=c(2,2))
plot(fit3)
detach(data)

##############################################################
re_data = data[-c(135,143,74,43,72,53,77),]
attach(re_data)
l = log(re_data$노인시설)
l[is.infinite(l)] <- 0
l
m4 <- lm(l ~ 병원 + 상권 + 도시공원 + 버스정류장 + 공공기관 + 총.인구)
fit4 <- step(m4, direction = "both")
smfit4 <- summary(fit4)
smfit4

n <- nrow(re_data)
X <- model.matrix(fit4)
p <- ncol(X)
y.hat <- predict(fit4)
res <- fit4$residuals
sigma.hat <- smfit4$sigma
ri <- rstandard(fit4)
par(mfrow=c(2,2)) 
plot(fit4)

shapiro.test(fit4$residuals) 
hist(res)
detach(re_data)
#########################################################################

re_data = data[-c(135,143,74,43,72,53,77),]

attach(re_data)
r = sqrt(re_data$노인시설)
r

m5 <- lm(r ~ 병원 + 상권 + 도시공원 + 버스정류장 + 공공기관 + 총.인구,data = re_data)
fit5 <- step(m5, direction = "both")
smfit5 <- summary(fit5)
smfit5

n <- nrow(data)
X <- model.matrix(fit5)
p <- ncol(X)
y.hat <- predict(fit5)
res <- fit5$residuals
sigma.hat <- smfit5$sigma
ri <- rstandard(fit5)
par(mfrow=c(2,2))
plot(fit5)

shapiro.test(fit5$residuals)

detach(re_data)


#########################################################################
install.packages("lmtest",repos = "http://cran.us.r-project.org")
library(lmtest)
dwtest(fit5, alternative="greater") # 오차항 독립성 O

pii <- influence(fit5)$hat
Ci <- cooks.distance(fit5)
dfits <- dffits(fit5)
abs(dfits)>=2*sqrt(p/(n-p)) # 81,92,114,145,150,172,187,189

pbar <- p/n
lev.ind <- pii>2*pbar
lev.ind # 7,11,15,21,44,45,56,92,110,114,145,146,148,150,154,187,189


infl.ind <- 1:n %in% order(Ci, decreasing=T)[1:2]
names(infl.ind) <- names(lev.ind)
infl.ind #7

#######################################################################
re_data1 = data[-c(7,135,143,74,43,72,53,77),]

attach(re_data1)
r1 = sqrt(re_data1$노인시설)
r1


newfit <- lm(r1 ~ 병원 + 상권 + 도시공원 + 버스정류장 + 공공기관 + 총.인구,data = re_data1) # 이상치 7 제거
fit_final <- step(newfit, direction = "both")
smfit_final <- summary(fit_final)
smfit_final



n <- nrow(re_data)
X <- model.matrix(newfit)
p <- ncol(X)
y.hat <- predict(newfit)
res <- newfit$residuals
sigma.hat <- newfit$sigma
ri <- rstandard(newfit)
par(mfrow=c(2,2))
plot(newfit)

shapiro.test(fit5$residuals)

detach(re_data1)
#############################################3


install.packages("lm.beta") # 표준화 베타 계수
library(lm.beta)
lm.beta(fit5)

install.packages('car') # 다중공선성 vif
library(car)
vif(fit5) # 10미만이므로 다중공선성 x

confint(fit5, level=0.95)
