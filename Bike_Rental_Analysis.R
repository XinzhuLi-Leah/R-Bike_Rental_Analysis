setwd("/Users/lixinzhu/Desktop/RData/R-bike_rental")
bike_data <- read.csv("/Users/lixinzhu/Desktop/RData/R-bike_rental/hour.csv")
# 查看数据结构
head(bike_data)
#检查数据结构
str(bike_data)
summary(bike_data)
# 检查是否有缺失值
sum(is.na(bike_data))
# 将dteday转换为日期格式--因为他原本的类型是character
bike_data$dteday <- as.Date(bike_data$dteday)

#explorary data analysis

# 1- 按天汇总租赁数量
daily_data <- aggregate(cnt ~ dteday, data = bike_data, sum)
#底下这个使用管道操作 与上面的aggregate作用一致！
library(dplyr)
rental_cnt_by_day <- bike_data %>% group_by(dteday) %>% summarize(cnt_day= sum(cnt))
#绘制趋势图 trending analysis 
library(ggplot2)
ggplot(rental_cnt_by_day, aes(x=dteday, y=cnt_day,group=1))+
  geom_line()+
  labs(title = "Daily Bike Rentals", x = "Date", y = "Total Rentals")+
  theme_bw()

# 进行范围的缩大 与缩小
library(plotly)
p <- ggplot(rental_cnt_by_day, aes(x = dteday, y = cnt_day)) +
  geom_line() +
  labs(title = "Daily Bike Rentals", x = "Date", y = "Total Rentals")
ggplotly(p)


#双轴 趋势图  区分注册用户和临时用户
library(dplyr)
daily_data <- bike_data %>% group_by(dteday) %>% summarise(casual_total= sum(casual),registered_total=sum(registered))

ggplot(daily_data, aes(x = dteday)) +
  geom_line(aes(y = casual_total, color = "Casual 用户")) +
  geom_line(aes(y = registered_total, color = "Registered 用户")) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Registered 用户")) +
  labs(title = "用户行为趋势对比", x = "date", y = "count")



# 2- 上面分析了按照day,下面分析不同小时的租赁数量 
rental_cnt_by_hour <- bike_data %>% group_by(hr) %>% summarize(cnt_hr= sum(cnt))
ggplot(rental_cnt_by_hour,aes(x=hr, y=cnt_hr))+
  geom_bar(stat = "identity")+
  labs(title = "Bike Rentals by Hour", x = "Hour of Day", y = "Total Rentals")+
  theme_bw()


rental_cnt_by_hour <- bike_data %>%
  group_by(hr, season) %>%
  summarize(cnt_hr = sum(cnt))

ggplot(rental_cnt_by_hour, aes(x = hr, y = cnt_hr,fill = season)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ season, scales = "free", ncol = 4) +  
  labs(title = "Bike Rentals by Hour", x = "Hour of Day", y = "Total Rentals") +
  scale_y_continuous(limits = c(0, 110000)) +
  theme_bw()

#小时里的注册用户和临时用户
count_casual_registerd <- bike_data %>% group_by(hr) %>% summarise(cnt_registered = sum(registered), cnt_casual = sum(casual))

ggplot(count_casual_registerd, aes(x = hr)) +
  geom_bar(aes(y = cnt_registered, fill = "Registered"), stat = "identity", position = "stack") +
  geom_bar(aes(y = cnt_casual, fill = "Casual"), stat = "identity", position = "stack") +
  labs(title = "Bike Rentals by Hour and User Type", x = "Hour of Day", y = "Total Rentals") +
  scale_fill_manual(values = c("Registered" = "blue", "Casual" = "orange")) +
  theme_bw()



# 3- 不同天气情况下的租赁情况的箱线图 boxplot.  Clear, Few clouds, Partly cloudy, Partly cloudy
#分组下 记得先将x轴的转化为factor
bike_data$weathersit <- as.factor(bike_data$weathersit)
bike_data$weathersit <- factor(bike_data$weathersit,
                               levels=c(1:4),
                               labels = c("Clear","Few clouds","Partly cloudy","Rainy"))
ggplot(bike_data,aes(x=weathersit, y=cnt))+
  geom_boxplot()+
  labs(x="Wheather Condition",
       y="Count",
       title="Bike Rentals by Weather")

# 4- 接下来继续分析季节 1:winter, 2:spring, 3:summer, 4:fall
rental_cnt_by_season <- bike_data %>% group_by(season) %>% summarize(cnt_season= sum(cnt))

rental_cnt_by_season$season <- factor(rental_cnt_by_season$season,
                                      levels=c(1,2,3,4),
                                      labels =c("winter","spring","summer","fall"))
ggplot(rental_cnt_by_season,aes(x=season, y=cnt_season))+
  geom_bar(stat = "identity")+
  labs(title = "Bike Rentals by Season", x = "Season", y = "Total Rentals")+
  theme_bw()

# 结合以上进行方差分析 季节+天气 结果都是显著的！
aov_result1 <- aov(cnt ~ season, data = bike_data)
summary(aov_result1)
TukeyHSD(aov_result1)

aov_result2 <- aov(cnt ~ weathersit, data = bike_data)
summary(aov_result2) 
TukeyHSD(aov_result2) 

# 5- 回归分析
# 构建线性回归模型
bike_data$season <- as.factor(bike_data$season)
bike_data$weathersit <- as.factor(bike_data$weathersit)

lm_model <- lm(cnt ~ temp + hum + windspeed + season + weathersit + weekday, data = bike_data)
step_model <- step(lm_model)
summary(step_model)

library(car)
vif(step_model)



# 6- 时间序列分析
daily_data <- aggregate(cnt ~ dteday, data = bike_data, sum)
head(daily_data)
# 将数据转换为时间序列对象
library(xts)
library(tseries)
str(daily_data)
# 使用ts函数创建时间序列
ts<- ts(daily_data$cnt, start = c(2011, 1), frequency = 365)
# 查看时间序列对象
plot(ts)

plot(ts)
adf.test(ts) #不平稳

ts_d1 <- diff(ts, differences = 1)
ts_d1 <- na.omit(ts_d1)
adf.test(ts_d1) #平稳了
plot(ts_d1)

# white-noise 检验-结果表明p <0.05, 拒绝原假设（原假设是序列是白噪声），序列不是白噪声 可以进行后续的步骤
for(k in 1:4)print(Box.test(ts_d1,lag=1*k))

# 检查 ACF 和 PACF
acf(ts_d1, main = "ACF ") 
pacf(ts_d1, main = "PACF ")   

# ARIMA 模型拟合-自动选择最佳模型
library(forecast)
auto_model <- auto.arima(ts, seasonal = TRUE)
summary(auto_model) 


#模型诊断（白噪声检验等）原假设是白噪声，结果表明：p>0.05 残差是白噪声 表明模型提取的很充分了
for(k in 1:3)print(Box.test(auto_model$residuals,lag=1*k))
checkresiduals(auto_model)
#预测未来30天的
forecast_values <- forecast(auto_model, h = 60)  # 预测未来 60 天
autoplot(forecast_values) +
  xlab("Date") +
  ylab("Count") +
  ggtitle("Bikes Forecasting with ARIMA Model 60days") +
  theme_minimal()
show(forecast_values)


#holt-winters 三参数指数平滑
x.fit <- HoltWinters(ts)
plot(x.fit)
x.fore <- forecast(x.fit,h=60)
autoplot(x.fore ) +
  labs(title = "Holt-Winters Forecast 60days", x = "Date", y = "Rentals")


accuracy(forecast(auto_model))
accuracy(forecast(x.fit))
#SARIMA 更合适

