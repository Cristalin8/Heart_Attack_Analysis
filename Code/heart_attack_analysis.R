#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("caret")
#install.packages("rsample")
#install.packages("AICcmodavg")
#install.packages("vip")

library(tidyverse)
library(ggplot2)
library(caret)
library(rsample)
library(AICcmodavg)
library(vip)

data <- read.csv("C:/Users/cioba/OneDrive/Рабочий стол/UTM/Anul 3/Semestrul 1/AD/Proiect/Date/heart.csv")

summary(data)
glimpse(data)

# Factorizare pentru variabilele categorice
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$chol <- as.factor(data$chol)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)

summary_data <- data %>%
  group_by(sex) %>%
  summarize(mean_trtbps = mean(trtbps))

ggplot(summary_data, aes(x = sex, y = mean_trtbps, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Sex", y = "Mean Arterial Pressure", fill = "Sex") +
  ggtitle("Mean Arterial Pressure by Sex")

ggplot(data, aes(x = sex, y = cp, fill = age)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Sex", y = "Type pain", fill = "Sex") +
  ggtitle("Types of pain in both sexes depending on age")

ggplot(data, aes(x = age, fill = chol)) +
  geom_density(alpha = 0.5) +
  labs(x = "Age", y = "Density", fill = "Cholesterol") +
  ggtitle("Cholesterol level depending on age")

total_counts <- data %>%
  group_by(sex, restecg) %>%
  summarise(total = n())

ggplot(data, aes(x = sex, y = restecg, fill = factor(restecg))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(data = total_counts, aes(x = sex, y = restecg, label = total), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Contingency Table: Number of Cases by Sex and illness",
       x = "Sex", y = "Number of Cases", fill = "Illness") 

counts_heart_atack <- data %>%
  group_by(sex, output) %>%
  summarise(total = n())

ggplot(data, aes(x = sex)) +
  geom_bar(position = "dodge", aes(fill = sex)) + 
  geom_text(data = counts_heart_atack, aes(x = sex, y = output, label = total, color = sex), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 4, color = "black") +  
  scale_fill_manual(values = c("man" = "#748B75", "woman" = "#BAD5C1")) +  
  facet_wrap(~output) +
  labs(title = "Number of Cases by Sex and Heart Attack",
       x = "Sex", y = "Number of Cases")

# Construirea modelului logistic
data_split <- initial_split(data,prop = 0.7, strata = 'output')
data_train <- training(data_split)
data_test <- testing(data_split)


#model 1(age, sex, cp, trtbps, chol, fbs, restecg, thalachh, output)
set.seed(101)
model1 <- glm(
  output ~ 
    age + 
    sex + 
    cp + 
    trtbps +
    chol +
    restecg +
    thalachh, 
  
  family = 'binomial',
  data = data_train
)

#model 2(restecg)
set.seed(101)
model2 <- glm(
  output ~ 
    age + 
    sex + 
    cp + 
    trtbps +
    chol +
    thalachh, 
  
  family = 'binomial',
  data = data_train
)

#model 3(fbs, restecg)
set.seed(101)
model3 <- glm(
  output ~ 
    age + 
    cp + 
    trtbps +
    chol +
    thalachh, 
  
  family = 'binomial',
  data = data_train
)

#model 4(chol, fbs, restecg)
set.seed(101)
model4 <- glm(
  output ~ 
    age + 
    sex + 
    cp + 
    trtbps +
    thalachh, 
  
  family = 'binomial',
  data = data_train
)

#model 5(chol, fbs)
set.seed(101)
model5 <- glm(
  output ~ 
    age + 
    sex + 
    cp + 
    trtbps +
    restecg +
    thalachh, 
  
  family = 'binomial',
  data = data_train
)

#model 6(chol, fbs, restecg, cp)
set.seed(101)
model6 <- glm(
  output ~ 
    age + 
    sex + 
    trtbps +
    thalachh, 
  
  family = 'binomial',
  data = data_train
)

#model 7(chol, fbs, restecg, age)
set.seed(101)
model7 <- glm(
  output ~ 
    sex + 
    cp + 
    trtbps +
    chol +
    thalachh, 
  
  family = 'binomial',
  data = data_train
)

vip(model1, num_features = 10)

model_list <- list( heart_atack_model_1 = model1,
                    heart_atack_model_2 = model2,
                    heart_atack_model_3 = model3,
                    heart_atack_model_4 = model4,
                    heart_atack_model_5 = model5,
                    heart_atack_model_6 = model6,
                    heart_atack_model_7 = model7)

aic_table <- aictab(model_list)

print(aic_table)

library(ROCR)
m4_prob <- predict(model7, data_train, type = 'response')

m2_prob <- predict(model2, data_train, type = 'response')

perf1 <- prediction(m4_prob, data_train$output) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')
perf2 <- prediction(m2_prob, data_train$output) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')

plot(perf1, col = 'black', lty = 2)
plot(perf2, add = TRUE, col = 'blue')
legend(0.9, 0.5, legend = c('model7', 'model2'),
       col = c('black', 'blue'), lty = 2:1, cex = 0.8)
