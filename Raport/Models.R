# Installing packages
install.packages("tidyverse")
install.packages('corrplot')
install.packages('openintro')

install.packages('rsample')
# Importing libraries
library(tidyverse)

install.packages("reshape")
library(ggplot2)
library(reshape)

library(openintro)
library(readr)
library(dplyr)
library(knitr)

library (corrplot)


# Установка необходимых пакетов
install.packages("randomForest")
install.packages("e1071")

# Загрузка библиотек
library(randomForest)
library(e1071)


library(rsample)
### Prerequisites ###
library(dplyr)

library(ggplot2)

# modeling packages
library(caret)


library(rsample)

# model interpretability packages
library(vip)



# Access data
library(modeldata)
#PRE-Proccesing Data

# Importing CSV-file
spaceship <- read.csv("C:\\Users\\jija0\\OneDrive\\Универ\\DataScience\\1_resources\\Spaceship.csv")


# Show Data
head(spaceship)
glimpse(spaceship)
summary(spaceship)
# rows 8693 and columns 14


# Changing Data Types from character to factor
spaceship <- data.frame(lapply(spaceship, function(x) {
  if (is.character(x)) {
    as.factor(x)
  } else {
    x
  }
}))


summary(spaceship)
glimpse(spaceship)
# Подготовка данных
missing_data <- data.frame(
  Feature = c("PassengerId", "HomePlanet", "CryoSleep", "Cabin", "Destination", "Age", 
              "VIP", "RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck", "Name", "Transported"),
  MissingCount = c(0, 0, 0, 0, 0, 179, 0, 181, 183, 208, 183, 188, 0, 0)
)



# Создание гистограммы пропущенных значений
ggplot(missing_data, aes(x = Feature, y = MissingCount, fill = Feature)) +
  geom_bar(stat = "identity", color = "white") +
  labs(title = "Гистограмма пропущенных значений по признакам",
       x = "Признак",
       y = "Количество пропущенных значений") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Используйте table для подсчета частот каждого уровня
frequency_table <- table(spaceship$HomePlanet)

# Выведите результат
print(frequency_table)




# Checking for missing data
sum(is.na(spaceship))

replace_empty_with_mode <- function(factor_column) {
  # Преобразовать факторный столбец в character для подсчета частот
  char_column <- as.character(factor_column)
  
  # Найти уникальный уровень, который встречается чаще всего
  mode_level <- names(sort(table(char_column), decreasing = TRUE))[1]
  
  # Заменить пустые значения этим уровнем
  factor_column_filled <- factor(ifelse(char_column == "", mode_level, char_column))
  
  return(factor_column_filled)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# Пример использования
# Предположим, что ваш датафрейм называется df, а столбец, который вы хотите обработать, - 'YourColumn'
spaceship$HomePlanet <- replace_empty_with_mode(spaceship$HomePlanet)
spaceship$CryoSleep <- replace_empty_with_mode(spaceship$CryoSleep)
spaceship$VIP <- replace_empty_with_mode(spaceship$VIP)
spaceship$Destination <- replace_empty_with_mode(spaceship$Destination)

glimpse(spaceship)



# Применяем операцию к столбцу PassengerId и убираем лишнее
spaceship <- spaceship %>%
  mutate(PassengerId = substr(PassengerId, 1, 4))


# Преобразуем факторный столбец Category в числовой
spaceship$PassengerId <- as.numeric(spaceship$PassengerId)
#Теперь PAssengerId numeric

glimpse(spaceship)


summary(spaceship)






# Assuming 'spaceship' is your data frame and 'Age' is the column with missing values
mean_age <- mean(spaceship$Age, na.rm = TRUE)  # Calculate the mean age

# Replace missing values with the mean
spaceship$Age <- ifelse(is.na(spaceship$Age), mean_age, spaceship$Age)



# Создание признака 'Age_grp'
age_bins <- c(0, 12, 19, 35, 60, 80, Inf)
age_labels <- c('Child', 'Teenager', 'Young Adult', 'Middle-Aged', 'Senior', 'Pensioner')

spaceship$Age_grp <- cut(spaceship$Age, breaks = age_bins, labels = age_labels, right = FALSE)


glimpse(spaceship)


# Разделение 'Cabin' на отдельные признаки
spaceship <- spaceship %>%
  separate(Cabin, into = c("Deck", "Deck_Number", "Side"), sep = "/") %>%
  mutate(Deck_Number = as.numeric(Deck_Number))

# Changing Data Types from character to factor
spaceship <- data.frame(lapply(spaceship, function(x) {
  if (is.character(x)) {
    as.factor(x)
  } else {
    x
  }
}))

glimpse(spaceship)


# Пример с заменой пропущенных значений в категориальном признаке 'YourColumn' фактором (модой)
spaceship$Side <- ifelse(is.na(spaceship$Side),
                                     levels(spaceship$Side)[which.max(table(spaceship$Side))],
                         levels(spaceship$Side))

spaceship <- data.frame(lapply(spaceship, function(x) {
  if (is.character(x)) {
    as.factor(x)
  } else {
    x
  }
}))

spaceship$Deck <- replace_empty_with_mode(spaceship$Deck)

glimpse(spaceship)

# Проверка уникальных значений в столбце Cabin
unique_values <- unique(spaceship$Side)
print(unique_values)

glimpse(spaceship)
summary(spaceship)

# Удаление столбцов 'Name' и 'Deck_Number'
spaceship <- spaceship %>%
  select(-Name, -Deck_Number)


# Создание вектора численных признаков
numeric_features <- c('RoomService', 'FoodCourt', 'ShoppingMall', 'Spa', 'VRDeck')

# Замена пропущенных значений средними значениями
spaceship[numeric_features] <- lapply(spaceship[numeric_features], function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})
glimpse(spaceship)

# Создание признака 'Total Spend'
#spaceship$Total_Spend <- rowSums(spaceship[, c('RoomService', 'FoodCourt', 'ShoppingMall', 'Spa', 'VRDeck')], na.rm = FALSE)
#glimpse(spaceship)




# НОВЫЙ ДАТАСЕТ
# Копируем данные в новый датасет
df2 <- data.frame(spaceship)
glimpse(df2)


dim(df2)

# Peek at response variable
head(df2$Transported)

#Попробуем стандартизировать данные

# Стандартизация числовых столбцов
#df2$PassengerId_scaled<-scale(df2$PassengerId)
#df2$Age_scaled<-scale(df2$Age)
#df2$RoomService_scaled<-scale(df2$RoomService)
#df2$FoodCourt_scaled<-scale(df2$FoodCourt)
#df2$ShoppingMall_scaled<-scale(df2$ShoppingMall)
#df2$Spa_scaled<-scale(df2$Spa)
#df2$VRDeck_scaled<-scale(df2$VRDeck)
#df2$Total_Spend_scaled<-scale(df2$VRDeck)



# Пример: factor_column - ваш фактор-столбец
# Пример: columns_to_convert - ваши столбцы для преобразования
columns_to_convert <- c('Transported', 'CryoSleep', 'VIP')
                        
df2 <- df2 %>%
  mutate(across(all_of(columns_to_convert), ~ as.logical(.))) %>%
  mutate(across(all_of(columns_to_convert), ~ as.numeric(.)))

glimpse(df2)



## Create training (70%) and test (30%) sets
df_split <- initial_split(df2, prop = .7, strata = 'Transported')
df_train <- training(df_split)

df_test <- testing(df_split)

nrow(df_train)
nrow(df_test)

glimpse(df2)

# Используйте table для подсчета частот каждого уровня
frequency_table <- table(df_train$Transported)

# Выведите результат
print(frequency_table)




print(df_train$Transported)
print(df_test$Transported)
# Multiple logistic regression
model3 <- glm(
  Transported ~ PassengerId + HomePlanet + CryoSleep + Deck + Side + Destination  + RoomService + FoodCourt + ShoppingMall + Spa + VRDeck + Age, family = 'binomial',
  data = df_train
)
tidy(model3)





# Предсказать вероятности классов
predicted_probabilities <- predict(model3, df_test, type = "response")

# Преобразовать вероятности в классы (0 или 1) с порогом 0.5 (можете выбрать другой порог)
predicted_classes <- ifelse(predicted_probabilities >= 0.5, 1, 0)

# Создать Confusion Matrix
conf_matrix <- table(predicted_classes, df_test$Transported)

# Вывести Confusion Matrix
print(conf_matrix)

# Вычислить Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

glimpse(df2)
# 0.7194761 HomePlanet + CryoSleep
#Accuracy: 0.7074356 HomePlanet + CryoSleep + Destination
# 0.7374509  HomePlanet + CryoSleep + Age_gr 
# 0.7274472 HomePlanet + CryoSleep + Age_grp + Total_Spend
#0.728215 HomePlanet + CryoSleep + Age_grp (после того как я оставила данные, а не удаляла пропущенные)






model_rf <- randomForest(Transported ~ ., data = df_train)
predicted_rf <- predict(model_rf, df_test)

# Создать Confusion Matrix для Random Forest
conf_matrix_rf <- table(predicted_rf, df_test$Transported)

# Вывести Confusion Matrix
print(conf_matrix_rf)

# Вычислить Accuracy для Random Forest
accuracy_rf <- sum(diag(conf_matrix_rf)) / sum(conf_matrix_rf)
cat("Random Forest Accuracy:", accuracy_rf, "\n")


library(caret)

# Определите метрику, которую вы хотите использовать (например, "accuracy")
metric <- "accuracy"

# Определите контроль (trainControl)
ctrl <- trainControl(method = "cv", number = 5)

# Обучите модель с caret
model_rf_caret <- train(Transported ~ ., data = df_train, method = "rf",
                        trControl = ctrl, metric = metric)

# Выведите результаты
print(model_rf_caret)



glimpse(df2)


# Support Vector Machine (SVM)
model_svm <- svm(Transported ~ ., data = df_train)
predicted_svm <- predict(model_svm, df_test)

# Создать Confusion Matrix для SVM
conf_matrix_svm <- table(predicted_svm, df_test$Transported)

# Вывести Confusion Matrix
print(conf_matrix_svm)

# Вычислить Accuracy для SVM
accuracy_svm <- sum(diag(conf_matrix_svm)) / sum(conf_matrix_svm)
cat("SVM Accuracy:", accuracy_svm, "\n")

summary( 
  resamples(
    list(
      model1 = model_rf,model_2 = model_3,  model3 = model_svm
    ))
)$statistics$Accuracy



# Установка пакета pROC (если ещё не установлен)
install.packages("pROC")

# Загрузка пакета pROC
library(pROC)

# Create performance objects
perf1 <- performance(pred1, measure = "tpr", x.measure = "fpr")
perf2 <- performance(pred2, measure = "tpr", x.measure = "fpr")
perf3 <- performance(pred3, measure = "tpr", x.measure = "fpr")

roc1 <- roc(df_test$Transported, m1_prob)
roc2 <- roc(df_test$Transported, m2_prob)
roc3 <- roc(df_test$Transported, m3_prob)

# Create a data frame with ROC data
roc_data <- data.frame(
  SVM = roc1$tpr,
  RandFor = roc2$tpr,
  LogReg = roc3$tpr,
  FPR = 1 - roc1$specificities  # Calculating FPR from specificity
)

# Plot ROC curves with thicker lines
# Plot ROC curves with even thicker and solid lines
plot(perf1, col = 'black', main = "ROC Curves", lty = 1, lwd = 3)  
plot(perf2, add = TRUE, col = 'blue', lty = 1, lwd = 3)  
plot(perf3, add = TRUE, col = 'red', lty = 1, lwd = 3)   

# Add legend
legend(0.6, 0.4, legend = c('SVM', 'Random Forest', 'Logistic Regression'),
       col = c('black', 'blue', 'red'), lty = 1, lwd = 3, cex = 0.8) 




### Feature interpretation
vip(cv_model3, num_features = 20, fill = "#08306b")
?vip
