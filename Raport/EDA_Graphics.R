# Installing packages
install.packages("tidyverse")
install.packages('corrplot')
install.packages('openintro')
install.packages("jsonlite")

# Importing libraries
library(tidyverse)
library(openintro)
library(readr)
library(dplyr)
library(knitr)

library (corrplot)


library(networkD3)

# Загрузим библиотеку jsonlite
library(jsonlite)


#PRE-Proccesing Data

# Importing CSV-file
spaceship <- read.csv("C:\\Users\\jija0\\OneDrive\\Универ\\DataScience\\1_resources\\Spaceship.csv")

# Show Data
head(spaceship)
glimpse(spaceship)

# rows 8693 and columns 14

variable_types <- sapply(spaceship, class)
print(variable_types)



# Вывести типы данных в виде красивой таблицы
kable(as.data.frame(sapply(spaceship, class)), 
      col.names = c("Column Name", "Data Type"), 
      caption = "Data Types in 'spaceship'")

# Changing Data Types from character to factor
spaceship <- data.frame(lapply(spaceship, function(x) {
  if (is.character(x)) {
    as.factor(x)
  } else {
    x
  }
}))

# Вывести типы данных в виде красивой таблицы
kable(as.data.frame(sapply(spaceship, class)), 
      col.names = c("Column Name", "Data Type"), 
      caption = "Data Types in 'spaceship'")




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


# Checking for missing data
sum(is.na(spaceship))

# Подсчет количества нулевых (пропущенных) значений в каждом столбце
null_counts <- spaceship %>%
  summarise_all(~ sum(is.na(.)))

# Преобразование результатов в удобный формат
null_counts_df <- as.data.frame(t(null_counts))

print(null_counts_df)


# Checking the levels of all categorical columns
PassengerId_levels <- levels(spaceship$PassengerId)
print(PassengerId_levels)

# Применяем операцию к столбцу PassengerId и убираем лишнее
spaceship <- spaceship %>%
  mutate(PassengerId = substr(PassengerId, 1, 4))


# Используем функцию unique для получения уникальных значений столбца
unique_values <- unique(spaceship$PassengerId)

# Выводим уникальные значения
print(count(unique_values))
count_unique_values <- length(unique(spaceship$PassengerId))
# Преобразуем факторный столбец Category в числовой
spaceship$PassengerId <- as.numeric(spaceship$PassengerId)
#Теперь PAssengerId numeric

#Удаляем лишний столбец
HomePlanet_levels <- levels(spaceship$HomePlanet)
print(HomePlanet_levels)
# Using filter() to filter out all rows of comics with level that have very low counts
spaceship <- spaceship %>%
  filter(HomePlanet != "")
# Using droplevels() to drop the unused levels from the dataframe
spaceship$HomePlanet <- droplevels(spaceship$HomePlanet)
HomePlanet_levels <- levels(spaceship$HomePlanet)
print(HomePlanet_levels)


CryoSleep_levels <- levels(spaceship$CryoSleep)
print(CryoSleep_levels)
spaceship <- spaceship %>%
  filter(CryoSleep != "")
# Using droplevels() to drop the unused levels from the dataframe
spaceship$CryoSleep <- droplevels(spaceship$CryoSleep)




Destination_levels <- levels(spaceship$Destination)
print(Destination_levels)
spaceship <- spaceship %>%
  filter(Destination != "")
spaceship$Destination <- droplevels(spaceship$Destination)

VIP_levels <- levels(spaceship$VIP)
print(VIP_levels)
spaceship <- spaceship %>%
  filter(VIP != "")
spaceship$VIP <- droplevels(spaceship$VIP)

# Используйте table для подсчета частот уникальных значений в колонке HomePlanet
planet_counts <- as.data.frame(table(spaceship$VIP))
# Выведите результат
print(planet_counts)




summary(spaceship)


# Ваш код для создания графика
ggplot(data = spaceship, aes(x = Destination, fill = HomePlanet)) +
  theme(plot.background = element_rect(fill = NA),
        legend.background = element_rect(fill = NA),
        panel.background = element_rect(fill = NA)) +
  geom_bar(position = "dodge", stat = "count", color = "black") +
  scale_fill_manual(values = c("#08306b", "#2AB1DE", "#70EEED")) +
  labs(title = "Side-by-Side Bar Chart of Destination and HomePlanet",
       x = "Destination",
       y = "Count")

ggplot(data = spaceship, aes(x = Destination, fill = HomePlanet)) +
  theme(
    plot.background = element_rect(fill = NA),
    legend.background = element_rect(fill = NA),
    panel.background = element_rect(fill = NA),
    text = element_text(color = "black", size = 12)  # Изменяем цвет и размер текста
  ) +
  geom_bar(position = "dodge", stat = "count", color = "black") +
  scale_fill_manual(values = c("#08306b", "#2AB1DE", "#70EEED")) +
  labs(title = "Side-by-Side Bar Chart of Destination and HomePlanet",
       x = "Destination",
       y = "Count")+
  labs(
    title =  "Side-by-Side Bar Chart of Destination and HomePlanet", element_text(color = "black", size = 14),  # Изменяем цвет и размер заголовка
    x = "Destination", element_text(color = "black", size = 12),  # Изменяем цвет и размер подписи оси X
    y = "Count",element_text(color = "black", size = 12)  # Изменяем цвет и размер подписи оси Y
  )+
  theme(plot.background = element_rect(fill = '#638EB1'))+




# Создание массива узлов
nodes_data <- '[{"name":"Europa", "id":0},
                {"name":"Mars", "id":1},
                {"name":"Earth", "id":2},
                {"name":"TRAPPIST-1e", "id":3},
                {"name":"55 Cancri e", "id":4},
                {"name":"PSO J318.5-22", "id":5}]'

# Создание массива связей (линков)
links_data <- '[{"source":0,"target":4,"value":845},
                {"source":0,"target":5,"value":19},
                {"source":0,"target":3,"value":1132},
                {"source":1,"target":4,"value":184},
                {"source":1,"target":5,"value":45},
                {"source":1,"target":3,"value":1404},
                {"source":2,"target":4,"value":656},
                {"source":2,"target":5,"value":678},
                {"source":2,"target":3,"value":2950}]'
  
# Преобразование строк JSON в данные R
nodes <- fromJSON(nodes_data)

links <- fromJSON(links_data)
blue_palette <- 'd3.scaleOrdinal().range([ "#2AB1DE","#08306b", "#70EEED",  "#fd8d3c",  "#fdbe85", "#feedde","#c6dbef", "#deebf7", "#f7fbff"])'

# Создание сетевого графика
sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "TWh", fontSize = 25, nodeWidth = 30,
              colourScale = JS(blue_palette))


# Создание таблицы пересечения
cross_table <- table(spaceship$HomePlanet, spaceship$Destination)

# Вывод таблицы
print(cross_table)

# Assuming 'data' is your dataframe
outcome_counts <- spaceship %>%
  group_by(Transported) %>%
  summarise(count = n())


# Create the pie chart with percentage labels
ggplot(outcome_counts, aes(x = "", y = count, fill = Transported)) +
  theme(
    plot.background = element_rect(fill = NA),
    legend.background = element_rect(fill = NA),
    panel.background = element_rect(fill = NA))+
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#08306b", "#2AB1DE")) +
  labs(title = "Distribution of Transported Cases",
       fill = "Transported Status") +
  geom_text(aes(label = scales::percent(count / sum(count)), y = count), position = position_stack(vjust = 0.5), color = "white", size = 8) +
  theme(axis.text.x = element_text(angle = 0,  vjust = 0.5, hjust = 1, size = 15,color = "black"),
        axis.text.y = element_text(size = 15, color = "black"), 
        plot.title = element_text(size = 25),
        axis.title.x = element_text(size = 20),  # размер шрифта для меток оси x
        axis.title.y = element_text(size = 20))

  
  
ggplot(spaceship, aes(x = HomePlanet, fill = Transported)) +
  geom_bar(position = "dodge", color = "white") +
  scale_fill_manual(values = c("#08306b", "#2AB1DE", "#70EEED")) +
  labs(title = "Transported vs. HomePlanet", x = "HomePlanet", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


ggplot(spaceship, aes(x = CryoSleep, fill = Transported)) +
  geom_bar(position = "stack", color = "white") +
  scale_fill_manual(values = c("#08306b", "#2AB1DE", "#70EEED")) +
  labs(title = "Transported vs. CryoSleep", x = "CryoSleep", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0,  vjust = 0.5, hjust = 1, size = 15,color = "black"),
        axis.text.y = element_text(size = 15, color = "black"), 
        plot.title = element_text(size = 25),
        axis.title.x = element_text(size = 20),  # размер шрифта для меток оси x
        axis.title.y = element_text(size = 20))


glimpse(spaceship)

ggplot(spaceship, aes(x = VIP, fill = Transported)) +
  theme(
    plot.background = element_rect(fill = NA),
    legend.background = element_rect(fill = NA),
    panel.background = element_rect(fill = NA),
    text = element_text(color = "black", size = 12)  # Изменяем цвет и размер текста
  )+ 
  geom_bar(position = "dodge", color = "white") +
  labs(title = "Transported vs. VIP", x = "VIP", y = "Count") +
  scale_fill_manual(values = c("#08306b", "#2AB1DE", "#70EEED"))+
  theme(axis.text.x = element_text(angle = 0,  vjust = 0.5, hjust = 1, size = 15,color = "black"),
        axis.text.y = element_text(size = 15, color = "black"), 
        plot.title = element_text(size = 25),
        axis.title.x = element_text(size = 20),  # размер шрифта для меток оси x
        axis.title.y = element_text(size = 20))
  
  
  
ggplot(spaceship, aes(x = Destination, fill = Transported)) +
  geom_bar(position = "dodge", color = "white") +
  labs(title = "Transported vs. Destination", x = "Destination", y = "Count") +
  scale_fill_manual(values = c("#08306b", "#2AB1DE", "#70EEED"))
theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

   
# Построение гистограммы с использованием ggplot2
ggplot(spaceship, aes(x = Age, fill = Transported)) +
  geom_histogram(position = "stack", alpha = 0.7, bins = 30,  color = "white") +
  scale_fill_manual(values = c("#08306b", "#2AB1DE")) +
  labs(title = "Age Feature Distribution") +
  theme_minimal()

ggplot(spaceship, aes(x = Age, fill = HomePlanet)) +
  geom_histogram(position = "stack", alpha = 0.7, bins = 30, color = "white") +
  scale_fill_manual(values = c("#08306b", "#2AB1DE", "#70EEED")) +
  labs(title = "Age Feature Distribution") +
  theme_minimal()





# Assuming 'spaceship' is your data frame and 'Age' is the column with missing values
mean_age <- mean(spaceship$Age, na.rm = TRUE)  # Calculate the mean age

# Replace missing values with the mean
spaceship$Age <- ifelse(is.na(spaceship$Age), mean_age, spaceship$Age)



# Создание признака 'Age_grp'
age_bins <- c(0, 12, 19, 35, 60, Inf)
age_labels <- c('Child', 'Teenager', 'Young Adult', 'Middle-Aged', 'Senior')
spaceship$Age_grp <- cut(spaceship$Age, breaks = age_bins, labels = age_labels, right = FALSE)


glimpse(spaceship)

ggplot(spaceship, aes(x = Age_grp, fill = Transported)) +
  geom_bar(position = "dodge", color = "white") +
  labs(title = "Transported vs. Age group", x = "Age group", y = "Count") +
  scale_fill_manual(values = c("#08306b", "#2AB1DE", "#70EEED"))
theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


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

# Checking the levels of all categorical columns
Side_levels <- levels(spaceship$Side)
print(Side_levels)

# Проверка уникальных значений в столбце Cabin
unique_values <- unique(spaceship$Side)

print(unique_values)


unique_values <- unique(spaceship$Deck_Number)
print(unique_values)

unique_values <- unique(spaceship$Deck)
print(unique_values)


summary(spaceship)
spaceship <- na.omit(spaceship)
spaceship$Side <- droplevels(spaceship$Side)

Side_levels <- levels(spaceship$Side)
print(Side_levels)
summary(spaceship)



# Удаление столбцов 'Name' и 'Deck_Number'
spaceship <- spaceship %>%
  select(-Name, -Deck_Number)

summary(spaceship)
ggplot(spaceship, aes(x = Side, fill = Transported)) +
  geom_bar(position = "dodge", color = "white") +
  labs(title = "Transported vs. Side", x = "Side", y = "Count") +
  scale_fill_manual(values = c("#08306b", "#2AB1DE", "#70EEED"))
theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(spaceship, aes(x = Deck, fill = Transported)) +
  geom_bar(position = "dodge", color = "white") +
  labs(title = "Transported vs. Deck", x = "Deck", y = "Count") +
  scale_fill_manual(values = c("#08306b", "#2AB1DE", "#70EEED"))
theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# Создание признака 'Total Spend'
spaceship$Total_Spend <- rowSums(spaceship[, c('RoomService', 'FoodCourt', 'ShoppingMall', 'Spa', 'VRDeck')], na.rm = FALSE)
glimpse(spaceship)


# Создание графика рассеяния
ggplot(spaceship, aes(x = Total_Spend, y = FoodCourt, color = Transported)) +
  geom_point() +
  labs(title = "Scatter Plot of Total Spending by FoodCourt and Transported",
       x = "Total Spending", y = "FoodCourt") +
  scale_color_manual(values = c("darkblue", "orange")) +
  theme_minimal() +
  xlim(0, 4000) +
  ylim(0,4000)

# Создание графика рассеяния
ggplot(spaceship, aes(x = Total_Spend, y = Spa, color = Transported)) +
  geom_point() +
  labs(title = "Scatter Plot of Total Spending by Spa and Transported",
       x = "Total Spending", y = "Spa") +
  scale_color_manual(values = c("darkblue", "orange")) +
  theme_minimal() +
  xlim(0, 4000) +
  ylim(0,4000)



numeric_fields <- c('Age','Deck_Number', 'RoomService', 
                            'FoodCourt', 'ShoppingMall', 'Spa', 
                            'VRDeck','Total_Spend','Transported_numeric', 
                            'PassengerId')
# Создание матрицы корреляции

df_corr <- cor(spaceship[numeric_fields])

# Вывод матрицы корреляции
print(df_corr)

# Построение тепловой карты
ggplot(data = as.data.frame(as.table(df_corr)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "#08306b", high = "#70EEED") +
  geom_text(aes(label = sprintf("%.1f", Freq)), vjust = 1) +
  labs(title = "Correlation Heatmap",
       x = "Variables",
       y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




