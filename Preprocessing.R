library(tidyverse)
library(miscset)
library(dplyr)
data <- read.csv("~/Desktop/Churn/1Churn.csv", header=TRUE)
dim_desc(data)
glimpse(data)
names(data)
glimpse(data)
data <- data %>% mutate_if(is.character, as.factor)
data$SeniorCitizen <- as.factor(data$SeniorCitizen)
glimpse(data)
data %>% map(~ sum(is.na(.)))
data <- data %>%
  mutate(TotalCharges = replace(TotalCharges,
                                is.na(TotalCharges),
                                median(TotalCharges, na.rm = T)))
sum(is.na(data$TotalCharges))
data_t <- data %>%
  select_if(is.factor) %>%
  summarise_all(n_distinct)


data_t[1:8] %>%
  print(width = Inf)
data_t[9:15] %>%
  print(width = Inf)
data_t[16:18] %>%
  print(width = Inf)
ggplot(data) +
  geom_bar(aes(x = gender, fill = Churn), position = "dodge") + scale_fill_manual(values=c("#999999", "#FFB6C1"))
data %>%
  group_by(gender,Churn) %>%
  summarise(n=n())
ggplot(data) +
  geom_bar(aes(x = SeniorCitizen, fill = Churn), position = "dodge") + scale_fill_manual(values=c("#999999", "#FFB6C1"))
data %>%
  group_by(SeniorCitizen) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
data %>%
  group_by(SeniorCitizen, Churn) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
ggplot(data) +
  geom_bar(aes(x=Partner, fill = Churn), position = "dodge") + scale_fill_manual(values=c("#999999", "#FFB6C1"))
data %>%
  group_by(Partner) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
data %>%
  group_by(Partner, Churn) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
ggplot(data) +
  geom_bar(aes_string(x="Dependents", fill="Churn"), position = "dodge") + scale_fill_manual(values=c("#999999", "#FFB6C1"))
data %>% group_by(Dependents, Churn) %>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n))
data %>% group_by(Dependents) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
ggplot(data, aes(x = SeniorCitizen, y = TotalCharges)) +
  geom_boxplot() + scale_fill_manual(values=c("#2980B9", "#FF3333"))


ggplot(data, aes(x = Partner, y = TotalCharges)) +
  geom_boxplot()


ggplot(data, aes(x = Dependents, y = TotalCharges)) +
  geom_boxplot()


data %>%
  select(SeniorCitizen, Churn, TotalCharges, tenure) %>%
  filter(SeniorCitizen == 1, Churn == "Yes") %>%
  summarize(n = n(),
            total = sum(TotalCharges),
            avg_tenure = sum(tenure)/n)


data %>%
  select(Partner, Churn, TotalCharges, tenure) %>%
  filter(Partner == "No", Churn == "Yes") %>%
  summarise(n = n(),
            total = sum(TotalCharges),
            avg_tenure = sum(tenure)/n)


data %>%
  select(Dependents, Churn, TotalCharges, tenure) %>%
  filter(Dependents == "No", Churn == "Yes") %>%
  summarise(n = n(),
            total = sum(TotalCharges),
            avg_tenure = sum(tenure)/n)


dependents <- data %>% filter(Dependents == "No")

ggplotGrid(ncol=2,
           lapply(c("PhoneService","MultipleLines","InternetService","OnlineSecurity","OnlineBackup",
                    "DeviceProtection"),
                  function(col){
                    ggplot(dependents,aes_string(col)) + geom_bar(aes(fill=Churn),position="dodge")
                  }))

ggplotGrid(ncol=2,
           lapply(c("TechSupport","StreamingTV","StreamingMovies","Contract",
                    "PaperlessBilling"),
                  function(col){
                    ggplot(dependents,aes_string(col)) + geom_bar(aes(fill=Churn),position="dodge")
                  }))

ggplot(dependents) +
  geom_bar(aes(x=PaymentMethod,fill=Churn), position = "dodge")


