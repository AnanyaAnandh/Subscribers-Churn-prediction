wdbc <- read.csv("1Churn.csv", header = F)
features <- c("cols= ["gender","SeniorCitizen","Partner","Dependents","tenure","PhoneService","MultipleLines","InternetService","OnlineSecurity","OnlineBackup","DeviceProtection","TechSupport","StreamingTV","StreamingMovies","Contract","PaperlessBilling","PaymentMethod","MonthlyCharges","TotalCharges","Churn"]n",
)
names(wdbc) <- c("customerID", "Churn", paste0(features,"_mean"), paste0(features,"_se"), paste0(features,"_worst"))

wdbc.pr <- prcomp(wdbc[c(3:32)], center = TRUE, scale = TRUE)
summary(wdbc.pr)