setwd("C:/Users/frede/OneDrive - Regis University/MSDS_696/MSDS696_Data_Science_Practicum_II/")

"# Building an Employee Churn Model to Develop a Strategic Retention Plan"

"## Dataset Analysis"

"In this case study, a HR dataset was sourced from Kaggle is on the 
[IBM HR Analytics Employee Attrition & Performance]
(https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset) 
which contains employee data for 1,470 employees with various information 
about the employees. I will use this dataset to predict when employees are 
going to quit by understanding the main drivers of employee churn."

install.packages(c("psych", "dplyr","tidyverse","ggthemr","GGally","corr","corrplot2","brglm2","ROSE","ROCR","caret"))
library(psych)
library(dplyr)
library(tidyverse)
library(ggthemr)
library(ggpubr)
library(scales)
library(GGally)
library(corrr)
library(corrplot)
library(brglm2)
library(ROSE)
library(ROCR)
library(caret)

"### Importing the data"

"Let's import the dataset and make of a copy of the source file for this analysis. 
The dataset contains 1,470 rows and 35 columns."

# Read Excel file
df_source <- read.csv("Data/WA_Fn-UseC_-HR-Employee-Attrition.csv")


# Making copy of the dataset
library(data.table)
dataHR <- copy(df_source)

"### Data Description and Exploratory Visualizations"

"In this section, we will provide data visualizations that summarizes or extracts 
relevant characteristics of features in our dataset. Let's look at each column in 
detail, get a better understanding of the dataset, and group them together when 
appropriate."

"__Overview__"

#Dataset columns
names(dataHR)

#Renaming the column 
colnames(dataHR)[1] <- "Age"

# Dataset columns
head(dataHR,5)

"Now we will get a glimpse over the data. By using the `describe()` 
function from psych package because it gives a more detailed information about the data."

describe(dataHR)
str(dataHR)

"The data provided has no missing values. In HR Analytics, employee data 
is unlikely to feature large ratio of missing values as HR Departments 
typically have all personal and employment data on-file. 
However, the type of documentation data is being kept in has a massive 
impact on the accuracy and the ease of access to the HR data."

cat("Thus Data Set has ",dim(dataHR)[1], " Rows and ", dim(dataHR)[2], " Columns" )

"Some of the columns contain only a single value for the entire column. 
This kind of column is not informative for our modeling. So we will remove the columns."

dataHR <- dataHR %>% select(-EmployeeCount, -StandardHours, -Over18)

"We will make sure once again that we don't have NA values in our dataset."

sum(is.na(dataHR))

"__Numerical features overview__"

summary(dataHR)

library(Hmisc)

hist.data.frame(dataHR)

"
A few observations can be made based on the information and histograms for numerical features:

* Many histograms are tail-heavy; indeed several distributions are right-skewed (e.g. MonthlyIncome DistanceFromHome, YearsAtCompany). Data transformation methods may be required to approach a normal distribution prior to fitting a model to the data.
* Age distribution is a slightly right-skewed normal distribution with the bulk of the staff between 25 and 45 years old.
"

"### Feature distribution by target attribute"

"Our target variable is in the Attrition column. Before doing any 
modeling, we want to know the distribution of our Attrition variable. 
If it is imbalanced, further handling will be needed."

"__Attrition__"

attr <- dataHR %>%
  group_by(Attrition) %>%
  summarise(Total = n()) 
print(attr)

attr %>% 
  ggplot(aes(x=Attrition, y=Total)) +
  geom_col(fill = "skyblue") +
  ggtitle("Total Numbers of Attrition")

"__Age__"

age_attr <- dataHR %>%
  group_by(Attrition) %>%
  summarise(mean = mean(Age),
            sd = sd(Age))
print(age_attr)

"The age distributions for Active and Ex-employees only differs by one year.
The average age of ex-employees is __33.6__ years old, while __37.6__ is the average age for current employees."

"__Gender__"

gender_attr <- dataHR %>%
  group_by(Attrition, Gender) %>%
  summarise(Total = n())
print(gender_attr)

gender_attr %>%
  ggplot(aes(x=Attrition, y=Total, fill=Gender)) +
  geom_col(position="dodge")

pie_attr_male <- gender_attr %>%
  filter(Gender == "Male") %>%
  ggplot(aes(x="", y=Total, fill=Attrition)) +
  geom_bar(width=1, stat="identity") + 
  coord_polar("y", start=0) +
  ggtitle("Pie Chart \nAttrition Male") +
  geom_text(aes(y = Total/2 + c(5, 10), 
                label = percent(Total/sum(Total))), size=5)

pie_attr_female <- gender_attr %>%
  filter(Gender == "Female") %>%
  ggplot(aes(x="", y=Total, fill=Attrition)) +
  geom_bar(width=1, stat="identity") + 
  coord_polar("y", start=0) +
  ggtitle("Pie Chart \nAttrition Female") +
  geom_text(aes(y = Total/2 + c(5, 10), 
                label = percent(Total/sum(Total))), size=5)

ggarrange(pie_attr_male, pie_attr_female)

"Gender distribution shows that the dataset features a higher relative 
proportion of male ex-employees than female ex-employees, with normalized 
gender distribution of ex-employees in the dataset at 17.0% for Males and 14.8% for Females."

"__Correlation__"
dataHR$Attrition <- ifelse(dataHR$Attrition == "Yes",1,0)
data_hr <- dataHR[,c("Attrition", "Age", "DailyRate", "DistanceFromHome", "Education",
             "EnvironmentSatisfaction", "HourlyRate", "JobInvolvement", "JobLevel",
             "JobSatisfaction", "MonthlyIncome", "MonthlyRate", "NumCompaniesWorked",
             "PercentSalaryHike", "PerformanceRating", "RelationshipSatisfaction",
             "StockOptionLevel","TrainingTimesLastYear","WorkLifeBalance","YearsAtCompany",
             "YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager")]
round(cor(data_hr),2)

corrplot(corr = cor(data_hr, use="complete.obs"),method="ellipse")

"As shown above, "Monthly Rate", "Number of Companies Worked" and 
"Distance From Home" are positively correlated to Attrition; while 
"Total Working Years", "Job Level", and "Years In Current Role" are 
negatively correlated to Attrition."



















































