setwd("C:/Users/frede/OneDrive - Regis University/MSDS_696/MSDS696_Data_Science_Practicum_II/")

"# Using Predictive Modeling to Analyze Employee Churn"

"# Defining the Business Problem"

"Within the US, employers face an average employee churn of about 10%-15% annually 
which can prove costly to companies especially those in their early-stages of 
growth (Law, 2019). Kolowich (2018) wrote that when valued employees leave 
abruptly, it is estimated that it costs companies 30% from its other employees' 
annual salary to hire junior employees. However, that percentage can be increased 
to 400% when replace more senior position roles (Kolowich, 2018).

For companies finding a replacement difficult because the company is trying to find 
someone who is as productive as their former employee. The company also must 
consider the loss of knowledge and business acumen about the company, and time and 
resources needed to teach the new hire. As a result, this process can be a serious 
problem for companies that are facing high rates of attrition due to the extra load 
management being placed on other employees (Ashe-Edmunds, 2017). However, many 
companies today try to resolve this issue by creating programs that provide training 
and career development, and improved work-life balance to boost employee retention (Regan, 2020).

The fact that employee churn has and will continue be an issue that companies face, 
within this data science project I will be creating an model with the IBM dataset, 
provided by [Kaggle] (https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset). 
In this analysis, I hope to use this dataset to build a model to predict when employees 
are going to quit by understanding the main drivers of employee churn.
"

"### Importing the data"

"Let's import the dataset and make of a copy of the source file for this analysis."

# Read Excel file
df_source <- read.csv("Data/WA_Fn-UseC_-HR-Employee-Attrition.csv")
names(df_source)

colnames(df_source)[1] <- "Age" # Renaming the column

# Making copy of the dataset
library(data.table)
HR_data <- copy(df_source)

"### Exploratory Data Analysis"

"Let's look at the data and see how it is formatted before performing analysis"

str(HR_data)

"From the data, we can see that there are 1,470 observations and 35 variables with 
various information about the employees."

"Now let us have a glimpse of the data but instead of using the `glimpse()` or 
`summary()` functions, lets use the `skim()` function. The reason why is because 
can provide more detail about the data, such as the missing rate, complete rate, 
and a mini histogram of each variable (Quinn & Waring, 2019)."

install.packages('skimr')
library(skimr)
skim(HR_data)

"From a glace of the mini histograms, it seems that several variables are tail-heavy. 
So let's use the hist() function to have a better look at some of these variables."

hist(HR_data$MonthlyIncome,main="Distribution for Monthly Income",xlab="Monthly Income",ylab="Count",col="green")

hist(HR_data$DistanceFromHome,main="Distribution for Distance from Home ",xlab="Distance from Home",ylab="Count",col="grey")

hist(HR_data$YearsAtCompany,main="Distribution for Years at Company ",xlab="Years at Company",ylab="Count",col="blue")

hist(HR_data$Age,main="Distribution for Age",xlab="Age",ylab="Count",col=blues9)

"After looking at the MonthlyIncome, DistanceFromHome, and YearsAtCompany they do 
show a right-skewed in their distributions. The distribution for Age also has normal 
distribution that looks slightly right-skewed with majority being in the age range of 30 to 40."

prop.table(table(HR_data$Gender)) #Percentage of Gender

"The table above show that 60% of the dataset gender is male."

"Within our exploratory analysis, the Attrition column will be used as our target 
variable. Before continuing out analysis of the data, we should find out the 
distribution and percentage of the Attrition variable. "

library(dplyr)
library(magrittr)

HR_data %>% group_by(Attrition) %>% summarise(Total = n()) %>% print()

library(ggplot2)

ggplot(HR_data,aes(Attrition,fill=Attrition))+geom_bar() + ggtitle("Total Numbers of Attrition")

prop.table(table(HR_data$Attrition)) #Percentage of Attrition

"From the table above, we see approximately 16% of IBM employees are leaving"

"Now that we have set the Attrition as our target variable, we can see how it 
affects the other variables in the dataset. In order to reduce time producing 
single graphs for these variables, we are going to use the `grid()` and `gridExtra()` 
functions to help arrange multiple grid-based plots on a page (Phiri, 2013)."

library(grid)
library(gridExtra)
age_graph <- ggplot(HR_data,aes(Age,fill=Attrition))+geom_density()+facet_grid(~Attrition)
gender_graph <- ggplot(HR_data,aes(Gender,fill=Attrition))+geom_bar()
marital_graph <- ggplot(HR_data,aes(MaritalStatus,fill=Attrition))+geom_bar()
business_graph <- ggplot(HR_data,aes(BusinessTravel,fill=Attrition))+geom_bar()
grid.arrange(age_graph,gender_graph,marital_graph,business_graph,ncol=2, bottom = "Figure 1")

"
In Figure 1, we see the following: 
1.	Age: Most employees that leave IBM are around 30 years old.
2.	Gender: We see that majority of separated employees are Male and that is due to our dataset being comprised of 60% Male.
3.	Marital Status: Employees that are Single show the highest signs of Attrition, while Divorced employees are the lowest.
4.	Business Travel: Among employee who leave IBM, most travel.

"

YAC_graph <- ggplot(HR_data,aes(YearsAtCompany,fill = Attrition))+geom_bar()
YSP_graph <- ggplot(HR_data,aes(YearsSinceLastPromotion,fill = Attrition))+geom_bar()
YCM_graph <- ggplot(HR_data,aes(YearsWithCurrManager,fill = Attrition))+geom_bar()
MTHincome_graph <- ggplot(HR_data,aes(MonthlyIncome,fill=Attrition))+geom_density()
OT_graph<- ggplot(HR_data,aes(OverTime,fill=Attrition))+geom_bar()
grid.arrange(YAC_graph,YSP_graph,YCM_graph,MTHincome_graph,OT_graph,ncol=2, bottom = "Figure 2")

"
In Figure 2, we see the following: 
5.	Years at Company: Employees who have been with IBM for <3 years make up a larger proportion of those quitting the company.
6.	Years Since Last Promotion: Employees that have been recently promoted are making up a larger proportion of those who quit IBM.
7.	Years With Current Manager: Newly hired managers are also a reason for employees to quit.
8.	Monthly Income: We see higher levels of attrition among the lower segment of monthly income. 
9.	Over Time: Employees who work overtime also have a larger proportion that are quitting.

"













































































































































































































