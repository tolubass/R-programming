library("readr")
stud_data <- read.csv("C:/Users/HP/Desktop/Test Jupyter/Students_data.csv")
print(stud_data)
head(stud_data)
tail(stud_data)

#checking the shape of the data
rw_cl <- nrow(stud_data)
print(rw_cl)
cat("\n The data contains ", nrow(stud_data), "rows and ", 
    ncol(stud_data),  "respectively" )

#checking the class, summary and column names of the data

check.function <- function(class, summary, colnames, strings){
  print(class)
  print(summary)
  print(colnames)
  print(strings)
}
check.function(class(stud_data))
check.function(summary(stud_data))
check.function(colnames(stud_data))
check.function(str(stud_data))

#checking the numbers of the rows and columns of the data
total_cols = ncol(stud_data)
cat("\n Number of columns: ", total_cols)
#rows
total_rows = nrow(stud_data)
cat("\n Number of rows: ", total_rows)

total_col_names = colnames(stud_data)
cat("\n Number of column names: \n")
print(total_col_names)


print("\n class types: ")
total_class_types = vector()
for(i in 1: total_cols)
{
  total_class_types[i] = class(stud_data[1,i])
  cat("\n Type of ", total_col_names[i], " is ", total_class_types[i])
}

#checking the unique values of the objects in the data
# for gender, class, from 1, from 2, from 3, from 4
unik.function <- function(gender, class, from1, from2, from3){
  print(gender)
  print(class)
  print(from1)
  print(from2)
  print(from3)
}
unik.function(unique(stud_data$gender))
unik.function(unique(stud_data$class))
unik.function(unique(stud_data$from1))
unik.function(unique(stud_data$from2))
unik.function(unique(stud_data$from3))

#checking for missing values

## understanding the missing values
install.packages("mice")
install.packages("VIM")

library(mice)
library(VIM)
data_pattern <- md.pattern(stud_data)
print(data_pattern)

#STATISTICAL ANALYSIS
head(stud_data)
gpa_mean <- mean(stud_data$GPA)
cat("\n the mean for GPA is: ", gpa_mean)
algebra_mean <- mean(stud_data$Algebra)
cat("\n the mean for Alegbra is : ", algebra_mean)
calculus1_mean <- mean(stud_data$Calculus1)
cat("\n the mean for Calculus1 is: ", calculus1_mean)

min(stud_data$GPA)
max(stud_data$Algebra)
range(stud_data$Calculus1)
range(stud_data$Calculus2)
max(stud_data$Calculus1) - min(stud_data$Calculus2)
mean(stud_data$Statistics)
median(stud_data$Probability)
sd(stud_data$Measure)
var(stud_data$Measure)
mad(stud_data$Measure)
quantile(stud_data$Measure)
median(stud_data$Measure)
mean(stud_data$Measure)
IQR(stud_data$Measure)


mode = function(x){
  ta = table(x)
  tam = max(ta)
  if(all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mode)
}

# understanding and getting the numeric data
int_stud_data <- Filter(is.numeric, stud_data)
sdf_cols_int <- colnames(int_stud_data)
print("The integers values are;")
print(sdf_cols_int)

for(i in 1:ncol(int_stud_data))
{
  # remove null from column
  column_name = sdf_cols_int[i]
  cat("\n\n column : ", column_name)
  
  column_int_stud_data = int_stud_data[i]
  column_int_stud_data = column_int_stud_data[!apply(is.na(column_int_stud_data) |
                                                       column_int_stud_data == "", 1, all),]
  
  #count of total values
  total_count_column = nrow(int_stud_data[i])
  
  #count of null values
  column_int_stud_data = total_count_column - length(column_int_stud_data)
  
  #percentage of null in column
  column_null_perc = column_int_stud_data/total_count_column
  cat("\n null value percentage : ", column_null_perc)
  
  
  #range of column
  column_range = range(column_int_stud_data)
  cat("\n range : ", column_range)
  #printing quantile of column
  column_quantile = quantile(column_int_stud_data)
  cat("\n quantile : ", column_quantile)
  
  #printing the minimum of the column
  column_min = min(column_int_stud_data)
  cat("\n minimum : ", column_min)
  
  #printing the maximum of the column
  column_max = max(column_int_stud_data)
  cat("\n maximum : ", column_max)
  
  #printing the mean of the column
  column_mean = mean(column_int_stud_data)
  cat("\n mean:", column_mean)
  
  #printing the median
  column_median = median(column_int_stud_data)
  cat("\n median:", column_median)
  
}

mode = function(x){
  ta = table(x)
  tam = max(ta)
  if(all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mode)
}


Algebra_mode <- Mode(stud_data$Algebra)
print(Algebra_mode)

#visualizing the statistics
hist(stud_data$GPA, col = "blue", border = "red")


int_stud_data = vector()
for(i in 1: ncol(stud_data))
{
  mod_val <- Mode(stud_data[, i])
  #cat(i, ":  ", mod_val, "\n")
  cat("\n Mode of ", total_col_names[i], " is ", mod_val )
}

#dropping ID, from4 and y
drop <- c("ID", "from4", "y" )
new_stud_data <- stud_data[,!(names(stud_data) %in% drop)]
print(new_stud_data)
View(new_stud_data)
summary(new_stud_data)

#DATA VISUALIZATION

plot(new_stud_data$class, new_stud_data$GPA, col = "green")
barplot(H,names.arg = M, xlab = "class", ylab = "GPA", col = "green", 
        border = "red", main = "class vs GPA")
#hist(stud_perf$reading.score, col = "blue", prob = TRUE, border = "red")
#hist(stud_perf$writing.score, col = "red", prob = TRUE, border = "green")

barplot(H,names.arg = M, xlab = "Algebra", ylab = "gender", col = "blue", 
        border = "red", main = "gender vs Algebra")



barplot(H,names.arg = M, xlab = "gender", ylab = "Algebra", col = "blue", 
        border = "red", main = "gender vs algebra")
hist(new_stud_data$Algebra, col = "red", prob = TRUE, border = "green")
