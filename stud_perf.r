library(readr)
stud_perf <- read.csv("C:/Users/HP/Desktop/Test Jupyter/StudentsPerformance.csv")
print(stud_perf)
#checking the head of the data be seeing all columns and few rows
head(stud_perf)
#checking the tail of the data
tail(stud_perf)

#About the database
print(class(stud_perf))
print(summary(stud_perf))
print(colnames(stud_perf))

#checking the data types
str(stud_perf)
#checking the numbers of the rows and columns of the data
total_cols = ncol(stud_perf)
cat("\n Number of columns: ", total_cols)
#rows
total_rows = nrow(stud_perf)
cat("\n Number of rows: ", total_rows)

total_col_names = colnames(stud_perf)
cat("\n Number of column names: \n")
print(total_col_names)

print("\n class types: ")
total_class_types = vector()
for(i in 1: total_cols)
{
  total_class_types[i] = class(stud_perf[1,i])
  cat("\n Type of ", total_col_names[i], " is ", total_class_types[i])
}

row_data <- nrow(stud_perf)
print(row_data)
cat("\n The data contains ", nrow(stud_perf), "rows and ", 
    ncol(stud_perf), " columns respectively" )


## understanding the missing values
install.packages("mice")
install.packages("VIM")

library(mice)
library(VIM)
data_pattern <- md.pattern(stud_perf)
print(data_pattern)

#checking for the unique values for the objects 
unique(stud_perf$gender)
unique(stud_perf$race.ethnicity)
unique(stud_perf$parental.level.of.education)
unique(stud_perf$lunch)
unique(stud_perf$test.preparation.course)

#imputation_plot = aggr(stud_perf,
#                       col = color_v,
#                       numbers = TRUE,
#                       sortVars = FALSE,
#                       labels = names(stud_perf),
#                       cex.axis = 0.9,
#                       gap = 2,
#                       ylabs = c("missing data", "pattern"))
#print(imputation_plot)

#STATISTICAL ANALYSIS
math_mean <- mean(stud_perf$math.score)
cat("\n the mean for math_score is: ", math_mean)
reading_mean <- mean(stud_perf$reading.score)
cat("\n the mean for reading score is : ", reading_mean)
writting_mean <- mean(stud_perf$writing.score)
cat("\n the mean for writting score is: ", writting_mean)

min(stud_perf$writing.score)
max(stud_perf$writing.score)
range(stud_perf$writing.score)
range(stud_perf$reading.score)
max(stud_perf$writing.score) - min(df$writing.score)
mean(stud_perf$writing.score)
median(stud_perf$writing.score)
sd(stud_perf$writing.score)
var(stud_perf$writing.score)
mad(stud_perf$writing.score)
quantile(stud_perf$writing.score)
median(stud_perf$writing.score)
mean(stud_perf$writing.score)
IQR(stud_perf$writing.score)
79 - 57.75

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
mode(stud_perf$math.score)
mode(stud_perf$reading.score)
mode(stud_perf$writing.score)

# understanding numeric data 
int_stud_perf <- Filter(is.numeric, stud_perf)
sdf_cols_int <-colnames(int_stud_perf)
print(sdf_cols_int)

for(i in 1:ncol(int_stud_perf))
{
  # remove null from column
  column_name = sdf_cols_int[i]
  cat("\n\n column : ", column_name)
  
  column_int_stud_perf = int_stud_perf[i]
  column_int_stud_perf = column_int_stud_perf[!apply(is.na(column_int_stud_perf) |
                                                       column_int_stud_perf == "", 1, all),]
  
  #count of total values
  total_count_column = nrow(int_stud_perf[i])
  
  #count of null values
  column_int_stud_perf = total_count_column - length(column_int_stud_perf)
  
  #percentage of null in column
  column_null_perc = column_int_stud_perf/total_count_column
  cat("\n null value percentage : ", column_null_perc)
  
  #range of column
  column_range = range(column_int_stud_perf)
  cat("\n range : ", column_range)
  
  #printing quantile of column
  column_quantile = quantile(column_int_stud_perf)
  cat("\n quantile : ", column_quantile)
  
  #printing the minimum of the column
  column_min = min(column_int_stud_perf)
  cat("\n minimum : ", column_min)
  
  #printing the maximum of the column
  column_max = max(column_int_stud_perf)
  cat("\n maximum : ", column_max)
  
  #printing the mean of the column
  column_mean = mean(column_int_stud_perf)
  cat("\n mean:", column_mean)
  
  #printing the median
  column_median = median(column_int_stud_perf)
  cat("\n median:", column_median)
  
  #printing the mode using user defined function
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
  column_mode = mode(stud_perf$math.score)
  print(column_mode)
  column_mode = mode(stud_perf$reading.score)
  print(column_mode)
  column_mode = mode(stud_perf$writing.score)
  print(column_mode)
  
  #printing the median absolute deviation
  column_mad = mad(column_int_stud_perf)
  cat("\n median absolute deviation: ", column_mad)
  
  #variance
  column_variance = var(column_int_stud_perf)
  cat("\n variance: ", column_variance)
  
  #standard deviation
  column_std = sd(column_int_stud_perf)
  cat("\n standard deviation: ", column_std)
  
}

#DATA VISUALIZATION

plot(stud_perf$math.score, stud_perf$reading.score, col = "blue")
barplot(H,names.arg = M, xlab = "gender", ylab = "math.score", col = "blue", 
        border = "red", main = "Parental level of Education")
hist(stud_perf$reading.score, col = "blue", prob = TRUE, border = "red")
hist(stud_perf$writing.score, col = "red", prob = TRUE, border = "green")
