#Problem No 1
# Check the file exist in working directory 

if(!file.exists("ME_DA_Test.txt")){
  # Read the the file if not available in the working directory
  row_data <- read.table(file.choose(), sep = "\t", header = TRUE)
  head(row_data)
}else{
  
  row_data <- read.table("ME_DA_Test.txt", sep = "\t", header = TRUE)
  head(row_data)
}

#Problem No 2

# function to identify age category
age_indicator_function <- function(data,col){
  no_row = nrow(data);
  new_col = c()
  for(i in 1:no_row)
  {
    if((data[i,col] >= 18) && (data[i,col] <= 24))
    {
      new_col[i] <- "Age 18-24"
    }
    else if((data[i,col] >= 25) && (data[i,col] <= 44))
    {
      new_col[i] <- "Age 18-24"
    }
    else if((data[i,col] >= 45) && (data[i,col] <= 64))
    {
      new_col[i] <- "Age 18-24"
    }
    else
    {
      new_col[i] <- "Age 18-24"
    }    
  }
  return (new_col)
}
# function call
indicator_age <- age_indicator_function(row_data,"age")
row_data <- cbind(row_data,indicator_age)  #Attach new column to data using cbind function
head(row_data)


#Problem No 3

#Returns top 3 box 
max3_values <- function(dataset)
{
  # Return top 4 values in specified database
  max_3 <- head(unique(sort(dataset, decreasing = TRUE, index.return = FALSE)),4)
  # print(max_3)
  # check correct value.
  if (any(max_3==99))
  {
    #print("99 is avalable")
    max_3 <- max_3[max_3 != 99]
    #print(max_3)
  }else{
    #print("99 is not avalable")
    
    max_3 <- head(max_3,3)
    #print(max_3)
  }
}

# Returns the respond value is top 3 box
check_top_3 <- function(respondent_value,data_set)
{
  top_3 <- max3_values(data_set)
  if(any(top_3 == respondent_value))
  {
    return(1)
  }
  else{
    return(0)
  }
}

# Multi variable check function
# Multi variable check function
multi_variables_check <- function(respondent_value)
{
  #Brand Familiarity
  check_brand_familiarity <- check_top_3(respondent_value, row_data$familiarity)
  if(check_brand_familiarity)
  {print("Respondent value is in Brand Familiarity top 3 box")}
  else
  {print("Respondent value is NOT in Brand Familiarity top 3 box")  }
  
  #Brand Favorability
  check_brand_favorability <- check_top_3(respondent_value, row_data$favorability)
  if(check_brand_favorability)
  { print("Respondent value is in Brand Favorability top 3 box")}
  
  else
  {print("Respondent value is NOT in Brand Favorability top 3 box")}
  
  #Brand Consideration
  check_brand_consideration <- check_top_3(respondent_value, row_data$consideration)
  if(check_brand_consideration)
  { print("Respondent value is in Brand Consideration top 3 box")}
  else
  {  print("Respondent value is NOT in Brand Consideration top 3 box") }
  
  #Brand Imagery
  imageatt0 <- check_top_3(respondent_value, row_data$imageattr0)
  imageatt1 <- check_top_3(respondent_value, row_data$imageattr1)
  imageatt2 <- check_top_3(respondent_value, row_data$imageattr2)
  imageatt4 <- check_top_3(respondent_value, row_data$imageattr4)
  imageatt5 <- check_top_3(respondent_value, row_data$imageattr5)
  imageatt6 <- check_top_3(respondent_value, row_data$imageattr6)
  imageatt7 <- check_top_3(respondent_value, row_data$imageattr7)
  imageatt8 <- check_top_3(respondent_value, row_data$imageattr8)
  imageatt9 <- check_top_3(respondent_value, row_data$imageattr9)
  imageatt10 <- check_top_3(respondent_value, row_data$imageattr10)
  imageatt11 <- check_top_3(respondent_value, row_data$imageattr11)
  imageatt15 <- check_top_3(respondent_value, row_data$imageattr15)
  imageatt17 <- check_top_3(respondent_value, row_data$imageattr17)
  imageatt19 <- check_top_3(respondent_value, row_data$imageattr19)
  imageatt20 <- check_top_3(respondent_value, row_data$imageattr20)
  imageatt21 <- check_top_3(respondent_value, row_data$imageattr21)
  imageatt22 <- check_top_3(respondent_value, row_data$imageattr22)
  imageatt23 <- check_top_3(respondent_value, row_data$imageattr23)
  imageatt24 <- check_top_3(respondent_value, row_data$imageattr24)
  
  if (imageatt0 || imageatt1 || imageatt2 || imageatt4 || imageatt5 || imageatt6 || imageatt7 || imageatt8 || imageatt9 || imageatt10 || imageatt11 || imageatt15 || imageatt17 || imageatt19 || imageatt20 || imageatt21 || imageatt22 || imageatt23 || imageatt24 )
  { print("Respondent value is in Brand Imagery top 3 box") }
  else
  { print("Respondent value is NOT in Brand Service top 3 box")}  
}

#check vaule 6 in the  top tree boxes in attribute Familiarity, Favorability, Consideration and service( imageatt1..23)
multi_variables_check(6)

#check vaule 8 in the  top tree boxes in attribute Familiarity, Favorability, Consideration and service( imageatt1..23)
multi_variables_check(8)



#Problem No 4:

# get top 3 values
top_3_values <- function(data_set){
  return(head(unique(sort(data_set, decreasing = TRUE, index.return = FALSE)),3))  
}

#Case 1: Significant difference between means in both male and female have selected values top 3 box consideration box

# Assumption 1: samples are independent 
# Assumption 2: Given sample data equal variance
# H0 : variance for both male and female have selected top 3 box consideration box value is same.
# H1 : variance for both male and female have selected top 3 box consideration box value is NOT same. 

var.test(row_data$consideration[consideration == top_3_values(row_data$ consideration)]~row_data$gender[consideration == top_3_values(row_data$ consideration)])

# Independent sample test 
#H0 :  There is no significant difference between means in both male and female have selected values top 3 box consideration box.
#H1 : There is significant difference between means in both male and female have selected values top 3 box consideration box.
t.test(row_data$consideration[consideration == top_3_values(row_data$ consideration)]~row_data$gender[consideration == top_3_values(row_data$ consideration)])


#Case 2: significant difference between means in both male and female have selected values top 3 box familiarity box

# Assumption 1: Given sample data equal variance
# H0 : variance for both male and female have selected top 3 box familiarity box value is same.
# H1 : variance for both male and female have selected top 3 box familiarity box value is NOT same. 

var.test(row_data$familiarity[familiarity == top_3_values(row_data$familiarity)]~row_data$gender[familiarity == top_3_values(row_data$familiarity)])

# Independent sample test 
# H0 :  There is no significant difference between means in both male and female have selected values top 3 box familiarity box.
# H1: There is significant difference between means in both male and female have selected values top 3 box familiarity box.
t.test(row_data$familiarity[familiarity == top_3_values(row_data$familiarity)]~row_data$gender[familiarity == top_3_values(row_data$familiarity)])


#Case 3: significant difference between means in both male and female have selected values top 3 box favorability box.
# Assumption 1: Given sample data equal variance
#H0 :  There is no significant difference between means in both male and female have selected values top 3 box favorability box.
#H1 : There is significant difference between means in both male and female have selected values top 3 box favorability box.

var.test(row_data$favorability[favorability == top_3_values(row_data$favorability)]~row_data$gender[favorability == top_3_values(row_data$favorability)])

# Independent sample test 
#H0 : Both male and female have selected values top 3 box favorability box are same.
#H1 : Both male and female have selected values top 3 box favorability box value are NOT same. 

#Welch Two Sample t-test
t.test(row_data$favorability[favorability == top_3_values(row_data$favorability)]~row_data$gender[favorability == top_3_values(row_data$favorability)])



#Problem No:5

#Find correlation between the dependent variable independent variable.

cor(data_filter$consideration,data_filter$favorability+data_filter$familiarity)

data_filter <- subset(row_data, select = c(familiarity,favorability,consideration,indicator_age))
data_filter <- subset(data_filter,consideration == top_3_values(data_filter$consideration) & familiarity == top_3_values(data_filter$familiarity) & indicator_age == "Age 18-24" & favorability == top_3_values(data_filter$favorability))
lm_result <- lm(data_filter$consideration~data_filter$familiarity+data_filter$favorability,data=data_filter)
summary(lm_result)



