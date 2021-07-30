

# Introduction ------------------------------------------------------------

'The purpose of this code is to give you basic understanding of using R language and Rstudio'
'while also showing some advanced tips and tricks to facilate and ease your work'
'The code is build by different sections (marked by the -------------------) that shows'
'different aspects of using R/conducting basic data analysis'
'It is advised to use this code as a complementary tool to formal training'
'For specific advices in R, it is usually worthwhile to search the web.'
'For knowledge of specific use of packages, there are many good cheatsheets on the most common ones'
'some of those are added to this code'

# setting-up your code/loading packages ----------------------------------

# folder location where the source data is found, and where outputs will be saved
setwd("c:\\r_location\\")
# a vector of all the packages you want to load. make sure you have installed them first (only necessary once per package)
# if you want to load another package, simple add it to the list
Packages <-
  c(
    "data.table",
    "dplyr",
    "haven",
    "ggplot2",
    "DataExplorer",
    "validate",
    "datapasta",
    "openxlsx",
    "stringr",
    "gmodels",
    "survey",
    "purrr",
    "rlang"
  )
# this line loads all the packages you have written
lapply(Packages, library, character.only = TRUE)

# if you're just interested in specific functions from conflicted packages or want to reduce chance of conflicting (or just not load unnecessary whole packages), use import
# import::from(package_name,fun1,fun2,fun3...)
import::from(diffobj, diffStr, diffChr, diffPrint)
import::from(glue, glue)
import::from(magrittr, "%<>%")
import::from(dataCompareR, rCompare)
import::from(tidyr, pivot_longer, pivot_wider)
import::from(plyr, rbind.fill, round_any)
# you can also change the function name
import::from(Hmisc, to_cs = Cs)

# you can also import all functions/all except certain ones
# import::from(survey,.all=T)
# import::from(survey,.except = c("svyby"))

# see specific explanation on packages in "advanced packages" section

# if you want to use an installed package but without loading it, you have to use
# its name before each function (such as with the import::)

#advanced package management
#the "librarian" package allows to install (if necessary), update and load all packages in just 1 line.
librarian::shelf(data.table,dplyr,haven,ggplot2,DataExplorer,validate,ggplot2,datapasta,openxlsx,stringr,gmodels,survey,purrr,rlang,dtplyr,quiet = T)

# importing a file --------------------------------------------------------

# (file.name is how the file is named in the folder, for example - "yeshuvim_2012_2020.xlsx")
# (dont forget to include the extension in the file name, meaning - ".csv", ".xlsx"...)
# if its csv:
data <- fread(file.name)
# if its excel (with 1 sheet only)
data <- read_xlsx(file.name)
# if its excel (with multiple sheets) - then you have to import each sheet seperatly
data <- read_xlsx(file.name, sheet = sheet_number)
# if its spss
data <- read_sav(file.name)
# if its stata
data <- read_dta(file.name)

# advanced file import options
# within the read functions there are options to import specific rows/columns, useful when you dont need all columns or rows
# nrows() = how many rows to read (for exp - nrows=10000, will only load the first 10k rows)
# header - T/F whether the first row contains column names
# na.string - which strings should be counted as NA, for exp - ",,"/"."/"na", etc
# stringsasfactors - should string columns be imported as factors
# skip - ignore the X first lines (for example if the desired table only starts at the 3 row in excel, then skip=2)
# select/drop = which columns to keep/drop
# col.names = if there are no colnames present or you want to replace immediately the column names, then you can write the col names vector here
# keepleadingzeroes = if T, then columns with numeric data with zeros at the start(such as phone nums) will be read as characters, keeping the zeros

# import to a list
# if you want to import several files to a list, for exp if you have almost identical files which you want later to merge/run the same funs on it, then its useful to import to a list
# this function for exp imports all file in a specific folder (location is "used_sources) by file name pattern (which is "name_pattern)
# later all imported data is named after the file name
import_files <- function(name_pattern) {
  import <- list.files(path = used_sources, pattern = name_pattern)
  x <- lapply(import, function(i) {
    fread(paste0(used_sources, i), integer64 = "numeric")
  })
  names(x) <- import
  x <- x
}



# how to load pre-existing data (the R comes with several built-it dataframes. useful for general code testing)
data(iris)

# if the file is not in your wd but somewhere else (such as a folder inside your wd)
data <- fread(paste0(".//data_folder//", file.name))

# basic syntax ------------------------------------------------------------

# vectors
# vectors are a type of list that can contain a single type of data, such as list of names, list of numbers
# many functions allow using vectors as inputs (or outputs) so you dont have to copy each row for each item in the vector
# for example, the "lapply" function in row 10 does exactly that. instead of loading each package separately using the library function
# i created a vector of all the packages and with lapply i used the library function on all the vector in just one line.

# another example is in line 100 with the "select" example. i used a vector with the column names to select them all at once.
# not all functions allow using vectors, and others need some adaptations to work with them. its a trail and error thing...

# to create a vector, just add "c" and parentheses
v <- c(1, 2, 3, 4, 5)
# if its a vector of names:
v <- c("one", "two", "three", "four")

# how to copy/modify existing database using the "<-"
data <-
  data # good if you want to modify/change things in data and save these changes in the original data.
data1 <-
  data # creates a copy of data. important if you want to keep the original as it is

# how to select specific column using the "$"
iris$Species # this will choose only the species column inside iris

# "chaining" (doing several actions within the same data) using the " %>% "
data <-
  data %>% mutate(new_column = 5) # this will create a new column within the "data" database
data1 <-
  data %>% mutate(new_column = 5) # this will create a database named "data1", which is a copy of data, but now with the new_column

# functions
# to check which arguments a certain function has, use args(function_name)
#for example
args(mutate)
# to get more information on a function (such as what do each argument do), use ?function_name
#for example
?mutate


# adding/modifying data ----------------------------------------------------

# how to add a new column - with the "mutate" function
data <- data %>% mutate(new_column = 5)
# how to add a new column/modifiy existing column with conditions (based on other column), what comes before the "~" is the condition, and what comes after is the "if true"/result
# in this case, the "new column" is a binary column, which includes text, and has only 2 options, either "below 5", or "euqal or bigger than 5")
data <-
  data %>% mutate(new_column = case_when(old_column < 5 ~ "below 5", old_column >= 5 ~ "equal or bigger than 5"))

# how to rename a column
data <-
  data %>% rename(new_col_name = old_col_name) # rename one column
data <-
  data %>% rename(new_col_name1 = old_col_name1, new_col_name2 = old_col_name2) # rename several columns

# how to do actions by groups (in this example, any analysis made after this line will be done by year)
data <- data %>% group_by(year) # group by one column
data <-
  data %>% group_by(year, population_group) # group by several columns

# how to filter a database (so only certain rows will be included) in this exp, rows where sepal length is below 0.5 will be filtered
iris_filtered <-
  iris %>% filter(Sepal.Length > 0.5) # filter by one condition
iris_filtered <-
  iris %>% filter(Sepal.Length > 0.5 &
                    Sepal.Width >= 1) # filter by several conditions

# how to select specific columns (i.e to remove all other columns)
iris_less_columns <-
  iris %>% select(c("Species", "Sepal.Length", "Sepal.Width"))

# show only distinct rows,if no argument is given, then it is based on all columns
data <- data %>% distinct()
# however, if you want to check if rows are distinct only by some columns, by mindful that you have to also add keep_all=T if you want the other columns to be kept.
# otherwise they will be deleted
data <- data %>% distinct(id, year, .keep_all = T)

# actions on columns by each row (rowwise)
# for exp, say you want to create a new column which is the sum of other columns.
# by default, using "sum" on a column in mutate will give the sum of the entire column (by groups)
# if you want to add a column which is the sum of other columns by row, a convenient tool is "rowwise", for exp:
data <- data %>%
  rowwise() %>%
  mutate(new_col = sum(col_a, col_b, col_c))

# adding a lagging/leading column (for exp, value of last year/next row, etc..)
# "default" argument will give default value when cant compute lag/lead value (for first/last row of DF)
# "n" value is the num of rows/group_rows to go back(or ahead)
# by row
data <-
  data %>% mutate(previous_year_value = lag(value, order_by = year))
# by groups (only need to add group_by)
my_iris %>%
  group_by(Species) %>%
  dplyr::mutate(my_lag = dplyr::lag(Sepal.Length,
                                    n = 1,
                                    default = NA))


# summarizing data --------------------------------------------------------

# useful link with basic things you can do with summarise - 
# https://www.r-bloggers.com/2021/06/summarize-in-r-data-summarization-in-r/

# how to get summaries (summaries shows you only the results of your analysis by the different groups you created)
# this summary will give the average of the column sepal.length
summary1 <-
  iris %>% summarise(avg_length = mean(Sepal.Length)) # create only one summary column
summary1 <-
  iris %>% summarise(avg_length = mean(Sepal.Length),
                     avg_width = mean(Sepal.Width)) # create several summary columns

# an example using the iris data base (this will get the average sepal length by each species, notice the difference from the previous example)
summary2 <- iris %>%
  group_by(Species) %>%
  summarise(avg_sepal_length = mean(Sepal.Length))

# how to add "total" rows when summarizing with groups
# simply add a total group with mutate (based on the original DF) for each grouping variable
# it is important to do this before the "group_by"
summary2 <- iris %>%
  bind_rows(mutate(.,Species = "total_species")) %>% 
  group_by(Species) %>%
  summarise(avg_sepal_length = mean(Sepal.Length))

# get the nth value of a column with summarize (for exp - first/last/second value)
# the basic syntax is (x=col_name,n=number of value, order_by=this/other column)
summary2 <- iris %>%
  group_by(Species) %>%
  summarise(first_flower = nth(Sepal.Length,2,order_by = flower_name))


# other possibilites for summary could be "median","sd","precentile"...

# weighted summaries:
# some summary functions can use columns as weights (for exp, if your using a survey where every obs is weighted)
summary2 <- iris %>%
  group_by(Species) %>%
  summarise(mean_adj <- weighted.mean(Sepal.Length, weights_column))

# pivoting data -----------------------------------------------------------

# how to pivot data from long to wide. the names_from is the category column (such as year, population group, month, etc), and the values_from is where the "numbers" or data is.
# in this example, new columns of sepal.length and sepal.width will be created for each species.
pivot_data <-
  exp_data %>% pivot_wider(names_from = Species,
                           values_from = c("Sepal.Length", "Sepal.Width"))

# optinal arguments for pivot_wider
# names_prefix - add a prefix to the new column, such as "month of", "gender by" etc.
# names_sep - if you want to pivot by several columns with names_from, this allows that and adds a seperator for the new name. such as "employed_men_arab"



# pivot from wide to long
# basic pivot longer
# in general, pivot longer is more difficult since its harder to extract the different column values from the new columns
# the names to is the column of the group variable, and the values to is the column of the values ("actual" data)
# the cols is which are the relevant columns to be unpivoted
# in this case, imagine we have "sepal.length_setosa" "sepal.length_verocisa" etc. in order for the code to know
# that only the "setosa" or "verocisa" is the species, you need to provide "names_prefix",
# in this case - "sepal.length_"
unpivot_data <-
  pivot_data %>% pivot_longer(cols = c(1:3),
                              names_to = "species",
                              values_to = "Sepal.Length")

# advanced pivot longer - if you have several different kinds of columns which were all originally pivoted from the same group
# such as data of employed/unemployed and out of workforce per year which were pivoted by.
# it is possible to pivot them all back using this syntax. the "names_to" referes to the columns that are going to be created
# notice that the "value" columns has a "." in front of it. that means that for each type of data (unemployed/employed/out of workforce)
# a new column will be created by that name, based on a pattern. the "year" is an exp for the common column they were pivoted by before then
# the "names pattern" allows us to identify which part of the column name is the name of the "value" and which part is the "year"
# for exp, lets say we have a column name "employed_2012", the leftmost "(.+)" corresponds with the leftmost "names_to" (i.e the "value")
# the rightmost "(.+)" corresponds with the rightmost "names_to" (in this case - the year). the "_" in between signifies that there
# is an "_" between the 2 desired parts.
# for a column name such as "population_total_employed_year_2012" the names_pattern will be ("population_total_(.+)_year_(.+)")
results <-
  data %>% pivot_longer(
    cols = c(5:25),
    names_to = c(".value", "year"),
    names_pattern = c("(.+)_(.+)")
  )


# merging data ------------------------------------------------------------


# if you want to merge DFs by columns (side by side, i.e DFs with different columns but same/partially same rows)
merge_df <- merge(first_df, second_df, by)
# by is a vector of the columns that contain the matching values on which to merge by (such as I.D, group value, year, etc)
# you can also use all.x/all.y/all = T to keep rows which didnt have a match in the other df (x refers the first df, y the second)
# all=T would keep all rows, and by default only matched rows will be kept.

# however, this is an inefficient way to merge, and it will be time consuming especially in large data frames.
# here are few tips to improve merge run times:
# 1) using "merge" function with data.tables is faster then a simple merge function (i.e turn the df's into data.tables)
# 2) dplyr joins is faster then "merge". 
full_join() #will give rows from both dfs (while merging all matched rows)
left_join() / right_join() #would give all rows from left/right dfs, aside from the matched rows
inner_join() #would return only matched rows
# 3) data.table joins are the fastest way to merge data.tables.
#   the basic data.table merge is right join (all rows from the dt inside the bracets, and any matches from the outermost)
#   for inner join, add "nomatch = 0" to the argument.
#  option 1 - without keys: 
joined_dt[main_dt, on=key_column]  #notice the the leftmost dt is the one we want to join, and the inner one is the main df
# option 2 - with keys: 
joined_dt[main_dt] #if both dt has set keys, no need for the "on" argument. this is the fastest way to merge

# if you want to bind DFs together (one after the other, i.e DFs with different rows but same/partially same columns)
option_1 <- bind_rows(first_df, second_df)
option_2 <-
  rbind.fill(first_df, second_df) # missing columns will be filled with NA

# when merging data, especially when using bind_ros/rbind.fill, its important to make sure that columns that should match are actually identical in their names
# common mistakes are when one column has uppercase letters (Year,year) - these columns will be considered as different
# as a general rule, its good to convert all column names to lower case
names(DF) <- tolower(names(DF))
# then compare using "janitor" package the columns between the different DFs.
compare_df_cols(DF1, DF2)
# if columns that should match are'nt, because of a type error for example (income,imcome), consider renaming columns before the merge

# data table syntax (dplyr alternative) -----------------------------------

# the data.table package has a different syntax then dplyr for doing basic modifications such as adding/changing columns, its useful when the data.frame is very large, or doing very specific changes
# for exp, since data.table filter/grouping is per rows and not permanent for the whole data, its easier to change specific rows in data.table without having to narrow the data and merging all the different subgroups

# when working with data.table syntax, its important that the data object will be defined as DT. for that use setDT
# when using dplyr syntax on a data.table, it will probably change the type back to data.frame. therefore its important to use setDT again
setDT(data)

# adding columns
data[, new_col := col1 + col2]
# filter rows
data[col1 == 6 & col2 == 4, ]
# group by
data[, , by = .(group1, group2)]
# add new column, which is the sum of columns by index - this exp the new col is the sum of the first ten columns
# this example is for introducing the ".SD", which basically means "for each object in range", here the meaning is each column
data[, new_col := rowSums(.SD), .SDcols = 1:10]
# complete example - for rows where condition==1, create a new column which equal the sum of income by groups of year
data[condition == 1, new_col := sum(income), by = year]

# create column based on criterion (similar to "case_when" for dplyr)
# here a new col is created with default value of 1, but when value of col_1 is greater than col_2, the new_col value is replaced as 1
data[, new_col := 0][col_1 > col_2, new_col := 1]

# setnames - a useful function to change column names by order which they appear in each vector (the first "old_col" will be replaced by the first "new_col"...)
setnames(data,
         c("old_col_name_1", "old_col_name_2"),
         c("new_col_name_1,new_col_name_2"))
# also, setnames has a "skip_absent" argument. if =T, then the function wont return error but rather ignore and skip when there isnt a column matching a name given in old_names

# rbindlist - good for merging by columns lists of data.tables.
merge <- rbindlist(list_of_DT)
# has useful arguments: use.names - whether to merge by column name or by column index
# fill - fill empty columns with NAS
# idcol - write from which DT the row came from

# setcolorder - rearranges the order of the columns in the DT
setcolorder(data, c("col_1", "col_2", ...))

# adding lagging/leading column
# shift for both operations (lag and lead)
# "type" argument determines if lag or lead. lag is default
# n - num of rows/group_rows to move by
# by - general data.table syntax for grouping
# fill - default value for missing computations
DF[, lag_col := shift(basic_col, n = 1, fill = 0), by = group]

#rows and column totals
data[, total:=Reduce(`+`, .SD),.SDcols = grep("N_", names(data), value = T)]
data %<>% 
  adorn_totals("row")
# checking your data/code -------------------------------------------------

value <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
value2 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
t <-
  as.data.frame(list(value, value2), col.names = c("value", "value2"))


# how to show the rows that are different
t1 <- t[which(t$value != t$value2), ]
# how to show rows that are matching
t[which(t$value == t$value2), ]
# how to show rows that are matching in several criterion
t[which((t$value != t$value2) | (t$value != t$value3)), ]
# example using which to correct the error, in the last code we identified rows 2,9,10 as problematic
t$value2[2] <- 2


# option 2 is to use the "validate" package. in the data you can design a test/several tests,
# and the package will show you all the rows and summaries of the rows where the test fails
check_that(iris2, Sepal.Length > 5) %>% summary()


# how to look at your data

head(t) # see top rows
tail(t) # see bottom rows
str(t) # see details on all the columns (and their type) in the data
summary(t$value) # basic summary statistics for a specific column, has to be a numerical one (not text)
names(t) # shows all column names
unique(t$value) # shows all unique answers in a column, useful when you want to check for irregularities in column
which(t > 1) # look at the example above for how to use "which"
CrossTable(data$col1, data$col2) # shows to quantities and frequncies of the interaction of at least two vars in your data (such as age and gender)
compare_df_cols(df_1, df_2) # compares column names+type between two dataframes

# DataExplorer
introduce(requests) # an analysis on the columns of a data set
create_report(requests) # a visualized report on the vars of a given datasets (opens in html)

iris2 <- iris
iris2[c(18, 44), ] <- 999
iris2[20, ] <- NA

# diffobj - good for comparison different datasets with small differences between them (marks specific changes in each row)
diffPrint(target = iris2, current = iris)
diffStr # difference in str of DFs
diffChr # compares vectors of characters

# create/use functions ----------------------------------------------------

# to create a function, use this syntax
# new_func <- function(arg1,arg2,...){
# do something
# }
# arguments can have default values
mupltiply_by <- function(arg1, arg2 = 2) {
  new_results <- arg1 * arg2
}
# functions can even use other functions, notice that you can give specific arguments to the inner functions, or add them as arguments to the general functions (for exp- arg3)
multiply_twice <- function(arg1, arg2, arg3) {
  first_multi <- arg1 * arg2
  second_multi <- mupltiply_by(1, arg3)
  results <- first_multi * second_multi
}

# if you want to use a variable inside a DF when using dplyr syntax, you have to add {{}} inside the function, otherwise you'll get an error
dplyr_function <- function(data, var_to_group) {
  data %>%
    group_by({
      {
        var_to_group
      }
    })
}
# if you want to use variables as a formula (for exp for "survey" functions) use "as formula"
svy_results <- function(formula, data) {
  results <- svytable(formula1, data)
}
results <-
  svy_results(as.formula(paste("~", gender, "+", survey_question, sep = "")))

# writing "<<-" instead of "<-" will assign value to that variable outside of the function scope
#   i.e for exp idea<<- 5 inside a function will create an "idea" variable in the global environment.
#   however, this is a great risk of running over variables and confusing the work environment, so use with caution

#how to turn a function argument to string inside a dataframe (doesnt work with dplyr mutate because of reasons unknown)
# but it can be used with data.table syntax
arg_to_string <- function(var){
  new_col <- expr_text(substitute(var,env = environment()))
  setDT(iris)
  iris[,substitute(new_col):="hello"]
}

t <- arg_to_string(OUTPUTS)

#however, i can use it a column value, notice that theres no need for "substitute"
arg_to_string_2 <- function(var){
  new_col <- expr_text(substitute(var,env = environment()))
  iris %>% 
    group_by(Species) %>% 
    summarise(total=n()) %>% 
    mutate(exp = new_col)
}

t <- arg_to_string_2(OUTPUTS)


# Functions: special arguments --------------------------------------------


# working with the "..." (unlimited) arguments:
# the "..." argument means that it will pass on any number of arguments to the following function
# basic example:
# the "geom_point" function has many possible arguments, this example allows the user to create a basic chart of some data
# while also giving the possiblity to add any (or none) design argument.
easy_plot <- function(...){
  ggplot(data,aes(x=x,y=y))+geom_point(...)
}

#"..." argument in dplyr
# in this example, this function allows to group by unlimited amount of coulmns (including 0) and then paste those names together
# important to note that paste cant except an empty group (paste vars cant be empty), therefore the "name1" argument is added
# so a name for the new column could be given even when there isnt any grouping variable
# also notice that when using enquoed variables, if the object is a list and not a single object, you need to use "!!!" instead of "!!"
sum_by_any_var <- function(data,...,name1=NULL){
  paste_vars <- enquos(...,name1)
  data %<>% 
    group_by(...) %>% 
    summarise(obs=n()) %>% 
    mutate(new_col = paste(!!!paste_vars,sep=""))
}

# in case you want to merge using "..." (for exp if your group_by is also by ...), you need to combine data.table syntax
# setting a key for each table tells the merge function which columns to use as keys
# the "setkey" function works only for data.tables, and can use the "..." as input.
# for exp:
t1 <- sum_by_any_var(iris,Species,Petal.Length,name1="hello") #with 2 grouping variables
t2 <- sum_by_any_var(iris,name1="hello") #with 0 grouping variables
sum_by_any_var <- function(data,...){
  d1 <- data %>% 
    group_by(...) %>% 
    summarise(obs=n())
  setDT(d1)
  setkey(d1,...)
  d2 <- data %>% 
    filter(some_column=value) %>% 
    group_by(...) %>% 
    summarise(obs_filtered=n())
  setDT(d2)
  setkey(d2,...)
  d1 %<>%
    merge(d2)
}

#if you want to check how many arguments were used as ...
# you cant use the following:
func_how_much <- function(a,...){
  count <- length(enquos(...))
}
# for exp, func_how_much(1) - would return 0 (since the first argument is the "a")
# for exp, func_how_much(1,1,1) - would return 2
# for exp, func_how_much(1,1) - would return 1

#working with null arguments
sum_by_0_2_vars <- function(data,
                            group1 = NULL,
                            group2 = NULL) {
  # regular operations involving the functions, its okay to group by null, it just ignores the command
  results <- data %>%
    group_by({{group1}}, {{group2}}) %>%
    summarise(obs = n())
  # this should be put at the end of the function, because so far it has only been relevant to the "paste" function
  # thus this can be used in order to create the "population group function"
  # the first if is "if the second argument is not null" (which by logic entails that the first one is already not null)
  if (!quo_is_null(enquo(group2))) {
    by_var <- enquo(group1)
    by_var2 <- enquo(group2)
    results %>%
      mutate(population_group = paste(!!by_var, !!by_var2, sep = "_"))
  }
  # the second if is only if the first one is not null (because the first if was F, we know the second argument is null)
  else if (!quo_is_null(enquo(group1))) {
    by_var <- enquo(group1)
    results %>%
      mutate(population_group = paste(!!by_var))
  }
  # else is if both arguments are null
  else {
    results %>%
      mutate(population_group = "population_group")
  }
}
# For loops ---------------------------------------------------------------

# for loops are when something is needed to be done in a repetitive order. 
# however, for loops can be very memory-heavy and cause delays when used inefficiently.
# the general rule of thumb is avoid when possible.
# for example
for (row in data){
  data$cal_mean[row,1] <- mean(data[row,1],data[row,2])
} # this is a slow process and can be easily replaced without a loop
data$cal_mean <- rowMeans(data[, c(1, 2)])

#when using lists, most of the times you can use better alternatives such as lapply/map
#for exp - this map function would add (and write) an excel sheet for each element in result list, and name it according to the name of the object
Map(function(data, name){
  addWorksheet(my_wb, name)
  writeData(my_wb, name, data)}, results, names(results))
#another example using lapply - for each element of packages vector, apply library function
lapply(packages,libarary)

#here are a few additional tips for looping

# more efficient algorithms - Calculations should be only run once if possible, so:
#   Store the results and access them rather than repeatedly recalculating
#   specifically, try to avoid indexing the whole data when only some rows/columns are needed
#   it is preferred to create a new column/vector independently, and then to merge it with the large dataframe outside of loop
#   Take non-loop-dependent calculations out of loops
#   Avoid calculations which aren't necessary (e.g. don't use regular expressions with fixed searches will do)

# Using more efficient functions can produce moderate or large speed gains. For instance, paste0 produces a small efficiency gain but .colSums() and its relatives produce somewhat more pronounced gains. mean is particularly slow.
#   Try for better vectorization, which can often but not always help. In this regard, inherently vectorized commands like ifelse, diff, and the like will provide more improvement than the apply family of commands (which provide little to no speed boost over a well-written loop).
#   cbind will slow you down really quickly.

# You can also try to provide more information to R functions. For instance, use vapply rather than sapply, and specify colClasses when reading in text-based data. Speed gains will be variable depending on how much guessing you eliminate.
 
# consider optimized packages: The data.table package can produce massive speed gains where its use is possible, in data manipulation and in reading large amounts of data (fread).

#useful link - https://stackoverflow.com/questions/2908822/speed-up-the-loop-operation-in-r
# strings -----------------------------------------------------------------

# for a complete explanation, see the "string R" cheatsheet
# sometimes you are looking for a specific patterns using a string
# there are symbol that helps with improving the pattern search
# the "*" marks every other letter, such as "*ir*", this will search every string that has "ir" in between at least two other letters
# the "^" marks the beginning of a string, such as"^apple" will find only words that start with apple
# the "$" marks the end of a string, such as"^orange" will find only words that end with orange


# lists -------------------------------------------------------------------

# the first option is to create for example different results as different objects, and then merge them
pattern_results_1 <- data %>% func(arg1, arg2)
pattern_results_2 <- data %>% func(arg3, arg4)
results <- mget(ls(pattern = "pattern"))
# if later you want to combin all the results into the same table, assuming you have identical columns, use bind_rows on the list of DF
results_combined <- bind_rows(results)

# the second (and prefered option) is to create the results already in a list, making it easier to operate
# (for exp if you want to run the same function on all results) and to export (write_xlsx needs lists anyway)
# first you need to create the empty list
results <- list()
results[["name of specific result"]] <- func(arg1, arg2)

# if you want to run the same fun for all objects in list, use lapply (the "packages" set-up uses this type of lapply)
lapply(list_of_objects, function_name, fun_arg1, fun_arg2...)

# a similar way is to use map
map(list_of_objects, function_name,...)

# also see the "advanced import options" on how to import files directly into a list

# writing to list, when input DF is also in a list
list_of_results <- list()
list_of_inputs <- list()
# for example, lets create a list of inputs (although you could also create it directly along the work process)
test <-
  list("by_age" = by_age,
       "by_anaf" = by_anaf,
       "by_education" = by_education)
# the "pluck" functions takes the first list element that matches by name pattern (therefore its important to name your list elements)
# this fun for exp creates a new list element in the list_of_results which is a summary of a DF which exists in the list_of_inputs, 
# by name pattern (no intermediate object creating needed)
results_gen <- function(var_name) {
  list_of_results[[paste0(var_name, "_analysis")]] <<-
    pluck(list_of_inputs, var_name) %>%
    group_by(population_group) %>%
    summarise(total_unemp = sum(unemployed))
}
# here is where we use the function
results_gen("by_anaf")

#extract object from list (also possible with "pluck")
x <- list[[1]]
#extract the name of the list object
x<- names(list)[1]

#store arguments in a list (they are not evaluated)
list_of_args <- alist(maslul,anaf)

#how to load the list of arguments into a function

#option 1: use "formals" to set the list as default arguments
formals(a_function) <- list_of_args #in this case, the list_of_args objects must be like this - var=maslul,var2=anaf

#option 2: use the list with "do.call". do.call runs a function using a list of arguments. 
x <- do.call(a_function,list_of_args)

#run a function on pairs of objects from list (doesnt allow interaction, only first with first, second with second, etc...)
t <- map2(list_a,list_b,test_fun)


# detailed packages -------------------------------------------------------

# useful packages:
# import - good for taking only specific functions from packages to avoid conflicts you dont need (and even prefered not to) load this package. only install it
# dplyr - amazing package for cleaning/modifying data. almost all the examples
# in this code
# data.table - allows fast reading/writing/modifying csv files
# tidyr - relevant for "pivot_wide" - mainly for summaries (see pivot:data)
# openxlsx - allows reading/writing for excel with advanced formats (see saving files)
# haven - allows reading spss/stata files (see importing a file)
# ggplot2 - useful for graphs and visualization (see visualization)
# validate - good for checking your data according to specific rules (see checking your data)
# stringr - good for replacing strings by patterns (see strings)
# janitor - good for comparing between col names of different dataframes (see checking your data)
# survey - useful for analysis of surveys
# rlang - useful when assessing and working around missing arguments (useful when amount of args is unknown)
# styler - an addin which styles your code when its messy to a more readable version
# gmodels - allows to crosstab your data (see checking your data)
# Hmisc - allows for Cs function. turns vector of object to name strings (see general tips)
# diffobj - good for comparison different datasets with small differences between them (marks specific changes in each row)
# DataExplorer - creates a detailed report on your data, columns, etc... (see checking your data)
# magrittr - adds the %<>% symbol (see general tips)
# validate - another good package for validating data by rules. shows num of pass/fail/na (see checking your data)
# datapasta - allows pasting tables to R and automatically turn them to tibbles
# glue - easier inserting vars into text instead of paste. works in dplyr too (see general tips)
# conflicted - identifies and resolves conflicts in similarly named functions between packages. prefered not to load it, just install it (see general tips)


# alternatives
# validate - observer,
# openxlsx - readxl,writexl

# visualizations ----------------------------------------------------------

# look for "ggplot2 cheat-sheet" for more explanation of how to create interesting visualizations
# such as creating labels and legends, modifying colors and shapes...

# basic bar chart
ggplot(summary2, aes(x = Species, y = avg_sepal_length)) +
  geom_bar(stat = "identity")
# basic histogram
ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram()
# basic x-y scatter
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()

# saving files ------------------------------------------------------------

# as csv
fwrite(data, file.name)
# as excel
write.xlsx(data, file.name)

# if you want to save your file in a different place other than your wd (such as a folder inside your wd)
fwrite(data, paste0(".//outputs//", file.name))

# openxlsx - advanced formatting for excel exporting
class(iris$Sepal.Length) <-
  "percentage" # for example how to show column as percentage
write.xlsx(iris, "test.xlsx")
# additional arguments
# as_table = T will turn the output dataframe to an excel table, along with possible styles

# for a bit more advanced formatting: (the process involves creating a workbook and sheet)
mywb <- createWorkbook() # create a workbook named "mywb"
addWorksheet(mywb, "sheet1") # add an excel sheet named "sheet1"
writeData(mywb, "sheet1", iris) # add a dataframe to the sheet. also possible to writedatatable (becomes an excel table)
# if you want to add commas and determine nums of decimal, you have to create to styles and apply them one on top of the other
style_num <-
  createStyle(numFmt = "COMMA") # for example, numbers will be shown with commas
style_num2 <-
  createStyle(numFmt = "0.0") # for example, numbers will be shown with  only 1 decimal
style_per <-
  createStyle(numFmt = "0%") # percentages will be without decimals, etc
addStyle(
  mywb,
  "sheet1",
  style = style_num,
  rows = 2:nrow(iris),
  cols = c(2:4, 6),
  gridExpand = T
) # add style_num to the table in sheet1 (reference can be index as well), to all rows except the first (because its a header) and to specific columns (if more than one column is chosen, gridexpand =  T)
addStyle(
  mywb,
  "sheet1",
  style = style_num2,
  rows = 2:nrow(iris),
  cols = c(2:4, 6),
  gridExpand = T
)
addStyle(
  mywb,
  "sheet1",
  style = style_per,
  rows = 2:nrow(iris),
  cols = 1
)

saveWorkbook(mywb, "test.xlsx", overwrite = T) # in case file already exists, overwrite= T
# its also possible to add plots to the excel sheet
mywb %>% insertPlot(sheet = "sheet1",
                    startCol = "G",
                    startRow = 3) # add last plot to sheet1, starting at cel "G3"
# and even simple to add text to the worksheet
writeData(my_wb, "sheet1", "This is an example", startCol = 1, startRow = 1)

# Code management ----------------------------------------------------------

#r jobs - allows running another R script in parallel 

# location - the leftmost option in the console panel
# without blocking the console (useful when you have a code that takes a long time to run but you want to continue to work during that time)
# just press "start local job"

# By default, the job will run in a clean R session, and its temporary workspace will be discarded when the job is complete. 
#   This is the fastest and safest configuration, good for reproducible scripts that have no side effects.
#   However, if you want to feed data from your current R session into the job, 
#   or have the job return data to your current R session, change the dialog options as follows:
# Run job with copy of global environment: If ticked, this option saves your global environment and loads it into the job’s R session before it runs. 
#   This is useful because it will allow your job to see all the same variables you can see in the IDE. Note that this can be slow if you have large objects in your environment.
# Copy job results: By default, the temporary workspace in which the job runs is not saved. 
#   If you’d like to import data from your job back into your R session, you have a couple of choices:
# Global environment: This places all the R objects your job creates back in your R session’s global environment. 
#   Use this option with caution! The objects created by the job will overwrite, without a warning, any objects that have the same name in your environment.
# Results object: This places all the R objects your job creates into a new environment named yourscript_results.

#r projects - a simple tool to manage your work

#an project is a way to store all the necessary parts of your work, such as data, visualization, and r scripts.
# to start an R project, go to file->new project, then save it in a location, preferably the folder where you will 
# be storing the files/scripts necessary for the work.
# notice now that those files appear in the "Files" panel on your right. you can open and manage files from there
# also, using a project automatically sets the scripts' work directory to that location (no more need to "setwd"), and allows relative references 
#   for exp, moving the entire project folder would not damage the code since the paths for the data, outputs etc is relative to the location of the project)
#   especially useful when collaborating with others or working on a project from different systems
# another advantage is the ability to easily use functions from other scripts in your project or run whole/partial scripts
# lastly, using projects gives the ability to do version control, i.e track all the changes in the code versions. for exp by using Git.
# a suggested set up for a work project would be creating a folder with the project name and save the project inside it
#   then, create 3 main folders - data (for import), scripts, outputs. this creates order and simplicity in your work

# general tips ------------------------------------------------------------

# conflicted - solving packages conflict
# common conflicts (and the recommended resolve, using "conflict_prefer" function from "conflicted)
conflicted::conflict_scout() # find the conflicts
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer(":=", "data.table")
conflicted::conflict_prefer("between", "dplyr")
conflicted::conflict_prefer("collapse", "dplyr")
conflicted::conflict_prefer("first", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")
conflicted::conflict_prefer("last", "dplyr")

# glue (string insert)
name <- "john"
v <- glue("my name is {name}")

# Hmisc - vector of objects to names
v <-Cs(apples, bananas) # gives the same results as c("apples", "bananas"). doesnt work with dplyr nesting (using {{}})
  
# magrittr syntax
data %<>% func() # the same as data <- data %>% func (meaning chaining+assign)

# section menu (navigation)
# pressing on the rightest button (next to "source") on the upper panel above the code
# will give you an easy navigation panel between code sections

# how to delete objects
# simple delete
rm(object_name)
# delete several objects
rm(data_1, data_2, data_3)
# delete by pattern
rm(list = ls(pattern = "pattern"))
# delete everything
rm(list = ls())
# delete using the "environment" pane (usually on the right).
# first make sure it is in grid mode
# then tick the desired objects to delete and press the broom symbol

# snippets
# the ability to create code shortcuts based on key words (such as a built-in function frame)
# go to tools->global option->code->snippets
# for example:
snippet fun
  ${1:name} <- function(${2:variables}){
    $(0)
}
# just writing "fun" will offer this basic function skeleton, with built it brackets
# and places to fill out necessary text (for exp - name,variables,etc...)

# other new snippets (you can create snippets of your own)
snippet plt
  ggplot(${1:data},aes(${2:aes}))+
    geom_${3:geom}()

snippet set
  packages <- c(${1:package_vector})
  lapply(packages,library,character.only=	T)

snippet imp
import::from(${1:package_name},${2:func})

snippet dat1
  ${1:data} %<>% ${2:action}

snippet dat2
  ${1:data} <- ${1:dataset} %>% ${2:action}

# general settings
# you should check the "code"/"general"/"appearance" tabs inside tools->global options.
# there are many interesting settings and features there.

# keyboard shortcuts
# ctrl+shift+m - insert "%>%"
# alt- or alt+ - insert "<-"
# alt+arrowup/down - move code up/down 1 row
# Ctrl + left click to view the R object
# Ctrl + Shift + O to access the outlined sections
# ctrl+shift+c - mark all selected rows as comments
# ctrl+shift+r - insert code section
# ctrl+enter - run current line/selection
# ctrl+L - fold selection. ctrl+shift+l - unfold selection
# clt+o - fold all. shift+alt+o - unfold all
# ctrl+alt+x - turn code into function
# ctrl+d - delete row
# ctrl+shift+alt+m - rename variable in scope
# ctrl+i - fix indets
# ctrl+shift+a - reformat selection
# Cmd/Ctrl + ↑ - Type the start of a code you have run in the past then press Cmd/Ctrl + ↑. That will list all the commands you’ve typed that start those letters
# keyboard shortcut list - Alt + Shift + K
# Cmd/Ctrl + Alt + P. This resends the previously sent chunk from the editor to the console. This is very convenient when you’re exploring data. You send the whole block once with Cmd/Ctrl + Enter, then you modify the value of n and press Cmd/Ctrl + Shift + P to resend the complete block.


# self generated/changes to shortcuts
# shift+d - use style on row (from styler addin)
# alt+`` - insert <-


# how to make dplyr run faster:
# it is possible to use dplyr syntax with data.table infrastructure in the background
# that can lead to X3-X5 speed boost with barely any changes to the code.
# this requires the dtplyr package.
# then, it is necessary to create a "lazy" copy of your DF.
# the "immutable" argument means that when F, you could change the DF (i.e add columns and such)
# this can only be done when source DF is a data.table
# you can also set keys for the data.table (isnt possible afterwards when its already a "lazy dt")
iris_lazy <- lazy_dt(as.data.table(iris),immutable =F)

#then, run your dplyr code as usual. for example:
results <- iris_lazy %>%  filter(Sepal.Length>=0.5) %>% mutate(new_col=5*Sepal.Length) %>% group_by(Species) %>% 
  summarise(obs=n(),mean_sepal = mean(Sepal.Width),mean_sepal2 = mean(Sepal.Width))

# lastly, you must turn the lazy_df back into a regular one, with: 
results <- as.data.table(results)
# or just add "as.data.table" as piped to summary command

# please note, there are some dplyr functions that dont translate well to dtplyr
# for exp, data.table currently doesn't keep empty factor levels, i.e it doesn't
# have the .drop=F equivalent that dplyr's group_by has.
# therefore, in order to keep empty groups, you need to add them later as a separate command.
# this command adds empty groups, with the following arguments:
# factor_col = the column/variable with the empty groups. group1/2/etc = other grouping variables
# that the factor_col has to complete for each group. (for exp pop_group*gender), factor_col = pop_group, group1=gender
# if there is more than one variable that has empty groups, you need to run this again with a different factor_col
data[, .SD[.(factor_col=c(levels(factor_col))), on="factor_col"], by=.(group1,group2,...)]

# if you want to create a large data.frame, for exmp when checking run speeds of code
# you can duplicate existing tables to make them longer:
iris <- iris %>% slice(rep(1:n(), each = 10000)) #duplicates each row 10,000 times


# missions for this code --------------------------------------------------

# add to the code:
#1) working with regressions
#3) working with online data (api and such)