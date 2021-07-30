# 1) set-up: global vars--------------------------------------------------------------

setwd( "D:\\Research\\Nili_Ben_Tubin_13800258_Research\\work idan")
DATA_FILE <- ".\\data_sources\\"
OBS_MIN <- 5
MAX_YEAR <- 2021
SCRIPT_NAME <- "saka_multi_year"
DATA_FILE <- ".\\data_sources\\"
ISSUE_DATE<- format(Sys.Date(),format="%d_%m_%y")
OUTPUT <- ".\\to_transfer\\"
MIN_AGE <- 25
MAX_AGE <- 64

#turn of scientific notation
options(scipen=999)

# 2) set-up: load packages--------------------------------------------------------------

# using libraries in the research room is different. the location of the packages has to be specified (lib.loc). see following code
libloc<- "D:\\Research\\Nili_Ben_Tubin_13800258_Research\\Roe_work_files\\R_Packages\\3.6"
packages <- c("crayon","dplyr","data.table","forcats","tidyr","writexl","janitor","magrittr","openxlsx","glue")
lapply(packages,library,lib.loc = libloc, character.only = T)
requireNamespace("plyr",lib.loc = libloc)


# 3) function: summaries------------------------------------------------------------------------------


#the "name" argument is for cases when the data is filtered for a certain group
#such as "45+" or "18-24", and we want to mention the data by name. 
#otherwise we will receive names such as "total pop" (which is not recognizable) and not "45+ total pop"
#the weight and month is if you want to use different months for the analysis (such as bi-monthly)
#the default is a monthly analysis

#if you want to show results by year (and not by month/month_pairs) use the "yearly" version of the function

#please notice the default arguments:
#group1/2 - if you want to divide the data to specific groups such as gender, leom, etc...
#name - in case you dont filter by groups, you need to add name "such as total population". however sometimes it is needed when using only 1 group
#month&weight is default to month and monthly weight, but it is a possibility to do an analysis by month-pairs (you need to adjust a new month-pair column and month-pair-weight)
#min/max year - which years will be added to the analysis? by default its only the current year
#min/max age - which ages should be filtered? by default its 25-64. otherwise you can use 15&100)
sum_basic_month_and_year <- function(data,group1=NULL,group2=NULL,month=chodeshseker,weight=mishkalsofi,name=NULL,min_year=MAX_YEAR,max_year=MAX_YEAR,min_age=MIN_AGE,max_age=MAX_AGE){
  data <- data %>% 
    filter(shnatseker>=min_year&shnatseker<=max_year&gil>=min_age&gil<=max_age) %>% 
    group_by(shnatseker,{{group1}},{{group2}},{{month}},workforce_attr) %>%
    summarise(population_total=sum({{weight}}),obs=n()) %>%
    filter(obs>=OBS_MIN) %>%
    pivot_wider(names_from=c(workforce_attr),values_from=c(population_total,obs)) %>%
    rowwise() %>% 
    mutate(population_total_workforce= sum(population_total_employed,population_total_unemployed,na.rm = T),
           population_total_all_population=sum(population_total_out_of_workforce,population_total_workforce,na.rm = T)) %>%
    mutate(employed_to_population_ratio=population_total_employed/population_total_all_population,
           unemployed_to_workforce_ratio=population_total_unemployed/population_total_workforce,
           workforce_to_population_ratio=population_total_workforce/population_total_all_population,
           obs_workforce = sum(obs_employed,obs_unemployed,na.rm = T),
           obs_total = sum(obs_workforce,obs_out_of_workforce,na.rm = T)) %>%
    rename(out_of_workforce = population_total_out_of_workforce,
           out_of_workforce_obs = obs_out_of_workforce) %>%
    relocate(population_total_unemployed, .before = population_total_employed) %>%
    relocate(obs_unemployed, .before = obs_employed) %>% 
    pivot_wider(names_from=c({{month}}),values_from=starts_with(c("population_total","obs","unemployed","employed","workforce","out"))) %>%
    select(-starts_with(c("out"))) %>% 
    ungroup()
  if (!rlang::quo_is_null(enquo(group2))){
    by_var <- enquo(group1)
    by_var2 <- enquo(group2)
    data <- data %>% 
      mutate(population_group=paste(!!by_var,!!by_var2,sep="_")) %>% 
      relocate(population_group,.after= shnatseker) %>% 
      select(-c({{group1}},{{group2}}))
  }
  else if (!rlang::quo_is_null(enquo(group1))){
    by_var <- enquo(group1)
    data <- data %>% 
      mutate(population_group = paste(name,!!by_var,sep="")) %>% 
      relocate(population_group,.after= shnatseker) %>% 
      select(-c({{group1}}))
  }
  else {
    data <- data %>% 
    mutate(population_group=name) %>% 
      relocate(population_group,.after= shnatseker)
  }
}

sum_basic_month_and_year_mit <- function(data,group1=NULL,group2=NULL,month=chodeshseker,weight=mishkalsofi,name=NULL,min_year=MAX_YEAR,max_year=MAX_YEAR,min_age=MIN_AGE,max_age=MAX_AGE){
  data <- data %>% 
    filter(shnatseker>=min_year&shnatseker<=max_year&gil>=min_age&gil<=max_age) %>% 
    group_by(shnatseker,{{group1}},{{group2}},{{month}},workforce_attr_inc_mit) %>%
    summarise(population_total=sum({{weight}}),obs=n()) %>%
    filter(obs>=OBS_MIN) %>%
    pivot_wider(names_from=c(workforce_attr_inc_mit),values_from=c(population_total,obs)) %>%
    rowwise() %>% 
    mutate(population_total_workforce=sum(population_total_employed,population_total_unemployed,na.rm = T),
           population_total_all_population=sum(population_total_out_of_workforce,population_total_workforce,population_total_mityaesh,na.rm = T),
           population_total_workforce_expanded = sum(population_total_workforce,population_total_mityaesh,na.rm = T)) %>% 
    mutate(employed_to_population_ratio=population_total_employed/population_total_all_population,
           unemployed_to_workforce_ratio=population_total_unemployed/population_total_workforce,
           mityaesh_to_workforce_expanded_ratio = population_total_mityaesh/population_total_workforce_expanded,
           unemployed_to_workforce_expanded_ratio = population_total_unemployed/population_total_workforce_expanded,
           unemployed_and_mityaesh_to_workforce_expanded_ratio = (population_total_unemployed+population_total_mityaesh)/population_total_workforce_expanded,
           workforce_to_population_ratio=population_total_workforce/population_total_all_population,
           obs_workforce = sum(obs_employed,obs_unemployed,na.rm = T),
           obs_workforce_expanded = sum(obs_workforce,obs_mityaesh,na.rm = T),
           obs_total = sum(obs_workforce,obs_out_of_workforce,na.rm = T)) %>%
    rename(out_of_workforce = population_total_out_of_workforce, 
           out_of_workforce_obs = obs_out_of_workforce) %>%
    relocate(population_total_unemployed, .before = population_total_employed) %>%
    relocate(population_total_mityaesh,.after=population_total_unemployed) %>% 
    relocate(population_total_workforce_expanded,.after=population_total_workforce) %>% 
    relocate(obs_workforce_expanded,.after=obs_workforce) %>%
    relocate(mityaesh_to_workforce_expanded_ratio,.after=unemployed_to_workforce_ratio) %>% 
    relocate(obs_mityaesh,.after=obs_unemployed) %>%
    relocate(obs_unemployed, .before = obs_employed) %>% 
    pivot_wider(names_from=c({{month}}),values_from=starts_with(c("population_total","obs","unemployed","employed","workforce","out","mityaesh"))) %>%
    select(-starts_with(c("out"))) %>% 
    ungroup()
  if (!rlang::quo_is_null(enquo(group2))){
    by_var <- enquo(group1)
    by_var2 <- enquo(group2)
    data <- data %>% 
      mutate(population_group=paste(!!by_var,!!by_var2,sep="_")) %>% 
      relocate(population_group,.after= shnatseker) %>% 
      select(-c({{group1}},{{group2}}))
  }
  else if (!rlang::quo_is_null(enquo(group1))){
    by_var <- enquo(group1)
    data <- data %>% 
      mutate(population_group = paste(name,!!by_var,sep="")) %>% 
      relocate(population_group,.after= shnatseker) %>% 
      select(-c({{group1}}))
  }
  else {
    data <- data %>% 
      mutate(population_group=name) %>% 
      relocate(population_group,.after= shnatseker)
  }
}

#notice that using this function while the current year isnt full yet (for exp having data for only 6 months of current year)
#you'll get bad results because its using weight shanti and not monthly, i.e all weights are divided by 12 (which is ok for full year, but not partial years)
sum_basic_year_only <- function(data,weight=mishkalshnati,group1=NULL,group2=NULL,name=NULL,min_year,max_year=MAX_YEAR,min_age=MIN_AGE,max_age=MAX_AGE){
  data <- data %>% 
    filter(shnatseker>=min_year&shnatseker<=max_year&gil>=min_age&gil<=max_age) %>% 
    group_by(shnatseker,workforce_attr) %>%
    summarise(population_total=sum({{weight}}),obs=n()) %>%
    filter(obs>=OBS_MIN) %>%
    pivot_wider(names_from=c(workforce_attr),values_from=c(population_total,obs)) %>%
    mutate(population_total_workforce=population_total_employed+population_total_unemployed,
           population_total_all_population=population_total_out_of_workforce+population_total_workforce) %>%
    mutate(employed_to_population_ratio=population_total_employed/population_total_all_population,
           unemployed_to_workforce_ratio=population_total_unemployed/population_total_workforce,
           workforce_to_population_ratio=population_total_workforce/population_total_all_population,
           obs_workforce = obs_employed+obs_unemployed,
           obs_total = obs_workforce+obs_out_of_workforce) %>%
    rename(out_of_workforce = population_total_out_of_workforce,
           out_of_workforce_obs = obs_out_of_workforce) %>%
    relocate(population_total_unemployed, .before = population_total_employed) %>%
    relocate(obs_unemployed, .before = obs_employed) %>% 
    select(-starts_with(c("out")))
  if (!rlang::quo_is_null(enquo(group2))){
    by_var <- enquo(group1)
    by_var2 <- enquo(group2)
    data <- data %>% 
      mutate(population_group=paste(!!by_var,!!by_var2,sep="_")) %>% 
      relocate(population_group,.after= shnatseker) %>% 
      select(-c({{group1}},{{group2}}))
  }
  else if (!rlang::quo_is_null(enquo(group1))){
    by_var <- enquo(group1)
    data <- data %>% 
      mutate(population_group = paste(name,!!by_var,sep="")) %>% 
      relocate(population_group,.after= shnatseker) %>% 
      select(-c({{group1}}))
  }
  else {
    data <- data %>% 
      mutate(population_group=name) %>% 
      relocate(population_group,.after= shnatseker)
  }
}

monthly_regular_groups_results <- function(data,min_year,max_year){
  results <- list()
  results[["by_main_population"]] <- sum_basic_month_and_year(data,name="all population",min_year = min_year,max_year = max_year)
  results[["by_gender"]] <- sum_basic_month_and_year(data,group1=sex,min_year = min_year,max_year = max_year)
  results[["by_pop_group"]] <- sum_basic_month_and_year(data,group1=group,min_year = min_year,max_year = max_year)
  results[["by_pop_and_gender"]] <- sum_basic_month_and_year(data,group1=group,group2=sex,min_year = min_year,max_year = max_year)
  results[["by_high_tech"]] <- sum_basic_month_and_year(data,group1=high_tech,min_year = min_year,max_year = max_year)
  results[["by_disabled"]] <- sum_basic_month_and_year(data,group1=disabled,min_year = min_year,max_year = max_year)
  results[["by_disabled_and_gender"]] <- sum_basic_month_and_year(data,group1=disabled,group2=sex,min_year = min_year,max_year = max_year)
  results[["by_young"]] <- sum_basic_month_and_year(data,name="18to24",min_age = 18,max_age = 24,min_year = min_year,max_year = max_year)
  results[["by_young_and_gender"]] <- sum_basic_month_and_year(data,group1=sex,name="18to24_",min_age = 18,max_age = 24,min_year = min_year,max_year = max_year)
  results[["by_45_and_over"]] <- sum_basic_month_and_year(data,name="45_and_over",min_age = 45,max_age = 100,min_year = min_year,max_year = max_year)
  results[["by_45_and_over_and_gender"]] <- sum_basic_month_and_year(data,group1=sex,name="45_and_over_",min_age = 45,max_age = 100,min_year = min_year,max_year = max_year)
  results[["by_62_67"]] <- sum_basic_month_and_year(data,name="62to67",min_age = 62,max_age = 67,min_year = min_year,max_year = max_year)
  results[["by_62_67_and_gender"]] <- sum_basic_month_and_year(data,group1=sex,name="62to67_",min_age = 62,max_age = 67,min_year = min_year,max_year = max_year)
  results[["by_25_54"]] <- sum_basic_month_and_year(data,name="25to54",min_age = 25,max_age = 54,min_year = min_year,max_year = max_year)
  results[["by_25_54_and_gender"]] <- sum_basic_month_and_year(data,group1=sex,name="25to54_",min_age = 25,max_age = 54,min_year = min_year,max_year = max_year) 
  results[["by_55_64"]] <- sum_basic_month_and_year(data,name="55to64",min_age = 55,max_age = 64,min_year = min_year,max_year = max_year)
  results[["by_55_64_and_gender"]] <- sum_basic_month_and_year(data,group1=sex,name="55to64_",min_age = 55,max_age = 64,min_year = min_year,max_year = max_year)
  results <- results
}

monthly_extended_groups_results <- function(data,min_year,max_year){
  results <- list()
  results[["by_main_population"]] <- sum_basic_month_and_year_mit(data,name="all population",min_year = min_year,max_year = max_year)
  results[["by_gender"]] <- sum_basic_month_and_year_mit(data,group1=sex,min_year = min_year,max_year = max_year)
  results[["by_pop_group"]] <- sum_basic_month_and_year_mit(data,group1=group,min_year = min_year,max_year = max_year)
  results[["by_pop_and_gender"]] <- sum_basic_month_and_year_mit(data,group1=group,group2=sex,min_year = min_year,max_year = max_year)
  results[["by_high_tech"]] <- sum_basic_month_and_year_mit(data,group1=high_tech,min_year = min_year,max_year = max_year)
  results[["by_disabled"]] <- sum_basic_month_and_year_mit(data,group1=disabled,min_year = min_year,max_year = max_year)
  results[["by_disabled_and_gender"]] <- sum_basic_month_and_year_mit(data,group1=disabled,group2=sex,min_year = min_year,max_year = max_year)
  results[["by_young"]] <- sum_basic_month_and_year_mit(data,name="18to24",min_age = 18,max_age = 24,min_year = min_year,max_year = max_year)
  results[["by_young_and_gender"]] <- sum_basic_month_and_year_mit(data,group1=sex,name="18to24_",min_age = 18,max_age = 24,min_year = min_year,max_year = max_year)
  results[["by_45_and_over"]] <- sum_basic_month_and_year_mit(data,name="45_and_over",min_age = 45,max_age = 100,min_year = min_year,max_year = max_year)
  results[["by_45_and_over_and_gender"]] <- sum_basic_month_and_year_mit(data,group1=sex,name="45_and_over_",min_age = 45,max_age = 100,min_year = min_year,max_year = max_year)
  results[["by_62_67"]] <- sum_basic_month_and_year_mit(data,name="62to67",min_age = 62,max_age = 67,min_year = min_year,max_year = max_year)
  results[["by_62_67_and_gender"]] <- sum_basic_month_and_year_mit(data,group1=sex,name="62to67_",min_age = 62,max_age = 67,min_year = min_year,max_year = max_year)
  results[["by_25_54"]] <- sum_basic_month_and_year_mit(data,name="25to54",min_age = 25,max_age = 54,min_year = min_year,max_year = max_year)
  results[["by_25_54_and_gender"]] <- sum_basic_month_and_year_mit(data,group1=sex,name="25to54_",min_age = 25,max_age = 54,min_year = min_year,max_year = max_year) 
  results[["by_55_64"]] <- sum_basic_month_and_year_mit(data,name="55to64",min_age = 55,max_age = 64,min_year = min_year,max_year = max_year)
  results[["by_55_64_and_gender"]] <- sum_basic_month_and_year_mit(data,group1=sex,name="55to64_",min_age = 55,max_age = 64,min_year = min_year,max_year = max_year)
  results <- results
}

freq_table <- function(data,var_name){
  x <- plyr::count(data,var_name)
  x<- x %>% mutate(ratio = freq/sum(freq))
}

# 4) function: export -----------------------------------------------------------------------------

gen_col_names_yearly <- function(){
  a <- c("שנה","אוכלוסייה")
  b <- paste("אבטלה מותאם", sep = "_")
  c <- paste("תעסוקה מותאם", sep = "_")
  d <- paste("בכח העבודה", sep = "_")
  e <- paste("סהכ אוכלוסייה", sep = "_")
  new <- c(a,b,c,d,e)
}

gen_col_names_ratio_yearly <- function(){
  a <- c("שנה","אוכלוסייה")
  b <- paste("שיעור אבטלה מותאם", sep = "_")
  c <- paste("שיעור תעסוקה מותאם",  sep = "_")
  d <- paste("שיעור השתתפות בכוח העבודה", sep = "_")
  new <- c(a,b,c,d)
}

#change all population_group column from english to hebrew before exporting
group_names <- function(data){
  data <- data%>%
    mutate(population_group = case_when(
      population_group == "all population" ~ "כלל האוכלוסייה",
      population_group == "female" ~ "נשים",
      population_group == "male" ~ "גברים",
      population_group == "Arab" ~ "ערבים",
      population_group == "Haredi" ~ "חרדים",
      population_group == "Jew_non_Haredi (including Other)" ~ "יהודים שאינם חרדים (כולל אחרים)",
      population_group == "Arab_female" ~ "נשים ערביות",
      population_group == "Arab_male" ~ "גברים ערבים",
      population_group == "Haredi_female" ~ "נשים חרדיות",
      population_group == "Haredi_male" ~ "גברים חרדים",
      population_group == "Jew_non_Haredi (including Other)_female" ~ "נשים יהודיות שאינן חרדיות (כולל אחרים)",
      population_group == "Jew_non_Haredi (including Other)_male" ~ "גברים יהודים שאינם חרדים (כולל אחרים)",
      population_group == "high_tech" ~ "מובטלים בענפי ההייטק",
      population_group == "not_high_tech" ~ "מובטלים שלא בענפי ההייטק",
      population_group == "disabled" ~ "אנשים עם מוגבלות",
      population_group == "not_disabled" ~ "אנשים ללא מוגבלות",
      population_group == "disabled_female" ~ "נשים עם מוגבלות",
      population_group == "disabled_male" ~ "גברים עם מוגבלות",
      population_group == "not_disabled_female" ~ "נשים ללא מוגבלות",
      population_group == "not_disabled_male" ~ "גברים ללא מוגבלות",
      population_group == "18to24" ~ "גילאי 18-24",
      population_group == "18to24_female" ~ "גילאי 18-24 - נשים",
      population_group == "18to24_male" ~ "גילאי 18-24 גברים",
      population_group == "45_and_over" ~ "גילאי 45+",
      population_group == "45_and_over_female" ~ "גילאי 45+ - נשים",
      population_group == "45_and_over_male" ~ "גילאי 45+ - גברים",
      population_group == "62to67" ~ "גילאי 62-67",
      population_group == "62to67_female" ~ "גילאי 62-67 - נשים",
      population_group == "62to67_male" ~ "גילאי 62-67 - גברים",
      population_group == "25to54" ~ "גילאי 25-54",
      population_group == "25to54_female" ~ "גילאי 25-54 - נשים",
      population_group == "25to54_male" ~ "גילאי 25-54 - גברים",
      population_group == "55to64" ~ "גילאי 55-64",
      population_group == "55to64_female" ~ "גילאי 55-64 - נשים",
      population_group == "55to64_male" ~ "גילאי 55-64 - גברים",
    ))
}

#create a vector in order to translate months from numbers to hebrew names
month_heb_abb <- c("ינו", "פבר", "מרץ", "אפר", "מאי", "יוני", "יולי", "אוג", "ספט", "אוק", "נוב", "דצמ")

#create column names in hebrew according to the amount of months
gen_col_names <- function(){
  a <- c("אוכלוסייה")
  b <- paste("אבטלה מותאם", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  c <- paste("תעסוקה מותאם", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  d <- paste("בכח העבודה", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  e <- paste("סהכ אוכלוסייה", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  new <- c(a,b,c,d,e)
}

gen_col_names_ratio <- function(){
  a <- c("אוכלוסייה")
  b <- paste("שיעור אבטלה מותאם", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  c <- paste("שיעור תעסוקה מותאם", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  d <- paste("שיעור השתתפות בכוח העבודה", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  new <- c(a,b,c,d)
}

# 5) import----------------------------------------

wrk_df <- fread("wrk_df_12_21.csv",integer64 = "numeric")

# 6) monthly analysis-----------------------------------------------------------------------------------------------------

results <- monthly_regular_groups_results(wrk_df,min_year = 2021,max_year = 2021)

#export
MONTHS <- 4
#col names (in hebrew) for results_total
new_col_names_total <- gen_col_names()

#col names (in hebrew) for results_ratio
new_col_names_ratio <- gen_col_names_ratio()

#col names (in hebrew) for results_observations
new_col_names_observation <- gen_col_names()

results <- bind_rows(results)

#split the big DF to different tables by their columns, later they will be different excel sheets

results_totals <- results %>%
  select(starts_with(c("population_group","population_total"))) %>% 
  group_names

results_ratios <- results %>%
  select(contains(c("population_group","ratio"))) %>% 
  group_names

results_observations <- results %>%
  select(starts_with(c("population_group","obs"))) %>% 
  group_names

#here the column names are actually replaced
names(results_totals) <- new_col_names_total
names(results_observations) <- new_col_names_observation
names(results_ratios) <- new_col_names_ratio

#create a list of DFS that will be exported as different excel sheets
DATA_NAME <- "סקרי כוח אדם 2021 מעודכן לאפריל"
explanation<- data.frame(ISSUE_DATE,SCRIPT_NAME,DATA_FILE,DATA_NAME)

#create the excel file
mywb_general <- createWorkbook()
#add empty sheets
addWorksheet(mywb_general,"סהכ")
addWorksheet(mywb_general,"שיעורים")
addWorksheet(mywb_general,"תצפיות")
addWorksheet(mywb_general,"הסבר")
#assign data to the empty sheets
writeData(mywb_general,"סהכ",results_totals)
writeData(mywb_general,"שיעורים",results_ratios)
writeData(mywb_general,"תצפיות",results_observations)
writeData(mywb_general,"תצפיות",explanation)
#create specific styles
style_num <- createStyle(numFmt = "COMMA")
stle_per <- createStyle(numFmt = "0.0%")
#replace default styles of cells
addStyle(mywb_general,"סהכ",style = style_num,rows = 2:nrow(results_totals),cols=2:ncol(results_totals),gridExpand = T)
addStyle(mywb_general,"שיעורים",style = stle_per,rows = 2:nrow(results_ratios),cols=2:ncol(results_ratios),gridExpand = T)
addStyle(mywb_general,"תצפיות",style = style_num,rows = 2:nrow(results_observations),cols=2:ncol(results_observations),gridExpand = T)
#adjust column width
setColWidths(mywb_general,"סהכ",cols = 1:ncol(results_totals),widths = "auto")
setColWidths(mywb_general,"תצפיות",cols = 1:ncol(results_observations),widths = "auto")
setColWidths(mywb_general,"שיעורים",cols = 1:ncol(results_ratios),widths = "auto")
#the paste0 allows to add elements to file name, such as output (for saving it in a different location) and current date
saveWorkbook(mywb_general,paste0(OUTPUT,paste("saka21_general_",ISSUE_DATE,".xlsx")),overwrite = T)


# 7) monthly analysis including mityaashim------------------------------------------------------------------------------------------------------------


results_expanded <- monthly_extended_groups_results(wrk_df,min_year = 2021,max_year = 2021)
results_expanded_2020 <- monthly_extended_groups_results(wrk_df,min_year = 2020,max_year = 2020)


#export
results_expanded <- bind_rows(results_expanded)

#split the big DF to different tables by their columns, later they will be different excel sheets

results_totals_expanded <- results_expanded %>%
  select(starts_with(c("population_group","population_total"))) %>% 
  group_names

results_ratios_expanded <- results_expanded %>%
  select(contains(c("population_group","ratio"))) %>% 
  group_names

results_observations_expanded <- results_expanded %>%
  select(starts_with(c("population_group","obs"))) %>% 
  group_names


#create the excel file
mywb_expanded <- createWorkbook()
#add empty sheets
addWorksheet(mywb_expanded,"סהכ")
addWorksheet(mywb_expanded,"שיעורים")
addWorksheet(mywb_expanded,"תצפיות")
#assign data to the empty sheets
writeData(mywb_expanded,"סהכ",results_totals_expanded)
writeData(mywb_expanded,"שיעורים",results_ratios_expanded)
writeData(mywb_expanded,"תצפיות",results_observations_expanded)
#create specific styles
style_num <- createStyle(numFmt = "COMMA")
stle_per <- createStyle(numFmt = "0.0%")
#replace default styles of cells
addStyle(mywb_expanded,"סהכ",style = style_num,rows = 2:(nrow(results_totals_expanded)+1),cols=2:ncol(results_totals_expanded),gridExpand = T)
addStyle(mywb_expanded,"שיעורים",style = stle_per,rows = 2:(nrow(results_ratios_expanded)+1),cols=2:ncol(results_ratios_expanded),gridExpand = T)
addStyle(mywb_expanded,"תצפיות",style = style_num,rows = 2:(nrow(results_observations_expanded)+1),cols=2:ncol(results_observations_expanded),gridExpand = T)
#adjust column width
setColWidths(mywb_expanded,"סהכ",cols = 1:ncol(results_totals_expanded),widths = "auto")
setColWidths(mywb_expanded,"תצפיות",cols = 1:ncol(results_observations_expanded),widths = "auto")
setColWidths(mywb_expanded,"שיעורים",cols = 1:ncol(results_ratios_expanded),widths = "auto")
#the paste0 allows to add elements to file name, such as output (for saving it in a different location) and current date
saveWorkbook(mywb_expanded,paste0(OUTPUT,paste("saka21_expanded_",ISSUE_DATE,".xlsx")),overwrite = T)

#export - 2020
results_expanded_2020 <- bind_rows(results_expanded_2020)

#split the big DF to different tables by their columns, later they will be different excel sheets

results_totals_expanded2020 <- results_expanded_2020 %>%
  select(starts_with(c("population_group","population_total"))) %>% 
  group_names

results_ratios_expanded2020 <- results_expanded_2020 %>%
  select(contains(c("population_group","ratio"))) %>% 
  group_names

results_observations_expanded2020 <- results_expanded_2020 %>%
  select(starts_with(c("population_group","obs"))) %>% 
  group_names


#create the excel file
mywb_expanded2020 <- createWorkbook()
#add empty sheets
addWorksheet(mywb_expanded2020,"סהכ")
addWorksheet(mywb_expanded2020,"שיעורים")
addWorksheet(mywb_expanded2020,"תצפיות")
#assign data to the empty sheets
writeData(mywb_expanded2020,"סהכ",results_totals_expanded2020)
writeData(mywb_expanded2020,"שיעורים",results_ratios_expanded2020)
writeData(mywb_expanded2020,"תצפיות",results_observations_expanded2020)
#create specific styles
style_num <- createStyle(numFmt = "COMMA")
stle_per <- createStyle(numFmt = "0.0%")
#replace default styles of cells
addStyle(mywb_expanded2020,"סהכ",style = style_num,rows = 2:(nrow(results_totals_expanded2020)+1),cols=2:ncol(results_totals_expanded2020),gridExpand = T)
addStyle(mywb_expanded2020,"שיעורים",style = stle_per,rows = 2:(nrow(results_ratios_expanded2020)+1),cols=2:ncol(results_ratios_expanded2020),gridExpand = T)
addStyle(mywb_expanded2020,"תצפיות",style = style_num,rows = 2:(nrow(results_observations_expanded2020)+1),cols=2:ncol(results_observations_expanded2020),gridExpand = T)
#adjust column width
setColWidths(mywb_expanded2020,"סהכ",cols = 1:ncol(results_totals_expanded2020),widths = "auto")
setColWidths(mywb_expanded2020,"תצפיות",cols = 1:ncol(results_observations_expanded2020),widths = "auto")
setColWidths(mywb_expanded2020,"שיעורים",cols = 1:ncol(results_ratios_expanded2020),widths = "auto")
#the paste0 allows to add elements to file name, such as output (for saving it in a different location) and current date
saveWorkbook(mywb_expanded2020,paste0(OUTPUT,paste("saka_expanded_2020_",ISSUE_DATE,".xlsx")),overwrite = T)


# 8) functions: cleaning--------------------------------------------------------------

def_occupations <- function(data){
  data %>% 
    mutate(mishlach2 = substr(semelmishlachyad3,1,2),
           mishlach3 = substr(semelmishlachyad3,1,3),
           branch2 = substr(semelanafkalkali3,1,2),
           branch3 = substr(semelanafkalkali3,1,3))
}

def_sex <- function(data){
  data %>%
    mutate(sex=case_when(
      min==1~"male",
      min==2~"female"))
}

def_midgam_shotef <- function(data){
  data %>% 
    filter(shavuapkida!=0)
}

def_month_pairs <- function(data){
  data %>%
    mutate(month_pairs = case_when(
      chodeshseker==1|chodeshseker==2~"month1-2",
      chodeshseker==3|chodeshseker==4~"month3-4",
      chodeshseker==5|chodeshseker==6~"month5-6",
      chodeshseker==7|chodeshseker==8~"month7-8",
      chodeshseker==9|chodeshseker==10~"month9-10",
      chodeshseker==11|chodeshseker==12~"month11-12",
    ))
}

def_work_attr <- function(data){
  data %>%
    mutate(workforce_attr=case_when(
      muasak==1 ~ "employed",
      muasak==2 ~ "unemployed",
      is.na(muasak) ~ "out_of_workforce"
    ))
  
}

def_pop_groups <- function(data){
  data %>% mutate(group=case_when(
    leom == 2 ~ "Arab",
    ramatdat == 5 ~ "Haredi",
    T ~ "Jew_non_Haredi (including Other)"
  ))
  
}

def_pop_groups_12_13 <- function(data){
  data %>% mutate(group2=case_when(
    leom == 2 ~ "Arab",
    T ~ "Jew"
  ))
  
}

#people who are missing from work because of corona
def_corona_missing <- function(data){
  data %>% 
    mutate(corona_missing = case_when(
      (sibaneedar==9 | sibaneedar==10) &tchunatavodashvuit==3 ~ 1,
      T ~ 0))
}

#define the workforce characteristics of the corona-crisis. can be run on pre-crisis database to recreate "muasak", but only if you define "needarcorona"=0 variable
def_corona_work_attr <- function(data){
  data %>%
    mutate(corona_workforce_attr=case_when(
      corona_missing==1 ~ "unemployed",
      muasak==1 ~ "employed",
      muasak==2 ~ "unemployed",
      is.na(muasak) ~ "out_of_workforce"
    ))
  
}

def_weights <- function(data){
  data %>% 
    mutate(half_m_weights = mishkalsofi/2)
}

def_inconsistances_fix <- function(data){
  data %>% 
    mutate(group=case_when(shnatseker<=2013~group2,T~group)) %>%
    mutate(workforce_attr=case_when(shnatseker<=2019|(shnatseker==2020&chodeshseker<=2)~workforce_attr,T~corona_workforce_attr)) %>% 
    select(-c("group2","corona_workforce_attr"))
}

def_not_working_not_learning <- function(data){
  data %>% 
    mutate(not_learn_not_work = case_when(
      limudimveavoda==5~1,corona_missing==1&limudimveavoda==2~1,T~0
    ),
    learning = case_when(limudimveavoda==1|limudimveavoda==4~1,T~0),
    work_and_learn = case_when(not_learn_not_work==1~"not working and not learning",
                               learning ==1~"learning during or without work",
                               T~"other"))
}
#we are using the broad definition for disabled (anyone who has difficulty with everyday chores)
#for a narrow definition (only when bituach leumi considers it as a disability), see "disabled analysis" at the bottom
def_disabled_broad <- function(data){
  data %>% 
    mutate(disabled=case_when(
      pratmugbalkashe==1|pratmugbalkashe==2~"disabled",
      T~"not_disabled"
    ))
}

#defines who is working in the high tech sector
def_high_tech <- function(data){
  data %>%
    mutate(
      high_tech=case_when(
        branch2==21~"high_tech",
        branch2==26~"high_tech",
        branch2==61~"high_tech",
        branch2==62~"high_tech",
        branch3==303~"high_tech",
        branch3==631~"high_tech",
        branch3==720~"high_tech",
        branch3==721~"high_tech",
        semelanafkalkali3 ==""~"not_high_tech",
        T~"not_high_tech")
    )
}

def_age_group <- function(data){
  data %>% 
    mutate(age_group = case_when(
      gil>=18&gil<=24~"18-24",
      gil>=25&gil<=30~"25-30",
      gil>=31&gil<=40~"31-40",
      gil>=41&gil<=50~"41-50",
      gil>=51&gil<=65~"51-65"
    ))
}

def_age_group_wide <- function(data){
  data %>% 
    mutate(age_group_wide = case_when(
      gil>=18&gil<=24~"18_24",
      gil>=25&gil<=34~"25-34",
      gil>=35&gil<=54~"35_54",
      gil>=55&gil<=64~"55_64",
      gil>=65&gil<=74~"65_74",
      T~"other"))
}

#if the person or one of his parents was born in Ethiopia
def_ethiopian <- function(data){
  data %>%
    mutate(ethiopian=case_when(
      semeleretzleda==250~"ethiopian",
      semeleretzledaav==250~"ethiopian",
      semeleretzledaem==250~"ethiopian",
      T~"not_ethiopian"
    ))
}

#if he is an arab from the south, then he's bedoui
def_bedouin <- function(data){
  data %>%
    mutate(bedouin=case_when(
      (leom==2 & machozmegurim==6) ~"bedouin",
      T~"not_bedouin"
    ))
}

def_work_attr_inc_mit <- function(data){
  data %>% 
    mutate(workforce_attr_inc_mit = case_when(((chodeshhafsakamityaesh>=3&shnathafsakamityaesh==2020)|shnathafsakamityaesh==2021)&(sibahifsiklaavodmityaesh==1|sibahifsiklaavod==1)&tchunatavodashvuit==6~"mityaesh",T~workforce_attr))
}


# 9) clean----------------------------------------

raw <- fread(paste0(DATA_FILE,"saka_2012_2021.csv"),integer64 = "numeric")

wrk_df <- raw %>% 
  def_occupations() %>% 
  def_month_pairs() %>% 
  def_sex() %>% 
  def_work_attr() %>% 
  def_midgam_shotef() %>% 
  def_pop_groups_12_13() %>% 
  def_pop_groups() %>% 
  def_corona_missing() %>% 
  def_corona_work_attr %>% 
  def_weights %>% 
  def_inconsistances_fix %>% 
  def_not_working_not_learning %>% 
  def_age_group %>% 
  def_age_group_wide %>% 
  def_disabled_broad %>% 
  def_high_tech %>% 
  def_work_attr_inc_mit
  #def_ethiopian 
  # %>% def_bedouin

fwrite(wrk_df,"wrk_df_12_21.csv")


