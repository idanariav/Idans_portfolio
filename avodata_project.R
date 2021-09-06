
# 1) Set-up: Global Vars --------------------------------------------------

#tell R where to save and load files from
setwd( "D:\\Research\\RoniS_12730180_Research\\IdanA")

#these vars are useful for some parts of the code (mostly export and analysis), and for convenience its easier to put them all in one place
#you must make sure that all the relevant source databases are in this folder. especially important when including new files
USED_SOURCES <- ".\\input_data\\used\\" # the ".\\" means that its relative to the "wd" you defined, for exp - here its "in your wd, look for the "input data" folder..."
PROCCESED_SOURCES <- ".\\input_data\\proccesed_input\\" #where proccessed inputs will be saved for later(such as combined yearly saka files)
TEST_DATA <- ".\\input_data\\TEST_DATA\\" #here is where i keep merav's files
OUTPUTS <- ".\\OUTPUTS\\" #where OUTPUTS will be saved
MIN_OBS <- 5 #rows with lower amount of obs will be deleted
MIN_OBS_COMPLETE <- 5 #rows with lower amount of obs will be considered as creating privacy problems and need to be deleted because of it
MAIN_MISHLACH_OBS <- 15 #if main mishlachi yad have to be subjected to a different level of filtering by obs, then this allows it. otherwise, write =MIN_OBS
PERCENTILE_MIN_OBS <- 20 #how many obs are needed to give an estimate of percentiles
MEDIAN_MIN_OBS <- 10 #how many obs are needed to give an estimate of median wage
MIN_YEAR <- 2012 #some functions are based on certain year or num of years. therefore its important to keep this updated
MAX_YEAR <- 2019 #some functions are based on certain year or num of years. therefore its important to keep this updated
Z_FILTER <- 4 #amount of years a certain mishlach yad has to be without wage/employed diviation of more than 1.5 z score in order to not be filtered. the more years you have, the more likley a certain mishlach yad will reach this z filter
Z_FILTER2 <- 8 #ids with z score higher than this (meaning the distance between their wage and the avg_wage of their mishlach yad) will be removed
Z_YEARLY_DIVIATION <- 1.5 #if (per mishlach) the avg wage is above\below X SD for the general avg wage difference (per year), than mark it
MIN_AGE <- 25 #min age of avodata population
WORK_WEEKS_PER_MONTH <- 4.28 #this is the amount of work weeks per month (because in saka the question on work hours is weekly, but the wage is monthly)
RELEVANT_COLUMNS <- #this takes only the relevant columns necessary from Saka, useful for rememebring what's relevant and improve code run speed (makes the data smaller)
  c("shnatseker","semelmishlachyad","mishkalshnati","misparzehut_fic","leom","ramatdat","semelanafkalkali","gil","machozmegurim",
                      "machozyishuvavoda","nayadutyishuvavoda","min","anafkalkalink","muasak","teudagvoha","shaotbederechklal","shaotavodalemaase","shavuapkida")
PACKAGES <- c("crayon","dplyr","data.table","forcats","tidyr","openxlsx","haven","readr","cli","utf8","stringi","magrittr","janitor") #all the list of packages to be loaded, in some cases the loading order of the packages is important, so don't switch the order without reason
LIBLOC<- "D:\\Research\\RoniS_12730180_Research\\r_packages\\3.6" #point to the location where the packages are installed. verify that this location is correct otherwise the packages wont load


#turn of scientific notation
options(scipen=999)


# 2) Set-up: Loading Packages ---------------------------------------------

# using libraries in the research room is different. the location of the packages has to be specified (lib.loc). see following code
lapply(PACKAGES,library,lib.loc = LIBLOC,character.only = T)
requireNamespace("plyr",lib.loc = LIBLOC) #needed only for few specefic purposes and has a lot of "conflicts" with "dplyr", thats why i'm using
requireNamespace("rlang",lib.loc = LIBLOC) #needed only for few specefic purposes and perhaps has "conflicts"
#"requirenamesspace" instead of library. that means that i can use the packages, but only if i include the name before, such as plyr::rbind.fill, which avoids the conflicts.

#crayon - #for some reason this is necessary to load "dplyr" package
#dplyr - #amazing package for cleaning/modifying data, can't work without it
#data.table - allows fast reading/writing/modifying csv files (very noticeable difference on cbs computers, espcially on large files +1 million rows)
#forcats - for some reason this is necessary to load "tidyr" package
#tidyr - tidyr - relevant for "pivot_wide" - all the summaries in this file
#writexl - allows exporting for excel, doesn't require java or other packages so you can use it on cbs computers
#cli - necessary for glimpse function (similar to "str" but more informative)
#utf8 - necessary for glimpse function
#stringi - necessary for "pivot_longer"
#rlang - necessary for checking if arguments are null (for the summ_saka function)
#magrittar - allows the "%<>% symbol, which is equivalent to data <- data %>%
#haven - relevant for importing spss/stata files

# 3) Functions: summaries -------------------------------------------------


#this functions will create the basic "avodata" result table for each type of population.
# since each group of columns require a different filter of rows, the data is divided into
# several different DF, which are later merged.
# the following explanation is for each of those DF:
# z: hourly wage data is the most intensive filter on the data, since it requires not only wage data from ITA,
#   but of data on work hours in saka. therefore this DF is used to calculate the OBS_distinct, which will later be 
#   used to filter and examine the rest of the data per the mishlach-yad
# t1: this DF represent the smallest amount of filtering. just any obs with either data from ITA (income data) or defiend as employed in saka
# t2: gives the analysis by branchs, i.e branches in columns (2 digits)
# y: this DF is the medium amount of filtering, removing obs with extereme or non-existent income data
# the syntax of this function is based on data.table packages, since it improved preformance and runtime
# significantly. I advise keeping using this syntax instead of dplyr, even though its less intuative.
# the "..." which appears in the function arguments means any (including 0) arguments which are passed
# to the function that are not the other named arguments (such as "category name")
# each time the "..." appears in the function, it refrences those added arguments that were passed
# specifically, I use the "..." arguments to add the grouping vars
# the reason I also have "group1" and "group2" is because I need those grouping vars in both as a string
# and as a named object (without "")

summ_saka_any_var <- function(data,...,pop_name=NULL,category_name,group1=NULL,group2=NULL){
  paste_vars <- enquos(pop_name,group1,group2) #this will be later used as the population name based on the grouping columns. such as pop_group+gender= arab_female
  count <- length(enquos(...)) #this counts how many grouping vars are used in the function. important later for removing compleating rows.
  setkey(data,shnatseker,semelmishlachyad,...) #defines columns for the original data that will later be used for merging
  #Z DF - hourly wage/work hours data - highest level of filtering
  z <- data[(!is.na(wage_by_hour))&z_remove_hour==0&z_remove==0, #filter the data
            .(obs_distinct=length(unique(misparzehut_fic,na.rm = T)), #create obs_distinct column
              hourly_wage = weighted.mean(wage_by_hour, mishkalshnati, na.rm = T), #create hourly_wage column
              avg_work_hours = weighted.mean(hours_worked, mishkalshnati, na.rm = T)), #create avg_work_hours column
            by=c("shnatseker","semelmishlachyad",...)] # calculate columns by groups
  #t1 DF - amount of workers - lowest level of filtering
  t1 <- data[,.(amt_workers = sum(mishkalshnati)/1000), #create amt_workers column (by thousands)
             by=c("shnatseker","semelmishlachyad",...)] # calculate columns by groups
  #y DF - wage data  - medium level of filtering
  y <- data[z_remove==0, #filter data
            .(avg_wage = weighted.mean(total_monthly_earning, mishkalshnati, na.rm = T), #create avg_wage column
              percentile_25 = plyr::round_any(quantile(total_monthly_earning, probs = c(0.25),na.rm = T),1000), #create percentile_25 column, rounded to the nearest 1000
              percentile_50 = plyr::round_any(quantile(total_monthly_earning, probs = c(0.50),na.rm = T),1000), #create percentile_50 column, rounded to the nearest 1000
              percentile_75 = plyr::round_any(quantile(total_monthly_earning, probs = c(0.75),na.rm = T),1000)), #create percentile_75 column, rounded to the nearest 1000
            by=c("shnatseker","semelmishlachyad",...)] # calculate columns by groups
  # the following lines are merging with data.table syntax. silimar to left_join where the DT inside the brackets is the main DT,i.e all its rows will be kept.
  # from the DT outside the brackets, only matching rows will be merged
  y <- z[y]
  y <- t1[y]
  y %<>% tidyr::complete({{group1}},{{group2}},shnatseker,semelmishlachyad) #this creates empty rows for factors without obs
  #such as cases when there are no obs for arabs/public sector... so that an empty row would appear with NA. similar to .drop=F with dplyr
  y %<>% mutate(population_group := paste(!!!paste_vars,sep="_")) #add the population_group column
  setDT(y)
  y[,population_group := gsub("*_$","",population_group)] #sometimes there is an extra "_" at the begining or end of the string. this removes it
  y[,population_group := gsub("^_*","",population_group)]
  y[,category := category_name] # add the category column
  y <- y[population_group!=""&population_group!="NA"&population_group!="_"&semelmishlachyad!=""&semelmishlachyad!="."] #remove unwanted rows
  y[is.na(obs_distinct),obs_distinct:=0] #for those empty rows that were added with "complete", mark the obs as 0
  setcolorder(y,neworder = c("shnatseker","semelmishlachyad","category","population_group","amt_workers","obs_distinct","avg_wage","percentile_25",
                             "percentile_50","percentile_75","hourly_wage","avg_work_hours")) #set new order of columns. columns that are not mentioned will be added at the end of this list by default
  #this "if" is based on how many grouping vars are there. if there's 2, a special completing rows function will be needed
  if (count>1){
    y %<>%
      del_completing_rows_interaction(.,{{group1}},{{group2}}) %>% #if it has more than 1 grouping vars, run special row compleating function
      as.data.table()
  }
  else {
    y %<>%
      select(-c(...)) %>% 
      as.data.table()
  }
}

#merges the result table with the matching "truth" table so you could "update downwards" which sub groups should be deleted (if their parent group has been deleted)
# for example, if the "Haredi" parent group should be deleted, then Haredi by gender should also be deleted.
merge_sums <- function(data,chart,var,varname){
  chart <- chart %>% 
    select(c("shnatseker","semelmishlachyad",varname))
  data <- data %>% 
    merge(chart,by = c("shnatseker","semelmishlachyad"),all.x = T)
  data <- data %>% 
    mutate(delete_group = case_when(
      delete_group+{{var}} >=1~1,T~delete_group)) %>% #if it's parent group (marked by the "var" column) needs to be deleted, then mark it for deletion
    select(-c(varname))
}



#this gets the table which shows which mishlachi yad should be filtered by their z_score i.e level of diviation (of avg wage)
gen_month_check_avg_wage <- function(data){ 
  x<-data %>% 
    select("shnatseker","semelmishlachyad","avg_wage","population_group") %>% 
    filter(population_group=="all population") %>% #i take only the "total" row of each mishlach yad
    mutate(shnatseker = as.numeric(shnatseker))
  y<-x %>% 
    mutate(shnatseker = shnatseker+1) %>% #get next years wage
    select(-population_group) %>% 
    filter(shnatseker<=MAX_YEAR) %>% 
    rename(avg_wage_last_year = avg_wage)
  month_check <- merge(x,y,by = c("shnatseker","semelmishlachyad"),all.x=T) %>%
    filter(shnatseker>MIN_YEAR) %>% 
    mutate(avg_diff = (avg_wage/avg_wage_last_year)-1) %>% #calculate the difference between current and last years avg_wage
    group_by(shnatseker) %>% 
    mutate(z_score = (avg_diff - mean(avg_diff,na.rm = T))/sd(avg_diff,na.rm = T)) %>% #calculate z score for each mishlach yad per year
    ungroup() %>% 
    group_by(semelmishlachyad) %>%
    mutate(count_z = case_when(
      abs(z_score)>=Z_YEARLY_DIVIATION ~ 1,T~0)) %>% 
    summarise(count_z_avg_wage = sum(count_z,na.rm = T))
  month_check <-month_check #this is so that in the end the function would return the month check table
} 

#this gets the table which shows which mishlachi yad should be filtered by their z_score  i.e level of diviation (of amount of workers)
gen_month_check_workers <- function(data){ 
  x<-data %>% 
    select("shnatseker","semelmishlachyad","amt_workers","population_group") %>% 
    filter(population_group=="all population") %>% #i take only the "total" row of each mishlach yad
    mutate(shnatseker = as.numeric(shnatseker))
  y<-x %>% 
    mutate(shnatseker = shnatseker+1) %>% #get next years amount of workers
    select(-population_group) %>% 
    filter(shnatseker<=MAX_YEAR) %>% 
    rename(amt_workers_last_year = amt_workers)
  month_check <- merge(x,y,by = c("shnatseker","semelmishlachyad"),all.x=T) %>%
    filter(shnatseker>MIN_YEAR) %>% 
    mutate(avg_diff = (amt_workers/amt_workers_last_year)-1) %>% #calculate the difference between current and last years avg_wage
    group_by(shnatseker) %>% 
    mutate(z_score = (avg_diff - mean(avg_diff,na.rm = T))/sd(avg_diff,na.rm = T)) %>% #calculate z score for each mishlach yad per year
    ungroup() %>% 
    group_by(semelmishlachyad) %>%
    mutate(count_z = case_when(
      abs(z_score)>=Z_YEARLY_DIVIATION ~ 1,T~0)) %>% 
    summarise(count_z_workers = sum(count_z,na.rm = T))
  month_check <-month_check #this is so that in the end the function would return the month check table
} 

#this gets the table which shows which mishlachi yad should be filtered by their z_score i.e level of diviation (of hourly wage)
gen_month_check_hourly_wage <- function(data){ 
  x<-data %>% 
    select("shnatseker","semelmishlachyad","hourly_wage","population_group") %>% 
    filter(population_group=="all population") %>% #i take only the "total" row of each mishlach yad
    mutate(shnatseker = as.numeric(shnatseker))
  y<-x %>% 
    mutate(shnatseker = shnatseker+1) %>% #get next years hourly wage
    select(-population_group) %>% 
    filter(shnatseker<=MAX_YEAR) %>% 
    rename(hourly_wage_last_year = hourly_wage)
  month_check <- merge(x,y,by = c("shnatseker","semelmishlachyad"),all.x=T) %>%
    filter(shnatseker>MIN_YEAR) %>% 
    mutate(avg_diff = (hourly_wage/hourly_wage_last_year)-1) %>% #calculate the difference between current and last years avg_wage
    group_by(shnatseker) %>% 
    mutate(z_score = (avg_diff - mean(avg_diff,na.rm = T))/sd(avg_diff,na.rm = T)) %>% #calculate z score for each mishlach yad per year
    ungroup() %>% 
    group_by(semelmishlachyad) %>%
    mutate(count_z = case_when(
      abs(z_score)>=Z_YEARLY_DIVIATION ~ 1,T~0)) %>% 
    summarise(count_z_hourly_wage = sum(count_z,na.rm = T))
  month_check <-month_check #this is so that in the end the function would return the month check table
} 
# this function and the "adjust results" both clean obs that should be deleted.
# the reason these functions are seperated is becuase there are some tables that I need
# to create based on mishlachy yad with enough obs (after removing delete=1) but before
# i "unnecesarly" remove obs that should only be deleted because of compleating rows requirements
# thats because I'm only getting summarised data ungrouped mishlach yad, so no risk for using compleating rows data
# after those tables are created, you can run the "adjust results" function
primary_clean_results <- function(data){
  data <- data[!grepl("X",semelmishlachyad)] #filter unknown mishlach yad
  data[,nchar := nchar(as.character(semelmishlachyad))] #helper column
  data <- data[nchar==4] #make sure that only 4 digits mishlachi yad are in the results (in the data there are some that arent)
  #where delete is 1 (meaning yes), because of low obs, turn those columns into empty columns
  data[delete==1, c("amt_workers","avg_wage","percentile_25","percentile_50","percentile_75","hourly_wage","avg_work_hours") := NA] 
  data[,nchar:=NULL] #delete the nchar column
}

#actual deletion of obs that should be deleted because of compleating rows (previously they were just marked for deletion) or other conditions
adjust_results <- function(data){
  data[obs_distinct<MEDIAN_MIN_OBS,c("percentile_50") := NA] #median wage has a different amount of min obs needed 
  data[obs_distinct<PERCENTILE_MIN_OBS,c("percentile_25","percentile_75") := NA] #percentiles has a different amount of min obs needed
  #where delete group is 1 (meaning yes) because of completing rows, turn those columns into empty columns
  data[delete_group==1|obs_distinct<MIN_OBS, c("amt_workers","avg_wage","percentile_25","percentile_50","percentile_75","hourly_wage","avg_work_hours") := NA]
  #if main mishlachi yad have a higher standart of obs than the sub-groups, this allows deletion by that standart.
  data[category=="all population"&obs_distinct<MAIN_MISHLACH_OBS, c("amt_workers","avg_wage","percentile_25","percentile_50","percentile_75","hourly_wage","avg_work_hours") := NA]
  data <- data[(population_group!="X")&(population_group!="u")&(population_group!="T"),]
  data %>% 
    select(-c("delete","delete_group")) %>% 
    pivot_wider(names_from = c("shnatseker"),values_from = c("amt_workers","obs_distinct","avg_wage","percentile_25","percentile_50","percentile_75","hourly_wage","avg_work_hours")) #turn data from long to wide, so the years wont be different rows but columns
}

#the separation of different results table (by name pattern) is to work by the different completing rows requirements
get_results <- function(data){
  #in general, the prefix for each table, such as "rslt_", "diff_", "other" is so that later we could merge them
  #by name and activate a different function of compleating rows.
  #get all necesary result tables by different groups
  rslt_all_population <- summ_saka_any_var(data,category_name = "all population",pop_name = "all population")  %>% mutate(population_group = gsub("*_$","",population_group)) %>% as.data.table()
  rslt_gender <- summ_saka_any_var(data, group1=sex, category_name = "gender","sex")
  rslt_age_group_1 <- summ_saka_any_var(data, group1=age_group1, category_name ="age","age_group1")
  rslt_age_group_2 <- summ_saka_any_var(data, group1=age_group2, category_name ="age","age_group2")
  rslt_living_district <- summ_saka_any_var(data, group1=living_district, category_name ="living district","living_district")
  rslt_work_district <- summ_saka_any_var(data, group1=work_district, category_name ="work district","work_district")
  rslt_worker_type <- summ_saka_any_var(data, group1=type_of_worker, category_name ="work status by main income","type_of_worker")
  else_mobility <- summ_saka_any_var(data, group1=work_mobility, category_name ="work mobility","work_mobility")
  rslt_anaf <- summ_saka_any_var(data, group1=occupation_sector, category_name ="occupation sector","occupation_sector")
  rslt_high_tech <- summ_saka_any_var(data, group1=high_tech, category_name ="high tech","high_tech")
  rslt_public_sector <- summ_saka_any_var(data, group1=public_sector, category_name ="public sector","public_sector")
  rslt_up_to_bagrut_and_bagrut <- summ_saka_any_var(data, group1=edu_diploma, category_name ="education","edu_diploma")
  rslt_up_to_bagrut_inc <- summ_saka_any_var(data, group1=edu_diploma2, category_name ="education","edu_diploma2")
  rslt_education_above_bagrut <- summ_saka_any_var(data, group1=edu_diploma3, category_name ="education","edu_diploma3")
  rslt_higher_education <- summ_saka_any_var(data, group1=high_edu, category_name ="higher education","high_edu")
  rslt_higher_education2 <- summ_saka_any_var(data, group1=high_edu2,category_name ="higher education","high_edu2")
  #the pop groups are separated because the "remove compleating rows" process is different for them
  diff_pop_1 <- summ_saka_any_var(data, group1=pop_group, category_name ="pop_group","pop_group")
  diff_pop_2 <- summ_saka_any_var(subset(data,pop_group2!="arab"), category_name ="pop_group",pop_name = "jews_total","pop_group2") %>% mutate(population_group = gsub("*_$","",population_group)) %>% as.data.table()
  #the pop by gender results are separated because we need to "update downwards" in case one of the parent group is marked for deletion
  pop_by_gender <- summ_saka_any_var(data,group1=pop_group,group2=sex,category_name = "pop_group by gender","pop_group","sex")
  other_pop_all_jews_by_gender <- summ_saka_any_var(subset(data,pop_group2=="jew_total"),group1=sex,category_name ="pop_group by gender",pop_name ="jew_total","sex")
  other_pop_jews_by_gender <- pop_by_gender[grepl("jew_non_haredi",population_group),]
  other_pop_arabs_by_gender <- pop_by_gender[grepl("arab",population_group),]
  other_pop_haredi_by_gender <- pop_by_gender[grepl("^haredi",population_group),]
  remove(pop_by_gender)
  #for parent poplation group
  all_pop_results <- rbindlist(list(diff_pop_1,diff_pop_2))
  all_pop_results <- del_completing_pop_rows(all_pop_results)
  del_pop_chart <-  del_completing_pop_rows_1(all_pop_results)
  #for mobility
  else_mobility <- del_row_non_completing(else_mobility)
  #for the pop by gender groups
  other_pop_all_jews_by_gender %<>% 
    del_completing_rows() %>% 
    merge_sums(.,del_pop_chart,jews_total,"jews_total")
  other_pop_jews_by_gender %<>% 
    merge_sums(.,del_pop_chart,jew_non_haredi,"jew_non_haredi")
  other_pop_arabs_by_gender %<>% 
    merge_sums(.,del_pop_chart,arab,"arab")
  other_pop_haredi_by_gender %<>% 
    merge_sums(.,del_pop_chart,haredi,"haredi")
  all_sub_pop <- mget(ls(pattern = "other_"))
  all_sub_pop_results <- rbindlist(all_sub_pop)
  #for the rest of the group
  #turn all results into a list
  all_summs <- mget(ls(pattern = "rslt_"))
  all_summs <- lapply(all_summs,del_completing_rows)
  #for the age group
  del_age_chart <- del_parent_chart(all_summs[["rslt_age_group_1"]])
  all_summs[["rslt_age_group_2"]] <- all_summs[["rslt_age_group_2"]] %>% 
    merge_sums(.,del_age_chart,age_45_and_over,"age_45_and_over")
  #finalise the unified results table into the disiered output
  all_results <- rbindlist(all_summs)
  #unite all the different tables after they had their matching compleating rows function
  results <- rbindlist(list(all_results,all_pop_results,all_sub_pop_results,else_mobility),use.names=TRUE)
  results <- primary_clean_results(results)
  month_check_avg_wage <- as.data.table(gen_month_check_avg_wage(results)) #get a summary table which checks which mishlachi yad should be filtered by their z score (by avg_wage)
  month_check_workers <- as.data.table(gen_month_check_workers(results)) #get a summary table which checks which mishlachi yad should be filtered by their z score (by amt_workers)
  month_check_hourly_wage <- as.data.table(gen_month_check_hourly_wage(results)) #get a summary table which checks which mishlachi yad should be filtered by their z score (by hourly wage)
  results <- month_check_avg_wage[results, on="semelmishlachyad"]
  results <- month_check_workers[results, on="semelmishlachyad"]
  results <- month_check_hourly_wage[results, on="semelmishlachyad"]
  results <- results[(count_z_avg_wage<Z_FILTER)&(count_z_workers<Z_FILTER)]
  results[count_z_hourly_wage>=Z_FILTER,c("hourly_wage","avg_work_hours") := NA] #in contrast to the former "gen month check" here only the hourly wage data is deleted and not the whole mishlach yad
  results[,c("count_z_avg_wage","count_z_workers","count_z_hourly_wage") := NULL]
  results <- adjust_results(results)
}




# 4) Functions: Compleating Rows and Export -------------------------------

#in the following functions I intentionally ignore when "obs_distinct"==0 for checking criteria of compleating rows because when a group is empty there is no risk of privacy violation, 
# and after that I include those cases back for deletion

#mark rows for deletion if its based on low amount of obs or matches certain criterions for completing rows
del_completing_rows <- function(data){
  data[,delete:=0][obs_distinct<MIN_OBS_COMPLETE&obs_distinct!=0, delete:=1]
  data[,rows:=seq_len(.N),by=c("shnatseker","semelmishlachyad")]
  data[,delete_group:=fifelse((sum(delete)>0&max(rows)==2)|(max(rows)>2&sum(delete)==1&sort(amt_workers,F)[2]==amt_workers),1,0),by=c("shnatseker","semelmishlachyad")]
  data[,rows:=NULL]
  data[obs_distinct==0, delete:=1]
  #if it is has 2 groups & at least 1 has to be deleted, then also delete the other
  #or, if it has more than 2 groups and only one has to be deleted, the group that should be deleted is the group with the second lowest amount of workers, than mark it for deletion (because the first lowest is the one to be deleted anyway)
  #which cases were left as 0 (no need to delete): if none has to be deleted (sum(delete)==0), or if it has more than two groups and more than one has to be deleted
}

#mark only rows with low obs to be deleted, without treating completing rows
del_row_non_completing<-function(data){
  data[,delete:=0][obs_distinct<MIN_OBS_COMPLETE&obs_distinct!=0, delete:=1]
  data[,delete_group:=0]
}

#creates a "truth" table about whether a certain group should be deleted by year&mishlach yad
del_parent_chart <- function(data){
  data %>%
    mutate(delete = case_when(obs_distinct>=MIN_OBS_COMPLETE|obs_distinct==0 ~ 0 , T ~ 1)) %>% 
    mutate(delete = case_when(
      delete+delete_group >= 1~1,T~0)) %>% #if he should be deleted from either source, mark him for deletion
    select(c("shnatseker","semelmishlachyad","population_group","delete")) %>% 
    pivot_wider(names_from = population_group,values_from = delete) #turn it from wider to longer, so now every row (which represents a mishlach yad~year)
  #has a binary column for every group if it should be deleted (useful for checking deletion combinations, such as with jews\arabs\haredi)
}

#this function is separated from the next one so i could create the "del_pop_chart", which is the key table that allows me to manage which pop groups should be removed 
del_completing_pop_rows_1 <- function(data){
  data %>% 
    group_by(semelmishlachyad,shnatseker) %>% 
    mutate(delete = case_when(obs_distinct>=MIN_OBS_COMPLETE|obs_distinct==0 ~ 0 , T ~ 1)) %>% 
    select(c("semelmishlachyad","shnatseker","population_group","delete")) %>% 
    pivot_wider(names_from = population_group,values_from = delete) %>% 
    mutate(haredi = case_when(arab==1 ~1,T~haredi), #if arab not muvhak -  remove jews total and haredi - leave only jews non haredi
           jews_total = case_when(arab==1 ~1,T~ jews_total),
           jew_non_haredi = case_when(haredi==1&arab==0~1,T~jew_non_haredi), #if haredi not muvhak and jews muvhak- leave only jew total
           arab = case_when(arab==0&jews_total==1 ~ 1,T~arab)) #if only arab remains, remove arab (because we can get the "jews total" from arabs)
}

del_completing_pop_rows_2 <- function(data){
  data %>% 
    pivot_longer(col = c("arab","haredi","jew_non_haredi","jews_total"), names_to = c("population_group"), values_to = c("delete")) %>% 
    mutate(delete_group = delete)
}

#this unifies the two previous functions to get the unified pop groups results table
del_completing_pop_rows <- function(data){
  t1 <- data %>% 
    del_completing_pop_rows_1() %>% 
    del_completing_pop_rows_2()
  results <- merge(data,t1,by = c("shnatseker","semelmishlachyad","population_group"),all.x = T)
  results %<>% mutate(delete = case_when(obs_distinct==0~1,T~delete))
}

#in case there are two grouping vars, i special check for completing rows is required.
# for example, population group by gender has two different parents from which those rows
# can be completed. the first is the pop parent group (such as total arab,haredi, etc...)
# and the second is gender parent group (such as total men, total women)
#therefore, it is enough that one sub group is missing (such as arab_female)
# that not only arab male is needed to be deleted, but also an entire other group
del_completing_rows_interaction <- function(data,group1,group2){
  data %>% 
    mutate(delete = case_when(obs_distinct>=MIN_OBS_COMPLETE|obs_distinct==0 ~ 0 , T ~ 1)) %>% #creates the basic delete column
    ungroup() %>% 
    group_by(shnatseker,semelmishlachyad,{{group1}}) %>% 
    mutate(total_group = sum(amt_workers),delete_group=case_when(sum(delete)>=1~1,T~0)) %>% #first do a locate "delete_group" i.e delete the
    # first completing rows. such as if arab_female should be deleted, then also mark arab_male for deletion
    ungroup() %>% 
    group_by(shnatseker,semelmishlachyad) %>%
    #this expands the delete_group according to the logic we presented. if a certain group has to be deleted, then delete the next one based on lowest amount of workers
    mutate(delete_group=case_when(sum(delete_group)>=1&sort(total_group,F)[3]==total_group~1,T~delete_group)) %>%
    select(-c("total_group")) %>% 
    select(-c({{group1}},{{group2}}))
  
}

#convert english population group names to hebrew, the "paste" is to add quotations to specific cells
group_names <- function(data){
  data %>%   
    mutate(population_group_heb = case_when(
      population_group == "all population" ~ paste("סה",'"',"כ עובדים במשק",sep=""),
      population_group == "female" ~ "נשים כללי",
      population_group == "male" ~ "גברים כללי",
      population_group == "25-34" ~ "25-34",
      population_group == "35-44" ~ "35-44",
      population_group == "age_45_and_over" ~ "45+",
      population_group == "45-54" ~ "45-54",
      population_group == "age_55_and_over" ~ "55+",
      population_group == "center and tel aviv living" ~ paste("מרכז+ת",'"',"א מגורים",sep=""),
      population_group == "jerusalem and yosh living" ~ paste("ירושלים+יו",'"',"ש מגורים",sep=""),
      population_group == "north and haifa living" ~ "צפון+חיפה מגורים",
      population_group == "south living" ~ "דרום מגורים",
      population_group == "center and tel aviv work" ~ paste("מרכז+ת",'"',"א עבודה",sep=""),
      population_group == "jerusalem and yosh work" ~ paste("ירושלים+יו",'"',"ש עבודה",sep=""),
      population_group == "north and haifa work" ~ "צפון+חיפה עבודה",
      population_group == "south work" ~ "דרום עבודה",
      population_group == "unknown or unpermanent" ~ "לא קבוע או לא ידוע עבודה",
      population_group == "freelancer by main inc" ~ "עצמאים לפי הכנסה עיקרית",
      population_group == "wage worker by main inc" ~ "שכירים לפי הכנסה עיקרית",
      population_group == "works in living settlement" ~ "עובד ביישוב המגורים",
      population_group == "works in more than one settlement" ~ "עובד ביותר מיישוב אחד",
      population_group == "works outside living settlement" ~ "עובד מחוץ ליישוב המגורים",
      population_group == "A" ~ "חקלאות, ייעור ודיג",
      population_group == "B" ~ "כרייה וחציבה",
      population_group == "C" ~ "תעשייה וחרושת",
      population_group == "D" ~ "אספקת חשמל, גז, קיטור ומיזוג אוויר",
      population_group == "E" ~ "אספקת מים; שירותי ביוב, טיפול באשפה ובפסולת ושירותי טיהור",
      population_group == "F" ~ "בינוי",
      population_group == "G" ~ "מסחר סיטוני וקמעוני; תיקון כלי רכב מנועים ואופנועים",
      population_group == "H" ~ "שירותי תחבורה, אחסנה, דואר ובלדרות",
      population_group == "I" ~ "שירותי אירוח ואוכל",
      population_group == "J" ~ "מידע ותקשורת",
      population_group == "K" ~ "שירותים פיננסיים ושירותי ביטוח",
      population_group == "L" ~ paste("פעילויות בנדל",'"',"ן",sep=""),
      population_group == "M" ~ "שירותים מקצועיים, מדעיים וטכניים",
      population_group == "N" ~ "שירותי ניהול ותמיכה",
      population_group == "O" ~ "מינהל מקומי, מינהל ציבורי וביטחון; ביטוח לאומי חובה",
      population_group == "P" ~ "חינוך",
      population_group == "Q" ~ "שירותי בריאות ושירותי רווחה וסעד",
      population_group == "R" ~ "אמנות, בידור ופנאי",
      population_group == "S" ~ "שירותים אחרים",
      population_group == "high_tech" ~ "ענף הייטק",
      population_group == "not_high_tech" ~ "לא ענף הייטק",
      population_group == "not public sector" ~ "לא מגזר ציבורי",
      population_group == "public sector" ~ "מגזר ציבורי",
      population_group == "bagrut" ~ "ת.בגרות",
      population_group == "less than bagrut" ~ "עד ת.בגרות (לא כולל)",
      population_group == "up to bagrut" ~ "עד ת.בגרות (כולל)",
      population_group == "proffesional traning+mahat+above bagrut" ~ paste("ת.על תיכונית+הכשרה מקצועית+מה",'"',"ט",sep=""),
      population_group == "B.A" ~ "תואר ראשון",
      population_group == "M.A+" ~ "תואר שני ומעלה",
      population_group == "B.A+" ~ "תואר ראשון ומעלה",
      population_group == "jews_total" ~ paste("יהודים סה",'"',"כ כללי",sep=""),
      population_group == "jew_total_female" ~ paste("נשים יהודיות סה",'"',"כ",sep=""),
      population_group == "jew_total_male" ~ paste("גברים יהודים סה",'"',"כ",sep=""),
      population_group == "jew_non_haredi" ~ "יהודים שאינם חרדים כללי",
      population_group == "jew_non_haredi_female" ~ "נשים יהודיות שאינן חרדיות",
      population_group == "jew_non_haredi_male" ~ "גברים יהודים שאינם חרדים",
      population_group == "arab" ~ "ערבים כללי",
      population_group == "arab_female" ~ "נשים ערביות",
      population_group == "arab_male" ~ "גברים ערבים",
      population_group == "haredi" ~ "חרדים כללי",
      population_group == "haredi_female" ~ "נשים חרדיות",
      population_group == "haredi_male" ~ "גברים חרדים"
    )) %>% 
    relocate(population_group_heb,.after=population_group)
}

#convert english category names to hebrew, the "paste" is to add quotations to specific cells
category_names <- function(data){
  data %>% 
    mutate(category_heb = case_when(
      category == "all population" ~ paste("סה",'"',"כ עובדים במשק",sep=""),
      category == "gender" ~ "מגדר",
      category == "age" ~ "גיל",
      category == "living district" ~ "מחוז מגורים",
      category == "work district" ~ "מחוז עבודה",
      category == "work status by main income" ~ "מעמד בעבודה לפי הכנסה עיקרית",
      category == "work mobility" ~ "ניידות בעבודה",
      category == "occupation sector" ~ "ענף כלכלי",
      category == "high tech" ~ "ענפי הייטק",
      category == "public sector" ~ "מגזר ציבורי לפי ענף כלכלי",
      category == "education" ~ paste("השכלה כולל מה",'"',"ט והכשרות",sep=""),
      category == "higher education" ~ "השכלה אקדמית",
      category == "pop_group" ~ "לאום",
      category == "pop_group by gender" ~ "לאום לפי מגדר"

    )) %>% 
   relocate(category_heb,.after=category)
}

rows_order <- function(data){
  #this is the order of the population rows for each mishlach yad, if you want to change/add/remove, simply
  #adjust the vector accordingly
  output_order <- c("all population",
                    "male",
                    "female",
                    "25-34",
                    "35-44",
                    "age_45_and_over",
                    "45-54",
                    "age_55_and_over",
                    "jerusalem and yosh living",
                    "north and haifa living",
                    "center and tel aviv living",
                    "south living",
                    "jerusalem and yosh work",
                    "north and haifa work",
                    "center and tel aviv work",
                    "south work",
                    "unknown or unpermanent",
                    "wage worker by main inc",
                    "freelancer by main inc",
                    "works in living settlement",
                    "works in more than one settlement",
                    "works outside living settlement",
                    "A",
                    "B",
                    "C",
                    "D",
                    "E",
                    "F",
                    "G",
                    "H",
                    "I",
                    "J",
                    "K",
                    "L",
                    "M",
                    "N",
                    "O",
                    "P",
                    "Q",
                    "R",
                    "S",
                    "not_high_tech",
                    "high_tech",
                    "not public sector",
                    "public sector",
                    "less than bagrut",
                    "bagrut",
                    "up to bagrut",
                    "proffesional traning+mahat+above bagrut",
                    "B.A",
                    "M.A+",
                    "B.A+",
                    "jews_total",
                    "jew_total_male",
                    "jew_total_female",
                    "jew_non_haredi",
                    "jew_non_haredi_male",
                    "jew_non_haredi_female",
                    "arab",
                    "arab_male",
                    "arab_female",
                    "haredi",
                    "haredi_male",
                    "haredi_female")
  data %>% 
    mutate(to_sort = match(population_group,output_order)) %>% 
    group_by(semelmishlachyad) %>% 
    arrange(to_sort,.by_group=T) %>% 
    select(-c("to_sort"))
}


# 5) Activation: Import, Analysis and Export ------------------------------

#workable processed DFs
#the "integer64" is a soluation to a missing package at the CBS, regarding how to treat large numbers.
#the "stringsasfactor" is important because when you save this file, factors there were designed here turned back to regular columns (such as pop_group, high_tech, etc)
#therefore the solution is to treat all string columns as factors while reading them.
work_adj_saka <- fread(paste0(PROCCESED_SOURCES,"work_adj_saka_update.csv"), integer64 = "numeric", stringsAsFactors = T)

#creates the filtered work database accroding to the relevant age group.
#filter for workers, or unemployed who have income according to the IRS, this is only to capture the real amount of workers by year/mishlach
#setDT(work_adj_saka) #in case you created again the work_adj_saka file, it needs to be a data.table
work_adj_saka_analysis <- work_adj_saka[gil>=MIN_AGE&(workforce_attr=="employed"|!is.na(total_income))]

#get all the necessary summaries
results <- get_results(work_adj_saka_analysis)

#change order/translate cols to hebrew so that it will match desired output
results_exp <- results %>% rows_order() %>% group_names() %>%  category_names()

#write results to excel file (neccesary in order to export hebrew)
write.xlsx(results_exp,paste0(OUTPUTS,"results.xlsx"))



# 6) Import: Import Patterns ----------------------------------------------

#in order to import properly all the relevant files, please make sure that all related files start with this pattern (the rest can be differet, for example roni_inc_2012/roni_inc_2013). the "^" means "starts with"
INCOME_FILES_PATTERN_INC <- "^roni_inc"
INCOME_FILES_PATTERN_ISK <- "^roni_isk"
SKILLATTEARIM_PATTERN <- "^roni_shkilattearim"
BOG_PATTERN <- "^roni_bog"
SAKA_SURVEY_PATTERN <- "^saka"
DIPLOMOTMAHAT_PATTERN <- "^diplomotmahat"
HACHSHARA_MIK_PATTERN <- "^hachshara_mik"
MIRSHAM_HASCALA_PATTERN <- "^mirsham_hascala"


# 7) Import: Functions ----------------------------------------------------

#import all files in the "used_sources" folder that matches the name pattern
import_files <- function(name_pattern){
  import <- list.files(path = USED_SOURCES, pattern = name_pattern) #create a list of file names to import
  x <- lapply(import, function(i) {fread(paste0(USED_SOURCES,i), integer64 = "numeric")}) #import them as data.table
  names(x) <- import #change name of file based on the original name
  x <- lapply(x,def_tolower)
}

#replace all capital letters with lower letters, makes writing column names easier and less errors
def_tolower <- function(data){
  data <- rename_all(data,tolower)
}


# 8) Activation: Import ---------------------------------------------------

#import all roni_inc yearly files
income_files_inc <- import_files(INCOME_FILES_PATTERN_INC)
#import all roni_isk yearly files
income_files_isk <- import_files(INCOME_FILES_PATTERN_ISK)
#import all roni_skilattearim yearly files
skilattearim_files <- import_files(SKILLATTEARIM_PATTERN)
#import all roni_bog yearly files
bog_files <- import_files(BOG_PATTERN)
#import all saka yearly files
saka_files <- import_files(SAKA_SURVEY_PATTERN)
#import all diplomotmahat files
diplomahat_files <- import_files(DIPLOMOTMAHAT_PATTERN)
#import all hachshara_mik files
hachshara_mik_files <- import_files(HACHSHARA_MIK_PATTERN)
#import all mirsham_hascala files
mirsham_hascala_files <- import_files(MIRSHAM_HASCALA_PATTERN)


# 9) Functions: Creating Processed dataframes -----------------------------

#create sum_income_inc_isk_fic processed file
#this function is based on importing all the relevant files from the folder
#and working on them as a list, and only at the end all the DFs are merged into one.
# thats why there are "lapply" and "for loops" - thats how you can work on multiple DFs (in a list) with the same code
#the structure of this code is a bit different than regular DF %>%  mutate... because working with data.table syntax instead of dplyr syntax
#is much much faster for large dataframes (1M+ obs). 
create_sum_inc_isk <- function(){
  #wage worker files
  income_files_inc <- lapply(income_files_inc,function(x){x[,pratt_masc_vtash := as.integer(pratt_masc_vtash)]}) #if this column isnt defined as a number, turn it into a number
  income_files_inc <- lapply(income_files_inc,function(x){x[,anafk_3 := NULL]}) #if this column exists, remove it
  income_files_inc <- lapply(income_files_inc, setnames,old="control_shana_shotef",new = "year",skip_absent = T) #replace coulmn names so that it will match across all files
  all_inc_files <- rbindlist(income_files_inc,use.names = T, fill = T) #unite all DF together
  all_inc_files <- all_inc_files[!is.na(pratt_masc_vtash)] #filter empty rows
  all_inc_files <- all_inc_files[pratt_masc_vtash>0] #filter rows in which the income is 0 or less
  all_inc_files <- all_inc_files[worked_months>0] #filter rows in which the worked months =0
  all_inc_files <- all_inc_files %>%  distinct() #remove duplicates
  setDT(all_inc_files)
  set(all_inc_files,j = "is_wage", value = 1) #mark all as wage workers
  all_inc_files[,3:14 := lapply(.SD,function(x) ifelse(sum(x)>=1,1,0)),by = .(year,misparzehut_fic)] #if he worked (at the job) at a certain month, mark that month as a "worked month"
  all_inc_files[,total_inc_wage := sum(pratt_masc_vtash,na.rm = T),by = .(year,misparzehut_fic)] #income for all different yearly saleries
  all_inc_files <- all_inc_files[,.SD[1],by = .(year,misparzehut_fic)] #keep only 1 row per person per year (because we have already took all the necesary data from his other jobs, if exists)
  all_inc_files[,worked_months := rowSums(.SD),.SDcols = 3:14] #recalculate worked_months (based on all jobs)
  all_inc_files[,total_monthly_wage :=total_inc_wage/worked_months,by = .(year,misparzehut_fic)] #calculate monthly wage based on income (per wage job) divided by actual months which he worked
  all_inc_files <- all_inc_files[total_monthly_wage>=100] #filter rows in where monthly wage is lower than 100
  all_inc_files[,c("hodesh1","hodesh2","hodesh3","hodesh4","hodesh5","hodesh6","hodesh7","hodesh8","hodesh9","hodesh10",
                   "hodesh11","hodesh12","prat_sug_misra","worked_months","pratt_masc_vtash"):=NULL] #remove uneseccary columns
  #freelancer files
  income_files_isk <- lapply(income_files_isk, setnames,old="hahnasaiskit",new = "hahnasa_iskit",skip_absent = T) #replace coulmn names so that it will match across all files
  income_files_isk <- lapply(income_files_isk, setnames,old="hahnasa_iskit_2013",new = "hahnasa_iskit",skip_absent = T) #replace coulmn names so that it will match across all files
  income_files_isk <- lapply(income_files_isk, setnames,old="anaf_2013",new = "anaf_kalkali3",skip_absent = T) #replace coulmn names so that it will match across all files
  income_files_isk <- lapply(income_files_isk,function(x){x[,hahnasa_iskit := as.integer(hahnasa_iskit)]}) #if this column isnt defined as a number, turn it into a number
  income_files_isk <- lapply(income_files_isk,function(x){x[,anaf_kalkali3 := as.integer(anaf_kalkali3)]}) #if this column isnt defined as a number, turn it into a number
  for (i in names(income_files_isk)){
    income_files_isk[[i]] <- income_files_isk[[i]] %>% 
      mutate(year = parse_number(names(income_files_isk[i])))} #create a year column based on the year number in the file name (since most of these files dont include a year column)
  all_isk_files <- rbindlist(income_files_isk,use.names = T, fill = T)#unite all DF together
  all_isk_files <- all_isk_files[!is.na(hahnasa_iskit)] #filter empty rows
  all_isk_files <- all_isk_files[hahnasa_iskit>0] #remove rows where his income as a freelancer is small than 0
  all_isk_files <- all_isk_files %>% distinct(misparzehut_fic,year,hahnasa_iskit,.keep_all = T) #remove duplicate rows (based on these columns)
  setDT(all_isk_files)
  set(all_isk_files,j = "is_freelance", value = 1) #mark all as freelancers
  all_isk_files[,monthly_freelance :=hahnasa_iskit/12,by = .(year,misparzehut_fic)] #assesment of his monthly income as a freelancer (because we dont have actual worked months for freelancers)
  all_isk_files[,c("anaf_kalkali3","shnatmas"):=NULL] #remove uneseccary columns
  #merge freelance and wage worker data
  sum_income_inc_isk_fic <- all_inc_files %>% 
    merge(all_isk_files,by = c("misparzehut_fic","year"),all=T)
  sum_income_inc_isk_fic[,is_wage_only := 0][(is_wage==1&is.na(is_freelance)),is_wage_only :=1] #is he exclusivly a wage worker
  sum_income_inc_isk_fic[,is_freelance_only := 0][(is.na(is_wage)&is_freelance==1),is_freelance_only :=1] #is he exclusivly a freelancer
  sum_income_inc_isk_fic[,freelance_as_main_inc := 0,by = .(year,misparzehut_fic)][(hahnasa_iskit>total_inc_wage)|is_freelance_only==1,freelance_as_main_inc :=1,by = .(year,misparzehut_fic)] #binary var, gets 1 if his income as freelancer higher than income as wage worker (from all jobs)
  sum_income_inc_isk_fic[,total_monthly_earning := sum(total_monthly_wage,monthly_freelance,na.rm = T),by = .(year,misparzehut_fic)] #the some of his monthly earnings (wage+freelancer)
  sum_income_inc_isk_fic[,total_income := sum(hahnasa_iskit,total_inc_wage,na.rm = T),by = .(year,misparzehut_fic)] #his total (yearly) income from all sources
  setnames(sum_income_inc_isk_fic,c("hahnasa_iskit"),c("inc_freelance"))
  setcolorder(sum_income_inc_isk_fic,c("year","misparzehut_fic","total_inc_wage","total_monthly_wage","inc_freelance","monthly_freelance","total_monthly_earning","total_income"))
}


#create "hascala_combied"
create_hascala_combined <- function(){
  #mahat data
  diplomahat_files <- lapply(diplomahat_files, setnames,old=c("shana"),new = c("shnatzakautteudaaltichon"),skip_absent = T)  #replace coulmn names so that it will match across all files
  mahat_diplomas <- bind_rows(diplomahat_files) #unite all DF together
  mahat_diplomas <- mahat_diplomas[,c("shnatzakautteudaaltichon","misparzehut_fic","maslullimudtamat")] #select relevant columns
  mahat_diplomas <- mahat_diplomas[!is.na(maslullimudtamat)] #remove empty rows
  mahat_diplomas <- mahat_diplomas %>% 
    distinct(misparzehut_fic,maslullimudtamat,shnatzakautteudaaltichon, .keep_all=T) %>% #remove duplicate rows
    group_by(misparzehut_fic) %>% 
    slice_min(shnatzakautteudaaltichon) %>%   #if he has more than one traning, take the first he finished, because since then he is considered a graduet
    mutate(shnat_netunim = shnatzakautteudaaltichon) #renaming for merging later with other files
  #hachshara miktzoit data
  hachshara_mik_files <- lapply(hachshara_mik_files, setnames,old=c("syom","hatchala"),new = c("shana_syom","shana_hat"),skip_absent = T) #replace coulmn names so that it will match across all files
  hachshara_mik <- bind_rows(hachshara_mik_files) #unite all DF together
  hachshara_mik <- hachshara_mik[,c("year","status","status1","actual_indate","actual_outdate","status_temp","mosad_fic","semel_mosad","v1"):=NULL] #remove uneseccary columns
  hachshara_mik <- hachshara_mik[boger==1&anaf1>=1&anaf1<=19]#only those who are entitled to a degree, and from specific anaf
  hachshara_mik <- hachshara_mik %>% 
    select(-c("boger")) %>%
    group_by(misparzehut_fic,anaf1) %>% 
    distinct() %>%  #remove duplicate rows
    slice_min(shana_syom) %>% #for every individual, take only the first training he finished in the same anaf
    slice_head(n=1) %>% #if rows are identical (in shnat syom), take the first row
    ungroup() %>% #ungroup so that i can group again, now not with anaf, so that all obs of the same person will be counted as part of "row", and not just "rows" of different anafim
    group_by(misparzehut_fic) %>% 
    mutate(row = row_number()) %>% #create a helper column that will help pivot that data later (i.e anaf_1,anaf_2, etc...)
    pivot_wider(names_from = row, values_from = c("anaf1","maslul1","megama1","shana_hat","shana_syom")) %>% 
    mutate(shnat_netunim = shana_syom_1) #renaming for merging later with other files
  #mirsham hascala data
  mirsham_hascala <- bind_rows(mirsham_hascala_files) #unite all DF together
  mirsham_hascala <-mirsham_hascala[!is.na(semelteudagvohabeyoter)&shnat_netunim!=-9&!is.na(shnat_netunim)]#remove obs without last known diploma or with missing year
  mirsham_hascala <- mirsham_hascala %>% 
    group_by(misparzehut_fic,shnat_netunim) %>% 
    distinct() #remove duplicate rows
  #merge all DFs together (mahat+hacshara_mik+mirsham hascala)
  merge_list <- list(mirsham_hascala,hachshara_mik,mahat_diplomas)
  hascala_combined <- as.data.table(Reduce(function(d1,d2) merge(d1,d2,by = c("misparzehut_fic","shnat_netunim"), all = T),merge_list)) #all=T is important so that obs that dont exist in all 3 will remain, for exp obs in mahat without mirsham hascala
  hascala_combined[!is.na(shana_syom_1)|!is.na(shnatzakautteudaaltichon),MIN_YEAR := min(shana_syom_1,shnatzakautteudaaltichon,na.rm = T),by=misparzehut_fic]#create a var that shows the first year he finished training, which would continue on all further obs for the same individual. useful for the next row
  hascala_combined[,semelteudagvohabeyoter_fixed := as.double(semelteudagvohabeyoter)][shnat_netunim>=MIN_YEAR&(semelteudagvohabeyoter>=5|is.na(semelteudagvohabeyoter)), semelteudagvohabeyoter_fixed:=as.double(4)]#only if he did traning and his highest recorded diploma is high school, than change it to above highschool education level (for every year since his training unless he achived higher degree), the or "is.na" is because we have obs in hacsara mik that dont appear in mirsham hascala
  setnames(hascala_combined,old = "shnat_netunim",new = "year")
}

#create "saka raw file"
#saka has some differences, mainly between 2018 and the rest. its important to change the column names so that they would match between all files
#most mistmaches found are not important for this project
#if new columns are used for this project, make sure they match across all years/saka files
create_saka_combined <- function(){
  saka_files <- lapply(saka_files, setnames, old = "nayadutyishuvavodamechushav", new = "nayadutyishuvavoda",skip_absent = T)
  saka_files <- lapply(saka_files, setnames, old = "semelmishlachyadnew", new = "semelmishlachyad",skip_absent = T)
  saka_files <- lapply(saka_files, setnames, old = "semelanafkalkalimechushav", new = "semelanafkalkali",skip_absent = T)
  saka_raw <- rbindlist(saka_files,use.names = T, fill = T) #merge all files in the list by column names (and fill empty rows)
}

#this function creates the unified high education table by id and year
#create "all_degrees"
create_all_degrees <- function(){
  degree_weights <- bind_rows(skilattearim_files) #unite all DF together
  degree_weights <- degree_weights[!is.na(shnatnetunim)] #remove empty rows
  degree_weights <- degree_weights %>% 
    distinct() %>% #remove duplicate rows
    filter(miktzoahaskalagvoha>0) %>%
    group_by(misparzehut_fic,toaracademilamas) %>% 
    slice_head(n=2) %>% #for every degree, take only the first two subjects
    rename(shnat_kovets = shnatnetunim) %>% #so the col names will match between files
    mutate(row = row_number()) %>% 
    pivot_wider(names_from = row, names_prefix = "miktzoahaskalagvoha_", values_from = c("miktzoahaskalagvoha")) %>% 
    rename(toaracademilamas_bog = toaracademilamas,miktzoahaskalagvoha1_bog = miktzoahaskalagvoha_1, miktzoahaskalagvoha2_bog = miktzoahaskalagvoha_2)#so the col names will match between files
  graduets <- bind_rows(bog_files) #unite all DF together
  graduets <- graduets[!is.na(toaracademilamas_bog)]  #remove empty rows
  graduets[miktzoahaskalagvoha2_bog==-9,miktzoahaskalagvoha2_bog:=NA] #in some of the files, a "-9" represents missing, so this makes sure all missing are NA
  all_degrees <- plyr::rbind.fill(graduets,degree_weights) #unite all DF together
  all_degrees <<- all_degrees %>%
    filter(toaracademilamas_bog>=10&toaracademilamas_bog<=40) %>% #remove unwanted degrees
    group_by(misparzehut_fic,toaracademilamas_bog) %>% 
    slice_min(shnat_kovets) %>%  #for each degree, take the first one he finished
    slice_head(n=1) %>% # if he finished more than one degree (of the same level) in the same year, take the first
    mutate(miktzoahaskalagvoha1_bog = case_when(is.na(miktzoahaskalagvoha1_bog) ~ miktzoahaskalagvoha2_bog, T ~ miktzoahaskalagvoha1_bog)) %>% #for some reason, some people had subject written in 2 but 1 was NA, which caused problems when merging, so this fixes it
    ungroup() %>% 
    group_by(misparzehut_fic) %>%
    rename(year= shnat_kovets) %>% 
    mutate(toaracademilamas_bog = case_when(toaracademilamas_bog ==10~"ba",toaracademilamas_bog ==20~"ma",toaracademilamas_bog ==30~"md",toaracademilamas_bog ==40~"phd")) %>% #this helps making the columns more understandable and less confusing
    pivot_wider(names_from = toaracademilamas_bog,values_from = c("miktzoahaskalagvoha1_bog","miktzoahaskalagvoha2_bog","year")) %>%  #turn data from long to wide to get columns for every degree
    relocate(year_ba,year_ma,year_md,year_phd,miktzoahaskalagvoha1_bog_ba,miktzoahaskalagvoha2_bog_ba,
             miktzoahaskalagvoha1_bog_ma,miktzoahaskalagvoha2_bog_ma,miktzoahaskalagvoha1_bog_md,miktzoahaskalagvoha2_bog_md,miktzoahaskalagvoha1_bog_phd,miktzoahaskalagvoha2_bog_phd,.after = misparzehut_fic) #sets the column order
  
}

#this function combines the data from "all degrees" with the "hascala combined data"
hascala_combined_fixed <- function(){
  x <- all_degrees %>% 
    group_by(misparzehut_fic) %>% 
    summarise(min_ba=min(year_ba,year_md,na.rm = T),min_ma=min(year_ma,na.rm = T),min_phd=min(year_phd,na.rm = T)) #create a table where for each i.d there is min year of each degree (if has), medical doctors count as B.A
  #for some reason i get "inf" when i run "min", this solves this
  x$min_phd[which(!is.finite(x$min_phd))]<-NA
  x$min_ma[which(!is.finite(x$min_ma))]<-NA
  x$min_ba[which(!is.finite(x$min_ba))]<-NA
  hascala_combined <- hascala_combined %>% 
    merge(x,by=c("misparzehut_fic"),all=T)
  hascala_combined <- hascala_combined %>%
    group_by(misparzehut_fic) %>% 
    mutate(year = case_when(is.na(year)~max(min_ba,min_ma,min_phd,na.rm = T),T~as.double(year))) %>% #this is because we have obs that appear in all_degrees but not in mirsham hascala
    mutate(semelteudagvohabeyoter_fixed = case_when(year>=min_phd&(semelteudagvohabeyoter_fixed>1|is.na(semelteudagvohabeyoter_fixed))~as.double(1), #the or is.na is because we have obs in mirsham hascala without last diploma
                                                    year>=min_ma&(semelteudagvohabeyoter_fixed>2|is.na(semelteudagvohabeyoter_fixed))~as.double(2),
                                                    year>=min_ba&(semelteudagvohabeyoter_fixed>3|is.na(semelteudagvohabeyoter_fixed))~as.double(3),T~as.double(semelteudagvohabeyoter_fixed))) %>% #if its after the date where he got the relevant degree and his "highest diploma" isnt updated, this updates it
    mutate(semelteudagvohabeyoter_fixed_rev = 8-semelteudagvohabeyoter_fixed) #reverse the order of the nums to match the saka code of highest degree
}


# 10) Activation: Processing source databases -----------------------------

# yearly roni_inc_file + #yearly roni_isk_file - files about annual income for wage workers/freelancers = "sum_income_inc_isk_2012_2017_fic"---

#this functions creates the unified income (as wage worker and as freelancer) by id and year with all necesary processing.
#you can view the relevant steps in "Functions for creating proccessed source files"
sum_income_inc_isk_fic <- create_sum_inc_isk()

#save the processed DF for later use
fwrite(sum_income_inc_isk_fic,paste0(PROCCESED_SOURCES,"sum_income_inc_isk_fic.csv"))

# mirsham_hascala + hachshara_mik + roni_diploma_fic + diplomotmahat = "hascala_combined" ---

#this function creates the unfied education table, featuring what is the highest diploma per id per year
#you can view the relevant steps in "Functions for creating proccessed source files"
hascala_combined <- create_hascala_combined()

#you can view the relevant steps in "Functions for creating proccessed source files"
all_degrees <- create_all_degrees()

#this takes the "hascala combined" and merges it with the "all_degrees" DF. you can view the relevant steps in "Functions for creating proccessed source files"
hascala_combined <- hascala_combined_fixed()

#save the processed DF for later use
fwrite(hascala_combined,paste0(PROCCESED_SOURCES,"hascala_combined.csv"))

#yearly saka surveys
saka_combined <- create_saka_combined()
saka_combined %<>% select(RELEVANT_COLUMNS) #narrows the data so using it would be more convinent and faster
#save the processed DF for later use
fwrite(saka_combined,paste0(PROCCESED_SOURCES,"saka_combined.csv"))





# 11) Functions: Saka Modifing --------------------------------------------

#most of the following functions are to create the different groups which later the analysis will be by (such as gender, population group, age, etc.) in the saka survey files
#meaning the different rows/sub-divide of each mishlach yad in the main output
#the "as factor" which appears in most of the functions is so that later, when i analyis a specific mishlach yad, empty groups
#will also appear instead of only the exisiting groups. for exp - if i have arab/jews/haredi, and in a certain mishlach yad only arabs work in, i still want
#him to write - "jews:0" and "haredi:0" instead of dropping them completly. turning these vars to factors is the solution.

#defining main population analysis groups. "Others" (mainly non-jewish post-soviet-union immigrants) are included with the non-haredi jews
#the haredi here are according to self-definition (in contrast to by last school for exp)
def_pop_groups <- function(data){
  data %>% mutate(pop_group=case_when(
    leom == 2 ~ "arab",
    ramatdat == 5 ~ "haredi",
    T ~ "jew_non_haredi"), 
    pop_group = as.factor(pop_group))
  
}

#creating another pop_group, now with jews total (haredi and non haredi combined)
def_pop_groups2 <- function(data){
  data %>% mutate(pop_group2=case_when(
    leom == 2 ~ "arab",
    T ~ "jew_total"),
    pop_group2 = as.factor(pop_group2))
  
}

#getting mishlach yad and anaf by 2 and 3 digits as well
def_occupations <- function(data){
  data %>% 
    mutate(mishlach2 = substr(semelmishlachyad,1,2),
           mishlach3 = substr(semelmishlachyad,1,3),
           branch2 = substr(semelanafkalkali,1,2),
           branch3 = substr(semelanafkalkali,1,3))
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
        semelanafkalkali ==""~"not_high_tech",
        T~"not_high_tech"),
      high_tech = as.factor(high_tech)
    )
}

#creating age groups
def_age_group1 <- function(data){
  data %>% 
    mutate(age_group1 = case_when(
      gil>=25&gil<=34 ~ "25-34",
      gil>=35&gil<=44 ~ "35-44",
      gil>=45 ~ "age_45_and_over"),
      age_group1 = as.factor(age_group1))
}

#creating age groups
def_age_group2 <- function(data){
  data %>% 
    mutate(age_group2 = case_when(
      gil>=45&gil<=54 ~ "45-54",
      gil>=55 ~ "age_55_and_over"),
      age_group2 = as.factor(age_group2))
}

#creating working and living districts by names
def_district <- function(data){
  data %>% 
    mutate(living_district = case_when(
      machozmegurim == 1|machozmegurim == 7 ~ "jerusalem and yosh living",
      machozmegurim == 2|machozmegurim == 3 ~ "north and haifa living",
      machozmegurim == 5|machozmegurim == 4 ~ "center and tel aviv living",
      machozmegurim == 6 ~ "south living",
      is.na(machozmegurim) ~ "unknown"
    ),
    living_district = as.factor(living_district),
    work_district = case_when(
      machozyishuvavoda == 1| machozyishuvavoda == 7 ~ "jerusalem and yosh work",
      machozyishuvavoda == 2| machozyishuvavoda == 3 ~ "north and haifa work",
      machozyishuvavoda == 5| machozyishuvavoda == 4 ~ "center and tel aviv work",
      machozyishuvavoda == 6 ~ "south work",
      machozyishuvavoda == 9 ~ "unknown or unpermanent"
    ),
    work_district = as.factor(work_district))
}

#creating a var by where the individual works
def_mobility <- function(data){
  data %>% 
    mutate(work_mobility = case_when(
      nayadutyishuvavoda ==1 ~ "works in living settlement",
      nayadutyishuvavoda ==2 ~ "works outside living settlement",
      nayadutyishuvavoda ==3 ~ "works in more than one settlement"),
      work_mobility = as.factor(work_mobility))
}

#individual gender
def_sex <- function(data){
  data %>%
    mutate(sex=case_when(
      min==1~"male",
      min==2~"female"),
      sex = as.factor(sex))
}

#turning the anaf (by 1 digits) to a factor var
def_anaf <- function(data){
  data %>% 
    mutate(occupation_sector = as.factor(anafkalkalink))
}

#the occupation level of the individual
def_work_attr <- function(data){
  data %>%
    mutate(workforce_attr=case_when(
      muasak==1 ~ "employed",
      muasak==2 ~ "unemployed",
      is.na(muasak) ~ "out_of_workforce"),
      workforce_attr = as.factor(workforce_attr))
}

#binary var - is part of the public sector (by specific brances) or not
def_public_sector <- function(data){
  data %>% 
    mutate(public_sector = case_when(
      branch2 == 30|branch2 == 35|
        branch2 == 36|branch2==37|
        branch2 == 83|branch2==84|
        branch2 == 91|branch2==99|
        branch3 ==850|branch3 ==851|
        branch3 ==852|branch3 ==853|
        branch3 ==854|branch3 ==855|
        branch3 ==861|branch3 ==944|
        semelanafkalkali==5310|semelanafkalkali==8620|
        semelanafkalkali==8623 ~"public sector", T ~"not public sector"
    ),
    public_sector = as.factor(public_sector))
}

#merging the saka and hascala_combined last_diploma columns into a main one. the logic is
#that whichever place gives him the higher diploma, thats the one who is chosen.
def_last_diploma <- function(data){
  data %>% 
    mutate(highest_diploma = case_when(
      is.na(semelteudagvohabeyoter_fixed_rev) ~ teudagvoha,
      teudagvoha == 99|teudagvoha == 10|teudagvoha == 8|teudagvoha == 9 ~ as.integer(semelteudagvohabeyoter_fixed_rev),
      teudagvoha >= semelteudagvohabeyoter_fixed_rev ~ teudagvoha, T ~ as.integer(semelteudagvohabeyoter_fixed_rev)
    ))
}

#basic high diploma group #1
def_higher_degree <- function(data){
  data %>% 
    mutate(high_edu = case_when(
      highest_diploma ==5 ~ "B.A",
      highest_diploma ==6|highest_diploma ==7 ~ "M.A+"),
      high_edu = as.factor(high_edu))
}

#basic high diploma group #2
def_higher_degree2 <- function(data){
  data %>% 
    mutate(high_edu2 = case_when(
      highest_diploma >=5&highest_diploma <=7 ~ "B.A+"),
      high_edu2 = as.factor(high_edu2))
}

#basic type of education group #1
def_education <- function(data){
  data %>% 
    mutate(edu_diploma = case_when(
      highest_diploma >=0&highest_diploma <=2 ~ "less than bagrut",
      highest_diploma==3 ~ "bagrut"),
      edu_diploma = as.factor(edu_diploma))
}

#basic type of education group #2
def_education2 <- function(data){
  data %>% 
    mutate(edu_diploma2 = case_when(
      highest_diploma >=0&highest_diploma <=3 ~ "up to bagrut"),
      edu_diploma2 = as.factor(edu_diploma2))
}
#basic type of education group #3
def_education3 <- function(data){
  data %>% 
    mutate(edu_diploma3 = case_when(
      highest_diploma==4 ~ "proffesional traning+mahat+above bagrut"),
      edu_diploma3 = as.factor(edu_diploma3))
}

#binary var if he is mainly a wage worker or a freelancer, depends on data from the inc_isk files
def_type_of_worker <- function(data){
  data %>% 
    mutate(type_of_worker = case_when(
      freelance_as_main_inc ==1 ~ "freelancer by main inc",
      freelance_as_main_inc ==0 ~ "wage worker by main inc"),
      type_of_worker = as.factor(type_of_worker))
}

#this function will remove obs where the individual's wage is much higher (by z score) from their mishlach yad's avg_wage
def_z_deleting <- function(data){
  data %>% 
    group_by(shnatseker,semelmishlachyad) %>% 
    mutate(mean_avg = weighted.mean(total_monthly_earning, mishkalshnati, na.rm = T),
           z_score = (total_monthly_earning - mean_avg)/sd(total_monthly_earning,na.rm = T),
           z_remove = case_when(abs(z_score)>Z_FILTER2 ~1, T ~0)) %>% 
    mutate(mean_hourly_wage = weighted.mean(wage_by_hour, mishkalshnati, na.rm = T),
           z_score_hour = (wage_by_hour - mean_hourly_wage)/sd(wage_by_hour,na.rm = T),
           z_remove_hour = case_when(abs(z_score_hour)>Z_FILTER2 ~1, T ~0))
}

#some of the respondents have "98" as reported work hours, which means "irregular", in those cases it is prefered to use their reported hours from last week, since its more accurate than 98.
#must run before hourly_wage
def_fix_hours <- function(data){
  data %>% 
    mutate(hours_worked = case_when(shaotbederechklal==98~shaotavodalemaase,T~shaotbederechklal))
}

#calculate hourly wage per worker
def_hourly_wage <- function(data){
  data <- as.data.table(data)
  data[!is.na(total_monthly_earning)&!is.na(hours_worked)&hours_worked>0,wage_by_hour :=total_monthly_earning/(hours_worked*WORK_WEEKS_PER_MONTH)] 
  #his wage divided by the amount of hours he usually works in a week *4.28 (work weeks in a month)
}

#remove midgam kavua
def_midgam_shotef <- function(data){
  data %>% 
    filter(shavuapkida!=0)
}



# 12) Merge all Processed DF's into Saka ----------------------------------

#import the proccesed DF's as data.tables
sum_income_inc_isk_fic <- fread(paste0(PROCCESED_SOURCES,"sum_income_inc_isk_fic.csv"))
saka_combined <- fread(paste0(PROCCESED_SOURCES,"saka_combined.csv"), integer64 = "numeric")
hascala_combined <- fread(paste0(PROCCESED_SOURCES,"hascala_combined.csv"))

#merge together into one workable DF
work_raw_saka <- merge(saka_combined,sum_income_inc_isk_fic, by.x = c("misparzehut_fic","shnatseker"),by.y =   c("misparzehut_fic","year"),all.x = T)
work_raw_saka %<>% merge(hascala_combined, by.x = c("misparzehut_fic","shnatseker"),by.y =   c("misparzehut_fic","year"),all.x = T)


fwrite(work_raw_saka,paste0(PROCCESED_SOURCES,"work_raw_saka.csv"))

#use this if you have a raw_saka saved and you want to use it instead
#work_raw_saka <- fread(paste0(PROCCESED_SOURCES,"work_raw_saka.csv"), integer64 = "numeric")

#run functions which create necesary vars for creating the "avodata" table later
work_adj_saka <- work_raw_saka %>% 
  def_age_group1() %>% 
  def_age_group2() %>% 
  def_district() %>% 
  def_pop_groups() %>%
  def_pop_groups2() %>% 
  def_occupations() %>% 
  def_high_tech() %>% 
  def_mobility() %>% 
  def_sex() %>% 
  def_last_diploma() %>% 
  def_work_attr() %>% 
  def_public_sector() %>% 
  def_higher_degree() %>% 
  def_higher_degree2() %>% 
  def_education() %>% 
  def_education2() %>% 
  def_education3() %>% 
  def_type_of_worker() %>% 
  def_anaf() %>% 
  def_fix_hours() %>% 
  def_hourly_wage() %>%
  def_z_deleting() %>%
  def_midgam_shotef()


#save the worable DF that is imported in section 5
fwrite(work_adj_saka,paste0(PROCCESED_SOURCES,"work_adj_saka_update.csv"))
