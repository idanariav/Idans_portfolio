#set-up: global vars--------------------------------------------------------------

# as default, global variables should be written with capital letters
OBS_MIN <- 5
MAX_YEAR <- 2021
DATA_FILE <- ".\\data_sources\\"
PROCESSED_DATA_FILE <- ".\\data_sources\\processed\\"
ISSUE_DATE<- format(Sys.Date(),format="%d_%m_%y")
OUTPUT <- ".\\to_transfer\\"
MIN_AGE <- 25
MAX_AGE <- 64
MONTHS <- 7

#turn of scientific notation
options(scipen=999)

#set-up: load packages--------------------------------------------------------------

packages <- c("crayon","dplyr","data.table","forcats","tidyr","janitor","magrittr","openxlsx","glue")
lapply(packages,library, character.only = T)
requireNamespace("plyr")

#functions: cleaning--------------------------------------------------------------

#create a var for 2/3 digits of anaf and mishlach yad
def_occupations <- function(data){
  data %>% 
    mutate(mishlach2 = substr(semelmishlachyad3,1,2),
           mishlach3 = substr(semelmishlachyad3,1,3),
           branch2 = substr(semelanafkalkali3,1,2),
           branch3 = substr(semelanafkalkali3,1,3))
}
#turn sex from numeric to string var
def_sex <- function(data){
  data %>%
    mutate(sex=case_when(
      min==1~"male",
      min==2~"female"))
}
#all of our analysis are without "מדגם שוטף", this removes it
def_midgam_shotef <- function(data){
  data %>% 
    filter(shavuapkida!=0)
}
#sometimes because of lack of obs we want to do analysis with merging 2 months
#togther
def_month_pairs <- function(data){
  data %>%
    mutate(month_pairs = case_when(
      chodeshseker==1|chodeshseker==2~"month1-2",
      chodeshseker==3|chodeshseker==4~"month3-4",
      chodeshseker==5|chodeshseker==6~"month5-6",
      chodeshseker==7|chodeshseker==8~"month7-8",
      chodeshseker==9|chodeshseker==10~"month9-10",
      chodeshseker==11|chodeshseker==12~"month11-12"),
      mutate(half_m_weights = mishkalsofi/2))
}
#same logic for 3 months
def_triple_months <- function(data){
  data %>% 
    mutate(three_months = case_when(
      chodeshseker>=1&chodeshseker<=3~"month1-3",
      chodeshseker>=4&chodeshseker<=6~"month4-6",
      chodeshseker>=7&chodeshseker<=9~"month7-9",
      chodeshseker>=10&chodeshseker<=12~"month10-12"),
      third_m_weights = mishkalsofi/3)
}

#basic employment var
def_work_attr <- function(data){
  data %>%
    mutate(workforce_attr=case_when(
      muasak==1 ~ "employed",
      muasak==2 ~ "unemployed",
      is.na(muasak) ~ "out_of_workforce"
    ))
  
}
#population groups
def_pop_groups <- function(data){
  data %>% mutate(group=case_when(
    leom == 2 ~ "Arab",
    ramatdat == 5 ~ "Haredi",
    T ~ "Jew_non_Haredi (including Other)"
  ))
  
}
#population groups before 2014 (since there were no haredi in the survey at that time)
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



#there are columns that have changes (name wise or irrelevant such as corona) in the different years of the survey
# this fixes that
def_inconsistances_fix <- function(data){
  data %>% 
    mutate(group=case_when(shnatseker<=2013~group2,T~group)) %>%
    mutate(workforce_attr=case_when(shnatseker<=2019|(shnatseker==2020&chodeshseker<=2)~workforce_attr,T~corona_workforce_attr)) %>% 
    select(-c("group2","corona_workforce_attr"))
}

#those who are not working and also not learning at the moment
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

#creating age groups
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

#creating different age groups
def_age_group_regular <- function(data){
  data %>% 
    mutate(age_group_regular = case_when(
      gil>=25&gil<=54~"25to54",
      gil>=55&gil<=64~"55to64"))
}

#creates another different age group
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

#including those who left the workforce sinsce march 20 as a group
def_work_attr_inc_mit <- function(data){
  data %>% 
    mutate(workforce_attr_inc_mit = case_when(((chodeshhafsakamityaesh>=3&shnathafsakamityaesh==2020)|shnathafsakamityaesh==2021)&(sibahifsiklaavodmityaesh==1|sibahifsiklaavod==1)&tchunatavodashvuit==6~"mityaesh",T~workforce_attr))
}

#functions: summaries------------------------------------------------------------------------------


#the "name" argument is for cases when the data is filtered for a certain group
#such as "45+" or "18-24", and we want to mention the data by name. 
#otherwise we will receive names such as "total pop" (which is not recognizable) and not "45+ total pop"
#the weight and month is if you want to use different months for the analysis (such as bi-monthly)
#the default is a monthly analysis

#if you want to show results by year (and not by month/month_pairs) use the "yearly" version of the function

#please notice the default arguments:
#... - if you want to divide the data to specific groups such as gender, leom, etc... just include them as vars in the function call
#name - in case you dont filter by groups, you need to add name "such as total population". however sometimes it is needed when using only 1 group
#month&weight is default to month and monthly weight, but it is a possibility to do an analysis by month-pairs (you need to adjust a new month-pair column and month-pair-weight)
#min/max year - which years will be added to the analysis? by default its only the current year
#min/max age - which ages should be filtered? by default its 25-64. otherwise you can use 15&100)
#workforce - if you want to include a different method of mesuring employment, such as including mityaashim, or halat as employed, just insert a different employment group var
# for an explanation on the "if else", view the "del_completing_rows_interaction" function
#the count masures how many grouping variables we use (important for the completing rows function)

sum_basic_month_and_year <- function(data,...,month=chodeshseker,weight=mishkalsofi,name=NULL,min_year=MAX_YEAR,max_year=MAX_YEAR,min_age=MIN_AGE,max_age=MAX_AGE,workforce=workforce_attr){
  count <- length(enquos(...)) #count the number of added grouping vars
  paste_vars <- enquos(...,name) #later we will create a new column based on the grouping vars (such as "arab+female")
  data %<>% 
    filter(shnatseker>=min_year&shnatseker<=max_year&gil>=min_age&gil<=max_age) %>% 
    bind_rows(mutate(.,{{workforce}}:="all_population")) %>% #adds an "all population" to the "workforce" group 
    group_by(shnatseker,...,{{month}},{{workforce}}) %>%
    summarise(population_total=sum({{weight}}),obs=n()) #create a simple summary table based on the grouping variables
  #this if checks how many grouping vars are there, and run the relevant compleating rows code
  if (count<=1){
    data %<>%
      del_compleating_rows(.,{{month}},{{workforce}})
  }
  else {
    data %<>%
      del_compleating_rows_interaction(.,{{month}},{{workforce}},...)
  }
  setDT(data)
  #delete rows with low amount of obs or when needed to be deleted because of compleating rows conditions
  data[(obs<OBS_MIN)|(delete_group==1),c("population_total","obs"):=NA]
  data %<>%
    select(-c("delete","delete_group","rows")) %>% #remove unnecessary columns
    pivot_wider(names_from=c({{workforce}}),values_from=c(population_total,obs)) %>% #pivot table from long to wide
    rowwise() %>% #rowwise means that the "sum" would be to sum the columns for each row seperatly, instead of summing the entire column
    #these mutates will create all the column we want to present (such as ratio of unemployment, etc...)
    mutate(population_total_workforce= sum(population_total_employed,population_total_unemployed)) %>%
    mutate(employed_to_population_ratio=population_total_employed/population_total_all_population,
           unemployed_to_workforce_ratio=population_total_unemployed/population_total_workforce,
           workforce_to_population_ratio=population_total_workforce/population_total_all_population,
           obs_workforce = sum(obs_employed,obs_unemployed)) %>% 
    rename(out_of_workforce = population_total_out_of_workforce,
           out_of_workforce_obs = obs_out_of_workforce) %>%
    relocate(population_total_unemployed, .before = population_total_employed) %>%
    relocate(obs_unemployed, .before = obs_employed) %>%
    relocate(population_total_all_population, .after = population_total_workforce) %>%
    relocate(obs_all_population, .after = obs_workforce) %>%
    pivot_wider(names_from=c({{month}}),values_from=starts_with(c("population_total","obs","unemployed","employed","workforce","out"))) %>%
    select(-starts_with(c("out"))) %>% #remove unnecessary columns (of those who are out of the labor market)
    ungroup() %>%
    mutate(population_group= paste(!!!paste_vars,sep="_")) %>%
    mutate(population_group = gsub("_$","",population_group)) %>% #most of the time there is an added unecessary "_" at the end of each name, this removes it
    relocate(population_group,.after= shnatseker) %>%
    select(-c(...)) #remove grouping columns (since we dont need them anymore because we have "population group")
}


# this function does the same analysis as the previous function, but in the scope of an enitre year, instead of by month
#notice that using this function while the current year isnt complete yet (for exp having data for only 6 months of current year)
#will give bad results because its using yearly weights and not monthly, i.e all weights are divided by 12 (which is ok for full year, but not partial years)
# you can see comments made in the previous function for explanations on the code lines
sum_basic_year_only <- function(data,...,weight=mishkalshnati,name=NULL,min_year,max_year=MAX_YEAR,min_age=MIN_AGE,max_age=MAX_AGE,workforce=workforce_attr){
  count <- length(enquos(...))
  paste_vars <- enquos(...,name)
  data %<>%
    filter(shnatseker>=min_year&shnatseker<=max_year&gil>=min_age&gil<=max_age) %>% 
    bind_rows(mutate(.,{{workforce}}:="all_population")) %>% 
    group_by(shnatseker,...,{{workforce}}) %>%
    summarise(population_total=sum({{weight}}),obs=n())
  if (count<=1){
    data %<>%
      del_compleating_rows_year(.,{{workforce}})
  }
  else {
    data %<>%
      del_compleating_rows_year_interaction(.,{{workforce}},...)
  }
  setDT(data)
  data[(obs<OBS_MIN)|(delete_group==1),c("population_total","obs"):=NA]
  data %<>%
    select(-c("delete","delete_group","rows")) %>% 
    pivot_wider(names_from=c({{workforce}}),values_from=c(population_total,obs)) %>%
    rowwise() %>% 
    mutate(population_total_workforce= sum(population_total_employed,population_total_unemployed,na.rm = T)) %>% 
    mutate(employed_to_population_ratio=population_total_employed/population_total_all_population,
           unemployed_to_workforce_ratio=population_total_unemployed/population_total_workforce,
           workforce_to_population_ratio=population_total_workforce/population_total_all_population,
           obs_workforce = sum(obs_employed,obs_unemployed)) %>% 
    rename(out_of_workforce = population_total_out_of_workforce,
           out_of_workforce_obs = obs_out_of_workforce) %>%
    relocate(population_total_unemployed, .before = population_total_employed) %>%
    relocate(population_total_all_population, .after = population_total_workforce) %>%
    relocate(obs_all_population, .after = obs_workforce) %>%
    relocate(obs_unemployed, .before = obs_employed) %>% 
    select(-starts_with(c("out"))) %>% 
    ungroup() %>%
    mutate(population_group= paste(!!!paste_vars,sep="_")) %>%
    mutate(population_group = gsub("_$","",population_group)) %>%
    relocate(population_group,.before= population_total_unemployed) %>%
    select(-c(...))
}

#generate the summarie tables for the each group that we decided to include in the monthly analysis
#since this is a function that uses other function, i tied all the arguments of each sum_basic_month... to arguments
#in the parent function. that way for example i can set min age = 20 when using the parent fun, and it will set the min age for all sub functions
monthly_regular_analysis <- function(data,min_year=MAX_YEAR,max_year=MAX_YEAR,workforce=workforce_attr,min_age=MIN_AGE,max_age=MAX_AGE){
  results <- list()
  results[["by_main_population"]] <- sum_basic_month_and_year(data,name="all population",min_year = min_year,max_year = max_year,workforce={{workforce}},min_age=min_age,max_age=max_age)
  results[["by_gender"]] <- sum_basic_month_and_year(data,sex,min_year = min_year,max_year = max_year,workforce={{workforce}},min_age=min_age,max_age=max_age)
  results[["by_pop_group"]] <- sum_basic_month_and_year(data,group,min_year = min_year,max_year = max_year,workforce={{workforce}},min_age=min_age,max_age=max_age)
  results[["by_pop_and_gender"]] <- sum_basic_month_and_year(data,group,sex,min_year = min_year,max_year = max_year,workforce={{workforce}},min_age=min_age,max_age=max_age)
  results[["by_high_tech"]] <- sum_basic_month_and_year(data,high_tech,min_year = min_year,max_year = max_year,workforce={{workforce}},min_age=min_age,max_age=max_age)
  results[["by_disabled"]] <- sum_basic_month_and_year(data,disabled,min_year = min_year,max_year = max_year,workforce={{workforce}},min_age=min_age,max_age=max_age)
  results[["by_disabled_and_gender"]] <- sum_basic_month_and_year(data,disabled,sex,min_year = min_year,max_year = max_year,workforce={{workforce}},min_age=min_age,max_age=max_age)
  results[["by_young"]] <- sum_basic_month_and_year(data,name="18to24",min_age = 18,max_age = 24,min_year = min_year,max_year = max_year,workforce={{workforce}})
  results[["by_young_and_gender"]] <- sum_basic_month_and_year(data,sex,name="18to24_",min_age = 18,max_age = 24,min_year = min_year,max_year = max_year,workforce={{workforce}})
  results[["by_45_and_over"]] <- sum_basic_month_and_year(data,name="45_and_over",min_age = 45,max_age = 100,min_year = min_year,max_year = max_year,workforce={{workforce}})
  results[["by_45_and_over_and_gender"]] <- sum_basic_month_and_year(data,sex,name="45_and_over_",min_age = 45,max_age = 100,min_year = min_year,max_year = max_year,workforce={{workforce}})
  results[["by_62_67"]] <- sum_basic_month_and_year(data,name="62to67",min_age = 62,max_age = 67,min_year = min_year,max_year = max_year,workforce={{workforce}})
  results[["by_62_67_and_gender"]] <- sum_basic_month_and_year(data,sex,name="62to67_",min_age = 62,max_age = 67,min_year = min_year,max_year = max_year,workforce={{workforce}})
  results[["by_age_groups"]] <- sum_basic_month_and_year(data,age_group_regular,min_year = min_year,max_year = max_year,workforce={{workforce}})
  results[["by_age_groups_and_gender"]] <- sum_basic_month_and_year(data,age_group_regular,sex,min_year = min_year,max_year = max_year,workforce={{workforce}})
  results <- results
}

#same as before, but for the yearly analysis
yearly_regular_analysis <- function(data,min_year,max_year=MAX_YEAR,workforce=workforce_attr,min_age=MIN_AGE,max_age=MAX_AGE){
  results <- list()
  results[["by_main_population"]] <- sum_basic_year_only(data,name="all population",min_year = min_year,max_year = max_year,workforce={{workforce}},min_age=min_age,max_age=max_age)
  results[["by_gender"]] <- sum_basic_year_only(data,sex,min_year = min_year,max_year = max_year,workforce={{workforce}},min_age=min_age,max_age=max_age)
  results[["by_pop_group"]] <- sum_basic_year_only(data,group,min_year = min_year,max_year = max_year,workforce={{workforce}},min_age=min_age,max_age=max_age)
  results[["by_pop_and_gender"]] <- sum_basic_year_only(data,group,sex,min_year = min_year,max_year = max_year,workforce={{workforce}},min_age=min_age,max_age=max_age)
  results[["by_high_tech"]] <- sum_basic_year_only(data,high_tech,min_year = min_year,max_year = max_year,workforce={{workforce}},min_age=min_age,max_age=max_age)
  results[["by_disabled"]] <- sum_basic_year_only(data,disabled,min_year = min_year,max_year = max_year,workforce={{workforce}},min_age=min_age,max_age=max_age)
  results[["by_disabled_and_gender"]] <- sum_basic_year_only(data,disabled,sex,min_year = min_year,max_year = max_year,workforce={{workforce}},min_age=min_age,max_age=max_age)
  results[["by_young"]] <- sum_basic_year_only(data,name="18to24",min_age = 18,max_age = 24,min_year = min_year,max_year = max_year,workforce={{workforce}})
  results[["by_young_and_gender"]] <- sum_basic_year_only(data,sex,name="18to24_",min_age = 18,max_age = 24,min_year = min_year,max_year = max_year,workforce={{workforce}})
  results[["by_45_and_over"]] <- sum_basic_year_only(data,name="45_and_over",min_age = 45,max_age = 100,min_year = min_year,max_year = max_year,workforce={{workforce}})
  results[["by_45_and_over_and_gender"]] <- sum_basic_year_only(data,sex,name="45_and_over_",min_age = 45,max_age = 100,min_year = min_year,max_year = max_year,workforce={{workforce}})
  results[["by_62_67"]] <- sum_basic_year_only(data,name="62to67",min_age = 62,max_age = 67,min_year = min_year,max_year = max_year,workforce={{workforce}})
  results[["by_62_67_and_gender"]] <- sum_basic_year_only(data,sex,name="62to67_",min_age = 62,max_age = 67,min_year = min_year,max_year = max_year,workforce={{workforce}})
  results[["by_age_groups"]] <- sum_basic_year_only(data,age_group_regular,min_year = min_year,max_year = max_year,workforce={{workforce}})
  results[["by_age_groups_and_gender"]] <- sum_basic_year_only(data,age_group_regular,sex,min_year = min_year,max_year = max_year,workforce={{workforce}})
  results <- results
}


#this function covers the basic formula of deleting compleating rows.
#for more advanced/specific versions (such as different action by pop, or multi-level deletion - 
#additional code is requiered)
#the difference between the first (without interaction) and the second (with interaction) function is such:
# imagine you have only 1 grouping var, thus is has only 1 "parent" var from which to complete missing rows
# however, if you have 2 grouping vars, there are 2 "parent" vars from which to complete missing rows
# thus, you have to delete according to both parent groups.
# example, if you have disabled*gender - that gives you 4 sub_groups (disabled/non-disabled*male/female)
# if disabled female is missing, its not enough to delete disabled male, because that only solved in relation to the "disabled" parent var
# you could still complete missing with the "gender" parent var (i.e total_female - non_disabled_female = disabled_female)
# therefore, when the interaction between 2 vars create 4 sub_groups, even just 1 missing demands to delete all.
# if there are 6 sub_groups (such as haredi/arab/jew*gender), it is enough to delete just 1 more group
# the "interaction" function is instructed to delete the next group according to lowest amount of population

del_compleating_rows <- function(data,month,workforce){
  data %>% 
    ungroup() %>% 
    group_by(shnatseker,{{month}},{{workforce}}) %>% 
    mutate(delete= case_when((obs<5)&(obs>0) ~1,T~0), rows=row_number(),
           delete_group = case_when(max(rows)<=2&sum(delete)>=1~1,
                                    max(rows)>2&sum(delete)>=2~0,
                                    max(rows)>2&sum(delete)==1&sort(population_total,F)[2]==population_total~1,T~0))
}

#when there are more than parent group, i.e an interaction between 2 grouping vars
# such as population & gender, a special function is required 
# since those empty rows can be completed from either parent group
# for exp - arab_female can be completed either from total arabs, or total_female
# thats why it is required to delete according to both groups, i.e
# to delete another group as well (not only arabs, but also haredi for exp)
del_compleating_rows_interaction <- function(data,month,workforce,...){
  list_vars <- enquos(...)
  data %<>% 
    ungroup() %>% 
    group_by(shnatseker,{{month}},{{workforce}}) %>% 
    mutate(delete= case_when((obs<5)&(obs>0) ~1,T~0), rows=row_number()) #simple delete by obs
  if (max(data$rows)<=4){
    data %<>%
      mutate(delete_group = case_when(sum(delete)>=1~1,T~0)) #if it has only two groups by grouping vars (such as male/female + hightech/non high tech) then its enough that 1 is missing to having delete all of them
  } else {
    data %<>%
      group_by(shnatseker,{{month}},{{workforce}},!!list_vars[[1]]) %>% 
      mutate(total_group=sum(population_total),
             delete_group = case_when(sum(delete)>=1~1,T~0)) %>% #mark for deletion the first completing parent group
      ungroup() %>% 
      group_by(shnatseker,{{month}},{{workforce}}) %>% 
      mutate(delete_group = case_when(sum(delete_group)>=1&sort(total_group,F)[3]==total_group~1,T~delete_group)) %>% #if a group has to be deleted, delete another group based on the next lowest amount of workers
      select(-c("total_group"))
  }
}
#same as the basic function, but relevant for the yearly summary
del_compleating_rows_year <- function(data,workforce){
  data %>% 
    ungroup() %>% 
    group_by(shnatseker,{{workforce}}) %>% 
    mutate(delete= case_when((obs<5)&(obs>0) ~1,T~0), rows=row_number(),
           delete_group = case_when(max(rows)<=2&sum(delete)>=1~1,
                                    max(rows)>2&sum(delete)>=2~0,
                                    max(rows)>2&sum(delete)==1&sort(population_total,F)[2]==population_total~1,T~0))
}

#same as the basic function, but relevant for the yearly summary
del_compleating_rows_year_interaction <- function(data,month,workforce,...){
  list_vars <- enquos(...)
  data %<>% 
    ungroup() %>% 
    group_by(shnatseker,{{workforce}}) %>% 
    mutate(delete= case_when((obs<5)&(obs>0) ~1,T~0), rows=row_number())
  if (max(data$rows)<=4){
    data %<>%
      mutate(delete_group = case_when(sum(delete)>=1~1,T~0))
  } else {
    data %<>%
      group_by(shnatseker,{{workforce}},!!list_vars[[1]]) %>% 
      mutate(total_group=sum(population_total),
             delete_group = case_when(sum(delete)>=1~1,T~0)) %>% 
      ungroup() %>% 
      group_by(shnatseker,{{workforce}}) %>% 
      mutate(delete_group = case_when(sum(delete_group)>=1&sort(total_group,F)[3]==total_group~1,T~delete_group)) %>% 
      select(-c("total_group"))
  }
}

#functions: export -----------------------------------------------------------------------------

#the populations (rows of the summary) will be ordered by this vector
rows_order <- function(data){
  output_order <- c(
    "all population",
    "female",
    "male",
    "Arab",
    "Haredi",
    "Jew_non_Haredi (including Other)",
    "Arab_female",
    "Arab_male",
    "Haredi_female",
    "Haredi_male",
    "Jew_non_Haredi (including Other)_female",
    "Jew_non_Haredi (including Other)_male",
    "high_tech",
    "not_high_tech",
    "disabled",
    "not_disabled",
    "disabled_female",
    "disabled_male",
    "not_disabled_female",
    "not_disabled_male",
    "18to24",
    "female_18to24",
    "male_18to24",
    "45_and_over",
    "female_45_and_over",
    "male_45_and_over",
    "62to67",
    "female_62to67",
    "male_62to67",
    "25to54",
    "25to54_female",
    "25to54_male",
    "55to64",
    "55to64_female",
    "55to64_male")
  data %>% 
    mutate(to_sort = match(population_group,output_order)) %>% 
    arrange(to_sort) %>% 
    select(-c("to_sort"))
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
      population_group == "female_18to24" ~ "גילאי 18-24 - נשים",
      population_group == "male_18to24" ~ "גילאי 18-24 גברים",
      population_group == "45_and_over" ~ "גילאי 45+",
      population_group == "female_45_and_over" ~ "גילאי 45+ - נשים",
      population_group == "male_45_and_over" ~ "גילאי 45+ - גברים",
      population_group == "62to67" ~ "גילאי 62-67",
      population_group == "female_62to67" ~ "גילאי 62-67 - נשים",
      population_group == "male_62to67" ~ "גילאי 62-67 - גברים",
      population_group == "25to54" ~ "גילאי 25-54",
      population_group == "25to54_female" ~ "גילאי 25-54 - נשים",
      population_group == "25to54_male" ~ "גילאי 25-54 - גברים",
      population_group == "55to64" ~ "גילאי 55-64",
      population_group == "55to64_female" ~ "גילאי 55-64 - נשים",
      population_group == "55to64_male" ~ "גילאי 55-64 - גברים",
    ))
}

#create hebrew group names for the monthly last diploma analysis
group_names_education <- function(data){
  data %>% 
    mutate(population_group = case_when(
      population_group==1~"תעודת סיום בית ספר יסודי או חטיבת ביניים",
      population_group==2~"תעודת סיום תיכון (שאינה תעודת בגרות)",
      population_group==3~"תעודת בגרות",
      population_group==4~"תעודת סיום בית ספר על תיכוני שאינה תעודה אקדמית",
      population_group==5~"תואר ראשון",
      population_group==6~"תואר שני"
    ))
}

#create a vector in order to translate months from numbers to hebrew names
month_heb_abb <- c("ינו", "פבר", "מרץ", "אפר", "מאי", "יוני", "יולי", "אוג", "ספט", "אוק", "נוב", "דצמ")

#create column names in hebrew according to the amount of monthsfor the "total/obs" worksheet including mityaesh columns
gen_col_names_mit <- function(){
  a <- c("אוכלוסייה")
  b <- paste("אבטלה מותאם", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  c <- paste("תעסוקה מותאם", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  d <- paste("מתייאשים", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  e <- paste("בכח העבודה", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  f <- paste("סהכ אוכלוסייה", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  new <- c(a,b,c,d,e,f)
}

#create column names in hebrew according to the amount of months for the "total/obs" worksheet
gen_col_names <- function(){
  a <- c("אוכלוסייה")
  b <- paste("אבטלה מותאם", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  c <- paste("תעסוקה מותאם", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  d <- paste("בכח העבודה", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  e <- paste("סהכ אוכלוסייה", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  new <- c(a,b,c,d,e)
}

#create column names in hebrew according to the amount of months for the "ratio" worksheet
gen_col_names_ratio <- function(){
  a <- c("אוכלוסייה")
  b <- paste("שיעור אבטלה מותאם", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  c <- paste("שיעור תעסוקה מותאם", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  d <- paste("שיעור השתתפות בכוח העבודה", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  new <- c(a,b,c,d)
}

#create hebrew col names for the monthly employed by anaf analysis
gen_col_names_anaf <- function(){
  a <- c("ענף כלכלי")
  b <- paste("כמות מועסקים מנופחת", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  c <- paste("תצפיות", MAX_YEAR, month_heb_abb[seq(1:MONTHS)], sep = "_")
  new <- c(a,b,c)
}

#create a list of col names in hebrew for the yearly summary version
gen_col_names_yearly <- function(){
  a <- c("שנה","אוכלוסייה")
  b <- paste("אבטלה מותאם", sep = "_")
  c <- paste("תעסוקה מותאם", sep = "_")
  d <- paste("בכח העבודה", sep = "_")
  e <- paste("סהכ אוכלוסייה", sep = "_")
  new <- c(a,b,c,d,e)
}

#same as before, but for ratio sheet
gen_col_names_ratio_yearly <- function(){
  a <- c("שנה","אוכלוסייה")
  b <- paste("שיעור אבטלה מותאם", sep = "_")
  c <- paste("שיעור תעסוקה מותאם",  sep = "_")
  d <- paste("שיעור השתתפות בכוח העבודה", sep = "_")
  new <- c(a,b,c,d)
}

# clean raw file----------------------------------------

#import the raw data
raw <- fread(paste0(DATA_FILE,"saka_2012_2021.csv"),integer64 = "numeric")

#run all cleaning/modifiying functions on the raw data
wrk_df <- raw %>% 
  def_occupations() %>% 
  def_month_pairs() %>% 
  def_sex() %>% 
  def_work_attr() %>% 
  def_midgam_shotef() %>% 
  def_pop_groups_12_13() %>% 
  def_pop_groups() %>% 
  def_corona_missing() %>% 
  def_corona_work_attr() %>% 
  def_inconsistances_fix() %>% 
  def_not_working_not_learning() %>% 
  def_age_group() %>% 
  def_age_group_regular() %>% 
  def_age_group_wide() %>% 
  def_disabled_broad() %>% 
  def_high_tech() %>% 
  def_work_attr_inc_mit()
  #def_ethiopian 
  # %>% def_bedouin

#save the updated data as a work file for future fast use
fwrite(wrk_df,paste0(PROCESSED_DATA_FILE,"wrk_df_12_21.csv"))

#alt: import ready work file----------------------------------------

#if no new data is included, you can simply import directly the adjusted work file
#without the need to clean it again
wrk_df <- fread(paste0(PROCESSED_DATA_FILE,"wrk_df_12_21.csv"),integer64 = "numeric")

#monthly analysis 25-64-----------------------------------------------------------------------------------------------------

#create the result output
results_25_64 <- monthly_regular_analysis(wrk_df)
#bind the list into one dataframe
results_25_64 <- bind_rows(results_25_64) %>% rows_order

#split the big DF to different tables by their columns, later they will be different excel sheets
to_export_25_64 <- list()
to_export_25_64[["סהכ"]] <- results_25_64 %>%
  select(starts_with(c("population_group","population_total"))) %>% 
  group_names

to_export_25_64[["שיעורים"]] <- results_25_64 %>%
  select(contains(c("population_group","ratio"))) %>% 
  group_names

to_export_25_64[["תצפיות"]] <- results_25_64 %>%
  select(starts_with(c("population_group","obs"))) %>% 
  group_names

#replace column names to hebrew
names(to_export_25_64[["סהכ"]]) <- gen_col_names()
names(to_export_25_64[["תצפיות"]]) <- gen_col_names()
names(to_export_25_64[["שיעורים"]]) <- gen_col_names_ratio()

#create the excel file
#add worksheet and assign data for each item in to_export_25_64_expanded list
mywb_general <- createWorkbook()
Map(function(dat,dat_name){
  addWorksheet(mywb_general,dat_name)
  writeData(mywb_general,dat_name,dat,startRow = 2)
},to_export_25_64,names(to_export_25_64))
#freeze panes and adjust column width for each sheet
Map(function(sheet_name){
  freezePane(mywb_general,sheet_name,firstCol = T)
  setColWidths(mywb_general,sheet_name,cols=1:ncol(to_export_25_64[["סהכ"]]),widths = "auto")
},names(mywb_general))
#create specific styles
style_num <- createStyle(numFmt = "COMMA")
stle_per <- createStyle(numFmt = "0.0%")
#replace default styles of cells
addStyle(mywb_general,"סהכ",style = style_num,rows = 2:(nrow(to_export_25_64[["סהכ"]])+2),cols=2:ncol(to_export_25_64[["סהכ"]]),gridExpand = T)
addStyle(mywb_general,"שיעורים",style = stle_per,rows = 2:(nrow(to_export_25_64[["סהכ"]])+2),cols=2:ncol(to_export_25_64[["סהכ"]]),gridExpand = T)
addStyle(mywb_general,"תצפיות",style = style_num,rows = 2:(nrow(to_export_25_64[["סהכ"]])+2),cols=2:ncol(to_export_25_64[["סהכ"]]),gridExpand = T)
#the paste0 allows to add elements to file name, such as output (for saving it in a different location) and current date
saveWorkbook(mywb_general,paste0(OUTPUT,paste("saka21_general_25_64_",ISSUE_DATE,".xlsx")),overwrite = T)

