library(tidyverse)
library(naivebayes)
library(rpart)
library(randomForest)
library(pROC)
library(ggplot2)
library(ggrepel)
library(formattable)

g <- glimpse
h <- head   
s <- summary   
t <- tail   

dc <- read_csv("psam_p11.csv")


# variables = colnames of interest
variables <- c('ST', 'ADJINC', "PWGTP", 'CIT', 'CITWP', 'COW', 'ENG', 'HINS1', 
               'HINS2', 'HINS3', 'HINS4', 'INTP', 'JWTR', 'LANX', 'MAR', 'MARHT', 
               'RETP', "SCH", 'SCHL', 'SEMP', 'SEX', 'SSP', "WAGP", 'YOEP', 'ANC',
               'ANC1P', 'DECADE', 'DRIVESP', 'ESR', 'FOD1P', 'HICOV', 'INDP',
               'JWAP', 'JWDP', 'LANP', 'MIGPUMA', 'MIGSP', 'NAICSP', 'NATIVITY',
               'NOP', 'OC', 'OCCP', 'PERNP', 'PINCP', 'POBP', 'POVPIP', 
               'POWPUMA', 'POWSP', 'PRIVCOV', 'PUBCOV', 'RAC1P', 'RAC2P', 
               'RAC3P', 'RACAIAN', 'RACASN', 'RACBLK', 'RACNH', 'RACWHT',
               'SCIENGP', 'SCIENGRLP', 'SFN', 'SFR', 'SOCP', 'WAOB')

variable_comments <- c('State code',
                       'Adjustment factor for income and earnings dollar amounts',
                       "Person's weight",
                       'Citizenship status', 
                       'Year of naturalization write-in',
                       'Class of worker', 
                       'Ability to speak English',
                       'Insurance through a current or former employer or union',
                       'Insurance purchased directly from an insurance company',
                       'Medicare, for people 65 and older, or people with certain disabilities',
                       'Medicaid, Medical Assistance, or any kind of government-assistance plan for those with low incomes or a disability',
                       'Interest, dividends, and net rental income past 12 months',
                       'Means of transportation to work',
                       'Language other than English spoken at home',
                       'Marital status',
                       'Number of times married',
                       'Retirement income past 12 months',
                       'School enrollment',
                       'Educational level',
                       'Self-employment income past 12 months',
                       'Sex', 'Social Security income past 12 months',
                       'Wages or salary income past 12 months',
                       'Year of entry to the US', 
                       'Ancestry recode', 'Recoded Detailed Ancestry - first entry',
                       'Decade of entry', 
                       'Number of vehicles calculated from JWRI',
                       'Employment status recode',
                       'Recoded field of degree - first entry',
                       'Health insurance coverage recode', 
                       'Industry recode for 2013 and later based on 2012 IND codes',
                       'Time of arrival at work - hour and minute',
                       'Time of departure for work - hour and minute',
                       'Language spoken at home', 
                       'Migration PUMA based on 2010 Census definition',
                       'Migration recode - State or foreign country code',
                       'NAICS Industry recode for 2013 and later based on 2012 NAICS codes',
                       'Nativity', 'Nativity of parent', 'Own child', 
                       'Occupation recode for 2012 and later based on 2010 OCC codes',
                       'Total person\'s earnings', 'Total person\'s income',
                       'Place of birth (Recode)', 'Income-to-poverty ratio recode',
                       'Place of work PUMA based on 2010 Census definition',
                       'Place of work - State or foreign country recode',
                       'Private health insurance coverage recode',
                       'Public health coverage recode', 
                       'Recoded detailed race code', 
                       'Recoded detailed race code', 
                       'Recoded detailed race code', 
                       'American Indian and Alaska Native recode (American Indian and Alaska Native alone or in combination with one or more other races)',
                       'Asian recode (Asian alone or in combination with one or more other races)',
                       'Black or African American recode (Black alone or in combination with one or more other races)',
                       'Native Hawaiian recode (Native Hawaiian alone or in combination with one or more other races)',
                       'White recode (White alone or in combination with one or more other races)',
                       'Field of Degree Science and Engineering Flag - NSF Definition', 
                       'Field of Degree Science and Engineering Related Flag - NSF Definition',
                       'Subfamily number', 'Subfamily relationship',
                       'SOC Occupation code for 2012 and later based on 2010 SOC codes',
                       'World area of birth')

# var: variables of primary interest
var <- data.frame(variables = variables, description = variable_comments)
var_of_interst <- c('Ability to speak English',
                    'Insurance through a current or former employer or union',
                    'Insurance purchased directly from an insurance company',
                    'Educational level',
                    'Health insurance coverage recode')

# var1: variables narrowed down
var1 <- var %>% 
        filter(description %in% var_of_interst)

# removing missing values in ENG (English level)
dc1 <- dc %>%
        select(var1$variables) %>%
        filter(!is.na(ENG)) 


# converting numeric to categorical values
ENG_d <- data.frame(ENG = 1:4, 
                    English = factor(c("Very Well",
                                       "Well",
                                       "Not Well",
                                       "Not at all"), 
                                     levels = c("Not at all",
                                                "Not Well",
                                                "Well",
                                                "Very Well")))


SCHL_d <- data.frame(SCHL = 1:24, 
                     Education = c("No Schooling Completed",
                                   "Nursery School or Pre-school",
                                   "Kindergarten", 
                                   "Grade 1",
                                   "Grade 2",
                                   "Grade 3",
                                   "Grade 4",
                                   "Grade 5",
                                   "Grade 6",
                                   "Grade 7",
                                   "Grade 8",
                                   "Grade 9",
                                   "Grade 10",
                                   "Grade 11",
                                   "12th Grade - no diploma",
                                   "Regular high school diploma",
                                   "GED or alternative credential",
                                   "Some college, but less than 1 year",
                                   "1 or more years of college credit, no degree",
                                   "Associate's degree",
                                   "Bachelor's degree",
                                   "Master's degree",
                                   "Professional degree beyond a bachelor's degree",
                                   "Doctorate degree"
                     )) %>%
        mutate(Education = factor(Education, 
                                  levels = c("No Schooling Completed",
                                             "Nursery School or Pre-school",
                                             "Kindergarten", 
                                             "Grade 1",
                                             "Grade 2",
                                             "Grade 3",
                                             "Grade 4",
                                             "Grade 5",
                                             "Grade 6",
                                             "Grade 7",
                                             "Grade 8",
                                             "Grade 9",
                                             "Grade 10",
                                             "Grade 11",
                                             "12th Grade - no diploma",
                                             "Regular high school diploma",
                                             "GED or alternative credential",
                                             "Some college, but less than 1 year",
                                             "1 or more years of college credit, no degree",
                                             "Associate's degree",
                                             "Bachelor's degree",
                                             "Master's degree",
                                             "Professional degree beyond a bachelor's degree",
                                             "Doctorate degree"
                                  )))


dc3 <- dc1 %>%
        
        mutate(SCHL = as.integer(SCHL))
        # converting ENG(English level) to a categorical variable
        left_join(ENG_d, by = "ENG") %>%
        
        # converting SCHL(Educational attainment) to a categorical variable
        left_join(SCHL_d, by = "SCHL") %>%     
        
        # converting HINS1(work health insurance), HINS2(private health insurance), 
        # HICOV (health insurance) to categorical variables
        mutate(Work_Insurance = ifelse(HINS1 == 1, "Yes", "No"),
               Private_Insurance = ifelse(HINS2 == 1, "Yes", "No"),
               Health_Insurance_Covered = ifelse(HICOV == 1, "Yes", "No")) %>%
        mutate(HINS1 = ifelse(HINS1 == 1, 1, 0),
               HINS2 = ifelse(HINS2 == 1, 1, 0),
               HICOV = ifelse(HICOV == 1, 1, 0))


dc4 <- dc3[!is.na(dc3$Education), ]

# data cleaning for observation number
dc5 <- dc4 %>%
        mutate(English = as.character(English),
               Education = as.character(Education))

make_table <- function(vec1, vec2, insr) {
        as.data.frame(table(vec1, vec2)) %>%
                mutate(Insurance = insr)
}

dc6_1 <- make_table(dc5$English, 
                    dc5$Work_Insurance, 
                    "Work")

dc6_2 <- make_table(dc5$English, 
                    dc5$Private_Insurance, 
                    "Private")

dc6_3 <- make_table(dc5$English, 
                    dc5$Health_Insurance_Covered, 
                    "Work, Private, or Other")

dc6_4 <- rbind(dc6_1, 
               dc6_2, 
               dc6_3) 

names(dc6_4) <- c("English", "Covered", "Number", "Insurance_Category")

dc6_4 <- dc6_4 %>%
        mutate(English = factor(English, 
                                levels = c("Not at all",
                                           "Not Well", 
                                           "Well",
                                           "Very Well")),
               Insurance_Category = factor(Insurance_Category,
                                           levels = c("Work", 
                                                      "Private",
                                                      "Work, Private, or Other")))

dc7_1 <- make_table(dc5$Education,
                    dc5$Work_Insurance,
                    "Work")

dc7_2 <- make_table(dc5$Education,
                    dc5$Private_Insurance,
                    "Private")

dc7_3 <- make_table(dc5$Education,
                    dc5$Health_Insurance_Covered,
                    "Work, Private, or Other")

dc7_4 <- rbind(dc7_1,
               dc7_2,
               dc7_3) 

names(dc7_4) <- c("Education", "Covered", "Number", "Insurance_Category")

dc7_4 <- dc7_4 %>%
        mutate(Education = factor(Education, 
                                  levels = c("No Schooling Completed",
                                             "Nursery School or Pre-school",
                                             "Kindergarten", 
                                             "Grade 1",
                                             "Grade 2",
                                             "Grade 3",
                                             "Grade 4",
                                             "Grade 5",
                                             "Grade 6",
                                             "Grade 7",
                                             "Grade 8",
                                             "Grade 9",
                                             "Grade 10",
                                             "Grade 11",
                                             "12th Grade - no diploma",
                                             "Regular high school diploma",
                                             "GED or alternative credential",
                                             "Some college, but less than 1 year",
                                             "1 or more years of college credit, no degree",
                                             "Associate's degree",
                                             "Bachelor's degree",
                                             "Master's degree",
                                             "Professional degree beyond a bachelor's degree",
                                             "Doctorate degree")),
               Insurance_Category = factor(Insurance_Category,
                                           levels = c("Work", 
                                                      "Private",
                                                      "Work, Private, or Other")))



# data cleaning for plotting
english_table1 <- as.data.frame(prop.table(table(dc4$English, 
                                                 dc4$Work_Insurance), 
                                           margin = 1))
english_table2 <- as.data.frame(prop.table(table(dc4$English, 
                                                 dc4$Health_Insurance_Covered), 
                                           margin = 1)) 
english_table3 <- rbind(english_table1, english_table2) %>%
        mutate(Ins = c(rep("Work Insurance", times = 8), 
                       rep("Work, Private, or Other Insurance", times = 8))) %>%
        mutate(Ins = factor(Ins, levels = c("Work Insurance", 
                                            "Work, Private, or Other Insurance")))

names(english_table3) <- c("Level", 
                           "Covered", 
                           "Proportion", 
                           "Category")



edu_table1 <- as.data.frame(prop.table(table(dc4$Education, 
                                             dc4$Work_Insurance), 
                                       margin = 1))
edu_table2 <- as.data.frame(prop.table(table(dc4$Education, 
                                             dc4$Health_Insurance_Covered), 
                                       margin = 1) )
edu_table3 <- rbind(edu_table1, edu_table2) %>% 
        filter(!is.nan(Freq)) %>% 
        mutate(Ins = c(rep("Work Insurance", times = 30), 
                       rep("Work, Private, or Other Insurance", times = 30))) %>%
        mutate(Ins = factor(Ins, levels = c("Work Insurance", 
                                            "Work, Private, or Other Insurance")))
names(edu_table3) = c("Education_Attainment", 
                      "Covered", 
                      "Proportion", 
                      "Category")

edu_table4 <- edu_table3 %>%
        filter(Covered == "Yes")

# Prediction model w logistic regression
logistic_prediction <- function(Input_numeric) {
        logistic_model <- glm(Input_numeric ~ 
                                      English + 
                                      Education, 
                              data = dc4, 
                              family = "binomial")
        logistic_prob <- predict(logistic_model, type = "response")
        logistic_prediction <- ifelse(logistic_prob > 0.5, 
                                      "Yes", 
                                      "No")
        return(logistic_prediction)
}

# Prediction model w Bayesian mothod
bayes_prediction <- function(Input_categorical) {
        bayes_model <- naive_bayes(Input_categorical ~ 
                                           English + 
                                           Education, 
                                   data = dc4,
                                   laplace = 1)
        bayes_prediction <- predict(bayes_model, newdata = dc4)
        return(bayes_prediction)
}

# Prediction model w classification tree (no pruning)
tree_prediction <- function(Input_categorical) {
        tree_model <- rpart(Input_categorical ~ 
                                    English + 
                                    Education,
                            data = dc4, method = "class")
        tree_prediction <- predict(tree_model, 
                                   newdata = dc4, 
                                   type = "class")
        return(tree_prediction)
}


# Prediction model w random forest 
randomforest_prediction <- function(Input_numeric) {
        forest_model <- randomForest(Input_numeric ~
                                             English +
                                             Education, 
                                     data = dc4, 
                                     ntree = 200)
        forest_prob <- predict(forest_model)
        forest_prediction <- ifelse(forest_prob > 0.5, 
                                    "Yes", 
                                    "No")
        return(forest_prediction)
}




# Predict coverage of work insurance via logistic regression
work_logistic_accuracy <- mean(logistic_prediction(dc4$HINS1) == dc4$Work_Insurance)

# Predict coverage of private insurance via logistic regression 
private_logistic_accuracy <- mean(logistic_prediction(dc4$HINS2) == dc4$Private_Insurance)

# Predict coverage of insurance via logistic regression 
insurance_logistic_accuracy <- mean(logistic_prediction(dc4$HICOV) == dc4$Private_Insurance)





# Predict coverage of work insurance via Bayesian 
work_bayes_accuracy <- mean(bayes_prediction(dc4$Work_Insurance) == dc4$Work_Insurance)

# Predict coverage of private insurance via Bayesian 
private_bayes_accuracy <- mean(bayes_prediction(dc4$Private_Insurance) == dc4$Private_Insurance) 

# Predict coverage of insurance via Byesian
insurance_bayes_accuracy <- mean(bayes_prediction(dc4$Health_Insurance_Covered) == dc4$Health_Insurance_Covered)




# Predict coverage of work insurance via classification tree 
work_tree_accuracy <- mean(tree_prediction(dc4$Work_Insurance) == dc4$Work_Insurance)

# Predict coverage of private insurance via classification tree 
private_tree_accuracy <- mean(tree_prediction(dc4$Private_Insurance) == dc4$Private_Insurance)

# Predict coverage of insurance via classification tree 
insurance_tree_accuracy <- mean(tree_prediction(dc4$Health_Insurance_Covered) == dc4$Health_Insurance_Covered)





# Predict coverage of work insurance via random forest 
work_rforest_accuracy <- mean(randomforest_prediction(dc4$HINS1) == dc4$Work_Insurance)

# Predict coverage of private insurance via random forest 
private_rforest_accuracy <- mean(randomforest_prediction(dc4$HINS2) == dc4$Private_Insurance)

# Predict coverage of insurance via random forest 
insurance_rforest_accuracy <- mean(randomforest_prediction(dc4$HICOV) == dc4$Private_Insurance)




# cleaning for comparing accuracy of each prediction 
Model <- c(rep("Logistic Regression", times = 3), 
           rep("Naive Bayes", times = 3),
           rep("Classification Tree", times = 3),
           rep("Random Forest", times = 3))

Insurance <- rep(c("Work Insurance",
                   "Private Insurance", 
                   "Work, Private, or Other Insurance"), times = 4)

Accuracy <- c(work_logistic_accuracy, 
              private_logistic_accuracy, 
              insurance_logistic_accuracy,
              work_bayes_accuracy,
              private_bayes_accuracy,
              insurance_bayes_accuracy,
              work_tree_accuracy,
              private_tree_accuracy,
              insurance_tree_accuracy,
              work_rforest_accuracy,
              private_rforest_accuracy,
              insurance_rforest_accuracy)

pred_table <- data.frame(Model = Model,
                         Insurance_Category = Insurance,
                         Accuracy = as.numeric(Accuracy)) %>%
        mutate(Accuracy = round(Accuracy * 100, digits = 2),
               Insurance_Category = factor(Insurance_Category, levels = 
                                                   c("Work Insurance",
                                                     "Private Insurance",
                                                     "Work, Private, or Other Insurance")),
               Model = factor(Model, levels = c("Logistic Regression",
                                                "Naive Bayes",
                                                "Classification Tree",
                                                "Random Forest"))) %>%
        spread(Model, Accuracy)






# function converting categorical to numeric (yes to 1, no to 0) 
# vec is a categorical variable
cat_to_num <- function(vec) {
        num_vec <- c()
        for (x in vec) {
                if (x == "Yes") {
                        num <- 1
                } else {
                        num <- 0
                }
                num_vec <- c(num_vec, num)
        }
        return(num_vec)
}


# Area under curve (AUC) function 
# both actual_vec1 and prob_vec2 are categorical
get_auc <- function(actual_vec1, prob_vec2) {
        roc <- roc(cat_to_num(actual_vec1), cat_to_num(prob_vec2))
        return(auc(roc))
}

# AUC
work_logistic_auc <- get_auc(dc4$Work_Insurance, 
                             logistic_prediction(dc4$HINS1))
private_logistic_auc <- get_auc(dc4$Private_Insurance,
                                logistic_prediction(dc4$HINS2))
insurance_logistic_auc <- get_auc(dc4$Health_Insurance_Covered,
                                  logistic_prediction(dc4$HICOV))
work_bayes_auc <- get_auc(dc4$Work_Insurance,
                          bayes_prediction(dc4$Work_Insurance))
private_bayes_auc <- get_auc(dc4$Private_Insurance,
                          bayes_prediction(dc4$Private_Insurance))
insurance_bayes_auc <- get_auc(dc4$Health_Insurance_Covered,
                               bayes_prediction(dc4$Health_Insurance_Covered))
work_tree_auc <- get_auc(dc4$Work_Insurance,
                         tree_prediction(dc4$Work_Insurance))
private_tree_auc <- get_auc(dc4$Private_Insurance,
                         tree_prediction(dc4$Private_Insurance))
insurance_tree_auc <- get_auc(dc4$Health_Insurance_Covered,
                               tree_prediction(dc4$Health_Insurance_Covered))
work_rforest_auc <- get_auc(dc4$Work_Insurance, 
                            randomforest_prediction(dc4$HINS1))
private_rforest_auc <- get_auc(dc4$Private_Insurance, 
                            randomforest_prediction(dc4$HINS2))
insurance_rforest_auc <- get_auc(dc4$Health_Insurance_Covered,
                                 randomforest_prediction(dc4$HICOV))



# data cleaning for AUC table
AUC <- c(work_logistic_auc, 
              private_logistic_auc, 
              insurance_logistic_auc,
              work_bayes_auc,
              private_bayes_auc,
              insurance_bayes_auc,
              work_tree_auc,
              private_tree_auc,
              insurance_tree_auc,
              work_rforest_auc,
              private_rforest_auc,
              insurance_rforest_auc)

AUC_table <- data.frame(Model = Model,
                         Insurance_Category = Insurance,
                         AUC = round(as.numeric(AUC), digits = 2)) %>%
        mutate(Insurance_Category = factor(Insurance_Category, levels = 
                                                   c("Work Insurance",
                                                     "Private Insurance",
                                                     "Work, Private, or Other Insurance")),
               Model = factor(Model, levels = c("Logistic Regression",
                                                "Naive Bayes",
                                                "Classification Tree",
                                                "Random Forest"))) %>%
        spread(Model, AUC)


# plotting 
english_population <-
        ggplot(dc6_4, aes(x = English, y = Number)) + 
        geom_bar(aes(fill = Covered), stat = "identity", width = 0.8) + 
        facet_grid(.~ Insurance_Category) + 
        theme_bw() +
        theme(axis.text = element_text(size = 10),
              strip.text = element_text(size = 12)) +
        ggtitle("Sample Size: English Level") +
        ylab("Population") +
        xlab("English Level") 

edu_population <- 
        ggplot(dc7_4, aes(x = Education, y = Number)) + 
        geom_bar(aes(fill = Covered), stat = "identity") + 
        facet_grid(.~ Insurance_Category) + 
        theme_bw() +
        theme(axis.text = element_text(size = 10),
              strip.text = element_text(size = 12),
              axis.text.x = element_blank()) +
        ggtitle("Sample Size: Education Level") +
        ylab("Population") +
        xlab("Education Attainment") 

english_vs_insurance <-
        ggplot(english_table3, aes(x = Level, 
                                   y = Proportion, 
                                   fill = Covered)) + 
        geom_bar(stat = "identity", 
                 position = "fill", 
                 width = 0.8) + 
        facet_grid(.~Category) + 
        theme_bw() +
        theme(axis.text = element_text(size = 10),
              strip.text = element_text(size = 12)) +
        ggtitle("Health Insurance Coverage by English Level") +
        ylab("Proportion of Health Insurance Coverage") +
        xlab("English Level") 



education_vs_insurance <- 
        ggplot(edu_table4, aes(x = Education_Attainment, 
                               y = Proportion)) +
        geom_point(size = 4, 
                   aes(color = Education_Attainment)) +
        facet_grid(.~ Category) +
        theme_bw() +
        theme(legend.title = element_blank(), 
              axis.text = element_text(size = 10),
              strip.text = element_text(size = 12),
              axis.text.x = element_blank()) +
        ggtitle("Health Insurance Coverage by Education Level") +
        ylab("Health Insurance Coverage \n(Proportion of Total Peoeple in the Same Education Attainment)") +
        xlab("Education Attainment") 

# final accuracy table presentation

pred_table_vis <- formattable(pred_table, 
                              list(area(col = 2:5) ~ color_tile("white", "lightpink")))

AUC_table_vis <- formattable(AUC_table, 
                             list(area(col = 2:5) ~ color_tile("white", "#FFCC66")))


