library(tidyverse)
library(naivebayes)
library(rpart)
library(randomForest)
library(pROC)
library(ggplot2)
library(formattable)

g <- glimpse
h <- head   
s <- summary   
t <- tail   

dc <- read_csv("psam_p11.csv")
md <- read_csv("psam_md.csv") 


#################### PRIMARY DATA CLEANING #############################

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


# primary data cleaning functions 
clean_func1 <- function(df) {
        df1 <- df %>%
                select(HINS1, HINS2, HICOV, SCHL, ENG) %>%
                mutate(SCHL = as.numeric(SCHL)) 
        df2 <- df1[complete.cases(df1), ]
        df3 <- df2 %>%
                left_join(ENG_d, by = "ENG") %>%
                left_join(SCHL_d, by = "SCHL") %>%
                mutate(Work_Insurance = ifelse(HINS1 == 1, "Yes", "No"),
                       Private_Insurance = ifelse(HINS2 == 1, "Yes", "No"),
                       Health_Insurance_Covered = ifelse(HICOV == 1, "Yes", "No")) %>%
                mutate(HINS1 = ifelse(HINS1 == 1, 1, 0),
                       HINS2 = ifelse(HINS2 == 1, 1, 0),
                       HICOV = ifelse(HICOV == 1, 1, 0))
        return(df3)
}



# cleaned data
md1 <- clean_func1(md)
dc1 <- clean_func1(dc)






########################## PREDICTIONS #############################


######### train_data = md1, test_data = dc1
                              

# Prediction model w logistic regression
logistic_prediction <- function(train_data, train_input, test_data) {
        logistic_model <- glm(train_input ~ 
                                      English + 
                                      Education, 
                              data = train_data, 
                              family = "binomial")
        logistic_prob <- predict(logistic_model, newdata = test_data, type = "response")
        logistic_prediction <- ifelse(logistic_prob > 0.5, 
                                      "Yes", 
                                      "No")
        return(logistic_prediction)
}



# Prediction model w Bayesian mothod
bayes_prediction <- function(train_data, train_input, test_data) {
        bayes_model <- naive_bayes(train_input ~ 
                                           English + 
                                           Education, 
                                   data = train_data,
                                   laplace = 1)
        bayes_prediction <- predict(bayes_model, newdata = test_data)
        return(bayes_prediction)
}

# Prediction model w classification tree (no pruning)
tree_prediction <- function(train_data, train_input, test_data) {
        tree_model <- rpart(train_input ~ 
                                    English + 
                                    Education,
                            data = train_data, method = "class")
        tree_prediction <- predict(tree_model, 
                                   newdata = test_data, 
                                   type = "class")
        return(tree_prediction)
}


# Prediction model w random forest 
randomforest_prediction <- function(train_data, train_input, test_data) {
        forest_model <- randomForest(train_input ~
                                             English +
                                             Education, 
                                     data = train_data, 
                                     ntree = 200)
        forest_prob <- predict(forest_model, newdata = test_data)
        forest_prediction <- ifelse(forest_prob > 0.5, 
                                    "Yes", 
                                    "No")
        return(forest_prediction)
}

# arguments: train_data, train_input, test_data
# Predict coverage of work insurance via logistic regression
work_logistic_accuracy <- mean(logistic_prediction(md1, 
                                                   md1$HINS1, 
                                                   dc1) == dc1$Work_Insurance)

# Predict coverage of private insurance via logistic regression 
private_logistic_accuracy <- mean(logistic_prediction(md1, 
                                                      md1$HINS2, 
                                                      dc1) == dc1$Private_Insurance)

# Predict coverage of insurance via logistic regression 
insurance_logistic_accuracy <- mean(logistic_prediction(md1, 
                                                        md1$HICOV, 
                                                        dc1) == dc1$Health_Insurance_Covered)




# Predict coverage of work insurance via Bayesian 
work_bayes_accuracy <- mean(bayes_prediction(md1, md1$Work_Insurance, 
                                             dc1) == dc1$Work_Insurance)

# Predict coverage of private insurance via Bayesian 
private_bayes_accuracy <- mean(bayes_prediction(md1, md1$Private_Insurance, 
                                                dc1) == dc1$Private_Insurance)

# Predict coverage of insurance via Byesian
insurance_bayes_accuracy <- mean(bayes_prediction(md1, md1$Health_Insurance_Covered, 
                                                  dc1) == dc1$Health_Insurance_Covered)






# Predict coverage of work insurance via classification tree 
work_tree_accuracy <- mean(tree_prediction(md1, md1$Work_Insurance, 
                                               dc1) == dc1$Work_Insurance)
                                                   

# Predict coverage of private insurance via classification tree 
private_tree_accuracy <- mean(tree_prediction(md1, md1$Private_Insurance, 
                                                  dc1) == dc1$Private_Insurance)
                                                      

# Predict coverage of insurance via classification tree  
insurance_tree_accuracy <- mean(tree_prediction(md1, md1$Health_Insurance_Covered, 
                                                    dc1) == dc1$Health_Insurance_Covered)
                                                        




# Predict coverage of work insurance via random forest 
work_rforest_accuracy <- mean(randomforest_prediction(md1, 
                                                      md1$HINS1, 
                                                      dc1) == dc1$Work_Insurance)

# Predict coverage of private insurance via random forest 
private_rforest_accuracy <- mean(randomforest_prediction(md1, 
                                                         md1$HINS2, 
                                                         dc1) == dc1$Private_Insurance)

# Predict coverage of insurance via random forest 
insurance_rforest_accuracy <- mean(randomforest_prediction(md1, 
                                                           md1$HICOV, 
                                                           dc1) == dc1$Health_Insurance_Covered)






###################### ACCURACY EVALUATION ##########################

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






########################## AUC EVALUATION ###############################

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
# actual_vec: numeric, 
# prob_vec: categorical
work_auc <- function(prob_vec) {
        roc <- roc(dc1$HINS1, cat_to_num(prob_vec))
        return(auc(roc))
}

private_auc <- function(prob_vec) {
        roc <- roc(dc1$HINS2, cat_to_num(prob_vec))
        return(auc(roc))
}

insurance_auc <- function(prob_vec) {
        roc <- roc(dc1$HICOV, cat_to_num(prob_vec))
        return(auc(roc))
}
        
        
work_logistic_auc <- work_auc(logistic_prediction(md1, 
                                                  md1$HINS1, 
                                                  dc1))

private_logistic_auc <- private_auc(logistic_prediction(md1, 
                                                        md1$HINS2, 
                                                        dc1))

insurance_logistic_auc <- insurance_auc(logistic_prediction(md1, 
                                                            md1$HICOV, 
                                                            dc1))


work_bayes_auc <- work_auc(bayes_prediction(md1, 
                                               md1$Work_Insurance, 
                                               dc1))

private_bayes_auc <- private_auc(bayes_prediction(md1, 
                                                  md1$Private_Insurance, 
                                                  dc1))

insurance_bayes_auc <- insurance_auc(bayes_prediction(md1, 
                                                         md1$Health_Insurance_Covered, 
                                                         dc1))


work_tree_auc <- work_auc(tree_prediction(md1, 
                                          md1$Work_Insurance, 
                                          dc1))

private_tree_auc <- private_auc(tree_prediction(md1, 
                                                 md1$Private_Insurance, 
                                                 dc1))

insurance_tree_auc <- insurance_auc(tree_prediction(md1, 
                                                     md1$Health_Insurance_Covered, 
                                                     dc1))

work_rforest_auc <- work_auc(randomforest_prediction(md1, 
                                                     md1$HINS1, 
                                                     dc1))

private_rforest_auc <- private_auc(randomforest_prediction(md1, 
                                                       md1$HINS2, 
                                                       dc1))

insurance_rforest_auc <- insurance_auc(randomforest_prediction(md1, 
                                                           md1$HICOV, 
                                                           dc1))

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

# data cleaning for plotting  




########################### DATA PRESENTATION ################################

# data cleaning 

dc_cat <- list(w = dc1$Work_Insurance, 
               p = dc1$Private_Insurance, 
               i = dc1$Health_Insurance_Covered,
               eng = dc1$English,
               edu = dc1$Education)
md_cat <- list(w = md1$Work_Insurance,
               p = md1$Private_Insurance,
               i = md1$Health_Insurance_Covered,
               eng = md1$English,
               edu = md1$Education)

ins <- c("w", "p", "i")
e <- c("eng", "edu")

make_table <- function(lst, ins, q) { 
        df <- dc1
        for (x in ins) {
                a <- unlist(lst[x])
                b <- unlist(lst[q])
                df1 <- as.data.frame(table(a, b)) %>%
                        mutate(Insurance = x)
                if (identical(df, dc1)) {
                        df <- df1
                } else {
                        df <- rbind(df, df1)
                }
                
        }
        return(df)
        }



table_cleaning <- function(df, tit) {
        names(df) <- c("Coverage", tit, "Population", "Insurance", "State")
        df1 <- df %>% 
                mutate(Insurance = recode(Insurance,
                                          w = "Work",
                                          p = "Private",
                                          i = "Work, Private, or Other")) %>%
                mutate(Insurance = factor(Insurance, 
                                          levels = c("Work",
                                                     "Private",
                                                     "Work, Private, or Other")),
                       State = factor(State, 
                                      levels = c("MD", "DC")))
        
        return(df1)
}


dc_english <- make_table(dc_cat, ins, "eng") %>%
        mutate(State = "DC")
md_english <- make_table(md_cat, ins, "eng") %>%
        mutate(State = "MD")
dc_education <- make_table(dc_cat, ins, "edu") %>% 
        mutate(State = "DC")
md_education <- make_table(md_cat, ins, "edu") %>%
        mutate(State = "MD")
        
english <- rbind(dc_english, md_english)
education <- rbind(dc_education, md_education)

english1 <- table_cleaning(english, "English")
education1 <- table_cleaning(education, "Education")


# final accuracy and auc ables presentation

pred_table_vis <- formattable(pred_table, 
                              list(area(col = 2:5) ~ 
                                           color_tile("white", "lightpink")))

AUC_table_vis <- formattable(AUC_table, 
                             list(area(col = 2:5) ~ 
                                          color_tile("white", "#FFCC66")))

# final population plots (sample size presentation)

english_sample_size_plot <-
        ggplot(english1, aes(x = English, 
                             y = Population, 
                             fill = Coverage)) +
        geom_bar(stat = "identity", width = 0.8) + 
        facet_grid(State ~ Insurance) + 
        theme_bw() +
        theme(axis.text = element_text(size = 10),
              strip.text = element_text(size = 12)) +
        ggtitle("Sample Size: English Level") +
        ylab("Population") +
        xlab("English Level") 

education_sample_size_plot <-
        ggplot(education1, aes(x = Education, 
                             y = Population, 
                             fill = Coverage)) +
        geom_bar(stat = "identity", width = 0.8) + 
        facet_grid(State ~ Insurance) + 
        theme_bw() +
        theme(axis.text = element_text(size = 10),
              strip.text = element_text(size = 12),
              axis.text.x = element_blank()) +
        ggtitle("Sample Size: Education Level") +
        ylab("Population") +
        xlab("Education Attainment: No Schooling (Left) to Doctorate Degree (Right)") 


english_proportion_plot <-
        ggplot(english1, aes(x = English, 
                             y = Population, 
                             fill = Coverage)) +
        geom_bar(stat = "identity", 
                 width = 0.8, 
                 position = "fill") + 
        facet_grid(State ~ Insurance) + 
        theme_bw() +
        theme(axis.text = element_text(size = 10),
              strip.text = element_text(size = 12)) +
        ggtitle("Proportion: English Level") +
        ylab("Proportion") +
        xlab("English Level") 


education_proportion_plot <-
        ggplot(education1, aes(x = Education, 
                               y = Population, 
                               fill = Coverage)) +
        geom_bar(stat = "identity", 
                 width = 0.8,
                 position = "fill") + 
        facet_grid(State ~ Insurance) + 
        theme_bw() +
        theme(axis.text = element_text(size = 10),
              strip.text = element_text(size = 12),
              axis.text.x = element_blank()) +
        ggtitle("Proportion: Education Level") +
        ylab("Proportion") +
        xlab("Education Attainment: No Schooling (Left) to Doctorate Degree (Right)")