library(tidyverse)
library(ideanet)
library(psych)
library(sjPlot)
library(ggplot2)
library(pandoc)
library(jtools)
library(interactions)
library(vtable)

load("~/Desktop/SOAN 244/wlu_nets (1).Rda")

list2env(wlu_nets, .GlobalEnv)
egos <- egos %>% left_join(summaries)

glimpse(egos)

#Some of the respondents listed NA for same sport, so I need to take all of them out of the sample
alters <- alters %>%
  filter(!ego_id==26,
         !ego_id==27,
         !ego_id==28,
         !ego_id==29,
         !ego_id==30,
         !ego_id==31,
         !ego_id==59,
         !ego_id==60,
         !ego_id==61,
         !ego_id==62)

egos <- egos %>%
  filter(!ego_id==26,
         !ego_id==27,
         !ego_id==28,
         !ego_id==29,
         !ego_id==30,
         !ego_id==31,
         !ego_id==59,
         !ego_id==60,
         !ego_id==61,
         !ego_id==62)

# making a single column for all belonging scores
egos <- egos %>% mutate(belonging_item_1 = 
                          case_when(belonging_item1_1 == T ~ 1,
                                    belonging_item1_2 == T ~ 2,
                                    belonging_item1_3 == T ~ 3,
                                    belonging_item1_4 == T ~ 4,
                                    belonging_item1_5 == T ~ 5),
                        belonging_item_2 = 
                          case_when(belonging_item2_1 == T ~ 1,
                                    belonging_item2_2 == T ~ 2,
                                    belonging_item2_3 == T ~ 3,
                                    belonging_item2_4 == T ~ 4,
                                    belonging_item2_5 == T ~ 5),
                        belonging_item_3 = 
                          case_when(belonging_item3_1 == T ~ 1,
                                    belonging_item3_2 == T ~ 2,
                                    belonging_item3_3 == T ~ 3,
                                    belonging_item3_4 == T ~ 4,
                                    belonging_item3_5 == T ~ 5),
                        belonging_item_4 = 
                          case_when(belonging_item4_1 == T ~ 1,
                                    belonging_item4_2 == T ~ 2,
                                    belonging_item4_3 == T ~ 3,
                                    belonging_item4_4 == T ~ 4,
                                    belonging_item4_5 == T ~ 5),
                        belonging_item_5 = 
                          case_when(belonging_item5_1 == T ~ 1,
                                    belonging_item5_2 == T ~ 2,
                                    belonging_item5_3 == T ~ 3,
                                    belonging_item5_4 == T ~ 4,
                                    belonging_item5_5 == T ~ 5),
                        belonging_item_6 = 
                          case_when(belonging_item6_1 == T ~ 1,
                                    belonging_item6_2 == T ~ 2,
                                    belonging_item6_3 == T ~ 3,
                                    belonging_item6_4 == T ~ 4,
                                    belonging_item6_5 == T ~ 5),
                        belonging_item_7 = 
                          case_when(belonging_item7_1 == T ~ 1,
                                    belonging_item7_2 == T ~ 2,
                                    belonging_item7_3 == T ~ 3,
                                    belonging_item7_4 == T ~ 4,
                                    belonging_item7_5 == T ~ 5),
                        belonging = (belonging_item_1 + belonging_item_2 + belonging_item_3 + belonging_item_4
                                     + belonging_item_5 + belonging_item_6 + belonging_item_7)/7)
                          
                        
                        


#creating class year column
egos <- egos %>% mutate (class_year = 
                           case_when(class_year_2025=="TRUE" ~ 2025,
                                     class_year_2026=="TRUE" ~ 2026,
                                     class_year_2027=="TRUE" ~ 2027,
                                     class_year_2028=="TRUE" ~ 2028))

# These egos had NA in their sport variable, so it is best to remove them

alters$same_team_Yes <- as.integer(as.logical(alters$same_team_Yes))
alters_same_sport <- alters %>% group_by(ego_id) %>% 
  summarize(prop_same_sport = mean(same_team_Yes, na.rm=T))

egos <- egos %>% left_join(alters_same_sport)


# Making plots of summary statistics for key variables

# TO DO ->> Filter out people who responded FALSE FALSE to greek 1 and 0. Create summary stats of class
# year to explain why Greek involvement distribution may be different than school averages
# then make plots of all of these statistics, also may want to include 
# ELIMINATE ALL FRESHMAN FROM DATA

egos$team_1 <- as.integer(as.logical(egos$team_1))

describe(egos)

# Plot of athletes vs non athletes
ggplot(egos, aes(team_1)) + geom_histogram(binwidth = 0.5, fill = "dodgerblue") + 
  labs(title = "Distribution of Athletes in Sample", 
       x = "Team Status (1=Athlete, 0 = Non-Athlete)")

# making a summary table of key variables

#Plotting Belonging
ggplot(egos, aes(belonging)) + geom_histogram(binwidth = 0.5, fill = "dodgerblue") + 
  labs(title = "Distribution of Belonging Score Across Sample", x = "Belonging", )

#Plotting Belonging bewteen athletes and non athletes
ggplot(egos, aes(x=team_1, y=belonging, fill=factor(team_1))) + geom_boxplot() +
  labs(title = "Distribution of Belonging, Athletes vs. Non-Athletes", x = "Team Status (1=Athlete, 0 = Non-Athlete)")
ggplot(egos, aes(x=team_1, y=prop_same_sport, fill=factor(team_1))) + geom_boxplot() +
  labs(title = "Distribution of Homophily, Athletes vs. Non-Athletes", x = "Team Status (1=Athlete, 0 = Non-Athlete)")



#Data frame of Summary Statistics
  

d <- egos[,c("team_1", "belonging", "prop_same_sport")]

egos$class_year_2025 <- as.integer(as.logical(egos$class_year_2025))
egos$class_year_2026 <- as.integer(as.logical(egos$class_year_2026))
egos$class_year_2027 <- as.integer(as.logical(egos$class_year_2027))
egos$class_year_2028 <- as.integer(as.logical(egos$class_year_2028))
egos$gender_man <- as.integer(as.logical(egos$gender_man))
egos$gender_woman <- as.integer(as.logical(egos$gender_woman))
egos$race_ethn_Indigenous <- as.integer(as.logical(egos$race_ethn_Indigenous))
egos$race_ethn_Asian <- as.integer(as.logical(egos$race_ethn_Asian))
egos$race_ethn_Black <- as.integer(as.logical(egos$race_ethn_Black))
egos$race_ethn_Latinx <- as.integer(as.logical(egos$race_ethn_Latinx))
egos$race_ethn_White <- as.integer(as.logical(egos$race_ethn_White))
egos$race_ethn_Multi <- as.integer(as.logical(egos$race_ethn_Multi))


df <- egos[,c("team_1", "belonging", "class_year_2025", "class_year_2026", "class_year_2027", "class_year_2028", "gender_man", "gender_woman",
              "race_ethn_Indigenous", "race_ethn_Asian", "race_ethn_Black", "race_ethn_Latinx", "race_ethn_White", "race_ethn_Multi")]

summary_table <- describe(d)
summary_table1 <- describe(df)
print(summary_table)
print(summary_table1)



#Regression Model

lm <- lm(belonging ~ team_1 + prop_same_sport*team_1 + class_year_2026 +
           class_year_2027 + class_year_2028 + gender_man +
           race_ethn_Indigenous + race_ethn_Asian + race_ethn_Black + race_ethn_Latinx +
           race_ethn_Multi, data=egos)
summary(lm)
modelsummary::modelsummary(lm, stars = T, output="regression.results2.docx")

summ(lm, confint = TRUE, ci.width=.5)

interact_plot(
  model = lm,
  pred = prop_same_sport,
  modx = team_1,
  modx_labels = c("Non-Athletes", "Athletes"),
  interval = TRUE,
  int.width = 0.95,
  x.label="Proportion Homophilous (Athlete Status)",
  y.label = "Predicted Belonging",
  main.title = "Interaction Effect of Athletes and Athletic Homophily on Belonging ")


