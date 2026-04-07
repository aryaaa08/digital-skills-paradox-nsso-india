library(haven) 
library(dplyr) 
library(ggplot2) 
library(car) 
library(broom) 

hh<- read_sav("D:/tiss academic material/SEM 3/Micro and Macro 
Databases/asssignment/NSSO/CMST80HH.sav") 
per<- read_sav("D:\\tiss academic material\\SEM 3\\Micro and Macro 
Databases\\asssignment\\NSSO\\CMST80PER.sav") 
hh <- hh %>% 
  mutate( 
    mpce_q = ntile(b3q5, 5) 
  ) 
hh <- hh %>% 
  mutate( 
    haqi_access = ifelse(b5q2 %in% c(1, "Yes", "YES"), 1, 0) 
  ) 
hh <- hh %>% 
  mutate( 
    haqi_fixed = ifelse(b5q4_i %in% c(1, "Yes", "YES"), 1, 0) 
  ) 
hh_clean <- hh %>% 
  select( 
    fsu, ssu, 
    sector, 
    b3q3,      
    b3q4,      
    mpce_q, 
    # religion 
    # social group 
    haqi_access, 
    haqi_fixed 
  ) 
per <- per %>% 
  mutate( 
    able_net = ifelse(b4q9 %in% c(1, "Yes", "YES"), 1, 0), 
    used_net = ifelse(b4q10 %in% c(1, "Yes", "YES"), 1, 0) 
  ) 
per <- per %>% 
  mutate( 
    email        
    = ifelse(b4q11 %in% c(1, "Yes", "YES"), 1, 0), 
    banking      
    = ifelse(b4q12 %in% c(1, "Yes", "YES"), 1, 0), 
    copypaste    = ifelse(b4q17 %in% c(1, "Yes", "YES"), 1, 0), 
    presentation = ifelse(b4q19 %in% c(1, "Yes", "YES"), 1, 0), 
    document     = ifelse(b4q20 %in% c(1, "Yes", "YES"), 1, 0) 
  ) 
per <- per %>% 
  mutate( 
    use_computer = ifelse(use_comp %in% c(1, "Yes", "YES"), 1, 0) 
  ) 
per <- per %>% 
  mutate( 
    IDCS = able_net + used_net + 
      email + banking + copypaste + 
      presentation + document 
  ) 

#MERGE HOUSEHOLD + PERSON DATA 
data <- per %>% 
  left_join(hh_clean, by = c("fsu", "ssu")) 

data <- data %>% 
  mutate( 
    HAQI = haqi_access + haqi_fixed + use_computer 
  ) 

data <- data %>% 
  rename(sector = sector.y) %>% 
  select(-sector.x) 


data %>% 
  group_by(b3q4, sector) %>% 
  summarise( 
    mean_IDCS = mean(IDCS, na.rm = TRUE), 
    n = n(), 
    .groups = "drop" 
  ) 

data <- data %>% 
  mutate( 
    social_group = factor( 
      b3q4, 
      levels = c(1, 2, 3, 9), 
      labels = c("Scheduled Tribe (ST)", 
                 "Scheduled Caste (SC)", 
                 "Other Backward Class (OBC)", 
                 "Others (General)") 
    ), 
    sector_lab = factor( 
      sector, 
      levels = c(1, 2), 
      labels = c("Rural", "Urban") 
    ) 
  ) 
ggplot(data, aes(x = social_group, y = IDCS, fill = sector_lab)) + 
  stat_summary(fun = mean, geom = "bar", position = "dodge") + 
  labs( 
    title = "Digital Capability Divide across Social Groups and Sector", 
    subtitle = "Mean Individual Digital Capability Score (IDCS) by Caste Group and Rural–Urban Location", 
    x = "Social Group (Caste Category)", 
    y = "Mean Digital Capability Score (IDCS)", 
    fill = "Sector", 
    caption = "Source: NSSO 80th Round Modular Telecom Survey (2025)" 
  ) + 
  scale_fill_manual( 
    values = c("Rural" = "#F8766D", "Urban" = "#00BFC4") 
  ) + 
  theme_minimal() + 
  theme( 
    plot.title = element_text(size = 14, face = "bold"), 
    plot.subtitle = element_text(size = 11), 
    axis.title = element_text(size = 11), 
    axis.text = element_text(size = 10), 
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 9), 
    plot.caption = element_text(size = 9, hjust = 0) 
  ) 
logit_model <- glm( 
  used_net ~ gender + age + factor(b3q4) + 
    factor(sector) + factor(mpce_q) + HAQI, 
  data = data, 
  family = binomial 
) 
exp(coef(logit_model)) 
data <- data %>% 
  mutate(age_num = as.numeric(age)) 
# LINEAR REGRESSION 
linear_model <- lm( 
  IDCS ~ gender + age_num + factor(b3q4) + 
    factor(sector) + factor(mpce_q) + HAQI, 
  data = data 
) 
summary(linear_model) 
linear_tidy <- tidy(linear_model) 
linear_table <- linear_tidy %>% 
  mutate( 
    term = case_when( 
      term == "(Intercept)" ~ "Intercept", 
      term == "gender2" ~ "Female (ref: Male)", 
      term == "gender3" ~ "Other gender", 
      term == "age_num" ~ "Age (years)", 
      term == "factor(b3q4)2" ~ "SC (ref: ST)", 
      term == "factor(b3q4)3" ~ "OBC (ref: ST)", 
      term == "factor(b3q4)9" ~ "Others (ref: ST)", 
      term == "factor(sector)2" ~ "Urban (ref: Rural)", 
      term == "factor(mpce_q)2" ~ "MPCE Q2 (ref: Q1)", 
      term == "factor(mpce_q)3" ~ "MPCE Q3 (ref: Q1)", 
      term == "factor(mpce_q)4" ~ "MPCE Q4 (ref: Q1)", 
      term == "factor(mpce_q)5" ~ "MPCE Q5 (ref: Q1)", 
      term == "HAQI" ~ "Household Access Quality Index (HAQI)", 
      TRUE ~ term 
    ) 
  ) 
linear_table %>% 
  select(term, estimate, std.error, p.value) %>% 
  arrange(desc(abs(estimate))) 
# data <- data %>% 
#   mutate( 
#     exec_copy = ifelse(b4q17_1 %in% c(1, "Yes", "YES"), 1, 0), 
#     exec_msg  = ifelse(b4q18_1 %in% c(1, "Yes", "YES"), 1, 0), 
#     exec_ppt  = ifelse(b4q19_1 %in% c(1, "Yes", "YES"), 1, 0) 
#   ) 
data <- data %>% 
  select(-matches("^gap_|^CUG$")) 
data <- data %>% 
  mutate( 
    exec_copy = ifelse(b4q17_1 %in% c(1, "Yes", "YES"), 1, 0), 
    exec_msg  = ifelse(b4q18_1 %in% c(1, "Yes", "YES"), 1, 0), 
    exec_ppt  = ifelse(b4q19_1 %in% c(1, "Yes", "YES"), 1, 0) 
  ) 
data <- data %>% 
  mutate( 
    gap_copy = ifelse(copypaste == 1, 1 - exec_copy, NA), 
    gap_msg  = ifelse(email == 1, 1 - exec_msg, NA), 
    gap_ppt  = ifelse(presentation == 1, 1 - exec_ppt, NA) 
  ) 
data <- data %>% 
  mutate( 
    CUG = rowSums( 
      cbind(gap_copy, gap_msg, gap_ppt), 
      na.rm = TRUE 
    ) 
  ) 

data %>% 
  group_by(sector) %>% 
  summarise( 
    mean_CUG = mean(CUG, na.rm = TRUE) 
  ) 

ggplot(data, aes(x = sector, y = CUG, fill = sector)) + 
  stat_summary(fun = mean, geom = "bar", width = 0.6) + 
  labs( 
    title = "Capability–Utilisation Gap in Digital Skills", 
    subtitle = "Average unused digital skills among individuals by sector", 
    x = "Sector", 
    y = "Mean Capability–Utilisation Gap (CUG)", 
    fill = "Sector", 
    caption = "Source: NSSO 80th Round Modular Telecom Survey (2025)" 
  ) + 
  scale_x_discrete( 
    labels = c("Rural", "Urban") 
  ) + 
  scale_fill_manual( 
    values = c("Rural" = "#F8766D", "Urban" = "#00BFC4") 
  ) + 
  theme_minimal() + 
  theme( 
    plot.title = element_text(size = 14, face = "bold"), 
    plot.subtitle = element_text(size = 11), 
    axis.title = element_text(size = 11), 
    axis.text = element_text(size = 10), 
    legend.position = "none", 
    plot.caption = element_text(size = 9, hjust = 0) 
  ) 


ggplot(data, aes(x = social_group, y = CUG, fill = sector_lab)) + 
  stat_summary(fun = mean, geom = "bar", position = "dodge") + 
  labs( 
    title = "Capability–Utilisation Gap across Social Groups and Sector", 
    subtitle = "Mean unused digital skills by caste group and location", 
    x = "Social Group", 
    y = "Mean Capability–Utilisation Gap (CUG)", 
    fill = "Sector", 
    caption = "Source: NSSO 80th Round Modular Telecom Survey (2025)" 
  ) + 
  theme_minimal() 

data <- data %>% 
  mutate( 
    gender_f = factor(gender), 
    
    social_group_f = factor( 
      b3q4, 
      levels = c(1, 2, 3, 9), 
      labels = c("ST", "SC", "OBC", "Others") 
    ), 
    
    sector_f = factor( 
      sector, 
      levels = c(1, 2), 
      labels = c("Rural", "Urban") 
    ), 
    
    mpce_f = factor( 
      mpce_q, 
      levels = 1:5, 
      labels = c("Poorest", "Poor", "Middle", "Rich", "Richest") 
    ), 
    
    device_f = factor(b4q13), 
    
    freq_f = factor(b4q15) 
  ) 

ggplot(data, aes(x = factor(CUG))) + 
  geom_bar(fill = "steelblue", width = 0.6) + 
  labs( 
    title = "Distribution of Capability–Utilisation Gap (CUG)", 
    subtitle = "How many digital skills people know but do not use", 
    x = "Number of Unused Digital Skills (CUG Score)", 
    y = "Number of Individuals" 
  ) + 
  scale_x_discrete( 
    labels = c( 
      "0" = "0 (No unused skills)", 
      "1" = "1 (unused skill)", 
      "2" = "2 (unused skill)", 
      "3" = "3 (unused skills)" 
    ) 
  ) + 
  theme_minimal() + 
  theme( 
    plot.title = element_text(size = 14, face = "bold"), 
    plot.subtitle = element_text(size = 11), 
    axis.title = element_text(size = 11), 
    axis.text = element_text(size = 10), 
    plot.caption = element_text(size = 9, hjust = 0) 
  ) 


mean(data$CUG > 0, na.rm = TRUE) * 100 

data <- data %>% 
  mutate( 
    CUG_cat = case_when( 
      CUG == 0 ~ "No Gap", 
      CUG == 1 ~ "Low Gap (1 skill unused)", 
      CUG == 2 ~ "Medium Gap (2 skills unused)", 
      CUG == 3 ~ "High Gap (3 skills unused)" 
    ) 
  ) 
model_a1<- lm( 
  CUG~ gender_f+ age_num+ sector_f+ social_group_f+ mpce_f, 
  data= data 
) 
summary(model_a1) 
model_a2<- lm( 
  CUG~ gender_f+ age_num+ sector_f+ social_group_f+ 
    mpce_f+ HAQI+ freq_f+ device_f, 
  data= data 
) 
summary(model_a2) 
data<- data%>% 
  mutate( 
    high_CUG= ifelse(CUG==3,1,0) 
  ) 
model_b<- glm( 
  high_CUG~ gender_f+ age_num+ sector_f+ social_group_f+ 
    mpce_f+ HAQI+ freq_f+ device_f, 
  family= binomial, 
  data= data 
) 
summary(model_b) 
exp(coef(model_b)) 
library(nnet) 
data$CUG_cat<- relevel(as.factor(data$CUG_cat), ref="No Gap") 
model_c<- multinom( 
  CUG_cat~ gender_f+ age_num+ sector_f+ 
    social_group_f+ mpce_f+ HAQI, 
  data= data 
) 
summary(model_c) 
exp(coef(model_c)) 
model_d<- lm( 
  CUG~ gender_f* social_group_f+ 
    age_num+ sector_f+ mpce_f+ HAQI, 
  data= data 
) 

summary(model_d) 

model_e<- lm( 
  CUG~ device_f* sector_f+ 
    gender_f+ age_num+ social_group_f+ mpce_f, 
  data= data 
) 

summary(model_e) 


library(ggplot2) 

plot(pred_caste) + 
  geom_point(size = 3, color = "#0072B2") + 
  geom_errorbar( 
    aes(ymin = conf.low, ymax = conf.high), 
    width = 0.15, 
    color = "#0072B2" 
  ) + 
  labs( 
    title = "Adjusted Capability–Utilisation Gap by Social Group", 
    subtitle = "Predicted unused digital skills after controlling for access quality and socio-economic factors", 
    x = "Social Group", 
    y = "Predicted Number of Unused Digital Skills (CUG)", 
    caption = "Predicted values from Model A2 (controls: age, gender, sector, income, HAQI, device type, 
frequency)" 
  ) + 
  scale_y_continuous( 
    labels = function(x) round(x, 3) 
  ) + 
  theme_minimal() + 
  theme( 
    plot.title = element_text(size = 14, face = "bold"), 
    plot.subtitle = element_text(size = 11), 
    axis.title = element_text(size = 11), 
    axis.text = element_text(size = 10), 
    plot.caption = element_text(size = 9, hjust = 0) 
  ) 

library(ggeffects) 
plot(pred_haqi) + 
  labs( 
    title = "Effect of Household Access Quality on Unused Digital Skills", 
    subtitle = "Predicted Capability–Utilisation Gap after controlling for socio-economic factors", 
    x = "Household Access Quality Index (HAQI)", 
    y = "Predicted Number of Unused Digital Skills (CUG)", 
    caption = "Predicted values from Model A2; controls include age, gender, caste, income, sector, device type, 
and internet frequency" 
  ) + 
  scale_x_continuous( 
    breaks = 0:3, 
    labels = c( 
      "0\n(Poor access)", 
      "1", 
      "2", 
      "3\n(High-quality access)" 
    ) 
  ) + 
  scale_y_continuous( 
    labels = function(x) round(x, 3) 
  ) + 
  theme_minimal() + 
  theme( 
    plot.title = element_text(size = 14, face = "bold"), 
    plot.subtitle = element_text(size = 11), 
    axis.title = element_text(size = 11), 
    axis.text = element_text(size = 10), 
    plot.caption = element_text(size = 9, hjust = 0) 
  ) 


data%>% 
  summarise( 
    CopyPaste_Gap= mean(gap_copy, na.rm=TRUE)*100, 
    Email_Gap= mean(gap_msg, na.rm=TRUE)*100, 
    Presentation_Gap= mean(gap_ppt, na.rm=TRUE)*100 
  ) 

model_copy<- glm( 
  gap_copy~ gender_f+ age_num+ sector_f+ social_group_f+ device_f, 
  family= binomial, 
  data= data 
) 

model_ppt<- glm( 
  gap_ppt~ gender_f+ age_num+ sector_f+ social_group_f+ device_f, 
  family= binomial, 
  data= data 
) 

data<- data%>% 
  mutate( 
    CUG_weighted=0.3* gap_copy+ 
      0.3* gap_msg+ 
      0.4* gap_ppt 
  ) 

data_net<- data%>% filter(used_net==1) 
model_robust<- lm( 
  CUG~ gender_f+ age_num+ sector_f+ social_group_f+ HAQI, 
  data= data_net 
) 
summary(model_robust)