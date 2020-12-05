### Load database -------------------------------------------------------------
#Configurate wd
setwd("C:/Users/veren/Docker/Evaluar")


#Abrir base de datos
paqraw_ed <- readr::read_delim("01_Data/2020-01-24_Base-CIP_ED.csv", delim = ";",
                               col_names = TRUE, na = c("#N/A", "#NV", "", "NA", "NaN"),
                               locale = locale(encoding = "latin1"), skip_empty_rows = T) %>% 
  mutate_if(is.character, tolower)


summary(paqraw_ed)
head(paqraw_ed)


paqraw <- paqraw_ed
rm(paqraw_ed)



### Generate fictitious database------------------------------------------------
##Generate covariance matrix from real data
data.frame(names(paqraw))

paqraw_cvm <- paqraw[c(12:20, 24)]

paqraw_cvm <- paqraw_cvm %>% 
  mutate(nota = as.numeric(as.factor(case_when(`NOTA DESEMPEÑO ALCANZADO (LETRA)` %in% c("a", "b") ~ 3,
                                               `NOTA DESEMPEÑO ALCANZADO (LETRA)` == "c" ~ 2,
                                               `NOTA DESEMPEÑO ALCANZADO (LETRA)` == "d" ~ 1)))) %>% 
  dplyr::select(-`NOTA DESEMPEÑO ALCANZADO (LETRA)`)

cvm <- cov(paqraw_cvm, use = "complete.obs")


##Generate fabricated dataset
set.seed = 1

perf <- data.frame(MASS::mvrnorm(n = 225,
                                 mu = apply(paqraw_cvm, 2, function(x) mean(x, na.rm = T)),
                                 Sigma = cvm,
                                 empirical = T))


#Correct names
names(perf) <- c("quality", "service_orientation", "innovation",
                 "organisation", "problem_solving", "curiosity",
                 "determination", "analysis", "empowerment",
                 "performance")


#Insert missing values
data.frame(round((colMeans(is.na(paqraw_cvm)*100)),2))

perf <- perf %>% 
  mutate_at(vars(organisation, problem_solving),
                   ~ifelse(sample(c(TRUE, FALSE), size = length(.),
                                  replace = TRUE, prob = c((100-3.11), 3.11)),
                           as.numeric(.), NA))


#Round to integers
perf[c(1:5, 10)] <- apply(perf[c(1:5, 10)], 2, function(x) round(x))


#Correct ranges
summary(perf)

perf <- perf %>% 
  mutate_at(vars(1:5), ~case_when(. > 5 ~ 5,
                                  TRUE ~ .)) %>% 
  mutate_at(vars(6:9), ~case_when(. > 1 ~ 1,
                                  TRUE ~ .)) %>%
  mutate(performance = case_when(performance < 1 ~ 1,
                                 TRUE ~ performance)) 



#Convert performance into ordered factor
perf <- perf %>% 
  mutate(performance = as.factor(case_when(performance == 3 ~ "A",
                                           performance == 2 ~ "B",
                                           performance == 1 ~ "C")),
         performance = ordered(performance, levels = c("C", "B", "A")))


#Summary
summary(perf)

round(cor(mutate(perf, performance = as.numeric(performance)),
          use = "complete.obs"),2)



### Save database------------------------------------------------
setwd("C:/Users/veren/github/ML_Project_Predict_Employee_Performance")


write_csv(perf, path = "01_Data/performance.csv")