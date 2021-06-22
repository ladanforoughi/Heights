library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(kableExtra)
data(heights)
options(digits = 3)

head(heights)
kable(head(heights) , "pandoc" , caption = "Heights Datasets")

# the percentage of data is female and male 
Percentage  <- heights %>% 
  summarise (F_perc.= mean(sex=="Female"),
             M_perc.= mean(sex == "Male"),
             F_no. = sum(sex=="Female"),
             M_no. = sum(sex == "Male"))

kable(Percentage , "pandoc", caption = "Percentage and Number of Female and Male in Heights Dataset")

# Average and standard deviation of height based on sex
sex_avg_height <- heights %>% 
  group_by(sex) %>% 
  summarize(Average_of_Height = mean(height), 
            standard_deviation = sd(height)) 
kable(sex_avg_height ,"pandoc", caption = "Average of Height in Female/Male group")

# plot of the sex percentage 
sex_avg_height %>% group_by(sex) %>%
  ggplot(aes(sex,Average_of_Height)) + 
  geom_bar(stat= "identity") + 
  ggtitle("Average of Female and Male in Heights dataset")

ggsave("figs/bar-sex_avgHeight.png")

QC25to75<- heights %>% group_by(sex) %>% summarise(Q25 = quantile(height,0.25),
                                        Q50 = quantile(height,0.5),
                                        Q75 = quantile(height, 0.75))

kable(QC25to75 , "pandoc", caption = "Quantile of Height in Female and Male")

