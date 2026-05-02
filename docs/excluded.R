################################################################################
# Exploring

**GDP per capita compared to spending on education**
  
  ## Left Column
  
  ### Average GDP per capita over 10 years
  
  ```{r GDP world table, eval=TRUE, echo=FALSE, message=FALSE}
library(tinytable)
library(dplyr)

gdp_table <- gdp_data %>% 
  filter(year >= 2014 & year <= 2023) %>% 
  group_by(entity) %>% 
  summarize(gdp_pcap = mean(gdp_pcap)) %>% 
  #arrange(desc(gdp_pcap)) %>% 
  mutate("rank" = row_number(desc(gdp_pcap)))
tt(gdp_table)
```

## Left-mid Column

### Average government expenditure on education over 10 years

```{r spending world table, eval=TRUE, echo=FALSE, message=FALSE}
library(tinytable)
library(dplyr)

spending_table <- edu_spending %>% 
  filter(year >= 2014 & year <= 2023) %>% 
  group_by(entity) %>% 
  summarize(edu_spending_gdp = mean(edu_spending_gdp)) %>% 
  #arrange(desc(edu_spending_gdp)) %>% 
  mutate("rank" = row_number(desc(edu_spending_gdp)))
tt(spending_table)
```

## Right-mid Column

### Average gender inequality (Atkinson index) over 10 years

```{r inequality world table, eval=TRUE, echo=FALSE, message=FALSE}
library(tinytable)
library(dplyr)

ineq_table <- edu_equality %>% 
  filter(year >= 2014 & year <= 2023) %>% 
  group_by(entity) %>% 
  summarize(ineq_edu = mean(ineq_edu)) %>% 
  #arrange(desc(ineq_edu)) %>% 
  mutate("rank" = row_number(desc(ineq_edu)))
tt(ineq_table)
```

## Right Column

### Average enrollment over 10 years

```{r gender edu world table, eval=TRUE, echo=FALSE, message=FALSE}
library(tinytable)
library(dplyr)
library(tidyr)

gender_edu_table <- edu_gender %>% 
  filter(year >= 2014 & year <= 2023) %>% 
  group_by(entity) %>% 
  pivot_longer(cols = f_tertiary:f_upper_secondary,
               names_to = "level",
               values_to = "enrollment") %>% 
  summarize(enrollment = mean(enrollment, na.rm = TRUE)) %>% 
  #arrange(desc(enrollment)) %>% 
  mutate("rank" = row_number(desc(enrollment)))
tt(gender_edu_table)
```
################################################################################

gender_edu_fem <- edu_gender %>% 
  filter(year >= 2014 & year <= 2023) %>% 
  filter(entity %in% c("Burundi","Czechia","Greece","Luxembourg","Guinea","Somalia","Tuvalu","United States","Democratic Republic of Congo")) %>% 
  pivot_longer(cols = contains("f_"),
               names_to = "female",
               values_to = "f_enrollment") %>% 
  pivot_longer(cols = contains("m_"),
               names_to = "male",
               values_to = "m_enrollment")

edu_gender_c <- edu_gender %>% 
  filter(year >= 2014 & year <= 2023) %>% 
  filter(entity %in% c("Burundi","Czechia","Greece","Luxembourg","Guinea","Somalia","Tuvalu","United States","Democratic Republic of Congo"))

ggplot(edu_gender_c,
       aes(x = year,
           y = f_tertiary,
           color = entity))+
  geom_path()+
  theme_bw()

ggplot(gender_edu_fem,
       aes(x = year,
           y = unique(f_enrollment),
           color = entity))+
  geom_path()+
  theme_bw()






