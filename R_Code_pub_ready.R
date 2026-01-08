
# ---R packages
library(readxl)
library(tidyverse)
library(gtsummary)
library(gt)
library(cards)

mydata <- read_excel('P:/abc/demo.xlsx')


mylab <- list(
  id	 = 	"Study ID",
  age	 = 	"Age in year",
  division	 = 	"Administrative area",
  residence	 = 	"Place of residence",
  edu	 = 	"Educational level",
  wealth	 = 	"Wealth index",
  wt	 = 	"Weight in kilograms",
  ht	 = 	"Height in centimeters",
  gender	 = 	"Gender of participants",
  income	 = 	"Monthly income (Thousand)",
  expenditure	 = 	"Monthly expenditure (Thousand)",
  infection	 = 	"Pathogen infection",
  disease	 = 	"Having a disease"
)

#---------Descriptive Analysis with p-value
Tab1 <- mydata %>%
  filter(!is.na(infection)) %>%
  select(-id) %>%
  tbl_summary(
    by=infection,
    missing = 'no',
    label = mylab,
    statistic = list(
      all_categorical() ~'{n} ({p})',
      all_continuous() ~'{mean} ± {sd}'
    ),
    digits = list(
      all_categorical() ~c(0,2),
      all_continuous() ~c(2,2,2)
    )
  ) %>%
  bold_labels() %>%
  add_overall(last=TRUE) %>%
  add_p(
    test=list(
      all_categorical() ~ 'chisq.test',
      all_continuous() ~ 't.test'
    ),
    pvalue_fun =~ style_pvalue(.x, digits = 3)
  )
print(Tab1)


#----------Box plot----------
my_box <- mydata %>%
  select(residence,wealth, ht)%>%
  drop_na() %>%
  ggplot(aes(x=residence, y=ht, fill =wealth )) +
  geom_boxplot(
    outliers = TRUE,
    outlier.color = 'red',
    outlier.size = 2,
    notch = TRUE,
    )+
  labs(
    x='Place of residence',
    y='Respondent height',
    title = 'Distribution of respondent height',
    fill='Wealth index'
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 14,
      face = 'bold'
    ),
    axis.title.x = element_text(
      hjust = 0.5,
      size = 12,
      face = 'italic'
    ),
    axis.title.y = element_text(
      hjust = 0.5,
      size = 12,
      face = 'italic'
    ),
    legend.position = 'top'
    
  ) +
  scale_fill_brewer(palette = 'Set3')

print(my_box)

#---Bar diagram of outcome variable

PDat <- mydata %>%
  select(disease, wealth, residence) %>%
  drop_na() %>%
  group_by(wealth, residence) %>%
  summarise(
    p = mean(disease)*100,
    .groups = 'drop'
  )
#


my_bar <- PDat %>%
  ggplot(aes(x = residence, y = p, fill = wealth)) + 
  geom_col(
    position = position_dodge(width = 0.65),
    width = 0.45,
    color = "white"
  ) + 
  geom_text(
    aes(label = sprintf("%.1f", p)),
    position = position_dodge(width = 0.65),
    size = 4,
    vjust = -0.4
  ) +
  labs(
    x = "Place of residence",
    y = "Percentage of disease",
    fill = "Wealth index"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.line = element_line(color = "grey40")
  ) +
  expand_limits(y = max(PDat$p) + 5)


print(my_bar)

#----Logistic Regression

A2 <- mydata %>%
  select(disease, infection, age, wealth, edu, ht, gender) %>%
  drop_na() %>%
    mutate(
    infection = as.factor(infection),
    wealth    = as.factor(wealth),
    edu       = as.factor(edu),
    gender    = as.factor(gender),
    
    wealth = relevel(wealth, ref = '5th quintile')
  )


#-----------------Unadjusted-----------More independent variables but separate models
OR1<-A2 %>%
  select(disease, infection, age, wealth, edu, ht, gender) %>%
  tbl_uvregression(
    method = glm,
    y=disease,
    method.args = list(family=binomial(link = 'logit')),
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 3),
    label = mylab
    
  ) %>%
  modify_column_merge(
    pattern = "{estimate} ({conf.low}, {conf.high})",
    rows = !is.na(estimate)
  ) %>%
  modify_header(estimate ~"**OR (95% CI)**") %>%
  bold_labels()


print(OR1)

#--------------Adjusted------------More independent variables in a single model



OR2 <-glm(disease~ infection + age + wealth + edu + ht + gender,
          data = A2,
          family = binomial(link='logit')) %>%
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    label = mylab
  ) %>%
  modify_column_merge(
    pattern = "{estimate} ({conf.low}, {conf.high})",
    rows = !is.na(estimate)
  ) %>%
  modify_header(estimate ~"**OR (95% CI)**") %>%
  bold_labels()




print(OR2)


#--Publication-ready tables in R--------



OR_Tab <-tbl_merge(
  tbls = list(OR1, OR2),
  tab_spanner = c('**Unadjusted**', '**Adjusted**')
)

print(OR_Tab)

#--Export--Findings--

Tab1 %>%
  as_gt()%>%
  gtsave(filename = "Des_Table.docx",
         path="P:/abc")


ggsave(
  'my_box.jpeg',
  plot = my_box,
  device = 'jpeg',
  dpi = 900,
  limitsize = FALSE,
  units = 'in',
  width = 25,
  height = 11.5,
  path = 'P:/abc'
)


ggsave(
  'my_bar.jpeg',
  plot = my_bar,
  device = 'jpeg',
  dpi = 900,
  limitsize = FALSE,
  units = 'in',
  width = 25,
  height = 11.5,
  path = 'P:/abc'
)

OR_Tab %>%
  as_gt()%>%
  gtsave(filename = "OR_Reg_Table.docx",
         path="P:/abc")

