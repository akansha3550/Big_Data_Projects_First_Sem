#install packages

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("skimr")
install.packages("dplyr")
install.packages("prodlim")
install.packages("arsenal")
install.packages("car")
install.packages("janitor")
install.packages("tidyr")
install.packages("gplots")
install.packages("ggpubr")
install.packages("forcats")
install.packages("gridExtra")
install.packages("ggalt")
install.packages("corrgram")
install.packages("corrplot")
install.packages("plm")
install.packages("arsenal")
install.packages("mice")
install.packages("sf")
install.packages("Hmisc")
install.packages("patchwork")
install.packages("reshape2")
install.packages("viridis")





#load packages

library(tidyverse)
library(ggplot2)
library(lubridate)
library(skimr)
library(dplyr)
library(prodlim)
library(arsenal)
library(car)
library(janitor)
library(tidyr)
library(gplots)
library(ggpubr)
library(forcats)
library(gridExtra)
library(ggalt)
library(corrgram)
library(corrplot)
library(plm)
library(arsenal)
library(mice)
library(sf)
library(Hmisc)
library(patchwork)
library(reshape2)
library(viridis)
warning = FALSE


#Explore the data

str(report_2015) 
str(report_2016)
str(report_2017)
str(report_2018)
str(report_2019)


#Check column names
compare_df_cols(report_2015, report_2016, report_2017, report_2018, report_2019)


#Add column names and Remove Column names
data_2015 <-  add_column(report_2015, 'Year'=2015)
data_2015 <- select(data_2015, -'Standard Error', -'Rank', -'Dystopia')
view (data_2015)


data_2016 <- add_column(report_2016, 'Year'=2016)
data_2016 <- select(data_2016 ,-'LowerConfidenceInterval', 
                    -'UpperConfidenceInterval', -'Rank', 
                    -'Dystopia')
view (data_2016)


data_2017 <- add_column(report_2017, 'Year' = 2017, 'Region' = NA)  
data_2017 <-select(data_2017 ,-'Whisker.high', -'Whisker.low',-'Rank', -'Dystopia')
view (data_2017)

data_2018 <- add_column(report_2018, 'Year' = 2018, 'Region' = NA ) 
data_2018 <- select(data_2018 , -'Rank')
view (data_2018)

data_2019 <- add_column(report_2019, 'Year' = 2019, 'Region' = NA) 
data_2019 <- select(data_2019 , -'Rank')
view(data_2019)



#Reorder columns
order <- c("Country","Region","Year", "Score","Economy",
           "Family","Health", "Freedom","Government","Generosity")
data_2015 <-data_2015[, order]
data_2016 <-data_2016[, order]
data_2017 <-data_2017[, order]
data_2018 <-data_2018[, order]
data_2019 <-data_2019[, order]


#Merge the datasets
df<- rbind(data_2015, data_2016, data_2017, data_2018, data_2019) 


#Unify countries' names
df$Country[df$Country=="Macedonia"]<-"N.Macedonia"
df$Country[df$Country=="North Macedonia"]<-"N.Macedonia"
df$Country[df$Country=="Northern Cyprus"]<-"N.Cyprus"
df$Country[df$Country=="North Cyprus"]<-"N.Cyprus"
df$Country[df$Country=="Hong Kong S.A.R., China"]<-"Hong Kong"
df$Country[df$Country=="Taiwan Province of China"]<-"Taiwan"
df$Country[df$Country=="Trinidad & Tobago"]<-"Trinidad and Tobago"
df$Country[df$Country=="Somaliland region"]<-"Somaliland Region"


#Sort by 'Country' in alphabetical order
df <- arrange(df, Country)

#Check the data type
sapply(df, mode)


#Convert var. 'Government' values into numeric
df$Government<-as.numeric(df$Government)

#Check the result
is.numeric(df$Government)

sum(duplicated(df)) # No duplicates

#Detect missing values
md.pattern(df, rotate.names = TRUE)

#Locate the missing value in 'Government'
which(is.na(df$Government)) # row 736
df[736,] # United Arab Emirates


#Impute the missing value with the mean of var. 'Government' for UAE
df <- df %>%
  group_by(Country) %>% 
  mutate_at(vars(Government), ~replace_na(., mean(., na.rm = TRUE)))


#Fill the missing values in var. 'Region'
df <- df %>%
  group_by(Country) %>%
  fill(Region, .direction = c('updown'))

#Check the result
md.pattern(df, rotate.names = TRUE) # a missing value in 'Region'


#Locate the missing value
which(is.na(df$Region)) # row 236
df[236,] # Gambia (data available for 2019 only)


#Fill the missing value
df$Region <- df$Region %>% replace_na('Sub-Saharan Africa')
md.pattern(df, rotate.names = TRUE) 

head(df)

#Number of countries by region
df %>%
  group_by(Region) %>%
  summarise(num = n_distinct(Country)) %>%
  ggplot(aes(x= fct_reorder(Region, num, .desc = TRUE), 
             y = num, fill=Region)) +
  geom_bar(stat='identity', alpha = 0.85)+
  labs(title = 'Number of Countries by Region', 
       x = NULL, y = NULL) +
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 16), 
        axis.text.x = element_text(angle=70, hjust = 1),
        legend.position = 'none')+
  scale_fill_brewer(palette = 'BrBG')


#Region ranking by happiness score 

df %>%
  group_by(Region) %>%
  summarise(avg = mean(Score)) %>%
  ggplot(aes(x = fct_reorder(Region, avg), y = avg )) +
  geom_col(fill = 'rosybrown3', alpha = 0.9)+
  labs(title = 'Region Ranking by Happiness Score', 
       y = 'Happiness Score (avg)', x = NULL)+
  theme(plot.title = element_text(size = 16),
        text = element_text(size = 16))+
  coord_flip()


#Countries ranked by happiness score

americas <- df %>%
  filter(Region %in% c('North America', 'Latin America and Caribbean')) %>%
  group_by(Country) %>%
  summarise(avg = mean(Score)) %>%
  ggplot(aes(y = fct_reorder(Country, avg), x = avg)) +
  geom_col(fill="darkslategray4", alpha = 0.6) +
  labs(title= "Countries by Happiness Score",
       subtitle = 'The Americas',
       x ='Happiness Score (avg)', y = NULL) +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        text = element_text(size = 14))
americas



cee <- df %>%
  filter(Region == 'Central and Eastern Europe') %>%
  group_by(Country) %>%
  summarise(avg = mean(Score)) %>%
  ggplot(aes(y = fct_reorder(Country, avg), x = avg)) +
  geom_col(width=0.8, fill="tan2") +
  labs(title= "Countries by Happiness Score",
       subtitle = 'Central and Eastern Europe',
       x ='Happiness Score (avg)', y = NULL) +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        text = element_text(size = 14))
cee


mdl_east<- df %>%
  filter(Region == 'Middle East and Northern Africa') %>%
  group_by(Country) %>%
  summarise(avg = mean(Score)) %>%
  ggplot(aes(y = fct_reorder(Country, avg), x = avg)) +
  geom_col(width=0.8, fill="darkseagreen4", alpha = 0.7) +
  labs(title= "Countries by Happiness Score",
       subtitle = 'Middle East and Northern Africa',
       x ='Happiness Score (avg)', y = NULL) +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        text = element_text(size = 14))
mdl_east


south_asia <- df %>%
  filter(Region == 'Southern Asia') %>%
  group_by(Country) %>%
  summarise(avg = mean(Score)) %>%
  ggplot(aes(y = fct_reorder(Country, avg), x = avg)) +
  geom_col(width=0.9, fill="brown", alpha = 0.6) +
  labs(title= "Countries by Happiness Score",
       subtitle = 'Southern Asia',
       x ='Happiness Score (avg)', y = NULL) +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        text = element_text(size = 14))
south_asia



africa <- df %>%
  filter(Region == 'Sub-Saharan Africa') %>%
  group_by(Country) %>%
  summarise(avg = mean(Score)) %>%
  ggplot(aes(y = fct_reorder(Country, avg), x = avg)) +
  geom_col(width=0.6, fill="firebrick4", alpha = 0.5) +
  labs(title= "Countries by Happiness Score",
       subtitle = 'Sub-Saharan Africa',
       x ='Happiness Score (avg)', y = NULL) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 11),
        text = element_text(size = 7))
africa



#Countries by happiness score
se_asia <- df %>%
  filter(Region %in% c('Southeastern Asia', 'Australia and New Zealand')) %>%
  group_by(Country) %>%
  summarise(avg = mean(Score)) %>%
  ggplot(aes(y = fct_reorder(Country, avg), x = avg)) +
  geom_col(width=0.9, fill="lightblue4") +
  labs(title= "Countries by Happiness Score",
       subtitle = 'Southern Asia, Australia, New Zealand',
       x ='Happiness Score (avg)', y = NULL) +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        text = element_text(size = 14))
se_asia


#Countries by happiness score

we <- df %>%
  filter(Region == 'Western Europe') %>%
  group_by(Country) %>%
  summarise(avg = mean(Score)) %>%
  ggplot(aes(y = fct_reorder(Country, avg), x = avg)) +
  geom_col(width=0.9, fill="darkgoldenrod3", alpha = 0.8) +
  labs(title= "Countries by Happiness Score",
       subtitle = 'Western Europe', x ='Happiness Score (avg)', y = NULL) +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        text = element_text(size = 14))
we


#Change in average happiness score over time: 
trend <- df %>% 
  group_by(Year) %>%
  summarise(avg = mean(Score)) %>%
  ggplot(aes(df, x = Year, y = avg)) +
  geom_point(size = 0.8, color = 'steel blue')+
  geom_line (size = 0.4, color = 'steel blue') +
  labs(title = 'Global trend',
       x = NULL, y = 'Happiness Score (avg)')+
  theme(plot.title = element_text(size = 14),
        text = element_text(size = 12))

r_score <- df %>% 
  group_by(Region, Year) %>% 
  summarise(avg_score = mean(Score)) %>%
  ggplot(aes(x = Year, y = avg_score, color = Region)) +
  geom_line(size = 0.4) +
  geom_point(size = 0.6)+
  labs(title = 'Regional Trends',
       x = NULL, y = 'Happiness Score (avg)')+
  theme(plot.title = element_text(size = 14),
        text = element_text(size = 12))+
  scale_color_brewer(palette = 'RdBu')

trend + r_score + plot_layout(guides = "collect")+
  plot_annotation(title = 'Change in Happiness Score', 
                  theme = theme(plot.title = element_text(size = 16)))




#fequency distribution of happiness score by year

hist <- df %>%
  ggplot(aes(x=Score, group=Year, fill=factor(Year))) +
  geom_histogram(bins=10, position = 'identity',color =
                   'white', fill = 'lightpink3',alpha = 0.7) +
  labs(title = 'Frequency Distribution of Happiness Score', 
       x = 'Happiness Score', y = "Counts") +
  theme(plot.title = element_text(size = 16),
        text = element_text(size = 14))+
  facet_wrap(~ Year)+
  theme_minimal()

hist


#Spread and skewness of score values by region
dstr <- df %>%
  ggplot(aes(y = fct_reorder(Region, Score), x = Score, fill = Region))+
  geom_boxplot(outlier.size = .8, alpha = 0.8)+
  labs(title = 'Distributions of Happiness Score', y = NULL,
       x = 'Happiness Score')+
  theme(
    plot.title = element_text(size = 16), 
    text = element_text(size = 14),
    legend.position = 'none')+
  scale_fill_brewer(palette = 'BrBG')
dstr



#Change in factors over time

reg_e <- df %>% 
  group_by(Region, Year) %>% 
  summarise(avg_ec = mean(Economy)) %>%
  ggplot(aes(x = Year, y = avg_ec, color = Region)) +
  geom_line(size = 0.2) +
  labs(x = NULL, y = "GDP per capita", color = 'Region')+ 
  theme(text = element_text(size = 8))+
  scale_color_brewer(palette = 'PRGn')

reg_h <- df %>% 
  group_by(Region, Year) %>% 
  summarise(avg_h = mean(Health)) %>%
  ggplot(aes(x = Year, y = avg_h, color = Region)) +
  geom_line(size = 0.2) +
  labs(x = NULL, y = "Health", color = 'Region') +
  theme(text = element_text(size = 8))+
  scale_color_brewer(palette = 'RdBu')

reg_fm <- df %>% 
  group_by(Region, Year) %>% 
  summarise(avg_fm = mean(Family)) %>%
  ggplot(aes(x = Year, y = avg_fm, color = Region)) +
  geom_line(size = 0.2) +
  labs(x = NULL, y = "Family", color = 'Region') +
  theme(text = element_text(size = 8))+
  scale_color_brewer(palette = 'RdBu')

reg_fr <- df %>% 
  group_by(Region, Year) %>% 
  summarise(avg_fr = mean(Freedom)) %>%
  ggplot(aes(x = Year, y = avg_fr, color = Region)) +
  geom_line(size = 0.2) +
  labs(x = NULL, y = "Freedom", color = 'Region') +
  theme(text = element_text(size = 8))+
  scale_color_brewer(palette = 'RdBu')

reg_gv <- df %>% 
  group_by(Region, Year) %>% 
  summarise(avg_cr = mean(Government)) %>%
  ggplot(aes(x = Year, y = avg_cr, color = Region)) +
  geom_line(size = 0.2) +
  labs(x = NULL, y = "Government", color = 'Region') +
  theme(text = element_text(size = 8))+
  scale_color_brewer(palette = 'RdBu')

reg_gn <- df %>% 
  group_by(Region, Year) %>% 
  summarise(avg_cr = mean(Generosity)) %>%
  ggplot(aes(x = Year, y = avg_cr, color = Region)) +
  geom_line(size = 0.2) +
  labs(x = NULL, y = "Generosity", color = 'Region') +
  theme(text = element_text(size = 8))+
  scale_color_brewer(palette = 'RdBu')


trends <- reg_e+reg_h+reg_fr+reg_fm+reg_gv+reg_gn+
  plot_layout(guides = "collect", ncol = 3)+
  plot_annotation(title = 'Factor Trends', 
                  theme = theme(plot.title = element_text(size = 16)))
trends




#Factor spread and skewness by region

dstr_ec <- df %>%
  ggplot(aes(y = fct_reorder(Region, Economy), x = Economy, fill = Region))+
  geom_boxplot(outlier.size = 0.7, alpha = 0.8)+
  labs(title = 'Distributions of GDP per capita', y = NULL, 
       x = 'GDP per capita')+
  theme(plot.title = element_text(size = 16),
        text = element_text(size = 14), 
        legend.position = 'none')+
  scale_fill_brewer(palette = 'Blues')
dstr_ec


#Distribution of freedom

dstr_fr <- df %>%
  ggplot(aes(y = fct_reorder(Region, Freedom), 
             x = Freedom, fill = Region))+
  geom_boxplot(outlier.size = 0.7, alpha = 0.8)+
  labs(title = 'Distributions of Freedom', y = NULL, 
       x = 'Freedom')+
  theme(plot.title = element_text(size = 16),
        text = element_text(size = 14), 
        legend.position = 'none')+
  scale_fill_brewer(palette = 'OrRd')
dstr_fr




dstr_fam <- df %>%
  ggplot(aes(y = fct_reorder(Region, Family), 
             x = Family, fill = Region))+
  geom_boxplot(outlier.size = 0.7, alpha = 0.8)+
  labs(title = 'Ditributions of Family and Social Support', 
       y = NULL, x = 'Family and Social Support')+
  theme(plot.title = element_text(size = 16),
        text = element_text(size = 14), 
        legend.position = 'none')+
  scale_fill_brewer(palette = 'PuBu')
dstr_fam




dstr_hl <- df %>%
  ggplot(aes(y = fct_reorder(Region, Health), x = Health, fill = Region))+
  geom_boxplot(outlier.size = 0.7, alpha = 0.8)+
  labs(title = 'Distributions of Health', 
       y = NULL, x = 'Health')+
  theme(plot.title = element_text(size = 16),
        text = element_text(size = 14), 
        legend.position = 'none')+
  scale_fill_brewer(palette = 'Purples')
dstr_hl




dstr_gov <- df %>%
  ggplot(aes(y = fct_reorder(Region, Government), 
             x = Government, fill = Region))+
  geom_boxplot(outlier.size = 0.7, alpha = 0.8)+
  labs(title = 'Distributions of Trust in Government', 
       y = NULL, x = 'Government')+
  theme(plot.title = element_text(size = 16),
        text = element_text(size = 14), 
        legend.position = 'none')+
  scale_fill_brewer(palette = 'Dark2')
dstr_gov


dstr_gen <- df %>%
  ggplot(aes(y = fct_reorder(Region, Generosity), 
             x = Generosity, fill = Region))+
  geom_boxplot(outlier.size = 0.7, alpha = 0.8)+
  labs(title = 'Distributions of Generosity', y = NULL, 
       x = 'Generosity')+
  theme(plot.title = element_text(size = 16),
        text = element_text(size = 14), 
        legend.position = 'none')+
  scale_fill_brewer(palette = 'Paired')
dstr_gen

#Correlation between factors and happiness score

# Economy vs Happiness, global trend
econ <- df %>% 
  ggplot(aes(x = Economy, y = Score)) +
  geom_jitter(size = 0.5, color = 'navy blue') +
  geom_smooth(method = lm, size = 0.8, color = 'dark red') +
  stat_cor(method = "pearson", size = 6, label.y = 8)+
  labs(title = 'Economy vs Happiness',  x = 'GDP per capita')+
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 14))
econ



# Economy vs Happiness, regional trends
econ_reg <- df %>% 
  ggplot(aes(x = Economy, y = Score)) +
  geom_jitter(size = 0.5, color = 'steelblue') +
  geom_smooth(method = lm, size = 0.3, color = 'dark red')+
  stat_cor(method = "pearson", size = 3.5,
           label.y = 8)+
  labs(title = 'Economy vs Happiness', x = 'GDP per capita')+
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 16),
        legend.position="none" )+
  facet_wrap(~ Region, ncol = 4)
econ_reg



# Trust in Government/Corruption, global trend
gov <- df %>% 
  ggplot(aes(x = Government, y = Score)) +
  geom_jitter(size = 0.5, color = 'navy blue') +
  geom_smooth(size = 0.8, color = 'dark red') +
  stat_cor(method = "pearson", size = 6, label.x = 0.3, 
           label.y=2.9)+ 
  labs(title = 'Trust in Government vs Happiness', 
       x = 'Government')+
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 14))
gov



# Trust in Government/Corruption, regional trends
gov_reg <- df %>% 
  ggplot(aes(x = Government, y = Score)) +
  geom_jitter(size = 0.5, color='steelblue') +
  geom_smooth(size = 0.3, color = 'dark red')+
  stat_cor(method = "pearson", size = 3.5,
           label.x = 0.28, label.y=2.8)+
  labs(title = 'Trust in Government vs Happiness', x = 'Government')+
  theme(plot.title = element_text(size = 16),
        text = element_text(size = 14),
        legend.position="none" )+
  facet_wrap(~ Region, ncol = 4)
gov_reg



# Health and Life Expectancy, global trend
hl <- df %>% 
  ggplot(aes(x = Health, y = Score)) +
  geom_jitter(size = 0.5, color = 'navy blue') +
  geom_smooth(method = lm, size = 0.8, color = 'dark red') +
  stat_cor(method = "pearson", size = 6, label.y = 8)+ 
  labs(title = 'Health vs Happiness', x = 'Health')+
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 14))
hl




# Health and Life Expectancy, regional trends
hl_reg <- df %>% 
  ggplot(aes(x = Health, y = Score)) +
  geom_jitter(size = 0.3, color = 'steelblue') +
  geom_smooth(method = lm, size = 0.3, color = 'dark red')+
  stat_cor(method = "pearson", size = 3.5)+
  labs(title = 'Health vs Happiness', x = 'Health')+
  theme(plot.title = element_text(size = 16),
        text = element_text(size = 14),
        legend.position="none" )+
  facet_wrap(~ Region, ncol = 4)
hl_reg


# Family and Social Support, global trend
fam <- df %>% 
  ggplot(aes(x = Family, y = Score)) +
  geom_jitter(size = 0.5, color = 'navy blue') +
  geom_smooth(size = 0.8, color = 'dark red') +
  stat_cor(method = "pearson", size = 6, label.y = 8)+
  labs(title = 'Family and Social Support vs Happiness', 
       x = 'Family')+
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 14))
fam



# Family and Social Support, regional trends
fam_reg <- df %>% 
  ggplot(aes(x = Family, y = Score)) +
  geom_jitter(size = 0.3, color = 'steelblue') +
  geom_smooth(size = 0.3, color = 'dark red')+
  stat_cor(method = "pearson",  size = 3.5)+
  labs(title = 'Family and Social Support vs Happiness', x = 'Family')+
  theme(plot.title = element_text(size = 16),
        text = element_text(size = 14),
        legend.position="none" )+
  facet_wrap(~ Region, ncol = 4)
fam_reg



# Freedom, global trend
fr <- df %>% 
  ggplot(aes(x = Freedom, y = Score)) +
  geom_jitter(size = 0.3, color = 'navy blue') +
  geom_smooth(size = 0.8, color = 'dark red') +
  stat_cor(method = "pearson", size = 6)+ 
  labs(title = 'Freedom vs Happiness', x = 'Freedom',
       y = 'Happiness Score')+
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 14))
fr




# Freedom, regional trends
fr_reg <- df %>% 
  ggplot(aes(x = Freedom, y = Score)) +
  geom_jitter(size = 0.3, color = 'steelblue') +
  geom_smooth(size = 0.3, color = 'dark red')+
  stat_cor(method = "pearson", size = 3.5)+
  labs(title = 'Freedom vs Happiness',x = 'Freedom',
       y = 'Happiness Score')+
  theme(plot.title = element_text(size = 16),
        text = element_text(size = 14),
        legend.position="none" )+
  facet_wrap(~ Region, ncol = 4)
fr_reg




# Generosity, general trend
gen <- df %>% 
  ggplot(aes(x = Generosity, y = Score)) +
  geom_jitter(size = 0.5, color = 'navy blue') +
  geom_smooth(size = 0.8, color = 'dark red') +
  stat_cor(method = "pearson", size = 6,
           label.x = 0.6, label.y = 2.5)+ 
  labs(title = 'Generosity vs Happiness', x = 'Generosity',
       y = 'Happiness Score')+
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 14))
gen



gen_reg <- df %>% 
  ggplot(aes(x = Generosity, y = Score)) +
  geom_jitter(size = 0.3, color = 'steelblue') +
  geom_smooth(size = 0.3, color = 'dark red')+
  stat_cor(method = "pearson", size = 3.5,
           label.x = 0.5, label.y = 3)+
  labs(title = 'Generosity vs Happiness', x = 'Generosity',
       y = 'Happiness Score')+
  theme(plot.title = element_text(size = 16),
        text = element_text(size = 14),
        legend.position="none" )+
  facet_wrap(~ Region, ncol = 4)
gen_reg



#Correlation between factors

# Compute the correlation coefficients
corr_data <- df[, c('Economy', 'Health', 'Freedom', 'Generosity','Government', 'Family')]
corr1 <- cor(corr_data)
round(corr1, 2)





# *Visualize the correlation matrix*
corrplot(corr1, type = "full", order = "hclust", 
         method = 'pie',
         addCoef.col = 1.2,
         number.cex = 1.4,
         tl.col = "navy", tl.srt = 90,
         tl.cex = 1.3)



#Some differences between regions

# Economy and Generosity
econ_gen <- df %>% 
  ggplot(aes(x = Economy, y = Generosity)) +
  geom_jitter(size = 0.3, color = 'steelblue') +
  geom_smooth(size = 0.3, color = 'dark red') +
  stat_cor(method = "pearson", size = 3.5)+
  labs(title = 'Economy vs Generosity', x = 'GDP per capita')+
  theme(plot.title = element_text(size = 16),
        text = element_text(size = 12),
        legend.position="none" )+
  facet_wrap(~ Region, ncol = 4)
econ_gen




# Family vs Generosity
fam_gen <- df %>% 
  ggplot(aes(x = Family, y = Generosity)) +
  geom_jitter(size = 0.3, color = 'steelblue') +
  geom_smooth(size = 0.3, color = 'dark red') +
  stat_cor(method = "pearson", size = 3.5)+
  labs(title = 'Family and Social Support vs Generosity', 
       x = 'Family')+
  theme(plot.title = element_text(size = 16),
        text = element_text(size = 12),
        legend.position="none" )+
  facet_wrap(~ Region, ncol = 4)
fam_gen



# Economy and Government
ec_gov <- df %>% 
  ggplot(aes(x = Government, y = Economy)) +
  geom_jitter(size = 0.3, color = 'steelblue') +
  geom_smooth(size = 0.3, color = 'dark red') +
  stat_cor(method = "pearson", size = 3.5)+
  labs(title = 'Trust in Government vs Economy',
       x = 'Government', 
       y = 'GDP per capita')+
  theme(plot.title = element_text(size = 16),
        text = element_text(size = 12),
        legend.position="none" )+
  facet_wrap(~ Region, ncol = 4)
ec_gov



# Freedom and Government
fr_gov <- df %>% 
  ggplot(aes(x = Government, y = Freedom)) +
  geom_jitter(size = 0.3, color = 'steelblue') +
  geom_smooth(size = 0.3, color = 'dark red') +
  stat_cor(method = "pearson", size = 3.5,
           label.x = 0.25, label.y = 0)+
  labs(title = 'Trust in Government vs Freedom',
       x = 'Government', 
       y = 'Freedom')+
  theme(plot.title = element_text(size = 16),
        text = element_text(size = 12),
        legend.position="none" )+
  facet_wrap(~ Region, ncol = 4)
fr_gov
`geom_smo




#The happiness of European nations

# Top countries
top_we <- df %>%
  filter(Country %in% c('Denmark', 'Sweden', 'Norway', 
                        'Finland', 'Switzerland')) %>%
  select(Generosity, Government,Freedom,
         Family, Health,Economy) %>%
  group_by(Country) %>%
  summarise_each(funs(mean)) %>%
  melt(id = 'Country') %>%
  ggplot(aes(x = Country, 
             y = value, fill = variable)) +
  geom_col(position = 'fill', alpha = 0.85) +
  scale_y_continuous(labels = scales::percent)+
  labs(title='Western Europe', x = NULL, y = NULL) +
  theme(plot.title = element_text(size = 16),
        axis.text.x = element_text(angle = 70, hjust = 1), 
        text = element_text(size = 14))+
  scale_fill_brewer(palette = 'BrBG')

# Central and Eastern Europe
top_cee <- df %>%
  filter(Country %in% c('Czech Republic', 'Slovakia', 'Poland', 
                        'Lithuania', 'Uzbekistan')) %>%
  select(Generosity, Government,Freedom,
         Family, Health,Economy) %>%
  group_by(Country) %>%
  summarise_each(funs(mean)) %>%
  melt(id = 'Country') %>%
  ggplot(aes(x = Country, 
             y = value, fill = variable)) +
  geom_col(position = 'fill', alpha = 0.85) +
  scale_y_continuous(labels = scales::percent)+
  labs(title='Eastern Europe', x = NULL, y = NULL) +
  theme(plot.title = element_text(size = 16),
        axis.text.x = element_text(angle = 70, hjust = 1), 
        text = element_text(size = 14))+
  scale_fill_brewer(palette = 'BrBG')

top_eur <- top_we + top_cee +
  plot_layout(guides = "collect") +
  plot_annotation(title = 'What makes the happiest European nations happy',
                  theme = theme(plot.title = element_text(size = 20)))

top_eur


#The least happy European nations

# Bottom countries

# Western Europe
bottom_we <- df %>%
  filter(Country %in% c('Greece', 'Cyprus', 'Portugal', 
                        'N.Cyprus', 'Italy')) %>%
  select(Generosity, Government,Freedom,
         Family, Health,Economy) %>%
  group_by(Country) %>%
  summarise_each(funs(mean)) %>%
  melt(id = 'Country') %>%
  ggplot(aes(x = Country, 
             y = value, fill = variable)) +
  geom_col(position = 'fill', alpha = 0.85) +
  scale_y_continuous(labels = scales::percent)+
  labs(title='Western Europe', x = NULL, y = NULL) +
  theme(plot.title = element_text(size = 16),
        axis.text.x = element_text(angle = 70, hjust = 1), 
        text = element_text(size = 14))+
  scale_fill_brewer(palette = 'PRGn')

# Central and Eastern Europe
bottom_cee <- df %>%
  filter(Country %in% c('Armenia', 'Georgia', 'Ukraine', 
                        'Albania', 'Bulgaria')) %>%
  select(Generosity, Government,Freedom,
         Family, Health,Economy) %>%
  group_by(Country) %>%
  summarise_each(funs(mean)) %>%
  melt(id = 'Country') %>%
  ggplot(aes(x = Country, 
             y = value, fill = variable)) +
  geom_col(position = 'fill', alpha = 0.85) +
  scale_y_continuous(labels = scales::percent)+
  labs(title='Eastern Europe', x = NULL, y = NULL) +
  theme(plot.title = element_text(size = 16),
        axis.text.x = element_text(angle = 70, hjust = 1), 
        text = element_text(size = 14))+
  scale_fill_brewer(palette = 'PRGn')

europe_bottom <- bottom_we + bottom_cee +
  plot_layout(guides = "collect") +
  plot_annotation(title = 'What makes the least happy European nations happy',
                  theme = theme(plot.title = element_text(size = 16)))

europe_bottom




#Relationship the variables have with each other
v_color <- viridis::viridis(
  n = nrow(df)
)

df$color <- v_color[Matrix::invPerm(
  p = order(
    x = df$Score
  )
)]


pairs(
  formula = Score ~ Economy + Family +
    Health + Freedom +
    Generosity + Government,
  data = df,
  col = df$color,
  pch = 19
)




#data open friendly countries happy countries
open_data_happiness <- df[c("Region", "Score","Generosity")]

ggplot(open_data_happiness, 
       aes(x = Score, 
           y = Generosity)) +
  geom_point(aes(colour = Region),
             size = 2) +
  geom_smooth(method="lm") +
  labs(x = "Score",
       y = "Generosity",
       title = "Are open data friendly countries happy countries?",
       subtitle = "Data openness and happiness by country in all years") +
  scale_color_viridis(discrete = TRUE) +
  theme_minimal() +
  theme(text = element_text(size=16))





