#AKANSHA
#STUDENT ID: A124058

#Project->3

#install packages-->
install.packages(c("eurostat","countrycode"))
library(eurostat)
library(countrycode)

#downloading the dataset

data <- get_eurostat("prc_hicp_manr", time_format = "num", start_time = "2000M02", end_time = "2022M09", geo = "EU")

#match country codes with country names
data$geo <- countrycode(data$geo, origin = "iso2c", destination = "country.name")

#download dataset for specific country
data_country <- get_eurostat("prc_hicp_manr", time_format = "num", start_time = "2000M02", end_time = "2022M09", geo = "COUNTRY_CODE")

#match country code with country name
data_country$geo <- countrycode(data_country$geo, origin = "iso2c", destination = "country.name")
str(data_country)



library(ggplot2)
colnames(data)

# create a line plot of HICP over time for each country
ggplot(data, aes(x = time, y = values, group = geo, color = geo)) +
  geom_line() +
  scale_y_continuous(breaks = c(0, 10,20,30,40,50,60,70,80), limits = c(0,80)) +
  labs(title = "HICP for EU Countries", x = "Year", y = "HICP")



install.packages(c("dplyr","reshape2","factoextra"))
library(dplyr)
library(reshape2)
library(factoextra)

#create subsample of the data
subsample_data <- data_country %>% sample_n(20000)

#melt the subsample data
subsample_melted <- melt(subsample_data, id.vars = c("geo","time"), measure.vars = "values")
str(subsample_melted)


# reshape dataframe
subsample_melted_reshaped <- subset(subsample_melted, select = c("geo", "time", "value"))
colnames(subsample_melted_reshaped) <- c("geo", "time", "value")

# calculate distance matrix
dist_matrix <- dist(subsample_melted_reshaped[,c("time","value")], method = "minkowski", p = 1.5)

# Perform hierarchical clustering
linkage_matrix <- hclust(dist_matrix, method = "complete")

# Cut the tree into 4 clusters
cluster_assignments <- cutree(linkage_matrix, k = 4)

# Visualize the dendrogram
fviz_dend(linkage_matrix, k = 4, cex = 0.5, show_labels = F, color_labels_by_k = T)



