library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(PerformanceAnalytics)
library(ggpubr)
library(tibble)
library(MVN)

#read data
data=read.csv('credit.csv',header=TRUE)
# cek data
head(data)

# mengambil variabel numerik
data <- data[,c(1,7,8)]

# cek apakah ada atau tidaknya NA
data %>% anyNA()

# Cek seberapa banyak jumlah NA pada masing-masing kolom
data %>% is.na() %>% colSums()

#cek korelasi
chart.Correlation(data)

#melakukan standarisasi data
data_standar <- scale(data)

jarak <- dist(data_standar)

# k-means dengan 3 cluster
set.seed(100)

k3 <- kmeans(x= data_standar , centers = 3)
#visualiasi
fviz_cluster(k3, data = data_standar)

k2 <- kmeans(data_standar, centers = 2)
fviz_cluster(k2, data = data_standar)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = data_standar) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = data_standar) + ggtitle("k = 3")


library(gridExtra)
grid.arrange(p1, p2, nrow = 2)

# Elbow and Silhouette
set.seed(100)
fviz_nbclust(data_standar, kmeans, method = "wss")
fviz_nbclust(data_standar, kmeans, method='silhouette')

set.seed(123)
final <- kmeans(data_standar, 2)
print(final)

fviz_cluster(final, data = data_standar)

df.cluster = data.frame(data, final$cluster)
df.cluster

summary <- df.cluster %>%
  group_by(final.cluster) %>%
  summarize_all(list(mean = mean, min = min, max = max))

summary

# Hierarcical Clustering
hc_complete = hclust(dist(data_standar), method = "complete")
hc_average = hclust(dist(data_standar), method = "average")
hc_single = hclust(dist(data_standar), method = "single")

ggdendrogram(hc_complete, rotate = FALSE, size = 2) + labs(title = "Complete Linkage") 
ggdendrogram(hc_average, rotate = FALSE, size = 2) + labs(title = "Average Linkage")
ggdendrogram(hc_single, rotate = FALSE, size = 2) + labs(title = "Single Linkage")

cut_point = cutree(hc_complete, k = 2) #Memilih sebanyak 2 klaster
data %>%
  mutate(Klaster = cut_point) %>%
  group_by(Klaster) %>%
  summarize_all(list(mean = mean, min = min, max = max))
