data1 <- read.csv("data/data.csv") %>% as.data.frame() 

# Omit columns that contain any NA values
data1 <- na.omit(data1)
