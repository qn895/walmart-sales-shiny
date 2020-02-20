require("RCurl")
require("jsonlite")
require("dplyr")

# Read in data from DB

walmart_features <- data.frame(fromJSON(getURL(URLencode('oraclerest.cs.utexas.edu:5001/rest/native/?query="select * from WALMART_FEATURES"'),httpheader=c(DB='jdbc:oracle:thin:@aevum.cs.utexas.edu:1521/f16pdb', USER='cs329e_qmn76', PASS='orcl_qmn76', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))

walmart_stores <- data.frame(fromJSON(getURL(URLencode('oraclerest.cs.utexas.edu:5001/rest/native/?query="select * from WALMART_STORES"'),httpheader=c(DB='jdbc:oracle:thin:@aevum.cs.utexas.edu:1521/f16pdb', USER='cs329e_qmn76', PASS='orcl_qmn76', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))

# Here the data is retrieved in batches because else it's takes to long for the REST server to respond

walmart_train1 <- data.frame(fromJSON(getURL(URLencode('oraclerest.cs.utexas.edu:5001/rest/native/?query="select * from WALMART_TRAIN where store_id in (1,2,3,4,5,6)"'),httpheader=c(DB='jdbc:oracle:thin:@aevum.cs.utexas.edu:1521/f16pdb', USER='cs329e_qmn76', PASS='orcl_qmn76', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
walmart_train2 <- data.frame(fromJSON(getURL(URLencode('oraclerest.cs.utexas.edu:5001/rest/native/?query="select * from WALMART_TRAIN where store_id in (7,8,9,10,11,12)"'),httpheader=c(DB='jdbc:oracle:thin:@aevum.cs.utexas.edu:1521/f16pdb', USER='cs329e_qmn76', PASS='orcl_qmn76', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
#walmart_train3 <- data.frame(fromJSON(getURL(URLencode('oraclerest.cs.utexas.edu:5001/rest/native/?query="select * from WALMART_TRAIN where store_id in (13,14,15,16,17,18)"'),httpheader=c(DB='jdbc:oracle:thin:@aevum.cs.utexas.edu:1521/f16pdb', USER='cs329e_qmn76', PASS='orcl_qmn76', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
#walmart_train4 <- data.frame(fromJSON(getURL(URLencode('oraclerest.cs.utexas.edu:5001/rest/native/?query="select * from WALMART_TRAIN where store_id in (19,20,21,22,23,24)"'),httpheader=c(DB='jdbc:oracle:thin:@aevum.cs.utexas.edu:1521/f16pdb', USER='cs329e_qmn76', PASS='orcl_qmn76', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
#walmart_train5 <- data.frame(fromJSON(getURL(URLencode('oraclerest.cs.utexas.edu:5001/rest/native/?query="select * from WALMART_TRAIN where store_id in (25,26,27,28,29,30)"'),httpheader=c(DB='jdbc:oracle:thin:@aevum.cs.utexas.edu:1521/f16pdb', USER='cs329e_qmn76', PASS='orcl_qmn76', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))

# Combine data
walmart_train = rbind(walmart_train1, walmart_train2)

#walmart_train = rbind(walmart_train1, walmart_train2, walmart_train3, walmart_train4, walmart_train5)

# Reformat the date format and get ready to join
walmart_features$DATE_OF_WEEK <- as.Date(walmart_features$DATE_OF_WEEK, "%m/%d/%Y")
walmart_train$DATE_OF_WEEK <- as.Date(walmart_train$DATE_OF_WEEK, "%Y-%m-%d")

# Join
data = merge(merge(walmart_train, walmart_features, by=c("STORE_ID","DATE_OF_WEEK"), all=FALSE),walmart_stores, by="STORE_ID")

data$ISHOLIDAY = data$ISHOLIDAY.x
data$ISHOLIDAY.x = NULL
data$ISHOLIDAY.y = NULL
df <- data 

n <- 5

avg_sales_df <- df %>% group_by(STORE_ID) %>% summarise(AVG_SALES=mean(WEEKLY_SALES)) %>% arrange(desc(AVG_SALES))

avg_sales_df
#Display column names
names(data)

# Display a subset and summary of the data frame 
summary(data)
head(data)

subset <- subset(data, STORE_ID == 5)
head(subset)

subset2 <- subset(data, ISHOLIDAY = "TRUE")
head(subset2)
