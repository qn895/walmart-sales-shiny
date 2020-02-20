require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(gridExtra)

df <- readRDS("../01 Data/walmart_data.rds")
df$YEAR <- substr(df$DATE_OF_WEEK, 0, 4)

df$SALES_PER_CPI = as.numeric(df$WEEKLY_SALES) + as.numeric(df$CPI)
names(df)
summary(df)

avg_sales_df <- df %>% group_by(STORE_ID, TYPE_OF_STORE) %>% summarise(SUM_OF_SALES=sum(WEEKLY_SALES)) %>% arrange(desc(SUM_OF_SALES))

store_A <- (avg_sales_df %>% filter(TYPE_OF_STORE == "A"))
store_B <- (avg_sales_df %>% filter(TYPE_OF_STORE == "B"))

avg_sales_A <- mean(store_A$SUM_OF_SALES)
avg_sales_B <- mean(store_B$SUM_OF_SALES)

avg_sales_df$AVG_SALE_OF_TYPE <- ifelse(avg_sales_df$TYPE_OF_STORE == "A",avg_sales_A, avg_sales_B)
avg_sales_df$DIFFERENCE <- avg_sales_df$SUM_OF_SALES-avg_sales_df$AVG_SALE_OF_TYPE

ggplot(avg_sales_df, aes(x = STORE_ID, y=SUM_OF_SALES,fill=DIFFERENCE)) +
  labs(title='Type of Store Sales Averages')+ xlab("Store") + ylab("Sum of Sales") +
  geom_bar(stat = "identity") + coord_flip() + facet_wrap(~TYPE_OF_STORE) + scale_fill_gradient2(low=("red3"),high=("green4"))  + geom_text(aes(label=round(DIFFERENCE,0)), hjust=+.65)

