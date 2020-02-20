# server.R
# PROJECT 4
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)

shinyServer(function(input, output) {
  
  
  # PROCESSING INPUT VARIABLES

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
  top_n_stores <- as.vector(head(avg_sales_df,n)$STORE_ID)
  all_stores <- unique(avg_sales_df$STORE_ID)
  not_top_n <- setdiff(all_stores, top_n_stores)
  
  output$crosstabPlot2 <- renderPlot({
    
    if (input$inSet == 'all') {
      d4 <- df
    }
    else if (input$inSet == 'top') {
      d4 <- df %>% filter(STORE_ID %in% top_n_stores)
    }
    else {
      d4 <- df %>% filter(STORE_ID %in% not_top_n)
    }
  
    ggplot(d4, aes(TYPE_OF_STORE, STORE_ID)) +
      labs(title='Weekly Average Sales based on Store ID and Holiday For Store')+
      geom_tile(aes(fill = WEEKLY_SALES), colour = "grey50") +
      geom_text(size = 3, aes(fill = floor(WEEKLY_SALES), label = floor(WEEKLY_SALES)))
  })
  
  output$tableCalc <- renderPlot({

    df$YEAR <- substr(df$DATE_OF_WEEK, 0, 4)

    if (input$year == '2010') {
      df <- df %>% filter(YEAR == '2010')
    }
    else if (input$year == '2011') {
      df <- df %>% filter(YEAR == '2011')
    }
    else {
      df <- df %>% filter(YEAR == '2012')
    }
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
    
  })
  
  
})