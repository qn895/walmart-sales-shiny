require(tidyr)
require(dplyr)

# PERFORM ETL FOR THE TRAIN.CSV
setwd("C:/Users/jenng/OneDrive/School/Class of 2017 (UT)/CS 329E - Data Visualization/Projects/dv_tproject5_quynh/01 Data")
file_name <- "train.csv"

train_df <- read.csv(file_name, stringsAsFactors = FALSE)
names(train_df)


for(n in names(train_df)) {
  train_df[n] <- data.frame(lapply(train_df[n], gsub, pattern="[^ -~]",replacement= ""))
}
str(train_df)

dimensions <- c("Store", "Dept", "Date", "IsHoliday")

measures <- setdiff(names(train_df), dimensions)

measures

df <- train_df

if( length(measures) > 1 || ! is.na(dimensions)) {
  for(d in dimensions) {
    # Get rid of " and ' in dimensions.
    train_df[d] <- data.frame(lapply(df[d], gsub, pattern="[\"']",replacement= ""))
    # Change & to and in dimensions.
    train_df[d] <- data.frame(lapply(df[d], gsub, pattern="&",replacement= " and "))
    # Change : to ; in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern=":",replacement= ";"))
  }
  }
  if( length(measures) > 1 || ! is.na(measures)) {
    for(m in measures) {
      df[m] <- data.frame(lapply(df[m], gsub, pattern="[^--.0-9]",replacement= ""))
    }
  }
  
  write.csv(df, paste(gsub(".csv", "", file_name), ".reformatted.csv", sep=""), row.names=FALSE, na = "")
  
  tableName <- gsub(" +", "_", gsub("[^A-z, 0-9, ]", "", gsub(".csv", "", file_name)))
  sql <- paste("CREATE TABLE", tableName, "(\n-- Change table_name to the table name you want.\n")
  if( length(measures) > 1 || ! is.na(dimensions)) {
    for(d in dimensions) {
      sql <- paste(sql, paste(d, "varchar2(4000),\n"))
    }
  }
  if( length(measures) > 1 || ! is.na(measures)) {
    for(m in measures) {
      if(m != tail(measures, n=1)) sql <- paste(sql, paste(m, "number(38,4),\n"))
      else sql <- paste(sql, paste(m, "number(38,4)\n"))
    }
  }
  sql <- paste(sql, ");")
  cat(sql)
  
# PERFORM ETL FOR THE STORES.CSV
  
  #setwd("C:/Users/jenng/OneDrive/School/Class of 2017 (UT)/CS 329E - Data Visualization/Projects/dv_tproject5_quynh/01 Data")
  file_name <- "features.csv"
  stores_df <- read.csv(file_name, stringsAsFactors = FALSE)
  
  names(stores_df)
  
  
  for(n in names(stores_df)) {
    stores_df[n] <- data.frame(lapply(stores_df[n], gsub, pattern="[^ -~]",replacement= ""))
  }
  str(stores_df)
  stores_dimensions <- c("Type", "Store")
  stores_measures <- setdiff(names(stores_df), stores_dimensions)
  dimensions <- stores_dimensions
  measures <- stores_measures
  
  df <- stores_df
  
  if( length(measures) > 1 || ! is.na(dimensions)) {
    for(d in dimensions) {
      # Get rid of " and ' in dimensions.
      train_df[d] <- data.frame(lapply(df[d], gsub, pattern="[\"']",replacement= ""))
      # Change & to and in dimensions.
      train_df[d] <- data.frame(lapply(df[d], gsub, pattern="&",replacement= " and "))
      # Change : to ; in dimensions.
      df[d] <- data.frame(lapply(df[d], gsub, pattern=":",replacement= ";"))
    }
  }
  if( length(measures) > 1 || ! is.na(measures)) {
    for(m in measures) {
      df[m] <- data.frame(lapply(df[m], gsub, pattern="[^--.0-9]",replacement= ""))
    }
  }
  
  write.csv(df, paste(gsub(".csv", "", file_name), ".reformatted.csv", sep=""), row.names=FALSE, na = "")
  
  tableName <- gsub(" +", "_", gsub("[^A-z, 0-9, ]", "", gsub(".csv", "", file_name)))
  sql <- paste("CREATE TABLE", tableName, "(\n-- Change table_name to the table name you want.\n")
  if( length(measures) > 1 || ! is.na(dimensions)) {
    for(d in dimensions) {
      sql <- paste(sql, paste(d, "varchar2(4000),\n"))
    }
  }
  if( length(measures) > 1 || ! is.na(measures)) {
    for(m in measures) {
      if(m != tail(measures, n=1)) sql <- paste(sql, paste(m, "number(38,4),\n"))
      else sql <- paste(sql, paste(m, "number(38,4)\n"))
    }
  }
  sql <- paste(sql, ");")
  cat(sql)
  

# PERFORM ETL FOR THE FEATURES.CSV
  
  #setwd("C:/Users/jenng/OneDrive/School/Class of 2017 (UT)/CS 329E - Data Visualization/Projects/dv_tproject5_quynh/01 Data")
  file_name <- "features.csv"
  features_df <- read.csv(file_name, stringsAsFactors = FALSE)
  
  names(features_df)
  
  
  for(n in names(features_df)) {
    features_df[n] <- data.frame(lapply(features_df[n], gsub, pattern="[^ -~]",replacement= ""))
  }
  str(features_df)
  features_dimensions <- c("Store", "Date","IsHoliday")
  features_measures <- setdiff(names(features_df), features_dimensions)
  dimensions <- features_dimensions
  measures <- features_measures
  measures
  df <- features_df
  
  if( length(measures) > 1 || ! is.na(dimensions)) {
    for(d in dimensions) {
      # Get rid of " and ' in dimensions.
      train_df[d] <- data.frame(lapply(df[d], gsub, pattern="[\"']",replacement= ""))
      # Change & to and in dimensions.
      train_df[d] <- data.frame(lapply(df[d], gsub, pattern="&",replacement= " and "))
      # Change : to ; in dimensions.
      df[d] <- data.frame(lapply(df[d], gsub, pattern=":",replacement= ";"))
    }
  }
  if( length(measures) > 1 || ! is.na(measures)) {
    for(m in measures) {
      df[m] <- data.frame(lapply(df[m], gsub, pattern="[^--.0-9]",replacement= ""))
    }
  }
  
  write.csv(df, paste(gsub(".csv", "", file_name), ".reformatted.csv", sep=""), row.names=FALSE, na = "")
  
  tableName <- gsub(" +", "_", gsub("[^A-z, 0-9, ]", "", gsub(".csv", "", file_name)))
  sql <- paste("CREATE TABLE", tableName, "(\n-- Change table_name to the table name you want.\n")
  if( length(measures) > 1 || ! is.na(dimensions)) {
    for(d in dimensions) {
      sql <- paste(sql, paste(d, "varchar2(4000),\n"))
    }
  }
  if( length(measures) > 1 || ! is.na(measures)) {
    for(m in measures) {
      if(m != tail(measures, n=1)) sql <- paste(sql, paste(m, "number(38,4),\n"))
      else sql <- paste(sql, paste(m, "number(38,4)\n"))
    }
  }
  sql <- paste(sql, ");")
  cat(sql)