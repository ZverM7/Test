# script name: 
# plumber_example.R

# Define API

#* @get /get_table
#* @param foodlink
#* @response sumco2

get_table <- function(foodlink){
  
  foodlink = foodlink
  
  #packages to load
library(readr)
library(rvest)
library(stringr)
library(tidyverse)
library(data.table)
library(DBI)
library(jsonlite)
library(stringdist)
  #open connection
  ip <- "35.228.124.55"
  db_name <- "postgres"
  user <- "team"
  pwd <- "pdsp22"
  
  #Connection Setup
  db <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = db_name,
                       host = ip,
                       port = 5432,
                       user = user,
                       password = pwd)
  
  #load tables
  emissions_data <- dbReadTable(db, "emissions_data")
  cooking_vocab_data <- dbReadTable(db, "cooking_vocab")
  cooking_units_data <- data.table(dbReadTable(db, "cooking_units"))
  plurals_en <- data.table(dbReadTable(db, "plurals"))
  recipes_data <- data.table(dbReadTable(db, "recipes_data"))
  synonyms_data <- data.table(dbReadTable(db, "synonyms_data"))
  
  #link
  link <- read_html(foodlink)
  
  #raw scrape - Quantity
  ing_quant <- link %>%
    html_nodes(".ingredient-quantity") %>%
    html_text() %>%
    trimws(which = "both") %>%
    data.frame() 
  
  #raw scrape - Ingredient
  ing_name <- link %>%
    html_nodes(".ingredient-text") %>%
    html_text() %>%
    trimws(which = "both") %>%
    str_squish() %>%
    data.table()
  
  
  #Clean the quantity
  ing_quant <- ing_quant %>%
    rename("Quantity" = ".")
  
  #Separate the measurements
  ing_name <- ing_name %>%
    rename("Ingredient" = ".")
  
  ing_name$Quantity <- ing_quant$Quantity
  
  x <- 1
  for (row in ing_name$Ingredient) {
    measur <- str_extract(row, "(\\w+)")
    if ((measur %in% cooking_units_data$unit) == TRUE) {
      ing_name$Measurement[x] <- measur
      ing_name$Ingredient[x] <- str_remove(ing_name$Ingredient[x], measur)
    } else {
      ing_name$Measurement[x] <- 1
    }
   x <- x + 1
  }
  
  ing_name$Ingredient <- ing_name$Ingredient %>%
    trimws(which = "both") %>%
    str_squish() 
  
  #add kg converter 
  cooking_units_data$KG <- c(39.37007874, 39.37007874, 1, 1, 1, 1,1,1,1000,1000,1000,1000,1,1,1,1,1,1,1,1,2,1000000,1000000, 1000000,1000000,1000000,35.27336861,35.27336861,35.27336861,2,2,1,1,1,1,1,1,4.2268,415.54,100,4.2268,4.2268,2500,10,10,10,10,10,33.8140227,2.11,1.0567,33.8140227,0.26,0.26,0.26,9.92063,26.45503,1,1,1000,1,2795.56,2795.56,2.113376419,2.113376419,2.113376419,0.01,1.1,0.001,67.6280454,67.6280454,67.6280454,67.6280454,67.6280454,202.8841362,202.8841362,202.8841362,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1000,1000,1000,1.1,2500,1,1,1,1,1, 1,117.65, 117.65, 1000, 1000, 1000, 1000, 1, 1, 1, 1, 2.20462,1,1,1,1,1,1,1,1,1,1,1, 20000,20000, 1,1, 1, 1, 1)
  
  #White spaces
  ing_name$Ingredient <- ing_name$Ingredient %>%
    trimws(which = "both") %>%
    str_squish() %>%
    tolower()
  
  #eliminate vocab
  x <- 1
  for (row in ing_name$Ingredient) {
    row <- gsub("\\s*\\([^\\)]+\\)", "", row)
    ing_name$Ingredient[x] <- row
    x <- x + 1
  }
  
  ing_name$Ingredient_parsed <- " "
  
  #find the ingredients from the file
  x <- 1
  for (row in ing_name$Ingredient){
    row <- as.list(row)
    test_ing <- amatch(row, emissions_data$ingredient, maxDist = 0.5, method = "cosine")
    print(test_ing)
    for (words in test_ing){
      if (is.na(words) == FALSE) {
      ing_name$Ingredient_parsed[x] <- emissions_data$ingredient[words]
      }
      x <- x + 1
    }}
  
  #add info
  x <- 1
  for (row in ing_name$Ingredient_parsed) {
    if (row %in% emissions_data$ingredient) {
      #add emission
      ing_name$CO2[x] <- emissions_data %>%
        filter(ingredient == row) %>%
        select(emissions)
      #add variety
      ing_name$Variety[x] <- emissions_data %>%
        filter(ingredient == row) %>%
        select(variety)
    } else {
      #if nothing is parsed
      ing_name$CO2[x] <- 0
      ing_name$Variety[x] <- "N/A"
    }
    x <- x + 1
  }
  
  #add conversion
  x <- 1
  for (row in ing_name$Measurement) {
    if (row %in% cooking_units_data$unit) {
      #add conversion rate
      ing_name$Conversion[x] <- cooking_units_data %>%
        filter(unit == row) %>%
        select(KG)
    } else {
      #if nothing is parsed
      ing_name$Conversion[x] <- 1
    }
    x <- x + 1
  }
  
  #calculation
  x <- 1
  for (quant in ing_name$Quantity) {
    if (str_detect(quant, "-") == TRUE) {
      quant <- eval(parse(text=quant))
    }
    CO2 <- (strtoi(ing_name$CO2[x], base=16) / ing_name$Conversion[x]) * ing_name$Quantity[x]
    ing_name$CO2_Score[x] <- CO2
    x <- x + 1
  }
  
  sumco2 <-sum(as.numeric(ing_name$CO2))
  
  dbDisconnect(db)
  
}


#empty

