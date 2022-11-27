################################################################################################################################################
################################################################################################################################################
################################################################ Link Score ####################################################################
################################################################################################################################################
################################################################################################################################################


# script name: Score_function
# plumber_example.R

# Define API

#* @get /get_score
#* @param foodlink
#* @response print result

get_table <- function(foodlink){
  
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
  recipes_score <- data.table(dbReadTable(db, "recipes_scores"))
  
  #link
  link <- read_html(foodlink)
  
  title <- link %>%
    html_nodes(".title") %>%
    html_text() %>%
    trimws(which = "both")
  
  title <- title[1]
  
  if (title %in% recipes_score$Title){
    Score <- recipes_score %>%
      filter(Title == title) %>%
      select(Score)
  
  } else {
  
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
    
    #Cleaning
    ing_quant <- ing_quant %>%
      rename("Quantity" = ".")
    
    ing_name <- ing_name %>%
      rename("Ingredient" = ".")
    
    ing_name$Quantity <- ing_quant$Quantity
    
    #Measurements
    x <- 1
    for (row in ing_name$Ingredient) {
      measur <- str_extract(row, "(\\w+)")
      if ((measur %in% cooking_units_data$unit) == TRUE) {
        ing_name$Measurement[x] <- measur
        ing_name$Ingredient[x] <- str_remove(ing_name$Ingredient[x], measur)
      } else {
        ing_name$Measurement[x] <- 0.1
      }
      x <- x + 1
    }
    
    #White spaces
    ing_name$Ingredient <- ing_name$Ingredient %>%
      trimws(which = "both") %>%
      str_squish() %>%
      tolower()
    
    #add kg converter 
    cooking_units_data$KG <- c(39.37007874, 39.37007874, 1, 1, 1, 1,1,1,1000,1000,1000,1000,1,1,1,1,1,1,1,1,2,1000000,1000000, 1000000,1000000,1000000,35.27336861,35.27336861,35.27336861,2,2,1,1,1,1,1,1,4.2268,415.54,100,4.2268,4.2268,2500,10,10,10,10,10,33.8140227,2.11,1.0567,33.8140227,0.26,0.26,0.26,9.92063,26.45503,1,1,1000,1,2795.56,2795.56,2.113376419,2.113376419,2.113376419,0.01,1.1,0.001,67.6280454,67.6280454,67.6280454,67.6280454,67.6280454,202.8841362,202.8841362,202.8841362,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1000,1000,1000,1.1,2500,1,1,1,1,1, 1,117.65, 117.65, 1000, 1000, 1000, 1000, 1, 1, 1, 1, 2.20462,1,1,1,1,1,1,1,1,1,1,1, 20000,20000, 1,1, 1, 1, 1)
    
    #cleaning
    x <- 1
    for (row in ing_name$Ingredient){
      row <- gsub("\\s*\\([^\\)]+\\)", "", row)
      row <- str_replace(row, "-", " ")
      row <- str_replace(row, ",", "")
      row <- str_squish(row)
      row <- gsub(" or .*","", row)
      row <- str_split(row, " ")
      ing_name$Ingredient[x] <- row
      x <- x + 1
    }
    
    #eliminate vocab
    x <- 1
    for (row in ing_name$Ingredient) {
      cleaned_str <- row
      for (word in row){
        if (word %in% cooking_vocab_data$name){
          cleaned_str <- cleaned_str[!cleaned_str == word]
        }
      }
      
      cleaned_str <- paste(cleaned_str, collapse=" ")
      ing_name$Ingredient[x] <- cleaned_str
      x <- x + 1
    }
    
    #White spaces
    ing_name$Ingredient <- ing_name$Ingredient %>%
      trimws(which = "both") %>%
      str_squish() %>%
      tolower()
    
    
    
    x <- 1
    for (row in ing_name$Ingredient){
      
      for (word in plurals_en$source) {
        if (str_detect(row, word) == TRUE) {
          replacement <- plurals_en %>%
            filter(source == word) %>%
            select(target)
          
          replacement <- as.character(replacement)
          row <- str_replace(row, word, replacement)
        } else {
          row <- row
        }
      }
      ing_name$Ingredient[x] <- row
      x <- x + 1
    }
    
    x <- 1
    for (row in ing_name$Ingredient){
      
      for (word in synonyms_data$source) {
        if (str_detect(row, word) == TRUE) {
          replacement <- synonyms_data %>%
            filter(source == word) %>%
            select(target)
          
          replacement <- as.character(replacement)
          row <- str_replace(row, word, replacement)
        } else {
          row <- row
        }
      }
      ing_name$Ingredient[x] <- row
      x <- x + 1
    }
    
    
    ing_name$Ingredient_parsed <- " "
    
    #find the ingredients from the file
    x <- 1
    for (row in ing_name$Ingredient){
      row <- as.list(row)
      test_ing <- amatch(row, emissions_data$ingredient, maxDist = 0.1, method = "cosine")
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
          summarize(mean(emissions)) 
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
    
    #cleaning quantities
    suppressWarnings({
      x <- 1
      for (quant in ing_name$Quantity){
        
        if (quant == ""){
          ing_name$Quantity[x] <- 1
        }
        
        if (is.na(as.numeric(quant)) == TRUE){
          ing_name$Quantity[x] <- str_split(quant," ") 
        } else {
          ing_name$Quantity[x] <- quant
        }
        
        if (is.na(as.numeric(quant)) == FALSE) {
          ing_name$Quantity[x] <- as.numeric(quant)
        } else {
          ing_name$Quantity[x] <- quant
        }
        
        if (str_detect(quant, "1⁄4") == TRUE){
          ing_name$Quantity[x] <- 0.25
        }
        
        if (str_detect(quant, "1⁄2") == TRUE){
          ing_name$Quantity[x] <- 0.5
        }
        
        if (str_detect(quant, "2⁄3") == TRUE){
          ing_name$Quantity[x] <- 0.66
        }
        
        if (str_detect(quant, "3⁄4") == TRUE){
          ing_name$Quantity[x] <- 0.75
        }
        
        if (str_detect(quant, "1⁄8") == TRUE){
          ing_name$Quantity[x] <- 0.125
        }
        
        if (str_detect(quant, "1⁄3") == TRUE){
          ing_name$Quantity[x] <- 0.33
        }
        
        if (str_detect(quant, "-") == TRUE){
          quant <- gsub(" -.*","", quant)
          ing_name$Quantity[x] <- quant
        }
        x <- x + 1
      }
      
      x <- 1
      for (row in ing_name$Conversion){
        ing_name$CO2_Calculated[x] <- (as.numeric(ing_name$CO2[x])/as.numeric(row))*as.numeric(ing_name$Quantity[x])
        x <- x + 1
      }
      
      is.na(ing_name$CO2_Calculated[12])
      x <- 1
      for (row in ing_name$CO2_Calculated){
        if (is.na(row) == TRUE){
          ing_name$CO2_Calculated[x] <- (as.numeric(ing_name$CO2[x])/as.numeric(ing_name$Conversion[x])*0.1)
        }
        x <- x + 1
      }
    })
    
    Score <- (sum(as.numeric(ing_name$CO2_Calculated), na.rm=TRUE))
    
    title_id <- data.frame(Title = title, Score)
    dbWriteTable(db, "recipes_scores", title_id, append = T)
  }
}


################################################################################################################################################
################################################################################################################################################
################################################################ Learn More ####################################################################
################################################################################################################################################
################################################################################################################################################

#Learn more page

#* Find the CO2 for the ingredient
#* @param dash_ing the ingredient to search for
#* @get /dash_info
#* @response print learnmore


get_co2 <- function(dash_ing) {
  
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
  
  learnmore <- emissions_data %>%
    filter(ingredient == dash_ing)
  
}


################################################################################################################################################
################################################################################################################################################
################################################################ Manual Score ##################################################################
################################################################################################################################################
################################################################################################################################################

# script name: Score_function
# plumber_example.R

# Define API

#* @get /get_score_manual
#* @param list_ing
#* @param list_quant
#* @param list_meas
#* @response print result

webtable <- function(list_ing, list_quant, list_meas) {
  
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
  
  Ingredients <- list_ing
  Quantity <- list_quant
  Unit <- list_meas
  
  webtable <- data.frame(Ingredients, Quantity, Unit)
  
  webtable$Ingredient_parsed <- " "
  
  suppressWarnings({
    #find the ingredients from the file
    x <- 1
    for (row in webtable$Ingredients){
      row <- as.list(row)
      test_ing <- amatch(row, emissions_data$ingredient, maxDist = 0.1, method = "cosine")
      for (words in test_ing){
        if (is.na(words) == FALSE) {
          webtable$Ingredient_parsed[x] <- emissions_data$ingredient[words]
        }
        x <- x + 1
      }}
    
    #add info
    x <- 1
    for (row in webtable$Ingredient_parsed) {
      if (row %in% emissions_data$ingredient) {
        #add emission
        webtable$CO2[x] <- emissions_data %>%
          filter(ingredient == row) %>%
          summarize(mean(emissions)) 
        #add variety
        webtable$Variety[x] <- emissions_data %>%
          filter(ingredient == row) %>%
          select(variety)
      } else {
        #if nothing is parsed
        webtable$CO2[x] <- 0
        webtable$Variety[x] <- "N/A"
      }
      x <- x + 1
    }
    
    cooking_units_data$KG <- c(39.37007874, 39.37007874, 1, 1, 1, 1,1,1,1000,1000,1000,1000,1,1,1,1,1,1,1,1,2,1000000,1000000, 1000000,1000000,1000000,35.27336861,35.27336861,35.27336861,2,2,1,1,1,1,1,1,4.2268,415.54,100,4.2268,4.2268,2500,10,10,10,10,10,33.8140227,2.11,1.0567,33.8140227,0.26,0.26,0.26,9.92063,26.45503,1,1,1000,1,2795.56,2795.56,2.113376419,2.113376419,2.113376419,0.01,1.1,0.001,67.6280454,67.6280454,67.6280454,67.6280454,67.6280454,202.8841362,202.8841362,202.8841362,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1000,1000,1000,1.1,2500,1,1,1,1,1, 1,117.65, 117.65, 1000, 1000, 1000, 1000, 1, 1, 1, 1, 2.20462,1,1,1,1,1,1,1,1,1,1,1, 20000,20000, 1,1, 1, 1, 1)
    
    #add conversion
    x <- 1
    for (row in webtable$Unit) {
      if (row %in% cooking_units_data$unit) {
        #add conversion rate
        webtable$Conversion[x] <- cooking_units_data %>%
          filter(unit == row) %>%
          select(KG)
      } else {
        #if nothing is parsed
        webtable$Conversion[x] <- 1
      }
      x <- x + 1
    }
    
    x <- 1
    for (row in webtable$Conversion){
      webtable$CO2_Calculated[x] <- (as.numeric(webtable$CO2[x])/as.numeric(row))*as.numeric(webtable$Quantity[x])
      x <- x + 1
    }
    
    x <- 1
    for (row in webtable$CO2_Calculated){
      if (is.na(row) == TRUE){
        webtable$CO2_Calculated[x] <- (as.numeric(webtable$CO2[x])/as.numeric(webtable$Conversion[x])*0.1)
      }
      x <- x + 1
    }
  })
  
  result <- (sum(as.numeric(webtable$CO2_Calculated), na.rm=TRUE))
  
}

##
