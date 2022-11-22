#Learn more page

#* Find the CO2 for the ingredient
#* @param ingredient the ingredient to search for
#* @get /ing_co2
#* @response print learnmoreC02


get_co2 <- function(ingredient, variety, certification) {
  
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
  
  learnmore <- emissions_data[emissions_data[,"ingredient"]==ingredient & emissions_data[,"variety"]==variety & emissions_data[,"certification"]==certification,]
  
  learnmoreCO2<-learnmore[,3]
  
}