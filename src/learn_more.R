#Learn more page

#* Find the CO2 for the ingredient
#* @param ingredient the ingredient to search for
#* @get /ing_co2
#* @response print learnmoreC02


get_co2 <- function(ingredient, variety, certification) {
  
  learnmore <- emissions_data[emissions_data[,"ingredient"]==ingredient & emissions_data[,"variety"]==variety & emissions_data[,"certification"]==certification,]
  
  learnmoreCO2<-learnmore[,3]
  
}