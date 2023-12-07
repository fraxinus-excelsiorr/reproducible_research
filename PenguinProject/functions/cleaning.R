## ---------------------------
##
## Script name: Cleaning.r
##
## Purpose of script: 
##      Cleaning up the raw penguin data set by changing column names and removing columns. 
##
## Author: Dr. Lydia France
##
## Date Created: 2023-10-03
##
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------


#A new function to change the word "culmen" to "bill" in the column names 
culmen_to_bill <- function(penguins_data) {
  penguins_data %>%
  rename(bill_depth_mm = culmen_depth_mm, bill_length_mm = culmen_length_mm)
}


# A function to make sure the column names are cleaned up, 
# eg lower case and snake case
clean_column_names <- function(penguins_data) {
  penguins_data %>%
    clean_names()
}

# A function to make sure the species names are shortened
shorten_species <- function(penguins_data) {
  penguins_data %>%
    mutate(species = case_when(
      species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
      species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
      species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
    ))
}

# A function to remove any empty columns or rows
remove_empty_columns_rows <- function(penguins_data) {
  penguins_data %>%
    remove_empty(c("rows", "cols"))
}


# A function to subset the data based on the list of column names
subset_columns <- function(penguins_data, column_names) {
  penguins_data %>%
    select(all_of(column_names))
}

# Functions to subset the penguins data set based on species
#Adelie:
filter_by_adelie <- function(penguins_data, adelie) {
  penguins_data %>%
    filter(species == "Adelie")
}

#Chinstrap:
filter_by_chinstrap <- function(penguins_data, chinstrap) {
  penguins_data %>%
    filter(species == "Chinstrap")
}

#Gentoo:
filter_by_gentoo <- function(penguins_data, gentoo) {
  penguins_data %>%
    filter(species == "Gentoo")
}

# A function to filter the penguins data set based on island
# --- --- ---



# --- --- ---


# A function to remove rows which contain NA values
remove_NA <- function(penguins_data) {
  penguins_data %>%
    na.omit()
}












############################

