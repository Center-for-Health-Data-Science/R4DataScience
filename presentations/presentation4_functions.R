# Packages should be loaded in the main script.

# Function to calculate BMI
# Required packages: none
calculate_bmi <- function(weight_kg, height_m){
  
  bmi <- weight_kg/height_m^2
  
  return(bmi)
  
}

# Function to estimate birth year
# Required packages: none
estimate_birth_year <- function(currect_year = 2025, age){
  
  birth_year <- currect_year - age
  
  return(birth_year)
  
}


# Function to calculate BMI, with control points and error checking. 
# Required packages: none
calculate_bmi_2 <- function(weight_kg, height_m) {
  # Check if weight and height are numeric
  if (!is.numeric(weight_kg) | !is.numeric(height_m)) {
    stop("Both weight_kg and height_m must be numeric values.")
  }
  
  # Check if weight and height are positive
  if (weight_kg <= 0) {
    stop("Weight must be a positive value.")
  }
  if (height_m <= 0) {
    stop("Height must be a positive value.")
  }
  
  # Calculate BMI
  bmi <- weight_kg / height_m^2
  
  # Check if BMI is within a reasonable range
  if (bmi < 10 | bmi > 60) {
    warning("The calculated BMI is outside the normal range. Please check your input values.")
  }
  
  return(bmi)
  
}



