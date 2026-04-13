
# cal_BMI: Calculate Body Mass Index (BMI) given weight and height.
# @param weight: weight of the individual in kilograms
# @param height: height of the individual in meters
cal_BMI <- function(weight, height) {
  
  if(!is.numeric(weight) | !is.numeric(height)) {
    stop("Both weight and height must be numeric values.")
    
  }
  
  if(height <= 0 | weight <= 0) {
    stop("Both variables must be positive numeric values!")
  }
    
  BMI <- weight / (height^2)
  
  if(BMI < 15 | BMI > 60) {
    warning("The calculated BMI is outside the typical range. Please check the input values.")
  }
  
  return(BMI)

}











# est_age: Estimate age based on current year and birth year.
# @param current_year: the current year
# @param birth_year: the year of birth of the individual
est_age <- function(current_year, birth_year) {
  
  age <- current_year - birth_year
  
  return(age)
}



