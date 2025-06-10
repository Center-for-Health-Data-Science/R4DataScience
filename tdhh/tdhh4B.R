# Exercise 4B - Solutions: Scripting in R - Functions

calculate_risk_score <- function(BloodPressure, BMI, Smoking, PhysicalActivity){
  
  if (!is.numeric(BloodPressure) | !is.numeric(BMI) | !is.numeric(PhysicalActivity)) {
    stop("BloodPressure, BMI, and PhysicalActivity must be numeric values.")
  }
  
  if(! (is.character(Smoking) | is.factor(Smoking)) ){
    stop("Smoking should be a factor or character.")
  }
  
  risk_score <- 0
  
  if (BloodPressure > 90){
    risk_score <- risk_score + 0.5
  }
  
  if (Smoking == 'Smoker'){
    risk_score <- risk_score + 1
  }
  
  #BMI > 30 includes BMIs that are larger than 40, so we need to be careful of
  #the order in which we check, or include that the BMI should be <= 40 in order 
  #to add 1.   
  
  if (BMI > 40){
    risk_score <- risk_score + 2
  } 
  else if( BMI > 30) {
    risk_score <- risk_score + 1
  } 
  
  if (PhysicalActivity > 110){
    risk_score <- risk_score - 1
  }
  
  return(risk_score)
  
}
