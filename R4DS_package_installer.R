# Let's check if everything you need for the course is installed

# If there are packages missing, they will be installed by this script.

# Please run the COMPLETE script. At the end the console output will tell you
# whether all packages are installed or not.

# If not everything works immediately, don't panic. Run it again. Sometimes when dependiencies have to be installed the actual package you want is not correctly installed th first time around.

### important ###
# if you are a windows user, make sure to have Rtools installed
#https://cran.r-project.org/bin/windows/Rtools/

#####################
# first let's look at your R version
#####################

if(as.numeric(base::version$major) < 4){
  print('Your R version may be too old. Please update!')
  print('You can find help here:')
  print('https://uvastatlab.github.io/phdplus/installR.html')
}



#####################
# Ensure BiocManger is installed!
#####################
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.21")


#####################
# create lists of packages to be installed and which ones are already installed
#####################

# list package names that are needed from CRAN
l.packages <-  c("tidyverse",
                 "ggplot2",
                 "readxl", 
                 "writexl",
                 "GGally",
                 "ggpubr",
                 "quarto", 
                 "ggforce", 
                 "FactoMineR",
                 "factoextra",
                 "corrplot",
                 "caret",
                 "ContaminatedMixt",
                 "ggfortify",
                 "DESeq2",
                 "glmnet", 
                 "MASS", 
                 "randomForest",
                 "caTools",
                 "ModelMetrics")

#broom
#patchwork
#glue
#emmeans


# Empty vector to hold packages not yet installed:
m.packages <- c()

# Check is package is already installed
for(p in 1:length(l.packages)) {
  
  if (l.packages[p] %in% rownames(installed.packages())) {
    print(paste0(l.packages[p], ' is installed!'))
    try(library(l.packages[[p]], character.only = TRUE))
    
  } else {
    m.packages <- append(m.packages, l.packages[p])
  }
}




# Unlist missing packages
m.packages <- unlist(m.packages)



# Installing and loading missing packages:
if (length(m.packages) > 0) {
  sp <- paste(m.packages,collapse=", ")
  print(paste0('Package(s): ', sp, ' are missing, installing and loading now.'))
  
  for (p in 1:length(m.packages)) {
    print(paste0('Installing package ', m.packages[p],' from CRAN Repository'))
    try(install.packages(m.packages[p]))
    out <- try(library(m.packages[p], character.only = TRUE))
    
    if (class(out) == "try-error") {
      print(paste0('Installing package ', m.packages[p],' from Bioconductor Repository'))
      try(BiocManager::install(m.packages[p])) 
      out <- try(library(m.packages[p], character.only = TRUE))
      
      if (class(out) == "try-error") {
        print(paste0(m.packages[p], ' cannot be installed!'))
      }
    }
  }
}



# Are all packages loaded
all.packages <- (.packages(all.available = TRUE))


if(length(intersect(l.packages, all.packages)) == length(l.packages)) {
  print('all packages have been successfully installed')
} else {
  notInstalled <-  paste(setdiff(l.packages, all.packages), collapse = ", ") 
  print(paste0('Packages: ', notInstalled, ' not installed!')) 
  print('Try running the script again OR installing them manually with install.packages(name_of_package)')
}
