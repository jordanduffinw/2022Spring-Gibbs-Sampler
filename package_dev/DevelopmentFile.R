#Load libraries necessary for the exam
library(devtools)
library(roxygen2)

#Set the wd; change as needed:
#setwd()


#Verify the package and testing
current.code <- as.package("gibbs.pack")
load_all(current.code)
document(current.code)
check(current.code)
use_mit_license(current.code)

?step2fun
?step3fun

use_test("step1fun")
test("step1fun")
