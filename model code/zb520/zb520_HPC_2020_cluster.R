# CMEE 2020 HPC excercises R code HPC run code proforma

rm(list=ls()) # good practice 
graphics.off()

source("/rds/general/user/zb520/home/zeb-smells_HPC_2020_main.R")

iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX")) 

set.seed(iter)

if(iter <= 25){
    size <- 500
}else if(iter <= 50){
    size <- 1000
}else if(iter <= 75){
    size <- 2500
}else if(iter <= 100){
    size <- 5000
}

file_name <- paste("Simulation", as.character(iter), ".RData", sep = "")

cluster_run(0.0045432, size, 11.5*60, 1, size/10, 8*size, file_name)