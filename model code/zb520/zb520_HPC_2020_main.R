# CMEE 2020 HPC excercises R code main proforma
# you don't HAVE to use this but it will be very helpful.  If you opt to write everything yourself from scratch please ensure you use EXACTLY the same function and parameter names and beware that you may loose marks if it doesn't work properly because of not using the proforma.

name <- "Zebulon Bond"
preferred_name <- "Zeb"
email <- "zb520@imperial.ac.uk"
username <- "zb520"
personal_speciation_rate <- 0.0045432 #used the username zeb-smells to generate as this is my github username

# please remember *not* to clear the workspace here, or anywhere in this file. If you do, it'll wipe out your username information that you entered just above, and when you use this file as a 'toolbox' as intended it'll also wipe away everything you're doing outside of the toolbox.  For example, it would wipe away any automarking code that may be running and that would be annoying!
require(ggplot2)

# QUESTION 1
species_richness <- function(community){
  return(length(unique(community)))
}

# QUESTION 2
init_community_max <- function(size){
  return(seq(size))
}

# QUESTION 3
init_community_min <- function(size){
  return(rep(1, size))
}

# QUESTION 4
choose_two <- function(max_value){
  return(sample(seq(max_value), 2))  
}

# QUESTION 5 you can do this better
neutral_step <- function(community){
  #sample 2 random indexes and copy one over the other
  x <- sample(seq(length(community)), 2)
  community[x[1]] <- community[x[2]]

  return(community)
}

# QUESTION 6
neutral_generation <- function(community){
  # set number of iteratrions for 1 genereation
  x = length(community)/2

  # if x is non integer randomly round up or down
  if(x*2 %% 2 != 0){
    x = sample(c(floor(x), ceiling(x)),1)
  }
  
  for(i in seq(x)){
    community <- neutral_step(community)
  }

  return(community)
}

# QUESTION 7
neutral_time_series <- function(community,duration){
  # preallocate species richness vector
  x <- rep(NA, duration + 1)
  x[1] <- species_richness(community)

  #populate species richness vector
  for(i in seq(duration)){
    community <- neutral_generation(community)
    x[i + 1] <- species_richness(community)
  }

  return(x)
}

# QUESTION 8
question_8 <- function() {
  diversity <- neutral_time_series(init_community_max(100), 200)

  #plot graph
  graphics.off()
  plot(diversity, ylab = "Species Richness", xlab = "Generations", col = "darkred")
  
  return("The system will always reduce to a species richness of 1. As there is no speciation. 
  no new species can be introduced the system and the number will always reduce and monodominance achieved.")
}

# QUESTION 9
neutral_step_speciation <- function(community, speciation_rate){
  # randomly sample an index
  x <- sample(seq(length(community)), 2)

  #randomly select if speciation occurs
  if(runif(1) > speciation_rate){
    community[x[1]] <- community[x[2]]
  }
  else{
    community[x[1]] <- max(community) + 1
  }

  return(community)
}

# QUESTION 10
neutral_generation_speciation <- function(community, speciation_rate){
  # init length of gen X
  x = length(community)/2

  # if community is odd randomly round x up or down
  if(x*2 %% 2 != 0){
    x = sample(c(floor(x), ceiling(x)),1)
  }

  # perform neutral steps for generation
  for(i in seq(x)){
    community <- neutral_step_speciation(community, speciation_rate)
  }

  return(community)
}

# QUESTION 11
neutral_time_series_speciation <- function(community,speciation_rate,duration)  {
  # initialise return vector
  richnesses <- rep(NA, duration + 1)
  richnesses[1] <- species_richness(community)

  # add species richness of each generation to return vector
  for(i in seq(duration)){
    community <- neutral_generation_speciation(community, speciation_rate)
    richnesses[i + 1] <- species_richness(community)
  }

  return(richnesses)
}

# QUESTION 12
question_12 <- function() {
  # clear any existing graphs and plot your graph within the R window
  
  #perform simulations
  diversity_MAX <- neutral_time_series_speciation(init_community_max(100), 0.1, 200)
  diversity_MIN <- neutral_time_series_speciation(init_community_min(100), 0.1, 200)

  #create generation vector
  gens <- seq(201)

  #create dataframe
  temp <- data.frame(c(diversity_MAX, diversity_MIN), c(rep("max", 201), rep("min", 201)), c(gens, gens))
  names(temp) <- c("Species.Richness", "Minmax", "Generations")
  
  #plot
  graphics.off()
  qplot(Generations, Species.Richness, data = temp, colour = Minmax)

  return("Regardless of the initial conditions the system reaches a dynamic equilibrium 
  determined by the speciation rate.")
}

# QUESTION 13
species_abundance <- function(community)  {
  return(sort(as.vector(table(community)), decreasing = TRUE))
}

# QUESTION 14
octaves <- function(abundance_vector) {
  return(tabulate(floor(log2(abundance_vector)) + 1))
}

# QUESTION 15
sum_vect <- function(x, y) {

  #assign the longest vector as y
  if(length(x) > length(y)){
    z <- y
    y <- x
    x <- z
  }

  #add zeros to shortest vector
  x <- c(x, rep(0, length(y) - length(x)))

  return(x + y)
}

# QUESTION 16 
time_series_oct_avg <- function(community, speciation_rate, duration){

  #initialise x 
  x <- octaves(species_abundance(community))

  #perform burn in
  for(i in seq(200)){
    community <- neutral_generation_speciation(community, speciation_rate)
    x <- sum_vect(octaves(species_abundance(community)), x)
  }

  #continue the rest of the simulation
  for(i in seq(duration-200)){
    community <- neutral_generation_speciation(community, speciation_rate)
    if(i %% 20 == 0){
      x <- sum_vect(octaves(species_abundance(community)), x)
    }
  }

  return(x/(201 + (duration - 200)%/%20))
}

question_16 <- function()  {
  # run simulations
  octave_MAX <- time_series_oct_avg(init_community_max(100), 0.1, 2200)
  octave_MIN <- time_series_oct_avg(init_community_min(100), 0.1, 2200)
  
  # create data frame
  counts <- c(octave_MAX, octave_MIN)
  index <- c(seq(1, length(octave_MAX)), seq(1, length(octave_MIN)))
  min_max <- c(rep("MAX", length(octave_MAX)), rep("MIN", length(octave_MIN)))
  dataf <- data.frame(counts, index, min_max)
  names(dataf) <- c("counts", "index", "initial.Richness")
  
  # plot
  graphics.off()
  p <- ggplot(data = dataf, aes(x=index, y=counts, fill=initial.Richness)) + 
  geom_bar(stat="identity", position=position_dodge()) 
  print(p)

  return("the initial condition does not matter. The stable state is determined by the speciation rate")
}

# QUESTION 17
cluster_run <- function(speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations, output_file_name)  {

  # initialise
  community <- init_community_min(size)
  rich <- species_richness(community)
  oct <- list()
  oct[[1]] <- octaves(species_abundance(community))
  i <- 1

  #run simulation for wall time
  while(proc.time()[[3]] < wall_time*60){
    community <- neutral_generation_speciation(community, speciation_rate)
    i <- i + 1

    # record communitys
    if(i %% interval_rich == 0 && i < burn_in_generations){
      rich[i/interval_rich + 1] <- species_richness(community)
    }

    #record octaves
    if(i %% interval_oct == 0){
      oct[[i/interval_oct + 1]] <- octaves(species_abundance(community))
    }
  }
  
  #record time
  act_time <- proc.time()[[3]]/60

  #save output
  print(paste("The output has been saved to", output_file_name))
  save(
    rich, oct, community, speciation_rate, size, wall_time,
    interval_rich, interval_oct, burn_in_generations, act_time,
    file = output_file_name
  )
}

# Questions 18 and 19 involve writing code elsewhere to run your simulations on the cluster

# QUESTION 20 
avg_oct_calc <- function(data){
  # calculate the avg octave for 1 iteration
  X <- c(0,0)
  for(i in 1:length(data[[2]])){
    if(i > data[[9]]/data[[8]]){
      X <- sum_vect(X, data[[2]][[i]])
    }
  }
  return(X/(length(data[[2]]) - (data[[9]]/data[[8]])))
}

process_cluster_results <- function()  {
  
  #initialise
  tot_500  <- c(0,0)
  tot_1000 <- c(0,0)
  tot_2500 <- c(0,0)
  tot_5000 <- c(0,0)

  #total the avg octave for each size
  for(i in 1:100){
    load(paste("data/output/Simulation", i, ".RData", sep = ""))
    temp <- list(
      rich, oct, community, speciation_rate, size, wall_time,
      interval_rich, interval_oct, burn_in_generations, act_time
    )
    total_name <- paste("tot_", temp[[5]], sep = "")
    assign(total_name, sum_vect(get(total_name), avg_oct_calc(temp))) 
  }

  #create and save list
  combined_results <- list(tot_500/25, tot_1000/25, tot_2500/25, tot_5000/25)
  save(combined_results, file = "data/combinedresults.RData") 
  
}

plot_cluster_results <- function()  {
  # clear any existing graphs and plot your graph within the R window
  # load combined_results from your rda file
  # plot the graphs
  # load data and asign
  load("data/combinedresults.RData")
  cr <- combined_results

  #create vectors
  octs <- c(cr[[1]], cr[[2]], cr[[3]], cr[[4]])

  size <- c(
    rep("500", length(cr[[1]])),
    rep("1000", length(cr[[2]])),
    rep("2000", length(cr[[3]])),
    rep("4000", length(cr[[4]]))
  )

  index <- c(
    seq(1, length(cr[[1]])),
    seq(1, length(cr[[2]])),
    seq(1, length(cr[[3]])),
    seq(1, length(cr[[4]]))
  )

  #create data frame
  dataf <- data.frame(octs, size, index)
  names(dataf) <- c("counts", "population.size", "index")

  #plot
  graphics.off()
  p <- ggplot(data = dataf, aes(x=index, y=counts, fill=population.size)) + 
  geom_bar(stat="identity", position=position_dodge())
  print(p)

  return(combined_results)
}

# QUESTION 21
question_21 <- function()  {  
  ans = log(8, 3)
  return(
    list(ans, paste("to be thrice as wide it needs 8* as much material 8 = 3**", 
    as.character(ans), sep = ""))
    )
}

# QUESTION 22
question_22 <- function()  {
  ans = log(22, 3)
  return(
    list(ans, paste("to be thrice as wide it needs 22* as much material 22 = 3**",
    as.character(ans), sep = ""))
  )
}

# QUESTION 23
chaos_game <- function()  {
  #initialise X
  graphics.off()
  X = c(0, 0)

  #initilise corners
  coord = matrix(c(0,0,3,4,4,1), nrow = 2, ncol = 3)

  #set pointer
  par(cex = 0.01)
  par(pch = 19)

  #init plot
  plot(x = -1, xlim = c(0,4), ylim = c(0,4))

  #randomly select a point and travel halfway there and mark
  for(j in 1:1000){
    X <- (coord[,sample(c(1, 2, 3),1)] + X)/2
    points(x = X[1], y = X[2])
  }

  return("Sierpinski Triangle")
}

# QUESTION 24
turtle <- function(start_position, direction, length)  {
  a = length*sin(direction)
  b = length*cos(direction)
  end_point = c(start_position[1] + b, start_position[2] + a)
  segments(start_position[1], start_position[2], end_point[1], end_point[2])
  return(end_point) # you should return your endpoint here.
}

# QUESTION 25
elbow <- function(start_position, direction, length)  {
  x = turtle(start_position, direction, length)
  turtle(x, direction + 4/pi , length*0.95)
}

# QUESTION 26
spiral <- function(start_position, direction, length)  {
  if(length > 0.1){
    x = turtle(start_position, direction, length)
    spiral(x, direction + 4/pi , length*0.95)
  }

  return("As spiral function is recusrsive with no end condition you get infinite iterations")
}

# QUESTION 27
draw_spiral <- function()  { # broken ask james why!
  
  graphics.off()
  plot(x = 1, xlim = c(-1,5), ylim = c(0,6))
  spiral(c(0,0), 0, 4)
  
}

# QUESTION 28
tree <- function(start_position, direction, length)  {
  if(length > 0.01){
    x = turtle(start_position, direction, length)
    tree(x, direction - 4/pi , length*0.65)
    tree(x, direction + 4/pi , length*0.65)
  }  
}

draw_tree <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  plot(x = 1, xlim = c(0, 9), ylim = c(-6, 6))
  tree(c(0, 0), 0, 4)
}

# QUESTION 29
fern <- function(start_position, direction, length)  {
  if(length > 0.1){
    x = turtle(start_position, direction, length)
    fern(x, direction, length*0.87)
    fern(x, direction + 4/pi , length*0.38)
  }  
}

draw_fern <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  plot( x = 1, xlim = c(0, 35), ylim = c(-6, 10))
  fern(c(0, 0), 0, 4)
}

# QUESTION 30
fern2 <- function(start_position, direction, length, dir)  {
  if(length > 0.01){
    x = turtle(start_position, direction, length)
    fern2(x, direction + dir*(4/pi), length*0.38, dir)
    fern2(x, direction, length*0.87, dir*(-1))
  }
}

draw_fern2 <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  plot( x = 1, xlim = c(-11, 11), ylim = c(0, 30))
  fern2(c(0, 0), 0.5*pi, 4, 1)
}

# CHALLENGE questions - these are optional, substantially harder, and a maximum of 16% is available for doing them.

# intialise communitys of size size and species richness sr
init_community_rand <- function(size, sr){ #sr cannot be greater than size
  return(c(seq(sr), sample(seq(sr), size - sr, replace = TRUE)))
}

#run a simulation for x iterations and return the mean simulation
mean_lean_simulation_machine <- function(community, d, sr, iterations){
  communitys <- matrix(rep(community, iterations), nrow = length(community), ncol = iterations)
  diversity <- apply(communitys, 2, neutral_time_series_speciation, speciation_rate = sr, duration = d)
  div_mean <- apply(diversity, 1, mean)
  return(div_mean)
}

# CHALLENGE QUESTION A #quantile!!!
Challenge_A <- function() {
  # clear any existing graphs and plot your graph within the R window
  # run simulations with max and min diversity
  div_MAX_mean <- mean_lean_simulation_machine(init_community_max(100), 200, 0.1, 100)
  div_MIN_mean <- mean_lean_simulation_machine(init_community_min(100), 200, 0.1, 100)

  #create gens vec
  gens <- seq(201)

  #create dataframe
  temp <- data.frame(c(div_MAX_mean, div_MIN_mean), c(rep("max", 201), rep("min", 201)), c(gens, gens))
  names(temp) <- c("Species.Richness", "Minmax", "Generations")
  
  #plot
  graphics.off()
  qplot(Generations, Species.Richness, data = temp, colour = Minmax)
} # not finished need to add stuff

# CHALLENGE QUESTION B
Challenge_B <- function() {
  # clear any existing graphs and plot your graph within the R window
  # init
  x <- c()
  y <- c()
  
  #run simulations 
  for(i in seq(1, 100, by = 24)){
    x <- c(x, mean_lean_simulation_machine(init_community_rand(100, i), 200, 0.3, 100))
    y <- c(y, rep(as.character(i), 201))
  }

  #create generations vector
  gens <- seq(201)

  #create dataframe
  temp <- data.frame(x, y, rep(gens, 5))
  names(temp) <- c("Species.Richness", "Initial.Richness", "Generations")

  #plot
  graphics.off()
  qplot(Generations, Species.Richness, data = temp, colour = Initial.Richness, alpha = 0.8)
}

# CHALLENGE QUESTION C
Challenge_C <- function() {
  # clear any existing graphs and plot your graph within the R window

}

# CHALLENGE QUESTION D
# coalescence model
coalescence_model <- function(J, v){
  #init
  lineages <- rep(1, J)
  abundances <- c()
  N <- J 
  theta <- v*((J - 1)/(1 - v))

  #run simulation
  while(N > 1){
    j <- sample(seq(1,length(lineages)), 1)

    if (runif(1, 0, 1) < theta/(theta + N - 1)){
      abundances <- append(abundances, lineages[j])

    } else {
      i <- sample(seq(1, length(lineages))[-j], 1)
      lineages[i] <- lineages[i] + lineages[j]
    }

    lineages <- lineages[-j]
    N <- N - 1
  }
  
  #record final value
  abundances <- append(abundances, lineages)

  #return abundances in order
  return(sort(abundances, decreasing = TRUE))
}

Challenge_D <- function() {
  # clear any existing graphs and plot your graph within the R window
  #init
  totC_500  <- c(0,0)
  totC_1000 <- c(0,0)
  totC_2500 <- c(0,0)
  totC_5000 <- c(0,0)

  #run simulations return octaves
  for(i in 1:25){
    totC_500  <- sum_vect(octaves(coalescence_model(500 , 0.0045432)) , totC_500)
    totC_1000 <- sum_vect(octaves(coalescence_model(1000, 0.0045432)) , totC_1000)
    totC_2500 <- sum_vect(octaves(coalescence_model(2500, 0.0045432)) , totC_2500)
    totC_5000 <- sum_vect(octaves(coalescence_model(5000, 0.0045432)) , totC_5000)
  }

  #load data
  load("data/combinedresults.RData")

  #asign names
  cr <- combined_results
  cc <- list(totC_500/25, totC_1000/25, totC_2500/25, totC_5000/25) #combined coalescence

  #init vectors
  octs <- c()
  size <- c()
  index <- c()
  model <- c()

  #assign data to vectors
  for(i in 1:4){
    temp_octs <- c(cr[[i]], cc[[i]])
    octs <- append(octs, temp_octs)

    temp_size <- c(
      rep(paste(500*i + 1000*(i-2)), length(cr[[i]])), 
      rep(paste(500*i + 1000*(i-2)), length(cc[[i]]))
    )
    size <- append(size, temp_size)

    temp_index <- c(
      seq(1, length(cr[[i]])),
      seq(1, length(cc[[i]]))
    )
    index <- append(index, temp_index)

    temp_model <- c(
      rep("sequential", length(cr[[i]])),
      rep("coalescence", length(cc[[i]]))
    )
    model <- append(model, temp_model)
  }

  #create data frame
  dataf <- data.frame(octs, size, index, model)
  names(dataf) <- c("counts", "population.size", "index", "model")

  #plot
  graphics.off()
  p <- ggplot(data = dataf, aes(x=index, y=counts, fill=model)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  facet_wrap(.~ population.size)
  print(p)

  return("coalesnce does not require iterations of steady state as it starts at steady state 
  and works backwards. Species that do not apear in the steady state are not simulated")
}

# CHALLENGE QUESTION E
Challenge_E <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  X = c(2, 2)

  coord = matrix(c(0,0,3,4,4,1), nrow = 2, ncol = 3)

  par(cex = 0.01)
  par(pch = 19)

  plot(x = -1, xlim = c(0,4), ylim = c(0,4))

  for(j in 1:1000){
    X <- (coord[,sample(c(1, 2, 3),1)] + X)/2
    points(x = X[1], y = X[2])
  }

  return("Initial position of X does not matter if X is within the set of points in the Triangle.
  If X is outside the set of points a dot will be drawn. Once X moves back within the set it cannot escape.")
}

# CHALLENGE QUESTION F
Challenge_F <- function() {
  # clear any existing graphs and plot your graph within the R window
  
  return("type your written answer here")
}

# CHALLENGE QUESTION G should be written in a separate file that has no dependencies on any functions here.


