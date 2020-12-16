# CMEE 2020 HPC excercises R code main proforma
# you don't HAVE to use this but it will be very helpful.  If you opt to write everything yourself from scratch please ensure you use EXACTLY the same function and parameter names and beware that you may loose marks if it doesn't work properly because of not using the proforma.

name <- "Zebulon Bond"
preferred_name <- "Zeb"
email <- "zb520@imperial.ac.uk"
username <- "zeb-smells"

# please remember *not* to clear the workspace here, or anywhere in this file. If you do, it'll wipe out your username information that you entered just above, and when you use this file as a 'toolbox' as intended it'll also wipe away everything you're doing outside of the toolbox.  For example, it would wipe away any automarking code that may be running and that would be annoying!
require(ggplot2)
# Question 1

species_richness <- function(community){
  return(length(unique(community)))
}

# Question 2
init_community_max <- function(size){
  return(seq(size))
}

# Question 3
init_community_min <- function(size){
  return(rep(1, size))
}

# Question 4
choose_two <- function(max_value){
  return(sample(seq(max_value), 2))  
}

# Question 5 you can do this better
neutral_step <- function(community){
  #sample 2 random indexes and copy one over the other
  x <- sample(seq(length(community)), 2)
  community[x[1]] <- community[x[2]]

  return(community)
}

# Question 6
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

# Question 7
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

# Question 8
question_8 <- function() {
  diversity <- neutral_time_series(init_community_max(100), 200)

  graphics.off()
  plot(diversity, ylab = "Species Richness", xlab = "Generations", col = "darkred")
  # clear any existing graphs and plot your graph within the R window
  return("The system will always reduce to a species richness of 1. As there is no speciation. 
  no new species can be introduced the system and the number will always reduce and monodominance achieved.")
}

# Question 9
neutral_step_speciation <- function(community, speciation_rate){
  # randomly sample an index
  x <- sample(seq(length(community)), 2)

  
  if(runif(1) > speciation_rate){
    community[x[1]] <- community[x[2]]
  }
  else{
    community[x[1]] <- max(community) + 1
  }

  return(community)
}

# Question 10
neutral_generation_speciation <- function(community, speciation_rate){
  x = length(community)/2

  if(x*2 %% 2 != 0){
    x = sample(c(floor(x), ceiling(x)),1)
  }

  for(i in seq(x)){
    community <- neutral_step_speciation(community, speciation_rate)
  }

  return(community)
}

# Question 11
neutral_time_series_speciation <- function(community,speciation_rate,duration)  {
  x <- rep(NA, duration + 1)
  x[1] <- species_richness(community)

  for(i in seq(duration)){
    community <- neutral_generation_speciation(community, speciation_rate)
    x[i + 1] <- species_richness(community)
  }
  return(x)
}

# Question 12
question_12 <- function() {
  # clear any existing graphs and plot your graph within the R window
  
  diversity_MAX <- neutral_time_series_speciation(init_community_max(100), 0.1, 200)
  diversity_MIN <- neutral_time_series_speciation(init_community_min(100), 0.1, 200)
  gens <- seq(201)

  temp <- data.frame(c(diversity_MAX, diversity_MIN), c(rep("max", 201), rep("min", 201)), c(gens, gens))
  names(temp) <- c("Species.Richness", "Minmax", "Generations")
  
  graphics.off()
  qplot(Generations, Species.Richness, data = temp, colour = Minmax)

  return("Regardless of the initial conditions the system reaches a dynamic equilibrium 
  determined by the speciation rate.")
}

# Question 13
species_abundance <- function(community)  {
  return(sort(as.vector(table(community)), decreasing = TRUE))
}

# Question 14
octaves <- function(abundance_vector) {
  return(tabulate(floor(log2(abundance_vector)) + 1))
}

# Question 15
sum_vect <- function(x, y) {
  if(length(x) > length(y)){
    z <- y
    y <- x
    x <- z
  }
  x <- c(x, rep(0, length(y) - length(x)))
  return(x + y)
}

# Question 16 
question_16 <- function()  {
  # clear any existing graphs and plot your graph within the R window
  time_series_oct_avg <- function(community, speciation_rate, duration){
    x <- octaves(species_abundance(community))

    for(i in seq(200)){
      community <- neutral_generation_speciation(community, speciation_rate)
      x <- sum_vect(octaves(species_abundance(community)), x)
    }
    for(i in seq(duration-200)){
      community <- neutral_generation_speciation(community, speciation_rate)
      if(i %% 20 == 0){
        x <- sum_vect(octaves(species_abundance(community)), x)
      }
    }
    return(x/(201 + (duration - 200)%/%20))
  }

  octave_MAX <- time_series_oct_avg(init_community_max(100), 0.1, 2200)
  octave_MIN <- time_series_oct_avg(init_community_min(100), 0.1, 2200)

  
  if(length(octave_MAX) > length(octave_MIN)){
    octave_MIN <- c(octave_MIN, rep(0, length(octave_MAX) - length(octave_MIN)))
  }
  if(length(octave_MAX) < length(octave_MIN)){
    octave_MAX <- c(octave_MAX, rep(0, length(octave_MIN) - length(octave_MAX)))
  }
  
  #label this barplot
  graphics.off()
  counts <- matrix(rep(NA, 14), nrow = 7, ncol = 2)
  counts[,1] <- octave_MAX
  counts[,2] <- octave_MIN
  barplot(counts, col=c("darkblue", "red"), beside = TRUE)

  return("type your written answer here")
}

# Question 17

cluster_run <- function(speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations, output_file_name)  {
  community <- init_community_min(size)
  rich <- species_richness(community)
  oct <- list()
  oct[[1]] <- octaves(species_abundance(community))
  i <- 1

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
  act_time <- proc.time()[[3]]/60

  print(paste("The output has been saved to", output_file_name))
  save(
    rich, oct, community, speciation_rate, size, wall_time,
    interval_rich, interval_oct, burn_in_generations, act_time,
    file = output_file_name
  )
}



# Questions 18 and 19 involve writing code elsewhere to run your simulations on the cluster

# Question 20 
process_cluster_results <- function()  {
  combined_results <- list() #create your list output here to return
  # save results to an .rda file
  
}

plot_cluster_results <- function()  {
    # clear any existing graphs and plot your graph within the R window
    # load combined_results from your rda file
    # plot the graphs
    
    return(combined_results)
}

#i think uve done these wrong
# Question 21
question_21 <- function()  {  
  ans = log(8, 3)
  return(
    list(ans, paste("to be thrice as wide it needs 8* as much material 8 = 3**", 
    as.character(ans), sep = ""))
    )
}

# Question 22
question_22 <- function()  {
  ans = log(22, 3)
  return(
    list(ans, paste("to be thrice as wide it needs 22* as much material 22 = 3**",
    as.character(ans), sep = ""))
  )
}

# Question 23
chaos_game <- function()  {
  graphics.off()
  X = c(0, 0)

  coord = matrix(c(0,0,3,4,4,1), nrow = 2, ncol = 3)

  par(cex = 0.01)
  par(pch = 19)

  plot(x = -1, xlim = c(0,4), ylim = c(0,4))

  for(j in 1:10000){
    X <- (coord[,sample(c(1, 2, 3),1)] + X)/2
    points(x = X[1], y = X[2])
  }

  return("type your written answer here")
}

# Question 24
turtle <- function(start_position, direction, length)  {
    a = length*sin(direction)
    b = length*cos(direction)
    end_point = c(start_position[1] + b, start_position[2] + a)
    segments(start_position[1], start_position[2], end_point[1], end_point[2])
  return(end_point) # you should return your endpoint here.
}

# Question 25
elbow <- function(start_position, direction, length)  {
  x = turtle(start_position, direction, length)
  turtle(x, direction + 4/pi , length*0.95)
}

# Question 26
spiral <- function(start_position, direction, length)  {
  if(length > 0.1){
    x = turtle(start_position, direction, length)
    spiral(x, direction + 4/pi , length*0.95)
  }

  return("type your written answer here")
}

# Question 27
draw_spiral <- function()  { # broken ask james why!
  
  graphics.off()
  plot(x = 1, xlim = c(-1,5), ylim = c(0,6))
  spiral(c(0,0), 0, 4)
  
}

# Question 28
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

# Question 29
fern <- function(start_position, direction, length)  {
  if(length > 0.1){
    x = turtle(start_position, direction, length)
    fern(x, direction, length*0.87)
    fern(x, direction + 4/pi , length*0.38)
  }  
  return(NA)
}

draw_fern <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  plot( x = 1, xlim = c(0, 35), ylim = c(-6, 10))
  fern(c(0, 0), 0, 4)
}

# Question 30
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

# Challenge questions - these are optional, substantially harder, and a maximum of 16% is available for doing them.  
init_community_rand <- function(size, sr){ #sr cannot be greater than size
  return(c(seq(sr), sample(seq(sr), size - sr, replace = TRUE)))
}

mean_lean_simulation_machine <- function(community, d, sr, iterations){
  communitys <- matrix(rep(community, iterations), nrow = length(community), ncol = iterations)

  diversity <- apply(communitys, 2, neutral_time_series_speciation, speciation_rate = sr, duration = d)

  div_mean <- apply(diversity, 1, mean)

  return(div_mean)
}
# Challenge question A #quantile!!!
Challenge_A <- function() {
  # clear any existing graphs and plot your graph within the R window
  #make matrix 100 wide columns = max
 
  div_MAX_mean <- mean_lean_simulation_machine(init_community_max(100), 200, 0.1, 100)
  div_MIN_mean <- mean_lean_simulation_machine(init_community_min(100), 200, 0.1, 100)

  gens <- seq(201)

  temp <- data.frame(c(div_MAX_mean, div_MIN_mean), c(rep("max", 201), rep("min", 201)), c(gens, gens))
  names(temp) <- c("Species.Richness", "Minmax", "Generations")
  
  graphics.off()
  qplot(Generations, Species.Richness, data = temp, colour = Minmax)
} # not finished need to add stuff

# Challenge question B

Challenge_B <- function() {
  # clear any existing graphs and plot your graph within the R window
  x <- c()
  y <- c()
  for(i in seq(1, 100, by = 33)){
    x <- c(x, mean_lean_simulation_machine(init_community_rand(100, i), 200, 0.1, 100))
    y <- c(y, rep(as.character(i), 201))
  }

  gens <- seq(201)

  temp <- data.frame(x, y, rep(gens, 4))
  names(temp) <- c("Species.Richness", "Initial.Richness", "Generations")

  graphics.off()
  qplot(Generations, Species.Richness, data = temp, colour = Initial.Richness, alpha = 0.8)
}

# Challenge question C
Challenge_C <- function() {
  # clear any existing graphs and plot your graph within the R window

}

# Challenge question D
Challenge_D <- function() {
  # clear any existing graphs and plot your graph within the R window
  
  return("type your written answer here")
}

# Challenge question E
Challenge_E <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  X = c(2, 2)

  coord = matrix(c(0,0,3,4,4,1), nrow = 2, ncol = 3)

  par(cex = 0.01)
  par(pch = 19)

  plot(x = -1, xlim = c(0,4), ylim = c(0,4))

  for(j in 1:10000){
    X <- (coord[,sample(c(1, 2, 3),1)] + X)/2
    points(x = X[1], y = X[2])
  }
  return("type your written answer here")
}

# Challenge question F
Challenge_F <- function() {
  # clear any existing graphs and plot your graph within the R window
  
  return("type your written answer here")
}

# Challenge question G should be written in a separate file that has no dependencies on any functions here.


