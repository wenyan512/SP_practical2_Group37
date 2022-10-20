## Course title: Statistical Programming
## Project name: Practical 2
## Group number: 37
## Group Member: Enyan WU (s2303128),
#                Huiyu WU (s2303136), 
#                Xuantao LI (s1822046) 
## Group contribution:

## Project introduction:
# In this project, we simulate three strategies for this classical 100 prisoners problem to see how good they are in guiding prisoners to escape individually or together. 
# In question 1, we model the probability for individual escape. 
# Further, we compare the probability of all personers escape together following each strategy in question 2. 
# In question 5 and 6, we focus more on strategy 1 which is the best from each version, 
# we find out the distribution of loop length which is how many steps a prisoner can find his own card. 
# Finally, simulating the strategy 1 in the version of loop length. 


# verify the total time of running the 
start.time <- format(Sys.time(), format="%H:%M:%S")

# Function purpose: function 'Success' is going to check whether a person can succeed in finding their prisoner number after n open times or not
# Input: n = half of the number of people (2*n is the total number of prisoners)
#        card_dist = the card distribution, indicating how the card numbers placed in one experiment(nrep) 
#        strategy = 1/2/3 (choose one of strategies you want to investigate from three strategy)
# Output: TRUE = if the prisoner with specific prisoner_num can find his/her prisoner number
#         FALSE = if the prisoner with specific prisoner_num cannot find his/her prisoner number
Success <- function(n, card_dist, prisoner_num, strategy){
  ## for strategy 1 and strategy2
  if (strategy == 1 || strategy == 2){
    ## strategy 1
    if (strategy==1){
      # In strategy 1, 1st open the boxes with their number read the card number in it 
      k <- card_dist[prisoner_num]
    }
    ## strategy 2
    if (strategy==2){
      # In strategy 2, starting from a randomly selected box
      # a random box number that he/she will open 
      random_box <- sample(1:(2*n),1)
      # read the card number in this random box
      k <- card_dist[random_box]
    }
    
    # each prisoner has n chances of opening a box
    for (open_times in 1:n){
      # if the card in the box s/he open is her/his prisoner number, s/he success and return TRUE
      if (k == prisoner_num) return (TRUE)
      # if k is not their prisoner number
      else {
        # go to box number k (card number of the box s/he opend last time), and then read the new card number as new k
        k <- card_dist[k]
      }
    }
  }
  
  ## for strategy 3
  else if (strategy==3){
    # random n box numbers that a person will open
    n_random_box <- sample(1:(2*n), n, replace=FALSE)
    # the card number list that contain all card number in the boxes he/she open according to the n_random_box
    select_card_num <- card_dist[n_random_box]
    # if the prison number in the select_card_num list
    # which means one of he/she selected box contian a card with their prison number
    # so s/he succeeded in finding her/his prisoner number, and return TRUE
    if (prisoner_num %in% select_card_num) return (TRUE)
  }
  # if the prisoner cannot find the number after opening n boxes, s/he fail and return 'Success' FALSE
  return (FALSE) 
}


# Function purpose: function 'Pone' is going to estimate the probability of a single prisoner succeeding in finding their number.
# Input: n = half of the number of people (2*n is the total number of prisoners)
#        k = The prisoner number, indicate the prisoner you want to estimate
#        strategy = 1/2/3 (choose one of strategies you want to investigate from three strategy)
#        nreps = the number of replicate simulations to run in order to estimate the probability (10000 is a reasonable default)
# Output: the estimated probability of a single prisoner (k) succeeding in finding s/he number according to nreps repeat times, when using the strategy=1/2/3 .
Pone <- function(n, k, strategy, nreps){
  # count the number of the success that a person can find his/her number 
  count_success = 0
  for (i in 1:nreps) {
    # every repeat time change the distribution of the card number
    card_dist <- sample(1:(2*n), size = 2*n, replace = FALSE)
    # Using 'Success' function to check whether the prisoner can find her/his prisoner or not
    # if 'Success' return TRUE, which means s/he succeeded in find her/his number in this run
    # then add 1 to count_success, since s/he succeeded one more time
    if (Success(n,card_dist,k,strategy)) count_success <- count_success+1
  }
  # estimate the probability of a single prisoner (k) succeeding in finding his/her prisoner number
  prob = count_success/nreps
  return (prob)
}

# verify
Pone(50,2,3,1000)


# Function purpose: function 'Pall' is going to estimate the probability of all prisoners finding their number, so that all are released.
# Input: n = half of the number of people (2*n is the total number of prisoners)
#        strategy = 1/2/3 (choose one of strategies you want to investigate from three strategy)
#        nreps = the number of replicate simulations to run in order to estimate the probability (10000 is a reasonable default)
# Output: the estimated probability of all prisoners finding their number, so that all are released, according to nreps repeat times, when using the strategy=1/2/3 .
Pall <- function(n, strategy, nreps){
  # initialize the prisoner number and record all prisoners number
  prisoners_num <- c(1:(2*n))
  # count the number of the success that a person can find his/her number 
  count_success = 0
  for (i in 1:nreps){
    # every repeat time change the distribution of the card number
    card_dist <- sample(1:(2*n), size = 2*n, replace = FALSE)
    
    # count the number of prisoners who are successful finding their number
    success_prisoner <- 0
    # For each prisoner, we check whether s/he can find her/his prisoner number or not
    for (prisoner in prisoners_num){
      # Using 'Success' function to check whether the prisoner can find her/his prisoner or not
      # if 'Success' return TRUE, which means s/he succeeded in find her/his number in this run
      # then add 1 to count_success, since s/he succeeded one more time
      if (Success(n,card_dist,prisoner,strategy)) success_prisoner <- success_prisoner+1
    }
    # if the number of success is equal to the number of prisoner, it means all prisoners finding their number in this run
    if (success_prisoner == (2*n)) count_success <- count_success + 1
  }
  prob <- count_success/nreps
  return(prob)
}

# verify
# Pall(50,3,10000)


# function implementation example
nreps <- 10000    # the number of replicate simulations to run in order to estimate the probability(10000 is a reasonable default)

## for n= 5
n1 <- 5
## for strategy 1
# individual k=7 success probabilities under strategy 1
n5_individualk_prob_1 <- Pone(n1, 7, 1, nreps); n5_individualk_prob_1
# joint success probabilities under strategy 1
n5_joint_success_prob_1 <- Pall(n1, 1, nreps); n5_joint_success_prob_1

## for strategy 2
# individual k=7 success probabilities under strategy 2
n5_individualk_prob_2 <- Pone(n1, 7, 2, nreps); n5_individualk_prob_2
# joint success probabilities under strategy 2
n5_joint_success_prob_2 <- Pall(n1, 2, nreps); n5_joint_success_prob_2

## for strategy 3
# individual k=7 success probabilities under strategy 3
n5_individualk_prob_3 <- Pone(n1, 7, 3, nreps); n5_individualk_prob_3
# joint success probabilities under strategy 3
n5_joint_success_prob_3 <- Pall(n1, 3, nreps); n5_joint_success_prob_3


## for n= 50
n2 <- 50
## for strategy 1
# individual k=7 success probabilities under strategy 1
n50_individualk_prob_1 <- Pone(n2, 7, 1, nreps); n50_individualk_prob_1
# joint success probabilities under strategy 1
n50_joint_success_prob_1 <- Pall(n2, 1, nreps); n50_joint_success_prob_1

## for strategy 2
# individual k=7 success probabilities under strategy 2
n50_individualk_prob_2 <- Pone(n2, 7, 2, nreps); n50_individualk_prob_2
# joint success probabilities under strategy 2
n50_joint_success_prob_2 <- Pall(n2, 2, nreps); n50_joint_success_prob_2

## for strategy 3
# individual k=7 success probabilities under strategy 3
n50_individualk_prob_3 <- Pone(n2, 7, 3, nreps); n50_individualk_prob_3
# joint success probabilities under strategy 3
n50_joint_success_prob_3 <- Pall(n2, 3, nreps); n50_joint_success_prob_3

############################# surprising results ###############################

# From the results obtained from function "Pone", the estimated probabilities of a single prisoner succeeding in finding their number  
# for three strategies are converging to 0.5, 0.4, 0.5 respectively. However, the results from 
# function "Pall", which shows that the estimated probability of all prisoners finding their number is totally 
# different for three strategies. 
# Surprisingly, the estimated probability for all prisoner are released is over 30% for strategy 1, 
# and the success rate of other two strategies are low, especially it will result in nearly 0 for strategy 3. 
# The most important reason why the prisoners have such high probability to escape in strategy 1
# is the prisoners don't have to decide where he/she chooses at the first step.
# In other words, the success of one prisoner is not independent of the success of the other prisoners in strategy 1,
# since it will depend on how the cards are distributed.

############################# surprising results ###############################



end.time <- format(Sys.time(), format="%H:%M:%S")
time.diff <- as.difftime(c(start.time,end.time),units='secs')
time.diff <- as.numeric(time.diff,units='secs')
(time.diff[2]-time.diff[1])/60
end.time
start.time

