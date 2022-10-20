## Group Member: Enyan WU (s2303128),
#                Huiyu WU (s2303136), 
#                Xuantao LI (s1822046) 

## Address of your github repo:
# https://github.com/wenyan512/SP_practical2_Group37.git

## Group contribution:
# Overall, the weights of contribution for each group member are fairly equal. 
# We wrote our own coding based on our personal understanding of these questions. 
# We shared our ideas when anyone stick in somewhere and all of us enjoyed the teamwork. 
# The final version upload now is the final work we all committed for.

## Project introduction:
# In this project, we used stochastic simulation to investigate a somewhat surprising 
# probability puzzle, which is also known as the classic 2n prisoners problem. 
# Here we conducted this investigation under three strategies and compared the 
# corresponding probabilities derived to find if there are surprising results.
# After estimating the probabilities of going free (finding their numbers), 
# our next step, which is also the core of our investigation, would be estimating 
# the probability distribution of the number of steps needed to go free.

# The set up of this puzzle is basically as follows: 
# 2n prisoners each with unique numbers from 1 to 2n
# 2n boxes each with unique numbers from 1 to 2n painted on its lid
# 2n cards each printed with a unique number from 1 to 2n, are randomly placed one in each box
# Prisoners have the task of finding the card with their number on it by opening a maximum on n boxes.

# The three strategies are:
# 1) The prisoner starts at the box with their number on it, opens it and reads the number on 
# the card: k, say. If k is not their prisoner number, they go to box number k, open it and repeat 
# the process until they have either found the card with their number on it, or opened n boxes without 
# finding it.
# 2) As strategy 1, but starting from a randomly selected box.
# 3) Prisoners open n boxes at random, checking each card for their number.

# Under the three strategies, 
# We first estimated the probabilities of success (going free) for a single prisoner. 
# Then, we estimated the probabilities for all prisoners to succeed so that they are freed.

# Finally, we estimated a probability distribution of the number of times (from 1 to 2n) needed for 
# a prisoner to succeed (find her/his card). Based on this, we can better understand the surprising result.


############################# Write a function to avoid repetition in code #############################

# Function purpose: 
# function 'Success' is going to check whether a person can succeed in 
# finding their prisoner number after n open times or not

# Input: 
# n -> half of the number of prisoner (2*n is the total number of prisoners)
# card_dist -> the card distribution obtained from randomly simulation, indicating how the 
#              card numbers placed in each replication (nrep) 
# prisoner_num -> prisoner number, indicates which prisoner to be estimated
# strategy -> 1/2/3 (call one of the three strategies to investigate)

# Output: 
# TRUE = if the prisoner with specific prisoner_num can find his/her prisoner number
# FALSE = if the prisoner with specific prisoner_num cannot find his/her prisoner number

Success <- function(n, card_dist, prisoner_num, strategy){
  ## for strategy 1 and strategy2
  if (strategy == 1 || strategy == 2){
    ## strategy 1
    if (strategy==1){
      # In strategy 1, first open the boxes with their number read the card number in it 
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
      # if the card in the box s/he open is her/his prisoner number, s/he successes and return TRUE
      if (k == prisoner_num) return (TRUE)
      # if k is not their prisoner number
      else {
        # go to box number k (card number of the box s/he opened last time), and then 
        # read the new card number as new k
        k <- card_dist[k]
      }
    }
  }
  
  ## for strategy 3
  else if (strategy == 3){
    # randomly simulate n box numbers that a person will open
    n_random_box <- sample(1:(2*n), n, replace=FALSE)
    # create a list containing the corresponding cards placed in the boxes he/she 
    # opens according to the n_random_box
    select_card_num <- card_dist[n_random_box]
    # if the prison number in the select_card_num list
    # which means one of he/she selected box contains the card with their prison number
    # so s/he succeeds in finding her/his prisoner number, and return TRUE
    if (prisoner_num %in% select_card_num) return (TRUE)
  }
  # if the prisoner cannot find the number after opening n boxes, s/he fails and 'Success' returns FALSE
  return (FALSE) 
}

############################# Write a function to avoid repetition in code #############################


# Function purpose: 
# function 'Pone' is going to estimate the probability of a single prisoner 
# succeeding in finding their number.

# Input: 
# n -> half of the number of people (2*n is the total number of prisoners)
# k -> prisoner number, indicates which prisoner to be estimated
# strategy -> 1/2/3 (call one of the three strategies to investigate)
# nreps -> the number of replicate simulations to run in order to estimate the probability 
#          (10000 is a reasonable default)
# Output:
# the estimated probability of a single prisoner (k) succeeding in finding s/he 
# number according to nreps repeating times under strategy=1/2/3.

Pone <- function(n, k, strategy, nreps){
  # count the number of the success that a person can find his/her number 
  count_success = 0
  for (i in 1:nreps) {
    # every repeating time change the distribution of the card number
    # since after each prisoner’s go, the room is returned exactly to its original state 
    # indicates that the card distribution is the same for each replication
    card_dist <- sample(1:(2*n), size = 2*n, replace = FALSE)
    # Using 'Success' function to check whether the prisoner can find her/his prisoner or not
    # if 'Success' return TRUE, which means s/he succeeded in find her/his number in this run
    # then add 1 to count_success, since s/he succeeded in this replication out of nreps replications
    if (Success(n,card_dist,k,strategy)) count_success <- count_success + 1
  }
  # estimate the probability of a single prisoner (k) succeeding in finding his/her prisoner number
  prob = count_success/nreps
  return (prob)
}


# Function purpose: 
# function 'Pall' is going to estimate the probability of all prisoners finding 
# their number, so that all are released.

# Input: 
# n = half of the number of people (2*n is the total number of prisoners)
# strategy = 1/2/3 (call one of the three strategies to investigate)
# nreps = the number of replicate simulations to run in order to estimate the probability 
#         (10000 is a reasonable default)

# Output: 
# the estimated probability of all prisoners finding their number, so that all are released, 
# according to nreps repeating times under the strategy=1/2/3.

Pall <- function(n, strategy, nreps){
  # initialize the prisoner number and record all prisoners numbers
  prisoners_num <- c(1:(2*n))
  # count the number of the success that a person can find his/her number out of nreps replications
  count_success = 0
  for (i in 1:nreps){
    # every repeat time change the distribution of the card number
    card_dist <- sample(1:(2*n), size = 2*n, replace = FALSE)
    
    # count the number of prisoners who successfully found their number
    success_prisoner <- 0
    # For each prisoner, we check whether s/he can find her/his prisoner number or not
    for (prisoner in prisoners_num){
      # Using 'Success' function to check whether the prisoner can find her/his prisoner or not
      # if 'Success' return TRUE, which means s/he succeeded in find her/his number in this run
      # then add 1 to success_prisoner, since s/he succeeded in this replication out of nreps replications
      if (Success(n,card_dist,prisoner,strategy)) success_prisoner <- success_prisoner + 1
    }
    # if the number of success is equal to the number of prisoner (2n), it means all prisoners 
    # found their numbers in this run (succeed in this replication), we added 1 to count_success
    if (success_prisoner == (2*n)) count_success <- count_success + 1
  }
  prob <- count_success/nreps
  return(prob)
}


#### Function implementation example #### 
# Here we randomly select the prisoner number to be 7
# set the number of replicate simulations to run in order to estimate the probability
# (10000 is a reasonable default)
nreps <- 10000   

## for n= 5
n1 <- 5
## strategy 1
# individual k=7 success probabilities under strategy 1
n5_individualk_prob_1 <- Pone(n1, 7, 1, nreps); n5_individualk_prob_1
# joint success probabilities under strategy 1
n5_joint_success_prob_1 <- Pall(n1, 1, nreps); n5_joint_success_prob_1

## strategy 2
# individual k=7 success probabilities under strategy 2
n5_individualk_prob_2 <- Pone(n1, 7, 2, nreps); n5_individualk_prob_2
# joint success probabilities under strategy 2
n5_joint_success_prob_2 <- Pall(n1, 2, nreps); n5_joint_success_prob_2

## strategy 3
# individual k=7 success probabilities under strategy 3
n5_individualk_prob_3 <- Pone(n1, 7, 3, nreps); n5_individualk_prob_3
# joint success probabilities under strategy 3
n5_joint_success_prob_3 <- Pall(n1, 3, nreps); n5_joint_success_prob_3


## for n= 50
n2 <- 50
## strategy 1
# individual k=7 success probabilities under strategy 1
n50_individualk_prob_1 <- Pone(n2, 7, 1, nreps); n50_individualk_prob_1
# joint success probabilities under strategy 1
n50_joint_success_prob_1 <- Pall(n2, 1, nreps); n50_joint_success_prob_1

## strategy 2
# individual k=7 success probabilities under strategy 2
n50_individualk_prob_2 <- Pone(n2, 7, 2, nreps); n50_individualk_prob_2
# joint success probabilities under strategy 2
n50_joint_success_prob_2 <- Pall(n2, 2, nreps); n50_joint_success_prob_2

## strategy 3
# individual k=7 success probabilities under strategy 3
n50_individualk_prob_3 <- Pone(n2, 7, 3, nreps); n50_individualk_prob_3
# joint success probabilities under strategy 3
n50_joint_success_prob_3 <- Pall(n2, 3, nreps); n50_joint_success_prob_3


############################# surprising results ###############################

# From the results obtained from function "Pone", the estimated probabilities of 
# a single prisoner succeeding in finding their number under three strategies converges 
# to 0.5, 0.4, 0.5 respectively. However, the results from function "Pall" shows that 
# the estimated probabilities of all prisoners finding their numbers are relatively different 
# under these three strategies. Surprisingly, the estimated probability for all prisoner being 
# released is over 30% under strategy 1, which is unexpectedly high compared with the results 
# under another two strategies. Under strategy 3, the result is nearly 0.

# The most possible reason why the prisoners have such high probability to escape under 
# strategy 1 is the prisoners don't have to decide which box he/she opens first, 
# that is, there are more constraints under strategy 1. Another important point is that 
# the success of one prisoner is not independent of the success of the other prisoners in 
# this way, since they all depend on the way the numbers are distributed.

############################# surprising results ###############################


# Function purpose: 
# function 'dloop' is going to estimate the probability distribution of the number of times 
# which is also the loop length (from 1 to 2n), needed for a prisoner to succeed 
# (find her/his card) out of nreps replications.

# Input: 
# n = half of the number of people (2*n is the total number of prisoners)
# nreps = the number of replicate simulations to run in order to estimate the probability 
#         (10000 is a reasonable default)

# Output: 
# the probability distribution of the number of steps (from 1 to 2n) needed to go free.

dloop <- function(n, nreps){
  # initialize the prisoner number and record all prisoners numbers
  prisoners_num <- c(1:(2*n))
  
  # create a list and record the number of replication in which each loop length occurs 
  # at least once out of n replications
  count_l <- rep(0,(2*n))
  
  for (i in 1:nreps){
    # randomly simulate 2n cards with unique number from 1 to 2n, each of them is 
    # randomly placed one in each box.
    cards_dist <- sample(1:(2*n),2*n,replace=FALSE)
    
    # create a list to record the open times until finding their prisoner numbers for 2n prisoners
    open_times_l <- rep(0,(2*n))
    for (prisoner in prisoners_num){
      # store the card numbers in the boxes that all prisoners opened for the first time
      card_number <- cards_dist[prisoner]
      # record the loops/times that a person need to find her/his number
      # suppose s/he has already open one box
      open_times <- 1
      # if the card number in the box that s/he first opened was not her/his prisoner number, 
      # s/he still need to open until s/he finds her/his prisoner number
      while(card_number != prisoner){
        # new and current box numbers that s/he will open next based on the card numbers in the 
        # boxes that prisoners opened before
        box_num <- card_number
        # new and current card numbers that s/he open now based on the new box numbers
        card_number = cards_dist[box_num]
        # add 1 means s/he opens the box one more time
        open_times <- open_times + 1
      }
      # store the corresponding open times for 2n prisoners for each of the replication
      open_times_l[prisoner] <- open_times
    }
    # using unique() since we are asked to estimate the probability of each loop length 
    # from 1 to 2n occurring *at least once* in each replication. That is, say when there 
    # are more than one prisoner going free the first time s/he opened the box, we only 
    # count 1 for this replication, instead of summing all the number of prisoners who success
    # at the first time 
    
    # count the number of replication in which each loop length occurs at least once out of n 
    # replications by each time adding 1
    count_l[unique(open_times_l)] <- count_l[unique(open_times_l)] + 1
  }
  # the probability distribution required can be obtained by just divide *count_l* by *nreps*
  prob_dist <- count_l/nreps
  return(prob_dist)
}


## Provide example code using 'dloop' to estimate the probabilities for n = 50
dloop(50,nreps)


## Assessing the probability that there is no loop longer than 50 in a random 
## reshuffling of cards to boxes using 'dloop'
count <- 0

for (i in 1:nreps) {
  # for each replicating simulation, store counts for each loop length 
  # (from 1 to 2n) if it occurs at least once by adding 1 and 0 otherwise 
  each_repl = dloop(50,1)
  # check whether the there is no loop length longer than 50 (the 51st to the 
  # 100th numbers are 0s in each_repl). If so, add count by 1
  if (sum(each_repl[51:100] == 0) == 50) count <- count + 1
}

# The required probability can be obtained by just dividing count by nreps
prob <- count/nreps
print(prob)

## Visualize the probability distribution of each loop length (1 to 2n), which 
## is also the number of opening times needed for a prisoner to succeed 
barplot(dloop(50,10000),main="Visualization of probability distribution of each loop length",
        xlab='Loop length (1 to 2n)',ylab='Frequency in 10000 times',
        names.arg=c(1:100),col="blue")

############################# surprising results ###############################

# We surprisingly found that the probability that there is no loop longer than 50
# is very similar to the result derived from Pall(50, 10000), that is the 
# probability of all prisoners (100) finding their number. Both of them indicates that
# for 2n prisoners, all of them can be released before n opening times.

############################# surprising results ###############################
