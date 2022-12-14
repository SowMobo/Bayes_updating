####################################################
# studioBayes-solutions.r

# Solutions to R questions from Studio 5.
#
# The solutions require small modifications of the code from studioBayes.r
# What we did was copy that code and put it into a function so we 
# don't need a separate copy for each question. The arguments of the function
# are all the things we might want to change. If you look at the function
# code you should understand the syntax.

#------------------------------------------------
# Turn the StudioBayes.r code into a function 
studioBayesFunction = function(dice, prior, likelihoodTable, dataRolls, 
                               showIntermediateResults = TRUE,
                               isCensoredData = FALSE){
  
  # This code does Bayesian updating for the probability a certain die was 
  # chosen from the list of dice.
  # dice = list of dice (hypotheses)
  # prior = prior probabilities on the hypotheses
  # likelihoodTable -- each row is the probability table for a hypothesis
  # dataRolls -- random data for rolling a randomly chosen die
  # showIntermediateResults is a boolean to print tables and draw barplots of 
  #   intermediate posteriors, it defaults to TRUE. Setting it to FALSE can 
  #   significantly speed up the code
  # isCensoredData is a boolean used for problem 3. It says the data has been   
  # 'censored'
  # RED_FLAG: we don't check compatibility between the arguments
  # 1. length(dice) == length(prior) == number of rows of likelihoodTable
  # 2. Allowable data = 1 to number of columns of  likelihoodTable
  
  nrolls = length(dataRolls)
  
  # It's goog practice to just plot data
  plot(dataRolls, xlab="Roll index", main = "Sample for idd rolls")
  
  if(showIntermediateResults)
  {
    title = "Prior probabilities"
    barplot(prior, col = 'orange', width = rep(.1, 5), xlim = c(0, 5), space = 3,
            names = dice, main = title)
  }
  
  # Create data frame to hold our computations
  bayesTable = data.frame(
    dice = dice,
    prior = NA,
    likelihood = NA,
    posterior.unnormalized = NA,
    posterior.normalized = NA
  )
  
  # Initialize matrix whose jth column will store 
  # the posterior distribution after updating the jth roll
  # We will use this to make a nifty stacked bar plot at the end
  posteriorMat = matrix(NA, nrow = length(dice), ncol = nrolls)
  
  # Set the first prior
  prior.jroll = prior
  
  # Go throught the updating process for each roll
  for (jroll in 1:nrolls) {
    x.jroll = dataRolls[jroll]
    likelihoodColumn = x.jroll # Normally the likelihood column number is the roll
    
    if(isCensoredData){
      # Censored data = 0  corresponds to column 1 in the likelihood table
      # Censored data =  1 corresponds to column 2 in the likelihood table
      likelihoodColumn = x.jroll + 1
    }
    
    # Bayes updating becomes here
    likelihood.jroll = likelihoodTable[, likelihoodColumn]
    unnormalizedPosterior.jroll = likelihood.jroll * prior.jroll
    posterior.jroll = unnormalizedPosterior.jroll/sum(unnormalizedPosterior.jroll)
    
    # Store the posterior
    posteriorMat[,jroll] = posterior.jroll
    
    # put the table in our data frmae for easy plot
    bayesTable['prior'] = prior.jroll
    bayesTable['likelihood'] = likelihood.jroll
    bayesTable['posterior.unnormalized'] = unnormalizedPosterior.jroll
    bayesTable['posterior.normalized'] = posterior.jroll
    
    # Display intermediates results
    if(showIntermediateResults){
      # Plot the posterior 
      title = paste("Posterior probalilities after roll, roll: ", x.jroll)
      barplot(prior.jroll, col = 'orange', width = rep(.1, 5), xlim = c(0, 5), space = 3,
              names = dice, main = title)
      
      # Display Bayes table for roll
      title = paste("Bayes table after one roll ",jroll, " roll: ", x.jroll)
      print(title)
      print(bayesTable)
    }
    
    # Set Prior for the next updating
    prior.jroll = posterior.jroll
    
  }
  
  # Stacked barplot of the prior/posterior distribution
  # as a function of number of roll
  # cbind()---column is a easy way to add column to a matrix
  allProbs = cbind(prior, posteriorMat)
  barplot(allProbs, legend.text = paste("Sides", dice, sep = ""), 
          names.arg = c("0(Prior)", c(1:nrolls)), col = rainbow(length(dice)), border = NA)
  title(xlab = "Number of roll")
  title(main = "Stacked Barplot of Posterior(Prior) Probabilities")
  #Sub-bar heights equal posterior probabilities
}

#-----------------------------------------------
# Let define our standard dice and likelihoodTable
dice = c(4, 6, 8, 12, 20)
# Build likelihood table 5 dices(rows) x 20 possible outcomes (columns)
v = rep(0, 100)
standardLikelihoodTable = matrix(v, nrow = 5, ncol = 20)
# Make probabilities(row) for each dice  separately
standardLikelihoodTable[1, 1:4] = 1/4 # 4-sided dice, 5-20 outcomes are all 0
standardLikelihoodTable[2, 1:6] = 1/6 # 6-sided dice, 7-20 outcomes are all 0
standardLikelihoodTable[8, 1:8] = 1/8 # 8-sided dice, 9-20 outcomes are all 0
standardLikelihoodTable[1, 1:12] = 1/12 # 12-sided dice, 13-20 outcomes are all 0
standardLikelihoodTable[1, 1:20] = 1/20 # 20-sided dice
print(standardLikelihoodTable, digits = 3)

#----------------------------------------------------------
# Probl2a: Go through the original example form studio5.r
nrolls = 20 # roll the dice nrolls times
prior = c(0.2,0.2, 0.2, 0.2, 0.2)

# Choose the dice according the prior distribution
set.seed(1)
randomDie = sample(dice, 1, prob = prior)
toPrint = paste("We pick out at random from the drawer the dice : ", randomDie)
print(toPrint)
# generate data by rolling chosen dice
dataRolls = sample(1:randomDie, size = nrolls, replace = TRUE)
# Call studioBayesFunction for Bayes updating and display
studioBayesFunction(dice = dice, prior = prior, likelihoodTable = standardLikelihoodTable,
                    dataRolls = dataRolls, showIntermediateResults = TRUE, isCensoredData = FALSE)

# Probl2b : Use the same data as above and redo using a new prior
prior = c(.05, .05, .05, .05, .8) # New prior with high confidence putting on 20-sized
studioBayesFunction(dice = dice, prior = prior, likelihoodTable = standardLikelihoodTable, 
                    dataRolls = dataRolls, showIntermediateResults = TRUE, isCensoredData = FALSE)


# Prob 2c: Run 5 times the simulation, we see that the 20-seided dice is likely to be chosen from
# the drawer. Both cases converge to the same conclusion but the cas 2 converge quickly due to prior
# probabiltie biased toward 20-sided dice

#Prob 2b: Redo the simulation generating data from 20-sided dice
randomDie = 20
prior = c(0.2,0.2, 0.2, 0.2, 0.2)
set.seed(1)
dataRolls = sample(1:randomDie, size = nrolls, replace = TRUE)
studioBayesFunction(dice = dice, prior = prior, likelihoodTable = standardLikelihoodTable,
                    dataRolls = dataRolls, showIntermediateResults = TRUE, isCensoredData = FALSE)

# 3 - Censored data
dice = c(4, 6, 8, 12, 20)
#Build a likelihoods tabele of 5 dice x 2 possibles outcomes
# Column 1 is for censored data = 0 and column 2 is for censored data = 1
# To select a column we have to add a 1 to the data value
# e.g if data = 0 we select column 1
v = rep(0, 10)
censoredLikelihoodTable = matrix(v, nrow = 5, ncol = 2)
censoredLikelihoodTable[1,] = c(3/4, 1/4) # 4-sided die
censoredLikelihoodTable[2,] = c(5/6, 1/6) # 6-sided die
censoredLikelihoodTable[3,] = c(7/8, 1/8) # 8-sided die
censoredLikelihoodTable[4,] = c(11/12, 1/12) # 12-sided die
censoredLikelihoodTable[5,] = c(19/20, 1/20) # 20-sided die
print(censoredLikelihoodTable, digits = 3)

nrolls = 20 # roll the die nrolls times
prior = c(0.2,0.2, 0.2, 0.2, 0.2)

# Choose die according to prior distribution
set.seed(1)
randomDie = sample(dice, 1, prob = prior)

# Generate data
dataRolls = sample(1:randomDie, size = nrolls, replace = TRUE)

# Censor data
censoredDataRolls = (dataRolls == 13) 
# We set showIntermediateResults to FALSE to speed up the code
studioBayesFunction(dice, prior, censoredLikelihoodTable, censoredDataRolls,
                    showIntermediateResults = TRUE, isCensoredData = TRUE)

# Redo with nrolls  =200
nrolls = 2000
prior = c(0.2,0.2, 0.2, 0.2, 0.2)
# Choose die according to prior distribution
set.seed(1)
randomDie = sample(dice, 1, prob = prior)
# Generate data
dataRolls = sample(1:randomDie, size = nrolls, replace = TRUE)

#Censor data
censoredDataRolls = (dataRolls == 1)
studioBayesFunction(dice, prior, censoredLikelihoodTable, censoredDataRolls,
                    showIntermediateResults = FALSE, isCensoredData = TRUE)

#---------------------------------------
# Conclusion
# The simulation shows that we need at least 1300 roll for the posterior probability 
# distribution to converge.
# The simulation shows that 8-sided dice is likely to be pick out from the dice drawer.


