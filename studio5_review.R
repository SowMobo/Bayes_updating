# K-sided dice 
# Random process : choose among K-sided Dies at random and roll the die
# Construct Bayesian analysis inferinsing the chosen die type using outcomes of sample rolls

#---------------------------
# Updating for one roll
dice = c(4, 6, 8, 12, 20)
prior = c(.2, .2, .2, .2, .2)

# Build likelihood table 5 dice x 20 possible outcomes
v = rep(0, 100)
likelihoodTable = matrix(v, nrow = 5, ncol = 20)
# Make the probabilities (rows) for each die separatly
likelihoodTable[1, 1:4] = 1/4   # 4-sided die : outcomes 5-20 are all 0
likelihoodTable[2, 1:6] = 1/6   # 6-sided die : outcomes 7-20 are all 0
likelihoodTable[3, 1:8] = 1/8   # 8-sided die : outcomes 9-20 are all 0
likelihoodTable[4, 1:12] = 1/12 # 12-sided die : outcomes 13-20 are all 0
likelihoodTable[5, 1:20] = 1/20   # 20-sided die 
print(likelihoodTable, digits=3)

title = 'prior probabilities'
barplot(prior, col = 'blue', width = rep(.1, 5), xlim = c(0, 5), space = 3, names = dice, main = title)

# Choose die according to prior distribution
set.seed(1)
randomDie = sample(dice, 1, prob = prior)
# Roll the die once
x = sample(1:randomDie, size = 1, replace = TRUE)
print(x)

# Choose likelihood colunm:
likelihood = likelihoodTable[,x]
print(likelihood)

# Bayesian updating : posterior computing
unnormalizedPosterior = likelihood * prior
posterior = unnormalizedPosterior/(sum(unnormalizedPosterior))
print(posterior)

title = paste("Posterior probabilities after one roll, roll = ", x)
barplot(posterior, col = 'orange', width = rep(.1, 5), xlim = c(0, 5), space = 5, names = dice,
        main  = title)

#--------------------------------------------
# Storing the entire table in a data frame
# Put the Bayes table into a data frame for easy priting
# Data frame has named column, the syntax is name = values
# So dice = dice create a culumn named 'dice' with values from the dice vector
# Likewise, someothername = dice create a column named someothername with values drawn from dice vector
bayesTable = data.frame(
  dice = dice,
  prior = prior,
  likelihood = likelihood,
  posterior.unnormalized = unnormalizedPosterior,
  posterior.normalized = posterior
)

title = paste('Bayes table after one roll, roll = ', x)
print(title)
print(bayesTable)

# ----------------------------------------------------
# Roll dice repeatedly
# Just to keep this part self containing, we repeate the setup above
nrolls = 20 # Roll the chosen dice nrolls times
dice = c(4, 6, 8, 12, 20)
prior = rep(0.2, 5)
# Build likelihood table 5 dice x 20 possibles outcomes
v = rep(0, 100)
likelihoodTable = matrix(v, nrow = 5, ncol = 20)
likelihoodTable[1, 1:4] = 1/4  # 4-sided dice, outcomes 5-20 are all zeros
likelihoodTable[2, 1:6] = 1/6  # 6-sided dice, outcomes 7-20 are all zeros
likelihoodTable[3, 1:8] = 1/8  # 8-sided dice, outcomes 9-20 are all zeros
likelihoodTable[4, 1:12] = 1/12  # 12-sided dice, outcomes 13-20 are all zeros
likelihoodTable[5, 1:20] = 1/4  # 20-sided dice

# Choose a die according to prior distribution
#set.seed(1)
randomDie = sample(dice, 1, prob = prior)
print(randomDie)
# Get all the data at once
dataRolls = sample(1:randomDie, size = nrolls, replace = TRUE)
# It's good practice to just plot data to look for obvious problems
plot(dataRolls, xlab = 'Roll index', main = 'Sample of iid rolls')

# Plot the prior
title = "Prior probabilities"
barplot(prior, col = 'red', width = rep(.1, 5), space = 3, xlim = c(0, 5), 
        names = dice, main = title)

# Create a data frame to hold our computations
bayesTable = data.frame(
  dice = dice,
  prior = NA,
  likelihood = NA,
  posterior.unnormalized = NA,
  posterior.normalized =  NA
)

# Initialize a matrix whose jth column will store 
# the posterior distribution after updating the jth roll
# We will use this to make a nifty stacked bar plot at the end
posteriorMat = matrix(NA, nrow = length(dice), ncol = nrolls)
# Set
prior.jroll = prior
# Go through the updata process for each roll
for (jroll in 1:nrolls)
{
  x.jroll = dataRolls[jroll]
  
  likelihood.jroll = likelihoodTable[, x.jroll]
  unnormalizedPosterior.jroll = prior.jroll * likelihood.jroll
  posterior.jroll = unnormalizedPosterior.jroll/sum(unnormalizedPosterior.jroll)
  
  # Store the posterior
  posteriorMat[, jroll] = posterior.jroll
  
  # Plot the posterior
  title = paste('Posterior probabilities after roll: roll, ', x.jroll)
  barplot(posterior.jroll, col = rgb(0.3, 0.5, 0.8), width = rep(.1, 5), xlim= c(0, 5), space = 3,
          names= dice, main = title)
  
  # Store data into data frame for easy printing
  bayesTable['prior'] = prior.jroll
  bayesTable['likelihood'] = likelihood.jroll
  bayesTable['posterior.unnormalized'] = unnormalizedPosterior.jroll
  bayesTable['posterior.normalized'] = posterior.jroll
  
  title = paste("Bayes updating after one roll ", jroll, " rool:", x.jroll)
  print(title)
  print(bayesTable)
  
  # Update prior for the next roll
  prior.jroll = posterior.jroll
}

#Stacked barplot of prior/posterior distributions
# as a function of number of rolls
# (cbind--column bind is a easy way to add column to a matrix)
allProbs = cbind(prior, posteriorMat)
barplot(allProbs, legend.text = paste("Sides", dice, sep = ""), names.arg = c("0(Prior)", c(1:nrolls)),
col = rainbow(length(dice)))

title(xlab = "Number of Rolls")
title(main = "Stacked plot of posterior(prior) probabilities distributions")
#sub-bar heights represent posterior probabilities 

