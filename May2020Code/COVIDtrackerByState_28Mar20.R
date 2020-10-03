c <- read.csv("~/Desktop/HarvardDSI/COVID19/Data/Raw/COVIDtrackerRaw_27Mar20V2.csv", header = T, na.strings=c("","NA"))
summary(c)

# Replace NA and blank values with zero 
# Please note that most of these NA values are in Pending and Hospitalized, which we are not analyzing because data are not sufficient 
sum(is.na(c))
c[is.na(c)] <- 0

#Remove rows where total tests is zero 
c <- c[c$Total >0, ]

c$Date <- as.Date(as.character(c$Date), format = '%Y%m%d')

#Create a new column for percent positive cases on a given day
c$PercentPositive <- c$Positive / (c$Total)

####
####
#Subset the dataset to just one state
focal.state <- "NY"
c.state <- c[c$State == focal.state, ]

## Add a column for days since 10 cases
# First, find the first day at which the count surpasses 10 
days.for.10 <- max(which(c.state$Positive > 10)) - 1 
c.state$Timeline <- c(seq(days.for.10, days.for.10 - dim(c.state)[1] + 1 , -1 ) )

#Plot cases on an absolute timeline
plot(c.state$Positive ~ c.state$Date, xlab = "Date", ylab = "Number Positive Cases", pch = 20 )

#Plot cases on a "Days since 10 Cases" timeline
plot(c.state$Positive ~ c.state$Timeline, xlab = "Days Since 10 Cases", ylab = "Number Positive Cases", pch = 20 )

####
####
## If desiring to compare trajectories between states, enter a vector of states
state.vec <- c("IL", "NY", "MA", "WA", "WI", "CA")
# Pick starting case value
start.cases <- 5

#Create limits for plot based on values on selected states
plot.ylim.raw <- max(c$Positive[c$State %in% state.vec])

for(i in 1:length(state.vec)){
  current.state <- state.vec[i]
  
  state.mat <- c[c$State == current.state, ]
  d10 <- max(which(state.mat$Positive > start.cases)) - 1 
  state.mat$Timeline <- c(seq(d10, d10 - dim(state.mat)[1] + 1 , -1 ) )

  
  if(i == 1){
    plot(state.mat$Positive ~ state.mat$Timeline, xlab = "Days Since 10 Cases", ylab = "Number Positive Cases", pch = 20, xlim = c(0, 25), ylim = c(5, plot.ylim.raw), type = "l", log = "y" )
  } else {
    points(state.mat$Positive ~ state.mat$Timeline, type = "l", pch = 20, col = i)
  }
  
}

legend("topleft", bty = "n", col = seq(1, length(state.vec), 1), legend = state.vec, lwd = 2)


####
####
# Repeat for percent positive
state.vec <- c("IL", "NY", "MA", "WA", "WI", "CA")

#Create limits for plot based on values on selected states
plot.ylim.percent <- max(c$PercentPositive[c$State %in% state.vec])

for(i in 1:length(state.vec)){
  current.state <- state.vec[i]
  
  state.mat <- c[c$State == current.state, ]
  d10 <- max(which(state.mat$Positive > start.cases)) - 1 
  state.mat$Timeline <- c(seq(d10, d10 - dim(state.mat)[1] + 1 , -1 ) )
  
  
  if(i == 1){
    plot(state.mat$PercentPositive ~ state.mat$Timeline, xlab = "Days Since 10 Cases", ylab = "Percent Positive Cases", pch = 20, xlim = c(0, 25), ylim = c(0, plot.ylim.percent), type = "l" )
  } else {
    points(state.mat$PercentPositive ~ state.mat$Timeline, type = "l", pch = 20, col = i)
  }
  
}

legend("topright", bty = "n", col = seq(1, length(state.vec), 1), legend = state.vec, lwd = 2)

######
######
######
######
## Make a new dataset with each state standardized to the same timeline 
cases.start <- 5 

## Desired predictor variables:
# cumulative positive cases, number of new positive cases, percent positive cases, percent increase in positive cases since previour day 
# number of new tests, percent increase in number of new tests since previous day 

for(j in 1:length(unique(c$State))){
  
  pres.state <- unique(c$State)[j]
  
  temp.mat <- c[c$State == pres.state, ]
  d10 <- max(which(temp.mat$Positive > cases.start)) - 1 
  temp.mat$Timeline <- c(seq(d10, d10 - dim(temp.mat)[1] + 1 , -1 ) )
  
  # Add in columns for additional response variables 
  temp.mat$NewCases <- c(temp.mat$Positive[-length(temp.mat$Positive)] - temp.mat$Positive[-1] , 0 )
  temp.mat$NewTests <- c(temp.mat$Total[-length(temp.mat$Positive)] - temp.mat$Total[-1] , 0 )
  ## Note that due to data irregularities, some test numbers go DOWN -- maybe set all negative test results to zero?
  temp.mat$NewTests[temp.mat$NewTests < 0] <- 0
  
  temp.mat$FractCaseIncrease <- c(temp.mat$Positive[-length(temp.mat$NewCases)] / temp.mat$Positive[-1], NaN)
  temp.mat$FractPosIncrease <- c(temp.mat$NewCases[-length(temp.mat$NewCases)] / temp.mat$Positive[-1], NaN)
  temp.mat$FractTestIncrease <- c(temp.mat$NewTests[-length(temp.mat$NewTests)] / temp.mat$Total[-1], NaN)
  ## Create variable for day over day percent increase in cases 
  
  if(j == 1){
    combine.mat <- temp.mat
  } else {
    combine.mat <- rbind(combine.mat, temp.mat)
  }
  
}

## Make side by side plots of new cases per day and new tests per day 

par(mfrow = c(1, 2))

## Make plots of increase in cases over time 
plot.vec <- c("IL", "NY", "MA", "WA", "WI", "CA")

ylim.new.cases <- range(combine.mat$NewCases[combine.mat$State %in% plot.vec]) + 1

plot(0, 0, col = "white", xlim = c(0, 25), ylim = ylim.new.cases, log = "y", xlab = "Days Since 5 Cases", ylab = "New Daily Cases")
for(state in 1:length(plot.vec)){
  points(combine.mat$NewCases[combine.mat$State == plot.vec[state]] ~ combine.mat$Timeline[combine.mat$State == plot.vec[state]], type = "l", col = state)
}

legend("bottomright", bty = "n", col = seq(1, length(plot.vec), 1), legend = plot.vec, lwd = 2, seg.len = .5, x.intersp = .6)

# Problem in the new cases dataset is that some states did not report new cases on some days, leading to discontinuities in data. Just drop these data points where no new tests or cases were reported? 

## Make plot of testing capacity 
ylim.new.tests <- range(combine.mat$NewTests[combine.mat$State %in% plot.vec]) + 1

plot(0, 0, col = "white", xlim = c(0, 25), ylim = ylim.new.tests, log = "y", xlab = "Days Since 5 Cases", ylab = "New Daily Tests")
for(state in 1:length(plot.vec)){
  points(combine.mat$NewTests[combine.mat$State == plot.vec[state]] ~ combine.mat$Timeline[combine.mat$State == plot.vec[state]], type = "l", col = state)
}

#legend("topleft", bty = "n", col = seq(1, length(plot.vec), 1), legend = plot.vec, lwd = 2)

