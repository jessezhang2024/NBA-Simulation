#Update 9.24 whole code.
#when VAN model
library(readxl)
library(readr)
library(stats)
library(dplyr)
library(ggplot2)
# 1
# SET THE FUNCTHION
DesiredFormatNBA = function(df){
  # Convert the NBA Data as shown in image to n x 6 matrix where the columns are mentioned like below.
  BT <- data.frame(matrix(ncol=6,nrow=nrow(df), dimnames=list(NULL, c("Team A", "Team B", "Team A win", "Team B win", "Team A home", "Team B home"))))
  my_range <- 1:nrow(df)
  for(i in my_range){
    BT[i,1] <- df[i,3]
    BT[i,2] <- df[i,5]
  }

  for (i in my_range){
    BT[i,3] <- ifelse(df[i,4] > df[i,6], 1, 0)
    BT[i,4] <- ifelse(df[i,6] > df[i,4], 1, 0)

    BT[i,5] <- ifelse(BT[i,1] == df[i,5], 1, 0)
    BT[i,6] <- ifelse(BT[i,2] == df[i,5], 1, 0)
  }
  return(BT)
}

Data2Mat = function(df){
  # Data frame should have the columns 'Team.A', 'Team.B', 'Team.A.win', 'Team.B.win', 'Team.A.Home', 'Team.B.Home', as the desired format
  # This function then returns the h, w, nw, l and nl matrices required for our BT package.
  # In the NBA data, Team B was always the home team. Hence, essentially, 'Team.A.Home' is a column of 0s.

  Teams = unique(df$Team.B)

  df[,3] = as.numeric(df[,3])
  df[,4] = as.numeric(df[,4])
  df[,6] = as.numeric(df[,6])

  w = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))
  l = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))

  nw = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))
  nl = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))

  for(i in 1:nrow(df)){
    if (df[i,6] == 1){
      row = df[i,]
      w[row$Team.B, row$Team.A] = w[row$Team.B, row$Team.A] + (row$Team.B.win == 1)
      l[row$Team.B, row$Team.A] = l[row$Team.B, row$Team.A] + (row$Team.A.win == 1)
    }
  }
  for(i in 1:nrow(df)){
    if (df[i,6] == 0){
      row = df[i,]
      nw[row$Team.B, row$Team.A]  = nw[row$Team.B, row$Team.A] + (row$Team.B.win == 1)
      nw[row$Team.A, row$Team.B]  = nw[row$Team.A, row$Team.B] + (row$Team.A.win == 1)
      nl[row$Team.A, row$Team.B]  = nl[row$Team.A, row$Team.B] + (row$Team.B.win == 1)
      nl[row$Team.B, row$Team.A]  = nl[row$Team.B, row$Team.A] + (row$Team.A.win == 1)
    }
  }

  h = w + l
  return(list(h=h, w=w, l=l, nw = nw, nl = nl, Teams = Teams))
}

# This function returns the number of parameters for a specific type of model
ntheta = function(mat, model_number, R = NULL){
  if(model_number == 1){return(length(mat$Teams))}
  if(model_number == 2){return(length(mat$Teams) + 1)}
  if(model_number == 3){return(length(mat$Teams) + max(R))}
  if(model_number == 4){return(2*length(mat$Teams))}
  if(model_number == 5){return((1 + max(R))*length(mat$Teams))}
  if(model_number == 6){return(length(mat$Teams)*(length(mat$Teams) + 1))}
}
NBA_R = function(fixtures){
  data = fixtures
  Teams = unique(fixtures$Team.B)

  Atlantic = c('Boston Celtics', 'Brooklyn Nets', 'New York Knicks', 'Philadelphia 76ers', 'Toronto Raptors')
  Central = c('Chicago Bulls', 'Cleveland Cavaliers', 'Detroit Pistons', 'Indiana Pacers', 'Milwaukee Bucks')
  Southeast = c('Atlanta Hawks', 'Charlotte Hornets', 'Miami Heat', 'Orlando Magic', 'Washington Wizards')
  Northwest = c('Denver Nuggets', 'Minnesota Timberwolves', 'Oklahoma City Thunder', 'Portland Trail Blazers', 'Utah Jazz')
  Pacific = c('Golden State Warriors', 'Los Angeles Clippers', 'Los Angeles Lakers', 'Phoenix Suns','Sacramento Kings')
  Southwest = c('Dallas Mavericks', 'Houston Rockets','Memphis Grizzlies','New Orleans Pelicans','San Antonio Spurs')

  D = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))
  C = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))
  L = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))
  R = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))

  East = data.frame(Atlantic, Southeast, Central)
  West = data.frame(Southwest, Pacific, Northwest)
  NBA = data.frame(East, West)

  # Basic designation of relationship
  for(i in Teams){
    for(j in Teams){
      if(i != j){
        L[i, j] = 1
      }
    }
  }

  # Designating Conferences
  for(con1 in 1:ncol(East)){
    for(con2 in 1:ncol(East)){
      for(i in 1:nrow(East[con1])){
        for(j in 1:nrow(East[con2])){
          if(East[i, con1] != East[j, con2]){
            C[East[i, con1], East[j, con2]] = 1
          }
        }
      }
    }
  }

  for(con1 in 1:ncol(West)){
    for(con2 in 1:ncol(West)){
      for(i in 1:nrow(West[con1])){
        for(j in 1:nrow(West[con2])){
          if(West[i, con1] != West[j, con2]){
            C[West[i, con1], West[j, con2]] = 1
          }
        }
      }
    }
  }

  # Designating Divisions
  for(div in 1:ncol(NBA)){
    for(i in 1:nrow(NBA[div])){
      for(j in 1:nrow(NBA[div])){
        if(NBA[i, div] != NBA[j, div])
          D[NBA[i, div], NBA[j, div]] = 1
      }
    }
  }

  R = C + D + L
  return(R)
}

#R <- NBA_R(fixtures)

Log_Like1 = function(theta, mat){
  # mat contains mat$h, mat$w, mat$l, mat$nw, mat$nl
  # theta is the vector of thetas (skill levels)
  Teams = colnames(mat$h)

  log_like1 = 0

  for(i in 1:length(Teams)){
    for(j in 1:length(Teams)){
      log_like1 = log_like1 + (mat$w[i, j] + mat$l[j, i] + mat$nw[i, j])*(theta[i] - log(exp(theta[i]) + exp(theta[j])))
    }
  }
  return(-log_like1)
}

# Defining a gradient for the likelihood function
Log_Like_deriv1 = function(theta, mat){
  Teams = colnames(mat$h)
  output = rep(0,ntheta(mat, 1, R))
  for(i in 1:length(Teams)){
    log_like_deriv1 = 0
    for(j in 1:length(Teams)){
      log_like_deriv1 = log_like_deriv1 - (mat$h[i,j] + mat$h[j,i] + mat$nw[i,j] + mat$nw[j,i])*((exp(theta[i]))/(exp(theta[i]) + exp(theta[j])))
    }
    output[i] = sum(mat$w[i,]) + sum(mat$l[,i]) + sum(mat$nw[i,]) + log_like_deriv1
  }
  return(-1*output)
}

PCT_Table <- function(matrix){
  #Returns the wins and losses of the teams, along with their winning percentage
  Wins <- NA
  Losses <- NA
  Teams <- matrix$Teams
  WL <- data.frame(Teams, Wins, Losses)
  for (i in 1:nrow(WL)){
    WL[i,2] <- sum(matrix$w[i,] + matrix$l[,i] + matrix$nw[i,])
    WL[i,3] <- sum(matrix$w[,i] + matrix$l[i,] + matrix$nl[i,])
  }
  WL$PCT <- WL$Wins/(WL$Losses + WL$Wins)
  WL <- WL[order(-WL$PCT),]
  return(WL)
}

Home_Table <- function(matrix){
  #Returns the home wins and losses of the teams, along with their home winning percentage
  Home_Wins <- NA
  Home_Losses <- NA
  Teams <- matrix$Teams
  WL <- data.frame(Teams, Home_Wins, Home_Losses)
  for (i in 1:nrow(WL)){
    WL[i,2] <- sum(matrix$w[i,])
    WL[i,3] <- sum(matrix$l[i,])
  }
  WL$PCT <- WL$Home_Wins/(WL$Home_Losses + WL$Home_Wins)
  WL <- WL[order(-WL$PCT),]
  return(WL)
}
BT_Model = function(matrix, Model_type, R = NULL){
  # Returns the 'thetas' with the least parameter set to 0 for the specified Bradley-Terry model
  # The model type has to be specified in this manner
  # VAN - Vanilla, CHA - Common Homeground advantage, CHI - Common Hierarchical Model, TSH - Team Specific Homeground,
  # HIE - Hierarchical, PAI - Pairwise Homeground

  teams = matrix$Teams

  if(Model_type == 'VAN'){
    theta = rep(0, ntheta(matrix, 1)) # Optimization starting from 0
    Model = optim(par = theta, fn = Log_Like1, gr = Log_Like_deriv1, mat = matrix, method = 'BFGS') # Fitting Model
    least = teams[which.min(Model$par)]
    best = teams[which.max(Model$par)]

    Theta = Model$par - Model$par[which(teams == least)] # Re-basing so that the least ranked team has theta = 0
    Table = data.frame(teams, Theta)
    return(list(Table = Table, type = Model_type, Worst = least, Teams = teams))
  }

  if(Model_type == 'CHA'){
    theta = rep(0,ntheta(matrix, 2))
    Model = optim(par = theta, fn = Log_Like2, gr = Log_Like_deriv2, mat = matrix, method = 'BFGS')

    least = teams[which.min(Model$par[1:length(teams)])]
    best = teams[which.max(Model$par[1:length(teams)])]

    theta = Model$par[1:length(teams)] - Model$par[which(teams == least)]
    alpha = Model$par[(1+length(teams))]

    Table = data.frame(teams, Theta = theta)
    return(list(Table = Table, Alpha = alpha, type = Model_type, Worst = least, Best = best, Teams = teams))
  }

  if(Model_type == 'CHI'){
    advantages = c("Level 1", "Level 2", "Level 3")
    theta = rep(0, ntheta(matrix, 3, R))
    Model = optim(par = theta, fn = Log_Like3, gr = Log_Like_deriv3, mat = matrix, R = R, method = 'BFGS')

    least = teams[which.min(Model$par[1:length(teams)])]
    bigger_advantage = advantages[which.max(Model$par[(length(teams) + 1):(length(teams) + max(R))])]

    theta = Model$par[1:length(teams)] - Model$par[which(teams == least)]
    alpha = Model$par[(length(teams) + 1):(length(teams) + max(R))]

    Table = (data.frame(Theta = theta, row.names = teams))
    Alpha = data.frame(Alpha = alpha, row.names = 1:max(R))

    return(list(Table = Table, Alpha = Alpha, type = Model_type, Advantage = bigger_advantage, Teams = teams))
  }

  if(Model_type == 'TSH'){
    theta = rep(0,ntheta(matrix, 4))
    Model = optim(par = theta, fn = Log_Like4, gr = Log_Like_deriv4, mat = matrix, method = 'BFGS')

    least = teams[which.min(Model$par[1:length(teams)])]
    bigger_advantage = teams[which.max(Model$par[(length(teams) + 1):(2*length(teams))])]

    theta = Model$par[1:length(teams)] - Model$par[which(teams == least)]
    alpha = Model$par[(length(teams) + 1):(2*length(teams))]

    Table = data.frame(Theta = theta, row.names = teams)
    Alpha = data.frame(Alpha = alpha, row.names = teams)

    return(list(Table = Table, Alpha = Alpha, type = Model_type, Highest_Home_Advantage = bigger_advantage, Teams = teams))
  }

  if(Model_type == 'HIE'){
    theta = rep(0,ntheta(matrix, 5, R))
    Model = optim(par = theta, fn = Log_Like5, gr = Log_Like_deriv5, mat = matrix, R = R, method = 'BFGS')

    least = teams[which.min(Model$par[1:length(teams)])]

    theta = Model$par[1:length(teams)] - Model$par[which(teams == least)]
    Table = data.frame(theta, row.names = teams)
    Alpha = NA
    level = 1
    while(level <= max(R)){
      Level = Model$par[(level*length(teams) + 1) : ((level+1)*length(teams))]
      Alpha = data.frame(Alpha, Level, row.names = teams)
      level = level + 1
    }
    return(list(Table = Table,
                Alpha = Alpha[,-1], type = Model_type, Teams = teams))
  }

  if(Model_type == 'PAI'){
    theta = rep(0,ntheta(matrix, 6))
    Model = optim(par = theta, fn = Log_Like6, gr = Log_Like_deriv6, mat = matrix, method = 'BFGS')

    least = teams[which.min(Model$par[1:length(teams)])]

    Model$par[1:length(teams)] = Model$par[1:length(teams)] - Model$par[which(teams == least)]
    return(list(par = Model$par, teams = teams, type = Model_type))
  }

}

# Obtains the model number based on the model specification for the BT_test() function
ModelNumber = function(model){
  if (model == 'VAN'){
    return(1)
  } else if (model == 'CHA'){
    return(2)
  } else if (model == 'CHI'){
    return(3)
  } else if (model == 'TSH'){
    return(4)
  } else if (model == 'HIE'){
    return(5)
  } else if (model == 'PAI'){
    return(6)
  } else {
    return("Check the specification")
  }
}

BT_test = function(model1, model2, matrix, R = NULL){
  # Function for performing the hypothesis tests outlined in the thesis
  # Function fits the thetas while performing the hypothesis tests. Hence, this could also be used a precursor before obtaining theta values

  m1 = model1; m2 = model2
  if(ModelNumber(model1) > ModelNumber(model2)){
    m1 = model2; m2 = model1
  }
  if(ModelNumber(model1) == ModelNumber(model2)){
    return("Same order")
  }
  if(ModelNumber(m1) == 1 & ModelNumber(m2) == 2){
    stat = -2*(optim(par = rep(0,ntheta(matrix,2)), fn = Log_Like2, gr = Log_Like_deriv2, method = "BFGS", mat = matrix)$value -
                 optim(par = rep(0,ntheta(matrix,1)), fn = Log_Like1, gr = Log_Like_deriv1, method = "BFGS", mat = matrix)$value)
    cat('P-Value is ', 1 - pchisq(stat, 1), '\n')
    cat('with a statistic of ', stat, 'and 1 degree of freedom')
  }
  if(ModelNumber(m1) == 1 & ModelNumber(m2) == 3){
    stat = -2*(optim(par = rep(0,ntheta(matrix,3,R)), fn = Log_Like3, gr = Log_Like_deriv3, method = "BFGS", mat = matrix, R = R)$value -
                 optim(par = rep(0,ntheta(matrix,1)), fn = Log_Like1, gr = Log_Like_deriv1, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix,3,R) - ntheta(matrix,1)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 1 & ModelNumber(m2) == 4){
    stat = -2*(optim(par = rep(0,ntheta(matrix,4)), fn = Log_Like4, gr = Log_Like_deriv4, method = "BFGS", mat = matrix)$value -
                 optim(par = rep(0,ntheta(matrix,1)), fn = Log_Like1, gr = Log_Like_deriv1, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix, 4) - ntheta(matrix,1)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 1 & ModelNumber(m2) == 5){
    stat = -2*(optim(par = rep(0,ntheta(matrix,5,R)), fn = Log_Like5, gr = Log_Like_deriv5, method = "BFGS", mat = matrix, R = R)$value -
                 optim(par = rep(0,ntheta(matrix,1)), fn = Log_Like1, gr = Log_Like_deriv1, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix,5,R) - ntheta(matrix,1)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 1 & ModelNumber(m2) == 6){
    stat = -2*(optim(par = rep(0,ntheta(matrix,6)), fn = Log_Like6, gr = Log_Like_deriv6, method = "BFGS", mat = matrix)$value -
                 optim(par = rep(0,ntheta(matrix,1)), fn = Log_Like1, gr = Log_Like_deriv1, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix,6) - ntheta(matrix,1)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 2 & ModelNumber(m2) == 3){
    stat = -2*(optim(par = rep(0,ntheta(matrix,3,R)), fn = Log_Like3, gr = Log_Like_deriv3, method = "BFGS", mat = matrix, R = R)$value -
                 optim(par = rep(0,ntheta(matrix,2)), fn = Log_Like2, gr = Log_Like_deriv2, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix,3,R) - ntheta(matrix,2)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 2 & ModelNumber(m2) == 4){
    stat = -2*(optim(par = rep(0,ntheta(matrix,4)), fn = Log_Like4, gr = Log_Like_deriv4, method = "BFGS", mat = matrix)$value -
                 optim(par = rep(0,ntheta(matrix,2)), fn = Log_Like2, gr = Log_Like_deriv2, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix,4) - ntheta(matrix,2)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 2 & ModelNumber(m2) == 5){
    stat = -2*(optim(par = rep(0,ntheta(matrix,5,R)), fn = Log_Like5, gr = Log_Like_deriv5, method = "BFGS", mat = matrix, R = R)$value -
                 optim(par = rep(0,ntheta(matrix,2)), fn = Log_Like2, gr = Log_Like_deriv2, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix,5,R) - ntheta(matrix,2)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 2 & ModelNumber(m2) == 6){
    stat = -2*(optim(par = rep(0,ntheta(matrix,6)), fn = Log_Like6, gr = Log_Like_deriv6, method = "BFGS", mat = matrix)$value -
                 optim(par = rep(0,ntheta(matrix,2)), fn = Log_Like2, gr = Log_Like_deriv2, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix,6) - ntheta(matrix,2)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 3 & ModelNumber(m2) == 4){
    stat = -2*(optim(par = rep(0,ntheta(matrix,4)), fn = Log_Like4, gr = Log_Like_deriv4, method = "BFGS", mat = matrix)$value -
                 optim(par = rep(0,ntheta(matrix,3,R)), fn = Log_Like3, gr = Log_Like_deriv3, method = "BFGS", mat = matrix, R = R)$value)
    df = ntheta(matrix,4) - ntheta(matrix,3,R)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 3 & ModelNumber(m2) == 5){
    stat = -2*(optim(par = rep(0,ntheta(matrix,5,R)), fn = Log_Like5, gr = Log_Like_deriv5, method = "BFGS", mat = matrix, R = R)$value -
                 optim(par = rep(0,ntheta(matrix,3,R)), fn = Log_Like3, gr = Log_Like_deriv3, method = "BFGS", mat = matrix, R = R)$value)
    df = ntheta(matrix,5,R) - ntheta(matrix,3,R)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 3 & ModelNumber(m2) == 6){
    stat = -2*(optim(par = rep(0,ntheta(matrix,6)), fn = Log_Like6, gr = Log_Like_deriv6, method = "BFGS", mat = matrix)$value -
                 optim(par = rep(0,ntheta(matrix,3,R)), fn = Log_Like3, gr = Log_Like_deriv3, method = "BFGS", mat = matrix, R = R)$value)
    df = ntheta(matrix,6) - ntheta(matrix,3,R)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 4 & ModelNumber(m2) == 5){
    stat = -2*(optim(par = rep(0,ntheta(matrix,5,R)), fn = Log_Like5, gr = Log_Like_deriv5, method = "BFGS", mat = matrix, R = R)$value -
                 optim(par = rep(0,ntheta(matrix,4)), fn = Log_Like4, gr = Log_Like_deriv4, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix,5,R) - ntheta(matrix,4)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 4 & ModelNumber(m2) == 6){
    stat = -2*(optim(par = rep(0,ntheta(matrix,6)), fn = Log_Like6, gr = Log_Like_deriv6, method = "BFGS", mat = matrix)$value -
                 optim(par = rep(0,ntheta(matrix,4)), fn = Log_Like4, gr = Log_Like_deriv4, method = "BFGS", mat = matrix)$value)
    df = ntheta(matrix,6) - ntheta(matrix,4)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
  if(ModelNumber(m1) == 5 & ModelNumber(m2) == 6){
    stat = -2*(optim(par = rep(0,ntheta(matrix,6)), fn = Log_Like6, gr = Log_Like_deriv6, method = "BFGS", mat = matrix)$value -
                 optim(par = rep(0,ntheta(matrix,5,R)), fn = Log_Like5, gr = Log_Like_deriv5, method = "BFGS", mat = matrix, R = R)$value)
    df = ntheta(matrix,6) - ntheta(matrix,5,R)
    cat('P-Value is ', 1 - pchisq(stat, df), '\n')
    cat('with a statistic of ', stat, 'and', df, 'degrees of freedom')
  }
}

#THIS PLOT FUNCTION IS DIFFER FROM VAN'S, CHANGED TSH MODEL
BT_plots = function(model){
  # Takes a fitted Bradley-Terry model from the BT_Model() function and produces a plot of the theta values by team.
  # For hierarchical models (CHI and HIE), it prints each plot every ten seconds, starting with R_1, up to R_n

  if(model$type == 'VAN'){
    # Vanilla Model
    Teams = model$Teams
    table = model$Table
    Theta = table[order(-table$Theta),]
    PLOT <- Theta %>%
      arrange(Theta) %>%
      mutate(Teams = factor(teams, unique(teams))) %>%
      ggplot() + aes(x=Teams, y=Theta) +
      geom_segment( aes(x=Teams, xend=Teams, y=0, yend=Theta), color="black") +
      geom_point(color=rgb(0.2,0.7,0.1,0.5), size=3, alpha=0.6) +
      ylab("Theta") + xlab("Teams") +
      theme_light() +
      coord_flip() +
      ggtitle("Team rankings of this season - VAN model")+
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
      )
    return(PLOT)
  }
  if(model$type == 'CHA'){
    # Common Homeground Advantage Model
    Teams = model$Teams
    table = model$Table
    Theta = table[order(-table$Theta),]
    Theta$home <- Theta$Theta + model$Alpha
    PLOT <- Theta %>%
      arrange(Theta) %>%
      mutate(Teams = factor(teams, unique(teams))) %>%
      ggplot() +
      geom_segment(aes(x=Teams, xend=Teams, y=Theta, yend=home), color="black") +
      geom_point(aes(x=Teams, y=Theta), color=rgb(0.2,0.7,0.1,0.5), size=3, alpha = 0.6 ) +
      geom_point(aes(x=Teams, y=home), color=rgb(0.7,0.2,0.1,0.5), size=3, alpha = 0.6 ) +
      coord_flip()+
      theme_light()+
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())+
      xlab("Teams") +
      ylab("Theta")
    return(PLOT)
  }
  if(model$type == 'CHI'){
    # Common Hierarchical Home-ground Advantage Model
    for(i in model$Alpha[,]){
      table = data.frame(teams = model$Teams, Theta = model$Table[,], home = model$Table[,] + i)
      Teams = model$Teams
      Theta = table[order(-table$Theta),]
      print(Theta %>%
              arrange(Theta) %>%
              mutate(Teams = factor(teams, unique(teams))) %>%
              ggplot() +
              geom_segment(aes(x=Teams, xend=Teams, y=Theta, yend=home), color="black") +
              geom_point(aes(x=Teams, y=Theta), color=rgb(0.2,0.7,0.1,0.5), size=3, alpha = 0.6 ) +
              geom_point(aes(x=Teams, y=home), color=rgb(0.7,0.2,0.1,0.5), size=3, alpha = 0.6 ) +
              coord_flip()+
              theme_light()+
              theme(
                panel.grid.major.y = element_blank(),
                panel.border = element_blank(),
                axis.ticks.y = element_blank())+
              xlab("Teams") + ylab("Theta(CHI)"))
      Sys.sleep(10)
    }
  }

  if(model$type == 'TSH'){
    # Team-specific Home-ground Advantage Model
    table = data.frame(teams = model$Teams, Theta = model$Table, home = NA)
    for (i in 1:nrow(model$Table)){
      table$home[i] <- model$Table[i,] + model$Alpha[i,]
    }
    table$midpoint <- (table$Theta + table$home) / 2  # 计算 Theta 和 home 的平均值
    Teams = model$Teams
    #Theta = table[order(-table$Theta),]
    Theta = table[order(-table$midpoint),]
    PLOT <- Theta %>%
      arrange(midpoint) %>%
      mutate(Teams = factor(teams, unique(teams))) %>%
      ggplot() +
      geom_segment(aes(x=Teams, xend=Teams, y=Theta, yend=home), color="black") +
      geom_point(aes(x=Teams, y=Theta), color=rgb(0.2,0.7,0.1,0.5), size=3, alpha = 0.6 ) +
      geom_point(aes(x=Teams, y=home), color=rgb(0.7,0.2,0.1,0.5), size=3, alpha = 0.6 ) +
      geom_point(aes(x=Teams, y=midpoint), color="navy", size=3, alpha=0.6) +
      coord_flip()+
      theme_light()+
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())+
      xlab("Teams") +
      ylab("Theta(TSH)")
    return(PLOT)
  }
  if(model$type == "HIE"){
    # Hierarchical Home-ground Advantage Model
    for(i in 1:ncol(model$Alpha)){
      table = data.frame(teams = model$Teams, Theta = model$Table[,], home = model$Table[,] + model$Alpha[,i])
      Teams = model$Teams
      Theta = table[order(-table$Theta),]
      print(Theta %>%
              arrange(Theta) %>%
              mutate(Teams = factor(teams, unique(teams))) %>%
              ggplot() +
              geom_segment(aes(x=Teams, xend=Teams, y=Theta, yend=home), color="black") +
              geom_point(aes(x=Teams, y=Theta), color=rgb(0.2,0.7,0.1,0.5), size=3, alpha = 0.6 ) +
              geom_point(aes(x=Teams, y=home), color=rgb(0.7,0.2,0.1,0.5), size=3, alpha = 0.6 ) +
              coord_flip()+
              theme_light()+
              theme(
                panel.grid.major.y = element_blank(),
                panel.border = element_blank(),
                axis.ticks.y = element_blank())+
              xlab("Teams") + ylab("Theta"))
      Sys.sleep(5)
    }
  }
}

BT_predict = function(model, df, R = NULL){
  # Predicts the probability of either side winning a match based off the fitted BT Model
  # Data frame consists of Team A, Team B, Team A Home, Team B Home
  Table <- model$Table
  for (i in 1:nrow(df)){
    if(df[i,1] %in% model$Teams & df[i,2] %in% model$Teams){
      if(df[i,3] != df[i,4]){
        # Essentially saying Team A Home = 0 and Team B Home = 1
        if(model$type == 'VAN'){
          df$Team_B_win[i] = exp(Table$Theta[Table$teams == df[i,2]])/(exp(Table$Theta[Table$teams == df[i,2]]) + exp(Table$Theta[Table$teams == df[i,1]]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
        if(model$type == 'CHA'){
          df$Team_B_win[i] = exp(Table$Theta[Table$teams == df[i,2]] + model$Alpha)/(exp(Table$Theta[Table$teams == df[i,2]] + model$Alpha) + exp(Table$Theta[Table$teams == df[i,1]]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
        if(model$type == 'CHI'){
          df$Team_B_win[i] = exp(Table[df[i,2],] + model$Alpha[R[df[i,2],df[i,1]],])/(exp(Table[df[i,2],] + model$Alpha[R[df[i,2],df[i,1]],]) + exp(Table[df[i,1],]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
        if(model$type == 'TSH'){
          Alpha <- model$Alpha
          df$Team_B_win[i] = exp(Table[df[i,2],] + Alpha[df[i,2],])/(exp(Table[df[i,2],] + Alpha[df[i,2],]) + exp(Table[df[i,1],]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
        if(model$type == 'HIE'){
          Alpha <- model$Alpha
          df$Team_B_win[i] = exp(Table[df[i,2],] + model$Alpha[df[i,2], R[df[i,2],df[i,1]]])/(exp(Table[df[i,2],] + model$Alpha[df[i,2], R[df[i,2],df[i,1]]]) + exp(Table[df[i,1],]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
      }
      if(df[i,3] == df[i,4]){
        # Essentially saying Team A Home = 0 and Team B Home = 0
        if(model$type == 'VAN'){
          df$Team_B_win[i] = exp(Table$Theta[Table$teams == df[i,2]])/(exp(Table$Theta[Table$teams == df[i,2]]) + exp(Table$Theta[Table$teams == df[i,1]]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
        if(model$type == 'CHA'){
          df$Team_B_win[i] = exp(Table$Theta[Table$teams == df[i,2]])/(exp(Table$Theta[Table$teams == df[i,2]]) + exp(Table$Theta[Table$teams == df[i,1]]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
        if(model$type == 'CHI'){
          df$Team_B_win[i] = exp(Table[df[i,2],])/(exp(Table[df[i,2],]) + exp(Table[df[i,1],]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
        if(model$type == 'TSH'){
          Alpha <- model$Alpha
          df$Team_B_win[i] = exp(Table[df[i,2],])/(exp(Table[df[i,2],]) + exp(Table[df[i,1],]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
        if(model$type == 'HIE'){
          Alpha <- model$Alpha
          df$Team_B_win[i] = exp(Table[df[i,2],])/(exp(Table[df[i,2],]) + exp(Table[df[i,1],]))
          df$Team_A_win[i] = 1 - df$Team_B_win[i]
        }
      }
    }
  }
  return(df)
}

# 2
# Past years model

rawdata <- read_excel("NBA Data 2018-19 to 2022-23.xlsx")
formaldata<-DesiredFormatNBA(rawdata)#提取原始数据中的主要分析变量
matdata<-Data2Mat(formaldata)#继续转化主要分析变量为一些矩阵
fixtures= formaldata#fixture是只有几列的数据集
NBA_R = function(fixtures){
  data = fixtures
  Teams = unique(fixtures$Team.B)

  Atlantic = c('Boston Celtics', 'Brooklyn Nets', 'New York Knicks', 'Philadelphia 76ers', 'Toronto Raptors')
  Central = c('Chicago Bulls', 'Cleveland Cavaliers', 'Detroit Pistons', 'Indiana Pacers', 'Milwaukee Bucks')
  Southeast = c('Atlanta Hawks', 'Charlotte Hornets', 'Miami Heat', 'Orlando Magic', 'Washington Wizards')
  Northwest = c('Denver Nuggets', 'Minnesota Timberwolves', 'Oklahoma City Thunder', 'Portland Trail Blazers', 'Utah Jazz')
  Pacific = c('Golden State Warriors', 'Los Angeles Clippers', 'Los Angeles Lakers', 'Phoenix Suns','Sacramento Kings')
  Southwest = c('Dallas Mavericks', 'Houston Rockets','Memphis Grizzlies','New Orleans Pelicans','San Antonio Spurs')

  D = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))
  C = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))
  L = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))
  R = matrix(data = 0, nrow = length(Teams), ncol = length(Teams), dimnames = list(Teams, Teams))

  East = data.frame(Atlantic, Southeast, Central)
  West = data.frame(Southwest, Pacific, Northwest)
  NBA = data.frame(East, West)

  # Basic designation of relationship
  for(i in Teams){
    for(j in Teams){
      if(i != j){
        L[i, j] = 1
      }
    }
  }

  # Designating Conferences
  for(con1 in 1:ncol(East)){
    for(con2 in 1:ncol(East)){
      for(i in 1:nrow(East[con1])){
        for(j in 1:nrow(East[con2])){
          if(East[i, con1] != East[j, con2]){
            C[East[i, con1], East[j, con2]] = 1
          }
        }
      }
    }
  }

  for(con1 in 1:ncol(West)){
    for(con2 in 1:ncol(West)){
      for(i in 1:nrow(West[con1])){
        for(j in 1:nrow(West[con2])){
          if(West[i, con1] != West[j, con2]){
            C[West[i, con1], West[j, con2]] = 1
          }
        }
      }
    }
  }

  # Designating Divisions
  for(div in 1:ncol(NBA)){
    for(i in 1:nrow(NBA[div])){
      for(j in 1:nrow(NBA[div])){
        if(NBA[i, div] != NBA[j, div])
          D[NBA[i, div], NBA[j, div]] = 1
      }
    }
  }

  R = C + D + L
  return(R)
}
R <- NBA_R(fixtures)#表示队伍之间的胜负关系矩阵
VAN = BT_Model(matdata,'VAN',R=R)
Teams <-VAN$Teams


# 3
# simulation for new season
# Only for one new season's CHI and use for simulation
rawdatanew <- read_excel("2023-2024 regular.xlsx")
formaldatanew<-DesiredFormatNBA(rawdatanew)
matdatanew<-Data2Mat(formaldatanew)
VAN_2024 = BT_Model(matdatanew,'VAN',R=R)

plot<- BT_plots(VAN_2024)

# 4

# 4
# Simulation for new seasons playin

#Prob for playin
Simulation_Play_In = function(model, df, R){
  # A small function to obtain the probabilities for a play-in tournament 
  p <- BT_predict(model, df, R)
  for (i in 1:nrow(p)){
    p$Home_win[i] <- rbinom(1, 1, p$Team_B_win)
  }
  return(p)
}
#Process of playin
PlayInTournament = function(model, df, R){
  p <- Simulation_Play_In(model, df, R )
  Seeds <- data.frame(matrix(ncol = 2, nrow = 8))
  colnames(Seeds) <- c("Seeds", "Teams")
  Seeds$Seeds[1] <- "7th Seed - East"
  Seeds$Seeds[2] <- "7th Seed - West"
  Seeds$Seeds[3] <- "8th Seed - East"
  Seeds$Seeds[4] <- "8th Seed - West"
  Seeds$Seeds[5] <- "9th Seed - East"
  Seeds$Seeds[6] <- "9th Seed - West"
  Seeds$Seeds[7] <- "10th Seed - East"
  Seeds$Seeds[8] <- "10th Seed - West"
  p[5,] <- NA
  p[6,] <- NA
  if (p$Home_win[1] == 1){
    Seeds$Teams[1] <- p[1,2]
    p[5,2] <- p[1,1]
  } else {
    Seeds$Teams[1] <- p[1,1]
    p[5,2] <- p[1,2]
  }
  if (p$Home_win[2] == 1){
    Seeds$Teams[2] <- p[2,2]
    p[6,2] <- p[2,1]
  } else {
    Seeds$Teams[2] <- p[2,1]
    p[6,2] <- p[2,2]
  }
  if (p$Home_win[3] == 1){
    p[5,1] <- p[3,2]
    Seeds$Teams[7] <- p[3,1]
  } else {
    p[5,1] <- p[3,1]
    Seeds$Teams[7] <- p[3,2]
  }
  if (p$Home_win[4] == 1){
    p[6,1] <- p[4,2]
    Seeds$Teams[8] <- p[4,1]
  } else {
    p[6,1] <- p[4,1]
    Seeds$Teams[8] <- p[4,2]
  }
  New <- p[5:6,]
  New[,3:4] <- NA
  New[,3] <- 0
  New[,4] <- 0
  Final <- Simulation_Play_In(model,New,R)
  New <- rbind(p[1:4,],Final)
  if (Final$Home_win[1] == 1){
    Seeds$Teams[3] <- Final[1,2]
    Seeds$Teams[5] <- Final[1,1]
  } else {
    Seeds$Teams[3] <- Final[1,1]
    Seeds$Teams[5] <- Final[1,2]
  }
  if (Final$Home_win[2] == 1){
    Seeds$Teams[4] <- Final[2,2]
    Seeds$Teams[6] <- Final[2,1]
  } else {
    Seeds$Teams[4] <- Final[2,1]
    Seeds$Teams[6] <- Final[2,2]
  }
  Output <- Seeds
  return(Output)
  # Returns the seeds of the teams after the completion of the tournament
} 

playin2024 <- read_csv("2023-2024 palyin.csv", col_types = cols_only(
  `Team A` = col_character(),
  `Team A Home` = col_double(),
  `Team B` = col_character(),
  `Team B Home` = col_double()
))

playin2024 <- as.data.frame(playin2024)
ProbabilitiesVAN <- BT_predict(VAN_2024, formaldatanew, R)

Seedsrun <- function() {
  Seeds <<- PlayInTournament(VAN_2024, playin2024, R)  # 更新全局变量 Seeds
  Seeds <<- as.data.frame(Seeds)  # 这里也用 <<- 确保更新全局变量
  return(Seeds)
}
Seeds<-Seedsrun()


#5
# East playoffs
Seeds_East = function(Seeds){
  # Seeds_East takes the output from PlayInTournament() and gives the appropriate seeding for the 10 teams in the East.
  # An example of this season's seeding. 
  Seed_No <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  Team <- c("Boston Celtics", "New York Knicks", "Milwaukee Bucks", "Cleveland Cavaliers", "Orlando Magic", "Indiana Pacers", Seeds[1,2], Seeds[3,2], Seeds[5,2], Seeds[7,2])
  Seeds_East <- data.frame(Seed_No, Team)
  return(Seeds_East)
}
Seedsofeast<-Seeds_East(Seeds)
Simulation_Playoff_East = function(df, SE, Probabilities){
  # df is the fixture list of the Eastern Conference playoffs without the play-in teams added with two columns "Team A" and "Team B". Team A has the higher seed in this fixture list.
  # Probabilities is the results of BT_predict for the entire season
  # SE is the Seeds of the Eastern Conference obtained from Seeds_East()
  p <- df
  p[1,2] <- SE[8,2]
  p[2,2] <- SE[7,2]
  p$Home_Win <- NA
  p$Winner <- NA
  p$TeamA_home_win_prob <-NA
  p$TeamA_away_win_prob <-NA
  
  for (i in 1:nrow(p)){
    p$TeamA_home_win_prob[i] <- Probabilities$Team_B_win[Probabilities$Team.A == p[i,2] & Probabilities$Team.B == p[i,1]][1]
    p$TeamA_away_win_prob[i] <- Probabilities$Team_A_win[Probabilities$Team.A == p[i,1] & Probabilities$Team.B == p[i,2]][1]
    p$Home_Win[i]<-rbinom(1,2,p$TeamA_home_win_prob[i])+
      rbinom(1,2,p$TeamA_away_win_prob[i])+
      rbinom(1,1,p$TeamA_home_win_prob[i])+
      rbinom(1,1,p$TeamA_away_win_prob[i])+
      rbinom(1,1,p$TeamA_home_win_prob[i])
  }
  
  for (i in 1:nrow(p)){
    if (p$Home_Win[i] >= 4){
      p$Winner[i] <- p[i,1]
    } else {
      p$Winner[i] <- p[i,2]
    }
  }
  p[5:7,] <- NA
  p[5,1] <- p$Winner[1]
  p[5,2] <- p$Winner[4]
  p[6,1] <- p$Winner[2]
  p[6,2] <- p$Winner[3]
  if ((SE$Seed_No[SE$Team == p[5,1]]) > (SE$Seed_No[SE$Team == p[5,2]])){
    p[5,1:2] <- p[5,][c("Team.B", "Team.A")]
  }
  if ((SE$Seed_No[SE$Team == p[6,1]]) > (SE$Seed_No[SE$Team == p[6,2]])){
    p[6,1:2] <- p[6,][c("Team.B", "Team.A")]
  }
  Semis <- p[5:6,]
  for (i in 5:6){
    p$TeamA_home_win_prob[i] <- Probabilities$Team_B_win[Probabilities$Team.A == p[i,2] & Probabilities$Team.B == p[i,1]][1]
    p$TeamA_away_win_prob[i] <- Probabilities$Team_A_win[Probabilities$Team.A == p[i,1] & Probabilities$Team.B == p[i,2]][1]
    p$Home_Win[i]<-rbinom(1,2,p$TeamA_home_win_prob[i])+
      rbinom(1,2,p$TeamA_away_win_prob[i])+
      rbinom(1,1,p$TeamA_home_win_prob[i])+
      rbinom(1,1,p$TeamA_away_win_prob[i])+
      rbinom(1,1,p$TeamA_home_win_prob[i])
  }
  for (i in 5:6){
    if (p$Home_Win[i] >= 4){
      p$Winner[i] <- p[i,1]
    } else {
      p$Winner[i] <- p[i,2]
    }
  }
  p[7,1] <- p$Winner[5]
  p[7,2] <- p$Winner[6]
  if ((SE$Seed_No[SE$Team == p[7,1]]) > (SE$Seed_No[SE$Team == p[7,2]])){
    p[7,1:2] <- p[7,][c("Team.B", "Team.A")]
  }
  Final <- p[7,]
  for (i in 7){
    p$TeamA_home_win_prob[i] <- Probabilities$Team_B_win[Probabilities$Team.A == p[i,2] & Probabilities$Team.B == p[i,1]][1]
    p$TeamA_away_win_prob[i] <- Probabilities$Team_A_win[Probabilities$Team.A == p[i,1] & Probabilities$Team.B == p[i,2]][1]
    p$Home_Win[i]<-rbinom(1,2,p$TeamA_home_win_prob[i])+
      rbinom(1,2,p$TeamA_away_win_prob[i])+
      rbinom(1,1,p$TeamA_home_win_prob[i])+
      rbinom(1,1,p$TeamA_away_win_prob[i])+
      rbinom(1,1,p$TeamA_home_win_prob[i])
  }
  for (i in 7){
    if (p$Home_Win[i] >= 4){
      p$Winner[i] <- p[i,1]
    } else {
      p$Winner[i] <- p[i,2]
    }
  }
  return(p)
}
playoffs_East <- as.data.frame(read_csv("Playoffs_East2023.csv", show_col_types = FALSE))


East <- function() {
  result <- Simulation_Playoff_East(playoffs_East, Seedsofeast, ProbabilitiesVAN)
  EastResult <<- result
  return(EastResult)
}

# 6
# West playoffs
Seeds_West = function(Seeds){
  # Seeds_West takes the output from PlayInTournament() and gives the appropraie seeding for the 10 teams in the West. An example of this season's seeding. 
  Seed_No <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  Team <- c("Oklahoma City Thunder", "Denver Nuggets", "Minnesota Timberwolves", "Los Angeles Clippers", "Dallas Mavericks", "Phoenix Suns", Seeds[2,2], Seeds[4,2], Seeds[6,2], Seeds[8,2])
  Seeds_West <- data.frame(Seed_No, Team)
  return(Seeds_West)
}
playoffs_West <- as.data.frame(read_csv("Playoffs_West2023.csv", show_col_types = FALSE))
Simulation_Playoff_West = function(df, SW, Probabilities){
  # df is the fixture list of the Western Conference playoffs without the play-in teams added with two columns "Team A" and "Team B". Team A has the higher seed in this fixture list.
  # Probabilities is the results of BT_predict for the entire season
  # SW is the Seeds of the Western Conference obtained from Seeds_West
  p <- df
  p[1,2] <- SW[8,2]
  p[2,2] <- SW[7,2]
  p$Home_Win <- NA
  p$Winner <- NA
  p$TeamA_home_win_prob <-NA
  p$TeamA_away_win_prob <-NA
  # 查看 p 中的队伍名称
  #print(unique(p$Team.A))
  #print(unique(p$Team.B))
  for (i in 1:nrow(p)){
    p$TeamA_home_win_prob[i] <- Probabilities$Team_B_win[Probabilities$Team.A == p[i,2] & Probabilities$Team.B == p[i,1]][1]
    p$TeamA_away_win_prob[i] <- Probabilities$Team_A_win[Probabilities$Team.A == p[i,1] & Probabilities$Team.B == p[i,2]][1]
    p$Home_Win[i]<-rbinom(1,2,p$TeamA_home_win_prob[i])+
      rbinom(1,2,p$TeamA_away_win_prob[i])+
      rbinom(1,1,p$TeamA_home_win_prob[i])+
      rbinom(1,1,p$TeamA_away_win_prob[i])+
      rbinom(1,1,p$TeamA_home_win_prob[i])
  }
  for (i in 1:nrow(p)){
    if (p$Home_Win[i] >= 4){
      p$Winner[i] <- p[i,1]
    } else {
      p$Winner[i] <- p[i,2]
    }
  }
  p[5:7,] <- NA
  p[5,1] <- p$Winner[1]
  p[5,2] <- p$Winner[4]
  p[6,1] <- p$Winner[2]
  p[6,2] <- p$Winner[3]
  if ((SW$Seed_No[SW$Team == p[5,1]]) > (SW$Seed_No[SW$Team == p[5,2]])){
    p[5,1:2] <- p[5,][c("Team.B", "Team.A")]
  }
  if ((SW$Seed_No[SW$Team == p[6,1]]) > (SW$Seed_No[SW$Team == p[6,2]])){
    p[6,1:2] <- p[6,][c("Team.B", "Team.A")]
  }
  Semis <- p[5:6,]
  for (i in 5:6){
    p$TeamA_home_win_prob[i] <- Probabilities$Team_B_win[Probabilities$Team.A == p[i,2] & Probabilities$Team.B == p[i,1]][1]
    p$TeamA_away_win_prob[i] <- Probabilities$Team_A_win[Probabilities$Team.A == p[i,1] & Probabilities$Team.B == p[i,2]][1]
    p$Home_Win[i]<-rbinom(1,2,p$TeamA_home_win_prob[i])+
      rbinom(1,2,p$TeamA_away_win_prob[i])+
      rbinom(1,1,p$TeamA_home_win_prob[i])+
      rbinom(1,1,p$TeamA_away_win_prob[i])+
      rbinom(1,1,p$TeamA_home_win_prob[i])
  }
  for (i in 5:6){
    if (p$Home_Win[i] >= 4){
      p$Winner[i] <- p[i,1]
    } else {
      p$Winner[i] <- p[i,2]
    }
  }
  p[7,1] <- p$Winner[5]
  p[7,2] <- p$Winner[6]
  if ((SW$Seed_No[SW$Team == p[7,1]]) > (SW$Seed_No[SW$Team == p[7,2]])){
    p[7,1:2] <- p[7,][c("Team.B", "Team.A")]
  }
  Final <- p[7,]
  for (i in 7){
    p$TeamA_home_win_prob[i] <- Probabilities$Team_B_win[Probabilities$Team.A == p[i,2] & Probabilities$Team.B == p[i,1]][1]
    p$TeamA_away_win_prob[i] <- Probabilities$Team_A_win[Probabilities$Team.A == p[i,1] & Probabilities$Team.B == p[i,2]][1]
    p$Home_Win[i]<-rbinom(1,2,p$TeamA_home_win_prob[i])+
      rbinom(1,2,p$TeamA_away_win_prob[i])+
      rbinom(1,1,p$TeamA_home_win_prob[i])+
      rbinom(1,1,p$TeamA_away_win_prob[i])+
      rbinom(1,1,p$TeamA_home_win_prob[i])
  }
  for (i in 7){
    if (p$Home_Win[i] >= 4){
      p$Winner[i] <- p[i,1]
    } else {
      p$Winner[i] <- p[i,2]
    }
  }
  return(p)
}
Seedsofwest<-Seeds_West(Seeds)

West <- function() {
  result <- Simulation_Playoff_West(playoffs_West, Seedsofwest, ProbabilitiesVAN)
  WestResult <<- result
  return(WestResult)
}
#7 
# Final
# This seeds only include 21 teams of season, if for new data, should change it as automaticly get all rankings of new season
# This seeds only include 21 teams of season, if for new data, should change it as automaticly get all rankings of new season
Seeds_Tournament = function(){
  # Seeds of the entire league with an example of the 2022-23 season
  #https://www.espn.com/nba/standings/_/season/2023/group/league
  Seed_No <- c(1:21)
  Team <- c("Boston Celtics", "Oklahoma City Thunder", "Denver Nuggets", "Minnesota Timberwolves",
            "Los Angeles Clippers",  "Dallas Mavericks", "New York Knicks", "Milwaukee Bucks",
            "New Orleans Pelicans", "Phoenix Suns", "Cleveland Cavaliers", "Orlando Magic",
            "Los Angeles Lakers", "Philadelphia 76ers", "Indiana Pacers", "Miami Heat",
            "Sacramento Kings", "Golden State Warriors", "Houston Rockets","Chicago Bulls",
            "Atlanta Hawks")
  Seeds_Tournament <- data.frame(Seed_No, Team)
  return(Seeds_Tournament)
}
ST<-Seeds_Tournament()
Final = function(ST){
  
  # 使用 EastResult 和 WestResult 进行后续操作
  Finalteam <- data.frame(matrix(ncol = 2))
  colnames(Finalteam) <- c("Team.A", "Team.B")
  Finalteam[,1] <- EastResult$Winner[7]
  Finalteam[,2] <- WestResult$Winner[7]
  
  if ((ST$Seed_No[ST$Team == Finalteam[,1]]) > (ST$Seed_No[ST$Team == Finalteam[,2]])){
    Finalteam[,1:2] <- Finalteam[,c(2,1)]
  }
  Finalteam <<- Finalteam
  return(Finalteam)
  
}
#Finalteam<- Final(ST)


Simulation_Final = function(df, Probabilities){
  # df is the output from the function Final(), which adjusts the seeding to obtain the extra home court game
  p <- df
  p$Home_Win <- NA
  p$Winner <- NA
  for (i in 1:nrow(p)){
    p$TeamA_home_win_prob[i] <- Probabilities$Team_B_win[Probabilities$Team.A == p[i,2] & Probabilities$Team.B == p[i,1]][1]
    p$TeamA_away_win_prob[i] <- Probabilities$Team_A_win[Probabilities$Team.A == p[i,1] & Probabilities$Team.B == p[i,2]][1]
    p$Home_Win[i]<-rbinom(1,2,p$TeamA_home_win_prob[i])+
      rbinom(1,2,p$TeamA_away_win_prob[i])+
      rbinom(1,1,p$TeamA_home_win_prob[i])+
      rbinom(1,1,p$TeamA_away_win_prob[i])+
      rbinom(1,1,p$TeamA_home_win_prob[i])
  }
  for (i in 1){
    if (p$Home_Win[i] >= 4){
      p$Winner[i] <- p[i,1]
    } else {
      p$Winner[i] <- p[i,2]
    }
  }
  return(p)
}
#Finalgame<-Simulation_Final(Finalteam, ProbabilitiesCHI)
Finalgame2 = function(){
  return(Simulation_Final(Finalteam, ProbabilitiesVAN))
  
}

NBAPLAYOFFS <- function() {
  # Combine all the functions to obtain results of the simulations
  Seeds <- PlayInTournament(VAN_2024, playin2024, R = R)  # Use play-in data
  Seedsofeast <- Seeds_East(Seeds)
  EastResult <- East()  # Run the East playoffs
  Seedsofwest <- Seeds_West(Seeds)
  WestResult <- West()  # Run the West playoffs
  ST <- Seeds_Tournament()
  Finalteam <- Final(ST)  # Get the final teams
  Championship <- Finalgame2()  # Run the simulation for the championship
  
  # Prepare output
  Output <- data.frame(EastResult)
  colnames(Output) <- c("Team.A", "Team.B", "Home_Win", "Winner", "TeamA_home_win_prob", "TeamA_away_win_prob")
  Output[8:14, ] <- WestResult  # Add West results to output
  Output[15, ] <- Championship  # Add Championship result
  
  return(Output)  # Return only the Championship result
}

NBA <- function() {
  # Combine all the functions to obtain results of the simulations
  Seeds <- PlayInTournament(VAN_2024, playin2024, R = R)  # Use play-in data
  Seedsofeast <- Seeds_East(Seeds)
  EastResult <- East()  # Run the East playoffs
  Seedsofwest <- Seeds_West(Seeds)
  WestResult <- West()  # Run the West playoffs
  ST <- Seeds_Tournament()
  Finalteam <- Final(ST)  # Get the final teams
  Championship <- Finalgame2()  # Run the simulation for the championship
  
  # Prepare output
  Output <- data.frame(EastResult)
  colnames(Output) <- c("Team.A", "Team.B", "Home_Win", "Winner", "TeamA_home_win_prob", "TeamA_away_win_prob")
  Output[8:14, ] <- WestResult  # Add West results to output
  Output[15, ] <- Championship  # Add Championship result
  
  return(Championship$Winner)  # Return only the Championship result
}



#10 
# output

#print(CHI_2024)
modelplot<-BT_plots(VAN_2024)
#Seeds
#East
#West
#Finalgame
#RUNONCE
#Champion
#plot_simulations

