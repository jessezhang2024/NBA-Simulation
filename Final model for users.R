# Interface Interaction

#Functions for simulation
simulation_for_times <- function() {
  n <- as.numeric(readline(prompt = "The times you want to simulate: "))
  
  if (is.na(n) || n <= 0) {
    stop("Please enter a valid positive number.")
  }
  simulations <- n
  winners <- character(simulations)
  for (i in 1:simulations) {
    winners[i] <- NBA()  
  }
  
  winner_counts <- as.data.frame(table(winners))
  plot_simulation <- ggplot(winner_counts, aes(x = reorder(winners, Freq), y = Freq)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    geom_text(aes(label = Freq), vjust = -0.5, color = "blue") +
    labs(x = "Championship Winner", y = "Number of Simulations",
         title = paste("NBA Championship Simulations Results (", n, " simulations) in CHI model", sep = "")) +
    coord_flip() +
    theme_minimal()
  
  return(plot_simulation)
}
NBAsource <- function() {
  repeat {
    model_name <- readline(prompt = "Choose the model you want to use: VAN, CHA, CHI, TSH, HIE: ")
    if (model_name == "VAN") {
      suppressMessages(suppressWarnings(source("VAN MODEL.R")))
      message("VAN_MODEL loaded.")
      break
    } else if (model_name == "CHI") {
      suppressMessages(suppressWarnings(source("CHI MODEL.R")))
      message("CHI_MODEL loaded.")
      break
    } else if (model_name == "CHA") {
      suppressMessages(suppressWarnings(source("CHA MODEL.R")))
      message("CHA_MODEL loaded.")
      break
    } else if (model_name == "TSH") {
      suppressMessages(suppressWarnings(source("TSH MODEL.R")))
      message("TSH_MODEL loaded.")
      break
    } else if (model_name == "HIE") {
      suppressMessages(suppressWarnings(source("HIE MODEL.R")))
      message("HIE_MODEL loaded.")
      break
    } else {
      message("Invalid model name. Please choose from VAN, CHI, CHA, TSH, HIE.")
    }
  }
}
next_step <- function() {
  repeat {
    options <- readline(prompt = "Choose the next step: Modelplot,Seeds, East, West, Finalgame,One playoffs, Mutisimulation: ")
    
    if (options == "Seeds") {
      #Seeds<-Seedsrun()
      print(Seeds)
      Seedsofeast<-Seeds_East(Seeds)
      print(Seedsofeast)
      Seedsofwest<-Seeds_West(Seeds)
      print(Seedsofwest)
    } else if (options == "East") {
      #Seeds_East(Seds)
      print(East())
    } else if (options == "Modelplot") {
      print(modelplot)
    } else if (options == "West") {
      #Seeds_West(Seeds)
      print(West())
    } else if (options == "Finalgame") {
      print(Final(ST))
      print(Finalgame2())
    } else if (options == "One playoffs") {
    
      print(NBAPLAYOFFS())
    } else if (options == "Mutisimulation") {
      plot <- simulation_for_times()  # 
      print(plot)                    
                             
    } else {
      message("Invalid option. Please choose from Seeds, East, West, Finalgame, Process, mutisimulation.")
    }
    
    repeat {
      continue_simulation <- readline(prompt = "Simulate another? Choose 'Yes', 'Next', or 'Stop': ")
      
      if (continue_simulation == "Yes") {
        next_step()  #
        return      
      } else if (continue_simulation == "Next") {
        plot <- simulation_for_times()  
        print(plot)                     
        return       
      } else if (continue_simulation == "Stop") {
        message("Simulation stopped.")
        stop("Process terminated by user.")
      } else {
        message("Invalid choice. Please enter 'Yes', 'Next', or 'Stop'.")
      }
    }
    break 
  }
}

#User choose models
choose_model <- function() {
  NBAsource() 
  next_step() 
}

#Run whole model
choose_model()



