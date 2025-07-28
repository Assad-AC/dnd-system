box::use(
  dplyr[bind_rows]
)

library(ggplot2)

prob_difficulty_class <- function(dice_sides, dc, mod = 0, 
                                  circumstance = "normal") {

  singleTry_ReachDc_Prob <- (dice_sides - (dc - 1) + mod) / dice_sides

  if (circumstance == "normal") {
    return(singleTry_ReachDc_Prob)
  } else if (circumstance == "advantage") {
    complement_SingleTry_ReachDc_Prob <- 1 - singleTry_ReachDc_Prob

    all_Complement_TwoTries_ReachDc_Prob <- complement_SingleTry_ReachDc_Prob^2
   
    any_TwoTries_ReachDc_Prob <- 1 - all_Complement_TwoTries_ReachDc_Prob


    return(any_TwoTries_ReachDc_Prob)
  } else if (circumstance == "disadvantage") {
    all_TwoTries_ReachDc_Prob <- singleTry_ReachDc_Prob^2

    return(all_TwoTries_ReachDc_Prob)
  } else {
    stop("Invalid circumstance argument.")
  }
}


prob_roll <- function(dice_sides, targetRoll, mod = 0, circumstance = "normal") {
  mod_dice_sides <- dice_sides + mod

  # Usually this would be just 1/dice_sides. But taking into,
  # account the role of mod makes this different.
  singleTry_IsTargetRoll_Prob <- 
    ifelse(1 + mod <= targetRoll & targetRoll <= dice_sides + mod,
           1 / dice_sides,
           0.0)

  if (circumstance == "normal") {
    return(singleTry_IsTargetRoll_Prob)
  } else if (circumstance == "advantage") {
    complement_SingleTry_IsTargetRoll_Prob <- 1 - singleTry_IsTargetRoll_Prob

    all_Complement_TwoTries_ReachDc_Prob <- complement_SingleTry_IsTargetRoll_Prob^2

    any_TwoTries_IsTargetRoll_Prob <- 1 - all_Complement_TwoTries_ReachDc_Prob

    return(any_TwoTries_IsTargetRoll_Prob)
  } else if (circumstance == "disadvantage") {

    numOfModifiedOutcomesBelowTargetRoll <- targetRoll + mod

    singleTry_IsTargetRollOrLower_Prob <- 
      ifelse(numOfModifiedOutcomesBelowTargetRoll >= 0,
             numOfModifiedOutcomesBelowTargetRoll / dice_sides,
             0.0)

    takeLower_TwoTries_ReachDc_Prob <-
        singleTry_IsTargetRoll_Prob * singleTry_IsTargetRollOrLower_Prob

    return(takeLower_TwoTries_ReachDc_Prob)
  } else {
    stop("Invalid circumstance argument.")
  }
}


prob_distribution <- function(dice_sides, mod, circumstance = "normal") {

  maxRollVal <- dice_sides + mod 
  minRollVal <- 1 + mod
  
  probsTheoretical <-
    prob_roll(dice_sides = dice_sides,
              targetRoll = minRollVal:maxRollVal,
              mod = mod,
              circumstance = circumstance
            )

  df <- data.frame(DiceRoll = minRollVal:maxRollVal, 
                   Probability = probsTheoretical,
                   Circumstance = circumstance)

  return(df)
}

probabilityMassFunction_Df <-
  bind_rows(
    prob_distribution(20, -2, "normal"),
    prob_distribution(20, -2, "advantage"),
    prob_distribution(20, -2, "disadvantage")
)

probabilityMassFunction_Df |>
  ggplot(aes(x = DiceRoll, y = Probability)) + 
  geom_col(aes(fill = Circumstance), position = "dodge") +
  theme_bw()