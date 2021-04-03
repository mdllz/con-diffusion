# require >= 90% valid responses to include participant data
required_min_perc_valid_responses <- .9

# valid responses are >= 1 for orig or util
mark_valid_responses <- function(autdat) {
  valid_response_condition <- !(autdat$originality<1 | autdat$utility<1)
  autdat$valid_response <- valid_response_condition 
  return(autdat)
}

# function to clean aut data. the dataframe must include cols
# respondent_id, originality, utility
# input: dataframe with rows where originality or utility < 1
# output: dataframe without rows where originality or utility < 1
clean_aut_data <- function(autdat) {
  num_participants_before <- length(unique(autdat$respondent_id))
  num_responses_before <- nrow(autdat)
  cat("\nAUT data cleaning with required min % responses = ", 
      required_min_perc_valid_responses, ".")
  cat("\nNumber responses before:", num_responses_before)
  cat("\nNumber participants before:", num_participants_before)
  
  valid_responses <- mark_valid_responses(autdat)
  valid_participants <- get_valid_participants(valid_responses)
  cleandat <- autdat[autdat$respondent_id %in% valid_participants,]
  
  num_responses_after <- nrow(cleandat)
  num_participants_after <- length(unique(cleandat$respondent_id))
  num_responses_excluded <- num_responses_before - num_responses_after
  num_participants_excluded <- num_participants_before - num_participants_after
  cat("\nNumber responses excluded:", num_responses_excluded)
  cat("\nNumber participants excluded:", 
      num_participants_excluded, "\n")
  return(cleandat)
}

get_valid_participants <- function(autdat_valid_responses) {
  perc_valid <- tapply(autdat_valid_responses$valid_response,
                       autdat_valid_responses$respondent_id,
                       mean)
  hist(perc_valid,20)
  valid_pp <- as.numeric(names(
    perc_valid[perc_valid >required_min_perc_valid_responses]
    ))
  return(valid_pp)
}

create_aut_testdat <- function() {
  respondent_id <- c(101, 102, 103, 103, 104, 105)
  originality <- c(0, .5, .5, 2.5, 3, 5)
  utility <- c(5, 3.5, 0, 2, 1, .5)
  autdat <- data.frame(respondent_id, originality, utility)
}

test_mark_valid_responses <- function(autdat) {
  mark_valid_test <- mark_valid_responses(autdat)
  valid_responses_test <- mark_valid_test$valid_response
  valid_responses <- c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)
  cat("\ntest mark_valid_responses: ")
  if (identical(valid_responses, valid_responses_test)) {
    cat("PASSED")
  } else {
    cat("FAILED")
  }
}

test_get_valid_participants <- function(autdat) {
  autdat_valid_responses <- mark_valid_responses(autdat)
  test_valid_participants <- get_valid_participants(autdat_valid_responses)
  valid_participants <- c(104)
  cat("\ntest get_valid_participants: ")
  if (identical(test_valid_participants, valid_participants)) {
    cat("PASSED")
  } else {
    cat("FAILED")
  }
}

test_clean_aut_data <- function(autdat) {
  test_autdat_clean <- clean_aut_data(autdat)
  rownames(test_autdat_clean) <- 1:nrow(test_autdat_clean)
  autdat_clean <- data.frame(respondent_id=104, 
                             originality=3, utility=1)
  cat("\ntest clean_aut_data: ")
  if (identical(autdat_clean, test_autdat_clean)) {
    cat("PASSED")
  } else {
    cat("FAILED")
  }
}

run_aut_data_cleaning_tests <- function() {
  autdat <- create_aut_testdat()
  test_mark_valid_responses(autdat)
  test_get_valid_participants(autdat)
  test_clean_aut_data(autdat)
}

# run_aut_data_cleaning_tests()
