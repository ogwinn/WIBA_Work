# Install required packages if not already installed
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")

# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)

# Base URL of the API
BASE_URL = 'http://wiba.dev/api'


# Create text segments using the API's create_segments endpoint.
create_segments <- function(input_file, column_name, window_size = 3) {
  library(httr)
  library(jsonlite)
  library(dplyr)
  
  # Define the API endpoint URL
  url <- paste0(BASE_URL, "/create_segments")
  
  # Read the CSV file content as a string to be sent to the API
  csv_content <- tryCatch({
    paste(readLines(input_file, encoding = "UTF-8"), collapse = "\n")
  }, error = function(e) {
    stop("Error reading the input file. Please check the file path and encoding.")
  })
  
  # Prepare the payload for the POST request
  payload <- list(
    data = csv_content,
    column_name = column_name,
    window_size = window_size,
    step_size = 1
  )
  
  # Make the POST request
  response <- POST(url, body = toJSON(payload, auto_unbox = TRUE), encode = "json")
  
  # Check the response status
  if (status_code(response) == 200) {
    # Parse the JSON response into a dataframe
    result_json <- tryCatch({
      content(response, as = "parsed", type = "application/json")
    }, error = function(e) {
      stop("Error parsing the API response. Please check the API endpoint and response format.")
    })
    
    segments_df <- fromJSON(toJSON(result_json), flatten = TRUE) %>% as.data.frame(stringsAsFactors = FALSE)
    
    # Ensure all columns are either strings or numbers
    segments_df <- segments_df %>% mutate(across(where(is.list), ~ sapply(., toString)))
    
    # Print the segments
    print("Created segments:")
    print(segments_df)
    
    return(segments_df)
  } else {
    print(paste("Error:", status_code(response), content(response, as = "text")))
    return(NULL)
  }
}

# Function to extract information from given texts using the API's extract endpoint
wiba_extract <- function(texts) {
  
  # Define the API endpoint URL
  url <- paste0(BASE_URL, "/extract")
  
  # Prepare the payload for the POST request
  payload <- list(texts = texts)
  
  # Make the POST request
  response <- POST(url, body = toJSON(payload, auto_unbox = TRUE), encode = "json")
  
  # Check the response status
  if (status_code(response) == 200) {
    # Parse the JSON response into a dataframe
    result_json <- tryCatch({
      content(response, as = "parsed", type = "application/json")
    }, error = function(e) {
      stop("Error parsing the API response. Please check the API endpoint and response format.")
    })
    
    extract_df <- fromJSON(toJSON(result_json), flatten = TRUE) %>% as.data.frame(stringsAsFactors = FALSE)
    
    # Ensure all columns are either strings or numbers
    extract_df <- extract_df %>% mutate(across(where(is.list), ~ sapply(., toString)))
    
    # Print the extraction results
    print("Extraction results:")
    print(extract_df)
    
    return(extract_df)
  } else {
    print(paste("Error:", status_code(response), content(response, as = "text")))
    return(NULL)
  }
}

# Function to detect information in texts from a DataFrame using the API's detect endpoint
wiba_detect <- function(df) {
  library(httr)
  library(jsonlite)
  library(dplyr)
  
  # Define the API endpoint URL
  url <- paste0(BASE_URL, "/detect")
  
  # Convert the DataFrame to CSV content
  temp_file <- tempfile(fileext = ".csv")
  write.csv(df, temp_file, row.names = FALSE, fileEncoding = "UTF-8")
  csv_data <- tryCatch({
    paste(readLines(temp_file, encoding = "UTF-8"), collapse = "\n")
  }, error = function(e) {
    stop("Error reading the temporary file. Please check the file path and encoding.")
  })
  
  # Prepare the payload for the POST request
  payload <- list(texts = csv_data)
  
  # Make the POST request
  response <- POST(url, body = toJSON(payload, auto_unbox = TRUE), encode = "json")
  
  # Check the response status
  if (status_code(response) == 200) {
    # Parse the JSON response into a dataframe
    result_json <- tryCatch({
      content(response, as = "parsed", type = "application/json")
    }, error = function(e) {
      stop("Error parsing the API response. Please check the API endpoint and response format.")
    })
    
    detect_df <- fromJSON(toJSON(result_json), flatten = TRUE) %>% as.data.frame(stringsAsFactors = FALSE)
    
    # Ensure all columns are either strings or numbers
    detect_df <- detect_df %>% mutate(across(where(is.list), ~ sapply(., toString)))
    
    # Print the detection results
    print("Detection results:")
    print(detect_df)
    
    return(detect_df)
  } else {
    print(paste("Error:", status_code(response), content(response, as = "text")))
    return(NULL)
  }
}

# Function to analyze the stance of texts on given topics using the API's stance endpoint
wiba_stance <- function(texts, topics) {
  library(httr)
  library(jsonlite)
  library(dplyr)
  
  # Define the API endpoint URL
  url <- paste0(BASE_URL, "/stance")
  
  # Prepare the payload for the POST request
  payload <- list(texts = texts, topics = topics)
  
  # Make the POST request
  response <- POST(url, body = toJSON(payload, auto_unbox = TRUE), encode = "json")
  
  # Check the response status
  if (status_code(response) == 200) {
    # Parse the JSON response into a dataframe
    result_json <- tryCatch({
      content(response, as = "parsed", type = "application/json")
    }, error = function(e) {
      stop("Error parsing the API response. Please check the API endpoint and response format.")
    })
    
    stance_df <- fromJSON(toJSON(result_json), flatten = TRUE) %>% as.data.frame(stringsAsFactors = FALSE)
    
    # Ensure all columns are either strings or numbers
    stance_df <- stance_df %>% mutate(across(where(is.list), ~ sapply(., toString)))
    
    # Print the stance analysis results
    print("Stance results:")
    print(stance_df)
    
    return(stance_df)
  } else {
    print(paste("Error:", status_code(response), content(response, as = "text")))
    return(NULL)
  }
}

# Example usage (assuming BASE_URL is defined):
make_segments <- TRUE

# Create a sample dataframe with text data
text_data <- data.frame(
  text = c(
    "The new policy will help reduce traffic congestion in the city, so we should approve it.",
    "The new policy will increase traffic congestion in the city because it will lead to more vehicles on the road.",
    "The new policy will have no impact on traffic congestion in the city and is a waste of time."
  )
)

if (make_segments) {
  input_file <- tempfile(fileext = ".csv")
  write.csv(text_data, input_file, row.names = FALSE, fileEncoding = "UTF-8")
  create_segments_df <- create_segments(input_file, "text", 3)
  print(create_segments_df)
  
  if (!is.null(create_segments_df)) {
    # Detect texts
    detect_results <- wiba_detect(create_segments_df)
    print("Detection results:")
    print(detect_results)
    
    if (!is.null(detect_results)) {
      # Extract topics from texts
      texts <- detect_results$text
      extract_results <- wiba_extract(texts)
      print("Extraction results:")
      print(extract_results)
      
      # Rename extracted_topic to topic
      if (!is.null(extract_results) && "extracted_topic" %in% names(extract_results)) {
        extract_results <- extract_results %>% rename(topic = extracted_topic)
        print("Renamed extraction results:")
        print(extract_results)
        
        # Analyze stance on topics
        topics <- extract_results$topic
        stance_results <- wiba_stance(texts, topics)
        print("Stance results:")
        print(stance_results)
        
        # Combine detect_df, extract_df, stance_df
        if (!is.null(stance_results)) {
          combined_results <- cbind(detect_results, extract_results, stance_results)
          print("Combined results:")
          print(combined_results)
        }
      }
    }
  }
} else {
  # Detect texts
  detect_results <- wiba_detect(text_data)
  print("Detection results:")
  print(detect_results)
  
  if (!is.null(detect_results)) {
    # Extract topics from texts
    texts <- detect_results$text
    extract_results <- wiba_extract(texts)
    print("Extraction results:")
    print(extract_results)
    
    # Rename extracted_topic to topic
    if (!is.null(extract_results) && "extracted_topic" %in% names(extract_results)) {
      extract_results <- extract_results %>% rename(topic = extracted_topic)
      print("Renamed extraction results:")
      print(extract_results)
      
      # Analyze stance on topics
      topics <- extract_results$topic
      stance_results <- wiba_stance(texts, topics)
      print("Stance results:")
      print(stance_results)
      
      # Combine detect_df, extract_df, stance_df
      if (!is.null(stance_results)) {
        combined_results <- cbind(detect_results, extract_results, stance_results)
        print("Combined results:")
        print(combined_results)
      }
    }
  }
}