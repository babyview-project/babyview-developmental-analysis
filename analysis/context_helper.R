# Location smoothing, assumes that locations are being passed in two columns, a location column with comma-separated values and a probability with corresponding comma-separated values
# Helper function to parse location strings once
parse_location_frame <- function(location_str, prob_str) {
  if (is.na(location_str) || is.na(prob_str)) {
    return(list(locations = character(0), probs = numeric(0)))
  }

  locations <- trimws(strsplit(location_str, ",")[[1]])
  probs <- suppressWarnings(as.numeric(strsplit(prob_str, ",")[[1]]))

  # Filter valid entries
  valid <- !is.na(probs) & nzchar(locations) & length(locations) == length(probs)

  list(locations = locations[valid], probs = probs[valid])
}

# Optional normalization function to get rid of overlapping categories?
normalize_location <- function(loc) {
  loc <- tolower(trimws(loc))
  if (loc %in% c("backyard", "outside")) return("outside")
  return(loc)
}

# Main smoothing function - handles all cases
smooth_locations <- function(location_options, location_probs,
                             width = 5,
                             normalize = FALSE,
                             weight_type = "triangular") {

  n <- length(location_options)
  if (n == 0) return(character(0))
  if (width %% 2 == 0) stop("Width must be odd")

  # Pre-parse all frames
  parsed_frames <- Map(parse_location_frame, location_options, location_probs)

  # Generate weights based on type
  weights <- switch(weight_type,
                    "triangular" = {
                      center <- ceiling(width/2)
                      sapply(1:width, function(i) center - abs(i - center) + 1)
                    },
                    "uniform" = rep(1, width),
                    stop("Unknown weight_type")
  )
  weights <- weights / sum(weights)

  result <- rep(NA_character_, n)
  half_width <- floor(width/2)

  for (i in 1:n) {
    # Determine window
    start_idx <- max(1, i - half_width)
    end_idx <- min(n, i + half_width)

    # Get corresponding weights for this window
    weight_start <- half_width - (i - start_idx) + 1
    weight_end <- weight_start + (end_idx - start_idx)
    window_weights <- weights[weight_start:weight_end]
    window_weights <- window_weights / sum(window_weights)  # Renormalize

    # Aggregate scores
    location_scores <- numeric()
    names(location_scores) <- character()

    for (j in seq_along(window_weights)) {
      frame_idx <- start_idx + j - 1
      frame_data <- parsed_frames[[frame_idx]]
      weight <- window_weights[j]

      if (length(frame_data$locations) > 0) {
        for (k in seq_along(frame_data$locations)) {
          loc <- frame_data$locations[k]
          if (normalize) loc <- normalize_location(loc)

          if (!loc %in% names(location_scores)) {
            location_scores[loc] <- 0
          }
          location_scores[loc] <- location_scores[loc] + (frame_data$probs[k] * weight)
        }
      }
    }

    if (length(location_scores) > 0) {
      result[i] <- names(location_scores)[which.max(location_scores)]
    }
  }

  return(result)
}

# Simple wrapper functions for specific use cases
smooth_locations_weighted <- function(location_options, location_probs, width = 5) {
  smooth_locations(location_options, location_probs, width,
                   normalize = FALSE, weight_type = "triangular")
}

smooth_locations_uniform_normalized <- function(location_options, location_probs, width = 5) {
  smooth_locations(location_options, location_probs, width,
                   normalize = TRUE, weight_type = "uniform")
}

smooth_locations_normalized <- function(location_options, location_probs, width = 5) {
  smooth_locations(location_options, location_probs, width,
                   normalize = TRUE, weight_type = "triangular")
}

get_max_prob_location <- function(location_options, location_probs) {
  parsed <- parse_location_frame(location_options, location_probs)
  if (length(parsed$locations) == 0) {
    return(list(location = NA, prob = NA))
  }

  max_idx <- which.max(parsed$probs)
  list(location = parsed$locations[max_idx], prob = parsed$probs[max_idx])
}

