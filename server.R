library(httpuv)
library(jsonlite)


# Convert an R object into an HTTP JSON response.
json_response <- function(data, status = 200L) {
  list(
    status = as.integer(status),
    headers = list(
      "Content-Type" = "application/json; charset=UTF-8"
    ),
    body = jsonlite::toJSON(
      data,
      auto_unbox = TRUE,
      null = "null",
      pretty = TRUE
    )
  )
}


# Convert a query string such as:
#
#   a=10&b=5
#
# into:
#
#   list(a = "10", b = "5")
parse_query <- function(query_string) {
  if (is.null(query_string) || !nzchar(query_string)) {
    return(list())
  }
  
  pairs <- strsplit(
    query_string,
    split = "&",
    fixed = TRUE
  )[[1]]
  
  result <- list()
  
  for (pair in pairs) {
    parts <- strsplit(
      pair,
      split = "=",
      fixed = TRUE
    )[[1]]
    
    key <- utils::URLdecode(parts[[1]])
    
    value <- if (length(parts) >= 2L) {
      utils::URLdecode(
        paste(parts[-1], collapse = "=")
      )
    } else {
      ""
    }
    
    result[[key]] <- value
  }
  
  result
}


# This object represents our web application.
app <- list(
  call = function(request) {
    
    method <- request$REQUEST_METHOD
    path <- request$PATH_INFO
    
    message(method, " ", path)
    
    # Only GET requests are supported by this example.
    if (!identical(method, "GET")) {
      return(
        json_response(
          data = list(
            error = "Method not allowed",
            allowed_methods = "GET"
          ),
          status = 405L
        )
      )
    }
    
    # GET /
    if (identical(path, "/")) {
      return(
        json_response(
          list(
            message = "Welcome to the simple R API",
            endpoints = c(
              "/health",
              "/api/v1/add?a=10&b=5"
            )
          )
        )
      )
    }
    
    # GET /health
    if (identical(path, "/health")) {
      return(
        json_response(
          list(
            status = "ok",
            time = format(
              Sys.time(),
              tz = "UTC",
              usetz = TRUE
            )
          )
        )
      )
    }
    
    # GET /api/v1/add?a=10&b=5
    if (identical(path, "/api/v1/add")) {
      query <- parse_query(request$QUERY_STRING)
      
      if (is.null(query$a) || is.null(query$b)) {
        return(
          json_response(
            data = list(
              error = "Both 'a' and 'b' are required",
              example = "/api/v1/add?a=10&b=5"
            ),
            status = 400L
          )
        )
      }
      
      a <- suppressWarnings(as.numeric(query$a))
      b <- suppressWarnings(as.numeric(query$b))
      
      if (is.na(a) || is.na(b)) {
        return(
          json_response(
            data = list(
              error = "'a' and 'b' must both be numbers"
            ),
            status = 400L
          )
        )
      }
      
      return(
        json_response(
          list(
            a = a,
            b = b,
            result = a + b
          )
        )
      )
    }
    
    # No route matched.
    json_response(
      data = list(
        error = "Endpoint not found",
        path = path
      ),
      status = 404L
    )
  }
)


message("Starting API at http://127.0.0.1:8000")
message("Press Esc or Ctrl+C to stop the server.")

httpuv::runServer(
  host = "127.0.0.1",
  port = 8000L,
  app = app
)