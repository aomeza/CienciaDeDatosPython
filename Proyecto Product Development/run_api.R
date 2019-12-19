library(plumber)
library(logger)

# Specify how logs are written
log_dir <- "logs"
if (!fs::dir_exists(log_dir)) fs::dir_create(log_dir)
log_appender(appender_tee(tempfile("plumber_", log_dir, ".log")))

convert_empty <- function(string) {
  if (string == "") {
    "-"
  } else {
    string
  }
}

r <- plumb("prediction_api.R")

r$registerHooks(
  list(
    postroute = function(req, res) {
      # Log details about the request and the response
      log_info('{req$args$username} {convert_empty(req$PATH_INFO)} "{convert_empty(req$HTTP_USER_AGENT)}" {Sys.time()} "{res$body}" {res$status}')
    }
  )
)

r$run(port=8001)

