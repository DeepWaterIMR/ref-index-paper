# Run the public redfish index showcase end to end.

source("R/01_prepare_public_data.R")
source("R/02_fit_showcase_model.R")
source("R/03_public_audit.R")

prepared_data <- prepare_public_data()

if (!identical(Sys.getenv("SKIP_MODEL"), "true")) {
  showcase_model <- fit_showcase_model()
}

audit_result <- audit_public_surface()

message("Public redfish showcase workflow completed successfully.")
