## Set up pkg environment to manage cached data, loaded from Metadata
# 1. ebx5.cl_data: EBX code-lists
# 2. ebx5.gr_data: EBX group
# 3. ebx5.metadata: FishStat Metadata
ebx5_env <- new.env()

# Initialize placeholders for package data within ebx5_env
assign("ebx5.connection", NULL, envir = ebx5_env)
assign("ebx5.cl_data", NULL, envir = ebx5_env)
assign("ebx5.gr_data", NULL, envir = ebx5_env)

# Establish initial credentials for the Comtrade API.
.onLoad <- function(libname, pkgname) {
  #packageStartupMessage("EBX5 environment initialized")
}
