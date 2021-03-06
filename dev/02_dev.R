# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "thinkr" )

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "taxonomic" ) # Name of the module
golem::add_module( name = "plotly_bars" ) # Name of the module
golem::add_module( name = "plotly_pie" ) # Name of the module
golem::add_module( name = "plotly_line" ) # Name of the module
golem::add_module( name = "plotly_bubble" ) # Name of the module
golem::add_module( name = "leaflet" ) # Name of the module
golem::add_module( name = "DT" ) # Name of the module
golem::add_module( name = "field_selection" ) # Name of the module
golem::add_module( name = "plot_field_selector" ) # Name of the module



## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "create_group" ) 
golem::add_fct( "find_fields" ) 
golem::add_fct( "find_two_column_frequency" ) 
golem::add_fct( "yearly_trend_of_names" ) 
golem::add_fct( "find_field_for_plot" ) 

golem::add_utils( "helpers" )
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "dictionary", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "find_fields" )
usethis::use_test( "create_groups" )
usethis::use_test( "two_col_freq" )
usethis::use_test( "yearly_trend_of_names" )


# Documentation

## Vignette ----
usethis::use_vignette("dashboard.reactivfe")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

