
# Multiple Libraries Loader
multi_LibLoader <- function(LibNames_Vec){
  sapply(LibNames_Vec, require, character.only = TRUE)
}