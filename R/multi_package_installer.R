
# Package Checker,Validator & Installer
pkg_check_val_install <- function(packages_names_vec){
  
  # Function to check whether package is installed
  is.installed <- function(mypkg){
    is.element(mypkg, installed.packages()[,1])
  } 
  
  pkg_list = packages_names_vec
  pkgs_to_install = c()
  
  validator <- is.installed(mypkg = pkg_list)
  pos <- which(validator %in% c(FALSE))
  pkgs_to_install <- pkg_list[pos]
  
  # Checking if a Vector is empty or not
  # & Installing the packages only if the vector has some names
  
  if(length(pkgs_to_install) == 0){
    print("All the Listed Packages were installed")
  }else{
    print(paste("Installing required Packages",pkgs_to_install))
    install.packages(pkgs_to_install)
  }
  
}
