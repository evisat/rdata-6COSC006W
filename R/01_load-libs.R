#This line of code installs the packman page if you do not have it installed - if you do, it loads the package
if(!require(pacman))install.packages("pacman")

#loads libraries - with the p_load function of the pacman library, if libraries are not found, it downloads them and then loads them for you
pacman::p_load('dplyr', 'ggplot2', 'stringr')

#Specifies options so as to avoid any numeric data being displayed using scientific notation
options(scipen=999)
