#install.packages("renv")
#Cool pacakge to be ensure you always have your package original versions
#look the foloowing web site
#https://rstudio.github.io/renv/articles/renv.html

renv::init()
renv::init()

#To create functions, always have the "_function" in the end
usethis::use_r("Dif_dist_function")
usethis::use_r("My_coefplot_function")
usethis::use_r("plot_ordination_function")
usethis::use_r("plot_null_function")
usethis::use_r("sr_value_function")


##To add documentation
devtools::document()

?renv

library(pelinson.et.al.2020)

data("treatments")

renv::snapshot()


renv::restore()

treatments

ID <- treatments$ID
fish <- treatments$fish
isolation <- treatments$isolation
survey <- treatments$survey

usethis::use_data(ID)


install.packages("ps")

#This is to add data
usethis::use_data()




#######SEMPRE QUE COMEÇAR
renv::restore()


#######QUANDO FOR PARAR
renv::snapshot()


usethis::use_f



###########################################################
?usethis
