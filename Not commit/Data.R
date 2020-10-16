#Important data


use_data(coord,
                   com, #
                   com_orig, #
                   treatments, #
                   TRAITS, #
                   isolation_SS1, #
                   isolation_SS2,
                   isolation_SS3,
                   fish_SS1,
                   fish_SS2,
                   fish_SS3,
                   fish_isolation_SS1,
                   fish_isolation_SS2,
                   fish_isolation_SS3,
                   TRAITS_SS1,
                   TRAITS_SS2,
                   TRAITS_SS3,
                   com_SS1,
                   com_SS2,
                   com_SS3,
                   abundance_predators,
                   abundance_consumers)

abundance_predators #abundance of predatory insects in each observation
abundance_consumers #abundance of herbivores and detritivores in each observation


coord #Geographical coordinates of each pond

com_orig # Original communities

com # original communities excluding species that were not present in more than three observations.

ID # ID of communities

com_incomplete #Communities after removing ponds C4, C3, B3 and A4 from the first and second surveys. They were removed to achieve a balanced design ONLY to alanyse all samples together.

treatments # All relevant explanatory variables

#Important explanatory variables after removing ponds C4, C3, B3 and A4
ID_incomplete
SS_incomplete
fish_incomplete
isolation_incomplete

TRAITS #Traits and other relevant information about each taxa sampled

#Isolation explanatory variable for each survey
isolation_SS1#survey 1
isolation_SS2#survey 2
isolation_SS3#survey 3

#Fish explanatory variable for each survey
fish_SS1#survey 1
fish_SS2#survey 2
fish_SS3#survey 3

#Fish/isolation explanatory variable for each survey, this is only relevant for plots
fish_isolation_SS1#survey 1
fish_isolation_SS2#survey 2
fish_isolation_SS3#survey 3

#Community data for each survey.
com_SS1 #excluding species that were not present in more than two observations.
com_SS2 #excluding species that were not present in more than two observations.
com_SS3 #excluding species that were not present in more than two observations.

#Trait Data for each survey
TRAITS_SS1
TRAITS_SS2
TRAITS_SS3

treatments
