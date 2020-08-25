#' All taxa abundances for all surveys
#'
#' A dataset containing the abundances of all taxa in all surveys.
#'
#' @format A data frame with 68 rows or 'observarions' and 37 variables, the last 36 being the sampled 'taxa':
#' \describe{
#'   \item{ID}{The identity code of the pond}
#'   ...
#' }
#' @source {Experimental Data}
"com_orig"


#' Taxa abundances for all surveys
#'
#' A dataset containing the abundances of the taxa that were present in more than three observations in all surveys.
#'
#' @format A data frame with 68 rows or 'observations' and 20 variables or 'taxa':
#' @source {Experimental Data}
"com"


#' Explanatory variables
#'
#' A dataset containing the explanatary variables for the experiment.
#'
#' @format A data frame with 68 rows or 'observations' and 4 variables:
#' \describe{
#'   \item{ID}{The identity code of the pond}
#'   \item{survey}{Number of the survey, '1' is the first, '2' is the second and '3' is the third}
#'   \item{fish}{Fish treatment. Fish were either 'present' or 'absent'}
#'   \item{isolation}{The isolation treatment. It could be either '30', '120' or '480'}
#' }
#' @source {Experimental Data}
"treatments"


#' Traits and other relevant information
#'
#' Traits and other relevant information about each taxa sampled
#'
#' @format A data frame with 20 rows or 'observations' and 16 variables:
#' \describe{
#'   \item{species}{Factor variable: Ids of the sampled taxa}
#'   \item{trophic}{Factor variable: Trophic level of the sampled taxa}
#'   \item{microhabitat}{Factor variable: Microhabitat level of the sampled taxa}
#'   \item{volume}{Numeric variable: Volume of the largest individual of each sampled taxa}
#'   \item{mass}{Numeric variable: Wet mass of the largest individual of each sampled taxa}
#'   \item{volume_log}{Numeric variable: Log transformed volume of the largest individual of each sampled taxa}
#'   \item{family}{Character variable: Family of each sampled taxa}
#'   \item{order}{Character variable:  Order of each sampled taxa}
#'   \item{total_ab}{Numeric variable: Total abundance of each sampled taxa considering all surveys}
#'   \item{total_ab_log}{Numeric variable: Log transformed total abundance of each sampled taxa considering all surveys}
#'   \item{total_ab_SS1}{Numeric variable: Total abundance of each sampled taxa considering the first survey}
#'   \item{total_ab_SS2}{Numeric variable: Total abundance of each sampled taxa considering the second survey}
#'   \item{total_ab_SS3}{Numeric variable: Total abundance of each sampled taxa considering the third survey}
#'   \item{total_ab_SS1_log}{Numeric variable: Log transformed total abundance of each sampled taxa considering the first survey}
#'   \item{total_ab_SS2_log}{Numeric variable: Log transformed total abundance of each sampled taxa considering the second survey}
#'   \item{total_ab_SS3_log}{Numeric variable: Log transformed total abundance of each sampled taxa considering the third survey}
#' }
#' @source {Experimental Data}
"TRAITS"

#' Isolation Treatment identities in the First Survey
#'
#' @format Factor w/ 3 levels: "30","120","480", and 24 observations
#' @source {Experimental Data}
"isolation_SS1"

#' Isolation Treatment identities in the Second Survey
#'
#' @format Factor w/ 3 levels: "30","120","480", and 24 observations
#' @source {Experimental Data}
"isolation_SS2"

#' Isolation Treatment identities in the Third Survey
#'
#' @format Factor w/ 3 levels: "30","120","480", and 20 observations
#' @source {Experimental Data}
"isolation_SS3"


#' Fish Treatment identities in the First Survey
#'
#' @format Factor w/ 2 levels: "absent","present", and 24 observations
#' @source {Experimental Data}
"fish_SS1"

#' Fish Treatment identities in the Second Survey
#'
#' @format Factor w/ 2 levels: "absent","present", and 24 observations
#' @source {Experimental Data}
"fish_SS2"

#' Fish Treatment identities in the Third Survey
#'
#' @format Factor w/ 2 levels: "absent","present", and 20 observations
#' @source {Experimental Data}
"fish_SS3"



#' Fish and Isolation Treatment identities in the First Survey
#'
#' @format Factor w/ 6 levels: ""30 absent", "120 absent", "480 absent", "30 present", "120 present", "480 present", and 24 observations
#' @source {Experimental Data}
"fish_isolation_SS1"

#' Fish and Isolation Treatment identities in the Second Survey
#'
#' @format Factor w/ 6 levels: ""30 absent", "120 absent", "480 absent", "30 present", "120 present", "480 present", and 24 observations
#' @source {Experimental Data}
"fish_isolation_SS2"

#' Fish and Isolation Treatment identities in the Third Survey
#'
#' @format Factor w/ 6 levels: ""30 absent", "120 absent", "480 absent", "30 present", "120 present", "480 present", and 20 observations
#' @source {Experimental Data}
"fish_isolation_SS3"


#' Traits and other relevant information
#'
#' Traits and other relevant information about each taxa sampled in the FIRST survey
#'
#' @format A data frame with 9 rows or 'observations' and 16 variables:
#' \describe{
#'   \item{species}{Factor variable: Ids of the sampled taxa}
#'   \item{trophic}{Factor variable: Trophic level of the sampled taxa}
#'   \item{microhabitat}{Factor variable: Microhabitat level of the sampled taxa}
#'   \item{volume}{Numeric variable: Volume of the largest individual of each sampled taxa}
#'   \item{mass}{Numeric variable: Wet mass of the largest individual of each sampled taxa}
#'   \item{volume_log}{Numeric variable: Log transformed volume of the largest individual of each sampled taxa}
#'   \item{family}{Character variable: Family of each sampled taxa}
#'   \item{order}{Character variable:  Order of each sampled taxa}
#'   \item{total_ab}{Numeric variable: Total abundance of each sampled taxa considering all surveys}
#'   \item{total_ab_log}{Numeric variable: Log transformed total abundance of each sampled taxa considering all surveys}
#'   \item{total_ab_SS1}{Numeric variable: Total abundance of each sampled taxa considering the first survey}
#'   \item{total_ab_SS2}{Numeric variable: Total abundance of each sampled taxa considering the second survey}
#'   \item{total_ab_SS3}{Numeric variable: Total abundance of each sampled taxa considering the third survey}
#'   \item{total_ab_SS1_log}{Numeric variable: Log transformed total abundance of each sampled taxa considering the first survey}
#'   \item{total_ab_SS2_log}{Numeric variable: Log transformed total abundance of each sampled taxa considering the second survey}
#'   \item{total_ab_SS3_log}{Numeric variable: Log transformed total abundance of each sampled taxa considering the third survey}
#' }
#' @source {Experimental Data}
"TRAITS_SS1"


#' Traits and other relevant information
#'
#' Traits and other relevant information about each taxa sampled in the SECOND survey
#'
#' @format A data frame with 18 rows or 'observations' and 16 variables:
#' \describe{
#'   \item{species}{Factor variable: Ids of the sampled taxa}
#'   \item{trophic}{Factor variable: Trophic level of the sampled taxa}
#'   \item{microhabitat}{Factor variable: Microhabitat level of the sampled taxa}
#'   \item{volume}{Numeric variable: Volume of the largest individual of each sampled taxa}
#'   \item{mass}{Numeric variable: Wet mass of the largest individual of each sampled taxa}
#'   \item{volume_log}{Numeric variable: Log transformed volume of the largest individual of each sampled taxa}
#'   \item{family}{Character variable: Family of each sampled taxa}
#'   \item{order}{Character variable:  Order of each sampled taxa}
#'   \item{total_ab}{Numeric variable: Total abundance of each sampled taxa considering all surveys}
#'   \item{total_ab_log}{Numeric variable: Log transformed total abundance of each sampled taxa considering all surveys}
#'   \item{total_ab_SS1}{Numeric variable: Total abundance of each sampled taxa considering the first survey}
#'   \item{total_ab_SS2}{Numeric variable: Total abundance of each sampled taxa considering the second survey}
#'   \item{total_ab_SS3}{Numeric variable: Total abundance of each sampled taxa considering the third survey}
#'   \item{total_ab_SS1_log}{Numeric variable: Log transformed total abundance of each sampled taxa considering the first survey}
#'   \item{total_ab_SS2_log}{Numeric variable: Log transformed total abundance of each sampled taxa considering the second survey}
#'   \item{total_ab_SS3_log}{Numeric variable: Log transformed total abundance of each sampled taxa considering the third survey}
#' }
#' @source {Experimental Data}
"TRAITS_SS2"


#' Traits and other relevant information
#'
#' Traits and other relevant information about each taxa sampled in the THIRD survey
#'
#' @format A data frame with 18 rows or 'observations' and 16 variables:
#' \describe{
#'   \item{species}{Factor variable: Ids of the sampled taxa}
#'   \item{trophic}{Factor variable: Trophic level of the sampled taxa}
#'   \item{microhabitat}{Factor variable: Microhabitat level of the sampled taxa}
#'   \item{volume}{Numeric variable: Volume of the largest individual of each sampled taxa}
#'   \item{mass}{Numeric variable: Wet mass of the largest individual of each sampled taxa}
#'   \item{volume_log}{Numeric variable: Log transformed volume of the largest individual of each sampled taxa}
#'   \item{family}{Character variable: Family of each sampled taxa}
#'   \item{order}{Character variable:  Order of each sampled taxa}
#'   \item{total_ab}{Numeric variable: Total abundance of each sampled taxa considering all surveys}
#'   \item{total_ab_log}{Numeric variable: Log transformed total abundance of each sampled taxa considering all surveys}
#'   \item{total_ab_SS1}{Numeric variable: Total abundance of each sampled taxa considering the first survey}
#'   \item{total_ab_SS2}{Numeric variable: Total abundance of each sampled taxa considering the second survey}
#'   \item{total_ab_SS3}{Numeric variable: Total abundance of each sampled taxa considering the third survey}
#'   \item{total_ab_SS1_log}{Numeric variable: Log transformed total abundance of each sampled taxa considering the first survey}
#'   \item{total_ab_SS2_log}{Numeric variable: Log transformed total abundance of each sampled taxa considering the second survey}
#'   \item{total_ab_SS3_log}{Numeric variable: Log transformed total abundance of each sampled taxa considering the third survey}
#' }
#' @source {Experimental Data}
"TRAITS_SS3"



#' Taxa abundances for the first survey
#'
#' A dataset containing the abundances of the taxa that were present in more than two observations in the FIRST survey.
#'
#' @format A data frame with 24 rows or 'observations' and 9 variables or taxa':
#' @source {Experimental Data}
"com_SS1"

#' Taxa abundances for the second survey
#'
#' A dataset containing the abundances of the taxa that were present in more than two observations in the SECOND survey.
#'
#' @format A data frame with 24 rows or 'observations' and 18 variables or taxa':
#' @source {Experimental Data}
"com_SS2"

#' Taxa abundances for the third survey
#'
#' A dataset containing the abundances of the taxa that were present in more than two observations in the THIRD survey.
#'
#' @format A data frame with 20 rows or 'observations' and 18 variables or taxa':
#' @source {Experimental Data}
"com_SS3"


#' Total abundance of predatory insects
#'
#' total abundance of predatory insects considering all surveys.
#'
#' @format Numeric variable with 68 observations
#' @source {Experimental Data}
"abundance_predators"


#' Total abundance of herbivores and detritivores
#'
#' total abundance of herbivores and detritivores  considering all surveys.
#'
#' @format Numeric variable with 68 observations
#' @source {Experimental Data}
"abundance_consumers"

