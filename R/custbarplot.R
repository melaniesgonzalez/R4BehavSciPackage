#' Make a bar plot of responses on Likert scale
#'
#' @param data Name of dataset (must be in wide format, i.e., one row per participant)
#' @param xvar Name of variable of interest measured on Likert scale
#' @param grpvar Name of variable of type factor by which bars should be split
#' @param withingrp Logical value indicating whether or not percentages should be calculated within groups
#' @param scalepoints Numeric value indicating number of scalepoints in Likert scale (at present, can take on value of 5 or 7)
#' @param decimals Logical value indicating whether or not xvar is on a non-integer scale
#' @param scale Character value indicating scale labels (at present, can take on values of "Agree", "Quant", or "True")
#' @param ... Other arguments to pass on to ggplot
#' @return A barplot showing the counts and percentages of all values of xvar
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 "aes"
#' @importFrom ggplot2 "element_rect"
#' @importFrom ggplot2 "geom_col"
#' @importFrom ggplot2 "geom_text"
#' @importFrom ggplot2 "labs"
#' @importFrom ggplot2 "position_dodge2"
#' @importFrom ggplot2 "position_dodge"
#' @importFrom ggplot2 "scale_fill_discrete"
#' @importFrom ggplot2 "scale_x_continuous"
#' @importFrom ggplot2 "scale_y_continuous"
#' @importFrom ggplot2 "theme"
#' @importFrom ggplot2 "theme_bw"
#' @examples
#' mtcars$am <- as.factor(mtcars$am)
#' custbarplot(mtcars, xvar = gear, grpvar = am, withingrp = TRUE, scalepoints = 5, decimals = FALSE)
#' @export

custbarplot <- function(data, xvar, grpvar, withingrp = FALSE, scalepoints = 7, decimals = TRUE, scale = "Agree", ...) {

  n <- NULL
  xvar1 <- rlang::enquo(xvar)
  grpvarxvar <- rlang::enquos(grpvar, xvar)

  if (missing(grpvar)) {
    count_dataset <- data %>%
      dplyr::group_by(!! xvar1) %>%
      dplyr::summarize(n = n()) %>%
      dplyr::mutate(per = scales::percent(n / sum(n))) %>%
      tidyr::replace_na(list(n = 0)) # replace any NA values in n column with 0
    #return(count_dataset)

    out <- ggplot2::ggplot(count_dataset,
                           aes(x = dplyr::pull(count_dataset, 1),
                               y = dplyr::pull(count_dataset, 2))) +
      geom_col(position = position_dodge2(preserve = "total")) +
      labs(x = lazyeval::lazy(xvar),
           y = "count") +
      geom_text(aes(label = scales::percent(n / sum(n)),
                    y = n),
                vjust = -.5,
                size = 3,
                position = position_dodge(width = .8)) +
      geom_text(aes(label = n,
                    y = n),
                vjust = -2,
                hjust = -.15,
                size = 3,
                position = position_dodge(width = .8)) +
      geom_text(aes(label = "n = ",
                    y = n),
                vjust = -2,
                hjust = .85,
                size = 3,
                position = position_dodge(width = .8)) +
      scale_y_continuous(limits = c(0, max(dplyr::pull(count_dataset, 2)) * 1.1),
                         breaks = seq(0, max(dplyr::pull(count_dataset, 2)) * 1.1,
                                      by = 5)) +
      theme_bw() +
      theme()

    #return(out)
    #return(list(count_dataset, out))
  } else if (!missing(grpvar) & withingrp == TRUE){
    count_dataset <- data %>%
      dplyr::group_by(!!! grpvarxvar) %>%
      dplyr::summarize(n = n()) %>%
      dplyr::ungroup() %>%
      tidyr::complete(!!! grpvarxvar) %>%
      dplyr::arrange(!! xvar1) %>%
      tidyr::replace_na(list(n = 0)) %>% # replace any NA values in n column with 0
      dplyr::group_by(!! xvar1) %>%
      dplyr::mutate(per = scales::percent(n / sum(n)))
    #return(count_dataset)

    out <- ggplot2::ggplot(count_dataset, aes(x = dplyr::pull(count_dataset, 2),
                                                y = dplyr::pull(count_dataset, 3),
                                                fill = dplyr::pull(count_dataset, 1))) +
        geom_col(position = position_dodge2(preserve = "single")) +
        labs(x = lazyeval::lazy(xvar),
             y = "count") +
        scale_fill_discrete(name = lazyeval::lazy(grpvar)) +
        geom_text(aes(label = dplyr::pull(count_dataset, 4),
                      y = n),
                  vjust = -.5,
                  size = 3,
                  position = position_dodge(width = .8)) +
        geom_text(aes(label = n,
                      y = n),
              vjust = -2,
              hjust = -.15,
              size = 3,
              position = position_dodge(width = .8)) +
        geom_text(aes(label = "n = ",
                      y = n),
              vjust = -2,
              hjust = .85,
              size = 3,
              position = position_dodge(width = .8)) +
        scale_y_continuous(limits = c(0, max(dplyr::pull(count_dataset, 3)) * 1.1),
                           breaks = seq(0, max(dplyr::pull(count_dataset, 3)) * 1.1,
                                        by = 5)) +
        theme_bw() +
        theme(legend.position = c(.9,.9), legend.box.background = element_rect(colour = "black"))
    #return(out)
    #return(list(count_dataset, out))
  } else {
    count_dataset <- data %>%
      dplyr::group_by(!!! grpvarxvar) %>%
      dplyr::summarize(n = n()) %>%
      dplyr::ungroup() %>%
      tidyr::complete(!!! grpvarxvar) %>%
      dplyr::arrange(!! xvar1) %>%
      tidyr::replace_na(list(n = 0)) %>% # replace any NA values in n column with 0
      dplyr::mutate(per = scales::percent(n / sum(n)))
    #return(count_dataset)

    out <- ggplot2::ggplot(count_dataset, aes(x = dplyr::pull(count_dataset, 2),
                                                y = dplyr::pull(count_dataset, 3),
                                                fill = dplyr::pull(count_dataset, 1))) +
        geom_col(position = position_dodge2(preserve = "single")) +
        labs(x = lazyeval::lazy(xvar),
             y = "count") +
        scale_fill_discrete(name = lazyeval::lazy(grpvar)) +
        geom_text(aes(label = dplyr::pull(count_dataset, 4),
                      y = n),
                  vjust = -.5,
                  size = 3,
                  position = position_dodge(width = .8)) +
        geom_text(aes(label = n,
                      y = n),
              vjust = -2,
              hjust = -.15,
              size = 3,
              position = position_dodge(width = .8)) +
        geom_text(aes(label = "n = ",
                      y = n),
              vjust = -2,
              hjust = .85,
              size = 3,
              position = position_dodge(width = .8)) +
        scale_y_continuous(limits = c(0, max(dplyr::pull(count_dataset, 3)) * 1.1),
                           breaks = seq(0, max(dplyr::pull(count_dataset, 3)) * 1.1,
                                        by = 5)) +
        theme_bw() +
        theme(legend.position = c(.9,.9), legend.box.background = element_rect(colour = "black"))
    #return(out)
    #return(list(count_dataset, out))
    }

  if (scalepoints != 5 & scalepoints != 7){
    stop("Scalepoints argument must take on values of either 5 or 7")
  }

  if (decimals != TRUE & decimals != FALSE){
    stop("Decimals argument must take on logical value")
  }

  if (scale != "Agree" & scale != "Quant" & scale != "True"){
    stop("Scale argument must take on values of either 'Agree', 'Quant', or 'True'")
  }

  if (scalepoints == 7 & decimals == TRUE & scale == "Quant"){
    stop("Combination of scalepoints, decimals, and scale labels not pre-configured")
  }

  if (scalepoints == 7 & decimals == FALSE & scale == "Quant"){
    stop("Combination of scalepoints, decimals, and scale labels not pre-configured")
  }

  if (scalepoints == 7 & decimals == TRUE & scale == "True"){
    stop("Combination of scalepoints, decimals, and scale labels not pre-configured")
  }

  if (scalepoints == 7 & decimals == FALSE & scale == "True"){
    stop("Combination of scalepoints, decimals, and scale labels not pre-configured")
  }

  if (scalepoints == 7 & decimals == TRUE & scale == "Agree"){
    xlabels <- paste(seq(1, scalepoints,
                         by = .5),
                     c("Strongly \ndisagree",
                       "",
                       "Disagree",
                       "",
                       "Somewhat \ndisagree",
                       "",
                       "Neither \nagree not \ndisagree",
                       "",
                       "Somewhat \nagree",
                       "",
                       "Agree",
                       "",
                       "Strongly \nagree"),
                     sep = "\n")
  }

  if (scalepoints == 7 & decimals == FALSE & scale == "Agree"){
    xlabels <- paste(c(1:scalepoints),
                     c("Strongly \ndisagree",
                       "Disagree",
                       "Somewhat \ndisagree",
                       "Neither \nagree not \ndisagree",
                       "Somewhat \nagree",
                       "Agree",
                       "Strongly \nagree"),
                     sep = "\n")
  }

  if (scalepoints == 5 & decimals == TRUE & scale == "Agree"){
    xlabels <- paste(seq(1, scalepoints,
                         by = .5),
                     c("Strongly \ndisagree",
                       "",
                       "Disagree",
                       "",
                       "Neither \nagree not \ndisagree",
                       "",
                       "Agree",
                       "",
                       "Strongly \nagree"),
                     sep = "\n")
  }

  if (scalepoints == 5 & decimals == FALSE & scale == "Agree"){
    xlabels <- paste(c(1:scalepoints),
                     c("Strongly \ndisagree",
                       "Disagree",
                       "Neither \nagree not \ndisagree",
                       "Agree",
                       "Strongly \nagree"),
                     sep = "\n")
  }

  if (scalepoints == 5 & decimals == TRUE & scale == "Quant"){
    xlabels <- paste(seq(1, scalepoints,
                         by = .5),
                     c("Not at all",
                       "",
                       "Slightly",
                        "",
                        "Moderately",
                        "",
                        "Very",
                        "",
                        "Extremely"),
                     sep = "\n")
  }

  if (scalepoints == 5 & decimals == FALSE & scale == "Quant"){
    xlabels <- paste(c(1:scalepoints),
                     c("Not at all",
                       "Slightly",
                       "Moderately",
                       "Very",
                       "Extremely"),
                     sep = "\n")
  }

  if (scalepoints == 5 & decimals == TRUE & scale == "True"){
    xlabels <- paste(seq(1, scalepoints,
                         by = .5),
                     c("Not at \nall true",
                       "",
                       "Slightly \ntrue",
                       "",
                       "Moderately \ntrue",
                       "",
                       "Very \ntrue",
                       "",
                       "Extremely \ntrue"),
                     sep = "\n")
  }

  if (scalepoints == 5 & decimals == FALSE & scale == "True") {
    xlabels <- paste(c(1:scalepoints),
                     c("Not at \nall true",
                       "Slightly \ntrue",
                       "Moderately \ntrue",
                       "Very \ntrue",
                       "Extremely \ntrue"),
                     sep = "\n")
  }

  if (decimals ==TRUE) {
    outplot <- out +
      scale_x_continuous(limits = c(.5, scalepoints + .5),
                         breaks = seq(1, scalepoints,
                                      by = .5),
                         labels = xlabels)
    return(outplot)

  } else {
    outplot <- out +
      scale_x_continuous(limits = c(.5, scalepoints + .5),
                         breaks = seq(1, scalepoints),
                         labels = xlabels)
    return(outplot)

    }
}
