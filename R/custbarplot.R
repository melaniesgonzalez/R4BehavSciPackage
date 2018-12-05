#' Make a bar plot of responses on Likert scale
#'
#' @param data A dataset (must be in wide format, i.e., one row per participant)
#' @param xvar Variable of interest measured on Likert scale
#' @param scalepoints Numeric value indicating number of scalepoints in Likert scale (at present, can take on value of 5 or 7)
#' @param decimals Logical value indicating whether or not xvar is on a non-integer scale
#' @param scale Character value indicating scale labels (at present, can take on values of "Agree", "Quant", or "True")
#' @param ... Other arguments to pass on to ggplot
#' @return A barplot showing the counts and percentages of all values of xvar
#' @importFrom lazyeval "lazy"
#' @importFrom scales "percent"
#' @importFrom magrittr "%>%"
#' @importFrom tidyr "replace_na"
#' @importFrom ggplot2 "aes"
#' @importFrom ggplot2 "geom_col"
#' @importFrom ggplot2 "geom_text"
#' @importFrom ggplot2 "position_dodge"
#' @importFrom ggplot2 "labs"
#' @importFrom ggplot2 "scale_y_continuous"
#' @importFrom ggplot2 "scale_x_continuous"
#' @importFrom ggplot2 "theme_bw"
#' @importFrom ggplot2 "theme"
#' @examples
#' custbarplot(data = mtcars, xvar = gear, scalepoints = 5, decimals = FALSE)
#' @export

custbarplot = function(data = NULL, xvar = NULL, scalepoints = 7, decimals = TRUE, scale = "Agree", ...){
   n <- out5decimalfalse <- NULL
   count_dataset <- data %>%
      dplyr::count(.dots = lazyeval::lazy(xvar)) %>%
      tidyr::replace_na(list(n = 0)) # replace any NA values in n column with 0

    out <- ggplot2::ggplot(count_dataset, aes(x = dplyr::pull(count_dataset, 1), y = dplyr::pull(count_dataset, 2))) +
      geom_col(position = position_dodge(.4)) +
      labs(x = lazyeval::lazy(xvar), y = "count") +
      geom_text(aes(label = scales::percent(n / sum(n)), y = n),
            vjust = -.5,
            size = 3,
            position = position_dodge(width = .8)) +
      geom_text(aes(label = n, y = n),
            vjust = -2,
            hjust = -.15,
            size = 3,
            position = position_dodge(width = .8)) +
      geom_text(aes(label = "n = ", y = n),
            vjust = -2,
            hjust = .85,
            size = 3,
            position = position_dodge(width = .8)) +
      scale_y_continuous(limits = c(0, max(dplyr::pull(count_dataset, 2))*1.1), breaks = seq(0, max(dplyr::pull(count_dataset, 2))*1.1, by = 5)) +
      theme_bw() +
      theme()

    xlabels7decimalagree = paste(seq(1, 7, by = .5),c("Strongly \ndisagree", "", "Disagree", "", "Somewhat \ndisagree", "", "Neither \nagree not \ndisagree", "", "Somewhat \nagree", "", "Agree", "", "Strongly \nagree"), sep = "\n")
    xlabels7integeragree = paste(c(1:7),c("Strongly \ndisagree", "Disagree", "Somewhat \ndisagree", "Neither \nagree not \ndisagree", "Somewhat \nagree", "Agree", "Strongly \nagree"), sep = "\n")
    xlabels5decimalagree = paste(seq(1, 5, by = .5),c("Strongly \ndisagree", "", "Disagree", "", "Neither \nagree not \ndisagree", "", "Agree", "", "Strongly \nagree"), sep = "\n")
    xlabels5integeragree = paste(c(1:5),c("Strongly \ndisagree", "Disagree", "Neither \nagree not \ndisagree", "Agree", "Strongly \nagree"), sep = "\n")
    xlabels5decimalquant = paste(seq(1, 5, by = .5),c("Not at all", "", "Slightly", "", "Moderately", "", "Very", "", "Extremely"), sep = "\n")
    xlabels5integerquant = paste(c(1:5),c("Not at all", "Slightly", "Moderately", "Very", "Extremely"),sep = "\n")
    xlabels5decimaltrue = paste(seq(1, 5, by = .5), c("Not at \nall true", "", "Slightly \ntrue", "", "Moderately \ntrue", "", "Very \ntrue", "", "Extremely \ntrue"), sep = "\n")
    xlabels5integertrue = paste(c(1:5), c("Not at \nall true", "Slightly \ntrue", "Moderately \ntrue", "Very \ntrue", "Extremely \ntrue"), sep = "\n")

    out7decimalagree <- out +
      scale_x_continuous(limits = c(.5, 7.5), breaks = seq(1, 7, by = .5), labels = xlabels7decimalagree)
    out7integeragree <- out +
      scale_x_continuous(limits = c(.5, 7.5), breaks = seq(1, 7, by = 1), labels = xlabels7integeragree)
    out5decimalagree <- out +
      scale_x_continuous(limits = c(.5, 5.5), breaks = seq(1, 5, by = .5), labels = xlabels5decimalagree)
    out5integeragree <- out +
      scale_x_continuous(limits = c(.5, 5.5), breaks = seq(1, 5, by = 1), labels = xlabels5integeragree)
    out5decimalquant <- out +
      scale_x_continuous(limits = c(.5, 5.5), breaks = seq(1, 5, by = .5), labels = xlabels5decimalquant)
    out5integerquant <- out +
      scale_x_continuous(limits = c(.5, 5.5), breaks = seq(1, 5, by = 1), labels = xlabels5integerquant)
    out5decimaltrue <- out +
      scale_x_continuous(limits = c(.5, 5.5), breaks = seq(1, 5, by = .5), labels = xlabels5decimaltrue)
    out5integertrue <- out +
      scale_x_continuous(limits = c(.5, 5.5), breaks = seq(1, 5, by = 1), labels = xlabels5integertrue)

    if (scalepoints != 5 & scalepoints != 7){ return("Error: scalepoints argument must take on values of either 5 or 7") }
    if (decimals != TRUE & decimals != FALSE){ return("Error: decimals argument must take on logical value") }
    if (scale != "Agree" & scale != "Quant" & scale != "True"){ return("Error: scale argument must take on values of either 'Agree', 'Quant', or 'True'") }
    if (scalepoints == 7 & decimals == TRUE & scale == "Agree"){ return(out7decimalagree) }
    if (scalepoints == 7 & decimals == FALSE & scale == "Agree"){ return(out7integeragree) }
    if (scalepoints == 7 & decimals == TRUE & scale == "Quant"){ return("Error: combination of scalepoints, decimals, and scale labels not pre-configured") }
    if (scalepoints == 7 & decimals == FALSE & scale == "Quant"){ return("Error: combination of scalepoints, decimals, and scale labels not pre-configured") }
    if (scalepoints == 7 & decimals == TRUE & scale == "True"){ return("Error: combination of scalepoints, decimals, and scale labels not pre-configured") }
    if (scalepoints == 7 & decimals == FALSE & scale == "True"){ return("Error: combination of scalepoints, decimals, and scale labels not pre-configured") }
    if (scalepoints == 5 & decimals == TRUE & scale == "Agree"){ return(out5decimalagree) }
    if (scalepoints == 5 & decimals == FALSE & scale == "Agree"){ return(out5integeragree) }
    if (scalepoints == 5 & decimals == TRUE & scale == "Quant"){ return(out5decimalquant) }
    if (scalepoints == 5 & decimals == FALSE & scale == "Quant"){ return(out5integerquant) }
    if (scalepoints == 5 & decimals == TRUE & scale == "True"){ return(out5decimaltrue) }
    if (scalepoints == 5 & decimals == FALSE & scale == "True"){ return(out5integertrue) }
}
