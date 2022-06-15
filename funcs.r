############# calculate z_score
z_score_calc <- function(risk, or_vec){
  z_score <- round((risk - mean(or_vec)) / sd(or_vec), 4)
  
  return(z_score)
}

############# color vector selector
select_colors <- function(df){
  if (all(df$Class == c("Greater", "Less", "Equal"))) {
    colvec <-  c(colorpalette[3], colorpalette[1], colorpalette[4])
  } else if (all(df$Class == c("Less"))) {
    colvec <- c(colorpalette[1])
  } else if (all(df$Class == c("Greater"))) {
    colvec <- c(colorpalette[3])
  } else if (all(df$Class == c("Less", "Equal"))) {
    colvec <- c(colorpalette[1], colorpalette[4])
  } else if (all(df$Class == c("Greater", "Equal"))) {
    colvec <- c(colorpalette[3], colorpalette[4])
  } else {                                         # in this condition df$Class == c("Less", "Greater")
    colvec <- c(colorpalette[3], colorpalette[1])
  }
  return(colvec)
}