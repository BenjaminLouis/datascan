# FONCTIONS POUR LE V DE CRAMER
# L'argument unique est un jeu de données
scan_cramerv <- function(data){
  cv.test <- function(x,y) {
    CV <- sqrt(chisq.test(x, y, simulate.p.value = TRUE)$statistic /
                 (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
    return(as.numeric(CV))
  }
  quali <- data %>%
    select_if(function(x) is.character(x) | is.factor(x)) %>%
    colnames()
  result <- crossing(quali1 = quali, quali2 = quali) %>%
    mutate(cramV = map2_dbl(quali1, quali2, ~ cv.test(unlist(data[,.x]),unlist(data[,.y])))) %>%
    spread(key = "quali1", value = cramV)
  result <- as.data.frame(result)
  row.names(result) <- unlist(result[, 1])
  result <- as.matrix(result[,-1])
  corrplot(result, method = "color", type = "lower", number.cex = 0.7,
           addCoef.col = "black", tl.col = "red", tl.srt = 45, tl.cex = 0.8,
           diag = FALSE, is.corr = F)
  return(result)
}
