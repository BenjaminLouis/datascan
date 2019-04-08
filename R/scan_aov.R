
# Fonctions de d'un équivalent de corrélation entre les données quantitatives
# et les données qualitatives
# L'argument unique est un jeu de données
scan_aov <- function(data) {
  quanti <- data %>%
    select_if(function(x) is.double(x) | is.integer(x)) %>%
    colnames()
  quali <- data %>%
    select_if(function(x) is.character(x) | is.factor(x)) %>%
    colnames()
  get_r2 <- function(x, y) {
    mod <- lm(form = as.formula(paste0(y,"~",x)), data = data, na.action = na.omit)
    sqrt(summary(mod)$r.squared)
  }
  wh <- which.max(c(length(quanti), length(quali)))
  result <- crossing(quanti,quali) %>%
    mutate(r = map2_dbl(quali, quanti, get_r2)) %>%
    spread(key = c("quanti", "quali")[wh], value = r)
  result <- as.data.frame(result)
  row.names(result) <- unlist(result[, 1])
  result <- result[,-1]
  if (ncol(result) > 25) {
    n <- ceiling(ncol(result)/2)
    par(mfrow = c(2, 1),     # 2x2 layout
        oma = c(0, 0, 0, 0), # two rows of text at the outer left and bottom margin
        mar = c(0,0, 0, 0), # space for one row of text at ticks and to separate plots
        mgp = c(0, 0, 0),    # axis label at 2 rows distance, tick labels at 1 row
        xpd = NA)
    corrplot(as.matrix(result[,1:n]), method = "circle", tl.col = "red",
             col = c(rep("white",5),brewer.pal(n = 5, name = "RdYlGn")), tl.cex = 0.7,
             tl.srt = 45, is.corr = FALSE, cl.lim = c(0, 1), cl.pos = "r")
    corrplot(as.matrix(result[,n:ncol(result)]), method = "circle", tl.col = "red",
             col = c(rep("white",5),brewer.pal(n = 5, name = "RdYlGn")), tl.cex = 0.7,
             tl.srt = 45, is.corr = FALSE, cl.lim = c(0, 1), cl.pos = "r")
    par(mfrow = c(1, 1))
  } else {
    corrplot(as.matrix(result), method = "circle", tl.col = "red",
             col = c(rep("white",5),brewer.pal(n = 5, name = "RdYlGn")), tl.cex = 0.7,
             tl.srt = 90, is.corr = FALSE, cl.lim = c(0, 1))
  }
  return(as.matrix(result))
}

#------------------------------------------------------------------------

