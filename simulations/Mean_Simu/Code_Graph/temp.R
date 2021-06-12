load("/Users/stephaneguerrier/Documents/GitHub/rcov/simulations/simu_mean_VAR2.RData")

gg_color_hue <- function(n, alpha = 1) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha = alpha)[1:n]
}

par(mfrow = c(4, 2),
    oma = c(3.5,6,5,1),
    mar = c(0,0,0.5,0.5) + 0.1, xpd=FALSE)

x_min = 1
x_max = 6
y_min = 1.25
y_max = c(5.5, 8.5, 5.5, 6)
x_text = 0
quant = c(0.5, 0.75, 0.95)
k = length(quant)
cols = gg_color_hue(k, alpha = 1)
cols_trans = gg_color_hue(k, alpha = 0.2)
my_grey = "grey93"
B = 500
H = 10^3
text_col = c("$p = 60$", "$p = 300$")
text_lign = c("Normal", "Pareto", "Log-Normal", "Student (df = 3)")

array_x = array_y = array(NA, c(512,4,2,6))
for (i in 1:4){
  for (j in 1:2){
    for (l in 1:6){
      inter = density(results[,l,j,i])
      array_x[,i, j, l] = inter$x
      array_y[,i, j, l] = inter$y
    }
  }
}
pch = c(15, 16, 17)
array_y = array_y/max(array_y)/2
my_grey = "grey85"

for (ligne in 1:4){
  for (col in 1:2){

    vals = upper = lower = matrix(NA, 6, k)

    for (i in 1:6){
      vals[i,] = quantile(results[,i,col,ligne], probs = quant)
      inter = matrix(NA, H, length(quant))
      for (h in 1:H){
        x_star = results[sample(B, replace = TRUE),i,col,ligne]
        inter[h,] = quantile(x_star, probs = quant)
      }
      for (m  in 1:k){
        upper[i,m] = quantile(inter[,m], probs = 0.975)
        lower[i,m] = quantile(inter[,m], probs = 0.025)
      }
    }

    plot(NA, xlim = c(x_min, x_max) + c(-0.5, 0.5), ylim = c(y_min, y_max[ligne]), axes = FALSE, ann = FALSE)

    abline(h = 2:floor(y_max[ligne]), col = my_grey)
    abline(v = 1:7, col = my_grey)

    for (hh in 1:6){
      polygon(hh + c(array_y[,ligne,col,hh], -rev(array_y[,ligne,col,hh])),
              c(array_x[,ligne,col,hh], rev(array_x[,ligne,col,hh])), col = my_grey, border = NA)
    }


    for (i in 1:k){
      polygon(c(1:6, rev(1:6)),
              c(lower[,i], rev(upper[,i])),
              border = NA, col = cols_trans[i])

      lines(1:6, vals[,i], col = cols[i], pch = pch[i], type= "b")
    }

    if (col == 1 && ligne == 2){
      legend("topright", c("$\\hat{q}_{50}$", "$\\hat{q}_{75}$", "$\\hat{q}_{95}$"),
             col = cols, pch = pch, lwd = 1, bty = "n")
    }
    if (col == 1){
      mtext(text_lign[ligne], side = 2, line = 4.5)
      mtext("$L_{\\infty}$ error ???", side = 2, line = 2.5)
      axis(2, at = c(2:floor(y_max[ligne])))

    }


    if (ligne == 1){
      mtext(text_col[col], side = 3, line = 2)
    }

    if (ligne == 4){
      axis(1, at = 1:6, labels = c("$\\hat{\\theta}}$?", "$\\hat{\\theta}}$?", "$\\hat{\\theta}}$?",
                                   "$\\hat{\\theta}}$?", "$\\hat{\\theta}}$?", "$\\hat{\\theta}}$?"))
    }
  }
}
