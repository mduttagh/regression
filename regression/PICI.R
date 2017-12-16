#To Show Confidence Interval and Prediction Interval on the fitted regression line
# fit = Linear Regression Fitted Model
# df1 = Data Frame
# new_df[2] = x-variable
# new_df[1] = y-variable
install.packages("car","ggpubr") # if not already installed
require(car)
require(ggpubr)
fit <- lm(mpg ~ disp, data = mtcars)
fr <- as.formula(fit$call[[2]])
frdeparse <- deparse(fr, width.cutoff = 500L)
scatterplot(
  fr,
  data = mtcars,
  smoother = loessLine,
  id.n = 5,
  reg.line = lm,
  pch = 19,
  main = paste("Fitted Line Plot: ", frdeparse),
  lwd = 2,
  by.groups = FALSE
)
temp_var <- predict(fit, interval = "prediction")
new_df <- cbind(mtcars, temp_var)
p <- ggscatter(
  data = new_df,
  x = colnames(new_df[2]),
  y = colnames(new_df[1]),
  add = c("reg.line"),
  add.params = list(color = "blue", fill = "lightgray"),
  conf.int = TRUE
) + geom_line(aes(y = lwr),
              data = new_df,
              color = "red",
              linetype = "dashed") + geom_line(aes(y = upr),
                                               data = new_df,
                                               color = "red",
                                               linetype = "dashed") + theme_pubr()
print(ggpar(
  p,
  main = paste(
    "Fitted Regression Line with 95% CI and 95% PI\n",
    colnames(new_df[2]),
    "vs.",
    colnames(new_df[1]),
    "\nAdj R2=",
    signif(summary(fit1)$adj.r.squared, 5),
    "Intercept=",
    signif(fit1$coef[[1]], 5),
    " Slope=",
    signif(fit1$coef[[2]], 5),
    " p=",
    signif(summary(fit1)$coef[2, 4], 5)
  ),
  font.main = c(16, "bold"),
  font.x = c(14, "bold"),
  font.y = c(14, "bold")
))
