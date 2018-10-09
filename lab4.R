1. Read chapter 5 from "Advanced R, 2nd ed." H. Wickham (available online).
2. Create a GitHub repository Lab4 (files with R code for the assignment should be added to this repository)
3. Write the R code using ggplot function to recreate the graph from the Lecture 8 notes
curve(dnorm, from=-3, to=3, xlim=c(-3, 6), lwd=2, col=4, axes=FALSE, xlab=NA, ylab=NA)
curve(dnorm(x, mean=3.2), add=TRUE, col=2, lwd=2, from=0, to=6)
coord.x <- c(qnorm(0.95), seq(qnorm(0.95), 0, by=-0.01), 0)
coord.y <- c(0, dnorm(coord.x[-c(1, length(coord.x))], mean=3.2), 0) 
polygon(coord.x, coord.y, col=rgb(1, 0, 0, 0.5)) 
coord.x <- c(qnorm(0.95), seq(qnorm(0.95), 3, by=0.01), 3) 
coord.y <- c(0, dnorm(coord.x[-c(1, length(coord.x))]), 0) 
polygon(coord.x, coord.y, col=rgb(0, 0, 1, 0.5)) 
abline(v = qnorm(0.95), lty=2, lwd=2) 
text(1, 0.02, labels = expression(beta)) 
text(2, 0.02, labels = expression(alpha), col = "white") 
axis(1, at = c(0, 3.2), labels = c(expression(theta_0), expression(theta_a))) 
axis(2) 
box() 
legend("topleft", c("Type I error", "Type II error"), fill=c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))
ggplot(NULL, aes(x = c(-3, 6), y = c(0, 0.4))) +
  stat_function(fun = dnorm,
                args = list(mean = 0, sd = 1),
                lwd = 1,
                xlim = c(-3, 3),
                color = 'blue') +
  stat_function(fun = dnorm,
                xlim = c(qnorm(0.95), 3),
                geom = "area",
                alpha = 0.4,
                color = 'black',
                aes(fill = 'a')) + 
  stat_function(fun = dnorm,
                args = list(mean = 3.2, sd = 1),
                lwd = 1,
                xlim = c(0, 6),
                color = 'red') +
  stat_function(fun = dnorm,
                args = list(mean = 3.2, sd = 1),
                xlim = c(0, qnorm(0.95)), 
                geom = "area",
                alpha = 0.4,
                color = 'black',
                aes(fill = 'b')) +
  xlab("") +
  ylab("") +
  geom_vline(xintercept = qnorm(0.95),
             linetype = 'longdash',
             lwd = 0.7) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(color = 'black',
                                    fill = NA),
        legend.title = element_blank(),
        legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.box.margin = margin(c(1, 1, 1, 1)),
        legend.box.background = element_rect(color = 'black',
                                             fill = NA),
        legend.text = element_text(size = 14,
                                   margin = margin(l = 10)),
        axis.text.x = element_text(hjust = 0.5,
                                   size = 14,
                                   color = 'black',
                                   margin = margin(10, 0, 0, 0)),
        axis.text.y = element_text(angle = 90, 
                                   hjust = 0.5, 
                                   size = 14, 
                                   color = 'black',
                                   margin = margin(0, 10, 0, 0)),
        axis.ticks.length = unit(0.25, "cm")) +
  annotate("text", 
           x = 2,
           y = 0.02, 
           label = expression(alpha),
           color = 'white',
           size = 5) +
  annotate("text",
           x = 1,
           y = 0.02,
           label = expression(beta),
           color = 'black',
           size = 5) +
  scale_fill_manual(values = c("a" = "blue",
                               "b"= "red"), 
                    label = c("Type I error",
                              "Type II error")) +
  scale_x_continuous(breaks = c(0, 3.2), 
                     labels = c("theta_0", "theta_a"))

4. Implement a function that will check if a given positive integer is a prime number.
is.prime <- function (num) {
  # Check if the input is numeric.
  if (is.numeric(num) == F) {
    stop("The input should be a numeric value.")
  } else {
    # Check if the input is integer.
    check.integer <- function (num) {
      !grepl("[^[:digit:]]", format(num, digits = 20, scientific = F))
    }
    if (check.integer(num) == F) {
      stop("The input should be a positive integer.")
    } 
    else {
      # Check if the input is greater than +1.
      if (num <= 1) {
        return(F)
      } else if (num <= 3) {
        return(T)
      } else if (num %% 2 == 0 | num %% 3 == 0) {
        return(F)
      } else {
        i <- 5
        while (i * i <= num) {
          if (num %% i == 0 | num %% (i + 2) == 0) {
            return(F)
          } 
          i <- i + 6
        } 
        return(T)
      }
    }
  }
}
Test Sets
4.1 If the input is not numeric,
is.prime("Oleksandr")
4.2 If the input is not integer,
is.prime(123.123)
4.3 If the input is negative,
is.prime(-1)
4.4 From 0 to 100
for (i in 0:100) {
  if (is.prime(i) == T) {
    print(i)
  } else {
    print("No Prime")
  }
}