# Circular histogram ------------------------------------------------------------------------------

rm(list = ls())

load("timestamps.Rdata")
head(timestamps)

ts <- timestamps

# Make a circular histogram with geom_histogram() and coord_polar()
library(ggplot2)
clock <- ggplot(data.frame(ts), aes(x = ts)) +
  geom_histogram(breaks = seq(0, 24), colour = "blue", fill = "lightblue") +
  coord_polar() +
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24)) +
  theme(text = element_text(size = 25)) +
  ylab("")
plot(clock)


# Add arithmetic mean to the circular histogram
arithmetic_mean <- mean(ts)

clock + geom_vline(xintercept = arithmetic_mean,
                   linetype = 2, color = "red", size = 1.5)


# Convert the decimal timestamps to class "circular"
library(circular)
set.seed(2018)
ts <- rvonmises(n = 200, mu = circular(22, units = "hours"), kappa = 2)
ts <- circular(ts, units = "hours", template = "clock24")
head(ts)


# Estimate parameters mu (= periodic mean)
# and kappa (= concentration) from von Mises distribution
estimates <- mle.vonmises(ts)
p_mean <- estimates$mu %% 24
concentration <- estimates$kappa


# Add periodic mean to the circular histogram
clock <- clock + geom_vline(xintercept = as.numeric(p_mean),
                            linetype = 2, color = "red", size = 1.5)
plot(clock)


# Calculate the density (= likelihood) of the timestamps
# for the estimated von Mises distribution
densities <- dvonmises(ts, mu = p_mean, kappa = concentration)


# Binary time feature: TRUE if timestamp lies inside CI, FALSE otherwise
alpha <- 0.05

quantile <- qvonmises(1 - alpha, mu = p_mean, kappa = concentration) %% 24

cutoff <- dvonmises(quantile, mu = p_mean, kappa = concentration)
time_feature <- densities >= cutoff
head(time_feature)


# Compute confidence interval and add to circular histogram
lb <- qvonmises(1 - alpha, mu = p_mean, kappa = concentration) %% 24 # "left" boundary
ub <- qvonmises(alpha,     mu = p_mean, kappa = concentration) %% 24 # "right" boundary

i_ci <- which(densities >= cutoff) # = which(time_feature)

# Compute the density of midnight (00:00:00) (special case)
density0 <- dvonmises(circular(0, units = "hours", template = "clock24"),
                      mu = p_mean, kappa = concentration)

# Add confidence interval to circular histogram,
# depending on whether midnight lies inside the interval or not
ymax <- 35

if (density0 >= cutoff) {
  i_start <- which(ts <= min(lb, ub))
  i_end <- which(ts >= max(lb, ub))
  i_ci_start <- intersect(i_ci, i_start)
  i_ci_end <- intersect(i_ci, i_end)
  
  df.ci.start <- data.frame(ts = as.numeric(ts[i_ci_start]),
                            ymax = rep(ymax, length(i_ci_start)))
  df.ci.end <- data.frame(ts = as.numeric(ts[i_ci_end]),
                          ymax = rep(ymax, length(i_ci_end)))
  
  clock <- clock +
    geom_segment(aes(x = as.numeric(lb), y = 0, xend = as.numeric(lb), yend = ymax), linetype = 1, color = "orange") +
    geom_segment(aes(x = as.numeric(ub), y = 0, xend = as.numeric(ub), yend = ymax), linetype = 1, color = "orange") +
    geom_area(data = df.ci.start, aes(y = ymax), fill = "orange", alpha = 0.2) +
    geom_area(data = df.ci.end,   aes(y = ymax), fill = "orange", alpha = 0.2)
  
} else {
  df.ci <- data.frame(ts = as.numeric(ts[i_ci]),
                      ymax = rep(ymax, length(i_ci)))
  
  clock <- clock +
    geom_segment(aes(x = as.numeric(lb), y = 0, xend = as.numeric(lb), yend = ymax), linetype = 1, color = "orange") +
    geom_segment(aes(x = as.numeric(ub), y = 0, xend = as.numeric(ub), yend = ymax), linetype = 1, color = "orange") +
    geom_area(data = df.ci, aes(y = ymax), fill = "orange", alpha = 0.2)
}

plot(clock)


# Show timestamp of fraudulent transfer
clock + geom_vline(xintercept = 14, linetype = 1, color = "darkgreen", size = 1.5)


