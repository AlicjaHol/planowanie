library(tidyverse)

ggplot(data = data.frame(x = c(-4,4)),
       aes(x))+
  stat_function(fun = dnorm)+
  stat_function(fun = dt, 
                args = list(df = 2),
                color = "red")+
  stat_function(fun = dchisq, args = list(df = 4), color = "blue")

qnorm(0.975)
qt(0.975, df = 3)

set.seed(2020)
proby <- 1:100 %>% map(~rchisq(15, df = 2))
ggplot(as.data.frame(proby[[2]]),
       aes(proby[[2]]))+
  geom_density(fill = "grey")

proby %>% 
  map_dbl(~mean(.x)) %>% 
  as.data.frame() %>% 
  ggplot(., aes(.))+
  geom_density(fill = "grey")

library(mosaic)

proby2 <- do(100)*rchisq(10, df = 1)
dt.srednie <- data.frame(m = apply(proby2, 1, mean))
head(dt.srednie)
proby2 %>% 
  t() %>% 
  as.data.frame() %>% 
  select(V1:V9) %>% 
  pivot_longer(cols = V1:V9) %>% 
  ggplot(aes(value))+
  geom_density(fill = 'grey')+
  facet_wrap(~name)

dt.srednie %>% 
  ggplot(., aes(m))+
  geom_density(fill = "grey")

przedzialy <- apply(proby2, 1, function(x) confint(t.test(x)))
przedzialy %>% 
  map_dbl(~ifelse(.x$lower < 1 & .x$upper > 1, 1, 0)) %>% 
  sum

library(e1071)
skosnosci <- proby2 %>% 
  apply(., 1, skewness)
mean(skosnosci)

nmin <- 28+25*(mean(skosnosci))^2
nmin

proby3 <- do(100)*rchisq(58, df = 1)
dt.srednie2 <- data.frame(m = apply(proby3, 1, mean))
head(dt.srednie2)
proby3 %>% 
  t() %>% 
  as.data.frame() %>% 
  select(V1:V9) %>% 
  pivot_longer(cols = V1:V9) %>% 
  ggplot(aes(value))+
  geom_density(fill = 'grey')+
  facet_wrap(~name)

dt.srednie2 %>% 
  ggplot(., aes(m))+
  geom_density(fill = "grey")

przedzialy2 <- apply(proby3, 1, function(x) confint(t.test(x)))
przedzialy2 %>% 
  map_dbl(~ifelse(.x$lower < 1 & .x$upper > 1, 1, 0)) %>% 
  sum

powtorzena <- function(x) {
  proby3 <- do(100)*rchisq(58, df = 1)
  przedzialy2 <- apply(proby3, 1, function(x) confint(t.test(x)))
  przedzialy2 %>% 
    map_dbl(~ifelse(.x$lower < 1 & .x$upper > 1, 1, 0)) %>% 
    sum
}


1:100 %>% map_dbl(~powtorzena(.x)) %>% mean


x <- rt(12, df =14)
x
sd(x)
e <- 0.03

## n = (z*s)^/e^2

z_alfa <- qnorm(0.975)
nmin <- z_alfa^2*sd(x)^2/(0.03^2)
nmin

pu <- confint(t.test(x))
(pu$upper-pu$lower)/2


# N(12, 4)

proby <- seq(10, 100000, by =500) %>% 
  map(~rnorm(.x, mean = 12, sd = 4))

sdv <- proby %>% 
  map_dbl(~sd(.x))
m <- proby %>% 
  map_dbl(~mean(.x))

data.frame(n = seq(10, 100000, by =500),
           m = m) %>% 
  ggplot(aes(x = n, y = m))+
  geom_point(color = "red")+
  geom_line(color = "red")+
  geom_hline(yintercept = 12, linetype = 2)
