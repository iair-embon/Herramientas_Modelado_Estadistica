library(Lahman)
library(dplyr)
library(MASS)
library(bayesrules)

### a
df.selected <- Batting %>%
  select(playerID,H,AB)

### b
# desde un enfoque frecuentista eligiria a los que tienen promedio 1
# porque su estimacion de p("probabilidad con la que tiene un hit") es 1.

# eligiria a:
freq_better_player <- df.selected %>%
  filter(H == AB & H != 0)

### c
df.selected.1000Intentos <- df.selected %>%
  group_by(playerID) %>% 
  summarise(H = sum(H), AB = sum(AB)) %>%
  filter(AB > 1000)

# saco el promedio de exitos y de fallas para pasarle como parametros a la beta
alfa <-round( mean(df.selected.1000Intentos$H))
beta <- round(mean(df.selected.1000Intentos$AB - df.selected.1000Intentos$H))
prop <- df.selected.1000Intentos$H/df.selected.1000Intentos$AB
lambda <- fitdistr(prop, densfun = "beta", start = list(shape1= alfa, shape2=beta))

plot_beta(lambda$estimate[[1]],lambda$estimate[[2]])

### d
df.posterior <- df.selected.1000Intentos %>%
  mutate(alfa_i = lambda$estimate[[1]]+df.selected.1000Intentos$H,
         beta_i = lambda$estimate[[1]]-df.selected.1000Intentos$H + df.selected.1000Intentos$AB) %>%
  mutate(esperanza_posterior = alfa_i/(alfa_i+beta_i))

### e
ranking <- df.posterior %>%
  arrange(desc(esperanza_posterior))

x <- 1:length(df.posterior$playerID)
plot(x, df.posterior$esperanza_posterior, type = "l", col = "red")
lines(x, prop, col = "green")

plot(x, sort(df.posterior$esperanza_posterior), type = "l", col = "red")
lines(x, sort(prop), col = "green")
