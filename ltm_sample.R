# ltm_sample.R
#
# trying LTM package. based on http://bit.ly/1LzYhsx
# missing data DF, using bfi data instead
#
library(psych)
data(bfi)
data<-bfi[1:25]

describe(data)                        # means and SDs for data file with 12 ratings
scree(data, factors=FALSE)            # scree plot
omega(data)                           # runs bifactor model

library(ltm)
describe(data)                        # runs frequency tables for every item
fit<-grm(data)                        # graded response model
fit                                   # print cutpoints and discrimination

plot(fit)                             # plots item category characteristic curves
plot(fit, type="IIC")                 # plots item information curves

# next two lines calculate latent trait scores and assign them to variable named trait
pattern<-factor.scores(fit, resp.pattern=data)
trait<-pattern$score.dat$z1