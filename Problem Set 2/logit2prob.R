logit2prob <- function(logit){
        odds <- exp(logit)
        prob <- odds / (1 + odds)
        return(prob)
}


logit2prob(0.7891976)

0.14218974 + 1.13093141 + -0.48392359 