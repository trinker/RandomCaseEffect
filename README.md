This repo is a demonstration of the random mixed case effect on the
ability to read. [Mayall, Humphreys & Olson (1997)](Mayall1997.pdf)
document this effect.

> Mayall, K., Humphreys, G. W., & Olson, A. (1997). Disruption to word
> or letter processing?: The origins of case-mixing effects. Journal of
> Experimental Psychology: Learning, Memory, and Cognition, 23(5),
> 1275-1286. 10.1037/0278-7393.23.5.1275 (on UBlearns)

The authors argue that mixing case inappropriately has a negative impact
on the ability for readers to decode text. I wanted to test this effect
on myself. I needed a sentence and decided to make a minimal R function
to randomly alter case:

    random_upper <- function(x, prop = .5, ...){

        stopifnot(prop > 0 & prop <= 1)

        y <- strsplit(x, '')

        unlist(lapply(y, function(a){
            locs <- grepl('[a-z]', a)   
            ilocs <- which(locs)  
            to_upper <- sample(ilocs, ceiling(prop * length(ilocs)))  
            a[to_upper] <- toupper(a[to_upper])
            paste(a, collapse = '')
        }))
    }

    x <- "Many English words are formed by taking basic words and adding combinations of prefixes and suffixes to them."

    random_upper(x, .1)

    ## [1] "Many English words are formed by taking BaSic words aNd Adding combInations Of prefixeS and suFfixes to theM."

    random_upper(x, .3)

    ## [1] "MaNy EnGLish WordS are FormEd by taKiNG Basic wOrds and aDdinG cOmbInatIOns oF PrefiXes ANd suFfiXes To thEm."

    random_upper(x, .5)

    ## [1] "ManY ENgLISh WoRds are FORMED bY takIng BaSIC WorDS AND Adding CoMBiNATIONs OF PRefixes and SUffIXeS tO them."

Indeed, I informally recognize that mixing case made decoding harder
(likely removing automaticity in sight word recognition).
