-   [Random Case Effect](#random-case-effect)
-   [Random Scramble](#random-scramble)

This repo is a demonstration of the random mixed case effect and other
reading phenomenon on the ability to read.

Random Case Effect
==================

[Mayall, Humphreys & Olson (1997)](articles/Mayall1997.pdf) document
this effect.

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

    ## [1] "Many English words aRe fOrmed by takinG basic words aNd adding combinAtiOns of prEFixes and suffIxes to them."

    random_upper(x, .3)

    ## [1] "Many ENGliSH worDs aRE formed by tAKIng bAsiC worDs anD addIng combinaTions of pREfIxes aNd SUFfIxEs To thEm."

    random_upper(x, .5)

    ## [1] "MAnY EngliSH WOrDs ARe FOrmED by TAKING BAsic wOrdS ANd adDiNG cOMBinaTions of PrEfiXes ANd SUFFixeS TO tHem."

Indeed, I informally recognize that mixing case made decoding harder
(likely removing automaticity in sight word recognition).

Random Scramble
===============

The effect of scrambling internal letters (not first or last letter)
began as a semi-true Internet meme hoax. The hoax read as follows:

> It deosn't mttaer in waht oredr the ltteers in a wrod are, the olny
> iprmoetnt tihng is taht the frist and lsat ltteer be at the rghit
> pclae. The rset can be a toatl mses and you can sitll raed it wouthit
> porbelm. Tihs is bcuseae the huamn mnid deos not raed ervey lteter by
> istlef, but the wrod as a wlohe.

Obviously, there is an element of truth to this but indeed there is a
cost to scrambling letters. [Rayner, White, Johnson, & Liversedge
(2006)](articles/Mayall1997.pdf) document this effect.

> Rayner, K., White, S. J., Johnson, R. L., & Liversedge, S. P. (2006).
> Raeding wrods with jubmled lettres: There is a cost. Psychological
> Science, 17(3), 192-193. 10.1111/j.1467-9280.2006.01684.x

The code below produces random resamples of internal letters. The
`window` argument controls the window function (what letters are kept
together), while the `sample.grams` argument is a logical command that,
if true, reorders the gram sequences that have been sampled. For
example, let's say I had the sequence `123456`. Sampling grams of length
3 may produce `231564`. Setting `sample.grams = TRUE` may further
produce `564231`.

    random_scramble <- function(x, window = 2, sample.grams = TRUE, ...){

        if (!is.integer(window)) window <- as.integer(window)
        stopifnot(all(window > 1))

        y <- textshape::split_token(x, lower = FALSE)

        unlist(lapply(y, function(a){
            out <- unlist(lapply(a, function(b){

                ## get a sample from window if not fixed
                if (length(window) > 1) w <-  sample(window, 1) else w <- window

                ## ensure length of word is > 2
                len <- nchar(b)
                if (len < 4) return(b)

                z <- strsplit(b, '')[[1]]
                locs <- 2:(len - 1)

                if (length(locs) <= w) {
                    return(paste(c(z[1], paste(sample(z[locs]), collapse = ''), z[length(z)]), collapse = ''))
                }

                locs2 <- rep(1:floor(length(locs)/w), each = w)

                locs3 <- rle(c(locs2, c(rep(max(locs2) + 1, length(locs)%%w))))

                locs4 <- rep(locs3$values, sample(locs3$lengths))

                rands <- unlist(lapply(split(locs, locs4), function(grams){
                    paste(sample(z[grams]), collapse = '')
                }))

                if (sample.grams) rands <- sample(rands)

                paste(c(z[1], rands, z[length(z)]), collapse = '')  

            }))

            trimws(gsub("(\\s+)([.!?,;:])", "\\2", paste(out, collapse = ' '), perl = TRUE))

        }))

    }



    x <- c(
        "According to a study at an English University, it doesn't matter in what order the letters in a word are, the only important thing is that the first and last letter be at the right place. The rest can be a total mess and you can still read it without problem. This is because the human mind does not read every letter by itself but the word as a whole.",
        "A vehicle exploded at a police checkpoint near the UN headquarters in Baghdad on Monday killing the bomber and an Iraqi police officer",
        "Big council tax increases this year have squeezed the incomes of many pensioners",
        "A doctor has admitted the manslaughter of a teenage cancer patient who died after a hospital drug blunder."
    )

    random_scramble(x, 2)

    ## [1] "Adinorccg to a study at an Eisnglh Uitrsnievy, it d'nesot mtetar in what oredr the lteetrs in a word are, the olny imanoprtt thnig is taht the frsit and last lteter be at the rghit pcale. The rset can be a tatol mses and you can slitl read it wituhot pleborm. Tihs is baucese the huamn mind deos not read erevy lteter by ilestf but the wrod as a wlohe."
    ## [2] "A viclhee edepxold at a plocie cheopkcnit naer the UN haeqdrtreaus in Bhgdaad on Mdanoy kniillg the bbemor and an Iraqi picole oifecfr"                                                                                                                                                                                                                          
    ## [3] "Big ccinuol tax inceerass tihs yaer hvae sezeeuqd the incomes of many psionernes"                                                                                                                                                                                                                                                                                
    ## [4] "A dtoocr has aettidmd the mghuaetanslr of a teenage cancer patient who died aetfr a htasopil durg bdnuler."

    random_scramble(x, 2, sample.grams = FALSE)

    ## [1] "Accroding to a study at an Egnlish Unievsrtiy, it deosn't mtater in waht odrer the letters in a wrod are, the olny imoptrant thing is taht the first and last lteetr be at the right palce. The rset can be a total mess and you can sitll raed it wtihout porlbem. This is bceuase the human mnid does not read every lteetr by itslef but the wrod as a whole."
    ## [2] "A vehcile epxoledd at a ploice cehkcpoint near the UN haeqduarters in Bagdhad on Mnoady killing the bomber and an Iarqi ploice officer"                                                                                                                                                                                                                          
    ## [3] "Big cuoncil tax inrcaeses this year have squeezed the incoems of many pnesinoers"                                                                                                                                                                                                                                                                                
    ## [4] "A doctor has amdtited the mnalsuahgter of a teneage cnacer paitnet who deid after a hosiptal drug blnuedr."

    random_scramble(x, 2:5)

    ## [1] "Anidrccog to a stduy at an Eglsinh Urneivsity, it desn'ot metatr in what odrer the lettres in a word are, the olny iortmpnat thnig is that the fsrit and last ltteer be at the right pclae. The rest can be a total mses and you can slitl read it wuohtit prolbem. This is beacuse the human mind deos not read eevry ltteer by isletf but the wrod as a wlohe."
    ## [2] "A velhice epxdloed at a pcoile cinkcopeht naer the UN hertqadearus in Bdahagd on Madony kllniig the bbmeor and an Iraqi picloe offceir"                                                                                                                                                                                                                          
    ## [3] "Big ciuoncl tax inrcseaes tihs yaer have suqezeed the icmnoes of many pneisnores"                                                                                                                                                                                                                                                                                
    ## [4] "A doctor has admtteid the mtgheulasanr of a teeange ccenar ptaient who deid aetfr a htiapsol durg bdeulnr."

    random_scramble(x, 5)

    ## [1] "Ainrdcocg to a stduy at an Enligsh Uivnesirty, it den'sot metatr in what order the leettrs in a word are, the olny irtompnat tihng is that the fisrt and last lteter be at the rhigt pcale. The rest can be a ttoal mses and you can sitll raed it whtuoit poerlbm. This is basuece the human mnid does not read evrey ltteer by iltesf but the wrod as a whloe."
    ## [2] "A vliehce edpoelxd at a piloce cnkiopehct naer the UN hterarqdeuas in Badaghd on Maodny knliilg the bebmor and an Iqari piloce ofifecr"                                                                                                                                                                                                                          
    ## [3] "Big cuncoil tax inceraess this year have sqzeeued the imcnoes of many pensionres"                                                                                                                                                                                                                                                                                
    ## [4] "A dtoocr has ametitdd the mguehtanslar of a tneaege cnacer pieantt who deid afetr a hitpsoal durg bduenlr."
