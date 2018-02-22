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
> 1275-1286. 10.1037/0278-7393.23.5.1275

The authors argue that mixing case inappropriately has a negative impact
on the ability for readers to decode text. I wanted to test this effect
on myself. I needed a sentence and decided to make a minimal R function
to randomly alter case:

    #' Randomly Change the Case of Letters Within Words
    #' 
    #' Following Mayall, Humphreys, & Olson (1997), this function randomly 
    #' converts a proportion of lower case letters to upper case.
    #' 
    #' @param x A vector of text strings to upper case.
    #' @param prop A proportion of graphemes to change the case of.
    #' @param wrap An integer value of how wide to wrap the strings.  Using the default 
    #' \code{NULL} disables this feature.
    #' @param \ldots ignored.
    #' @return Prints wrapped lines with internal graphemes randomly converted to 
    #' upper case.
    #' @references 
    #' Mayall, K., Humphreys, G. W., & Olson, A. (1997). Disruption to word or letter 
    #' processing?: The origins of case-mixing effects. Journal of Experimental Psychology: 
    #' Learning, Memory, and Cognition, 23(5), 1275-1286. 10.1037/0278-7393.23.5.1275
    #' @export
    random_upper <- function(x, prop = .5, wrap = NULL, ...){

        stopifnot(prop > 0 & prop <= 1)

        ## splits each string to a vector of characters
        char_vects <- strsplit(x, '')

        ## loop through each vector of characters and replace lower with upper case
        out <- unlist(lapply(char_vects, function(chars){

            ## detect the lower case locations
            locs <- grepl('[a-z]', chars)   
            ilocs <- which(locs) 

            ## sample lower case locations to convert to upper 
            to_upper <- sample(ilocs, ceiling(prop * length(ilocs)))  
            chars[to_upper] <- toupper(chars[to_upper])

            ## collapse the vector of characters back to its original string
            paste(chars, collapse = '')

        }))

        ## optional string wrapping
        if (!is.null(wrap) && !is.na(as.integer(wrap))) {
            invisible(Map(function(x, wrapchar) {
                cat(strwrap(x, width = as.integer(wrap)), sep = '\n')
                if (wrapchar) cat('\n')
            }, out, c(rep(TRUE, length(out) - 1), FALSE)))
        } else {
            out
        }

    }


    x <- "Many English words are formed by taking basic words and adding combinations of prefixes and suffixes to them."

    ## 10% random upper
    random_upper(x, .1, 60)

    ## ManY EnglisH words are Formed by taking basic words and
    ## addIng coMbinations oF Prefixes and sUfFixes to them.

    ## 30% random upper
    random_upper(x, .3, 60)

    ## Many EnglisH Words Are FoRMeD by tAKiNg bAsic WOrDS aND
    ## addiNg CombinaTioNs of prefiXes and SufFiXes to TheM.

    ## Worst case (no pun intended)...50% random upper
    random_upper(x, .5, 60)

    ## MaNy ENGLIsH woRDS ARe foRmED bY TaKIng baSiC WORdS aNd
    ## adDiNg CoMbInatiOnS of pReFiXeS ANd SuFFixeS TO theM.

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
(2006)](articles/Rayner2006.pdf) document this effect.

> Rayner, K., White, S. J., Johnson, R. L., & Liversedge, S. P. (2006).
> Raeding wrods with jubmled lettres: There is a cost. Psychological
> Science, 17(3), 192-193. 10.1111/j.1467-9280.2006.01684.x

The code below produces random resamples of internal letters. The
`gram.length` argument controls the ngram window function (what letters
are kept together), while the `sample.grams` argument is a logical
command that, if true, reorders the gram sequences that have been
sampled. For example, let's say I had the sequence `123456`. Sampling
grams of length 3 may produce `231564`. Setting `sample.grams = TRUE`
may further produce `564231`.

    #' Transpose Internal Letters Within Words
    #' 
    #' Following a famous Internet meme and Rayner, White, Johnson, & Liversedge 
    #' (2006), this function randomly scrambles the internal (not the first or last
    #' letter of > 3 character words) letters.
    #' 
    #' @details Internet meme:
    #' 
    #' It deosn't mttaer in waht oredr the ltteers in a wrod are, the olny iprmoetnt 
    #' tihng is taht the frist and lsat ltteer be at the rghit pclae. The rset can be 
    #' a toatl mses and you can sitll raed it wouthit porbelm. Tihs is bcuseae the 
    #' huamn mnid deos not raed ervey lteter by istlef, but the wrod as a wlohe.
    #'
    #' @param x A vector of text strings to scramble.
    #' @param gram.length The length of gram groups to scramble.  Setting this lower
    #' will keep expected graphemes close together.  Setting it to a high value (e.g.,
    #' 100) will allow the positions of graphemes to deviate farther from the expected
    #' clustering.
    #' @param sample.grams logical.  If \code{TRUE} then the ngram groups don't retain
    #' their original location.  For example, let's say we had the sequence \code{123456}. 
    #' Sampling grams of length 3 (\code{gram.length} may produce \code{231564}. Setting 
    #' \code{sample.grams = TRUE} may further produce \code{564231}.
    #' @param wrap An integer value of how wide to wrap the strings.  Using the default 
    #' \code{NULL} disables this feature.
    #' @param \ldots ignored.
    #' @return Prints wrapped lines with internal graphemes scrambled.
    #' @references 
    #' Rayner, K., White, S. J., Johnson, R. L., & Liversedge, S. P. (2006). Raeding 
    #' wrods with jubmled lettres: There is a cost. Psychological Science, 17(3), 
    #' 192-193. 10.1111/j.1467-9280.2006.01684.x
    #' @export
    random_scramble <- function(x, gram.length = 2, sample.grams = TRUE, wrap = NULL, ...){

        if (!is.integer(gram.length)) gram.length <- as.integer(gram.length)
        stopifnot(all(gram.length > 1))

        ## splits the strings into a list of tokens (words and punctuation)
        token_vects <- textshape::split_token(x, lower = FALSE)

        ## loop through the vectors of tokens
        out <- unlist(lapply(token_vects, function(tokens){

            ## loop through the tokens within each vector
            out <- unlist(lapply(tokens, function(token){

                ## get a sample from gram.length if not fixed
                if (length(gram.length) > 1) win <- sample(gram.length, 1) else win <- gram.length

                ## ensure length of word is > 3 because you can't transpose words less than 2 internal characters 
                len <- nchar(token)
                if (len < 4) return(token)

                ## split tokens into characters and compute location of internal letters
                chars <- strsplit(token, '')[[1]]
                locs <- 2:(len - 1)

                ## If the length of the internal letters is <= the ngram window randomly 
                ##   sample internal letters, collapse characters, and return
                if (length(locs) <= win) {
                    return(
                        paste(
                            c(
                                chars[1], 
                                paste(sample(chars[locs]), collapse = ''), 
                                chars[length(chars)]
                            ), 
                            collapse = ''
                        )
                    )
                }

                ## Make gram groupings for all grams that match gram.length 
                ##    (exclude < gram.length char groups)
                locs2 <- rep(1:floor(length(locs)/win), each = win)

                ## add in the < gram.length groups and store as list of lengths 
                ##    and group assignments 
                locs3 <- rle(c(locs2, c(rep(max(locs2) + 1, length(locs)%%win))))

                ## Resample the lengths to allow the odd group out (if there is one)
                ##     to be located randomly rather than always at the end
                locs4 <- rep(locs3$values, sample(locs3$lengths))

                ## split the vector of chars into the groups, loop through, sample 
                ##     within each group to scramble and collapse the group characters
                rands <- unlist(lapply(split(locs, locs4), function(grams){
                    paste(sample(chars[grams]), collapse = '')
                }))

                ## optionally scramble the group location as well
                if (sample.grams) rands <- sample(rands)

                ## collapse the group gram strings together
                paste(c(chars[1], rands, chars[length(chars)]), collapse = '')  

            }))

            ## Paste tokens back together with single space and attmpt to strip out 
            ##     inappropriate spaces before punctuation.  This does not guarentee
            ##     original spacing of the strings.
            trimws(gsub("(\\s+)([.!?,;:])", "\\2", paste(out, collapse = ' '), perl = TRUE))

        }))

        ## optional string wrapping
        if (!is.null(wrap) && !is.na(as.integer(wrap))) {
            invisible(Map(function(x, wrapchar) {
                cat(strwrap(x, width = as.integer(wrap)), sep = '\n')
                if (wrapchar) cat('\n')
            }, out, c(rep(TRUE, length(out) - 1), FALSE)))
        } else {
            out
        }

    }

    x <- c(
        "According to a study at an English University, it doesn't matter in what order the letters in a word are, the only important thing is that the first and last letter be at the right place. The rest can be a total mess and you can still read it without problem. This is because the human mind does not read every letter by itself but the word as a whole.",
        "A vehicle exploded at a police checkpoint near the UN headquarters in Baghdad on Monday killing the bomber and an Iraqi police officer",
        "Big council tax increases this year have squeezed the incomes of many pensioners",
        "A doctor has admitted the manslaughter of a teenage cancer patient who died after a hospital drug blunder."
    )

    ## Bigram & retain gram location
    random_scramble(x, gram.length = 2, wrap = 70)

    ## Acinrdcog to a sudty at an Eglsinh Uinevrstiy, it doen'st mtaetr in
    ## waht oredr the lretets in a word are, the olny irotpmant thing is
    ## taht the fisrt and lsat lteter be at the rihgt pcale. The rest can be
    ## a tatol mess and you can slitl raed it withuot porlbem. Tihs is
    ## becasue the hamun mind deos not read erevy letter by istelf but the
    ## wrod as a wlohe.
    ## 
    ## A vehlcie eoldepxd at a picole cnikcpoeht near the UN hqdauaertres in
    ## Bhgadad on Mdanoy klnilig the bbemor and an Irqai plocie offecir
    ## 
    ## Big ccniuol tax irsenceas this year have suqezeed the incomes of many
    ## poniseners
    ## 
    ## A dotcor has admtited the mghanausletr of a tneeage cecnar pientat
    ## who deid atefr a hsotapil drug bedulnr.

    ## Bigram and reorder the mixed grams
    random_scramble(x, gram.length = 2, sample.grams = FALSE, wrap = 70)

    ## Accroidng to a study at an English University, it dose'nt mtater in
    ## waht order the lettres in a word are, the olny ipmortnat thing is
    ## that the fisrt and lsat letetr be at the rgiht plcae. The rest can be
    ## a total mses and you can still read it wtihout porbelm. Tihs is
    ## becasue the human mind does not read every lteter by itslef but the
    ## wrod as a whole.
    ## 
    ## A vheicle exploded at a police chekcponit near the UN haeqdaurters in
    ## Baghdad on Mnoday kililng the bmoebr and an Iraqi ploice officer
    ## 
    ## Big cuonicl tax icnreaess this yaer hvae squeezed the icnomes of mnay
    ## penisoners
    ## 
    ## A dcotor has admitetd the manlsauhgter of a tenegae canecr ptaeint
    ## who died afetr a hospiatl drug blunder.

    ## Gram length randomly between 2-5 retain gram location
    random_scramble(x, gram.length = 2:5, wrap = 70)

    ## Acocrindg to a sdtuy at an Engislh Uitsveirny, it d'onest meattr in
    ## what oderr the lreetts in a word are, the only iportmnat tnhig is
    ## that the fisrt and last ltteer be at the right place. The rest can be
    ## a total mess and you can sitll read it wituoht prolbem. Tihs is
    ## baecsue the human mind deos not raed evrey ltteer by itlsef but the
    ## wrod as a wolhe.
    ## 
    ## A vehicle eoxldped at a pciole chkceonipt naer the UN htreauredqas in
    ## Bdaaghd on Mnoady kililng the bmboer and an Iarqi picole ofeficr
    ## 
    ## Big ccinuol tax isaencers this yaer have sequezed the ieomcns of mnay
    ## poinesnres
    ## 
    ## A dootcr has aidmtted the mlsanautgher of a tenaege cacner peintat
    ## who deid after a htpaisol durg blnuder.

    ## 5-gram retin gram location
    random_scramble(x, gram.length = 5, wrap = 70)

    ## Ardcocnig to a sduty at an Egilnsh Urseitivny, it d'ensot mettar in
    ## waht order the ltteers in a wrod are, the olny itraonmpt tihng is
    ## taht the fsrit and last letter be at the rghit pacle. The rest can be
    ## a tatol mess and you can sltil read it wiotuht perlbom. Tihs is
    ## bcaeuse the haumn mind does not read eervy letter by ilestf but the
    ## wrod as a wolhe.
    ## 
    ## A vhilece exdoelpd at a pciole cehcipnkot near the UN htarerudaeqs in
    ## Bdagahd on Madnoy killnig the bbeomr and an Iaqri picole ofiecfr
    ## 
    ## Big cuconil tax incseears this yaer have szeueeqd the imcneos of many
    ## penreoisns
    ## 
    ## A dotcor has aetmitdd the mnslaahteugr of a tenagee canecr pteinat
    ## who died after a hpstiaol drug bueldnr.
