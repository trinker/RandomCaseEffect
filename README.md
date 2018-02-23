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

    ## ManY English wOrds are forMed by taking basic Words anD
    ## adding comBinatIOns of prefIxes and suffixes to them.

    ## 30% random upper
    random_upper(x, .3, 60)

    ## MANy EnglisH WORdS ArE Formed by tAking BAsic wOrds And
    ## addiNg combiNaTions oF preFixes aNd suFFixES tO Them.

    ## Worst case (no pun intended)...50% random upper
    random_upper(x, .5, 60)

    ## Many ENGlISh wOrDS are fORMed By tAKIng BaSIC WoRdS aND
    ## ADdInG combinATioNs OF PREFIxES and SufFixEs TO Them.

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
    #' @param remix.first logical.  If \code{TRUE} the first letter is allowed to be 
    #' remixed as well.
    #' @param remix.last logical.  If \code{TRUE} the last letter is allowed to be 
    #' remixed as well.
    #' @param \ldots ignored.
    #' @return Prints wrapped lines with internal graphemes scrambled.
    #' @references 
    #' Rayner, K., White, S. J., Johnson, R. L., & Liversedge, S. P. (2006). Raeding 
    #' wrods with jubmled lettres: There is a cost. Psychological Science, 17(3), 
    #' 192-193. 10.1111/j.1467-9280.2006.01684.x
    #' @export
    random_scramble <- function(x, gram.length = 2, sample.grams = TRUE, wrap = NULL, 
        remix.first = FALSE, remix.last = FALSE, ...){

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
                ##   Note: the value of 3 depends if the first and last letters are allowed to vary
                len <- nchar(token)
                if (len < 4 - (remix.first + remix.last)) return(token)

                ## split tokens into characters and compute location of internal letters
                chars <- strsplit(token, '')[[1]]
                locs <- (2 - remix.first):(len - (!remix.last))

                ## If the length of the internal letters is <= the ngram window randomly 
                ##   sample internal letters, collapse characters, and return
                if (length(locs) <= win) {
                    return(
                        paste(
                            c(
                                if (remix.first) '' else chars[1], 
                                paste(sample(chars[locs]), collapse = ''), 
                                if (remix.last) '' else chars[length(chars)]
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
                paste(
                    c(
                        if (remix.first) '' else chars[1], 
                        rands, 
                        if (remix.last) '' else chars[length(chars)]
                    ), 
                    collapse = ''
                )  

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

    ## Adroinccg to a sudty at an Ensilgh Utinivesry, it dseon't mtater in
    ## waht order the lrettes in a word are, the olny iornatmpt thing is
    ## that the fisrt and lsat letter be at the rhigt pcale. The rset can be
    ## a toatl mses and you can sltil raed it wituoht pborelm. Tihs is
    ## bacsuee the human mnid deos not raed every lteter by itself but the
    ## wrod as a wlohe.
    ## 
    ## A vlceihe epxedold at a pciole cniopkceht near the UN hqdrteaeruas in
    ## Bhagadd on Monady kliinlg the bebmor and an Iaqri ploice offiecr
    ## 
    ## Big couicnl tax incaerses this yaer hvae seeezqud the imocnes of mnay
    ## preisnenos
    ## 
    ## A dtoocr has aetdmitd the mnaghtesluar of a teegnae cecnar pnatiet
    ## who deid atefr a hsoiptal drug blunedr.

    ## Bigram and reorder the mixed grams
    random_scramble(x, gram.length = 2, sample.grams = FALSE, wrap = 70)

    ## Accodrnig to a study at an Egnlsih Unievrsity, it dose'nt matter in
    ## what oredr the lettres in a wrod are, the olny impotrant tihng is
    ## that the first and last letter be at the rihgt place. The rest can be
    ## a total mses and you can still raed it without porblem. This is
    ## because the human mind does not raed every letetr by istelf but the
    ## word as a whole.
    ## 
    ## A vheilce expoledd at a polcie chekcopint naer the UN heaqdautrers in
    ## Bgadhad on Monady kililng the bomebr and an Irqai police ofifecr
    ## 
    ## Big conuicl tax incresaes tihs year have squeezed the icnmoes of many
    ## penisnores
    ## 
    ## A dcootr has amditetd the manlsaughter of a teenage canecr patient
    ## who deid after a hosipatl drug blunedr.

    ## Gram length randomly between 2-5 retain gram location
    random_scramble(x, gram.length = 2:5, wrap = 70)

    ## Accoridng to a study at an Eislgnh Uivnsretiy, it d'nsoet mtater in
    ## what oerdr the lretets in a word are, the only inartpmot tnhig is
    ## taht the fisrt and lsat lteter be at the rihgt place. The rset can be
    ## a ttoal mses and you can sitll raed it wiuhtot peblorm. Tihs is
    ## bueasce the hmuan mind deos not raed ervey letter by ilstef but the
    ## word as a wohle.
    ## 
    ## A vehicle eoledpxd at a picole cehckionpt near the UN hruadeaetrqs in
    ## Bahadgd on Mnoday kililng the bobmer and an Iqari pciloe ociffer
    ## 
    ## Big cuocnil tax irseaecns tihs yaer hvae suqzeeed the iocnems of mnay
    ## psneinroes
    ## 
    ## A dcotor has aeittmdd the metanuahgslr of a teenage cnacer petiant
    ## who deid aeftr a hatispol durg bdlneur.

    ## 5-gram retin gram location
    random_scramble(x, gram.length = 5, wrap = 70)

    ## Adinroccg to a sutdy at an Egslinh Urisetnviy, it d'seont mtaetr in
    ## waht oedrr the ltetres in a wrod are, the only ipmtonart thnig is
    ## that the fsirt and last ltteer be at the rihgt palce. The rset can be
    ## a tatol mess and you can stlil read it whutoit pebrolm. This is
    ## bsuecae the hmaun mnid does not raed erevy letetr by iltsef but the
    ## word as a wolhe.
    ## 
    ## A vlihcee eepoldxd at a piolce cnipkochet naer the UN hertraeqauds in
    ## Bgdhaad on Modnay kliling the boembr and an Iraqi pciole oiceffr
    ## 
    ## Big ccnouil tax iraencses this year have seezueqd the icnmeos of many
    ## prenisenos
    ## 
    ## A dotocr has adttiemd the manlashugter of a tneagee canecr pntiaet
    ## who died aetfr a hastpiol drug beulndr.
