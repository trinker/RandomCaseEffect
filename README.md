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

    ## Many EnGlish words are foRMed by tAking basic wORdS anD
    ## aDding combinations of prefixes and suffixes to them.

    ## 30% random upper
    random_upper(x, .3, 60)

    ## ManY ENglIsh WordS Are FOrmED bY tAKing basiC woRds and
    ## adding combInaTions of PRefIxEs and SUffIxeS TO them.

    ## Worst case (no pun intended)...50% random upper
    random_upper(x, .5, 60)

    ## MANY EngLISh wORDS aRe fORMEd bY TAkinG baSiC worDS aNd
    ## ADding cOmbInATioNs Of PrEfIXeS AND SuFFixEs to theM.

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
`window` argument controls the window function (what letters are kept
together), while the `sample.grams` argument is a logical command that,
if true, reorders the gram sequences that have been sampled. For
example, let's say I had the sequence `123456`. Sampling grams of length
3 may produce `231564`. Setting `sample.grams = TRUE` may further
produce `564231`.

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

    ## Accronidg to a sduty at an Elgnsih Uevitinrsy, it dsnoe't metatr in
    ## waht odrer the lettres in a word are, the only ipmornatt tnihg is
    ## that the fsrit and last letter be at the rhgit plcae. The rset can be
    ## a ttaol mses and you can sitll raed it wtihuot poreblm. This is
    ## busecae the human mind deos not read eevry letter by itself but the
    ## wrod as a whloe.
    ## 
    ## A vilcehe expoledd at a pciole copckehint naer the UN hrtaereqduas in
    ## Badgahd on Monday kililng the bebmor and an Iqrai plocie oecffir
    ## 
    ## Big cicnuol tax ieascnres this yaer have szeeequd the iconmes of many
    ## ponsiernes
    ## 
    ## A dcootr has aitdmted the manslghetuar of a tneeage canecr pateint
    ## who died after a htapiosl drug bdelunr.

    ## Bigram and reorder the mixed grams
    random_scramble(x, gram.length = 2, sample.grams = FALSE, wrap = 70)

    ## Accroding to a study at an English Unievsrity, it deosn't mtater in
    ## what order the ltetres in a wrod are, the only ipmroatnt thnig is
    ## that the first and last lteter be at the right plcae. The rset can be
    ## a total mses and you can stlil raed it wtiohut porblem. Tihs is
    ## becuase the human mind does not raed every letter by istelf but the
    ## word as a wohle.
    ## 
    ## A vehicle exploedd at a police checkopnit naer the UN heaqdautrers in
    ## Bgahdad on Monday killnig the bomber and an Irqai polcie officer
    ## 
    ## Big counicl tax increases this year have squeeezd the incoems of mnay
    ## pneisonres
    ## 
    ## A doctor has amditted the mnalsaughter of a teenage canecr ptaeint
    ## who deid after a hosipatl drug bulnder.

    ## Gram length randomly between 2-5 retain gram location
    random_scramble(x, gram.length = 2:5, wrap = 70)

    ## Aodrccing to a study at an Esiglnh Univsertiy, it deso'nt metatr in
    ## what oderr the lteters in a word are, the olny imoprtant tnihg is
    ## that the frist and last lteter be at the rghit pcale. The rset can be
    ## a ttaol mess and you can still raed it wtuoiht probelm. Tihs is
    ## bcusaee the haumn mind does not read erevy ltteer by itself but the
    ## word as a wohle.
    ## 
    ## A vclehie eloxpded at a police cnopikhect near the UN hutarqadeers in
    ## Bhdagad on Madnoy kilinlg the bmober and an Iaqri ploice ofiecfr
    ## 
    ## Big cuoncil tax iacrneess tihs year have sueqzeed the inemcos of many
    ## psienrones
    ## 
    ## A dootcr has attedimd the msaanluehgtr of a tgaeene caencr peiatnt
    ## who deid atefr a hatipsol drug bldenur.

    ## 5-gram retin gram location
    random_scramble(x, gram.length = 5, wrap = 70)

    ## Arccoding to a study at an Eslignh Uvniseitry, it dns'oet matetr in
    ## what oedrr the letters in a wrod are, the olny imparntot tnihg is
    ## that the frist and lsat ltteer be at the rgiht palce. The rest can be
    ## a total mses and you can stlil read it wtouiht prlbeom. This is
    ## bscauee the hmuan mind deos not read evrey leettr by itlesf but the
    ## wrod as a wohle.
    ## 
    ## A vieclhe eolxdped at a police cnoihkecpt naer the UN hartreuqeads in
    ## Badhagd on Mdoany kniillg the bemobr and an Iaqri plioce offiecr
    ## 
    ## Big cinocul tax icnrseaes this year have seeezuqd the icnemos of many
    ## psoienners
    ## 
    ## A dtcoor has adtmeitd the mnslaahgeutr of a teneage cancer paeintt
    ## who deid atefr a hoipsatl durg bdenulr.
