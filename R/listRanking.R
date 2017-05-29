library(readtext)
library(quanteda)
library(stringr)
library(dplyr)

wdir <- getwd()

# Create corpus objects ---------------------------------------------------

## CDU
cduCorpus <- quanteda::corpus(
    readtext::readtext(
      paste0(wdir, 
             "/data/cdu/",
             "*",
             "/*.txt"), 
      docvarsfrom = "filenames")
  )
docvars(cduCorpus, "congress") <- gsub(" parteitag ", "_",
                                       gsub("/.*$","", cduCorpus$documents$doc_id),
                                       ignore.case = T)

# Replace text between parentheses
cduCorpus$documents$texts <-
  stringi::stri_replace_all_regex(cduCorpus$documents$texts, " \\(.*\\)", "")


  
## SPD
spdCorpus <- quanteda::corpus(
  readtext::readtext(
    paste0(wdir, 
           "/data/spd/",
           "*",
           "/*.txt"), 
    docvarsfrom = "filenames")
)

docvars(spdCorpus, "congress") <- gsub(" parteitag ", "_",
                                       gsub("/.*$","", spdCorpus$documents$doc_id),
                                       ignore.case = T)

# Replace text between parentheses
spdCorpus$documents$texts <-
  stringi::stri_replace_all_regex(spdCorpus$documents$texts, " \\(.*\\)", "")

# Create dfm and run wordfish ---------------------------------------------

# List of stopwords
wordlist <- readLines("data/stopWords.txt") %>% 
  stringi::stri_replace_all_fixed(.,",","") %>%
  stringi::stri_replace_all_fixed(.," ","") 

## CDU
congress <- unique(cduCorpus$documents$congress)

cduDfms <- list()
for(i in 1:length(congress)) {
  date = congress[i]
  cduDfms[congress[i]] <- dfm(corpus_subset(cduCorpus, 
                                            congress==date),
                tolower = TRUE, remove_punct = TRUE,remove_numbers = TRUE,
                remove_symbols = TRUE, 
                remove = wordlist) %>% 
      dfm_trim(., min_docfreq=2, max_docfreq = 0.90) %>% 
      dfm_remove(., stopwords("german"))
}


# Wordfish per congress
datalist <- list()
for(j in 1:length(cduDfms)) {

    wordfish <- quanteda::textmodel_wordfish(cduDfms[[j]])
    dat <- data.frame(congress = wordfish@x@docvars$congress,
                      speaker = wordfish@x@docvars$docvar1,
                      position = wordfish@theta)
    datalist[[j]] <- dat
}

# Final data frame
cduPositions = dplyr::bind_rows(datalist)

# Fix dimension orientation
cduPositions <- cduPositions %>%
  dplyr::mutate(position = ifelse(
    congress %in% c("1990_hamburg", "1991_dresden", "1993_berlin", "1994_bonn",
                    "1994_hamburg", "1995_karlsruhe", "1996_hannover",
                    "2000_essen", "2001_dresden", "2002_hannover","2003_leipzig",
                    "2004_duesseldorf","2005_dortmund", "2006_dresden", 
                    "2012_hannover"),
    position*-1,position)) %>% 
  dplyr::mutate(party = "CDU")



## SPD
congress <- unique(spdCorpus$documents$congress)

spdDfms <- list()
for(i in 1:length(congress)) {
  date = congress[i]
  spdDfms[congress[i]] <- dfm(corpus_subset(spdCorpus, 
                                            congress==date),
                              tolower = TRUE, remove_punct = TRUE,remove_numbers = TRUE,
                              remove_symbols = TRUE, 
                              remove = wordlist) %>% 
    dfm_trim(., min_docfreq=2, max_docfreq = 0.90) %>% 
    dfm_remove(., stopwords("german"))
}


# Wordfish per congress
datalist <- list()
for(j in 1:length(spdDfms)) {
  
  wordfish <- quanteda::textmodel_wordfish(spdDfms[[j]])
  dat <- data.frame(congress = wordfish@x@docvars$congress,
                    speaker = wordfish@x@docvars$docvar1,
                    position = wordfish@theta)
  datalist[[j]] <- dat
}

# Final data frame
spdPositions = dplyr::bind_rows(datalist)

# Fix dimension orientation
spdPositions <- spdPositions %>%
  dplyr::mutate(position = ifelse(
    congress %in% c("2002_Berlin", "2003_Berlin", "2003_Bochum", "2005_Karlsruhe",
                  "2007_Hamburg", "2008_Berlin", "2010_Berlin", "2013_Augsburg"),
  position*-1,position)) %>% 
  dplyr::mutate(party = "SPD")

# Combine both parties
positions <- dplyr::full_join(cduPositions, spdPositions) 

# Add dates
dates <- read.csv("data/congressDates.csv", stringsAsFactors = F) %>% 
  dplyr::filter(!duplicated(.))

dfull <- dplyr::full_join(positions,dates)

# Save results ------------------------------------------------------------
save(dfull, file = "data/positions.RData")
write.csv(dfull, file = "data/positions.csv")

