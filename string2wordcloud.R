library(wordcloud)
require(RColorBrewer)


string2wordcloud <- function(text){
  # special character recognizer
  is.sp <- function(x) any(x == c(".", ",", ":", ";", "&", "\n", "/"))
  
  # relevant words recognizer (test set for english)
  is.rw <- function(x) any(tolower(x) == c("about", "to", "from", "of", "as", "at", "by", "for", "in", "into", "on", "onto", "per", "up", "upon", "and", "or", "is", "be", "are","will","shall","a","an","the", "where","it","am","then","that","there","them","what"))
  
  # special characters and relevant words filters
  sp.filter <- function(x) paste(unlist(strsplit(x, ""))[!sapply(unlist(strsplit(x, "")), is.sp)],collapse = "")
  rw.filter <- function(x) x[!sapply(x, is.rw)]
  
  # split the string into words
  text.bywords <- sapply(unlist(strsplit(text, split = c(" ","\n","\r"))), sp.filter)
  text.bywords <- rw.filter(text.bywords)
  
  # count frequencies of every word and put them into data.frame
  word.freq.df <- as.data.frame(table(text.bywords))
  
  # create the wordcloud
  wordcloud(word.freq.df$text.bywords, word.freq.df$Freq, scale=c(10,.6),min.freq=2, max.words=Inf, random.order=FALSE, col=brewer.pal(8,"Dark2"))
}



# Example (1 Corinthians 13)

cor13 <- "13 If I speak in the tongues of men or of angels, but do not have love, I am only a resounding gong or a clanging cymbal. 2 If I have the gift of prophecy and can fathom all mysteries and all knowledge, and if I have a faith that can move mountains, but do not have love, I am nothing. 3 If I give all I possess to the poor and give over my body to hardship that I may boast,[b] but do not have love, I gain nothing.
4 Love is patient, love is kind. It does not envy, it does not boast, it is not proud. 5 It does not dishonor others, it is not self-seeking, it is not easily angered, it keeps no record of wrongs. 6 Love does not delight in evil but rejoices with the truth. 7 It always protects, always trusts, always hopes, always perseveres.
8 Love never fails. But where there are prophecies, they will cease; where there are tongues, they will be stilled; where there is knowledge, it will pass away. 9 For we know in part and we prophesy in part, 10 but when completeness comes, what is in part disappears. 11 When I was a child, I talked like a child, I thought like a child, I reasoned like a child. When I became a man, I put the ways of childhood behind me. 12 For now we see only a reflection as in a mirror; then we shall see face to face. Now I know in part; then I shall know fully, even as I am fully known.
13 And now these three remain: faith, hope and love. But the greatest of these is love."

string2wordcloud(cor13)
