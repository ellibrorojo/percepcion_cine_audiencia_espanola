lowerAndTrim <- function (string)
{
  return(gsub(" ", "", tolower(as.character(string))))
}