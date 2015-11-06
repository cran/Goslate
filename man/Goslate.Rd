\name{Goslate}
\alias{Goslate}
\title{Translate Text}
\description{
R interface to the Python package Goslate (Version 1.5.0).
Goslate provides an API to Google's free online language translation 
service by querying the Google translation website.
}
\usage{
Goslate(name="Goslate", writing=c("native", "roman", "both"), 
        proxy=NULL, retry_times=4L, max_workers=0L, timeout=4L,
        service_urls='https://translate.google.com', 
        debug=FALSE, reg_finalizer=TRUE)
}
\arguments{
  \item{name}{a character string giving the name of the Python object.}
  \item{writing}{a character string giving if romanisation should be performed. 
        Allowed values are \code{"native"}, \code{"roman"} and \code{"both"}. }
  \item{proxy}{a character string giving the url to a proxy server.}
  \item{retry_times}{an integer giving the number of times the function retries to query when the query fails.}
  \item{max_workers}{an integer giving the maximal number of threads used.}
  \item{timeout}{an integer giving the HTTP request timeout in seconds.}
  \item{service_urls}{a character vector giving a Google translate url list.}
  \item{debug}{a logical, if TRUE debugging is activated.}
  \item{reg_finalizer}{a logical, if TRUE a finalizer is registered.}
}
\value{
  An R6 Goslate object with the following methods.
}

\section{Methods}{
\describe{\item{bach_translate}{\code{(filename, to, from='auto')} \cr 
    The method \code{bach_translate} translates a file from language 
    \var{from} to language \var{to} and returns a character vector 
    which contains the translated text.
    \itemize{
      \item{\strong{filename}} a character string giving the name of the file from which the data should be read.
      \item{\strong{to}} a character string giving the output language in ISO 639-1 code.
      \item{\strong{from}} a character string giving the input language in ISO 639-1 code.
    }
}}
\describe{\item{detect}{\code{(text)} \cr
    The method \code{detect} detects the language of the input \var{text} and returns a vector of the detected languages in ISO 639-1 codes.
    \itemize{
      \item{\strong{text}} a character vector with input text.
    }
}}
\describe{\item{get_languages}{\code{()} \cr
    The method \code{get_languages} returns a vector containing the supported languages. The values are the names of the supported languages and the 
    vector names are the corresponding ISO 630-1 codes.
}}
\describe{\item{lookup_dictionary}{\code{(text, to, from='auto', }
    \code{examples=FALSE, etymology=FALSE, pronunciation=FALSE,}
    \code{related_words=FALSE, synonyms=FALSE, antonyms=FALSE, output_language=NULL)} \cr
    The method \code{lookup_dictionary} looks up a given word (or phrase) and retrieves its translation and additional information.
    \itemize{
      \item{\strong{text}} a character string giving the word (phrase) to be looked up.
      \item{\strong{to}} a character string giving the output language in ISO 639-1 code.
      \item{\strong{from}} a character string giving the language of the parameter \var{text} in ISO 639-1 code.
      \item{\strong{examples}} a logical, if TRUE example sentences are included in the return value.
      \item{\strong{etymology}} a logical, if TRUE the etymology of the word is included in the return value.
      \item{\strong{pronunciation}} a logical, if TRUE the pronunciation of the word is included in the return value.
      \item{\strong{synonyms}} a logical, if TRUE synonyms are included in the return value.
      \item{\strong{antonyms}} a logical, if TRUE antonyms are included in the return value.
      \item{\strong{output_language}} a character string controlling the output language from several names of the returned list. The output language should be given in ISO 639-1 code.
    }
}}
\describe{\item{translate}{\code{(text, to, from='auto')} \cr
    The method \code{translate} translates a given \var{text} from language \var{from} to language \var{to} and returns a character vector 
    which contains the translated text.
    \itemize{
      \item{\strong{text}} a character vector (or list of character strings) giving
                  the text to be translated.
      \item{\strong{to}} a character string giving the output language in ISO 639-1 code.
      \item{\strong{from}} a character string giving the input language in ISO 639-1 code.
    }
}}
}
\seealso{
\url{https://pypi.python.org/pypi/goslate#batch-translation} \cr
\url{http://pythonhosted.org/goslate/} \cr
\url{https://bitbucket.org/zhuoqiang/goslate}
}
\examples{
\dontshow{PythonInR:::pyCranConnect()}
if ( PythonInR:::pyIsConnected() ) {
## connect with default settings
gs <- Goslate()

## use a proxy
\dontrun{
gs <- Goslate(proxy="https://proxy-domain.name:8080")
}

## create a new Goslate object
gs <- Goslate(name="Goslate", writing="roman", proxy=NULL, 
    retry_times=4L, max_workers=0L, timeout=4L,
    service_urls=c('https://translate.google.com', 'https://translate.google.at'), 
    debug=FALSE)

## translate text from german to english
gs$translate(c('Natural language support but running in an English locale', 
'Free online language translation service instantly translates text and web pages.'
), to='en', from='de')
## translate text from spanish to english
gs$translate(c("Hola", "Mundo"), 'en')

\dontrun{
## translate a file from russian to german
gs$bach_translate("my_file.txt", to='de', from='ru')
}

## detect the language from a given text
language <- gs$detect("oversette")
gs$get_languages()[language]

## get the available languages
gs$get_languages()

## lookup a word in the dictionary
gs$lookup_dictionary("lookup", to="de")


}
}
\keyword{goslate}
