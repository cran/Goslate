\name{check_service_urls}
\alias{check_service_urls}
\title{Check if the Service Urls are Valid}
\description{
Check if the given service urls are valid.
}
\usage{
check_service_urls(service_urls)
}
\arguments{
  \item{service_urls}{a character vector giving the service urls.}
}
\value{
  A logical vector indicating which urls are valid.
}
\examples{
\dontshow{PythonInR:::pyCranConnect()}
if ( PythonInR:::pyIsConnected() ){ 
\dontrun{
service_urls <- c('https://translate.google.com', 
                  'https://translate.google.at', 
                  'https://translate.google.en')
check_service_urls(service_urls)
}
}
}

