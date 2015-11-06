
init_concurrent_querying <- '
try:
    import concurrent.futures
    %s["executor"] = concurrent.futures.ThreadPoolExecutor(max_workers=%s)
except ImportError:
    %s["executor"] = None
'

init_proxy <- '
import urllib2
proxy_handler = urllib2.ProxyHandler({"https" : "%s"})
%s["opener"] = urllib2.build_opener(urllib2.HTTPHandler(proxy_handler),
                                    urllib2.HTTPSHandler(proxy_handler))
'

as_tuple <- function(x) {
    class(x) <- "tuple"
    x
}

GoslateObject <- R6Class(
    "Goslate",
    public=list(
        name=character(),
        name_get_languages=character(),
        name_detect=character(),
        name_lookup_dictionary=character(),
        name_ltranslate=character(),
        translate = function(text, to, from='auto'){
            pyCall(self$name_ltranslate,
                   kwargs=list(text=as.list(text), target_language=to,
                               source_language=from))
        },
        batch_translate = function(filename, to, from='auto') {
            cmd <- "list(%s.translate(open('%s', 'r'), target_language='%s', source_language='%s'))"
            return(pyGet(sprintf(cmd, self$name, filename, to, from)))
        },
        get_languages = function() pyCall(self$name_get_languages),
        detect = function(text){
            pyCall(self$name_detect, kwargs=list(text=as.list(text)))
        },
        lookup_dictionary = function(text, to, from='auto',
            examples=FALSE, etymology=FALSE, pronunciation=FALSE,
            related_words=FALSE, synonyms=FALSE, antonyms=FALSE,
            output_language=NULL) {
            pyCall(self$name_lookup_dictionary,
                   kwargs=list(text=text, target_language=to, source_language=from,
                       examples=examples, etymology=etymology, pronunciation=pronunciation,
                       related_words=related_words, synonyms=synonyms,
                       antonyms=antonyms, output_language=output_language))
        },
        initialize = function(name="Goslate",
            writing=c("native", "roman", "both"), proxy=NULL,
            retry_times=4L, max_workers=0L, timeout=4L,
            service_urls=c('https://translate.google.com'), debug=FALSE,
            reg_finalizer=TRUE) {

            writing <- match.arg(writing)

            ## set names
            self$name <- name
            self$name_get_languages <- sprintf("%s.get_languages", self$name)
            self$name_detect <- sprintf("%s.ldetect", self$name)
            self$name_lookup_dictionary <- sprintf("%s.lookup_dictionary", self$name)
            self$name_ltranslate <- sprintf("%s.ltranslate", self$name)

            ## load python
            packagePath <- find.package("Goslate")
            wd <- getwd()
            pyImport("os")
            pyExec(sprintf('os.chdir(%s)', shQuote(packagePath)))
            pyExec("import python.goslate.goslate as goslate")
            setwd(wd)

            ## default settings
            kwargs <- pyGet("dict()", FALSE)
            kwargs['opener'] <- NULL

            ## max_workers
            if ( max_workers == 0 ) {
                kwargs['executor'] <- NULL
            } else {
                pyExec( sprintf(init_concurrent_querying, 
                                kwargs$py.variableName, 
                                max_workers,
                                kwargs$py.variableName) )
            }

            ## writing
            w <- match.arg(writing)
            if (w == "native") wt <- 'trans'
            if (w == "roman")  wt <- 'translit'
            if (w == "both")   wt <- c('trans', 'translit')
            kwargs["writing"] <- as_tuple(wt)
           
            ## proxy
            if ( !is.null(proxy) ) {
                pyExec(sprintf(init_proxy, proxy, kwargs$py.variableName))
            }

            ## retry_times
            kwargs['retry_times'] <- retry_times

            ## timeout
            kwargs['timeout'] <- timeout

            ## service_urls
            kwargs['service_urls'] <- as_tuple(service_urls)

            ## debug
            kwargs['debug'] <- debug

            pyExec(sprintf("%s = goslate.Goslate(**%s)", self$name, kwargs$py.variableName))

            ## wrapper functions
            param <- "text, target_language, source_language"
            pyExec( sprintf("%s.ltranslate = lambda %s: list(%s.translate(%s))", 
                            self$name, param, self$name, param) )
            pyExec( sprintf("%s.ldetect = lambda text: list(%s.detect(text))", 
                            self$name, self$name) )

            if ( reg_finalizer ) {
                del_code <- "try:\n\tdel(%s)\nexcept:\n\tpass"
                reg.finalizer(self,
                    function(self) pyExec(sprintf(del_code, self$name)), onexit = TRUE)
            }
            
        }
    ) )


##  --------------------------------------------------------
##  Goslate
##  =======
##  --------------------------------------------------------
Goslate <- function(name="Goslate", writing=c("native", "roman", "both"), 
                    proxy=NULL, retry_times=4L, max_workers=0L, timeout=4L,
                    service_urls='https://translate.google.com', debug=FALSE,
                    reg_finalizer=TRUE) {
    writing <- match.arg(writing)
    return(GoslateObject$new(name, writing, proxy, retry_times, max_workers, 
                             timeout, service_urls, debug, reg_finalizer))
}

##  --------------------------------------------------------
##  Check Service Urls
##  ==================
##  --------------------------------------------------------
check_service_url_code <- '
try:
    txt <- c("is", "free", "software", "and", "comes", "with", 
             "ABSOLUTELY",  "NO", "WARRANTY", "You", "are", "welcome", 
             "to", "redistribute",  "it", "under", "certain", "conditions", 
             "Type", "license", "or",  "licence", "for", "distribution", 
             "details", "Natural", "language",  "support", "but", "running", 
             "in", "an", "English", "locale",  "is", "a", "collaborative", 
             "project", "with", "many", "contributors",  "Type", "contributors", 
             "for", "more", "information", "and",  "citation", "on", "how", 
             "to", "cite", "or", "packages",  "in", "publications", 
             "Type", "demo", "for", "some", "demos",  "help", "for", "online", 
             "help", "or", "helpstart", "for", "an",  "HTML", "browser", 
             "interface", "to", "help")
    word <- sample(txt, 1)
    x = gs_check_service_url.translate(word, target_language="de")
except:
    x = None
'

check_service_url <- function(service_url) {
    check_name <- "gs_check_service_url"
    gs_check <- Goslate(name=check_name, service_urls=service_url,
                        reg_finalizer = FALSE)
    x <- pyExecg(check_service_url_code)
    pyExec(sprintf("try:\n\tdel(%s)\nexcept:\n\tpass", check_name))
    return( !is.null(x) )
}

check_service_urls <- function(service_urls) {
    sapply(service_urls, check_service_url)
}
