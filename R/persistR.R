#' Easily store and retrieve credentials (or other information) so that they are
#' not revealed in source code.
#'
#' \code{setPersistent} and \code{getPersistent} store and retrieve values in an
#' .RData file so that they persist across R sessions.
#'
#' \code{setPersistent} manages values in a named list stored in an .RData file.
#' If value is passed with a key that already exists in the stored list, the
#' value in the stored list is replaced.  If the key doesn't already exist as a
#' name in the stored list, the value is added.  If a value of NULL is passed
#' for a key that already exists, the associated value and key (name) are
#' removed. Passing a new key with a NULL value has no effect on the stored
#' list.
#'
#' \code{getPersistent} retrieves the value associated with the "key" from the
#' stored list.
#'
#' @return \code{setPersistent} returns the name of the full path to the file
#'   where the value was stored, invisibly. \code{getPersistent} returns value
#'   associated with the \code{key} name in the stored list, or NULL if no value
#'   in the saved list is associated with the \code{key}.
#' @examples
#' # Example for SQL credentials
#' setPersistent(
#'   key = "MySQL:Example",
#'   value =
#'     list(
#'       user = "bob",
#'       password = "bobsPW",
#'       host = "127.0.0.1",
#'       port = 3306,
#'       dbname = "exampleSchema"
#'     )
#' )
#'
#' # Retrieve the list of credentials
#' getPersistent(key = "MySQL:Example")
#'
#' # Remove credentials when no longer needed
#' setPersistent(key = "MySQL:Example", value = NULL)
#'
#' # Example for ssh command
#' setPersistent(
#'   key = "SSH:remote3306",
#'   value = "ssh -i /home/bob/remote.pem -f -N -L 3306:192.168.1.5:3306 bob@192.168.1.5"
#' )
#'
#' # Retrieve the command as a character vector
#' getPersistent(key = "SSH:remote3306")
#'
#' # Remove command when no longer needed
#' setPersistent(key = "SSH:remote3306", value = NULL)
#'
#' @param key A character string that represents the name of the item to add to,
#'   retrieve from, or remove from the stored list.  If key is NULL for
#'   \code{getPersistent}, the names of all of the keys in the stored list will
#'   be returned.
#' @param value Value to add to the stored list, using the name in \code{key}.
#'   Set to NULL to remove the list entry associated with \code{key}.
#' @param filename The fully qualified name of the .Rdata file where a named
#'   list of values is stored.  (You may choose to have more than one file to
#'   store different collections of values.)  If filename is NULL, the value of
#'   the "PERSISTENT_FILE" variable from the .REnviron config file will be used.
#'   If there is no "PERSISTENT_FILE" value in the .REnviron config file, the
#'   default value of "~\.persistR.RData" will be used.
#' @export
setPersistent = function(key, value, filename = NULL) {
  if(is.null(filename)) filename = Sys.getenv("PERSISTENT_FILE")
  if(nchar(filename) == 0) filename = file.path(path.expand("~"), ".persistR.RData")
  if(file.exists(filename)){
    load(filename, envir = environment())
  } else {
    persistentData = list()
  }
  if(is.null(value)){
    persistentData = persistentData[names(persistentData) != key]
  } else {
    persistentData[[key]] = value
  }
  save(persistentData, file = filename)
  invisible(filename)
}

#' @rdname setPersistent
#' @export
getPersistent = function(key = NULL, filename = NULL) {
  filesource = "from the 'filename' parameter."
  if(is.null(filename)) {
    filename = Sys.getenv("PERSISTENT_FILE")
    filesource = "from the 'PERSISTENT_FILE' variable set in the .Renviron file."
  }
  if(nchar(filename) == 0) {
    filename = file.path(path.expand("~"), ".persistR.RData")
    filesource = "by default."
  }
  if(!file.exists(filename)){
    stop(
      "The requested bingeR data file '", filename,
      "' doesn't exist. \n  The file location and name were determined ",
      filesource,
      "\n  The default file is '~/.persistR.RData'\n  To read data from a specific file, pass the fully specified file name as the 'filename' parameter\n    or set the 'PERSISTENT_FILE' variable within the '.Renviron' file."
    )
  }
  load(filename, envir = environment())
  if(is.null(key)) return(names(persistentData))
  persistentData[[key]]
}
