get_session_id <- function(session, id){
  if (inherits(session , "session_proxy")) {
    id <- session$ns(id)
  }
  id
}

