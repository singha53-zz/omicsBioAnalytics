# functions from orphaned aws.dynamodb R-library
# https://github.com/cloudyr/aws.dynamodb/tree/master/R

#' @export
#' @rdname map_attributes
map_attributes <- function(item) {
  item_formatted <- list()
  for (i in seq_along(item)) {
    if (is.null(item[[i]])) {
      item_formatted[[i]] <- list(NULL = TRUE)
    } else if (is.list(item[[i]])) {
      if (any(names(item[[i]]) %in% "")) {
        item_formatted[[i]] <- list(L = unname(item[[i]]))
      } else {
        item_formatted[[i]] <- list(M = item[[i]])
      }
    } else if (is.raw(item[[i]])) {
      item_formatted[[i]] <- list(B = jsonlite::base64_enc(item[[i]]))
    } else if (is.logical(item[[i]])) {
      item_formatted[[i]] <- list(BOOL = item[[i]])
    } else if (is.numeric(item[[i]])) {
      if (length(item[[i]]) == 1L) {
        item_formatted[[i]] <- list(N = item[[i]])
      } else {
        item_formatted[[i]] <- list(NS = item[[i]])
      }
    } else {
      if (length(item[[i]]) == 1L) {
        item_formatted[[i]] <- list(S = as.character(item[[i]]))
      } else {
        item_formatted[[i]] <- list(SS = as.character(item[[i]]))
      }
    }
  }
  names(item_formatted) <- names(item)
  return(item_formatted)
}

#' @export
#' @rdname put_item
put_item <- function(
  table,
  item,
  condition = NULL,
  return_value = c("NONE", "ALL_OLD")) {
  # format items
  print(map_attributes(item))
  item_formatted <- map_attributes(item)

  return_value <- match.arg(return_value)
  bod <- list(TableName = table,
    ReturnValues = return_value,
    Item = item_formatted)
  if (!is.null(condition)) {
    bod$ConditionExpression <- condition
  }
  out <- dynamo_http(verb = "POST", body = bod,
    target = "DynamoDB_20120810.PutItem")
  if (return_value == "NONE") {
    return(NULL)
  } else {
    return(out)
  }
}

#' @export
#' @rdname dynamo_http
dynamo_http <- function(
  verb = "GET",
  headers = list(),
  body = NULL,
  target,
  verbose = getOption("verbose", FALSE),
  region = Sys.getenv("AWS_DEFAULT_REGION", "us-east-1"),
  key = NULL,
  secret = NULL,
  session_token = NULL) {
  # locate and validate credentials
  credentials <- aws.signature::locate_credentials(key = key, secret = secret,
    session_token = session_token, region = region, verbose = verbose)
  key <- credentials[["key"]]
  secret <- credentials[["secret"]]
  session_token <- credentials[["session_token"]]
  region <- credentials[["region"]]

  # generate request signature
  d_timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  url <- paste0("https://dynamodb.", region, ".amazonaws.com")
  sig <- aws.signature::signature_v4_auth(
    datetime = d_timestamp,
    region = region,
    service = "dynamodb",
    verb = verb,
    action = "/",
    query_args = NULL,
    canonical_headers = list(host = paste0("dynamodb.",
      region, ".amazonaws.com"),
      `x-amz-date` = d_timestamp),
    request_body = ifelse(length(body),
      jsonlite::toJSON(body, auto_unbox = TRUE), ""),
    key = key,
    secret = secret,
    session_token = session_token,
    verbose = verbose)
  # setup request headers
  headers[["x-amz-date"]] <- d_timestamp
  headers[["x-amz-target"]] <- target
  headers[["x-amz-content-sha256"]] <- sig$BodyHash
  headers[["Authorization"]] <- sig[["SignatureHeader"]]
  if (!is.null(session_token) && session_token != "") {
    headers[["x-amz-security-token"]] <- session_token
  }
  req_headers <- do.call(httr::add_headers, headers)

  # execute request
  if (verb == "GET") {
    r <- httr::GET(url, req_headers, body = body, encode = "json")
  } else if (verb == "POST") {
    r <- httr::POST(url, req_headers, body = body, encode = "json")
  } else if (verb == "POST") {
    r <- httr::PUT(url, req_headers, body = body, encode = "json")
  } else if (verb == "DELETE") {
    r <- httr::DELETE(url, req_headers, encode = "json")
    if (!httr::http_error(r)) {
      return(TRUE)
    }
  }
  cont <- httr::content(r, "text", encoding = "UTF-8")
  if (httr::http_error(r)) {
    httr::warn_for_status(r)
    h <- httr::headers(r)
    out <- try(structure(jsonlite::fromJSON(cont),
      headers = h, class = "aws_error"))
    if (inherits(out, "try-error")) {
      out <- xml2::as_list(xml2::read_xml(cont))
    }
    attr(out, "request_canonical") <- sig$CanonicalRequest
    attr(out, "request_string_to_sign") <- sig$StringToSign
    attr(out, "request_signature") <- sig$SignatureHeader
  } else {
    out <- try(jsonlite::fromJSON(cont, simplifyDataFrame = FALSE))
    if (inherits(out, "try-error")) {
      out <- xml2::as_list(xml2::read_xml(cont))
    }
  }
  return(out)
}
