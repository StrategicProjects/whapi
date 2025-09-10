#' Truncate long strings for logging
#'
#' @description
#' Helper function to shorten long strings when printing to logs.
#' If the input string exceeds `max` characters, it is truncated and
#' suffixed with `"... (truncated)"`.
#'
#' @param x A character string (or coercible to character).
#' @param max Integer. Maximum number of characters to display (default: 2000).
#'
#' @return A character string, possibly truncated.
#'
#' @examples
#' whapi_trunc("short text", max = 10)
#' whapi_trunc(paste(rep("a", 5000), collapse = ""), max = 20)
#' @export
whapi_trunc <- function(x, max = 2000L) {
  if (is.null(x)) return(NULL)
  s <- as.character(x)
  if (nchar(s) > max) paste0(substr(s, 1, max), "... (truncated)") else s
}

#' Redact sensitive values in headers/cookies
#'
#' @description
#' Replaces sensitive header or cookie values (e.g., `Authorization`,
#' `Cookie`, `X-API-Key`) with the literal string `"<redacted>"`.
#' Useful when logging HTTP requests while avoiding credential leaks.
#'
#' @param name Header or cookie name (character).
#' @param value Header or cookie value (character).
#'
#' @return The original value, or `"<redacted>"` if the header is considered sensitive.
#'
#' @examples
#' whapi_redact("Authorization", "Bearer abc123")
#' whapi_redact("Content-Type", "application/json")
#' @export
whapi_redact <- function(name, value) {
  sens <- c("authorization", "cookie", "set-cookie", "x-api-key",
            "x-auth-token", "proxy-authorization")
  if (tolower(name) %in% sens) return("<redacted>")
  value
}




#' Safely coerce to numeric
#'
#' @description
#' Utility to coerce a value into numeric (`double`), returning `NA_real_` if
#' input is `NULL` or `NA`.
#'
#' @param x Any R object (usually scalar).
#'
#' @return A numeric scalar (`double`) or `NA_real_`.
#'
#' @examples
#' whapi_to_num("123")
#' whapi_to_num(NULL)   # NA_real_
#'
#' @keywords internal
#' @export
whapi_to_num <- function(x) if (is.null(x) || is.na(x)) NA_real_ else as.numeric(x)


#' Coerce to POSIXct (UTC) safely
#'
#' @description
#' Converts a numeric timestamp (seconds since Unix epoch) into a
#' `POSIXct` in **UTC**. Returns `NA` when input is `NA`.
#'
#' @param x Numeric scalar (seconds since "1970-01-01 00:00:00 UTC").
#'
#' @return A `POSIXct` scalar or `NA`.
#'
#' @examples
#' whapi_to_posix(1756500000)
#' whapi_to_posix(NA)   # NA
#'
#' @keywords internal
#' @export
whapi_to_posix <- function(x) if (is.na(x)) as.POSIXct(NA) else as.POSIXct(x, origin = "1970-01-01", tz = "UTC")


#' Collapse a "changes" vector into a single string
#'
#' @description
#' Normalizes the webhook `changes` field (which may be `NULL`, vector, or nested list)
#' into a single comma-separated string of unique values.
#'
#' @param x A character vector, list, or `NULL` from Whapi payload.
#'
#' @return A character scalar (comma-separated) or `NA_character_` if empty.
#'
#' @examples
#' whapi_scalarize_change(c("text","text","reactions"))
#' # "text,reactions"
#'
#' whapi_scalarize_change(NULL) # NA
#'
#' @keywords internal
#' @export
whapi_scalarize_change <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.character(x) && length(x) == 1L) return(x)
  vals <- unique(as.character(unlist(x, recursive = TRUE, use.names = FALSE)))
  if (!length(vals)) NA_character_ else paste(vals, collapse = ",")
}


#' Ensure "messages" or "updates" input is always a list of objects
#'
#' @description
#' Utility to normalize webhook payloads so downstream parsers can safely `map_dfr()`.
#' Accepts single objects, arrays of objects, or `NULL`, and always returns a list.
#'
#' @param x Input object: `NULL`, a list (single message/update), or a list of lists.
#' @param kind Either `"message"` or `"update"`. Defines which keys to look for when
#'   deciding if input is a single object.
#'
#' @return A list of message/update objects (possibly length 0).
#'
#' @examples
#' whapi_as_array(list(id="abc", type="text"), kind="message")
#' whapi_as_array(NULL, "update") # empty list
#'
#' @keywords internal
#' @export
whapi_as_array <- function(x, kind = c("message","update")) {
  kind <- match.arg(kind)
  if (is.null(x)) return(list())
  if (is.list(x) && length(x) && is.list(x[[1]]) && length(names(x)) == 0) return(x)
  keys_msg <- c("id","type","chat_id","timestamp","from","from_name","from_me","text","action","reply","context")
  keys_upd <- c("id","trigger","before_update","after_update","changes")
  keys_expect <- if (kind == "message") keys_msg else keys_upd
  if (is.list(x) && any(keys_expect %in% names(x))) return(list(x))
  if (is.list(x) && length(x) && is.list(x[[1]])) return(x)
  list()
}


#' Convert R objects into pretty JSON (character)
#'
#' @description
#' Wrapper around `jsonlite::toJSON()` to serialize lists/data.frames into
#' human-readable JSON for logging or persistence.
#' Returns `NA_character_` if `jsonlite` is not installed or conversion fails.
#'
#' @param x Any R object (list, vector, data.frame).
#'
#' @return Character scalar containing pretty-printed JSON, or `NA_character_`.
#'
#' @examples
#' whapi_to_pretty_json(list(a=1, b=2))
#' whapi_to_pretty_json(mtcars[1:2,1:2])
#'
#' @keywords internal
#' @export
whapi_to_pretty_json <- function(x) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(NA_character_)
  ch <- tryCatch(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", pretty = TRUE),
                 error = function(e) NA)
  if (is.na(ch)) NA_character_ else as.character(ch)
}

#' Convert R objects to pretty JSON for logging
#'
#' @description
#' Converts R objects to a JSON string for easier inspection in logs.
#' Falls back to `utils::str()` output if `jsonlite` is not available
#' or if JSON conversion fails. Long outputs are truncated with [whapi_trunc].
#'
#' @param x An R object (list, data frame, etc.).
#' @param max Integer. Maximum number of characters to print (default: 2000).
#'
#' @return A character string containing JSON (pretty-printed if possible).
#'
#' @examples
#' whapi_log_pretty_json(list(a = 1, b = "test"))
#' whapi_log_pretty_json(mtcars[1:2, ], max = 100)
#' @export
whapi_log_pretty_json <- function(x, max = 2000L) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    return(whapi_trunc(utils::capture.output(str(x)), max))
  txt <- tryCatch(
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA, pretty = TRUE),
    error = function(e) NULL
  )
  whapi_trunc(if (is.null(txt)) utils::capture.output(str(x)) else txt, max = max)
}



#' Parse the raw body of a Plumber request
#'
#' @description
#' Attempts to parse the body of a Plumber `req` object.
#' Supports both `req$body` (already parsed) and `req$bodyRaw` (raw binary).
#' If the body looks like JSON (`{...}` or `[...]`), it tries to parse it
#' with `jsonlite::fromJSON`. Otherwise, it returns the raw text.
#'
#' @param req A Plumber request object (list-like).
#'
#' @return A list representing the parsed body, or `NULL` if no body is found.
#'
#' @examples
#' # Inside a Plumber endpoint:
#' # parsed <- whapi_parse_body(req)
#' # if (!is.null(parsed)) str(parsed)
#' @export
whapi_parse_body <- function(req) {
  if (!is.null(req$body)) {
    return(if (is.list(req$body)) req$body else as.list(req$body))
  }
  if (!is.null(req$bodyRaw)) {
    raw <- req$bodyRaw
    if (length(raw)) {
      ch <- tryCatch(rawToChar(raw), error = function(e) NULL)
      if (!is.null(ch) && nzchar(ch)) {
        if (requireNamespace("jsonlite", quietly = TRUE) &&
            grepl("^\\s*[\\{\\[]", ch)) {
          pj <- tryCatch(jsonlite::fromJSON(ch, simplifyVector = FALSE), error = function(e) NULL)
          return(pj %||% list(text = ch))
        }
        return(list(text = ch))
      }
    }
  }
  NULL
}


#' Log details of a Plumber request object using the cli package
#'
#' @description
#' Prints a structured summary of a `req` object (Plumber request) to the console,
#' with colored and formatted output using the **cli** package.
#' This helper is useful for debugging APIs, inspecting request metadata,
#' and logging incoming payloads in a readable way.
#'
#' The function logs:
#' - HTTP method, path, query string, host, client IP/port, content type, and length.
#' - Parsed arguments: `argsQuery`, `argsBody`, `args`.
#' - Headers (with sensitive values redacted).
#' - Cookies (always redacted).
#' - Parsed request body (`req$body` or `req$postBody`).
#'
#' @param req A Plumber request object, usually passed automatically inside an endpoint.
#'   Must contain fields such as `REQUEST_METHOD`, `PATH_INFO`, `HTTP_HOST`, etc.
#' @param show_headers Logical. Whether to print request headers (default: `TRUE`).
#'   Sensitive values are redacted.
#' @param show_cookies Logical. Whether to print cookies (default: `TRUE`).
#'   Values are always redacted.
#' @param show_body Logical. Whether to print the parsed body or raw `postBody` (default: `TRUE`).
#'   Useful for debugging JSON payloads.
#' @param max_chars Integer. Maximum number of characters to print for large JSON or raw bodies.
#'   Defaults to `2000`.
#'
#' @return Invisibly returns `NULL`. The function is called for its side-effect
#'   of printing formatted logs to the console.
#'
#' @examples
#' \dontrun{
#' # Inside a Plumber endpoint
#' #* @post /myendpoint
#' function(req, res) {
#'   whapi_log_plumber_req(req) # Prints nicely formatted info about the incoming request
#'   list(success = TRUE)
#' }
#'
#' # Print only metadata, no headers/body
#' whapi_log_plumber_req(req, show_headers = FALSE, show_body = FALSE)
#' }
#' @importFrom stats setNames
#' @importFrom utils str
#' @seealso [cli::cli_inform()], [cli::cli_rule()], [cli::cli_verbatim()]
#' @export
whapi_log_plumber_req <- function(req,
                              show_headers = TRUE,
                              show_cookies = TRUE,
                              show_body    = TRUE,
                              max_chars    = 2000L) {
    if (!requireNamespace("cli", quietly = TRUE)) stop("Package 'cli' is required.")

    method <- req$REQUEST_METHOD %||% "-"
    path   <- paste0(req$SCRIPT_NAME %||% "", req$PATH_INFO %||% "")
    qs     <- req$QUERY_STRING %||% ""
    host   <- req$HTTP_HOST %||% req$SERVER_NAME %||% "-"
    ip     <- req$REMOTE_ADDR %||% "-"
    port   <- req$REMOTE_PORT %||% "-"
    ct     <- req$HTTP_CONTENT_TYPE %||% req$CONTENT_TYPE %||% "-"
    clen   <- req$HTTP_CONTENT_LENGTH %||% req$CONTENT_LENGTH %||% "-"

    cli::cli_rule(center = "Incoming request")
    cli::cli_h1(sprintf("%s %s", method, path))
    if (nzchar(qs)) cli::cli_inform(c("?" = paste0("Query: ", qs)))
    cli::cli_inform(c(
      ">" = paste0("Host: ", host),
      ">" = paste0("Client: ", ip, ":", port),
      ">" = paste0("Content-Type: ", ct, " | Length: ", clen)
    ))

    # argsQuery / argsBody / args
    if (!is.null(req$argsQuery) && length(req$argsQuery)) {
      cli::cli_div(theme = list(.val = list(color = "cyan")))
      cli::cli_text("{.strong argsQuery}:")
      cli::cli_verbatim(whapi_log_pretty_json(req$argsQuery, max = max_chars))
      cli::cli_end()
    }
    if (!is.null(req$argsBody) && length(req$argsBody)) {
      cli::cli_div(theme = list(.val = list(color = "cyan")))
      cli::cli_text("{.strong argsBody}:")
      cli::cli_verbatim(whapi_log_pretty_json(req$argsBody, max = max_chars))
      cli::cli_end()
    }
    if (!is.null(req$args) && length(req$args)) {
      cli::cli_div(theme = list(.val = list(color = "cyan")))
      cli::cli_text("{.strong args}:")
      cli::cli_verbatim(whapi_log_pretty_json(req$args, max = max_chars))
      cli::cli_end()
    }

    # Headers
    if (isTRUE(show_headers) && !is.null(req$HEADERS) && length(req$HEADERS)) {
      headers <- lapply(names(req$HEADERS), function(nm) {
        val <- req$HEADERS[[nm]]
        setNames(list(whapi_trunc(whapi_redact(nm, val), max = max_chars)), nm)
      })
      headers <- do.call(c, headers)
      cli::cli_div(theme = list(.val = list(color = "magenta")))
      cli::cli_text("{.strong headers}:")
      cli::cli_verbatim(whapi_log_pretty_json(headers, max = max_chars))
      cli::cli_end()
    }

    # Cookies
    if (isTRUE(show_cookies) && !is.null(req$cookies) && length(req$cookies)) {
      ck <- lapply(names(req$cookies), function(nm) setNames(list("<redacted>"), nm))
      ck <- do.call(c, ck)
      cli::cli_div(theme = list(.val = list(color = "magenta")))
      cli::cli_text("{.strong cookies}:")
      cli::cli_verbatim(whapi_log_pretty_json(ck, max = max_chars))
      cli::cli_end()
    }

    # Body
    if (isTRUE(show_body)) {
      parsed <- whapi_parse_body(req)
      if (!is.null(parsed)) {
        cli::cli_div(theme = list(.val = list(color = "green")))
        cli::cli_text("{.strong body}:")
        cli::cli_verbatim(whapi_log_pretty_json(parsed, max = max_chars))
        cli::cli_end()
      } else if (!is.null(req$postBody) && nzchar(req$postBody)) {
        cli::cli_div(theme = list(.val = list(color = "green")))
        cli::cli_text("{.strong postBody}:")
        cli::cli_verbatim(whapi_trunc(req$postBody, max = max_chars))
        cli::cli_end()
      }
    }

    cli::cli_rule()
  }


#' Parse Whapi "messages" array into a normalized tibble
#'
#' @description
#' Converts the webhook field `messages` (from Whapi.Cloud) into a **row-per-message**
#' tibble with a stable schema.
#' It extracts common metadata (ids, chat, author, timestamps, source), plain text
#' content, action edits/reactions, and **interactive replies** along with the
#' **quoted context** (headers, body, sections/buttons as JSON).
#'
#' @details
#' - Handles multiple shapes and normalizes to a list of messages via a helper
#'   `whapi_as_array(lst, "message")`.
#' - Reply types supported:
#'   - `reply$type == "list_reply"` -> uses `reply$list_reply$id/title/description`
#'   - `reply$type == "buttons_reply"` (or `"button_reply"`) -> uses
#'     `reply$buttons_reply$id/title` (or `reply$button_reply`)
#'   - Fallback: `reply$id/title/description/body` when available
#' - Context of the quoted message is captured in:
#'   `context_quoted_id`, `context_quoted_author`, `context_header`, `context_body`,
#'   `context_label`, `context_footer`, and serialized JSON for `sections` and `buttons`
#'   (`context_sections_json`, `context_buttons_json`) via `whapi_to_pretty_json()`.
#' - The column `change` is set to `"reply"` for reply messages, `"reactions"` when a
#'   reaction is present, or `"text"` for edit operations.
#' - Time fields: `timestamp` (epoch seconds, numeric) and `timestamp_dt` (POSIXct UTC).
#'
#' @param lst A list or single object representing Whapi webhook `messages`.
#'   Accepts `NULL`, a single message, or an array of messages.
#' @param channel_id Character. Channel identifier (usually passed down from
#'   [whapi_flatten_webhook()]).
#' @param event_type Character. Event type (`"messages"` by default).
#' @param event_act Character. Event action (`"post"` by default).
#'
#' @return A tibble with one row per message. Includes contextual columns
#'   (`channel_id`, `event_type`, `event_action`).
#'
#' @keywords internal
#' @export
whapi_parse_messages <- function(lst, channel_id = NULL, event_type = NULL, event_act = NULL) {
  lst <- whapi_as_array(lst, "message")
  if (!length(lst)) return(tibble::tibble())

  # valores de contexto (locais)
  ch_id <- channel_id %||% NA_character_
  et    <- event_type %||% "messages"
  ea    <- event_act   %||% "post"

  purrr::map_dfr(lst, function(m) {
    msg_id    <- m$id %||% NA_character_
    chat_id   <- m$chat_id %||% NA_character_
    from      <- m$from %||% NA_character_
    from_name <- m$from_name %||% NA_character_
    from_me   <- if (is.null(m$from_me)) NA else isTRUE(m$from_me)
    msg_type  <- m$type %||% NA_character_
    source    <- m$source %||% NA_character_
    ts        <- whapi_to_num(m$timestamp %||% NA_real_)

    body_curr <- purrr::pluck(m, "text", "body", .default = NA_character_)

    action_type    <- purrr::pluck(m, "action", "type", .default = NA_character_)
    edited_type    <- purrr::pluck(m, "action", "edited_type", .default = NA_character_)
    edited_body    <- purrr::pluck(m, "action", "edited_content", "body", .default = NA_character_)
    reaction_emoji <- purrr::pluck(m, "action", "emoji", .default = NA_character_)

    # ---------- REPLY & CONTEXT ----------
    reply_type <- purrr::pluck(m, "reply", "type", .default = NA_character_)

    lr <- purrr::pluck(m, "reply", "list_reply", .default = NULL)
    br <- purrr::pluck(m, "reply", "buttons_reply", .default = NULL) %||%
      purrr::pluck(m, "reply", "button_reply",  .default = NULL)

    if (identical(reply_type, "list_reply") && !is.null(lr)) {
      reply_id    <- lr$id    %||% NA_character_
      reply_title <- lr$title %||% NA_character_
      reply_desc  <- lr$description %||% NA_character_
    } else if (identical(reply_type, "buttons_reply") && !is.null(br)) {
      reply_id    <- br$id    %||% NA_character_
      reply_title <- br$title %||% NA_character_
      reply_desc  <- NA_character_
    } else if (identical(reply_type, "button_reply") && !is.null(br)) {
      reply_id    <- br$id    %||% NA_character_
      reply_title <- br$title %||% NA_character_
      reply_desc  <- NA_character_
    } else {
      reply_id    <- purrr::pluck(m, "reply", "id",    .default = NA_character_)
      reply_title <- purrr::pluck(m, "reply", "title", .default = NA_character_)
      reply_desc  <- purrr::pluck(m, "reply", "description", .default = NA_character_)
    }
    reply_body <- purrr::pluck(m, "reply", "body", .default = NA_character_)

    context_quoted_id     <- purrr::pluck(m, "context", "quoted_id",     .default = NA_character_)
    context_quoted_author <- purrr::pluck(m, "context", "quoted_author", .default = NA_character_)
    context_header        <- purrr::pluck(m, "context", "quoted_content", "header", .default = NA_character_)
    context_body          <- purrr::pluck(m, "context", "quoted_content", "body",   .default = NA_character_)
    context_label         <- purrr::pluck(m, "context", "quoted_content", "label",  .default = NA_character_)
    context_footer        <- purrr::pluck(m, "context", "quoted_content", "footer", .default = NA_character_)
    context_sections      <- purrr::pluck(m, "context", "quoted_content", "sections", .default = NULL)
    context_buttons       <- purrr::pluck(m, "context", "quoted_content", "buttons",  .default = NULL)
    context_sections_json <- if (is.null(context_sections)) NA_character_ else whapi_to_pretty_json(context_sections)
    context_buttons_json  <- if (is.null(context_buttons))  NA_character_ else whapi_to_pretty_json(context_buttons)

    change <- dplyr::coalesce(
      if (identical(msg_type, "reply")) "reply" else NA_character_,
      if (!is.na(reaction_emoji)) "reactions" else NA_character_,
      if (!is.na(edited_type))    "text"      else NA_character_
    )

    tibble::tibble(
      channel_id     = ch_id,
      event_type     = et,
      event_action   = ea,
      change         = change,
      id             = msg_id,
      chat_id        = chat_id,
      from           = from,
      from_name      = from_name,
      from_me        = from_me,
      msg_type       = msg_type,
      body_before    = NA_character_,
      body_after     = body_curr,
      body           = body_curr,
      action_type    = action_type,
      edited_type    = edited_type,
      edited_body    = edited_body,
      reaction_emoji = reaction_emoji,
      reaction_id    = msg_id,
      trigger_id     = NA_character_,
      target_id      = purrr::pluck(m, "action", "target", .default = NA_character_),
      # reply/context
      reply_type            = reply_type,
      reply_id              = reply_id,
      reply_title           = reply_title,
      reply_description     = reply_desc,
      reply_body            = reply_body,
      context_quoted_id     = context_quoted_id,
      context_quoted_author = context_quoted_author,
      context_header        = context_header,
      context_body          = context_body,
      context_label         = context_label,
      context_footer        = context_footer,
      context_sections_json = context_sections_json,
      context_buttons_json  = context_buttons_json,
      # tempo e fonte
      timestamp      = ts,
      timestamp_dt   = whapi_to_posix(ts),
      source         = source,
      trigger_source = NA_character_,
      raw            = list(m)
    )
  })
}



#' Parse Whapi "messages_updates" array (patch events) into a tibble
#'
#' @description
#' Converts the webhook field `messages_updates` (patch events) into a
#' **row-per-update** tibble with a stable schema.
#' Aligns **before/after** snapshots, the **trigger** (who/what caused the change),
#' and summarizes the change type(s) in `change`.
#'
#' @details
#' - Normalizes input to an array via `whapi_as_array(lst, "update")`.
#' - Safely extracts from `before_update`, `after_update`, and `trigger`.
#' - Uses `whapi_scalarize_change()` to turn `changes` (vector/list) into a single string.
#' - Preserves the same schema as [whapi_parse_messages()] so both outputs can be
#'   combined with `dplyr::bind_rows()`.
#' - Reply/context columns are included but set to `NA`.
#' - Time fields: `timestamp` (epoch seconds) and `timestamp_dt` (POSIXct UTC).
#'
#' @param lst A list or single object representing Whapi webhook `messages_updates`.
#'   Accepts `NULL`, a single update object, or an array.
#' @param channel_id Character. Channel identifier (usually passed down from
#'   [whapi_flatten_webhook()]).
#' @param event_type Character. Event type (`"messages_updates"` by default).
#' @param event_act Character. Event action (`"patch"` by default).
#'
#' @return A tibble with one row per update. Includes contextual columns
#'   (`channel_id`, `event_type`, `event_action`).
#'
#' @keywords internal
#' @export
whapi_parse_updates <- function(lst, channel_id = NULL, event_type = NULL, event_act = NULL) {
  lst <- whapi_as_array(lst, "update")
  if (!length(lst)) return(tibble::tibble())

  # contexto local
  ch_id <- channel_id %||% NA_character_
  et    <- event_type %||% "messages_updates"
  ea    <- event_act   %||% "patch"

  purrr::map_dfr(lst, function(u) {
    before  <- u$before_update %||% list()
    after   <- u$after_update  %||% list()
    trigger <- u$trigger       %||% list()

    ts <- whapi_to_num((after$timestamp %||% before$timestamp) %||%
                         purrr::pluck(trigger, "timestamp", .default = NA_real_))

    tibble::tibble(
      channel_id     = ch_id,
      event_type     = et,
      event_action   = ea,
      change         = whapi_scalarize_change(u$changes),
      id             = u$id %||% NA_character_,
      chat_id        = (after$chat_id %||% before$chat_id) %||%
        purrr::pluck(trigger,"chat_id", .default = NA_character_),
      from           = (after$from %||% before$from) %||%
        purrr::pluck(trigger,"from", .default = NA_character_),
      from_name      = (after$from_name %||% before$from_name) %||%
        purrr::pluck(trigger,"from_name", .default = NA_character_),
      from_me        = {
        fm <- (after$from_me %||% before$from_me) %||%
          purrr::pluck(trigger,"from_me", .default = NA)
        if (is.null(fm)) NA else isTRUE(fm)
      },
      msg_type       = (after$type %||% before$type) %||% NA_character_,
      body_before    = purrr::pluck(before, "text","body", .default = NA_character_),
      body_after     = purrr::pluck(after,  "text","body", .default = NA_character_),
      body           = purrr::pluck(after,  "text","body",
                                    .default = purrr::pluck(before,"text","body", .default = NA_character_)),
      action_type    = purrr::pluck(trigger,"action","type", .default = NA_character_),
      edited_type    = purrr::pluck(trigger,"action","edited_type", .default = NA_character_),
      edited_body    = purrr::pluck(trigger,"action","edited_content","body", .default = NA_character_),
      reaction_emoji = purrr::pluck(trigger,"action","emoji", .default = NA_character_),
      reaction_id    = purrr::pluck(after,"reactions", 1, "id", .default = NA_character_),
      trigger_id     = trigger$id %||% NA_character_,
      target_id      = purrr::pluck(trigger,"action","target", .default = NA_character_),
      # colunas reply/context mantidas como NA
      reply_type            = NA_character_,
      reply_id              = NA_character_,
      reply_title           = NA_character_,
      reply_description     = NA_character_,
      reply_body            = NA_character_,
      context_quoted_id     = NA_character_,
      context_quoted_author = NA_character_,
      context_header        = NA_character_,
      context_body          = NA_character_,
      context_label         = NA_character_,
      context_footer        = NA_character_,
      context_sections_json = NA_character_,
      context_buttons_json  = NA_character_,
      timestamp      = ts,
      timestamp_dt   = whapi_to_posix(ts),
      source         = (after$source %||% before$source) %||% NA_character_,
      trigger_source = trigger$source %||% NA_character_,
      raw            = list(u)
    )
  })
}

#' Flatten Whapi webhook payload into tidy rows for persistence (robust)
#'
#' @param payload list parsed from JSON (e.g. plumber `req$body`)
#' @param verbose logical, print logs with cli?
#' @return tibble with normalized fields
#' @export
whapi_flatten_webhook <- function(payload, verbose = TRUE) {
  if (!requireNamespace("purrr",   quietly = TRUE)) stop("Package 'purrr' is required.")
  if (!requireNamespace("tibble",  quietly = TRUE)) stop("Package 'tibble' is required.")
  if (!requireNamespace("dplyr",   quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required.")
  if (!requireNamespace("cli",     quietly = TRUE)) stop("Package 'cli' is required.")

  if (isTRUE(verbose)) {
    cli::cli_inform(c(
      "i" = "Parsing Whapi webhook payload...",
      ">" = paste0("Top-level keys: ", paste(names(payload %||% list()), collapse = ", "))
    ))
  }

  channel_id <- payload$channel_id %||% NA_character_
  event_type <- purrr::pluck(payload, "event", "type",  .default = NA_character_)
  event_act  <- purrr::pluck(payload, "event", "event", .default = NA_character_)

  # >>> Passa o contexto <<<
  df_msgs <- whapi_parse_messages(
    payload$messages %||% list(),
    channel_id = channel_id, event_type = event_type, event_act = event_act
  )
  df_upd  <- whapi_parse_updates(
    payload$messages_updates %||% list(),
    channel_id = channel_id, event_type = event_type, event_act = event_act
  )
  df      <- dplyr::bind_rows(df_msgs, df_upd)

  if (!nrow(df)) {
    if (isTRUE(verbose)) cli::cli_warn("Payload contains no `messages` or `messages_updates` items.")
    return(tibble::tibble())
  }

  df |>
    dplyr::mutate(
      body_before        = stringr::str_trim(body_before %||% NA_character_),
      body_after         = stringr::str_trim(body_after  %||% NA_character_),
      body               = stringr::str_trim(body        %||% NA_character_),
      edited_body        = stringr::str_trim(edited_body %||% NA_character_),
      reply_title        = stringr::str_trim(reply_title %||% NA_character_),
      reply_description  = stringr::str_trim(reply_description %||% NA_character_)
    ) |>
    dplyr::relocate(channel_id, event_type, event_action, change,
                    id, chat_id, from, from_name, from_me, msg_type)
}


#' Parse a text message into command and arguments (Whapi helper)
#'
#' @description
#' Splits an incoming text (usually from a WhatsApp message) into a **command**
#' (the first token, starting with `/`) and its associated **arguments**.
#' Supports collapsing arguments into a single string or preserving them as
#' a character vector.
#'
#' @param text A character string containing the full message (e.g. `"/groe ajuda"`).
#' @param collapse_args Logical, default `TRUE`. If `TRUE`, all arguments are
#'   collapsed into a single space-separated string. If `FALSE`, arguments are
#'   returned as a character vector of tokens.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`command`}{The command string (first token), trimmed (e.g. `"/groe"`).}
#'   \item{`arguments`}{The arguments, either collapsed as a single string
#'   (default) or as a character vector, depending on `collapse_args`.}
#' }
#'
#' @examples
#' whapi_parse_command("/groe ajuda")
#' #> $command
#' #> [1] "/groe"
#' #>
#' #> $arguments
#' #> [1] "ajuda"
#'
#' whapi_parse_command("/groe nome empresa", collapse_args = FALSE)
#' #> $command
#' #> [1] "/groe"
#' #>
#' #> $arguments
#' #> [1] "nome" "empresa"
#'
#' @seealso [stringr::str_extract()], [stringr::str_split()]
#' @export
whapi_parse_command <- function(text, collapse_args = TRUE) {
  parts <- stringr::str_extract(text, "/.*") |>
    stringr::str_split("\\s+") |>
    unlist()

  if (collapse_args)
    arguments <- paste0(stringr::str_trim(parts[-1]), collapse = " ")
  else
    arguments <- stringr::str_trim(parts[-1])

  list(
    command   = stringr::str_trim(parts[1]),
    arguments = arguments
  )
}
