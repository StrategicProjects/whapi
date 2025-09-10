`%||%` <- function(x, y) if (is.null(x)) y else x

#' Normalize WhatsApp contact IDs (phone numbers only)
#'
#' @description
#' Cleans and normalizes WhatsApp phone numbers by:
#' - Skipping normalization if the entry already contains a JID suffix
#'   (e.g., "@s.whatsapp.net" or "@g.us");
#' - Otherwise, removing all non-digit characters (`+`, spaces, parentheses, hyphens, etc.);
#' - Removing the **ninth digit** (when present) right after the country code
#'   (CC) and area code (SS), in order to standardize to 12 or 13 digits.
#'
#' @details
#' This function is primarily designed for **Brazilian E.164 numbers** (e.g.,
#' `+55 (81) 9XXXX-YYYY`).
#'
#' @param to Character. A character vector of WhatsApp numbers in free
#'   text format or JIDs.
#'
#' @return A character vector of normalized IDs (phones -> digits only, JIDs kept as-is).
#'
#' @export
whapi_normalize_to <- function(to) {
  stopifnot(is.character(to))

  # If element looks like a JID, keep as-is
  is_jid <- grepl("@s\\.whatsapp\\.net$|@g\\.us$", to, ignore.case = TRUE)
  new_to <- to

  if (any(!is_jid)) {
    new_to[!is_jid] <- stringr::str_replace(
      stringr::str_remove_all(new_to[!is_jid], "\\D"),
      "(\\d{2})(\\d{2})(\\d{1})(\\d{8})",
      "\\1\\2\\4"
    )

    d <- stringr::str_length(new_to[!is_jid])
    test <- d < 12 | d > 13
    if (any(test)) {
      cli::cli_abort(c(
        "Valid numbers must have 13 or 12 digits, e.g.: +CC (SS) ZXXXX-YYYY",
        "i" = "There are {sum(test)} element(s) with the wrong number of digits: {which(test, arr.ind = TRUE)}.",
        "x" = "{sum(test)} numbers cannot be normalized."
      ))
    }
  }

  new_to
}

#' Convert numeric timestamp to POSIXct (UTC)
#'
#' @description
#' Utility function to convert a Unix timestamp (seconds since 1970-01-01 UTC)
#' into a `POSIXct` object.
#' Uses [lubridate::as_datetime()] for readability and consistency with the
#' tidyverse ecosystem.
#'
#' @param x A numeric or character vector representing a Unix timestamp
#'   (seconds since epoch). Can be `NULL` or `NA`.
#'
#' @return
#' A `POSIXct` object (in UTC) or `NA` if `x` is `NULL` or `NA`.
#'
#' @examples
#' # Single timestamp
#' whapi_to_posixct(1756426418)
#'
#' # Vector of timestamps (with NA)
#' whapi_to_posixct(c(1756426418, NA))
#'
#' # Character input
#' whapi_to_posixct("1756426418")
#'
#' @seealso [lubridate::as_datetime()]
#'
#' @importFrom lubridate as_datetime
#' @export
whapi_to_posixct <- function(x) {
  if (is.null(x)) return(lubridate::as_datetime(NA_real_, tz = "UTC"))
  nx <- suppressWarnings(as.numeric(x))
  lubridate::as_datetime(nx, tz = "UTC")
}

#' Extract common fields from Whapi API responses
#'
#' @description
#' Helper function to standardize parsing of Whapi message responses.
#' Whapi responses typically return a JSON object with a top-level element
#' `sent` and a nested `message` object containing details such as `id`,
#' `status`, `timestamp`, etc.
#' This function consolidates those fields into a flat tibble, making it easier
#' to work with message metadata in R.
#'
#' @details
#' The function safely looks up fields in multiple possible locations, since
#' Whapi responses are not always consistent across endpoints:
#' - **id**: prefers `out$message$id`, then `out$id`, then `out$message_id`;
#' - **status**: `out$message$status` or `out$status`;
#' - **timestamp**: `out$message$timestamp` or `out$timestamp`;
#' - **chat_id**: from `out$message$chat_id`, `out$message$to`, or `fallback_to`;
#' - **type**: `out$message$type` or `out$type`;
#' - **sent**: top-level `out$sent` (boolean, TRUE if successfully sent).
#'
#' The timestamp is returned both raw (numeric, seconds since epoch) and as a
#' parsed `POSIXct` column (`timestamp_dt`, UTC).
#'
#' @param out A list (parsed JSON) as returned by [httr2::resp_body_json()]
#'   from a Whapi request.
#' @param fallback_to Character(1). A fallback chat id (usually the `to` argument
#'   originally passed to the API) used when the response does not contain an
#'   explicit `chat_id` or `to`.
#'
#' @return
#' A tibble with one row and the following columns:
#' - `id`: message id;
#' - `to`: recipient chat id (phone or group);
#' - `status`: sending status (e.g., "pending", "sent");
#' - `timestamp`: numeric epoch timestamp (seconds);
#' - `timestamp_dt`: `POSIXct` parsed timestamp in UTC;
#' - `type`: message type (e.g., "text", "image", "location");
#' - `sent`: logical/boolean (TRUE if sent flag present);
#' - `resp`: the full raw response list for inspection.
#'
#' @examples
#' # Suppose `resp` is the parsed JSON returned from Whapi:
#'  out <- list(
#'    sent = TRUE,
#'    message = list(
#'      id = "abc123",
#'      chat_id = "558199999999@s.whatsapp.net",
#'      timestamp = 1756426418,
#'      type = "location",
#'      status = "pending"
#'    )
#'  )
#'
#'  whapi_extract_common_fields(out, fallback_to = "558199999999")
#'
#' @seealso
#' Used internally in wrappers like [whapi_send_text()],
#' [whapi_send_image()], [whapi_send_document()],
#' [whapi_send_location()].
#' @export
whapi_extract_common_fields <- function(out, fallback_to) {
  msg <- out$message %||% list()

  id        <- msg$id        %||% out$id        %||% out$message_id %||% NA_character_
  status    <- msg$status    %||% out$status    %||% NA_character_
  ts        <- msg$timestamp %||% out$timestamp %||% NA_real_
  chat_id   <- msg$chat_id   %||% msg$to        %||% fallback_to
  mtype     <- msg$type      %||% out$type      %||% NA_character_
  sent      <- out$sent      %||% NA

  tibble::tibble(
    id          = id,
    to          = chat_id,
    status      = status,
    timestamp   = ts,
    timestamp_dt= whapi_to_posixct(ts),
    type        = mtype,
    sent        = sent,
    resp        = list(out)
  )
}

#' Perform an HTTP request to Whapi.Cloud
#'
#' @description
#' Generic helper wrapping httr2 to call Whapi endpoints.
#' Supports methods: `"GET"`, `"POST"`, `"PUT"`.
#' Handles JSON encoding, retries, errors, and CLI logging.
#'
#' @param endpoint Character(1). Endpoint path (e.g. `"messages/text"`).
#'   Full URL is constructed as `"https://gate.whapi.cloud/{endpoint}"`.
#' @param payload List. Request body (for POST/PUT) or query (for GET). Default: `NULL`.
#' @param token Character(1). Bearer token. Defaults to env var `WHAPI_TOKEN`.
#' @param timeout Numeric. Timeout in seconds. Default `30`.
#' @param verbose Logical. Print progress via cli? Default `TRUE`.
#' @param method Character(1). HTTP method (`"GET"`, `"POST"`, `"PUT"`). Default `"POST"`.
#'
#' @return Parsed JSON response as a list.
#'
#' @examples
#' # out <- whapi_perform_request("messages/text", list(to="5581...", body="Hi"), method="POST")
#' # out <- whapi_perform_request("messages/12345", list(status="read"), method="PUT")
#'
#' @importFrom httr2 request req_method req_headers req_body_json req_url_query
#'   req_timeout req_retry req_error req_perform resp_body_json
#' @importFrom cli cli_inform cli_abort
#' @export
whapi_perform_request <- function(
    endpoint,
    payload = NULL,
    token   = Sys.getenv("WHAPI_TOKEN", unset = ""),
    timeout = 30,
    verbose = TRUE,
    method  = c("POST","GET","PUT")
) {
  if (!requireNamespace("httr2", quietly = TRUE)) stop("Package 'httr2' is required.")
  if (!requireNamespace("cli",   quietly = TRUE)) stop("Package 'cli' is required.")

  method <- match.arg(toupper(method), c("POST","GET","PUT"))

  if (!nzchar(token)) cli::cli_abort("Token not found. Provide `token` or set WHAPI_TOKEN.")
  stopifnot(is.character(endpoint), length(endpoint) == 1L, nzchar(endpoint))

  url <- paste0("https://gate.whapi.cloud/", endpoint)

  if (isTRUE(verbose)) {
    cli::cli_inform(c(
      "i" = paste("Calling Whapi endpoint:", url),
      ">" = paste("Method:", method)
    ))
  }

  req <- httr2::request(url) |>
    httr2::req_method(method) |>
    httr2::req_headers(
      Authorization = paste("Bearer", token),
      `Content-Type` = "application/json",
      Accept = "application/json"
    )

  if (!is.null(payload)) {
    if (method == "GET") {
      req <- req |> httr2::req_url_query(!!!payload)
    } else {
      req <- req |> httr2::req_body_json(payload, auto_unbox = TRUE)
    }
  }

  req <- req |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(max_tries = 3,
                     backoff = ~ httr2::req_retry(.x, max_tries = 3)) |>
    httr2::req_error(is_error = ~ .x$status_code >= 400)

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

#' Send a text message via Whapi.Cloud
#'
#' @param to Character. WhatsApp number in full international format WITHOUT "+"
#'   (digits only, e.g. "5581999999999"), or an existing group/channel id.
#' @param body Character. Text of the message (UTF-8).
#' @param token Character. Whapi Bearer token. By default, taken from the
#'   environment variable WHAPI_TOKEN if not provided.
#' @param quoted,edit Character (optional). Message IDs to quote or edit.
#' @param typing_time Numeric (optional). Seconds to simulate typing.
#' @param no_link_preview Logical (optional). TRUE to disable link preview.
#' @param wide_link_preview Logical (optional). TRUE to enable wide preview for links.
#' @param mentions Character vector (optional). Numbers to mention (without "+").
#'   Remember to include @<number> inside the message body as well.
#' @param view_once Logical (optional). TRUE to mark message as "view once".
#' @param timeout Numeric. Request timeout in seconds. Default: 30.
#' @param verbose Logical. Print messages via cli? Default: TRUE.
#'
#' @return A tibble with essential information (id, to, status, timestamp) and
#'   the full API response in column `resp` (as list).
#'
#' @examples
#' \dontrun{
#' # Make sure you set WHAPI_TOKEN in your environment or pass via argument
#'  Sys.setenv(WHAPI_TOKEN = "your_token_here")
#'
#' # Simple example:
#'  whapi_send_text("5581999999999", "Hello! Test message via API")
#'
#' # With extra options:
#'  whapi_send_text(
#'    to = "5581999999999",
#'    body = "Hello, @5581999999999 Ola",
#'    mentions = c("5581999999999"),
#'    typing_time = 2, no_link_preview = TRUE
#'  )
#'  }
#' @export
whapi_send_text <- function(
    to,
    body,
    token = Sys.getenv("WHAPI_TOKEN", unset = ""),
    quoted = NULL,
    edit = NULL,
    typing_time = NULL,
    no_link_preview = NULL,
    wide_link_preview = NULL,
    mentions = NULL,
    view_once = NULL,
    timeout = 30,
    verbose = TRUE
) {
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")
  if (!requireNamespace("cli", quietly = TRUE)) stop("Package 'cli' is required.")

  stopifnot(is.character(to), length(to) == 1L, nzchar(to))
  stopifnot(is.character(body), length(body) == 1L, nzchar(body))

  if (!nzchar(token)) {
    stop("Token not found. Provide `token` or set env var WHAPI_TOKEN.")
  }

  to_clean <- whapi_normalize_to(to)

  body_list <- list(
    to = to_clean,
    body = body,
    quoted = quoted,
    edit = edit,
    typing_time = typing_time,
    no_link_preview = no_link_preview,
    wide_link_preview = wide_link_preview,
    mentions = if (is.null(mentions)) NULL else as.list(whapi_normalize_to(as.character(mentions))),
    view_once = view_once
  )
  body_list <- body_list[!vapply(body_list, is.null, logical(1))]

  if (verbose) {
    cli::cli_inform(c(
      "i" = "Sending text message via Whapi.Cloud...",
      ">" = paste0("To: ", to_clean,
                   " | Body: ", substr(body, 1, min(nchar(body), 80)),
                   if (nchar(body) > 80) "...")
    ))
  }

  # Use the generic request function
  out <- whapi_perform_request(
    endpoint = "messages/text",
    payload  = body_list,
    token    = token,
    timeout  = timeout,
    verbose  = verbose,
    method = "POST"
  )

  whapi_extract_common_fields(out, fallback_to = to_clean)
}


#' Get a WhatsApp message by ID (Whapi.Cloud)
#'
#' @param message_id Character(1). The message ID to fetch.
#' @param resync Logical(1). Whether to resync from the device. Default FALSE.
#' @param token Bearer token (default from WHAPI_TOKEN env var).
#' @param timeout Timeout in seconds. Default 30.
#' @param verbose Print CLI progress? Default TRUE.
#'
#' @return Tibble with fields: id, type, subtype, chat_id, chat_name,
#'   from, from_name, from_me, source, timestamp, timestamp_dt,
#'   device_id, status, and raw `resp`.
#'
#' @examples
#' # whapi_get_message("PsobWy36679._7w-wKmB9tMeGQ")
#' @export
whapi_get_message <- function(
    message_id,
    resync  = FALSE,
    token   = Sys.getenv("WHAPI_TOKEN", unset = ""),
    timeout = 30,
    verbose = TRUE
) {
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")
  if (!requireNamespace("cli",    quietly = TRUE)) stop("Package 'cli' is required.")

  stopifnot(is.character(message_id), length(message_id) == 1L, nzchar(message_id))

  endpoint <- sprintf("messages/%s", message_id)

  out <- whapi_perform_request(
    endpoint = endpoint,
    payload  = list(resync = tolower(as.character(isTRUE(resync)))),
    token    = token,
    timeout  = timeout,
    verbose  = verbose,
    method   = "GET"
  )

  # Whapi may return message in `out$message` or at root
  msg <- out$message %||% out

  tibble::tibble(
    id           = msg$id         %||% NA_character_,
    type         = msg$type       %||% NA_character_,
    subtype      = msg$subtype    %||% NA_character_,
    chat_id      = msg$chat_id    %||% NA_character_,
    chat_name    = msg$chat_name  %||% NA_character_,
    from         = msg$from       %||% NA_character_,
    from_name    = msg$from_name  %||% NA_character_,
    from_me      = msg$from_me    %||% NA,
    source       = msg$source     %||% NA_character_,
    timestamp    = msg$timestamp  %||% NA_real_,
    timestamp_dt = whapi_to_posixct(msg$timestamp %||% NA_real_),
    device_id    = msg$device_id  %||% NA_real_,
    status       = msg$status     %||% NA_character_,
    resp         = list(out)
  )
}


#' whapi_slugify strings for safe IDs (e.g., button IDs in Whapi)
#'
#' @description
#' Converts free-text labels into a safe "slug" format suitable for use as
#' message button IDs or other identifiers in Whapi API requests.
#' Ensures that IDs contain only lowercase letters, digits, and underscores,
#' and are never empty (defaults to `"btn"` if the input is blank).
#'
#' @details
#' This function is particularly useful when creating interactive messages
#' (buttons, lists) in WhatsApp via Whapi, where each button requires a valid
#' `id`. By whapi_slugifying titles automatically, we can safely generate IDs even if
#' users provide arbitrary labels with spaces, accents, or symbols.
#'
#' Transformation steps:
#' 1. Convert to lowercase;
#' 2. Replace any sequence of non-alphanumeric characters with `_`;
#' 3. Trim leading/trailing underscores;
#' 4. Replace empty results with `"btn"`.
#'
#' @param x A character vector of labels.
#'
#' @return A character vector of the same length with slugified IDs.
#'
#' @examples
#' whapi_slugify(c("Yes!", "Call Us", "Sale!", "###"))
#' # -> "yes", "call_us", "promocao_rapida", "btn"
#'
#' # Use case in button creation:
#' titles <- c("Buy Now", "Learn More")
#' ids <- whapi_slugify(titles)
#' tibble::tibble(title = titles, id = ids)
#'
#' @seealso Used internally in `whapi_send_quick_reply()` and other
#' interactive message helpers.
#'
#' @importFrom stringr str_to_lower str_replace_all str_replace str_trim str_length
#' @export
whapi_slugify <- function(x) {
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "_")
  x <- stringr::str_replace(x, "^_+|_+$", "")
  x[stringr::str_length(x) == 0] <- "btn"
  x
}


#' Make identifiers unique while preserving order
#'
#' @description
#' Ensures that a character vector of identifiers is unique by appending
#' numeric suffixes (`_2`, `_3`, ...) when duplicates are found, while
#' preserving the original order.
#'
#' @details
#' This helper is particularly useful when generating button IDs for
#' WhatsApp interactive messages via Whapi. Even after whapi_slugifying labels,
#' duplicates may remain (e.g., two buttons both titled `"Yes"`).
#' The function guarantees uniqueness by incrementally appending a suffix.
#'
#' Algorithm:
#' - Iterates through `x` in order;
#' - Keeps a counter of how many times each ID has appeared;
#' - First occurrence is left unchanged;
#' - Subsequent duplicates get suffixed with `_<n>`.
#'
#' @param x A character vector of IDs (possibly with duplicates).
#'
#' @return A character vector of the same length with unique IDs.
#'
#' @examples
#' whapi_make_unique(c("yes", "no", "yes", "yes", "maybe", "no"))
#' # -> "yes", "no", "yes_2", "yes_3", "maybe", "no_2"
#'
#' # Combined with whapi_slugify
#' titles <- c("Yes!", "Yes!", "No?")
#' ids <- whapi_make_unique(whapi_slugify(titles))
#' tibble::tibble(title = titles, id = ids)
#'
#' @seealso [whapi_slugify()] for slug-safe ID creation.
#'
#' @importFrom purrr map_chr
#' @export
whapi_make_unique <- function(x) {
  seen <- list()
  purrr::map_chr(x, function(s) {
    n <- seen[[s]] %||% 0L
    seen[[s]] <<- n + 1L
    if (n == 0L) {
      s
    } else {
      paste0(s, "_", n + 1L)
    }
  })
}

#' Build common message blocks for Whapi interactive messages
#'
#' @description
#' Internal helper that constructs the standard structure shared by Whapi
#' interactive messages (`button`, `list`, `mixed actions`, etc.).
#'
#' It automatically normalizes the recipient (`to`) using
#' [whapi_normalize_to()], and creates `header`, `body`, and `footer` blocks
#' only if the corresponding text is provided.
#'
#' @details
#' Many Whapi interactive endpoints (e.g., `messages/interactive`) require
#' the same basic structure:
#' - **to**: target number or chat id;
#' - **header**: optional text shown above the body;
#' - **body**: main message text (required);
#' - **footer**: optional small text shown below the body.
#'
#' This helper ensures consistency and avoids repeating boilerplate code
#' when building different interactive message payloads.
#'
#' @param to Character(1). Recipient phone number in international format
#'   (digits only, no `+`), or a group/channel id.
#' @param body_text Character(1). Main text of the interactive message body.
#' @param header_text Character(1), optional. Optional header text.
#' @param footer_text Character(1), optional. Optional footer text.
#'
#' @return
#' A named list ready to be merged into a Whapi interactive message payload,
#' containing elements: `to`, `header` (if provided), `body`, and `footer`
#' (if provided).
#'
#' @examples
#' \dontrun{
#' # Minimal body only
#'
#' whapi_common_blocks("5581999999999", body_text = "Choose an option below")
#'
#' # With header and footer
#' whapi_common_blocks(
#'   to = "5581999999999",
#'   body_text   = "Do you confirm?",
#'   header_text = "Booking Confirmation",
#'   footer_text = "Reply now"
#' )
#'}
#' @seealso [whapi_send_quick_reply()], [whapi_send_list()],
#' [whapi_send_mixed_actions()]
#' @export
whapi_common_blocks <- function(to, body_text, header_text = NULL, footer_text = NULL) {
  list(
    to     = whapi_normalize_to(to),
    header = if (!is.null(header_text)) list(text = header_text) else NULL,
    body   = list(text = body_text),
    footer = if (!is.null(footer_text)) list(text = footer_text) else NULL
  )
}


#' Coerce and normalize button specs for Whapi interactive messages
#'
#' @description
#' Internal helper that converts a `data.frame`/`tibble` or `list` of button
#' definitions into a normalized **list-of-lists**, applying a few rules:
#' - Accepts aliases `label` / `name` and maps them to `title`;
#' - Requires a non-empty `title`;
#' - Auto-generates `id` when missing using slugification plus uniqueness
#'   enforcement (e.g., `"Buy Now" -> "buy_now"`, duplicates become
#'   `"buy_now_2"`, `"buy_now_3"`, ...).
#'
#' This is useful before building payloads for Whapi interactive endpoints
#' (e.g., buttons, mixed actions, etc.).
#'
#' @param buttons A `data.frame`/`tibble` with columns per button (e.g.
#'   `title`, `id`, etc.) **or** a list of named lists. Can be `NULL`
#'   (returns empty list).
#' @param verbose Logical (default `TRUE`). If `TRUE`, prints progress messages
#'   via \pkg{cli} (how many buttons, how many ids auto-generated, etc.).
#'
#' @return A **list of named lists** (one per button), each guaranteed to have
#'   at least `title` (non-empty). If a button had no `id`, a slugified,
#'   unique `id` is created.
#'
#' @examples
#' # From tibble (title only -> ids auto-generated)
#' # tibble::tribble(~title, "Buy Now", "Buy Now", "Learn More") |>
#' #   whapi_coerce_buttons_base()
#'
#' # From list (mix with/without id)
#' # whapi_coerce_buttons_base(list(
#' #   list(title = "Website", url = "https://example.com"),
#' #   list(title = "Website")  # will get an auto id too
#' # ))
#'
#' @seealso `whapi_slugify()`, `whapi_make_unique()`
#'
#' @importFrom purrr transpose map map_lgl map_chr imap
#' @importFrom cli cli_inform cli_abort
#' @importFrom stringr str_trim str_length
#' @export
whapi_coerce_buttons_base <- function(buttons, verbose = TRUE) {

  if (is.null(buttons)) {
    if (isTRUE(verbose)) cli::cli_inform(c("i" = "No buttons provided (NULL)."))
    return(list())
  }

  # Accept df/tibble or list
  if (is.data.frame(buttons)) {
    btns <- buttons |>
      purrr::transpose() |>
      purrr::map(~ purrr::map(.x, ~ if (length(.x)) .x else NULL))
  } else if (is.list(buttons)) {
    btns <- purrr::map(buttons, as.list)
  } else {
    cli::cli_abort("`buttons` must be a data.frame/tibble or a list of button lists.")
  }

  if (isTRUE(verbose)) {
    cli::cli_inform(c("i" = paste0("Coercing ", length(btns), " button(s)...")))
  }

  # Map aliases label/name -> title
  btns <- purrr::imap(btns, ~ {
    if (is.null(.x$title)) {
      .x$title <- .x$label %||% .x$name
      if (isTRUE(verbose) && !is.null(.x$title)) {
        cli::cli_inform(c(">" = paste0("Button ", .y, ": mapped alias (label/name) to `title`.")))
      }
    }
    # trim title if present
    if (!is.null(.x$title)) .x$title <- stringr::str_trim(.x$title)
    .x
  })

  # Require non-empty title
  has_title <- purrr::map_lgl(btns, ~ !is.null(.x$title) && stringr::str_length(.x$title) > 0)
  if (!all(has_title)) {
    bad_idx <- which(!has_title)
    cli::cli_abort(c(
      "Every button must have a non-empty `title` (or alias `label`/`name`).",
      "x" = paste("Missing title at index(es):", paste(bad_idx, collapse = ", "))
    ))
  }

  # Auto-generate id if missing: whapi_slugify titles and enforce uniqueness
  titles   <- purrr::map_chr(btns, ~ .x$title %||% "")
  slug_ids <- whapi_slugify(titles)
  uniq_ids <- whapi_make_unique(slug_ids)

  missing_id_idx <- integer(0)
  btns <- purrr::imap(btns, ~ {
    if (is.null(.x$id)) {
      .x$id <- uniq_ids[as.integer(.y)]
      missing_id_idx <<- c(missing_id_idx, as.integer(.y))
    }
    .x
  })

  # Logs
  if (isTRUE(verbose)) {
    if (length(missing_id_idx)) {
      cli::cli_inform(c("v" = paste0("Auto-generated `id` for ",
                                     length(missing_id_idx), " button(s): ",
                                     paste(missing_id_idx, collapse = ", "))))
    } else {
      cli::cli_inform(c("v" = "All buttons already had `id`."))
    }
  }

  btns
}


#' Coerce and validate QUICK REPLY buttons for Whapi
#'
#' @description
#' Internal helper that prepares **quick reply** buttons for Whapi interactive
#' messages. It relies on [whapi_coerce_buttons_base()] to normalize input
#' (accept `data.frame`/`list`, map aliases `label`/`name` -> `title`,
#' auto-generate `id` via slug + uniqueness) and then:
#' - Enforces `type = "quick_reply"` for all buttons;
#' - Requires the fields `title` and `id`;
#' - Ensures the WhatsApp constraint of **1 to 3** buttons.
#'
#' @param buttons A `data.frame`/`tibble` (one row per button) or a list of
#'   named lists. `title` is required (or via alias), `id` will be auto-created
#'   when missing by [whapi_coerce_buttons_base()].
#' @param verbose Logical (default `TRUE`). If `TRUE`, prints progress messages
#'   via \pkg{cli}.
#'
#' @return A list-of-lists of buttons, each including at least `title`, `id`,
#'   and `type = "quick_reply"`.
#'
#' @examples
#' # tibble::tribble(~title, "YES", "NO") |>
#' #   whapi_coerce_buttons_base() |>
#' #   whapi_coerce_buttons_quick()
#'
#' @seealso [whapi_coerce_buttons_base()] for normalization; other coercers for
#'   mixed buttons (url/call/copy).
#'
#' @importFrom purrr map
#' @importFrom cli cli_inform cli_abort
#' @export
whapi_coerce_buttons_quick <- function(buttons, verbose = TRUE) {
  # normalize base (title/id mapping and autogen)
  btns <- whapi_coerce_buttons_base(buttons, verbose = verbose)

  n <- length(btns)
  if (n < 1) {
    cli::cli_abort("Provide at least one quick reply button.")
  }
  if (n > 3) {
    cli::cli_abort("WhatsApp allows a maximum of 3 quick reply buttons.")
  }
  if (isTRUE(verbose)) {
    cli::cli_inform(c("i" = paste0("Validating ", n, " quick reply button(s)...")))
  }

  # enforce type and required fields
  required <- c("title", "id")
  btns <- purrr::map(btns, ~ {
    .x$type <- "quick_reply"
    missing <- setdiff(required, names(.x))
    if (length(missing)) {
      cli::cli_abort(
        paste0("Missing fields in a quick reply button: ",
               paste(missing, collapse = ", "))
      )
    }
    .x
  })

  if (isTRUE(verbose)) {
    cli::cli_inform(c("v" = "Quick reply buttons ready."))
  }
  btns
}


#' Coerce and validate MIXED buttons (url / call / copy) for Whapi
#'
#' @description
#' Internal helper that prepares **mixed action** buttons for Whapi interactive
#' messages. It first normalizes input via [whapi_coerce_buttons_base()] (accepts
#' `data.frame`/`tibble` or `list`, maps aliases to `title`, auto-creates `id`
#' with slug + uniqueness) and then validates each button according to its
#' declared `type`:
#' - `url`  -> requires fields: `title`, `id`, `url`
#' - `call` -> requires fields: `title`, `id`, `phone_number`
#' - `copy` -> requires fields: `title`, `id`, `copy_code`
#'
#' Enforces WhatsApp constraints: **1 to 3** buttons per message.
#'
#' @param buttons A `data.frame`/`tibble` (one row per button) or a list of
#'   named lists. `title` is required (or via alias), `id` is auto-generated
#'   when missing by [whapi_coerce_buttons_base()]. Each button **must provide**
#'   a valid `type` among `{"url","call","copy"}`.
#' @param verbose Logical (default `TRUE`). If `TRUE`, prints progress messages
#'   via \pkg{cli}.
#'
#' @return
#' A list-of-lists of buttons, each validated to contain the fields required
#' for its `type`.
#'
#' @examples
#' # Example with a tibble:
#' # tibble::tribble(
#' #   ~title,        ~type, ~url,
#' #   "Website",     "url", "https://example.com",
#' #   "Call Support","call", NA
#' # ) |>
#' #   whapi_coerce_buttons_base() |>
#' #   whapi_coerce_buttons_mixed()
#'
#' # Example with a list:
#' # whapi_coerce_buttons_mixed(list(
#' #   list(type="url",  title="Website", url="https://example.com"),
#' #   list(type="call", title="Call us", phone_number="5581999999999"),
#' #   list(type="copy", title="Copy OTP", copy_code="123456")
#' # ))
#'
#' @seealso [whapi_coerce_buttons_base()] for normalization;
#'   [whapi_coerce_buttons_quick()] for quick-reply buttons.
#'
#' @importFrom purrr map
#' @importFrom cli cli_inform cli_abort
#' @export
whapi_coerce_buttons_mixed <- function(buttons, verbose = TRUE) {
  # Normalize base (title/id mapping and autogen)
  btns <- whapi_coerce_buttons_base(buttons, verbose = verbose)

  n <- length(btns)
  if (n < 1) {
    cli::cli_abort("Provide at least one button.")
  }
  if (n > 3) {
    cli::cli_abort("WhatsApp allows a maximum of 3 buttons.")
  }
  if (isTRUE(verbose)) {
    cli::cli_inform(c("i" = paste0("Validating ", n, " mixed button(s)...")))
  }

  valid_types <- c("url", "call", "copy")

  # Validate each button by its type and required fields
  btns <- purrr::map(btns, ~ {
    if (is.null(.x$type) || !(.x$type %in% valid_types)) {
      cli::cli_abort("Each button must have a valid `type` in {'url','call','copy'}.")
    }
    required <- switch(.x$type,
                       url  = c("title","id","url"),
                       call = c("title","id","phone_number"),
                       copy = c("title","id","copy_code")
    )
    missing <- setdiff(required, names(.x))
    if (length(missing)) {
      cli::cli_abort(
        paste0("Missing fields in a '", .x$type, "' button: ",
               paste(missing, collapse = ", "))
      )
    }
    .x
  })

  if (isTRUE(verbose)) {
    # Small breakdown by type for logging
    counts <- table(vapply(btns, `[[`, "", "type"))
    cli::cli_inform(c("v" = paste0(
      "Buttons OK (",
      paste(paste0(names(counts), "=", as.integer(counts)), collapse = ", "),
      ")."
    )))
  }

  btns
}

#' Validate `list_sections` for Whapi LIST interactive messages
#'
#' @description
#' Internal helper that validates the structure of `list_sections` used in
#' Whapi **LIST** messages. Each section must provide a non-empty `title`
#' and a non-empty `rows` list. Each row must provide non-empty `id` and
#' `title`. The `description` field is optional.
#'
#' @details
#' Expected structure:
#' ```r
#' list_sections <- list(
#'   list(
#'     title = "Section title",
#'     rows = list(
#'       list(id = "r1", title = "Row title", description = "Optional"),
#'       ...
#'     )
#'   ),
#'   ...
#' )
#' ```
#' This function performs **lightweight validation** and (optionally) trims
#' whitespace from titles and ids to avoid subtle formatting issues.
#'
#' @param list_sections A list of sections; each section is a named list with
#'   `title` (character) and `rows` (list of row objects).
#' @param verbose Logical (default `TRUE`). If `TRUE`, prints progress messages
#'   via \pkg{cli}.
#' @param trim Logical (default `TRUE`). If `TRUE`, trims whitespace from
#'   `section$title`, `row$title`, and `row$id` before validating.
#'
#' @return
#' The (possibly trimmed) `list_sections` object, invisibly unchanged in shape.
#'
#' @examples
#' sections <- list(
#'   list(
#'     title = "Burgers",
#'     rows = list(
#'       list(id = "r1", title = "Plain",  description = "No cheese, no sauce"),
#'       list(id = "r2", title = "Cheese", description = "With melted cheese")
#'     )
#'   )
#' )
#' whapi_validate_list_sections(sections)
#'
#' @seealso
#' Helpers for interactive payloads such as `whapi_coerce_buttons_base()`,
#' `whapi_coerce_buttons_quick()`, and `whapi_coerce_buttons_mixed()`.
#'
#' @importFrom purrr iwalk walk
#' @importFrom stringr str_trim str_length
#' @importFrom cli cli_inform cli_abort
#' @export
whapi_validate_list_sections <- function(list_sections, verbose = TRUE, trim = TRUE) {
  # quick shape checks
  if (is.null(list_sections) || !is.list(list_sections) || length(list_sections) < 1) {
    cli::cli_abort("For a LIST message, provide `list_sections` as a non-empty list of sections.")
  }

  if (isTRUE(verbose)) {
    cli::cli_inform(c("i" = paste0("Validating ", length(list_sections), " section(s) for LIST message...")))
  }

  # validate each section and its rows
  purrr::iwalk(list_sections, function(sec, si) {
    # optional trimming
    if (isTRUE(trim) && !is.null(sec$title)) {
      sec$title <- stringr::str_trim(sec$title)
      list_sections[[si]]$title <- sec$title
    }

    if (is.null(sec$title) || stringr::str_length(sec$title) == 0) {
      cli::cli_abort(paste0("Each section must have a non-empty `title` (section index ", si, ")."))
    }
    if (is.null(sec$rows) || !length(sec$rows)) {
      cli::cli_abort(paste0("Each section must have `rows` (list). Problem at section index ", si, "."))
    }

    purrr::iwalk(sec$rows, function(rw, ri) {
      if (isTRUE(trim)) {
        if (!is.null(rw$id))    { rw$id    <- stringr::str_trim(rw$id)    ; list_sections[[si]]$rows[[ri]]$id    <- rw$id }
        if (!is.null(rw$title)) { rw$title <- stringr::str_trim(rw$title) ; list_sections[[si]]$rows[[ri]]$title <- rw$title }
      }

      if (is.null(rw$id) || stringr::str_length(rw$id) == 0) {
        cli::cli_abort(paste0("Each row must have a non-empty `id` (section ", si, ", row ", ri, ")."))
      }
      if (is.null(rw$title) || stringr::str_length(rw$title) == 0) {
        cli::cli_abort(paste0("Each row must have a non-empty `title` (section ", si, ", row ", ri, ")."))
      }
      # description is optional
    })
  })

  if (isTRUE(verbose)) {
    cli::cli_inform(c("v" = "LIST sections validated successfully."))
  }

  list_sections
}

# 1) QUICK REPLY ---------------------------------------------------------------
#' Send a WhatsApp interactive message with QUICK REPLY buttons (Whapi.Cloud)
#'
#' @description
#' Sends an interactive message of type **QUICK REPLY** via Whapi.
#' Each button is normalized/validated by [whapi_coerce_buttons_quick()] and
#' automatically gets a unique slugified `id` if missing.
#'
#' @param to Character (length 1). Phone in E.164 digits (without "+") or group id.
#' @param body_text Character. Main body text.
#' @param buttons Data frame or list. Up to 3 items; fields: `title`, `id` (id auto-generated if missing).
#' @param header_text,footer_text Character (optional). Header/footer texts.
#' @param token Bearer token. Defaults to env var WHAPI_TOKEN.
#' @param timeout Numeric, request timeout in seconds. Default 30.
#' @param verbose Logical, print CLI messages. Default TRUE.
#'
#' @return A tibble with fields `id`, `to`, `status`, `timestamp`, and raw response `resp`.
#'
#' @examples
#' \dontrun{
#' # Sys.setenv(WHAPI_TOKEN = "your_token_here")
#' # whapi_send_quick_reply(
#' #   to = "5581999999999",
#' #   body_text = "Do you confirm?",
#' #   buttons = tibble::tribble(~title, "YES", "NO")
#' # )
#'}
#' @seealso [whapi_coerce_buttons_quick()], [whapi_common_blocks()], [whapi_perform_request()]
#' @export
whapi_send_quick_reply <- function(
    to,
    body_text,
    buttons,
    header_text = NULL,
    footer_text = NULL,
    token = Sys.getenv("WHAPI_TOKEN", unset = ""),
    timeout = 30,
    verbose = TRUE
) {
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")
  if (!requireNamespace("cli", quietly = TRUE)) stop("Package 'cli' is required.")

  # normalize and build payload
  blocks <- whapi_common_blocks(whapi_normalize_to(to), body_text, header_text, footer_text)
  btns   <- whapi_coerce_buttons_quick(buttons, verbose = verbose)

  payload <- list(
    to     = blocks$to,
    header = blocks$header,
    body   = blocks$body,
    footer = blocks$footer,
    type   = "button",
    action = list(buttons = btns)
  )

  if (isTRUE(verbose)) {
    cli::cli_inform(c(
      "i" = "Sending QUICK REPLY interactive message via Whapi.Cloud...",
      ">" = paste0("To: ", blocks$to,
                   " | Buttons: ", length(btns),
                   " | Preview: ", substr(body_text, 1, min(nchar(body_text), 60)),
                   if (nchar(body_text) > 60) "...")
    ))
  }

  out <- whapi_perform_request(
    endpoint = "messages/interactive",
    payload  = payload,
    token    = token,
    timeout  = timeout,
    verbose  = verbose,
    method   = "POST"
  )

  result <- whapi_extract_common_fields(out, fallback_to = blocks$to)

  if (isTRUE(verbose)) {
    cli::cli_inform(c("v" = paste0("QUICK REPLY sent (id=", result$id, "). Status: ", result$status)))
  }

  result
}

# 2) MIXED ACTIONS (url/call/copy) --------------------------------------------
#' Send a WhatsApp interactive message mixing URL/CALL/COPY buttons (Whapi.Cloud)
#'
#' @description
#' Sends an interactive **buttons** message that mixes `url`, `call`, and/or
#' `copy` actions. Input buttons are normalized/validated by
#' [whapi_coerce_buttons_mixed()] (aliases mapped to `title`, auto `id` creation,
#' required fields per type).
#'
#' @param to Character(1). Phone in E.164 digits (without "+") or group id.
#' @param body_text Character(1). Main body text.
#' @param buttons Data frame or list. Up to 3 items; each must define a `type`
#'   in `{'url','call','copy'}` and include the fields:
#'   - `url`:  `title`, `id`, `url`
#'   - `call`: `title`, `id`, `phone_number`
#'   - `copy`: `title`, `id`, `copy_code`
#'   (`id` is auto-generated if missing; `title` required)
#' @param header_text,footer_text Character (optional). Header/footer texts.
#' @param token Bearer token. Defaults to env var `WHAPI_TOKEN`.
#' @param timeout Numeric. Request timeout (seconds). Default 30.
#' @param verbose Logical. Print CLI messages? Default TRUE.
#'
#' @return
#' A tibble with fields `id`, `to`, `status`, `timestamp`, and the raw response
#' in `resp`.
#'
#' @examples
#' \dontrun{
#'  Sys.setenv(WHAPI_TOKEN = "your_token_here")
#'  whapi_send_mixed_actions(
#'    to = "5581999999999",
#'    body_text = "Pick an option:",
#'    buttons = list(
#'      list(type="url",  title="Website",   url="https://example.com"),
#'      list(type="call", title="Call us",   phone_number="5581999999999"),
#'      list(type="copy", title="Copy OTP",  copy_code="123456")
#'    )
#'  )
#'}
#' @seealso [whapi_coerce_buttons_mixed()], [whapi_common_blocks()], [whapi_perform_request()]
#' @export
whapi_send_mixed_actions <- function(
    to,
    body_text,
    buttons,
    header_text = NULL,
    footer_text = NULL,
    token = Sys.getenv("WHAPI_TOKEN", unset = ""),
    timeout = 30,
    verbose = TRUE
) {
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")
  if (!requireNamespace("cli",    quietly = TRUE)) stop("Package 'cli' is required.")

  # Normalize and build blocks
  blocks <- whapi_common_blocks(whapi_normalize_to(to), body_text, header_text, footer_text)
  btns   <- whapi_coerce_buttons_mixed(buttons, verbose = verbose)

  # Build payload for interactive buttons
  payload <- list(
    to     = blocks$to,
    header = blocks$header,
    body   = blocks$body,
    footer = blocks$footer,
    type   = "button",
    action = list(buttons = btns)
  )

  # Logging
  if (isTRUE(verbose)) {
    # small summary by type
    types <- vapply(btns, `[[`, "", "type")
    counts <- if (length(types)) paste(names(table(types)), as.integer(table(types)), sep = "=", collapse = ", ") else "none"
    cli::cli_inform(c(
      "i" = "Sending MIXED-ACTIONS interactive message via Whapi.Cloud...",
      ">" = paste0("To: ", blocks$to,
                   " | Buttons: ", length(btns),
                   " (", counts, ")",
                   " | Preview: ", substr(body_text, 1, min(nchar(body_text), 60)),
                   if (nchar(body_text) > 60) "...")
    ))
  }

  # Perform request (POST /messages/interactive)
  out <- whapi_perform_request(
    endpoint = "messages/interactive",
    payload  = payload,
    token    = token,
    timeout  = timeout,
    verbose  = verbose,
    method   = "POST"
  )

  result <- whapi_extract_common_fields(out, fallback_to = blocks$to)

  if (isTRUE(verbose)) {
    cli::cli_inform(c("v" = paste0("MIXED-ACTIONS sent (id=", result$id, "). Status: ", result$status)))
  }

  result
}


# 3) LIST ----------------------------------------------------------------------
#' Send a WhatsApp interactive LIST message (Whapi.Cloud)
#'
#' @description
#' Sends an interactive **LIST** message via Whapi.
#' Sections/rows are validated by [whapi_validate_list_sections()]. The payload
#' reuses [whapi_common_blocks()] to keep structure consistent across interactive
#' message types.
#'
#' @param to Character(1). Phone in E.164 digits (without "+") or group id.
#' @param body_text Character(1). Main body text.
#' @param list_sections A list of sections. Each section is a named list:
#'   `list(title = <chr>, rows = list(list(id=<chr>, title=<chr>, description=<chr>?), ...))`.
#' @param list_label Character(1), optional. Button label that opens the list.
#'   Default: `"Choose..."`
#' @param header_text,footer_text Character(1), optional. Header/footer texts.
#' @param token Bearer token. Defaults to env var `WHAPI_TOKEN`.
#' @param timeout Numeric. Request timeout in seconds. Default 30.
#' @param verbose Logical. Print CLI messages? Default TRUE.
#'
#' @return
#' A tibble with `id`, `to`, `status`, `timestamp`, and the raw response in `resp`.
#'
#' @examples
#' \dontrun{
#'  Sys.setenv(WHAPI_TOKEN = "your_token_here")
#'  sections <- list(
#'    list(
#'      title = "Burgers",
#'      rows = list(
#'        list(id="b1", title="Plain",  description="No cheese, no sauce"),
#'        list(id="b2", title="Cheese", description="With melted cheese")
#'      )
#'    ),
#'    list(
#'      title = "Drinks",
#'      rows = list(
#'        list(id="d1", title="Water"),
#'        list(id="d2", title="Soda", description="Assorted flavors")
#'      )
#'    )
#'  )
#'  whapi_send_list(
#'    to = "5581999999999",
#'    body_text = "Choose your order:",
#'    list_sections = sections,
#'    list_label = "Open menu",
#'    header_text = "Our Menu",
#'    footer_text = "Thanks!"
#'  )
#'}
#' @seealso [whapi_validate_list_sections()], [whapi_common_blocks()],
#'   [whapi_perform_request()], [whapi_extract_common_fields()]
#' @export
whapi_send_list <- function(
    to,
    body_text,
    list_sections,
    list_label = "Choose...",
    header_text = NULL,
    footer_text = NULL,
    token = Sys.getenv("WHAPI_TOKEN", unset = ""),
    timeout = 30,
    verbose = TRUE
) {
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")
  if (!requireNamespace("cli",    quietly = TRUE)) stop("Package 'cli' is required.")

  blocks <- whapi_common_blocks(whapi_normalize_to(to), body_text, header_text, footer_text)
  secs   <- whapi_validate_list_sections(list_sections, verbose = verbose, trim = TRUE)

  payload <- list(
    to     = blocks$to,
    header = blocks$header,
    body   = blocks$body,
    footer = blocks$footer,
    type   = "list",
    action = list(list = list(sections = secs, label = if (is.null(list_label)) "Choose..." else list_label))
  )

  # Logs
  if (isTRUE(verbose)) {
    total_rows <- sum(vapply(secs, function(s) length(s$rows), integer(1)))
    cli::cli_inform(c(
      "i" = "Sending LIST interactive message via Whapi.Cloud...",
      ">" = paste0("To: ", blocks$to,
                   " | Sections: ", length(secs),
                   " | Rows: ", total_rows,
                   " | Label: ", list_label,
                   " | Preview: ", substr(body_text, 1, min(nchar(body_text), 60)),
                   if (nchar(body_text) > 60) "...")
    ))
  }

  # POST /messages/interactive
  out <- whapi_perform_request(
    endpoint = "messages/interactive",
    payload  = payload,
    token    = token,
    timeout  = timeout,
    verbose  = verbose,
    method   = "POST"
  )

  result <- whapi_extract_common_fields(out, fallback_to = blocks$to)

  if (isTRUE(verbose)) {
    cli::cli_inform(c("v" = paste0("LIST sent (id=", result$id, "). Status: ", result$status)))
  }

  result
}


#' Get WhatsApp contact profile(s) via Whapi.Cloud
#'
#' @description
#' Fetches profile information for one or more contacts using
#' `GET /contacts/{ContactID}/profile` and returns a tidy tibble.
#' This version assumes the response body contains:
#' - `name` (string, user name),
#' - `about` (string, user info in About section),
#' - `icon` (string, profile preview icon URL),
#' - `icon_full` (string, full avatar URL).
#'
#' @details
#' - Each `contacts` element may be a phone number (free text) or a JID
#'   (e.g., `"1203...@g.us"`). Phone numbers are normalized via
#'   [whapi_normalize_to()] (12 digits total); JIDs are kept as-is.
#' - When `full = TRUE`, `_full=true` is added to the querystring to request
#'   higher-resolution avatars (if supported).
#'
#' @param contacts Character vector. Phones in E.164 digits (no "+") or chat IDs.
#' @param token Bearer token. Defaults to env var `WHAPI_TOKEN`.
#' @param full Logical. If `TRUE`, add `_full=true` query param. Default `TRUE`.
#' @param timeout Numeric. Request timeout (seconds). Default `30`.
#' @param verbose Logical. Print progress with \pkg{cli}? Default `TRUE`.
#'
#' @return
#' A tibble with one row per contact:
#' - `contact_id` (input id or JID as queried),
#' - `name`, `about`, `icon`, `icon_full`,
#' - `resp` with the raw response (list).
#'
#' @examples
#' \dontrun{
#' # Sys.setenv(WHAPI_TOKEN = "your_token_here")
#' # Single:
#' # whapi_get_contact_profile("5581999999999")
#'
#' # Mixed (number + group JID):
#' # whapi_get_contact_profile(c("5581999999999", "1203630xxxxxx@g.us"))
#'}
#' @importFrom tibble tibble
#' @importFrom purrr map_chr map_dfr
#' @importFrom cli cli_inform cli_abort
#' @importFrom stringr str_detect
#' @export
whapi_get_contact_profile <- function(
    contacts,
    token   = Sys.getenv("WHAPI_TOKEN", unset = ""),
    full    = TRUE,
    timeout = 30,
    verbose = TRUE
) {
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")
  if (!requireNamespace("purrr",  quietly = TRUE)) stop("Package 'purrr' is required.")
  if (!requireNamespace("cli",    quietly = TRUE)) stop("Package 'cli' is required.")
  if (!requireNamespace("stringr",quietly = TRUE)) stop("Package 'stringr' is required.")

  stopifnot(is.character(contacts), length(contacts) >= 1L)
  if (!nzchar(token)) cli::cli_abort("Token not found. Provide `token` or set WHAPI_TOKEN.")

  # Normalize phones; keep JIDs as-is
  ids <- purrr::map_chr(contacts, ~ if (stringr::str_detect(.x, "@")) { .x } else { whapi_normalize_to(.x) })

  if (isTRUE(verbose)) {
    cli::cli_inform(c(
      "i" = "Fetching contact profile(s) from Whapi.Cloud...",
      ">" = paste("Total contacts:", length(ids))
    ))
  }

  fetch_one <- function(cid) {
    endpoint <- sprintf("contacts/%s/profile", cid)
    query    <- if (isTRUE(full)) list(`_full` = "true") else NULL

    out <- whapi_perform_request(
      endpoint = endpoint,
      payload  = query,          # query params for GET
      token    = token,
      timeout  = timeout,
      verbose  = verbose,
      method   = "GET"
    )

    tibble::tibble(
      contact_id = cid,
      name       = out$name       %||% NA_character_,
      about      = out$about      %||% NA_character_,
      icon       = out$icon       %||% NA_character_,
      icon_full  = out$icon_full  %||% NA_character_,
      resp       = list(out)
    )
  }

  purrr::map_dfr(ids, fetch_one)
}

#' Send an image via Whapi.Cloud (file, url, or base64)
#'
#' @description
#' Sends an image using Whapi's `POST /messages/image`.
#' Supports three input modes through `type`:
#' - `"file"`: local path -> reads bytes and builds a `data:<mime>;name=<file>;base64,<...>` URI
#' - `"url"`: direct `http(s)` URL
#' - `"base64"`: pre-built data URI (`data:image/...;base64,...`)
#'
#' @param to Character(1). WhatsApp target (E.164 digits only, no "+") or chat id.
#' @param image Character(1). File path (for `type="file"`), URL (`type="url"`),
#'   or data URI (`type="base64"`).
#' @param type One of `c("file","url","base64")`. Default = `"file"`.
#' @param caption Optional caption text.
#' @param token Bearer token (defaults to env var `WHAPI_TOKEN` if not given).
#' @param timeout Numeric. Timeout in seconds. Default `30`.
#' @param verbose Logical. Show CLI messages? Default `TRUE`.
#'
#' @return
#' A tibble with `id`, `to`, `status`, `timestamp`, `timestamp_dt`, and raw `resp`.
#'
#' @examples
#' \dontrun{
#' # Sys.setenv(WHAPI_TOKEN = "your_token_here")
#'  whapi_send_image("5581999999999", image = "card.png", type = "file", caption = "Card")
#'  whapi_send_image("5581999999999", image = "https://site.com/img.png", type = "url")
#'  b64 <- openssl::base64_encode(readBin("card.png","raw",file.info("card.png")$size))
#'  data_uri <- sprintf("data:image/png;name=%s;base64,%s", basename("card.png"), b64)
#'  whapi_send_image("5581999999999", image = data_uri, type = "base64")
#'}
#' @importFrom stringr str_detect
#' @importFrom mime guess_type
#' @importFrom openssl base64_encode
#' @importFrom cli cli_inform cli_abort
#' @importFrom tibble tibble
#' @export
whapi_send_image <- function(
    to,
    image,
    type = c("file","url","base64"),
    caption = NULL,
    token   = Sys.getenv("WHAPI_TOKEN", unset = ""),
    timeout = 30,
    verbose = TRUE
) {
  # deps
  if (!requireNamespace("tibble",  quietly = TRUE)) stop("Package 'tibble' is required.")
  if (!requireNamespace("cli",     quietly = TRUE)) stop("Package 'cli' is required.")
  if (!requireNamespace("mime",    quietly = TRUE)) stop("Package 'mime' is required.")
  if (!requireNamespace("openssl", quietly = TRUE)) stop("Package 'openssl' is required.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required.")

  type <- match.arg(type)
  stopifnot(is.character(to), length(to) == 1L, nzchar(to))
  stopifnot(is.character(image), length(image) == 1L, nzchar(image))
  if (!nzchar(token)) cli::cli_abort("Token not found. Provide `token` or set WHAPI_TOKEN.")

  to_clean <- whapi_normalize_to(to)

  # Build `media` according to type
  if (type == "file") {
    if (!file.exists(image)) cli::cli_abort(paste0("File not found: ", image))
    file_name <- basename(image)
    file_size <- file.info(image)$size
    mime_type <- mime::guess_type(image) %||% "application/octet-stream"
    raw_bytes <- readBin(image, what = "raw", n = file_size)
    b64       <- openssl::base64_encode(raw_bytes)
    media     <- sprintf("data:%s;name=%s;base64,%s", mime_type, file_name, b64)

  } else if (type == "url") {
    if (!stringr::str_detect(image, "^https?://")) {
      cli::cli_abort("`image` must be an http(s) URL when type = 'url'.")
    }
    media <- image

  } else { # type == "base64"
    if (!stringr::str_detect(image, "^data:[a-z0-9]+/[a-z0-9+.-]+;.*base64,")) {
      cli::cli_abort("`image` must be a valid data URI when type = 'base64'.")
    }
    media <- image
  }

  payload <- list(
    to    = to_clean,
    media = media
  )
  if (!is.null(caption)) payload$caption <- caption

  # Logs
  if (isTRUE(verbose)) {
    prev <- if (!is.null(caption)) paste0(" | Caption: ", substr(caption, 1, min(nchar(caption), 50)),
                                          if (nchar(caption) > 50) "...") else ""
    cli::cli_inform(c(
      "i" = "Sending image via Whapi.Cloud...",
      ">" = paste0("To: ", to_clean, " | Type: ", type, prev)
    ))
  }

  # POST /messages/image using the generic helper
  out <- whapi_perform_request(
    endpoint = "messages/image",
    payload  = payload,
    token    = token,
    timeout  = timeout,
    verbose  = verbose,
    method   = "POST"
  )

  whapi_extract_common_fields(out, fallback_to = to_clean)
}


#' Send a DOCUMENT via Whapi.Cloud (file, url, or base64)
#'
#' @description
#' Sends a document using Whapi's `POST /messages/document`.
#' Supports three input modes via `type`:
#' - `"file"`   : local path -> reads bytes and builds a data URI
#'   (`data:<mime>;name=<file>;base64,<...>`);
#' - `"url"`    : direct `http(s)` URL;
#' - `"base64"` : pre-built data URI (`data:application/...;name=...;base64,...`).
#'
#' @param to Character(1). WhatsApp target (E.164 digits only, no "+") or chat id.
#' @param document Character(1). File path (when `type="file"`), URL (`"url"`), or data URI (`"base64"`).
#' @param type One of `c("file","url","base64")`. Default: `"file"`.
#' @param caption Optional caption text.
#' @param filename Optional filename shown to the user. If omitted:
#'   - `type="file"`   -> uses `basename(document)`;
#'   - `type="url"`    -> uses the URL basename (without query/fragment);
#'   - `type="base64"` -> tries the `name=` part inside the data URI.
#' @param token Bearer token (defaults to env var `WHAPI_TOKEN`).
#' @param timeout Numeric. Request timeout in seconds. Default `30`.
#' @param verbose Logical. Print CLI messages? Default `TRUE`.
#'
#' @return
#' A tibble with `id`, `to`, `status`, `timestamp`, `timestamp_dt`, and raw `resp`.
#'
#' @examples
#' \dontrun{
#'  Sys.setenv(WHAPI_TOKEN = "your_token_here")
#'  whapi_send_document("5581999999999", "report.pdf", type="file", caption="Monthly report")
#'  whapi_send_document("5581999999999", "https://example.com/contract.docx", type="url")
#'  b <- openssl::base64_encode(readBin("memo.odt","raw",file.info("memo.odt")$size))
#'  du <- sprintf("data:application/vnd.oasis.opendocument.text;name=%s;base64,%s",
#'   basename("memo.odt"), b)
#'  whapi_send_document("5581999999999", du, type="base64")
#' }
#' @importFrom stringr str_detect str_replace str_remove str_remove_all
#' @importFrom mime guess_type
#' @importFrom openssl base64_encode
#' @importFrom cli cli_inform cli_abort
#' @importFrom tibble tibble
#' @export
whapi_send_document <- function(
    to,
    document,
    type = c("file","url","base64"),
    caption = NULL,
    filename = NULL,
    token   = Sys.getenv("WHAPI_TOKEN", unset = ""),
    timeout = 30,
    verbose = TRUE
) {
  # deps
  if (!requireNamespace("tibble",  quietly = TRUE)) stop("Package 'tibble' is required.")
  if (!requireNamespace("cli",     quietly = TRUE)) stop("Package 'cli' is required.")
  if (!requireNamespace("mime",    quietly = TRUE)) stop("Package 'mime' is required.")
  if (!requireNamespace("openssl", quietly = TRUE)) stop("Package 'openssl' is required.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required.")

  type <- match.arg(type)
  stopifnot(is.character(to), length(to) == 1L, nzchar(to))
  stopifnot(is.character(document), length(document) == 1L, nzchar(document))
  if (!nzchar(token)) cli::cli_abort("Token not found. Provide `token` or set WHAPI_TOKEN.")

  to_clean <- whapi_normalize_to(to)

  infer_name_from_url <- function(u) {
    # remove query/fragment, then take basename
    clean <- stringr::str_remove(u, "[?#].*$")
    nm <- basename(clean)
    if (is.na(nm) || nm == "" || nm == "/") NULL else nm
  }

  infer_name_from_data_uri <- function(s) {
    # extract name=<...>; in the header
    m <- regexpr("name=([^;]+);", s, ignore.case = TRUE, perl = TRUE)
    if (m[1] > 0) {
      sub("name=([^;]+);.*$", "\\1", regmatches(s, m))
    } else NULL
  }

  # Build `media` and `final_filename`
  if (type == "file") {
    if (!file.exists(document)) cli::cli_abort(paste0("File not found: ", document))
    file_name <- basename(document)
    file_size <- file.info(document)$size
    mime_type <- mime::guess_type(document) %||% "application/octet-stream"

    raw_bytes <- readBin(document, what = "raw", n = file_size)
    b64       <- openssl::base64_encode(raw_bytes)
    media     <- sprintf("data:%s;name=%s;base64,%s", mime_type, file_name, b64)
    final_filename <- filename %||% file_name

  } else if (type == "url") {
    if (!stringr::str_detect(document, "^https?://")) {
      cli::cli_abort("`document` must be an http(s) URL when type = 'url'.")
    }
    media <- document
    final_filename <- filename %||% infer_name_from_url(document)

  } else { # "base64"
    if (!stringr::str_detect(document, "^data:[a-z0-9]+/[a-z0-9+.-]+;.*base64,")) {
      cli::cli_abort("`document` must be a valid data URI when type = 'base64'.")
    }
    media <- document
    final_filename <- filename %||% infer_name_from_data_uri(document)
  }

  payload <- list(
    to    = to_clean,
    media = media
  )
  if (!is.null(caption))        payload$caption  <- caption
  if (!is.null(final_filename)) payload$filename <- final_filename

  # Logs
  if (isTRUE(verbose)) {
    meta <- paste0(
      "To: ", to_clean,
      " | Type: ", type,
      if (!is.null(final_filename)) paste0(" | Filename: ", final_filename),
      if (!is.null(caption)) paste0(" | Caption: ",
                                    substr(caption, 1, min(nchar(caption), 50)),
                                    if (nchar(caption) > 50) "...")
    )
    cli::cli_inform(c("i" = "Sending document via Whapi.Cloud...", ">" = meta))
  }

  # POST /messages/document via the generic helper
  out <- whapi_perform_request(
    endpoint = "messages/document",
    payload  = payload,
    token    = token,
    timeout  = timeout,
    verbose  = verbose,
    method   = "POST"
  )

  whapi_extract_common_fields(out, fallback_to = to_clean)
}


#' Send a geographic location via Whapi.Cloud
#'
#' @description
#' Sends a location message using Whapi's `POST /messages/location`.
#' Supports optional `name` (short title) and `address` (human-readable).
#'
#' @param to Character(1). WhatsApp target (E.164 digits only, no "+") or chat id.
#' @param latitude Numeric(1). Latitude in decimal degrees (range: -90..90).
#' @param longitude Numeric(1). Longitude in decimal degrees (range: -180..180).
#' @param name Optional character(1). Short title for the location.
#' @param address Optional character(1). Human-readable address/description.
#' @param token Bearer token (defaults to env var `WHAPI_TOKEN`).
#' @param timeout Numeric. Request timeout (seconds). Default 30.
#' @param verbose Logical. Print CLI messages? Default TRUE.
#'
#' @return
#' A tibble with `id`, `to`, `status`, `timestamp`, `timestamp_dt`, and raw `resp`.
#'
#' @examples
#' \dontrun{
#'  Sys.setenv(WHAPI_TOKEN = "your_token_here")
#'  whapi_send_location("5581999999999",
#'    latitude = -8.063169, longitude = -34.871139,
#'    name = "Marco Zero", address = "Recife, PE")
#'
#' # Group id example
#'  whapi_send_location("1203630xxxxxxxx@g.us", -8.045, -34.91)
#'}
#' @importFrom cli cli_inform cli_abort
#' @importFrom tibble tibble
#' @export
whapi_send_location <- function(
    to,
    latitude,
    longitude,
    name    = NULL,
    address = NULL,
    token   = Sys.getenv("WHAPI_TOKEN", unset = ""),
    timeout = 30,
    verbose = TRUE
) {
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")
  if (!requireNamespace("cli",    quietly = TRUE)) stop("Package 'cli' is required.")

  # Validations
  stopifnot(is.character(to), length(to) == 1L, nzchar(to))
  if (!is.numeric(latitude)  || length(latitude)  != 1L || is.na(latitude))
    cli::cli_abort("`latitude` must be a single numeric.")
  if (!is.numeric(longitude) || length(longitude) != 1L || is.na(longitude))
    cli::cli_abort("`longitude` must be a single numeric.")
  if (latitude  < -90  || latitude  >  90)
    cli::cli_abort("`latitude` out of range [-90, 90].")
  if (longitude < -180 || longitude > 180)
    cli::cli_abort("`longitude` out of range [-180, 180].")
  if (!nzchar(token))
    cli::cli_abort("Token not found. Provide `token` or set WHAPI_TOKEN.")

  to_clean <- whapi_normalize_to(to)

  # Payload as per Whapi Location message
  payload <- list(
    to        = to_clean,
    latitude  = latitude,
    longitude = longitude
  )
  if (!is.null(name))    payload$name    <- name
  if (!is.null(address)) payload$address <- address

  # Logs
  if (isTRUE(verbose)) {
    cli::cli_inform(c(
      "i" = "Sending LOCATION message via Whapi.Cloud...",
      ">" = paste0("To: ", to_clean,
                   " | Lat/Lng: ", signif(latitude, 8), ", ", signif(longitude, 8),
                   if (!is.null(name))    paste0(" | Name: ", substr(name, 1, 40),
                                                 if (nchar(name) > 40) "..."),
                   if (!is.null(address)) paste0(" | Address: ", substr(address, 1, 50),
                                                 if (nchar(address) > 50) "..."))
    ))
  }

  # POST /messages/location via generic helper
  out <- whapi_perform_request(
    endpoint = "messages/location",
    payload  = payload,
    token    = token,
    timeout  = timeout,
    verbose  = verbose,
    method   = "POST"
  )

  whapi_extract_common_fields(out, fallback_to = to_clean)
}

#' Mark a WhatsApp message as READ (Whapi.Cloud)
#'
#' @description
#' Marks a message as **read** using `PUT /messages/{MessageID}`.
#' This endpoint returns only a minimalistic ACK in the body:
#' `{"success": true | false}`.
#'
#' @param message_id Character(1). Message ID (WAMID) to be marked as read.
#' @param token Character(1). Bearer token. Default: `Sys.getenv("WHAPI_TOKEN")`.
#' @param timeout Numeric(1). Timeout (s). Default: 30.
#' @param verbose Logical(1). Show logs via \pkg{cli}? Default: `TRUE`.
#'
#' @return
#' A tibble with the following columns:
#' - `id`        - the provided `message_id`;
#' - `status`    - `"read"` when `success=TRUE`, `"error"` otherwise;
#' - `success`   - logical value returned by the endpoint;
#' - `resp`      - raw response (list).
#'
#' @examples
#' \dontrun{
#' # Sys.setenv(WHAPI_TOKEN = "your_token_here")
#' # whapi_mark_message_read("PsqXn5SAD5v7HRA-wHqB9tMeGQ")
#' }
#' @importFrom cli cli_inform cli_abort
#' @importFrom tibble tibble
#' @export
whapi_mark_message_read <- function(
    message_id,
    token   = Sys.getenv("WHAPI_TOKEN", unset = ""),
    timeout = 30,
    verbose = TRUE
) {
  if (!requireNamespace("cli",    quietly = TRUE)) stop("Package 'cli' is required.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")

  stopifnot(is.character(message_id), length(message_id) == 1L, nzchar(message_id))
  if (!nzchar(token)) cli::cli_abort("Token not found. Provide `token` or set WHAPI_TOKEN.")

  endpoint <- sprintf("messages/%s", message_id)

  if (isTRUE(verbose)) {
    cli::cli_inform(c(
      "i" = "Marking message as READ via Whapi.Cloud...",
      ">" = paste0("Message ID: ", message_id)
    ))
  }


  out <- whapi_perform_request(
    endpoint = endpoint,
    payload  = list(status = "read"),
    token    = token,
    timeout  = timeout,
    verbose  = verbose,
    method   = "PUT"
  )

  success <- isTRUE(out$success)
  status  <- if (success) "read" else "error"

  if (isTRUE(verbose)) {
    if (success) {
      cli::cli_inform(c("v" = paste0("Marked as READ (id=", message_id, "). Success: TRUE")))
    } else {
      cli::cli_inform(c("!" = paste0("Failed to mark as READ (id=", message_id, "). Success: FALSE")))
    }
  }

  tibble::tibble(
    id      = message_id,
    status  = status,
    success = success,
    resp    = list(out)
  )
}


#' React to a WhatsApp message (Whapi.Cloud)
#'
#' @description
#' Sends (or removes) an **emoji reaction** to a message via
#' `PUT /messages/{MessageID}/reaction`.
#' The endpoint returns only `{ "success": true | false }`.
#'
#' @param message_id Character(1). Target message ID (WAMID).
#' @param emoji Character(1). Emoji to react with.
#'   To remove a reaction, pass an empty string `""`.
#' @param token Character(1). Bearer token. Defaults to env var `WHAPI_TOKEN`.
#' @param timeout Numeric(1). Request timeout in seconds. Default: 30.
#' @param verbose Logical(1). Print CLI logs? Default: TRUE.
#'
#' @return
#' A tibble with columns:
#' - `id`        - message_id reacted to
#' - `emoji`     - emoji used (or `""` if removed)
#' - `status`    - `"ok"` if success, `"error"` otherwise
#' - `success`   - logical flag from API
#' - `resp`      - raw response (list)
#'
#' @examples
#' \dontrun{
#' Sys.setenv(WHAPI_TOKEN = "your_token_here")
#' Add a reaction:
#' whapi_react_to_message("PsqXn5SAD5v7HRA-wHqB9tMeGQ", "<heart emoji>")
#'
#' # Remove a reaction:
#' whapi_react_to_message("PsqXn5SAD5v7HRA-wHqB9tMeGQ", "")
#'}
#' @importFrom cli cli_inform cli_abort
#' @importFrom tibble tibble
#' @export
whapi_react_to_message <- function(
    message_id,
    emoji,
    token   = Sys.getenv("WHAPI_TOKEN", unset = ""),
    timeout = 30,
    verbose = TRUE
) {
  if (!requireNamespace("cli",    quietly = TRUE)) stop("Package 'cli' is required.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")

  stopifnot(is.character(message_id), length(message_id) == 1L, nzchar(message_id))
  stopifnot(is.character(emoji), length(emoji) == 1L) # pode ser vazio para remover
  if (!nzchar(token)) cli::cli_abort("Token not found. Provide `token` or set WHAPI_TOKEN.")

  endpoint <- sprintf("messages/%s/reaction", message_id)

  if (isTRUE(verbose)) {
    cli::cli_inform(c(
      "i" = "Sending reaction via Whapi.Cloud...",
      ">" = paste0("Message ID: ", message_id,
                   " | Emoji: ", ifelse(nzchar(emoji), emoji, "<remove>"))
    ))
  }

  out <- whapi_perform_request(
    endpoint = endpoint,
    payload  = list(emoji = emoji),
    token    = token,
    timeout  = timeout,
    verbose  = verbose,
    method   = "PUT"
  )

  success <- isTRUE(out$success)
  status  <- if (success) "ok" else "error"

  if (isTRUE(verbose)) {
    if (success) {
      cli::cli_inform(c("v" = paste0("Reaction processed (id=", message_id, "). Success: TRUE")))
    } else {
      cli::cli_inform(c("!" = paste0("Failed to process reaction (id=", message_id, "). Success: FALSE")))
    }
  }

  tibble::tibble(
    id      = message_id,
    emoji   = emoji,
    status  = status,
    success = success,
    resp    = list(out)
  )
}


#' Check Whapi.Cloud channel health and status
#'
#' @description
#' Calls `GET /health` to retrieve channel status, versions,
#' uptime, device, IP, and user info.
#'
#' @param wakeup Logical(1). If TRUE, adds `wakeup=true` query param. Default: TRUE.
#' @param channel_type Character(1). Channel type, either `"web"` or `"mobile"`. Default: `"web"`.
#' @param token Character(1). Bearer token. Defaults to env var `WHAPI_TOKEN`.
#' @param timeout Numeric(1). Request timeout (s). Default 30.
#' @param verbose Logical(1). Print CLI logs? Default TRUE.
#'
#' @return
#' A tibble with key health information:
#' - `channel_id`, `uptime`, `version`, `core_version`, `api_version`,
#' - `device_id`, `ip`,
#' - `status_code`, `status_text`,
#' - `user_id`, `user_name`, `user_pushname`, `is_business`,
#' - `profile_pic`, `profile_pic_full`, `user_status`,
#' plus the raw response in `resp`.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(WHAPI_TOKEN = "your_token_here")
#' # Default check (wakeup=TRUE, channel_type="web")
#' whapi_check_health()
#'
#' # Check with channel_type = "mobile"
#' whapi_check_health(channel_type = "mobile")
#' }
#' @importFrom cli cli_inform cli_abort
#' @importFrom tibble tibble
#' @export
whapi_check_health <- function(
    wakeup       = TRUE,
    channel_type = c("web", "mobile"),
    token        = Sys.getenv("WHAPI_TOKEN", unset = ""),
    timeout      = 30,
    verbose      = TRUE
) {
  if (!requireNamespace("cli",    quietly = TRUE)) stop("Package 'cli' is required.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")

  channel_type <- match.arg(channel_type)

  if (!nzchar(token)) cli::cli_abort("Token not found. Provide `token` or set WHAPI_TOKEN.")

  # build query params
  query <- list()
  if (isTRUE(wakeup)) query$wakeup <- "true"
  query$channel_type <- channel_type

  if (isTRUE(verbose)) {
    cli::cli_inform(c(
      "i" = "Checking Whapi.Cloud health...",
      ">" = paste0("Wakeup: ", wakeup, " | Channel: ", channel_type)
    ))
  }

  out <- whapi_perform_request(
    endpoint = "health",
    payload  = query,
    token    = token,
    timeout  = timeout,
    verbose  = verbose,
    method   = "GET"
  )

  tibble::tibble(
    channel_id       = out$channel_id       %||% NA_character_,
    start_at         = out$start_at         %||% NA_real_,
    uptime           = out$uptime           %||% NA_real_,
    version          = out$version          %||% NA_character_,
    core_version     = out$core_version     %||% NA_character_,
    api_version      = out$api_version      %||% NA_character_,
    device_id        = out$device_id        %||% NA_real_,
    ip               = out$ip               %||% NA_character_,
    status_code      = out$status$code      %||% NA_real_,
    status_text      = out$status$text      %||% NA_character_,
    user_id          = out$user$id          %||% NA_character_,
    user_name        = out$user$name        %||% NA_character_,
    user_pushname    = out$user$pushname    %||% NA_character_,
    is_business      = out$user$is_business %||% NA,
    profile_pic      = out$user$profile_pic %||% NA_character_,
    profile_pic_full = out$user$profile_pic_full %||% NA_character_,
    user_status      = out$user$status      %||% NA_character_,
    resp             = list(out)
  )
}

#' Send a STICKER via Whapi.Cloud (file, url, or base64)
#'
#' @description
#' Sends a WhatsApp sticker via `POST /messages/sticker`.
#' The sticker must be in WebP format (`image/webp`).
#'
#' @param to Character(1). WhatsApp target (E.164 digits only, no "+") or chat id.
#' @param sticker Character(1).
#'   - If type = "file": local `.webp` file path (will be read and encoded to Base64 data-URI).
#'   - If type = "url": direct http(s) URL to the `.webp` file.
#'   - If type = "base64": full data-URI string (e.g. "data:image/webp;name=st.webp;base64,<...>").
#' @param type One of c("file","url","base64"). Default = "file".
#' @param token Bearer token (env var WHAPI_TOKEN if not provided).
#' @param timeout Numeric. Request timeout (seconds). Default 30.
#' @param verbose Logical. Print CLI messages? Default TRUE.
#'
#' @return tibble with id, to, status, timestamp, and raw response in `resp`.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(WHAPI_TOKEN = "your_token_here")
#'
#' # 1) Local file
#'  whapi_send_sticker("558191812121",
#'    sticker = "sticker.webp", type = "file")
#'
#' # 2) Remote URL
#'  whapi_send_sticker("558191812121",
#'    sticker = "https://example.com/condepe.webp", type = "url")
#'
#' # 3) Pre-encoded Base64
#'  b64 <- openssl::base64_encode(readBin("sticker.webp","raw",file.info("sticker.webp")$size))
#'  data_uri <- sprintf("data:image/webp;name=%s;base64,%s", basename("sticker.webp"), b64)
#'  whapi_send_sticker("558191812121", sticker = data_uri, type = "base64")
#'  }
#' @export
whapi_send_sticker <- function(
    to,
    sticker,
    type = c("file","url","base64"),
    token   = Sys.getenv("WHAPI_TOKEN", unset = ""),
    timeout = 30,
    verbose = TRUE
) {
  if (!requireNamespace("mime",    quietly = TRUE)) stop("Package 'mime' is required.")
  if (!requireNamespace("openssl", quietly = TRUE)) stop("Package 'openssl' is required.")
  if (!requireNamespace("cli",     quietly = TRUE)) stop("Package 'cli' is required.")
  if (!requireNamespace("tibble",  quietly = TRUE)) stop("Package 'tibble' is required.")

  type <- match.arg(type)
  stopifnot(is.character(to), length(to) == 1L, nzchar(to))
  stopifnot(is.character(sticker), length(sticker) == 1L, nzchar(sticker))
  if (!nzchar(token)) cli::cli_abort("Token not found. Provide `token` or set WHAPI_TOKEN.")

  to_clean <- whapi_normalize_to(to)

  # Build media according to type
  if (type == "file") {
    if (!file.exists(sticker)) cli::cli_abort("File not found: {sticker}")
    file_name <- basename(sticker)
    file_size <- file.info(sticker)$size
    raw_bytes <- readBin(sticker, what = "raw", n = file_size)
    b64       <- openssl::base64_encode(raw_bytes)
    media     <- sprintf("data:image/webp;name=%s;base64,%s", file_name, b64)

  } else if (type == "url") {
    if (!grepl("^https?://", sticker, ignore.case = TRUE)) {
      cli::cli_abort("`sticker` must be http(s) URL when type='url'.")
    }
    media <- sticker

  } else { # base64
    if (!grepl("^data:image/webp;.*base64,", sticker, ignore.case = TRUE)) {
      cli::cli_abort("`sticker` must be a valid WebP data URI when type='base64'.")
    }
    media <- sticker
  }

  payload <- list(
    to    = to_clean,
    media = media
  )

  if (verbose) {
    cli::cli_inform(c(
      "i" = "Sending sticker via Whapi.Cloud...",
      ">" = paste0("To: ", to_clean, " | Type: ", type)
    ))
  }

  out <- whapi_perform_request(
    endpoint = "messages/sticker",
    payload  = payload,
    token    = token,
    timeout  = timeout,
    verbose  = verbose,
    method   = "POST"
  )

  whapi_extract_common_fields(out, fallback_to = to_clean)
}



#' Build Whapi service sections from a tibble (with `section` column)
#'
#' @description
#' Converts a tibble of services (with columns `id`, `title`, `description`, and `section`)
#' into a nested list of **sections/rows** in the format expected by Whapi interactive
#' messages.
#'
#' @details
#' - If `section_order = NULL`, all sections are included, ordered alphabetically.
#' - If `section_order` is provided, it acts as both:
#'   - a **filter**: only sections listed will be included,
#'   - and an **order**: sections appear in the same order as in `section_order`.
#'
#' Within each section, rows are ordered by the numeric part of `id`
#' (via `readr::parse_number(id)`).
#'
#' @param tbl A tibble/data.frame containing at least:
#' - `section`   (chr): section name
#' - `id`        (chr): unique identifier (e.g., `"ap1"`, `"r2"`, `"os3"`)
#' - `title`     (chr): display title (can include emoji)
#' - `descricao` (chr): short description of the service
#' @param section_order Character vector defining desired order (and subset) of sections.
#'   If `NULL`, all sections are included in alphabetical order.
#'
#' @return A list of sections, where each section is a list with:
#' - `title`: section title
#' - `rows`: a list of rows, each being a list with `id`, `title`, and `description`
#'
#' @examples
#' de_para_servicos <- tibble::tibble(
#'   section   = c("Outros Servicos","Renovacoes","Anuencia Previa"),
#'   id        = c("os2","r4","ap1"),
#'   title     = c("Consulta Previa",
#'                 "Renovacao de Consulta Previa",
#'                 "Desmembramento"),
#'   descricao = c("Initial analysis...","Renewal...","Technical authorization...")
#' )
#'
#' # All sections (alphabetical)
#' whapi_build_servicos_sections(de_para_servicos)
#'
#' # Custom order and filter
#' whapi_build_servicos_sections(
#'   de_para_servicos,
#'   section_order = c("Anuencia Previa","Outros Servicos")
#' )
#'
#' @export
whapi_build_servicos_sections <- function(tbl, section_order = NULL) {
  stopifnot(all(c("section","id","title","descricao") %in% names(tbl)))

  tbl2 <- tbl |>
    dplyr::mutate(ord = readr::parse_number(id)) |>
    dplyr::filter(!is.na(section))

  if (!is.null(section_order)) {
    # filter + keep order
    tbl2 <- tbl2 |>
      dplyr::filter(section %in% section_order) |>
      dplyr::mutate(section = factor(section, levels = section_order))
  } else {
    # alphabetical
    tbl2 <- tbl2 |>
      dplyr::mutate(section = factor(section, levels = sort(unique(section))))
  }

  tbl2 <- tbl2 |>
    dplyr::arrange(section, ord)

  sections <- split(tbl2, tbl2$section) |>
    purrr::map(function(df_sec) {
      rows <- purrr::pmap(df_sec[, c("id","title","descricao")],
                          function(id, title, descricao) {
                            list(id = id, title = title, description = descricao)
                          })
      list(title = as.character(unique(df_sec$section)), rows = rows)
    }) |>
    unname()

  sections
}


#' Convert text to lowercase ASCII (remove accents)
#'
#' @description
#' Converts input text to lowercase and strips diacritics (accents) by
#' applying a `latin-ascii` transliteration. Useful for normalization before
#' matching or slug generation.
#'
#' @param x Character vector or string. If `NULL`, an empty string is used.
#'
#' @return A character vector in lowercase ASCII without accents.
#' @examples
#' whapi_to_ascii_lower("Sao Paulo")
#' #> "sao paulo"
#' @export
whapi_to_ascii_lower <- function(x) {
  stringr::str_to_lower(stringi::stri_trans_general(x %||% "", "latin-ascii"))
}

#' Keep only digits from a string
#'
#' @description
#' Removes all non-digit characters from the input string. Useful for cleaning
#' phone numbers, CNPJs/CPFs, or process codes.
#'
#' @param x Character vector or string. If `NULL`, an empty string is used.
#'
#' @return A string containing only numeric digits.
#' @examples
#' whapi_only_digits("(81) 98765-4321")
#' #> "81987654321"
#' @export
whapi_only_digits <- function(x) {
  stringr::str_replace_all(x %||% "", "\\D+", "")
}

#' Digit matching helper (with partial threshold)
#'
#' @description
#' Matches a digit-only `needle` inside a digit-only `haystack`.
#' If `needle` has at least `min_partial` digits, partial matching is allowed.
#' Otherwise, only exact matches are considered.
#'
#' @param haystack String with potential digits to search in.
#' @param needle String with digits to search for.
#' @param min_partial Minimum number of digits required to allow partial match.
#'
#' @return Logical (`TRUE`/`FALSE`) indicating whether a match was found.
#' @examples
#' whapi_match_digits("tel: 81998765432", "987654")
#' #> TRUE
#' whapi_match_digits("12345", "123", min_partial = 4)
#' #> FALSE
#' @export
whapi_match_digits <- function(haystack, needle, min_partial = 6) {
  hs <- whapi_only_digits(haystack); nd <- whapi_only_digits(needle)
  if (!nzchar(nd)) return(FALSE)
  if (nchar(nd) >= min_partial) stringr::str_detect(hs, stringr::fixed(nd)) else (nzchar(hs) && identical(hs, nd))
}

#' Clip long text with ellipsis
#'
#' @description
#' Shortens text to a maximum width (character length). If the text is longer,
#' it is truncated and an ellipsis (default `"..."`) is appended.
#'
#' @param x Character string to clip.
#' @param width Maximum length of the output including ellipsis.
#' @param ellipsis Character to indicate truncation.
#'
#' @return A clipped string with ellipsis if needed.
#' @examples
#' whapi_clip_text("This is a very long sentence that should be clipped.", width = 20)
#' #> "This is a very long..."
#' @export
whapi_clip_text <- function(x, width = 420, ellipsis = "...") {
  x <- stringr::str_trim(x %||% "")
  if (!nzchar(x)) return("")
  if (nchar(x) <= width) x else paste0(substr(x, 1, width - nchar(ellipsis)), ellipsis)
}

#' Days between two dates
#'
#' @description
#' Calculates the difference in days between two dates (`end - start`).
#' Returns `NA` if the start date is missing.
#'
#' @param start Start date (`Date` or coercible).
#' @param end End date (`Date` or coercible).
#'
#' @return Integer number of days, or `NA` if `start` is missing.
#' @examples
#' whapi_date_diff_days(Sys.Date() - 10, Sys.Date())
#' #> 10
#' @export
whapi_date_diff_days <- function(start, end) {
  if (is.na(start)) return(NA_integer_)
  as.integer(as.Date(end) - as.Date(start))
}

#' Safe date formatting
#'
#' @description
#' Formats a date safely, returning a fallback value (`na`) when the input
#' is `NULL` or `NA`.
#'
#' @param x Date or coercible to `Date`.
#' @param fmt Date format passed to [base::format()].
#' @param na Fallback string if the input is missing.
#'
#' @return A formatted date string, or the `na` placeholder if missing.
#' @examples
#' whapi_fmt_date(Sys.Date())
#' #> "31/08/2025"
#' whapi_fmt_date(NA)
#' #> "-"
#' @export
whapi_fmt_date <- function(x, fmt = "%d/%m/%Y", na = "-") {
  if (is.null(x) || is.na(x)) return(na)
  format(as.Date(x), fmt)
}
