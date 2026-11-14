# Carousel message functions for Whapi.Cloud
# These functions handle sending carousel messages with multiple cards

#' Validate and prepare buttons for carousel cards
#'
#' @description
#' Internal helper that validates buttons for carousel cards. Carousel card buttons
#' can be of different types: `url`, `quick_reply`, `call`, or `copy`.
#'
#' This function validates required fields for each button type:
#' - `url`         -> requires: `type`, `title`, `id`, `url`
#' - `quick_reply` -> requires: `type`, `title`, `id`
#' - `call`        -> requires: `type`, `title`, `id`, `phone_number`
#' - `copy`        -> requires: `type`, `title`, `id`, `copy_code`
#'
#' @param buttons A list of button objects. Each button must be a named list
#'   with the required fields for its type.
#' @param card_idx Integer. Card index for error messages.
#' @param verbose Logical (default `TRUE`). If `TRUE`, prints progress messages.
#'
#' @return The validated list of buttons (unchanged if valid).
#'
#' @examples
#' buttons <- list(
#'   list(type = "url", title = "Website", id = "btn1", url = "https://example.com"),
#'   list(type = "quick_reply", title = "More Info", id = "btn2")
#' )
#' whapi_validate_carousel_buttons(buttons, card_idx = 1)
#'
#' @importFrom cli cli_inform cli_abort
#' @importFrom purrr map
#' @export
whapi_validate_carousel_buttons <- function(buttons, card_idx = 1, verbose = TRUE) {
  if (is.null(buttons) || !is.list(buttons) || length(buttons) < 1) {
    cli::cli_abort(paste0("Card ", card_idx, " must have at least one button."))
  }

  valid_types <- c("url", "quick_reply", "call", "copy")

  # Use map with seq_along instead of imap to avoid adding names
  validated <- purrr::map(seq_along(buttons), function(idx) {
    btn <- buttons[[idx]]

    # Ensure it's a list
    if (!is.list(btn)) {
      cli::cli_abort(paste0("Card ", card_idx, ", button ", idx, " must be a list."))
    }

    # Check type
    if (is.null(btn$type) || !nzchar(btn$type)) {
      cli::cli_abort(paste0(
        "Card ", card_idx, ", button ", idx,
        " must have a `type` field (url, quick_reply, call, or copy)."
      ))
    }

    if (!(btn$type %in% valid_types)) {
      cli::cli_abort(paste0(
        "Card ", card_idx, ", button ", idx,
        " has invalid type '", btn$type, "'. ",
        "Must be one of: ", paste(valid_types, collapse = ", ")
      ))
    }

    # Check required fields based on type
    if (is.null(btn$title) || !nzchar(btn$title)) {
      cli::cli_abort(paste0("Card ", card_idx, ", button ", idx, " must have a `title`."))
    }

    if (is.null(btn$id) || !nzchar(btn$id)) {
      cli::cli_abort(paste0("Card ", card_idx, ", button ", idx, " must have an `id`."))
    }

    # Type-specific validations
    if (btn$type == "url") {
      if (is.null(btn$url) || !nzchar(btn$url)) {
        cli::cli_abort(paste0(
          "Card ", card_idx, ", button ", idx,
          " (type=url) must have a `url` field."
        ))
      }
    }

    if (btn$type == "call") {
      if (is.null(btn$phone_number) || !nzchar(btn$phone_number)) {
        cli::cli_abort(paste0(
          "Card ", card_idx, ", button ", idx,
          " (type=call) must have a `phone_number` field."
        ))
      }
    }

    if (btn$type == "copy") {
      if (is.null(btn$copy_code) || !nzchar(btn$copy_code)) {
        cli::cli_abort(paste0(
          "Card ", card_idx, ", button ", idx,
          " (type=copy) must have a `copy_code` field."
        ))
      }
    }

    # Return button as-is (no modifications) - keep all original fields
    btn
  })

  if (isTRUE(verbose)) {
    types <- vapply(validated, `[[`, "", "type")
    counts <- table(types)
    cli::cli_inform(c("v" = paste0(
      "Card ", card_idx, " buttons OK: ",
      paste(paste0(names(counts), "=", as.integer(counts)), collapse = ", ")
    )))
  }

  # Return unnamed list to ensure proper JSON serialization
  unname(validated)
}


#' Validate carousel cards structure for Whapi
#'
#' @description
#' Validates the structure of `cards` used in Whapi **CAROUSEL** messages.
#' Each card must provide:
#' - `media`: a list with `media` field (URL or base64 data URI)
#' - `text`: card description text
#' - `id`: unique card identifier
#' - `buttons`: list of button objects
#'
#' @details
#' For **image media**, you can use:
#' - **URL**: `"https://example.com/image.jpg"`
#' - **Base64**: `"data:image/jpeg;base64,/9j/4AAQ..."` (full data URI)
#'
#' Note: For base64, include the complete data URI with mimetype prefix.
#'
#' @param cards A list of card objects.
#' @param verbose Logical (default `TRUE`). If `TRUE`, prints progress messages.
#'
#' @return The validated cards list.
#'
#' @examples
#' cards <- list(
#'   list(
#'     media = list(media = "https://example.com/image1.jpg"),
#'     text = "Product 1",
#'     id = "card-1",
#'     buttons = list(
#'       list(type = "url", title = "Buy", id = "btn-1", url = "https://shop.com")
#'     )
#'   ),
#'   list(
#'     media = list(media = "https://example.com/image2.jpg"),
#'     text = "Product 2",
#'     id = "card-2",
#'     buttons = list(
#'       list(type = "quick_reply", title = "Info", id = "btn-2")
#'     )
#'   )
#' )
#' whapi_validate_carousel_cards(cards)
#'
#' @importFrom purrr map
#' @importFrom stringr str_trim str_detect
#' @importFrom cli cli_inform cli_abort
#' @export
whapi_validate_carousel_cards <- function(cards, verbose = TRUE) {
  if (is.null(cards) || !is.list(cards) || length(cards) < 1) {
    cli::cli_abort("Provide `cards` as a non-empty list of card objects.")
  }

  if (length(cards) < 2) {
    cli::cli_abort("A CAROUSEL message requires at least 2 cards.")
  }

  if (length(cards) > 10) {
    cli::cli_abort("A CAROUSEL message supports a maximum of 10 cards.")
  }

  if (isTRUE(verbose)) {
    cli::cli_inform(c("i" = paste0("Validating ", length(cards), " carousel card(s)...")))
  }

  # Use map with seq_along instead of imap to avoid adding names
  validated_cards <- purrr::map(seq_along(cards), function(idx) {
    card <- cards[[idx]]

    # Validate media structure
    if (is.null(card$media)) {
      cli::cli_abort(paste0("Card ", idx, " must have a `media` field."))
    }

    # Media can be a list with 'media' key, or just a string
    # Normalize to list format
    if (is.character(card$media)) {
      # If media is a string, convert to list format
      card$media <- list(media = card$media)
    }

    if (!is.list(card$media) || is.null(card$media$media) || !nzchar(card$media$media)) {
      cli::cli_abort(paste0(
        "Card ", idx, " `media` must have a non-empty `media` field ",
        "(URL or base64 data URI)."
      ))
    }

    # Validate text
    if (is.null(card$text) || !nzchar(stringr::str_trim(card$text))) {
      cli::cli_abort(paste0("Card ", idx, " must have a non-empty `text` field."))
    }

    # Validate id
    if (is.null(card$id) || !nzchar(stringr::str_trim(card$id))) {
      cli::cli_abort(paste0("Card ", idx, " must have a non-empty `id` field."))
    }

    # Validate buttons
    card$buttons <- whapi_validate_carousel_buttons(
      card$buttons,
      card_idx = idx,
      verbose = verbose
    )

    card
  })

  if (isTRUE(verbose)) {
    total_buttons <- sum(vapply(validated_cards, function(c) length(c$buttons), integer(1)))
    cli::cli_inform(c("v" = paste0(
      "CAROUSEL validated: ", length(validated_cards), " cards, ",
      total_buttons, " total buttons."
    )))
  }

  # Return unnamed list to ensure proper JSON serialization
  unname(validated_cards)
}


#' Send a WhatsApp CAROUSEL message via Whapi.Cloud
#'
#' @description
#' Sends an interactive **CAROUSEL** message via Whapi (`POST /messages/carousel`).
#' A carousel displays multiple cards that users can swipe horizontally, each with
#' an image, text, and interactive buttons.
#'
#' @details
#' Each card supports buttons of different types:
#' - `url`: Opens a URL (requires `url` field)
#' - `quick_reply`: Sends a quick reply (only needs `title` and `id`)
#' - `call`: Initiates a phone call (requires `phone_number` field)
#' - `copy`: Copies text to clipboard (requires `copy_code` field)
#'
#' For **images**, you can use:
#' - HTTP/HTTPS URLs
#' - Base64 data URIs (e.g., `"data:image/jpeg;base64,/9j/4AAQ..."`)
#'
#' @param to Character(1). WhatsApp target (E.164 digits only, no "+") or group id.
#' @param body_text Character(1). Main text shown above the carousel.
#' @param cards A list of card objects. Each card must have:
#'   \itemize{
#'     \item \code{media}: Image URL or base64 (can be string or list with \code{media} key)
#'     \item \code{text}: Card description
#'     \item \code{id}: Unique card ID
#'     \item \code{buttons}: List of button objects with \code{type}, \code{title},
#'       \code{id}, and type-specific fields
#'   }
#' @param token Character(1). Bearer token. Defaults to env var `WHAPI_TOKEN`.
#' @param timeout Numeric(1). Request timeout in seconds. Default: 30.
#' @param verbose Logical(1). Print CLI messages? Default: TRUE.
#'
#' @return A tibble with message metadata.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(WHAPI_TOKEN = "your_token_here")
#'
#' # Example with URL button
#' cards <- list(
#'   list(
#'     media = list(media = "https://example.com/product1.jpg"),
#'     text = "Amazing product!",
#'     id = "card-1",
#'     buttons = list(
#'       list(type = "url", title = "Buy Now", id = "btn-1",
#'            url = "https://shop.example.com/product1")
#'     )
#'   ),
#'   list(
#'     media = list(media = "https://example.com/product2.jpg"),
#'     text = "Best seller!",
#'     id = "card-2",
#'     buttons = list(
#'       list(type = "quick_reply", title = "More Info", id = "btn-2")
#'     )
#'   )
#' )
#'
#' whapi_send_carousel(
#'   to = "5581999999999",
#'   body_text = "Check out our products!",
#'   cards = cards
#' )
#'
#' # Example with mixed button types
#' cards_mixed <- list(
#'   list(
#'     media = list(media = "https://example.com/contact.jpg"),
#'     text = "Contact us",
#'     id = "card-contact",
#'     buttons = list(
#'       list(type = "url", title = "Website", id = "btn-web",
#'            url = "https://example.com"),
#'       list(type = "call", title = "Call Us", id = "btn-call",
#'            phone_number = "5581999999999")
#'     )
#'   ),
#'   list(
#'     media = list(media = "https://example.com/promo.jpg"),
#'     text = "Use code SAVE10",
#'     id = "card-promo",
#'     buttons = list(
#'       list(type = "copy", title = "Copy Code", id = "btn-copy",
#'            copy_code = "SAVE10")
#'     )
#'   )
#' )
#' }
#'
#' @importFrom cli cli_inform cli_abort
#' @importFrom tibble tibble
#' @export
whapi_send_carousel <- function(
    to,
    body_text,
    cards,
    token   = Sys.getenv("WHAPI_TOKEN", unset = ""),
    timeout = 30,
    verbose = TRUE
) {
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")
  if (!requireNamespace("cli",    quietly = TRUE)) stop("Package 'cli' is required.")
  if (!requireNamespace("purrr",  quietly = TRUE)) stop("Package 'purrr' is required.")
  if (!requireNamespace("stringr",quietly = TRUE)) stop("Package 'stringr' is required.")

  stopifnot(is.character(to), length(to) == 1L, nzchar(to))
  stopifnot(is.character(body_text), length(body_text) == 1L, nzchar(body_text))

  if (!nzchar(token)) {
    cli::cli_abort("Token not found. Provide `token` or set env var WHAPI_TOKEN.")
  }

  # Normalize recipient
  to_clean <- whapi_normalize_to(to)

  # Validate cards (this validates but doesn't transform buttons)
  validated_cards <- whapi_validate_carousel_cards(cards, verbose = verbose)

  # Build payload exactly as Whapi expects
  payload <- list(
    to    = to_clean,
    body  = list(text = body_text),
    cards = validated_cards
  )

  if (isTRUE(verbose)) {
    total_buttons <- sum(vapply(validated_cards, function(c) length(c$buttons), integer(1)))
    cli::cli_inform(c(
      "i" = "Sending CAROUSEL message via Whapi.Cloud...",
      ">" = paste0("To: ", to_clean,
                   " | Cards: ", length(validated_cards),
                   " | Buttons: ", total_buttons)
    ))
  }

  # POST /messages/carousel
  out <- whapi_perform_request(
    endpoint = "messages/carousel",
    payload  = payload,
    token    = token,
    timeout  = timeout,
    verbose  = verbose,
    method   = "POST"
  )

  result <- whapi_extract_common_fields(out, fallback_to = to_clean)

  if (isTRUE(verbose)) {
    cli::cli_inform(c("v" = paste0("CAROUSEL sent (id=", result$id, "). Status: ", result$status)))
  }

  result
}


#' Create a carousel card helper
#'
#' @description
#' Convenience function to create a single carousel card with proper structure.
#'
#' @param media Character(1). Image URL or base64 data URI.
#' @param text Character(1). Card description text.
#' @param id Character(1). Unique card identifier.
#' @param buttons A list of button objects. Each button must have:
#'   \itemize{
#'     \item \code{type}: "url", "quick_reply", "call", or "copy"
#'     \item \code{title}: Button text
#'     \item \code{id}: Button identifier
#'     \item Type-specific: \code{url}, \code{phone_number}, or \code{copy_code}
#'   }
#'
#' @return A named list representing a carousel card.
#'
#' @examples
#' # URL button
#' card1 <- whapi_carousel_card(
#'   media = "https://example.com/product.jpg",
#'   text = "Amazing product!",
#'   id = "card-1",
#'   buttons = list(
#'     list(type = "url", title = "Buy Now", id = "btn-1",
#'          url = "https://shop.example.com")
#'   )
#' )
#'
#' # Quick reply button
#' card2 <- whapi_carousel_card(
#'   media = "https://example.com/service.jpg",
#'   text = "Learn more about us",
#'   id = "card-2",
#'   buttons = list(
#'     list(type = "quick_reply", title = "Tell me more", id = "btn-2")
#'   )
#' )
#'
#' # Multiple button types
#' card3 <- whapi_carousel_card(
#'   media = "https://example.com/contact.jpg",
#'   text = "Get in touch",
#'   id = "card-3",
#'   buttons = list(
#'     list(type = "url", title = "Website", id = "btn-web",
#'          url = "https://example.com"),
#'     list(type = "call", title = "Call Us", id = "btn-call",
#'          phone_number = "5581999999999")
#'   )
#' )
#'
#' @importFrom cli cli_abort
#' @export
whapi_carousel_card <- function(media, text, id, buttons) {
  if (missing(media) || !is.character(media) || !nzchar(media)) {
    cli::cli_abort("`media` must be a non-empty string (URL or base64 data URI).")
  }

  if (missing(text) || !is.character(text) || !nzchar(text)) {
    cli::cli_abort("`text` must be a non-empty string.")
  }

  if (missing(id) || !is.character(id) || !nzchar(id)) {
    cli::cli_abort("`id` must be a non-empty string.")
  }

  if (missing(buttons) || !is.list(buttons) || length(buttons) < 1) {
    cli::cli_abort("`buttons` must be a non-empty list of button objects.")
  }

  list(
    media   = list(media = media),
    text    = text,
    id      = id,
    buttons = buttons
  )
}


#' Create a URL button for carousel
#'
#' @description
#' Helper to create a URL button for carousel cards.
#'
#' @param title Character(1). Button text.
#' @param id Character(1). Button identifier.
#' @param url Character(1). URL to open when clicked.
#'
#' @return A named list representing a URL button.
#'
#' @examples
#' btn <- whapi_btn_url("Visit Site", "btn-1", "https://example.com")
#'
#' @export
whapi_btn_url <- function(title, id, url) {
  list(type = "url", title = title, id = id, url = url)
}


#' Create a Quick Reply button for carousel
#'
#' @description
#' Helper to create a quick reply button for carousel cards.
#'
#' @param title Character(1). Button text.
#' @param id Character(1). Button identifier.
#'
#' @return A named list representing a quick reply button.
#'
#' @examples
#' btn <- whapi_btn_quick_reply("More Info", "btn-1")
#'
#' @export
whapi_btn_quick_reply <- function(title, id) {
  list(type = "quick_reply", title = title, id = id)
}


#' Create a Call button for carousel
#'
#' @description
#' Helper to create a call button for carousel cards.
#'
#' @param title Character(1). Button text.
#' @param id Character(1). Button identifier.
#' @param phone_number Character(1). Phone number to call.
#'
#' @return A named list representing a call button.
#'
#' @examples
#' btn <- whapi_btn_call("Call Us", "btn-1", "5581999999999")
#'
#' @export
whapi_btn_call <- function(title, id, phone_number) {
  list(type = "call", title = title, id = id, phone_number = phone_number)
}


#' Create a Copy button for carousel
#'
#' @description
#' Helper to create a copy button for carousel cards.
#'
#' @param title Character(1). Button text.
#' @param id Character(1). Button identifier.
#' @param copy_code Character(1). Text to copy to clipboard.
#'
#' @return A named list representing a copy button.
#'
#' @examples
#' btn <- whapi_btn_copy("Copy Code", "btn-1", "DISCOUNT20")
#'
#' @export
whapi_btn_copy <- function(title, id, copy_code) {
  list(type = "copy", title = title, id = id, copy_code = copy_code)
}
