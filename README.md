
<!-- README.md is generated from README.Rmd. Please edit that file -->

# whapi <a href="https://github.com/StrategicProjects/whapi"><img src="man/figures/logo.png" align="right" height="106" alt="whapi website" /></a>

<!-- badges: start -->

![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/whapi) 
![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/whapi) 
![License](https://img.shields.io/badge/license-MIT-darkviolet.svg) 
![](https://img.shields.io/badge/devel%20version-0.0.96-orangered.svg)

<!-- badges: end -->

> R wrapper for [whapi.cloud](https://whapi.cloud) — a lightweight
> WhatsApp API.

### Overview

The **whapi** package provides a tidyverse-friendly interface to the
[whapi.cloud](https://whapi.cloud) API, allowing you to send, receive,
and manage WhatsApp messages programmatically in R.

Features include:

- Sending **text, image, document, sticker, and location** messages;
- Sending **interactive messages** (quick replies, mixed actions,
  lists);
- Reacting to or marking messages as read;
- Fetching message details and contact profiles;
- Checking channel health and status;
- Helpers for logging and webhook flattening.

All outputs are returned as **tibbles** for easy integration into
tidyverse workflows.

### Installation

The package is under development. Install from source:

``` r
# install.packages("devtools")
devtools::install_github("StrategicProjects/whapi")
```

### Setup

1.  Get an API token from [Whapi.Cloud](https://whapi.cloud).
2.  Store it as an environment variable:

``` r
Sys.setenv(WHAPI_TOKEN = "your_api_token_here")
```

3.  Load the package:

``` r
library(whapi)
```

### Example Usage

#### Send a Text Message

``` r
whapi_send_text("5581999999999", "Hello from R! ✅")
```

#### Send an Image

``` r
whapi_send_image("5581999999999", image = "card.png", type = "file", caption = "Card")
```

#### Send a Sticker

``` r
whapi_send_sticker("5581999999999", sticker = "sticker.webp", type = "file")
```

#### Send an Interactive Quick Reply

``` r
whapi_send_quick_reply(
  to = "5581999999999",
  body_text = "Do you confirm?",
  buttons = tibble::tribble(~title, "YES", "NO")
)
```

#### React to a Message

``` r
whapi_react_to_message("PsqXn5SAD5v7HRA-wHqB9tMeGQ", "❤️")
```

#### Mark a Message as Read

``` r
whapi_mark_message_read("PsqXn5SAD5v7HRA-wHqB9tMeGQ")
```

#### Get a Contact Profile

``` r
whapi_get_contact_profile("5581999999999")
```

#### Check Channel Health

``` r
whapi_check_health()
```

------------------------------------------------------------------------

### Contributing

If you find any issues or have feature requests, feel free to create an
issue or a pull request on
[GitHub](https://github.com/StrategicProjects/whapi).

------------------------------------------------------------------------

### License

This package is licensed under the MIT License. See the `LICENSE` file
for more details.
