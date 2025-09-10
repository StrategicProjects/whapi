# utils::globalVariables for NSE bindings
utils::globalVariables(c(
  # whapi_build_servicos_sections
  "id", "section", "ord",

  # whapi_flatten_webhook
  "body_before", "body_after", "edited_body",
  "reply_title", "reply_description",
  "event_action", "change",
  "chat_id", "from", "from_name", "from_me", "msg_type"
))
