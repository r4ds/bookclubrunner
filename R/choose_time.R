#' Pick a Convenient Time
#'
#' Find the times with the most votes.
#'
#' @param book_name The abbreviation for an approved book, such as rpkgs or
#'   mshiny.
#' @param facilitator_id The Slack ID of the facilitator for this group.
#'   Available via the user's "full profile" in Slack.
#' @param required_choosers The minimum number of sign-ups necessary for the
#'   club to launch. Think really hard before changing this.
#'
#' @return The best times, as a tibble.
#' @export
choose_time <- function(book_name,
                        facilitator_id,
                        required_choosers = 5) {
  all_times <- .load_book_signups(book_name)
  valid_times <- .choose_time_summarize(
    .choose_time_valid(all_times, facilitator_id)
  )
  .choose_time_inform(
    valid_times,
    all_times,
    required_choosers,
    facilitator_id,
    book_name
  )

  return(invisible(valid_times))
}

.load_book_signups <- function(book_name) {
  dplyr::filter(
    bookclubdata::signups_read(book_name, refresh = TRUE),
    !(.data$datetime_utc %in% .unavailable_times()$unavailable_time)
  )
}

.unavailable_times <- function() {
  club_times <- bookclubdata::active_clubs_times(TRUE) |>
    dplyr::transmute(
      date_utc = bookclubdata::make_datetimes_utc(
        days = .data$day_utc,
        hours = .data$hour_utc,
        timezones = "UTC"
      ),
      next_hour = .data$date_utc + lubridate::hours(1),
      prev_hour = .data$date_utc - lubridate::hours(1)
    ) |>
    tidyr::pivot_longer(
      tidyr::everything(),
      values_to = "unavailable_time"
    ) |>
    dplyr::distinct(.data$unavailable_time) |>
    dplyr::arrange(.data$unavailable_time)

  return(club_times)
}

.choose_time_summarize <- function(valid_times) {
  dplyr::arrange(
    dplyr::summarize(
      valid_times,
      n = dplyr::n(),
      users = glue::glue_collapse(
        sort(unique(glue::glue("@{.data$user_name}"))), sep = ", "
      ),
      .by = "datetime_utc"
    ),
    dplyr::desc(.data$n)
  )
}

.choose_time_valid <- function(df, facilitator_id) {
  dplyr::filter(
    df,
    .data$datetime_utc %in% .extract_facilitator_times(df, facilitator_id)
  )
}

.warn_facilitator_minutes <- function(df, facilitator_id) {
  facilitator_minutes <- .calculate_facilitator_minutes(df, facilitator_id)
  if (facilitator_minutes > 0) {
    cli::cli_warn(
      "Facilitator timezone has a {facilitator_minutes} minute offset."
    )
  }
}

.calculate_facilitator_minutes <- function(df, facilitator_id) {
  bookclubdata::tz_minutes(
    .extract_facilitator_timezone(df, facilitator_id)
  )
}

.extract_facilitator_times <- function(df, facilitator_id) {
  facilitator_times <- df$datetime_utc[df$user_id == facilitator_id]
  .assert_facilitator_signup(facilitator_times)
  .warn_facilitator_minutes(df, facilitator_id)
  return(facilitator_times)
}

.extract_facilitator_timezone <- function(df, facilitator_id) {
  df$timezone[df$user_id == facilitator_id][[1]]
}

.assert_facilitator_signup <- function(facilitator_times) {
  if (!length(facilitator_times)) {
    cli::cli_abort("Facilitator must sign up before times can be chosen.")
  }
}

.choose_time_inform <- function(valid_times,
                                all_times,
                                required_choosers,
                                facilitator_id,
                                book_name) {
  if (max(valid_times$n) < required_choosers) {
    .choose_time_inform_too_few(
      all_times,
      required_choosers,
      facilitator_id,
      book_name
    )
  } else {
    .choose_time_inform_facilitator(
      valid_times,
      all_times,
      required_choosers,
      facilitator_id
    )
  }
}

.choose_time_inform_too_few <- function(all_times,
                                        required_choosers,
                                        facilitator_id,
                                        book_name) {
  withr::local_options(cli.condition_width = Inf)
  user_tags <- paste0("@", sort(unique(all_times$user_name)))
  starter_msg <- glue::glue(
    ": You have all indicated interest in joining this cohort to read {book_name}, but we",
    "don't yet have enough people with overlappping schedules to launch.",
    .sep = " "
  )
  general_message <- "@here: If you're in this channel, you must have some interest in joining a cohort."
  book_esc <- utils::URLencode(book_name)
  book_url <- glue::glue("https://r4ds.io/bookclubber?bookname={book_esc}")
  after_app <- glue::glue(
    "especially any green times (the shading indicates how many people have chosen that time).",
    "I'll check again next Monday to see if we are ready to launch!",
    .sep = " "
  )
  for_me <- glue::glue("choose_time(\"{book_name}\", \"{facilitator_id}\")")
  cli::cli_inform(
    c(
      "!" = "No times with {required_choosers}+ users.",
      "",
      "{user_tags}{starter_msg}",
      "",
      "{general_message} Please check the app ({book_url}) -- {after_app}",
      "",
      "For me: This is the code to check this club again:",
      "{.code {for_me}}"
    )
  )
}

.choose_time_inform_facilitator <- function(valid_times,
                                            all_times,
                                            required_choosers,
                                            facilitator_id) {
  clean_times <- valid_times |>
    dplyr::filter(.data$n >= required_choosers) |>
    dplyr::mutate(
      datetime_r4ds = lubridate::with_tz(
        .data$datetime_utc, "America/Chicago"
      ),
      datetime_facilitator = lubridate::with_tz(
        .data$datetime_utc,
        .extract_facilitator_timezone(all_times, facilitator_id)
      ),
      day_you = lubridate::wday(
        .data$datetime_facilitator, label = TRUE, abbr = FALSE
      ),
      hour_you = lubridate::hour(.data$datetime_facilitator),
      day_r4ds = lubridate::wday(
        .data$datetime_r4ds, label = TRUE, abbr = FALSE
      ),
      hour_r4ds = lubridate::hour(.data$datetime_r4ds)
    ) |>
    dplyr::select(
      "n",
      "day_you",
      "hour_you",
      "day_r4ds",
      "hour_r4ds",
      "users"
    )

  minutes_you <- stringr::str_pad(
    .calculate_facilitator_minutes(all_times, facilitator_id),
    2,
    pad = "0"
  )

  cli::cli_bullets(
    glue::glue_data(
      clean_times,
      "{n} people: {day_you}s {hour_you}:{minutes_you} ({users})",
      "({day_r4ds}s {hour_r4ds}:00 R4DS time)",
      .sep = "\n"
    )
  )
}
