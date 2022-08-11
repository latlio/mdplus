# Match mentors and mentees, MD+ Pilot
# Developer: Lathan Liou (lathan.liou@icahn.mssm.edu)

library(MatchIt)
library(dplyr)
library(purrr)
library(googlesheets4)
library(blastula)
library(glue)
library(gt)

# Global Vars ----
MENTOR_MENTEE_RATIO <- 3

# Functions ----
create_smtp_creds_key(
  id = "mdplus_gmail",
  user = "lathan@mdplusplus.org",
  provider = "gmail"
)

# hufzkokwjdqknvle

# prepare_test_message() %>%
#   smtp_send(
#     to = "lathanliu21@gmail.com",
#     from = "lathan@mdplusplus.org",
#     subject = "Testing the `smtp_send()` function",
#     credentials = creds_key("mdplus_gmail")
#   )

send_admin_email <- function(ratio,
                             residency_match_results,
                             industry_match_results,
                             unique_mentors,
                             unique_mentees,
                             insufficient_residency,
                             insufficient_industry,
                             unmatched_mentors,
                             unmatched_mentees,
                             admin_emails = c("lathan@mdplusplus.org",
                                              "sahit.menon@gmail.com",
                                              "sherman@alleycorp.com")) {
  #' unique_ indicates that mentor/mentee noted a specific type of mentorship
  #' insufficient_ indicates that mentor/mentee did not provide enough info to
  #' succesffully match

  time <- Sys.Date()

  n_success_mentor <- nrow(residency_match_results %>% filter(type == 1)) +
    nrow(industry_match_results %>% filter(type == 1))

  n_success_mentee <- nrow(residency_match_results %>% filter(type == 0)) +
    nrow(industry_match_results %>% filter(type == 0))

  n_nomatch_mentor <- nrow(unmatched_mentors)

  n_nomatch_mentee <- nrow(unmatched_mentees)

  residency_match_tbl <- residency_match_results %>%
    # select(-c(weights, timestamp, distance)) %>%
    gt(rowname_col = "id") %>%
    tab_header(
      title = "Residency-only matches",
      subtitle = "Subclass defines 1:X match"
    ) %>%
    as_raw_html()

  industry_match_tbl <- industry_match_results %>%
    select(-c(id, weights, timestamp, distance)) %>%
    gt(rowname_col = "id") %>%
    tab_header(
      title = "Industry matches",
      subtitle = "Subclass defines 1:X match"
    ) %>%
    as_raw_html()

  unique_mentors_tbl <- unique_mentors %>%
    select(-c(timestamp, type)) %>%
    gt(rowname_col = "id") %>%
    tab_header(
      title = "Unique Mentors",
      subtitle = "Need to be manually matched"
    ) %>%
    as_raw_html()

  unique_mentees_tbl <- unique_mentees %>%
    select(-c(timestamp, type)) %>%
    gt(rowname_col = "id") %>%
    tab_header(
      title = "Unique Mentees",
      subtitle = "Need to be manually matched"
    ) %>%
    as_raw_html()

  insufficient_residency_tbl <- insufficient_residency %>%
    select(-c(timestamp, type)) %>%
    gt(rowname_col = "id") %>%
    tab_header(
      title = "Insufficient Info (Residency)",
      subtitle = "Need to be manually matched"
    ) %>%
    as_raw_html()

  insufficient_industry_tbl <- insufficient_industry %>%
    select(-c(timestamp, type)) %>%
    gt(rowname_col = "id") %>%
    tab_header(
      title = "Insufficient Info (Industry)",
      subtitle = "Need to be manually matched"
    ) %>%
    as_raw_html()

  unmatched_mentors_tbl <- unmatched_mentors %>%
    select(-c(timestamp, type)) %>%
    gt(rowname_col = "id") %>%
    tab_header(
      title = "All unmatched mentors",
      subtitle = "Need to be manually matched"
    ) %>%
    as_raw_html()

  unmatched_mentees_tbl <- unmatched_mentees %>%
    select(-c(timestamp, type)) %>%
    gt(rowname_col = "id") %>%
    tab_header(
      title = "All unmatched mentees",
      subtitle = "Need to be manually matched"
    ) %>%
    as_raw_html()

  body_text <- blastula::md(glue(
    "Hey Admin Team,

The match results ran on {time} for a mentee:mentor ratio of {ratio} are below.

type == 1 refers to a mentor (0 = mentee).

Capacity (1 = Regular, 2 = once a month, 3 = once every few months, 4 = one-off).


# Quick Stats

* Matched {n_success_mentor} mentors and {n_success_mentee} mentees
* Did not match {n_nomatch_mentor} mentors and {n_nomatch_mentee} mentees


# Non-matched mentors:

{unmatched_mentors_tbl}


# Non-matched mentees:

{unmatched_mentees_tbl}


# Specific mentorship desired (and needs to be matched manually):

{unique_mentors_tbl}

{unique_mentees_tbl}


# Insufficient info provided (and needs to be matched manually):

{insufficient_residency_tbl}

{insufficient_industry_tbl}


# Successful matches:

{residency_match_tbl}

{industry_match_tbl}


Thanks,

Lathan"
  ))
  body_text %>%
    compose_email() %>%
    smtp_send(
      from = "lathan@mdplusplus.org",
      to = admin_emails,
      subject = "MD+ Matching Results Admin Report",
      credentials = creds_key(id = "mdplus_gmail")
    )
}

draft_email <- function(mentor_name,
                        mentor_school,
                        mentor_role,
                        mentee_name,
                        mentee_school,
                        mentee_role,
                        shared_area) {
  body_text <- blastula::md(glue(
"Hello {mentor_name} and {mentee_name},

Congrats! You both have been matched through our pilot matching initiative based on
your mentorship goals, areas of interest ({shared_area}), specialty, and commitment level.
{mentor_name} is a {mentor_role} at {mentor_school}. {mentee_name} is a {mentee_role} at {mentee_school}.
For our pilot program, we encourage you to set up a time to meet in the next month!
Our hope is to send out a survey to pilot participants after their initial meeting
to evaluate and improve on the MD+ mentorship program. Thank you for participating!

Best,

MD+"
  ))
  body_text
}

send_match_emails <- function(match_df_subclass) {
  mentor <- match_df_subclass %>%
    select(-c(id, subclass, weights)) %>%
    filter(type == 1)

  mentees <- match_df_subclass %>%
    select(-c(id, subclass, weights)) %>%
    filter(type == 0)

  email_texts <- pmap(mentees, ~draft_email(mentor$name,
                                            mentor$medical_school,
                                            mentor$current_role,
                                            ..2, #name
                                            ..4, #medical school
                                            ..5, #role
                                            ..8) %>% #area
                       compose_email(body = .))
  map2(email_texts, mentees$email, ~
         .x %>%
         smtp_send(
           from = "lathan@mdplusplus.org",
           to = c(mentor$email, .y),
           subject = "Your MD+ Mentor-Mentee Match!",
           credentials = creds_key(id = "mdplus_gmail")
  ))
}

# Test this out ----
# test_df <- tibble(subclass = 1,
#                   name = c("Sherman", "Lathan", "Sahit"),
#                   type = c(1, 0, 0),
#                   medical_school = c("Sinai", "Sinai", "UCSD"),
#                   current_role = c("MS4", "MS1", "MS2"),
#                   area1 = c("VC", "Data Science", "VC"),
#                   email = c("sherman@alleycorp.com",
#                             "lathanliu21@gmail.com"
#                             "sahit.menon@gmail.com"))
# send_match_emails(test_df)

# Prep data ----
# Prepare mentor data
raw_mentor_responses <- read_sheet("https://docs.google.com/spreadsheets/d/1S0xUpSjye8MYUqRX5haeBzvU3TJjbK17D8mCbqGwiSs/edit#gid=1690367673",
                                   col_types = "Tcccccccccccccc")

mentor_responses <- raw_mentor_responses %>%
  setNames(c("timestamp",
             "name",
             "email",
             "medical_school",
             "current_role",
             "residency",
             "mentor_focus",
             "area1",
             "area2",
             "specialty1",
             "area1b",
             "area2b",
             "specialty1b",
             "capacity",
             "notes")) %>%
  mutate(area1 = coalesce(area1, area1b),
         area2 = coalesce(area2, area2b),
         specialty = coalesce(specialty1, specialty1b)) %>%
  mutate(area1 = coalesce(area1, area2),
         area2 = coalesce(area1, area2)) %>%
  select(-c(area1b, area2b, specialty1, specialty1b)) %>%
  mutate(specialty = case_when(
    specialty == "Radiology" | specialty == "Interventional Radiology" ~ "Radiology/Interventional",
    TRUE ~ specialty
  )) %>%
  mutate(type = "mentor",
         area1 = as.factor(case_when(
           area1 == "Data Science" ~ 1,
           area1 == "Digital Health" ~ 2,
           area1 == "AI/ML" ~ 3,
           area1 == "Clinical Trials" ~ 4,
           area1 == "Genetics/Gene Therapy" ~ 5,
           area1 == "Pharmacology" ~ 6,
           area1 == "Venture Capital" ~ 7,
           area1 == "Healthcare Consulting" ~ 8,
           area1 == "Wearable" ~ 9)),
         area2 = as.factor(case_when(
           area2 == "Data Science" ~ 1,
           area2 == "Digital Health" ~ 2,
           area2 == "AI/ML" ~ 3,
           area2 == "Clinical Trials" ~ 4,
           area2 == "Genetics/Gene Therapy" ~ 5,
           area2 == "Pharmacology" ~ 6,
           area2 == "Venture Capital" ~ 7,
           area2 == "Healthcare Consulting" ~ 8,
           area2 == "Wearable" ~ 9)),
         specialty = as.factor(case_when(
           specialty == "Family Medicine" ~ 1,
           specialty == "Internal Medicine" ~ 2,
           specialty == "Pediatrics" ~ 3,
           specialty == "Emergency Medicine" ~ 4,
           specialty == "General Surgery" ~ 5,
           specialty == "Radiology/Interventional" ~ 6,
           specialty == "OB/GYN" ~ 7,
           specialty == "Psychiatry" ~ 8,
           specialty == "Neurology" ~ 9,
           specialty == "Anesthesiology" ~ 10)),
         capacity = as.factor(case_when(
           capacity == "Twice a month (i.e regular check-ins for skills development)" ~ 1,
           capacity == "Once a month (i.e longitudinal mentorship with monthly time blocked off)" ~ 2,
           capacity == "Once every few months (i.e checking in as needed without a specific request)" ~ 3,
           capacity == "One time session (i.e answering a specific time-sensitive question for an interview)" ~ 4
         ))) %>%
  relocate(specialty, .after = "area2")

## any unique mentor_focus for manual inspection?
(unique_mentors <- mentor_responses %>%
  filter(!mentor_focus %in% c("Advice on industry AND residency",
                              "Advice on pursuing residency/specialty",
                              "Advice on pursuing an industry of interest (or specific area of healthcare)")))

raw_mentee_responses <- read_sheet("https://docs.google.com/spreadsheets/d/1LzvDoCbEC28OCyCXpE86xZVWIp7fIp1UTPNTJWovoFk/edit#gid=2103126388",
                                   col_types = "Tcccccccccccccccccc")

mentee_responses <- raw_mentee_responses %>%
  setNames(c("timestamp",
             "name",
             "email",
             "medical_school",
             "current_role",
             "residency",
             "mentor_focus",
             "area1",
             "area2",
             "specialty1",
             "specialty2",
             "specialty3",
             "area1b",
             "area2b",
             "specialty1b",
             "specialty2b",
             "specialty3b",
             "capacity",
             "notes")) %>%
  mutate(area1 = coalesce(area1, area1b),
         area2 = coalesce(area2, area2b),
         specialty = coalesce(specialty1, specialty1b),
         specialty2 = coalesce(specialty2, specialty2b),
         specialty3 = coalesce(specialty3, specialty3b)) %>%
  mutate(area1 = coalesce(area1, area2),
         area2 = coalesce(area1, area2)) %>%
  select(-c(area1b, area2b, specialty1, specialty1b, specialty2b, specialty3b)) %>%
  select(-c(specialty2, specialty3)) %>% #temp measure
  mutate(specialty = case_when(
    specialty == "Radiology" | specialty == "Interventional Radiology" ~ "Radiology/Interventional",
    is.na(specialty) ~ "Unknown",
    TRUE ~ specialty
  )) %>%
  mutate(type = "mentee",
         area1 = as.factor(case_when(
           area1 == "Data Science" ~ 1,
           area1 == "Digital Health" ~ 2,
           area1 == "AI/ML" ~ 3,
           area1 == "Clinical Trials" ~ 4,
           area1 == "Genetics/Gene Therapy" ~ 5,
           area1 == "Pharmacology" ~ 6,
           area1 == "Venture Capital" ~ 7,
           area1 == "Healthcare Consulting" ~ 8,
           area1 == "Wearable" ~ 9)),
         area2 = as.factor(case_when(
           area2 == "Data Science" ~ 1,
           area2 == "Digital Health" ~ 2,
           area2 == "AI/ML" ~ 3,
           area2 == "Clinical Trials" ~ 4,
           area2 == "Genetics/Gene Therapy" ~ 5,
           area2 == "Pharmacology" ~ 6,
           area2 == "Venture Capital" ~ 7,
           area2 == "Healthcare Consulting" ~ 8,
           area2 == "Wearable" ~ 9)),
         specialty = as.factor(case_when(
           specialty == "Family Medicine" ~ 1,
           specialty == "Internal Medicine" ~ 2,
           specialty == "Pediatrics" ~ 3,
           specialty == "Emergency Medicine" ~ 4,
           specialty == "General Surgery" ~ 5,
           specialty == "Radiology/Interventional" ~ 6,
           specialty == "OB/GYN" ~ 7,
           specialty == "Psychiatry" ~ 8,
           specialty == "Neurology" ~ 9,
           specialty == "Anesthesiology" ~ 10)),
         capacity = as.factor(case_when(
           capacity == "Twice a month (i.e regular check-ins for skills development)" ~ 1,
           capacity == "Once a month (i.e longitudinal mentorship with monthly time blocked off)" ~ 2,
           capacity == "Once every few months (i.e checking in as needed without a specific request)" ~ 3,
           capacity == "One time session (i.e answering a specific time-sensitive question for an interview)" ~ 4
         ))) %>%
  relocate(specialty, .after = "area2")

## any unique mentor_focus for manual inspection?
(unique_mentees <- mentee_responses %>%
  filter(!mentor_focus %in% c("Advice on industry AND residency",
                              "Advice on pursuing an industry of interest (or specific area of healthcare)",
                              "Advice on pursuing residency/specialty")))

mentor_mentee_df <- bind_rows(mentor_responses, mentee_responses) %>%
  mutate(type = case_when(
    type == "mentee" ~ 0,
    type == "mentor" ~ 1
  ))

residency_only <- mentor_mentee_df %>%
  filter(mentor_focus == "Advice on pursuing residency/specialty")

# who didn't provide enough info and needs to be matched manually
(no_info_residency <- residency_only %>%
  filter(is.na(specialty)))

industry_df <- mentor_mentee_df %>%
  filter(mentor_focus %in% c("Advice on industry AND residency",
                             "Advice on pursuing an industry of interest (or specific area of healthcare)"))

# who didn't provide enough info and needs to be matched manually
(no_info_industry <- industry_df %>%
    filter(is.na(area1)))

# Match ----
residency_matches <- matchit(type ~ specialty + capacity, data = residency_only %>%
                               filter(!name %in% c(no_info_residency$name)),
                             distance = "mahalanobis")
residency_match_df <- get_matches(residency_matches) #not producing sensible results

residency_match_df <- residency_only %>%
  filter(name %in% c("Ann Pongsakul",
                     "Pranati Movva",
                     "Victoria Kusztos",
                     "Kylie Long")) %>%
  mutate(subclass = c(1,2,1,2))

industry_matches <- matchit(type ~ capacity + area1 + area2, data = industry_df %>%
                     filter(!name %in% c(no_info_industry$name)), ratio = MENTOR_MENTEE_RATIO)
industry_match_df <- get_matches(industry_matches)

unmatched_mentors <- mentor_responses %>%
  filter(name %in% setdiff(mentor_responses$name, c(residency_match_df$name,
                                                    industry_match_df$name)))

unmatched_mentees <- mentee_responses %>%
  filter(name %in% setdiff(mentee_responses$name, c(residency_match_df$name,
                                                    industry_match_df$name)))

# Notify ----
send_admin_email(MENTOR_MENTEE_RATIO,
                 residency_match_df,
                 industry_match_df,
                 unique_mentors,
                 unique_mentees,
                 no_info_residency,
                 no_info_industry,
                 unmatched_mentors,
                 unmatched_mentees)

map(unique(residency_match_df$subclass), ~ residency_match_df %>%
      filter(subclass == .x) %>%
      send_match_emails())

map(unique(industry_match_df$subclass), ~ industry_match_df %>%
      filter(subclass == .x) %>%
      send_match_emails())


