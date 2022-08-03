# Match mentors and mentees, MD+ Pilot
# Developer: Lathan Liou (lathan.liou@icahn.mssm.edu)

library(MatchIt)
library(dplyr)
library(purrr)
library(googlesheets4)
library(blastula)
library(glue)

# Global Vars ----
MENTOR_MENTEE_RATIO <- 3

# Functions ----
create_smtp_creds_key(
  id = "mdplus_gmail",
  user = "lathan@mdplusplus.org",
  provider = "gmail"
)

# hufzkokwjdqknvle

prepare_test_message() %>%
  smtp_send(
    to = "lathanliu21@gmail.com",
    from = "lathan@mdplusplus.org",
    subject = "Testing the `smtp_send()` function",
    credentials = creds_key("mdplus_gmail")
  )

send_admin_email <- function() {
}

generate_admin_report <- function() {

}

draft_email <- function(mentor_name,
                        mentor_school,
                        mentor_role,
                        mentee_name,
                        mentee_school,
                        mentee_role,
                        shared_area) {
  body_text <- md(glue(
"Hello {mentor_name} and {mentee_name},

Congrats! You both have been matched through our pilot matching initiative based on your mentorship goals, areas of interest ({shared_area}), specialty, and commitment level. {mentor_name} is a {mentor_role} at {mentor_school}. {mentee_name} is a {mentee_role} at {mentee_school}. Please feel free to reach out to one another to set up a time to connect!

Best,

MD+"
  ))
  body_text
}

send_match_emails <- function(match_df_subclass) {
  mentor <- match_df_subclass %>%
    filter(type == 1)

  mentees <- match_df_subclass %>%
    filter(type == 0)

  email_texts <- pmap(mentees, ~draft_email(mentor$name,
                                            mentor$medical_school,
                                            mentor$current_role,
                                            ..2, #name
                                            ..4, #medical school
                                            ..5, #role
                                            ..6) %>% #area
                       compose_email(body = .))
  map2(email_texts, mentees$email, ~
         .x %>%
         smtp_send(
           from = "lathan@mdplusplus.org",
           to = c(mentor$email, .y),
           subject = "[TEST] Your MD+ Mentor-Mentee Match!",
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
raw_mentor_responses <- read_sheet("https://docs.google.com/spreadsheets/d/1S0xUpSjye8MYUqRX5haeBzvU3TJjbK17D8mCbqGwiSs/edit#gid=1690367673")

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
  mutate(type = "mentor",
         area1 = factor(area1, levels = c("Data Science",
                                          "Digital Health",
                                          "AI/ML",
                                          "Clinical Trials",
                                          "Genetics/Gene Therapy",
                                          "Pharmacology",
                                          "Venture Capital",
                                          "Healthcare Consulting",
                                          "Wearable")),
         area2 = factor(area2, levels = c("Data Science",
                                          "Digital Health",
                                          "AI/ML",
                                          "Clinical Trials",
                                          "Genetics/Gene Therapy",
                                          "Pharmacology",
                                          "Venture Capital",
                                          "Healthcare Consulting",
                                          "Wearable")),
         specialty = factor(specialty, levels = c("Family Medicine",
                                                  "Internal Medicine",
                                                  "Pediatrics",
                                                  "Emergency Medicine",
                                                  "General Surgery",
                                                  "Radiology",
                                                  "Interventional Radiology",
                                                  "OB/GYN",
                                                  "Psychiatry",
                                                  "Neurology")),
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
                              "Advice on pursuing residency/specialty")))

raw_mentee_responses <- read_sheet("https://docs.google.com/spreadsheets/d/1LzvDoCbEC28OCyCXpE86xZVWIp7fIp1UTPNTJWovoFk/edit#gid=2103126388")

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
    is.na(specialty) ~ "Unknown",
    TRUE ~ specialty
  )) %>%
  mutate(type = "mentee",
         area1 = factor(area1, levels = c("Data Science",
                                          "Digital Health",
                                          "AI/ML",
                                          "Clinical Trials",
                                          "Genetics/Gene Therapy",
                                          "Pharmacology",
                                          "Venture Capital",
                                          "Healthcare Consulting",
                                          "Wearable")),
         area2 = factor(area2, levels = c("Data Science",
                                          "Digital Health",
                                          "AI/ML",
                                          "Clinical Trials",
                                          "Genetics/Gene Therapy",
                                          "Pharmacology",
                                          "Venture Capital",
                                          "Healthcare Consulting",
                                          "Wearable")),
         specialty = factor(specialty, levels = c("Family Medicine",
                                                  "Internal Medicine",
                                                  "Pediatrics",
                                                  "Emergency Medicine",
                                                  "General Surgery",
                                                  "Radiology",
                                                  "Interventional Radiology",
                                                  "OB/GYN",
                                                  "Psychiatry",
                                                  "Neurology",
                                                  "Unknown")),
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
residency_matches <- matchit(type ~ capacity + specialty, data = residency_only %>%
                               filter(!name %in% c(no_info_residency$name)), ratio = MENTOR_MENTEE_RATIO)
residency_match_df <- get_matches(residency_matches)

industry_matches <- matchit(type ~ capacity + area1 + area2, data = industry_df %>%
                     filter(!name %in% c(no_info_industry$name)), ratio = MENTOR_MENTEE_RATIO)
industry_match_df <- get_matches(industry_matches)

# Notify ----
map(unique(residency_match_df$subclass), ~ residency_match_df %>%
      filter(subclass == .x) %>%
      send_match_emails())

map(unique(industry_match_df$subclass), ~ industry_match_df %>%
      filter(subclass == .x) %>%
      send_match_emails())
