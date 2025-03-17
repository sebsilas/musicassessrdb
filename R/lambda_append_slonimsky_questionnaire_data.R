


append_slonimsky_questionnaire_data <- function(user_id,
                                                session_id,
                                                questionnaire_data) {

  response <- tryCatch({

    logging::loginfo('user_id: %s', user_id)
    logging::loginfo('session_id: %s', session_id)
    logging::loginfo('questionnaire_data: %s', questionnaire_data)

    # Append session
    questionnaire_data <- jsonlite::fromJSON() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(user_id = user_id,
                    session_id = session_id,
                    completion_time = Sys.time() ) %>%
      dplyr::relocate(user_id, session_id, completion_time)


    DBI::dbWriteTable(db_con, "slonimsky_questionnaire_responses", questionnaire_data, row.names = FALSE, append = TRUE)

    # Return response

    list(
      status = 200,
      message = "You have successfully added a Slonimsky questionnaire response!"
    )


  }, error = function(err) {
    logging::logerror(err)
    list(
      status = 400,
      message = "Something went wrong"
    )
  })


}




# Init table for storing *questionnaire info*

# slonimsky_questionnaire_info <- '{
#   "sections": {
#     "playing_by_ear": {
#       "questions": [
#           {
#             "id": "PBE_01",
#             "key": "regular_practice",
#             "text": "I regularly practise playing music by ear without using sheet music or tablature.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5]
#           },
#           {
#             "id": "PBE_02",
#             "key": "sing_before_playing",
#             "text": "Before playing by ear, I first try to sing or hum a melody to internalise it.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5]
#           },
#           {
#             "id": "PBE_03",
#             "key": "learning_resources",
#             "text": "I use learning resources such as ear-training apps, CDs, instructional books, or computer programs to improve my ability to play by ear.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5]
#           },
#           {
#             "id": "PBE_04",
#             "key": "ear_training_exercises",
#             "text": "I engage in structured ear training exercises, such as interval identification or melody recall, to improve my musical perception.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5]
#           },
#           {
#             "id": "PBE_05",
#             "key": "playing_with_recordings",
#             "text": "I attempt to play along with recordings to reproduce melodies and chord progressions by ear.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5]
#           },
#           {
#             "id": "PBE_06",
#             "key": "transcribe_music_by_ear",
#             "text": "I actively transcribe music (melodies, chord progressions, basslines) by ear to improve my listening skills.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5]
#           },
#           {
#             "id": "PBE_07",
#             "key": "formal_training",
#             "text": "I have received formal or structured training focused on playing by ear.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5]
#           },
#           {
#             "id": "PBE_08",
#             "key": "improvisation",
#             "text": "When improvising, I primarily rely on what I hear rather than written notation, but I can also integrate notation when needed.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5]
#           },
#           {
#             "id": "PBE_09",
#             "key": "dedicated_practice",
#             "text": "I spend dedicated practice time figuring out melodies, harmonies, or rhythms purely by ear.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5]
#           },
#           {
#             "id": "PBE_10",
#             "key": "avoid_playing_by_ear",
#             "text": "I avoid playing by ear because I find it frustrating or too difficult.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5],
#             "negative_coding": true
#           },
#           {
#             "id": "PBE_11",
#             "key": "first_teacher_emphasis",
#             "text": "My first teacher has emphasized playing by ear and improvisation.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5]
#           },
#           {
#             "id": "PBE_12",
#             "key": "play_melody_by_ear",
#             "text": "I can accurately play a melody on my instrument after hearing it once or twice.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5]
#           },
#           {
#             "id": "PBE_13",
#             "key": "chord_progressions_by_ear",
#             "text": "I can recognise and reproduce common chord progressions by ear without relying on notation.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5]
#           },
#           {
#             "id": "PBE_14",
#             "key": "identify_key_by_ear",
#             "text": "I can determine the key of a song by listening to it.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5]
#           },
#           {
#             "id": "PBE_15",
#             "key": "replicate_notes_by_ear",
#             "text": "I can hear and accurately replicate individual notes within a chord.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5]
#           },
#           {
#             "id": "PBE_16",
#             "key": "transcribe_melody_by_ear",
#             "text": "I can transcribe an unfamiliar melody by ear with a high level of accuracy.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5]
#           },
#           {
#             "id": "PBE_17",
#             "key": "struggle_playing_in_real_time",
#             "text": "I struggle to follow and play along with a song in real time by ear.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5],
#             "negative_coding": true
#           },
#           {
#             "id": "PBE_18",
#             "key": "difficulty_identifying_harmony",
#             "text": "I often find it difficult to identify harmonic changes in a song just by listening.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5],
#             "negative_coding": true
#           },
#           {
#             "id": "PBE_19",
#             "key": "retain_melody_by_ear",
#             "text": "When learning a new melody, I retain it better if I hear it first rather than reading notation.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5]
#           },
#           {
#             "id": "PBE_20",
#             "key": "weekly_practice_time",
#             "text": "Estimate the amount of time per week you dedicate to practising playing by ear.",
#             "type": "multiple_choice",
#             "options": [
#               "Less than 1 hour",
#               "1–2 hours",
#               "2–3 hours",
#               "More than 3 hours"
#             ]
#           },
#           {
#             "id": "PBE_21",
#             "key": "years_learning_by_ear",
#             "text": "How many years have you been actively learning to play music by ear?",
#             "type": "multiple_choice",
#             "options": [
#               "Less than 1 year",
#               "1–3 years",
#               "4–6 years",
#               "More than 6 years"
#             ]
#           },
#           {
#             "id": "PBE_22",
#             "key": "natural_ability_playing_by_ear",
#             "text": "I have always been good at playing music by ear and improvising.",
#             "type": "likert",
#             "scale": [1, 2, 3, 4, 5]
#           }
#         ]
#       },
#     "music_training": {
#       "questions": [
#         {
#           "id": "MT_03",
#           "key": "never_complimented_talent",
#           "text": "I have never been complimented for my talents as a musical performer.",
#           "type": "likert",
#           "scale": [
#             1,
#             2,
#             3,
#             4,
#             5,
#             6,
#             7
#           ],
#           "negative_coding": true
#         },
#         {
#           "id": "MT_06",
#           "key": "instruments_played",
#           "text": "I can play _ musical instruments.",
#           "type": "multiple_choice",
#           "options": [
#             "0",
#             "1",
#             "2",
#             "3",
#             "4",
#             "5",
#             "6 or more"
#           ]
#         },
#         {
#           "id": "MT_07",
#           "key": "not_consider_musician",
#           "text": "I would not consider myself a musician.",
#           "type": "likert",
#           "scale": [
#             1,
#             2,
#             3,
#             4,
#             5,
#             6,
#             7
#           ],
#           "negative_coding": true
#         }
#       ]
#     },
#     "perceptual_abilities": {
#       "questions": [
#         {
#           "id": "PA_03",
#           "key": "difficulty_spotting_mistakes",
#           "text": "I find it difficult to spot mistakes in a performance of a song even if I know the tune.",
#           "type": "likert",
#           "scale": [
#             1,
#             2,
#             3,
#             4,
#             5,
#             6,
#             7
#           ],
#           "negative_coding": true
#         },
#         {
#           "id": "PA_06",
#           "key": "detect_out_of_time",
#           "text": "I can tell when people sing or play out of time with the beat.",
#           "type": "likert",
#           "scale": [
#             1,
#             2,
#             3,
#             4,
#             5,
#             6,
#             7
#           ]
#         },
#         {
#           "id": "PA_07",
#           "key": "detect_out_of_tune",
#           "text": "I can tell when people sing or play out of tune.",
#           "type": "likert",
#           "scale": [
#             1,
#             2,
#             3,
#             4,
#             5,
#             6,
#             7
#           ]
#         }
#       ]
#     },
#     "singing_abilities": {
#       "questions": [
#         {
#           "id": "SA_03",
#           "key": "hit_right_notes",
#           "text": "I am able to hit the right notes when I sing along with a recording.",
#           "type": "likert",
#           "scale": [
#             1,
#             2,
#             3,
#             4,
#             5,
#             6,
#             7
#           ]
#         },
#         {
#           "id": "SA_04",
#           "key": "cannot_sing_harmony",
#           "text": "I am not able to sing in harmony when somebody is singing a familiar tune.",
#           "type": "likert",
#           "scale": [
#             1,
#             2,
#             3,
#             4,
#             5,
#             6,
#             7
#           ],
#           "negative_coding": true
#         },
#         {
#           "id": "SA_05",
#           "key": "fear_singing_public",
#           "text": "I don\'t like singing in public because I\'m afraid that I would sing wrong notes.",
#           "type": "likert",
#           "scale": [
#             1,
#             2,
#             3,
#             4,
#             5,
#             6,
#             7
#           ],
#           "negative_coding": true
#         }
#       ]
#     },
#     "instrumental_experience": {
#       "questions": [
#         {
#           "id": "BI_01",
#           "key": "best_instrument",
#           "text": "The instrument I play best (including voice) is:",
#           "type": "dropdown"
#         },
#         {
#           "id": "ST_01",
#           "key": "start_playing_age",
#           "text": "What age did you start to play an instrument?",
#           "type": "multiple_choice",
#           "options": [
#             "2",
#             "3",
#             "4",
#             "5",
#             "6",
#             "7",
#             "8",
#             "9",
#             "10",
#             "11",
#             "12",
#             "13",
#             "14",
#             "15",
#             "16",
#             "17",
#             "18",
#             "19",
#             "I don\'t play any instrument"
#           ]
#         }
#       ]
#     },
#     "absolute_pitch": {
#       "questions": [
#         {
#           "id": "AP_01",
#           "key": "absolute_pitch",
#           "text": "Do you have absolute pitch? Absolute or perfect pitch is the ability to recognise and name an isolated musical tone without a reference tone, e.g. being able to say \'F#\' if someone plays that note on the piano.",
#           "type": "yes_no"
#         }
#       ]
#     },
#     "demographics": {
#       "questions": [
#         {
#           "id": "D_01",
#           "key": "dob",
#           "text": "What is your date of birth?",
#           "type": "dob"
#         },
#         {
#           "id": "D_02",
#           "key": "gender",
#           "text": "What is your gender?",
#           "type": "multiple_choice",
#           "options": [
#             "Male",
#             "Female",
#             "Non-binary",
#             "Prefer not to say",
#             "Other"
#           ]
#         },
#         {
#           "id": "D_03",
#           "key": "country",
#           "text": "Which country do you currently reside in?",
#           "type": "multiple_choice",
#           "options": "COUNTRY_LIST"
#         },
#         {
#           "id": "D_04",
#           "key": "native_language",
#           "text": "What is your native language?",
#           "type": "multiple_choice",
#           "options": "ISO_639_3"
#         },
#         {
#           "id": "D_05",
#           "key": "musical_background",
#           "text": "Did you grow up in a musical household (e.g., parents or siblings played an instrument or sang)?",
#           "type": "yes_no"
#         },
#         {
#           "id": "D_06",
#           "key": "formal_music_education",
#           "text": "Have you received formal music education (e.g., private lessons, school music programs, conservatory training)?",
#           "type": "yes_no"
#         }
#       ]
#     }
#   }
# }' %>% jsonlite::fromJSON() %>%
#   unlist(recursive = FALSE) %>%
#   dplyr::bind_rows() %>%
#   tidyr::unnest(questions) %>%
#   dplyr::select(id, key, text, type, negative_coding) %>%
#   dplyr::mutate(negative_coding = dplyr::case_when(is.na(negative_coding) ~ FALSE, TRUE ~ negative_coding))
#
#
# db_con <- musicassessr_con()
#
# DBI::dbWriteTable(db_con, "slonimsky_questionnaire_info", slonimsky_questionnaire_info, row.names = FALSE)
#
#
# db_disconnect(db_con)



# Init table for storing *responses*

# init_tb <- jsonlite::fromJSON(
#   '{
#   "absolute_pitch": "yes",
#   "avoid_playing_by_ear": 3,
#   "best_instrument": "Alto Saxophone",
#   "cannot_sing_harmony": 4,
#   "chord_progressions_by_ear": 3,
#   "country": "Albania",
#   "dedicated_practice": 3,
#   "detect_out_of_time": 4,
#   "detect_out_of_tune": 4,
#   "difficulty_identifying_harmony": 3,
#   "difficulty_spotting_mistakes": 4,
#   "dob": "2025-03-15",
#   "ear_training_exercises": 3,
#   "fear_singing_public": 4,
#   "first_teacher_emphasis": 3,
#   "formal_music_education": "yes",
#   "formal_training": 3,
#   "gender": "Non-binary",
#   "hit_right_notes": 4,
#   "identify_key_by_ear": 3,
#   "improvisation": 3,
#   "instruments_played": "1",
#   "learning_resources": 3,
#   "musical_background": "yes",
#   "native_language": "Mandarin Chinese",
#   "natural_ability_playing_by_ear": 4,
#   "never_complimented_talent": 5,
#   "not_consider_musician": 3,
#   "play_melody_by_ear": 3,
#   "playing_with_recordings": 3,
#   "regular_practice": 3,
#   "replicate_notes_by_ear": 3,
#   "retain_melody_by_ear": 4,
#   "sing_before_playing": 3,
#   "start_playing_age": "7",
#   "struggle_playing_in_real_time": 3,
#   "transcribe_melody_by_ear": 3,
#   "transcribe_music_by_ear": 3,
#   "weekly_practice_time": "1–2 hours",
#   "years_learning_by_ear": "1–3 years"
# }'
# ) %>%
#   tibble::as_tibble() %>%
  # dplyr::mutate(user_id = 1L,
  #               session_id = 1L,
  #               completion_time = Sys.time() ) %>%
  # dplyr::relocate(user_id, session_id, completion_time)


# db_con <- musicassessr_con()
#
# DBI::dbWriteTable(db_con, "slonimsky_questionnaire_responses", init_tb, row.names = FALSE, overwrite = TRUE)
#
#
# db_disconnect(db_con)
