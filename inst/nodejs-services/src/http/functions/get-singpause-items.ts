import { db } from '../../core/rds'

export const handler = async (event) => {
  let result = {}
  const userId = JSON.parse(event.body).user_id
  try {
    result["songs"] = await db`select distinct
    "item_bank_singpause_2024_phrase".song_name,
     "item_bank_singpause_2024_phrase".item_type ,
              STRING_AGG("item_bank_singpause_2024_phrase".item_id, '# ') AS phrases,

    STRING_AGG(COALESCE(score::text, 'null'), '# ') AS scores,
    "item_bank_singpause_2024_item".item_id,
         null  as img
	
     FROM "item_bank_singpause_2024_phrase"
  LEFT JOIN (
    SELECT
      "trial_id",
      "audio_file",
      "instrument",
      "attempt",
      "item_id",
      "display_modality",
      "phase",
      "rhythmic",
      "session_id",
      "test_id",
      "stimulus_abs_melody",
      "stimulus_durations",
      "user_id",
      "psychTestR_session_id",
      "experiment_id",
      "session_time_started",
      "session_time_completed"
    FROM (
      SELECT
        "q01".*,
        RANK() OVER (PARTITION BY "item_id" ORDER BY "session_time_started" DESC) AS "col01"
      FROM (
        SELECT
          "trials".*,
          "user_id",
          "psychTestR_session_id",
          "experiment_id",
          "sessions"."session_time_started" AS "session_time_started",
          "sessions"."session_time_completed" AS "session_time_completed"
        FROM "trials"
        LEFT JOIN "sessions"
          ON ("trials"."session_id" = "sessions"."session_id")
      ) AS "q01"
      WHERE
        ("user_id" = ${userId} and item_id in (select item_id from item_bank_singpause_2024_phrase ibsp))
    ) AS "q01"
    WHERE ("col01" <= 1)
  ) AS "...2"
    ON ("item_bank_singpause_2024_phrase"."item_id" = "...2"."item_id")
  LEFT join "item_bank_singpause_2024_item"  ON "item_bank_singpause_2024_phrase".song_name = "item_bank_singpause_2024_item".song_name
  LEFT JOIN (
    SELECT "scores_trial".*
    FROM "scores_trial"
    WHERE ("measure" = 'opti3')
  ) AS "...3"
    ON ("...2"."trial_id" = "...3"."trial_id")
    group by "item_bank_singpause_2024_phrase".song_name,"item_bank_singpause_2024_item".item_id, "item_bank_singpause_2024_phrase".item_type
    
    
union  select distinct
            "item_bank_singpause_2024_item".song_name,
                    "item_bank_singpause_2024_item".item_type  ,

    NULL AS phrases,

    STRING_AGG(COALESCE(score::text,'null'), '# ') AS scoress,
    "item_bank_singpause_2024_item".item_id,"item_bank_singpause_2024_item".image

     FROM "item_bank_singpause_2024_item"
  LEFT JOIN (
    SELECT
      "trial_id",
      "audio_file",
      "instrument",
      "attempt",
      "item_id",
      "display_modality",
      "phase",
      "rhythmic",
      "session_id",
      "test_id",
      "stimulus_abs_melody",
      "stimulus_durations",
      "user_id",
      "psychTestR_session_id",
      "experiment_id",
      "session_time_started",
      "session_time_completed"
    FROM (
      SELECT
        "q01".*,
        RANK() OVER (PARTITION BY "item_id" ORDER BY "session_time_started" DESC) AS "col01"
      FROM (
        SELECT
          "trials".*,
          "user_id",
          "psychTestR_session_id",
          "experiment_id",
          "sessions"."session_time_started" AS "session_time_started",
          "sessions"."session_time_completed" AS "session_time_completed"
        FROM "trials"
        LEFT JOIN "sessions"
          ON ("trials"."session_id" = "sessions"."session_id")
      ) AS "q01"
      WHERE
        ("user_id" = ${userId} and item_id in (select item_id from item_bank_singpause_2024_item))
    ) AS "q01"
    WHERE ("col01" <= 1)
  ) AS "...2"
    ON ("item_bank_singpause_2024_item"."item_id" = "...2"."item_id")
  LEFT JOIN (
    SELECT "scores_trial".*
    FROM "scores_trial"
    WHERE ("measure" = 'opti3')
  ) AS "...3"	
    ON ("...2"."trial_id" = "...3"."trial_id") 
       LEFT join "item_bank_singpause_2024_phrase"  ON "item_bank_singpause_2024_item".song_name = "item_bank_singpause_2024_phrase".song_name 

       group by "item_bank_singpause_2024_item".item_type,"item_bank_singpause_2024_item".item_id,"item_bank_singpause_2024_item".song_name,"item_bank_singpause_2024_item".image` ?? []


    console.log(result["songs"])

    if (result["songs"].count > 0) {
      let songs = result["songs"].filter((item) => item.item_type == "item")
      console.log("songs", JSON.stringify(songs))
      result["songs"] = result["songs"].filter((item) => item.item_type == "phrase")
      console.log("result['songs']", JSON.stringify(result["songs"]))

      result["songs"].forEach((item, index) => {
        let phrases = item["phrases"].split("#");
        console.log("phrases", JSON.stringify(phrases))

        result["songs"][index].phrases = phrases
        console.log("scores items:", JSON.stringify(item["scores"]))

        let scores = item["scores"].split("#");
        console.log("scores", JSON.stringify(scores))

        scores.forEach((score, index) => {
          scores[index] = isNaN(parseFloat(score)) ? "0" : (parseFloat(score) * 100).toFixed(0)
        })
        console.log("scores", JSON.stringify(songs.filter((song) => song.song_name == item.song_name)))

        let song_score = songs.filter((song) => song.song_name == item.song_name)[0]["scores"][0]
        console.log("scores", JSON.stringify(song_score))

        let image = songs.filter((song) => song.song_name == item.song_name)[0].img

        result["songs"][index].song_score = isNaN(parseFloat(song_score)) ? "0" : (parseFloat(song_score) * 100).toFixed(0)
        result["songs"][index]["image"] = image
        result["songs"][index].scores = scores

      });
    }
  } catch (error) {
    console.error('Database connection error', error);
  }

  console.log(JSON.stringify(result
  ))
  return {
    statusCode: 200,
    body: JSON.stringify(result
    ),
  };
};