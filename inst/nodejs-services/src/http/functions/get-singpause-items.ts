import { db } from '../../core/rds'

export const handler = async (event) => {
  let result = {}

  const phrase_bank_id = JSON.parse(event.body).phrase_bank_id ?? 9
  const item_bank_id = JSON.parse(event.body).item_bank_id ?? 8

  const phrase_table = await db`select item_bank_name  from item_banks where item_bank_id = ${phrase_bank_id} limit 1`
  const item_table = await db`select item_bank_name  from item_banks where item_bank_id = ${item_bank_id} limit 1`

  const phrase_table_name = `item_bank_${phrase_table[0].item_bank_name}`
  const item_table_name = `item_bank_${item_table[0].item_bank_name}`
  const userId = JSON.parse(event.body).user_id
  try {
    result["songs"] = await db` SELECT DISTINCT
    ${db(phrase_table_name)}.song_name,
    ${db(phrase_table_name)}.item_type,
    STRING_AGG(${db(phrase_table_name)}.item_id, '#') AS phrases,
    STRING_AGG(DISTINCT ${db(phrase_table_name)}.audio_file, '#') AS audio_file,
    STRING_AGG(DISTINCT ${db(phrase_table_name)}.lyrics_file, '#') AS lyrics_file,
    STRING_AGG(${db(phrase_table_name)}.abs_melody, '#') AS abs_melody,
    STRING_AGG(${db(phrase_table_name)}.durations, '#') AS durations,
    STRING_AGG(COALESCE(score::text, 'null'), '# ') AS scores,
    ${db(item_table_name)}.item_id,
    NULL AS img
  FROM ${db(phrase_table_name)}
  LEFT JOIN (
    SELECT
      trials.*,
      RANK() OVER (PARTITION BY item_id ORDER BY trial_time_completed DESC) AS rank
    FROM trials
    LEFT JOIN sessions ON trials.session_id = sessions.session_id
    WHERE user_id = ${userId} AND item_id IN (
      SELECT item_id FROM ${db(phrase_table_name)}
    )
  ) AS ranked_trials ON ${db(phrase_table_name)}.item_id = ranked_trials.item_id
  LEFT JOIN ${db(item_table_name)}
    ON ${db(phrase_table_name)}.song_name = ${db(item_table_name)}.song_name
  LEFT JOIN (
    SELECT * FROM scores_trial WHERE measure = 'opti3'
  ) AS scores ON ranked_trials.trial_id = scores.trial_id
  GROUP BY ${db(phrase_table_name)}.song_name, ${db(item_table_name)}.item_id, ${db(phrase_table_name)}.item_type

  UNION

  SELECT DISTINCT
    ${db(item_table_name)}.song_name,
    ${db(item_table_name)}.item_type,
    NULL AS phrases,
    STRING_AGG(DISTINCT COALESCE(${db(item_table_name)}.audio_file::text, 'null'), '#') AS audio_file,
    STRING_AGG(DISTINCT COALESCE(${db(item_table_name)}.lyrics_file::text, 'null'), '#') AS lyrics_file,
    STRING_AGG(DISTINCT ${db(item_table_name)}.abs_melody, '#') AS abs_melody,
    STRING_AGG(DISTINCT ${db(item_table_name)}.durations, '#') AS durations,
    STRING_AGG(COALESCE(score::text, 'null'), '#') AS scores,
    ${db(item_table_name)}.item_id,
    ${db(item_table_name)}.image
  FROM ${db(item_table_name)}
  LEFT JOIN (
    SELECT
      trials.*,
      RANK() OVER (PARTITION BY item_id ORDER BY trial_time_completed DESC) AS rank
    FROM trials
    LEFT JOIN sessions ON trials.session_id = sessions.session_id
    WHERE user_id = ${userId} AND item_id IN (
      SELECT item_id FROM ${db(item_table_name)}
    )
  ) AS ranked_trials ON ${db(item_table_name)}.item_id = ranked_trials.item_id
  LEFT JOIN (
    SELECT * FROM scores_trial WHERE measure = 'opti3'
  ) AS scores ON ranked_trials.trial_id = scores.trial_id
  LEFT JOIN ${db(phrase_table_name)}
    ON ${db(item_table_name)}.song_name = ${db(phrase_table_name)}.song_name
  GROUP BY ${db(item_table_name)}.item_type, ${db(item_table_name)}.item_id, ${db(item_table_name)}.song_name, ${db(item_table_name)}.image` ?? []


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

        let audio_file = item["audio_file"].split("#");

        result["songs"][index].audio_file = audio_file

        let lyrics_file = item["lyrics_file"].split("#");

        result["songs"][index].lyrics_file = lyrics_file

        let song_lyrics_file = songs.filter((song) => song.song_name == item.song_name)[0]["lyrics_file"]
        let song_audio_file= songs.filter((song) => song.song_name == item.song_name)[0]["audio_file"]



        let abs_melody = item["abs_melody"].split("#");

        result["songs"][index].abs_melody = abs_melody
        console.log("abs_melody",abs_melody)

        let durations = item["durations"].split("#");
        console.log("durations",durations)

        result["songs"][index].durations = durations

        let song_abs_melody = songs.filter((song) => song.song_name == item.song_name)[0]["abs_melody"]
        let song_durations= songs.filter((song) => song.song_name == item.song_name)[0]["durations"]

        let scores = item["scores"].split("#");
        console.log("scores", JSON.stringify(scores))

        scores.forEach((score, index) => {
          scores[index] = isNaN(parseFloat(score)) ? "0" : parseFloat(score)
        })
        console.log("scores", JSON.stringify(songs.filter((song) => song.song_name == item.song_name)))

        let song_score = songs.filter((song) => song.song_name == item.song_name)[0]["scores"][0]
        console.log("scores", JSON.stringify(song_score))

        let image = songs.filter((song) => song.song_name == item.song_name)[0].img

        result["songs"][index].song_score = isNaN(parseFloat(song_score)) ? "0" : (parseFloat(song_score) * 100).toFixed(0)
        result["songs"][index]["image"] = image
        result["songs"][index].scores = scores
        result["songs"][index].song_lyrics_file = song_lyrics_file
        result["songs"][index].song_audio_file = song_audio_file
        result["songs"][index].song_abs_melody = song_abs_melody
        result["songs"][index].song_durations = song_durations
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