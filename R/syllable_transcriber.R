
# reticulate::use_condaenv("audioenv", required = TRUE)


# reticulate::py_install(
#   c("torch", "torchaudio", "transformers", "soundfile", "librosa", "numpy<2"),
#   envname = "audioenv",
#   pip = TRUE,
#   force = TRUE
# )

# t <- syllable_transcriber_lambda("https://rand-om-public.s3.us-east-1.amazonaws.com/true.mp3",
#                                  model_path = "~/lyricassessr/data-raw/wav2vec2_xgb_bundle.qs") %>%
#   purrr::pluck('body') %>%
#   rjson::fromJSON() %>%
#   purrr::pluck('syllables') %>%
#   dplyr::bind_rows()


# ======================================================
#  HTTP endpoint: fetch syllable transcription results
# ======================================================

fetch_syllable_results_lambda <- function(
    db_con = NULL,
    upload_id,
    ...
) {

  if (is.null(upload_id) || is.na(upload_id)) {
    stop("upload_id is required")
  }

  # DB connection handling
  if (is.null(db_con)) {
    db_con <- musicassessr_con()
    disconnect_locally <- TRUE
  } else {
    disconnect_locally <- FALSE
  }

  logging::loginfo(
    "Fetching syllable results for upload_id=%s",
    upload_id
  )

  # --------------------------------------------------
  # Query results
  # --------------------------------------------------
  res <- dplyr::tbl(db_con, "syllable_predictions") %>%
    dplyr::filter(upload_id == !!upload_id) %>%
    dplyr::arrange(onset) %>%
    dplyr::collect()

  if (disconnect_locally) {
    db_disconnect(db_con)
  }

  # --------------------------------------------------
  # Polling semantics
  # --------------------------------------------------
  if (nrow(res) == 0) {
    return(list(
      statusCode = 200,
      body = jsonlite::toJSON(
        list(
          status = "pending",
          upload_id = upload_id
        ),
        auto_unbox = TRUE
      )
    ))
  }

  # --------------------------------------------------
  # Ready
  # --------------------------------------------------
  out <- res %>%
    dplyr::select(onset, dur, predicted_syllable)

  list(
    statusCode = 200,
    headers = list("Content-Type" = "application/json"),
    body = jsonlite::toJSON(
      list(
        status = "ready",
        upload_id = upload_id,
        syllables = out
      ),
      auto_unbox = TRUE
    )
  )
}


# ========================================================================
#  S3 TRIGGER — SYLLABLE TRANSCRIPTION
# ========================================================================

s3_trigger_syllable_transcription <- function(Records) {

  logging::loginfo("Inside s3_trigger_syllable_transcription")

  serverless_stage <- Sys.getenv("STAGE")

  logging::loginfo("serverless_stage: %s", serverless_stage)

  bucket <- Records[[9]][[3]][[1]]

  logging::loginfo("bucket: %s", bucket)

  key    <- utils::URLdecode(Records[[9]][[4]][[1]])

  logging::loginfo("key: %s", key)

  ext <- tools::file_ext(key)

  logging::loginfo("ext: %s", ext)

  if (!ext %in% c("wav","mp3","m4a","ogg")) {
    logging::logerror("%s not a validation extension", ext)
    return(invisible(NULL))
  }

  # ------------------------------------------------------------
  # Fetch upload_id from S3 metadata
  # ------------------------------------------------------------
  attribs <- get_s3_attribs(key, bucket)

  upload_id <- attribs %>% dplyr::pull(upload_id)

  logging::loginfo("upload_id: %s", upload_id)
  logging::loginfo("key: %s", key)

  if (is.null(upload_id) || is.na(upload_id)) {
    stop("upload_id missing from S3 metadata.")
  }

  # ------------------------------------------------------------
  # Ignore non-audio
  # ------------------------------------------------------------
  if (!grepl("\\.(wav|mp3|m4a|ogg)$", key, ignore.case = TRUE)) {
    logging::loginfo("Skipping non-audio file: %s", key)
    return(list(statusCode = 200))
  }

  # ------------------------------------------------------------
  # Build public URL
  # ------------------------------------------------------------
  audio_url <- sprintf(
    "https://%s.s3.amazonaws.com/%s",
    bucket,
    key
  )

  logging::loginfo("audio_url: %s", audio_url)

  # ------------------------------------------------------------
  # Invoke syllable transcription lambda
  # ------------------------------------------------------------
  syllable_lambda <- sprintf(
    "musicassessr-%s-syllable-transcriber",
    serverless_stage
  )

  logging::loginfo("syllable_lambda: %s", syllable_lambda)

  payload <- list(
    upload_id = upload_id,
    audio_url = audio_url
  )

  aws.lambda::invoke_function(
    name    = syllable_lambda,
    payload = jsonlite::toJSON(payload, auto_unbox = TRUE),
    type    = "Event"   # async fire-and-forget
  )

  logging::loginfo(
    "Invoked syllable transcriber for upload_id=%s",
    upload_id
  )

  list(
    statusCode = 200,
    upload_id = upload_id
  )
}


# ======================================================
# Main Lambda entry
# ======================================================
syllable_transcriber_lambda <- function(audio_url,
                                        upload_id = NA,
                                        model_path = "/opt/models/wav2vec2_xgb_bundle.qs",
                                        tmp_dir = "/tmp",
                                        ...) {

  logging::loginfo("Starting syllable transcription for: %s", audio_url)

  # ensure models are loaded (cached across Lambda invocations)
  .load_embedding_env(verbose = FALSE)
  .load_wav2vec_model()

  logging::loginfo("load syllable model...")
  model_bundle <- .load_syllable_model(model_path)

  # download input file
  local_audio <- file.path(tmp_dir, basename(audio_url))
  utils::download.file(audio_url, local_audio, mode = "wb")

  # step 1 — pyin segmentation
  transcribed <- run_pyin_segmentation(local_audio)


  if (nrow(transcribed) == 0) {
    return(list(
      statusCode = 200,
      body = jsonlite::toJSON(list(syllables = list()))
    ))
  }

  logging::loginfo("trimming segments...")

  # step 2 — trim segments to /tmp
  trimmed <- trim_segments(transcribed, local_audio, tmp_dir = tmp_dir)

  logging::loginfo("extract embeddings...")

  tictoc::tic("Extract embeddings")


  # step 3 — extract embeddings for each trimmed segment
  emb_df <- extract_wav2vec2_embeddings_batch(trimmed$trimmed_path)

  tictoc::toc()

  # step 4 — predict syllables
  logging::loginfo("predict syllables...")

  tictoc::tic("Predict syllables")

  preds <- predict_syllables(emb_df, model_bundle)

  tictoc::toc()

  out <- trimmed %>%
    dplyr::bind_cols(preds) %>%
    dplyr::select(onset, dur, predicted_syllable = .pred_class)

  logging::loginfo("Append to DB...")

  if(!is.scalar.na(upload_id)) {
    db_con <- musicassessr_con()

    out <- out %>%
      dplyr::mutate(upload_id = upload_id) %>%
      dplyr::relocate(upload_id)

    db_append_to_table(db_con, "syllable_predictions", out)

    db_disconnect(db_con)
  }


  list(
    statusCode = 200,
    headers = list("Content-Type" = "application/json"),
    body = jsonlite::toJSON(list(syllables = out), auto_unbox = TRUE)
  )
}



run_pyin_segmentation <- function(file_path) {
  logging::loginfo("Running PYIN segmentation...")
  tryCatch({
    py <- pyin::pyin(file_path, normalise = TRUE)
    tibble::as_tibble(py) %>% dplyr::rename(onset = onset, dur = dur, freq = freq)
  }, error = function(e) {
    logging::logerror("pyin failed: %s", e$message)
    tibble::tibble(onset = numeric(0), dur = numeric(0))
  })
}

trim_segments <- function(transcribed, file_path,
                          tmp_dir = "/tmp",
                          target_sr = 16000,
                          target_dur = 1.0,
                          pad_before = 0.1) {
  out_dir <- file.path(tmp_dir, "segments")
  dir.create(out_dir, showWarnings = FALSE)
  target_samples <- target_sr * target_dur

  transcribed %>%
    dplyr::mutate(trimmed_path = purrr::map2_chr(onset, dur, function(o, d) {
      base <- tools::file_path_sans_ext(basename(file_path))
      name <- sprintf("%s_%04dms.wav", base, round(o * 1000))
      out <- file.path(out_dir, name)
      start <- max(0, o - pad_before)
      cmd <- sprintf("sox %s %s trim %.3f %.3f rate %d channels 1",
                     shQuote(file_path), shQuote(out), start, target_dur, target_sr)
      system(cmd, ignore.stderr = TRUE)
      out
    }))
}


extract_wav2vec2_embeddings_batch <- function(paths, sr = 16000L) {

  env <- .load_embedding_env(FALSE)
  wav2vec <- .load_wav2vec_model()

  processor <- wav2vec$processor
  model <- wav2vec$model

  # -----------------------------
  # Load audio
  # -----------------------------
  wavs <- lapply(paths, function(p) {
    audio <- env$torchaudio$load(p)
    as.numeric(audio[[1]]$squeeze()$cpu()$numpy())
  })

  np <- env$np
  wavs_np <- lapply(wavs, function(x) np$array(x, dtype = "float32"))

  # -----------------------------
  # Batch inference
  # -----------------------------
  inputs <- processor(
    wavs_np,
    sampling_rate = sr,
    return_tensors = "pt",
    padding = TRUE
  )

  with(env$torch$no_grad(), {
    outputs <- model(inputs$input_values)
  })

  hidden_states <- outputs$hidden_states

  # -----------------------------
  # Convert to per-segment embeddings
  # -----------------------------
  layer_means <- lapply(hidden_states, function(h) {
    env$torch$mean(h, dim = 1L)$cpu()$numpy()
  })

  emb <- do.call(cbind, layer_means)

  emb_df <- as.data.frame(emb)

  names(emb_df) <- paste0("vec_", seq_len(ncol(emb_df)))

  tibble::as_tibble(emb_df)
}

extract_wav2vec2_embeddings <- function(file_path, sr = 16000L) {

  env <- .load_embedding_env(verbose = FALSE)
  wav2vec <- .load_wav2vec_model()

  processor <- wav2vec$processor
  model <- wav2vec$model

  audio <- env$torchaudio$load(file_path)
  wav_tensor <- audio[[1]]$squeeze()

  wav_vec <- as.numeric(wav_tensor$to(device = "cpu")$numpy())

  np <- env$np
  wav_np <- np$array(wav_vec, dtype = "float32")

  inputs <- processor(
    list(wav_np),
    sampling_rate = sr,
    return_tensors = "pt",
    padding = TRUE
  )

  with(env$torch$no_grad(), {
    outputs <- model(inputs$input_values)
  })

  hidden_states <- outputs$hidden_states

  flat <- unlist(lapply(
    hidden_states,
    function(h) as.numeric(env$torch$mean(h, dim = 1L)$cpu()$numpy())
  ))

  tibble::as_tibble_row(
    setNames(flat, paste0("vec_", seq_along(flat)))
  )
}

predict_syllables <- function(emb_df, model_bundle) {

  obj <- .load_prediction_workflow(model_bundle)

  model <- obj$model
  req_vars <- obj$req_vars

  if (all(grepl("^vec_", names(emb_df)))) {
    names(emb_df) <- paste0("layer1_dim", seq_along(emb_df))
  }

  missing <- setdiff(req_vars, names(emb_df))
  for (m in missing) emb_df[[m]] <- NA

  emb_df <- emb_df[, req_vars, drop = FALSE]

  predict(model, new_data = emb_df)
}


.load_syllable_model <- function(model_path = "/opt/models/wav2vec2_xgb_bundle.qs") {
  if (!exists(".syllable_model", envir = .GlobalEnv))
    assign(".syllable_model", qs::qread(model_path), envir = .GlobalEnv)
  get(".syllable_model", envir = .GlobalEnv)
}

.load_embedding_env <- function(verbose = TRUE) {

  reticulate::use_condaenv("audioenv", required = TRUE)

  if (!exists(".embedding_env", envir = .GlobalEnv)) {
    transformers <- reticulate::import("transformers")
    torch <- reticulate::import("torch")
    torchaudio <- reticulate::import("torchaudio")
    np <- reticulate::import("numpy", convert = FALSE)
    assign(".embedding_env", list(
      transformers = transformers, torch = torch,
      torchaudio = torchaudio, np = np
    ), envir = .GlobalEnv)
    if (verbose) message("✔ Python embedding env loaded")
  }
  get(".embedding_env", envir = .GlobalEnv)
}

.load_prediction_workflow <- function(model_bundle) {

  if (!exists(".syllable_model_fast", envir = .GlobalEnv)) {

    wf <- bundle::unbundle(model_bundle$model)

    model <- workflows::extract_fit_parsnip(wf)

    rec <- workflows::extract_preprocessor(wf)

    req_vars <- rec$var_info$variable[rec$var_info$role == "predictor"]

    assign(".syllable_model_fast", model, envir = .GlobalEnv)
    assign(".syllable_req_vars", req_vars, envir = .GlobalEnv)

    logging::loginfo("Loaded fast syllable model")

  }

  list(
    model = get(".syllable_model_fast", envir = .GlobalEnv),
    req_vars = get(".syllable_req_vars", envir = .GlobalEnv)
  )
}

get_s3_attribs <- function(key, bucket) {

  aws.s3::head_object(key, bucket) %>%
    attributes() %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(
      ~ .x |>
        stringr::str_remove("x-amz-meta-") |>
        stringr::str_remove("x-amz-") |>
        stringr::str_replace_all("-", "_")
    )
}

.load_wav2vec_model <- function() {

  if (!exists(".wav2vec_model", envir = .GlobalEnv)) {

    env <- .load_embedding_env(verbose = FALSE)

    processor <- env$transformers$Wav2Vec2Processor$from_pretrained(
      "facebook/wav2vec2-base-960h"
    )

    model <- env$transformers$Wav2Vec2Model$from_pretrained(
      "facebook/wav2vec2-base-960h",
      output_hidden_states = TRUE
    )

    model$eval()

    assign(
      ".wav2vec_model",
      list(
        processor = processor,
        model = model
      ),
      envir = .GlobalEnv
    )

    logging::loginfo("Loaded wav2vec2 model")
  }

  get(".wav2vec_model", envir = .GlobalEnv)
}
