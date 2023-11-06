# `corpus` package might be needed for the read_ndjson function but it is not on CRAN any more.
tmp <- tempfile()
system2("git", c("clone", "--recursive",
                 shQuote("https://github.com/patperry/r-corpus.git"), shQuote(tmp)))
devtools::install(tmp)
library(corpus)



# Clean data from Zeeswchimer tiktok download
# The function will clean and organize columns extracted from the the web-app zeeschwimer.
read_tok <- function(.x){
  original_data <- corpus::read_ndjson(.x)
  
  text_on_screen <- purrr::map(original_data$data.stickersOnItem, list(1, "stickerText")) |>
    purrr::map_chr(paste0, collapse = " ")
  
  challenge_title <- purrr::map_depth(original_data$data.challenges, .depth = 2, .ragged = T, "title") |>
    purrr::map_chr(paste0, collapse = "; ")
  
  sticker_name <- purrr::map_depth(original_data$data.effectStickers, .depth = 2, .ragged = T, "name") |>
    purrr::map_chr(paste0, collapse = "; ")
  
  sticker_id = purrr::map_depth(original_data$data.effectStickers, .depth = 2, .ragged = T, "ID") |>
    purrr::map_chr(paste0, collapse = "; ")
  
  table_read = tibble(item_id = original_data$item_id,
                      source_platform_url = original_data$source_platform_url,
                      video_id = original_data$data.id,
                      created_at = original_data$data.createTime,
                      like_count = original_data$data.stats.diggCount,
                      play_count = original_data$data.stats.playCount,
                      share_count = original_data$data.stats.shareCount,
                      comment_count = original_data$data.stats.commentCount,
                      description = original_data$data.desc,
                      video_duration = original_data$data.video.duration,
                      video_cover = original_data$data.video.originCover,
                      user_id = original_data$data.author.id,
                      user = original_data$data.author.uniqueId,
                      username = original_data$data.author.nickname,
                      bio = original_data$data.author.signature,
                      user_avatar = original_data$data.author.avatarMedium,
                      user_private = original_data$data.author.privateAccount,
                      user_verified = original_data$data.author.verified,
                      user_like_count = original_data$data.authorStats.diggCount,
                      user_followers = original_data$data.authorStats.followerCount,
                      user_following = original_data$data.authorStats.followingCount,
                      user_total_likes = original_data$data.authorStats.heartCount,
                      user_video_count = original_data$data.authorStats.videoCount,
                      music_title = original_data$data.music.title,
                      music_album = original_data$data.music.album,
                      music_author = original_data$data.music.authorName,
                      music_cover = original_data$data.music.coverLarge,
                      music_url = original_data$data.music.playUrl,
                      music_duration = original_data$data.music.duration)
  
  make_clean <- function(.x){
    if(length(.x) == 0){
      clean <- rep("", nrow(table_read))
    } else{
      clean <- .x
    }
    return(clean)
  }
  
  result <- table_read |>
    dplyr::mutate(video_text = make_clean(text_on_screen),
                  challenges = make_clean(challenge_title),
                  effect_name = make_clean(sticker_name),
                  effect_id = make_clean(sticker_id),
                  created_at = as.integer(created_at),
                  engagement = like_count + comment_count + share_count)|>
    tidyr::unnest(created_at) |>
    dplyr::mutate(created_at = lubridate::as_datetime(created_at))
  return(result)
}



# Make sure not to have overlapping data from multiple tiktok imports

latest_tok <- function(.x){
  strings <- .x |>
    dplyr::group_by(item_id) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(where(is.character), where(is.logical), created_at)
  
  numericals <- .x |>
    dplyr::mutate_if(is.integer, as.numeric) |>
    dplyr::group_by(item_id) |>
    dplyr::summarise_if(is.numeric, max)
  
  
  final <- numericals |>
    dplyr::left_join(strings) |>
    dplyr::select(colnames(.x))
  return(final)
}

### ------ actual running ------ ###

# Cleaning an actual dataset

tiktok <- read_tok("~/Downloads/tiktok.ndjson")

latest <- latest_tok(tiktok)
