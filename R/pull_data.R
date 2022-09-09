pull_track_data <- function(file_num) {
  if(file_num < 1 | file_num > 1000) {
    rlang::abort('file_num must be between 1 and 1000')
  }
  
  file_name = str_glue('Spotify MPD Clean/tracks_{file_num}.csv')
  x = read_csv(aws.s3::get_object(
    object = file_name,
    bucket = "cjtrackr",
    as="text"
  ), show_col_types = FALSE) 
  
  if(!("pid" %in% colnames(x) & "artist_name" %in% colnames(x))) {
    return(tibble(playlist_id=double(), artist_name=character()))
  }
  
  x %>% select(playlist_id = pid, artist_name)
}

pull_relevant_data <- function(file_nums) {
  mapper = tibble(artist_id_1=double(), artist_id_2=double(), playlist_count=double())
  artist_id = tibble(artist_name=character(), artist_id=double())
  max_artist_id = 0
  #max_artist_id = artist_id %>% summarise(max(artist_id)) %>% pull

  # Run up to 250
  for(i in file_nums) {
    print(i)
    track_data = pull_track_data(i)

    new_artists = track_data %>% distinct(artist_name) %>%
      anti_join(artist_id, by="artist_name") %>%
      mutate(
        artist_id = max_artist_id + row_number()
      )
    artist_id = artist_id %>% union_all(new_artists)
    max_artist_id = artist_id %>% summarise(max(artist_id)) %>% pull

    track_data = track_data %>%
      inner_join(artist_id, by ="artist_name") %>% select(artist_id, playlist_id)

    summarised_data = track_data %>%
      inner_join(track_data, by = "playlist_id", suffix = c("_1","_2")) %>%
      filter(artist_id_1 < artist_id_2) %>%
      group_by(artist_id_1, artist_id_2) %>%
      summarise(playlist_count = n_distinct(playlist_id), .groups='drop')

    mapper = mapper %>% union_all(summarised_data) %>%
      group_by(artist_id_1, artist_id_2) %>%
      summarise(playlist_count = sum(playlist_count), .groups='drop')
  }

  mapper %>% arrow::write_feather('mapper.arrow')
  artist_id %>% arrow::write_feather('artist_id.arrow')
}

get_my_data <- function() {
  mapper = arrow::read_feather('mapper.arrow')
  mapper_bilateral = mapper %>% 
    union_all(
      mapper %>% rename(artist_id_1=artist_id_2, artist_id_2=artist_id_1)
    )
  
  artist_2_counts = mapper_bilateral %>% 
    select(artist_id_2 = artist_id_1,playlist_count) %>% 
    group_by(artist_id_2) %>% 
    summarise(playlist_count_2 = sum(playlist_count))
  
  my_artists = arrow::read_feather('artist_id.arrow') %>% 
    semi_join(
      artist_2_counts %>% filter(playlist_count_2 > 69000), # 782 artists, keeps my favorites
      by = c("artist_id" = "artist_id_2")
    )
  
  my_artist_counts = mapper_bilateral %>% 
    semi_join(my_artists,by=c("artist_id_1"="artist_id"))
  
  max_playist_count_2 = artist_2_counts %>% summarise(max(playlist_count_2)) %>% pull
  
  my_artist_count_data = my_artist_counts %>% 
    rename(
      playlist_count_2_given_1 = playlist_count,
    ) %>% 
    group_by(
      artist_id_1
    ) %>% 
    mutate(
      max_playist_count_2_given_1 = max(playlist_count_2_given_1)
    ) %>% 
    ungroup %>% 
    left_join(
      artist_2_counts, by="artist_id_2"
    ) %>% 
    mutate(
      max_playist_count_2 = max_playist_count_2
    )
  
  my_artists %>% arrow::write_feather('my_artists.arrow')
  my_artist_count_data %>% arrow::write_feather('my_artist_count_data.arrow')
}


