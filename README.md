# series-manager
Manage viewed TV series

For now you can use it from GUILE CLI:

    (set! %series-base-path "/path/to/your/films")
    (add-series "Name" "regex that match name of series")

Note regex will be used also to find subtitles and alternative audios,
so make sure that regex did not match file extension.

    (mark-all-viewed! "Name")
    ;; Download new episodes
    (play* "Episode.Name.12")
    (save-status)

# Example

    (add-series "One-Punch Man" "One-Punch Man - ..")
    (play* "One-Punch Man - 05") ;; This will mark episode as viewed
    (save-status)
