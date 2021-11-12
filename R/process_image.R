require(tibble)
require(glue)
require(magick)
process_image <- 
  function(mapid = NA,
           extension = NA,
           save_thumbnail = TRUE) {
    stopifnot(!is.na(mapid))
    stopifnot(!is.na(extension) & extension %in% c("jpg", "png", "jpeg", "webp"))
    
    original <- image_read(glue("maps/{mapid}.{extension}"))
    img_info <- image_info(original)
    img_max_dim <- max(img_info$width, img_info$height)
    img_chunk <- img_max_dim / 16
    img_aspect <- glue("{round(img_info$width / img_chunk, 0)}x{round(img_info$height / img_chunk, 0)}")
    
    if (save_thumbnail) {
      original %>% 
        image_scale("400x400") %>% 
        image_write(path = glue("thumbnails/{mapid}.{extension}"))
    }
    return(tibble(mapid = mapid,
                  extension = extension,
                  aspect = img_aspect))
  }

