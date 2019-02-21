# Modified version of getVolumes from shinyFiles that takes account
# of /media not existing on a Linux file system.

harp_getVolumes <- function (exclude) {
  if (missing(exclude))
    exclude <- NULL
  function() {
    osSystem <- Sys.info()["sysname"]
    if (osSystem == "Darwin") {
      volumes <- dir_ls("/Volumes")
      names(volumes) <- basename(volumes)
    }
    else if (osSystem == "Linux") {
      volumes <- c(Computer = "/")
      if (dir.exists("/media")) {
        media <- fs::dir_ls("/media")
        names(media) <- basename(media)
      } else {
        media <- ""
      }
      volumes <- c(volumes, media)
    }
    else if (osSystem == "Windows") {
      volumes <- system("wmic logicaldisk get Caption",
        intern = T)
      volumes <- sub(" *\\r$", "", volumes)
      keep <- !tolower(volumes) %in% c("caption", "")
      volumes <- volumes[keep]
      volNames <- system("wmic logicaldisk get VolumeName",
        intern = T)
      volNames <- sub(" *\\r$", "", volNames)
      volNames <- volNames[keep]
      volNames <- paste0(volNames, ifelse(volNames == "",
        "", " "))
      volNames <- paste0(volNames, "(", volumes, ")")
      names(volumes) <- volNames
      volumes <- gsub(":$", ":/", volumes)
    }
    else {
      stop("unsupported OS")
    }
    if (!is.null(exclude)) {
      volumes <- volumes[!names(volumes) %in% exclude]
    }
    volumes
  }
}
