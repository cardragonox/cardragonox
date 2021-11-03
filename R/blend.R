#' blend
#'
#' Blend multiple layers of images into various combinations
#'
#' @param name Default = "fig". Base name of figure. Will be appended with numbers for each blend.
#' @param folder Default = "output". Name of folder to save figs in.
#' @param folder1 Default = NULL. Folder with images for layer 1.
#' @param colors1 Default = NULL. (OPTIONAL) List of colors to combine for layer 1 images.
#' @param folder2 Default = NULL. Folder with images for layer 2.
#' @param colors2 Default = NULL. (OPTIONAL) List of colors to combine for layer 2 images.
#' @param folder3 Default = NULL. Folder with images for layer 3.
#' @param colors3 Default = NULL. (OPTIONAL) List of colors to combine for layer 3 images.
#' @param folder4 Default = NULL. Folder with images for layer 4.
#' @param colors4 Default = NULL. (OPTIONAL) List of colors to combine for layer 4 images.
#' @param folder5 Default = NULL. Folder with images for layer 5.
#' @param colors5 Default = NULL. (OPTIONAL) List of colors to combine for layer 5 images.
#' @keywords blend, merge, nft
#' @return blended images
#' @export

blend <- function(name = "fig",
                  folder = "output",
                  folder1 = NULL,
                  colors1 = NULL,
                  folder2 = NULL,
                  colors2 = NULL,
                  folder3 = NULL,
                  colors3 = NULL,
                  folder4 = NULL,
                  colors4 = NULL,
                  folder5 = NULL,
                  colors5 = NULL) {

  ## For Testing
  # name = "fig"
  # folder = "output"
  # folder1 = NULL
  # colors1 = NULL
  # folder2 = NULL
  # colors2 = NULL
  # folder3 = NULL
  # colors3 = NULL
  # folder4 = NULL
  # colors4 = NULL
  # folder5 = NULL
  # colors5 = NULL


  #...........................
  # Initiate
  #..........................

  NULL -> images_1 -> images_2 -> images_3 -> images_4 -> images_5 ->
    colors_1 -> colors_2 -> colors_3 -> colors_4 -> colors_5

  # Internal color check
  colors_exist <- function(x) {
    sapply(x, function(X) {
      tryCatch(is.matrix(grDevices::col2rgb(X)),
               error = function(e) FALSE)
    })
  }


  #...........................
  # Check Inputs
  #..........................

  # Check folder
  if(!dir.exists(folder)){
    dir.create(folder)
  } else {
      if(!grepl("/",folder)){folder <- paste0(getwd(),"/",folder)}
    }

  folders_list <- list()
  if(!is.null(folder1)){folders_list <- append(folders_list, list(folder1=folder1))}
  if(!is.null(folder2)){folders_list <- append(folders_list, list(folder2=folder2))}
  if(!is.null(folder3)){folders_list <- append(folders_list, list(folder3=folder3))}
  if(!is.null(folder4)){folders_list <- append(folders_list, list(folder4=folder4))}
  if(!is.null(folder5)){folders_list <- append(folders_list, list(folder5=folder5))}
  names(folders_list)

  colors_list <- list()
  if(!is.null(colors1)){colors_list <- append(colors_list, list(colors1=colors1))}
  if(!is.null(colors2)){colors_list <- append(colors_list, list(colors2=colors2))}
  if(!is.null(colors3)){colors_list <- append(colors_list, list(colors3=colors3))}
  if(!is.null(colors4)){colors_list <- append(colors_list, list(colors4=colors4))}
  if(!is.null(colors5)){colors_list <- append(colors_list, list(colors5=colors5))}
  names(colors_list)

  # Check folders exist
  for(i in 1:length(folders_list)){
    folder_i <- folders_list[[i]]
    if(!dir.exists(folder_i)){stop(paste0("Folder provided: ", folder_i, " does not exist."))}
  }

  # Get list of images
  if(length(folders_list)>1){
    images_list <- list()
    for(i in 1:length(folders_list)){
      folder_i <- folders_list[[i]]; folder_i
      images_list[i] <- list(list.files(folder_i, full.names = T))
      names(images_list)[i] <- paste0("images_",i)
    }; images_list
  }

  # Check colors exist
  for(i in 1:length(colors_list)){
    colors_i <- unlist(colors_list[i])
    if(!all(colors_exist(colors_i))){
      print(paste0("The following color(s) in: ", names(colors_list)[i], " is/are not valid and will be removed:"))
      print(paste0(colors_i[!colors_exist(colors_i)]))
      colors_ix <- colors_i[colors_exist(colors_i)]; colors_ix
      colors_list[[i]] <- colors_ix
    }
  }

  #...........................
  # Blend
  #..........................

  names(images_list)
  names(colors_list)

  # Create Image lists
  if(length(images_list)>0){

    images_1 <- c()
    for(image_i in images_list[[1]]){
      images_1 <- c(images_1,magick::image_read(image_i))
    }

    if(length(colors_list)>0){
      colors_1 <- colors_list[[i]]; colors_1
      for(i in 1:length(images_1)){
        for(color_i in colors_1){
          temp_image <- images_1[[i]]; temp_image
          temp_info <- utils::capture.output(images_1[[1]][[1]]); temp_info
          width = as.numeric(unlist(strsplit(unlist(strsplit(temp_info,"\\s+"))[3],"x"))[1]); width
          height = as.numeric(unlist(strsplit(unlist(strsplit(temp_info,"\\s+"))[3],"x"))[2]); height
          # Fill Image with new color
          print(paste0("Applying color : ",color_i))
          print(paste0("For image : ",image_i, "..."))
          for(width_i in seq(0,width,by=width/5)){
            for(height_i in seq(0,height,by=height/5)){
              temp_image <- magick::image_fill(temp_image, color_i, point = paste0("+",width_i,"+",height_i), fuzz = 20)
            }
          }
          print(paste0("Finished applying color : ",color_i,"."))
          images_1 <- c(images_1,temp_image)

        }
      }

      }
  }
  if(length(images_list)>1){

    images_2 <- c()
    for(image_i in images_list[[2]]){
      images_2 <- c(images_2,magick::image_read(image_i))
    }

    if(length(colors_list)>1){
      colors_2 <- colors_list[[2]]; colors_2
      for(i in 1:length(images_2)){
        for(color_i in colors_2){
          temp_image <- images_2[[i]]; temp_image
          temp_info <- utils::capture.output(images_2[[1]][[1]]); temp_info
          width = as.numeric(unlist(strsplit(unlist(strsplit(temp_info,"\\s+"))[3],"x"))[1]); width
          height = as.numeric(unlist(strsplit(unlist(strsplit(temp_info,"\\s+"))[3],"x"))[2]); height
          # Fill Image with new color
          print(paste0("Applying color : ",color_i))
          print(paste0("For image : ",image_i, "..."))
          for(width_i in seq(0,width,by=width/5)){
            for(height_i in seq(0,height,by=height/5)){
              temp_image <- magick::image_fill(temp_image, color_i, point = paste0("+",width_i,"+",height_i), fuzz = 20)
            }
          }
          print(paste0("Finished applying color : ",color_i,"."))
          images_2 <- c(images_2,temp_image)

        }
      }

    }
  }
  if(length(images_list)>2){

    images_3 <- c()
    for(image_i in images_list[[3]]){
      images_3 <- c(images_3,magick::image_read(image_i))
    }

    if(length(colors_list)>2){
      colors_3 <- colors_list[[3]]; colors_3
      for(i in 1:length(images_3)){
        for(color_i in colors_3){
          temp_image <- images_3[[i]]; temp_image
          temp_info <- utils::capture.output(images_3[[1]][[1]]); temp_info
          width = as.numeric(unlist(strsplit(unlist(strsplit(temp_info,"\\s+"))[3],"x"))[1]); width
          height = as.numeric(unlist(strsplit(unlist(strsplit(temp_info,"\\s+"))[3],"x"))[2]); height
          # Fill Image with new color
          print(paste0("Applying color : ",color_i))
          print(paste0("For image : ",image_i, "..."))
          for(width_i in seq(0,width,by=width/5)){
            for(height_i in seq(0,height,by=height/5)){
              temp_image <- magick::image_fill(temp_image, color_i, point = paste0("+",width_i,"+",height_i), fuzz = 20)
            }
          }
          print(paste0("Finished applying color : ",color_i,"."))
          images_3 <- c(images_3,temp_image)

        }
      }

    }
  }
  if(length(images_list)>3){

    images_4 <- c()
    for(image_i in images_list[[4]]){
      images_4 <- c(images_4,magick::image_read(image_i))
    }

    if(length(colors_list)>3){
      colors_4 <- colors_list[[4]]; colors_4
      for(i in 1:length(images_4)){
        for(color_i in colors_4){
          temp_image <- images_4[[i]]; temp_image
          temp_info <- utils::capture.output(images_4[[1]][[1]]); temp_info
          width = as.numeric(unlist(strsplit(unlist(strsplit(temp_info,"\\s+"))[3],"x"))[1]); width
          height = as.numeric(unlist(strsplit(unlist(strsplit(temp_info,"\\s+"))[3],"x"))[2]); height
          # Fill Image with new color
          print(paste0("Applying color : ",color_i))
          print(paste0("For image : ",image_i, "..."))
          for(width_i in seq(0,width,by=width/5)){
            for(height_i in seq(0,height,by=height/5)){
              temp_image <- magick::image_fill(temp_image, color_i, point = paste0("+",width_i,"+",height_i), fuzz = 20)
            }
          }
          print(paste0("Finished applying color : ",color_i,"."))
          images_4 <- c(images_4,temp_image)

        }
      }

    }
  }
  if(length(images_list)>4){

    images_5 <- c()
    for(image_i in images_list[[5]]){
      images_5 <- c(images_5,magick::image_read(image_i))
    }

    if(length(colors_list)>4){
      colors_5 <- colors_list[[5]]; colors_5
      for(i in 1:length(images_5)){
        for(color_i in colors_5){
          temp_image <- images_5[[i]]; temp_image
          temp_info <- utils::capture.output(images_5[[1]][[1]]); temp_info
          width = as.numeric(unlist(strsplit(unlist(strsplit(temp_info,"\\s+"))[3],"x"))[1]); width
          height = as.numeric(unlist(strsplit(unlist(strsplit(temp_info,"\\s+"))[3],"x"))[2]); height
          # Fill Image with new color
          print(paste0("Applying color : ",color_i))
          print(paste0("For image : ",image_i, "..."))
          for(width_i in seq(0,width,by=width/5)){
            for(height_i in seq(0,height,by=height/5)){
              temp_image <- magick::image_fill(temp_image, color_i, point = paste0("+",width_i,"+",height_i), fuzz = 20)
            }
          }
          print(paste0("Finished applying color : ",color_i,"."))
          images_5 <- c(images_5,temp_image)

        }
      }

    }
  }

  # Combine Image Layers
  if(!is.null(images_1)){
    for(i in 1:length(images_1)){

      image_a <- images_1[[i]]; image_a

      # Check if images_2 exist
      if(!is.null(images_2)){
      for(j in 1:length(images_2)){
        image_b <- c(image_a, images_2[[j]]); image_b
        image_b <- magick::image_mosaic(image_b); image_b

        # Check if images_3 exist
        if(!is.null(images_3)){
          for(k in 1:length(images_3)){
            image_c <- c(image_b, images_3[[k]]); image_c
            image_c <- magick::image_mosaic(image_c); image_c

            # Check if images_4 exist
            if(!is.null(images_4)){
              for(l in 1:length(images_4)){
                image_d <- c(image_c, images_4[[l]]); image_d
                image_d <- magick::image_mosaic(image_d); image_d

                # Check if images_5 exist
                if(!is.null(images_5)){
                  for(m in 1:length(images_5)){
                    image_e <- c(image_d, images_5[[m]]); image_e
                    image_e <- magick::image_mosaic(image_e); image_e

                    # Print the combined images
                    magick::image_write(image_e, path = paste0(folder,"/",name,"_",i,"_",j,"_",k,"_",l,"_",m,".png"), format = "png")
                    print(paste0("Figure saved as : ", paste0(folder,"/",name,"_",i,"_",j,"_",k,"_",l,"_",m,".png")))

                  }} else {
                    # Print the combined images
                    magick::image_write(image_d, path = paste0(folder,"/",name,"_",i,"_",j,"_",k,"_",l,".png"), format = "png")
                    print(paste0("Figure saved as : ", paste0(folder,"/",name,"_",i,"_",j,"_",k,"_",l,".png")))
                  }

              }} else {
                # Print the combined images
                magick::image_write(image_c, path = paste0(folder,"/",name,"_",i,"_",j,"_",k,".png"), format = "png")
                print(paste0("Figure saved as : ", paste0(folder,"/",name,"_",i,"_",j,"_",k,".png")))
              }

          }} else {
            # Print the combined images
            magick::image_write(image_b, path = paste0(folder,"/",name,"_",i,"_",j,".png"), format = "png")
            print(paste0("Figure saved as : ", paste0(folder,"/",name,"_",i,"_",j,".png")))
          }


        }} else {
          # Print the combined images
          magick::image_write(image_a, path = paste0(folder,"/",name,"_",i,".png"), format = "png")
          print(paste0("Figure saved as : ", paste0(folder,"/",name,"_",i,".png")))
        }
    }} # For image 1



} # Close blend function
