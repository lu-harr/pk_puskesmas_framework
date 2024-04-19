########################################################################
# NAMES VECTOR CODE
# MAKE SITE NAMES LOOK NICE BY TRIMMING PUSKESMAS, NUMBERS FROM MoH SET
# I wrote this a long time ago but it seems to work fine?

library(stringr)

names_vec = health_sites$name
long_names = c()
names_vec = as.character(names_vec)
new_names = c()
for (i in 1:length(names_vec)){
  edit_name = unlist(str_split(names_vec[i], " "))
  
  if (health_sites$dataset[i] == "moh"){
    names_vec[i] = paste(edit_name[2: length(edit_name)], collapse = " ")
  }
  
  if (edit_name[1] == "Puskesmas"|
      edit_name[1] == "puskesmas"|
      edit_name[1] == "PUSKESMAS"|
      edit_name[1] == "Puskesmas,"|
      edit_name[1] == "POSKESDES"|
      edit_name[1] == "Pueskesmas"){
    #message(edit_name[2:length(edit_name)])
    names_vec[i] = paste(edit_name[2: (length(edit_name))], collapse=" ")
  }
  
  if (sum(nchar(edit_name)) > 25){
    long_names = c(long_names, i)
  }
  
  # names_vec is fine at this point
  tmp = ""
  for (word_ in unlist(str_split(names_vec[i], " "))){
    letters_ = unlist(str_split(word_, ""))
    if (length(letters_) > 0){
      if (utf8ToInt(letters_[1]) == 160){  # a weird space character keeps popping up ?
        letters_ = letters_[2:length(letters_)]
      }
      new_word = paste(c(letters_[1], tolower(letters_[2:length(letters_)])), collapse="")
      if (tmp == ""){  # ?
        tmp = new_word
      } else {
        tmp = paste(tmp, new_word, collapse = " ")
      }
    }
  }
  new_names = c(new_names, tmp)
}  

long_names
#names_vec[long_names] = sapply(names_vec[long_names], 
#                               function(x){paste(substr(x, 1, 20), "...", sep="")})
new_names[which(new_names == "Rsud Malinau")] = "RSUD Malinau"

health_sites$name = new_names

