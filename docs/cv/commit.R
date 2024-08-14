library(gert)

gert::git_add(dir(all.files=  TRUE, recursive = TRUE))
gert::git_commit_all(".")
gert::git_push()
