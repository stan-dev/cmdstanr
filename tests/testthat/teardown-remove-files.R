# remove any files that aren't .stan files from resources/stan,
# e.g. files created by $compile()
all_files_in_stan <-
  list.files(test_path("resources", "stan"),
             full.names = TRUE,
             recursive = TRUE)
not_stan_programs <- !grepl(".stan$", all_files_in_stan)
file.remove(all_files_in_stan[not_stan_programs])

# remove all files from answers/sandbox except README
all_files_in_sandbox <-
  list.files(test_path("answers", "sandbox"),
             full.names = TRUE,
             include.dirs = FALSE,
             recursive = TRUE)
not_readme <- !grepl("README$", all_files_in_sandbox)
file.remove(all_files_in_sandbox[not_readme])
file.remove(list.dirs(test_path("answers", "sandbox"), recursive = FALSE))
