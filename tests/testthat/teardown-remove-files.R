# remove any files that aren't .stan files from resources/stan,
# e.g. files created by $compile()
all_files_in_stan <-
  list.files(test_path("resources", "stan"),
             full.names = TRUE,
             recursive = TRUE)
not_stan_programs <- !grepl(".stan$", all_files_in_stan)
file.remove(all_files_in_stan[not_stan_programs])
