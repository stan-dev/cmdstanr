# remove any files that aren't .stan files from resources/stan,
# e.g. files created by $compile()

all_files <- list.files(test_path("resources", "stan"), full.names = TRUE)
not_stan_programs <- !grepl(".stan$", all_files)
file.remove(all_files[not_stan_programs])
