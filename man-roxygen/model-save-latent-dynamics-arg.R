#' @param save_latent_dynamics (logical) Should auxiliary diagnostic information
#'   about the sampler or variational algorithm be written to diagnostic CSV
#'   files? This argument replaces CmdStan's `diagnostic_file` argument. The
#'   content is controlled by the user's CmdStan installation. The default is
#'   `FALSE`, which is appropriate for almost every use case. To save temporary
#'   diagnostic files permanently, use
#'   [`$save_latent_dynamics_files()`][fit-method-save_latent_dynamics_files].
