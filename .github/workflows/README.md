# GitHub Actions Workflows

This directory contains GitHub Actions workflows for testing and maintaining cmdstanr.

## Testing with Custom CmdStan Tarballs

To test cmdstanr with a custom CmdStan tarball (e.g., during CmdStan releases):

1. Go to the [Actions tab](https://github.com/stan-dev/cmdstanr/actions)
2. Select the "Unit tests" workflow
3. Click "Run workflow"
4. Enter the CmdStan tarball URL in the `tarball_url` input field (or leave empty/enter "latest" for the latest release)
5. Click "Run workflow"

Example tarball URL:
```
https://github.com/stan-dev/cmdstan/releases/download/v2.35.0/cmdstan-2.35.0.tar.gz
```

If you leave the `tarball_url` field empty or enter "latest", the workflow will use the latest CmdStan release.

### Legacy Workflow

The `cmdstan-tarball-check.yaml` workflow is maintained for backwards compatibility but is deprecated. Please use the main `R-CMD-check.yaml` workflow instead as described above.
