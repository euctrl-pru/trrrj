do_package_checks()

if (Sys.getenv("BUILD_PKGDOWN") != "" && Sys.getenv("id_rsa") != "") {
  # pkgdown documentation can be built optionally. Other example criteria:
  # - `inherits(ci(), "TravisCI")`: Only for Travis CI
  # - `Sys.getenv("BUILD_PKGDOWN") != ""`: If the env var "BUILD_PKGDOWN" is set
  # - `Sys.getenv("TRAVIS_EVENT_TYPE") == "cron"`: Only for Travis cron jobs
  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

  if (ci_get_branch() == "master") {
    get_stage("install") %>%
      add_step(step_install_github("r-lib/pkgdown@1829398a4e97"))

    get_stage("deploy") %>%
      add_step(step_build_pkgdown()) %>%
      add_step(step_push_deploy(path = "docs", branch = "gh-pages"))
  }

  # run lintr after successful CI build
  get_stage("after_success") %>%
    add_code_step(lintr::lint_package())
}
