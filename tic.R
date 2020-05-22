# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks()

if (ci_on_travis() && ci_has_env("BUILD_PKGDOWN") && Sys.getenv("id_rsa") != "") {
  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())
  # creates pkgdown site and pushes to gh-pages branch
  # only for the runner with the "BUILD_PKGDOWN" env var set
  get_stage("deploy") %>%
    add_step(step_build_pkgdown()) %>%
    add_code_step(system('echo "trrrj.ansperformance.eu" > docs/CNAME')) %>%
    add_step(step_push_deploy(path = "docs", branch = "gh-pages"))
}
