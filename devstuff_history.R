# The goal of this file is to keep track of all devtools/usethis calls

# Hide this file from build
usethis::use_build_ignore("devstuff_history.R")

# Licence
usethis::use_mit_license(name = "Benjamin Louis")
# Readme
usethis::use_readme_md()
#CoC
usethis::use_code_of_conduct()

# Versionning
usethis::use_git()

#Badge
usethis::use_lifecycle_badge("Experimental")
# with continuous integration
usethis::use_travis()
usethis::use_appveyor()
usethis::use_coverage()

# Vignettes
usethis::use_vignette("datascan_utilisation")

#dependencies
usethis::use_pipe()
attachment::att_to_description()
usethis::use_tidy_description()

# For tests
usethis::use_testthat()
usethis::use_test("scan_numerics")
usethis::use_test("plot_hist")
usethis::use_test("vis_numerics")
usethis::use_test("scan_groups")
usethis::use_test("scan_corr")
usethis::use_test("vis_corr")

usethis::use_test("plot_bar")
usethis::use_test("plot_count")
usethis::use_test("plot_scatter")
usethis::use_test("plot_violin")

usethis::use_test("scan_cramerv")
usethis::use_test("scan_ngcovar")
usethis::use_test("scan_r")

usethis::use_test("utils")

usethis::use_test("vis_cramerv")
usethis::use_test("vis_ggcovar")
usethis::use_test("vis_groups")
usethis::use_test("vis_ngcovar")
usethis::use_test("vis_nncovar")
usethis::use_test("vis_r")


