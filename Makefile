all: man readme test check spellcheck

clean:
	rm -rf man/*
	rm -rf data/*
	rm -rf docs/*
	rm -rf inst/doc/*

docs: man readme

man:
	R --slave -e "devtools::document()"

data:
	R --slave -e "source('inst/scripts/simulate-data.R')"

readme:
	R --slave -e "rmarkdown::render('README.Rmd')"

test:
	R --slave -e "devtools::test()" > test.log 2>&1
	rm -f tests/testthat/Rplots.pdf

check:
	echo "\n===== R CMD CHECK =====\n" > check.log 2>&1
	R --slave -e "devtools::check(build_args = '--no-build-vignettes', args = '--no-build-vignettes', run_dont_test = TRUE, vignettes = FALSE)" >> check.log 2>&1

install:
	R --slave -e "devtools::install_local(getwd())"

spellcheck:
	echo "\n===== SPELL CHECK =====\n" > spell.log 2>&1
	R --slave -e "devtools::spell_check()" >> spell.log 2>&1

examples:
	R --slave -e "devtools::run_examples(test = TRUE, run = TRUE);warnings()"  >> examples.log
	rm -f Rplots.pdf

quicksite:
	R --slave -e "pkgdown::build_site(run_dont_run = TRUE, lazy = TRUE)"

site:
	R --slave -e "pkgdown::clean_site()"
	R --slave -e "pkgdown::build_site(run_dont_run = TRUE, lazy = FALSE)"

demo-solutionSettings:
	R --slave -e "devtools::load_all();options(shiny.launch.browser=TRUE);locationmisc::runExample('solutionSettings')"

demo-newSolutionSidebarPane:
	R --slave -e "devtools::load_all();options(shiny.launch.browser=TRUE);locationmisc::runExample('newSolutionSidebarPane')"

demo-mapManagerSidebarPane:
	R --slave -e "devtools::load_all();options(shiny.launch.browser=TRUE);locationmisc::runExample('mapManagerSidebarPane')"

demo-mapManager:
	R --slave -e "devtools::load_all();options(shiny.launch.browser=TRUE);locationmisc::runExample('mapManager')"

demo-debug:
	R --slave -e "devtools::load_all();options(shiny.launch.browser=TRUE);locationmisc::runExample('debug')"

deploy-solutionSettings:
	R -e "devtools::install_github('NCC-CNC/locationmisc')"
	R -e "rsconnect::deployApp('inst/example/solutionSettings', appName = 'solutionSettings', launch.browser = TRUE)"

.PHONY: clean data readme test check install man spellcheck examples demo-solutionSettings site quicksite
