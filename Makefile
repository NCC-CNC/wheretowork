# main commands
## command to open R in interactive session
R:
	R --quiet --no-save

## command to remove automatically generated files
clean:
	rm -rf man/*
	rm -rf docs/*
	rm -rf inst/doc/*

# R package development commands
## build docs and run tests
all: man readme test check spellcheck

## build docs
man:
	R --slave -e "devtools::document()"

## simulate data
data:
	R --slave -e "source('inst/scripts/simulate-data.R')"

## reubild readme
readme:
	R --slave -e "rmarkdown::render('README.Rmd')"

## run tests
test:
	R --slave -e "devtools::test()" > test.log 2>&1
	rm -f tests/testthat/Rplots.pdf

## test examples
examples:
	R --slave -e "devtools::run_examples(test = TRUE, run = TRUE);warnings()"  >> examples.log
	rm -f Rplots.pdf

## run checks
check:
	echo "\n===== R CMD CHECK =====\n" > check.log 2>&1
	R --slave -e "devtools::check(build_args = '--no-build-vignettes', args = '--no-build-vignettes', run_dont_test = TRUE, vignettes = FALSE)" >> check.log 2>&1

## check docs for spelling mistakes
spellcheck:
	echo "\n===== SPELL CHECK =====\n" > spell.log 2>&1
	R --slave -e "devtools::spell_check()" >> spell.log 2>&1

## install package
install:
	R --slave -e "devtools::install_local(getwd(), force = TRUE, upgrade = 'never')"

## build entire site
site:
	R --slave -e "pkgdown::clean_site()"
	R --slave -e "pkgdown::build_site(run_dont_run = TRUE, lazy = FALSE)"

## rebuild update files for site
quicksite:
	R --slave -e "pkgdown::build_site(run_dont_run = TRUE, lazy = TRUE)"

# commands to launch app
## launch local version using system libraries
debug:
	R -e "options(golem.app.prod = FALSE); golem::run_dev()"

## launch local version inside Docker container
demo-start:
	docker-compose up
	google-chrome "http://localhost:3838"

demo-skill:
	docker-compose down

## launch released version inside Docker container
launch-start:
	docker run -dp 3838:3838 --name wheretowork -it naturecons/wheretowork
	google-chrome "http://localhost:3838"

launch-kill:
	docker rm --force wheretowork

## deploy app on shinyapps.io
shinyapps:
	R -e "options('golem.app.prod' = FALSE);rsconnect::deployApp(getwd(), appName = 'where-to-work', launch.browser = TRUE)"

# Docker commands
## create local Docker image
image:
	docker build -t wheretowork-image .

# renv copmmands
## snapshot R package dependencies
snapshot:
	R -e "renv::snapshot()"

.PHONY: clean data readme test check install man spellcheck examples site quicksite snapshot deploy demo image debug snapshot
