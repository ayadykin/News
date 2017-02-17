PROJECT = news_webserver
PROJECT_DESCRIPTION = RESI API for News
PROJECT_VERSION = 0.1.0


DEPS = cowbow sync jsx lager
dep_cowbow = git https://github.com/ninenines/cowboy master
dep_sync = git https://github.com/rustyio/sync master
dep_jsx = git https://github.com/talentdeficit/jsx master
dep_lager = git https://github.com/erlang-lager/lager master

	
include erlang.mk
