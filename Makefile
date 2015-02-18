PROJECT = pal_google_oauth2

DEPS = pal_oauth2
dep_pal_oauth2 = git git://github.com/manifest/pal-oauth2.git develop

PLT_APPS = pt pal jsx hackney
SHELL_OPTS = \
	-eval 'application:ensure_all_started($(PROJECT), permanent)'

include erlang.mk

