PROJECT = shrimp
PROJECT_DESCRIPTION = Yet Another Reverse Proxy 
PROJECT_VERSION = 0.1.0


BUILD_DEPS = elvis_mk

DEPS =  gun   \
				sync  \
				cowboy 

dep_sync = git https://github.com/rustyio/sync.git
dep_sync_commit=master
RELX_TAR = 0

SHELL_OPTS = -pa ebin \
             -pa deps/*/ebin \
             -sname $(PROJECT) \
						 -s sync \
             -setcookie $(PROJECT) \
             -config rel/dev.config \
             +pc unicode \
             -run c erlangrc .

dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.1.2


DEP_PLUGINS = elvis_mk

REL_DEPS += relx
include erlang.mk

run : RELX_OPTS += --dev-mode


