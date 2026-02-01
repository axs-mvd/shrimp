PROJECT = shrimp
PROJECT_DESCRIPTION = Yet Another Reverse Proxy 
PROJECT_VERSION = 0.1.0


BUILD_DEPS = elvis_mk

DEPS = gun    \
       sync   \
       cowboy 

dep_sync = git https://github.com/rustyio/sync.git v0.4.1
dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.1.2

dep_gun_commit=2.2.0
dep_cowboy_commit=2.13.0

RELX_TAR = 0

SHELL_OPTS = -pa ebin               \
             -pa deps/*/ebin        \
             -sname $(PROJECT)      \
             -s sync                \
             -setcookie $(PROJECT)  \
             -config rel/dev.config \
             +pc unicode            \
             -run c erlangrc .

DEP_PLUGINS = elvis_mk

REL_DEPS += relx
include erlang.mk

run : RELX_OPTS += --dev-mode

