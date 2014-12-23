PROJECT = autodeploy
DEPS = cowboy lager

ifeq ($(shell findmnt -no FSTYPE /tmp), tmpfs)
RELX_OUTPUT_DIR ?= /tmp/autodeploy
endif

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

PLT_APPS = inets ssl xmerl
DIALYZER_DIRS = ebin

.PHONY: debug release

debug: ERLC_OPTS += -DDEBUG=1
debug: all

release: RELX_OUTPUT_DIR := _rel
release: clean all

