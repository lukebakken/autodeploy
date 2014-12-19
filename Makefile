PROJECT = autodeploy
DEPS = cowboy lager

ifeq ($(shell findmnt -no FSTYPE /tmp), tmpfs)
	RELX_OUTPUT_DIR ?= /tmp/zdsms
endif

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

