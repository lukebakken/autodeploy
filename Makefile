PROJECT = autodeploy
DEPS = cowboy lager
ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'
include erlang.mk
