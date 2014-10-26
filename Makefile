PROJECT = autodeploy
DEPS = cowboy lager poolboy
dep_poolboy = git git://github.com/devinus/poolboy
ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'
include erlang.mk
