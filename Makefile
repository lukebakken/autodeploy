PROJECT = autodeploy
DEPS = cowboy lager erlexec
dep_erlexec = git git://github.com/saleyn/erlexec.git
ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'
include erlang.mk
