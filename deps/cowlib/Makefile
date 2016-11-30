# See LICENSE for licensing information.

PROJECT = cowlib
PROJECT_DESCRIPTION = Support library for manipulating Web protocols.
PROJECT_VERSION = 1.3.0

#ERLC_OPTS += +bin_opt_info
ifdef HIPE
	ERLC_OPTS += -smp +native
	TEST_ERLC_OPTS += -smp +native
endif

LOCAL_DEPS = crypto
DIALYZER_OPTS = -Werror_handling -Wunmatched_returns

CI_OTP ?= OTP-18.0.3 OTP-18.1.5 OTP-18.2.4.1 OTP-18.3.4.4 OTP-19.0.7 OTP-19.1.5
CI_HIPE ?= OTP-19.1.5
CI_ERLLVM ?= $(CI_HIPE)

TEST_ERLC_OPTS += +'{parse_transform, eunit_autoexport}' +'{parse_transform, horse_autoexport}'
TEST_DEPS = horse triq
dep_horse = git https://github.com/extend/horse master
dep_triq = git https://github.com/krestenkrab/triq master

include erlang.mk

.PHONY: gen perfs

# Mimetypes module generator.

GEN_URL = http://svn.apache.org/repos/asf/httpd/httpd/trunk/docs/conf/mime.types
GEN_SRC = src/cow_mimetypes.erl.src
GEN_OUT = src/cow_mimetypes.erl

gen:
	$(gen_verbose) cat $(GEN_SRC) \
		| head -n `grep -n "%% GENERATED" $(GEN_SRC) | cut -d : -f 1` \
		> $(GEN_OUT)
	$(gen_verbose) wget -qO - $(GEN_URL) \
		| grep -v ^# \
		| awk '{for (i=2; i<=NF; i++) if ($$i != "") { \
			split($$1, a, "/"); \
			print "all_ext(<<\"" $$i "\">>) -> {<<\"" \
				a[1] "\">>, <<\"" a[2] "\">>, []};"}}' \
		| sort \
		| uniq -w 25 \
		>> $(GEN_OUT)
	$(gen_verbose) cat $(GEN_SRC) \
		| tail -n +`grep -n "%% GENERATED" $(GEN_SRC) | cut -d : -f 1` \
		>> $(GEN_OUT)

# Performance testing.

ifeq ($(MAKECMDGOALS),perfs)
.NOTPARALLEL:
endif

ci-extra:: perfs

perfs: test-build
	$(gen_verbose) erl -noshell -pa ebin deps/horse/ebin \
		-eval 'horse:app_perf($(PROJECT)), erlang:halt().'
