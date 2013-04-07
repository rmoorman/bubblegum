ERL_EBIN_DIRS=$(shell find $(PWD)/deps $(PWD)/lib $(PWD)/ -maxdepth 2 -name ebin)
ERL_OPTS= -pa $(ERL_EBIN_DIRS)
ERL_TEST_OPTS=-boot start_sasl -sasl sasl_error_logger false $(ERL_OPTS)
ERL_RUN= -eval 'application:start(funtester).'

REBAR=./rebar

###
.PHONY: all test eunit deps

all: clean compile test
rebar:
	wget http://cloud.github.com/downloads/basho/rebar/rebar && chmod u+x rebar

clean: rebar
	$(REBAR) clean
	rm -f *.access *.auth *.log *.dump

distclean: rebar
	$(REBAR) delete-deps

deps: rebar
	$(REBAR) get-deps

fcompile: rebar
	$(REBAR) skip_deps=true compile

compile: rebar
	$(REBAR) compile

.eunit/conf:
	mkdir -p .eunit
	ln -fs $(PWD)/conf $(PWD)/.eunit/conf

test: eunit
eunit: rebar .eunit/conf
	export PG_POOL_CONFIG=pg_pool.tests.conf \
		ERL_AFLAGS="$(ERL_TEST_OPTS)" \
		&& $(REBAR) -v skip_deps=true eunit

test-travis: rebar .eunit/conf
	export PG_POOL_CONFIG=pg_pool.travis.conf \
		ERL_AFLAGS="$(ERL_TEST_OPTS)" \
		&& $(REBAR) -v skip_deps=true eunit
	
docs: rebar
	$(REBAR) skip_deps=true doc

#dialyzer: compile
#	@dialyzer -c ebin

run:
	erl $(ERL_OPTS) $(ERL_RUN)

