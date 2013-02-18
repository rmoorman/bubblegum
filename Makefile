ERL_EBIN_DIRS=
ERL_EBIN_DIRS+=$(CURDIR)/ebin 
ERL_EBIN_DIRS+=$(CURDIR)/lib/*/ebin 
ERL_EBIN_DIRS+=$(CURDIR)/deps/*/ebin 
ERL_OPTS= -pa $(ERL_EBIN_DIRS)
ERL_RUN=

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

compile: deps
	$(REBAR) compile

test: eunit
eunit: rebar
	$(REBAR) skip_deps=true eunit

docs: rebar
	$(REBAR) skip_deps=true doc

#dialyzer: compile
#	@dialyzer -c ebin

run:
	erl $(ERL_OPTS) $(ERL_RUN)

