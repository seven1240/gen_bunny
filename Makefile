REBAR:=./rebar

.PHONY: all erl test clean doc int_test up

all: erl

erl:
	$(REBAR) get-deps compile

up:
	$(REBAR) update-deps compile

test: all
	@mkdir -p .eunit
	$(REBAR) skip_deps=true eunit

int_test: all
	@-rm -rf deps/gen_bunny
	(cd integration_tests; $(MAKE) test)

clean:
	$(REBAR) clean
	-rm -rvf deps ebin doc .eunit
	(cd integration_tests; $(MAKE) clean)

doc:
	$(REBAR) doc
