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

int_test: clean all
	@git checkout -b current
	(cd integration_tests; $(MAKE) test)
	@git checkout -q `git rev-parse current`
	@git branch -D current

clean:
	$(REBAR) clean
	-rm -rf deps ebin doc .eunit
	(cd integration_tests; $(MAKE) clean)

doc:
	$(REBAR) doc
