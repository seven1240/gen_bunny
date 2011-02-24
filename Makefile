REBAR:=./rebar

.PHONY: all erl up test int_test clean_deps clean doc

all: erl

erl:
	$(REBAR) get-deps compile

up:
	$(REBAR) update-deps compile

test: all
	@mkdir -p .eunit
	$(REBAR) skip_deps=true eunit

int_test:
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
