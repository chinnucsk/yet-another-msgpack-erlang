.PHONY: all compile deps clean test devrel rel

REBAR_CONFIG = rebar.config

APP_NAME = yet-another-msgpack-erlang

all: clean deps test

deps: get-deps update-deps
	@./rebar -C $(REBAR_CONFIG) compile

update-deps:
	@./rebar -C $(REBAR_CONFIG) update-deps

get-deps:
	@./rebar -C $(REBAR_CONFIG) get-deps

compile:
	@./rebar -C $(REBAR_CONFIG) compile skip_deps=true
	@./rebar -C $(REBAR_CONFIG) xref skip_deps=true

test:
	rm -rf .eunit
	@./rebar -C $(REBAR_CONFIG) eunit skip_deps=true

clean:
	@./rebar -C $(REBAR_CONFIG) clean skip_deps=true

distclean: clean
	@./rebar -C $(REBAR_CONFIG) clean
	@./rebar -C $(REBAR_CONFIG) delete-deps
	rm -rf dev

dialyze-build-otp-plt:
	dialyzer --build_plt --output_plt ~/.dialyzer.plt \
	    --apps erts kernel stdlib sasl inets mnesia crypto public_key snmp reltool ssl

dialyze-build-deps-plt:
	dialyzer --build_plt --output_plt .$(APP_NAME)_deps.plt --apps deps/*

dialyze-with-deps:
	dialyzer --check_plt --plt .$(APP_NAME)_deps.plt -c .
	dialyzer --plts ~/.dialyzer.plt .$(APP_NAME)_deps.plt --src src --no_native \
	    -Wunmatched_returns -Werror_handling -Wrace_conditions

dialyze:
	dialyzer --plts ~/.dialyzer.plt --src src --no_native \
	    -Wunmatched_returns -Werror_handling -Wrace_conditions
