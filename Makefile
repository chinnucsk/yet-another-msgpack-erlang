.PHONY: test deps

REBAR_CONFIG ?= rebar.config


all: deps compile test

compile:
	@./rebar -C ${REBAR_CONFIG} compile skip_deps=true
	@./rebar -C ${REBAR_CONFIG} xref skip_deps=true

# covertool has warnings, so compile first
deps:
	@./rebar -C ${REBAR_CONFIG} update-deps
	@./rebar -C ${REBAR_CONFIG} get-deps
	@./rebar -C ${REBAR_CONFIG} compile

clean:
	@./rebar -C ${REBAR_CONFIG} clean skip_deps=true

test: clean compile
	@./rebar -C ${REBAR_CONFIG} eunit skip_deps=true

distclean: clean
	@./rebar -C ${REBAR_CONFIG} delete-deps
