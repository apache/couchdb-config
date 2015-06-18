export BUILDDIR ?= /Users/elbrujohalcon/Projects/inaka/couchdb-config

REBAR ?= rebar -vvv

.PHONY: all app clean test shell xref

all:
	${REBAR} get compile

app:
	${REBAR} skip_deps=true compile

clean: app
	${REBAR} clean

test: app
	mkdir -p tmp/etc;
	cp test/fixtures/default_eunit.ini tmp/etc/default_eunit.ini;
	touch tmp/etc/local_eunit.ini;
	touch tmp/etc/eunit.ini;
	${REBAR} eunit

shell: all
	erl -pa deps/*/ebin `pwd`/ebin -boot start_sasl

xref: app
	${REBAR} xref
