# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.
export BUILDDIR ?= ..

REBAR ?= rebar

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
