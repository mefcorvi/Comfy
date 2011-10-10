.PHONY: all compile release test doc

all: compile

compile:
	@[ -f private/game_log.log ] && rm private/game_log.log || echo "Log file not exists"
	rebar skip_deps=true compile

release: compile
	rebar -f generate

test:
	rebar skip_deps=true eunit

run:
	@echo "Running..."
	@erlc bootstrapper.erl
	@erl -pa ebin ./apps/*/ebin ./deps/*/include ./deps/*/ebin .deps/*/include \
	-name comfy@127.0.0.1 \
	-config erl \
	-eval "bootstrapper:start()."

doc:
	set -v off
	rebar doc
