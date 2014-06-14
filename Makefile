REBAR := ./rebar
REBAR_URL := https://github.com/downloads/basho/rebar/rebar

.PHONY: compile test

all: compile

compile: $(REBAR)
	$(REBAR) get-deps compile

test: $(REBAR)
	$(REBAR) get-dep compile
	$(REBAR) eunit -v skip_deps=true

clean:
	$(REBAR) clean

./rebar:
	erl -noshell -s inets start -s ssl start \
        -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
        -s inets stop -s init stop
	chmod +x ./rebar
