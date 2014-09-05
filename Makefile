REBAR := ./rebar
DBDRIVER ?= sqlite

.PHONY: compile test

all: compile

compile: $(REBAR)
	$(REBAR) get-deps compile

test: $(REBAR)
	$(REBAR) -C rebar.test.config get-dep compile
	DBDRIVER=$(DBDRIVER) $(REBAR) -C rebar.test.config eunit -v skip_deps=true

clean: $(REBAR)
	$(REBAR) clean
