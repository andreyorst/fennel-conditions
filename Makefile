LUA ?= lua
FENNEL ?= fennel
VERSION ?= $(shell git describe --abbrev=0)
FNLSOURCES = $(wildcard impl/*.fnl) init.fnl
FNLMACROS = macros.fnl
FNLTESTS = $(wildcard tests/*.fnl)
FNLDOCS = $(FNLMACROS) $(FNLSOURCES)
LUASOURCES = $(FNLSOURCES:.fnl=.lua)
LUAEXECUTABLES ?= lua luajit
FENNELDOC := $(shell command -v fenneldoc)

.PHONY: build clean help doc $(LUAEXECUTABLES)

build: $(LUASOURCES)

${LUASOURCES}: $(FNLSOURCES)

%.lua: %.fnl
	$(FENNEL) --lua $(LUA) --compile --require-as-include $< > $@

clean:
	rm -f $(LUASOURCES)

test: $(FNLTESTS)
	@echo "Testing on" $$($(LUA) -v) >&2
	@$(foreach test,$?,$(FENNEL) --lua $(LUA) --metadata --correlate $(test) || exit;)
ifdef FENNELDOC
	@fenneldoc --mode check $(FNLDOCS) || exit
else
	@echo ""
	@echo "fenneldoc is not installed" >&2
	@echo "Please install fenneldoc to check documentation during testing" >&2
	@echo "https://gitlab.com/andreyorst/fenneldoc" >&2
	@echo ""
endif

testall: $(LUAEXECUTABLES)
	@$(foreach lua,$?,LUA=$(lua) make test || exit;)

doc:
	fenneldoc --project-version $(VERSION) --config $(FNLMACROS) $(FNLSOURCES)

help:
	@echo "make       -- create lua library" >&2
	@echo "make clean -- remove lua files" >&2
	@echo "make doc   -- create documentation with fenneldoc" >&2
	@echo "make help  -- print this message and exit" >&2
