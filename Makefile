LUA ?= lua
FENNEL ?= fennel
VERSION ?= $(shell git describe --abbrev=0)
FNLSOURCES = $(wildcard impl/*.fnl) init.fnl
FNLMACROS = macros.fnl
FNLTESTS = $(wildcard tests/*.fnl)
LUATESTS = $(FNLTESTS:.fnl=.lua)
FNLDOCS = $(FNLMACROS) $(FNLSOURCES)
LUASOURCES = $(FNLSOURCES:.fnl=.lua)
LUAEXECUTABLES ?= lua luajit
FENNELDOC := $(shell command -v fenneldoc)
LUACOV_COBERTURA := $(shell command -v luacov-cobertura)
LUACOV_CONSOLE := $(shell command -v luacov-console)
COMPILEFLAGS = --metadata --require-as-include

.PHONY: build clean help doc luacov luacov-console $(LUAEXECUTABLES)

build: $(LUASOURCES)

${LUASOURCES}: $(FNLSOURCES)

%.lua: %.fnl
	$(FENNEL) --lua $(LUA) --compile $(COMPILEFLAGS) $< > $@

clean:
	rm -f $(LUASOURCES) $(LUATESTS)

distclean: clean
	rm -rf luacov* coverage

test: $(FNLTESTS)
	@echo "Testing on" $$($(LUA) -v) >&2
	@$(foreach test,$?,$(FENNEL) --lua $(LUA) --metadata --correlate $(test) || exit;)
ifdef FENNELDOC
	@fenneldoc --mode check $(FNLDOCS) || exit
else
	@echo "" >&2
	@echo "fenneldoc is not installed" >&2
	@echo "Please install fenneldoc to check documentation during testing" >&2
	@echo "https://gitlab.com/andreyorst/fenneldoc" >&2
	@echo "" >&2
endif

testall: $(LUAEXECUTABLES)
	@$(foreach lua,$?,LUA=$(lua) make test || exit;)

luacov: COMPILEFLAGS = --no-metadata --correlate
luacov: distclean build $(LUATESTS)
	@$(foreach test,$(LUATESTS),$(LUA) -lluarocks.loader -lluacov $(test) || exit;)
	luacov
ifdef LUACOV_COBERTURA
	mkdir -p coverage
	luacov-cobertura -o coverage/cobertura-coverage.xml
endif
ifdef LUACOV_CONSOLE
	luacov-console .
	luacov-console --no-colored -s
endif

doc:
ifdef FENNELDOC
	fenneldoc --project-version $(VERSION) --config $(FNLMACROS) $(FNLSOURCES)
else
	@echo "" >&2
	@echo "fenneldoc is not installed" >&2
	@echo "Visit https://gitlab.com/andreyorst/fenneldoc for installation instructions" >&2
	@echo "" >&2
endif

help:
	@echo "make                -- create lua library" >&2
	@echo "make clean          -- remove lua files" >&2
	@echo "make distclean      -- remove all files not necessary for the project" >&2
	@echo "make luacov         -- run tests to produce luacov report" >&2
	@echo "make luacov-console -- run tests to produce luacov-console report" >&2
	@echo "make doc            -- create documentation with fenneldoc" >&2
	@echo "make help           -- print this message and exit" >&2
