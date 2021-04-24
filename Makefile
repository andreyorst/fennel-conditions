LUA ?= lua
FENNEL ?= fennel
FNLSOURCES = impl/condition-system.fnl
LUASOURCES = $(FNLSOURCES:.fnl=.lua)

.PHONY: build clean help doc

build: $(LUASOURCES)

${LUASOURCES}: $(FNLSOURCES)

%.lua: %.fnl
	$(FENNEL) --lua $(LUA) --compile --require-as-include $< > $@

clean:
	rm -f $(LUASOURCES)

doc:
	fenneldoc conditions.fnl $(FNLSOURCES)

help:
	@echo "make       -- create lua library" >&2
	@echo "make clean -- remove lua files" >&2
	@echo "make doc   -- create documentation with fenneldoc" >&2
	@echo "make help  -- print this message and exit" >&2
