IMAGE = haskell:latest

shell_SHELL = bash

run shell:
	docker run -it --rm \
	--name cis194 \
	--mount type=bind,source="$(CURDIR)",target=/app,readonly \
	$(IMAGE) $($@_SHELL)
