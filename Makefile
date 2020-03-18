IMAGE = haskell:latest

run:
	docker run -it --rm \
	--name cis194 \
	--mount type=bind,source="$(CURDIR)",target=/app,readonly \
	$(IMAGE)
