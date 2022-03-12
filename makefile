help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

init: ## Use the system python3 to create a virtual environment with poetry (requires curl).
	echo "TODO"

check: ## Run tests, and security audit.
	echo "TODO"

check-fast: ## Run tests, exit on first error.
	echo "TODO"

lint: ## Run linter.
	echo "TODO"

todo: ## Things that should be done...
	@grep -nR --color=always  --before-context=2  --after-context=2 TODO .

xxx: ## Things that require attention!
	@grep -nR --color=always --before-context=2  --after-context=2 XXX .

shell: ## Run a bash in ubuntu docker with the repository mounted as /united
	docker run -v $(PWD):/united -it --rm ubuntu:21.10 bash -c "apt update && apt install --yes cmake autopoint bsdmainutils debhelper dh-autoreconf dh-strip-nondeterminism dwz flex gettext gettext-base groff-base intltool-debian libarchive-zip-perl libdebhelper-perl libelf1 libffi-dev libfile-stripnondeterminism-perl libgc-dev libglib2.0-0 libgmp-dev libgmpxx4ldbl libicu67 libncurses-dev libncurses5-dev libpipeline1 libreadline-dev libsub-override-perl libtext-unidecode-perl gperf libuchardet0 libunistring-dev libxml-libxml-perl libxml-namespacesupport-perl libxml-sax-base-perl libxml-sax-perl libxml2 man-db pkg-config po-debconf tex-common texinfo tzdata ucf zlib1g-dev libsqlite3-dev libssl-dev git make libtool wget curl libck-dev build-essential uuid-dev && cd /united && bash"
