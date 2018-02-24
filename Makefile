NAME = slip
ASD = $(NAME).asd
SOURCES = src/slip.lisp src/utils.lisp src/core.lisp src/stages.lisp
LISP = sbcl
MANIFEST = manifest

all: build

build: $(NAME)

$(MANIFEST): $(ASD)
	$(LISP) --noinform --quit \
		--eval '(ql:quickload :$(NAME))' \
		--eval '(ql:write-asdf-manifest-file "$(MANIFEST)" :if-exists :supersede :exclude-local-projects nil)'

$(NAME): $(MANIFEST) $(SOURCES)
	buildapp --output $(NAME) \
		--manifest-file $(MANIFEST) \
		--load-system $(NAME) \
		--entry $(NAME):main
