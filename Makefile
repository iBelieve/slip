NAME = slip
ASD = $(NAME).asd
SOURCES = src/slip.lisp
LISP = sbcl
MANIFEST = manifest

all: build

build: $(NAME)

$(MANIFEST): $(ASD) $(SOURCES)
	$(LISP) --eval '(ql:quickload :$(NAME))' \
		--eval '(ql:write-asdf-manifest-file "$(MANIFEST)" :if-exists :supersede :exclude-local-projects nil)' \
		--eval '(quit)'

$(NAME): $(MANIFEST)
	buildapp --output $(NAME) \
		--manifest-file $(MANIFEST) \
		--load-system $(NAME) \
		--entry $(NAME):main
