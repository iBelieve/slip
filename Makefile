NAME = slip
ASD = $(NAME).asd
SOURCES = src/slip.lisp src/utils.lisp src/core.lisp \
		src/serve.lisp src/watch.lisp src/stages.lisp
LISP = sbcl

all: build

build: $(NAME)

$(NAME): $(ASD) $(SOURCES)
	$(LISP) --noinform --quit \
		--load $(ASD) \
		--eval '(ql:quickload :$(NAME))' \
		--eval '(asdf:make :$(NAME))'
