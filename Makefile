NAME = slip
ASD = $(NAME).asd
SOURCES = src/slip.lisp src/utils.lisp src/core.lisp \
	  src/serve.lisp src/watch.lisp src/livereload.lisp \
	  src/stages.lisp
#LISP = ccl --quiet --batch
LISP = sbcl --noinform --quit

all: build

build: $(NAME)

$(NAME): $(ASD) $(SOURCES)
	$(LISP) --eval '(require "asdf")' \
		--load $(ASD) \
		--eval '(ql:quickload :$(NAME))' \
		--eval '(asdf:make :$(NAME))'
