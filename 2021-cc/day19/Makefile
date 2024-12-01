info:
	@echo -e 'usage: make 1s,2s,1i,2i\n1s for part 1 with sample\n1i for part 1 with input\n...'

1s:
	@gcc *1.c -lm ../../util/util.c -g
	@./a.out || gdb a.out core*
	@for f in core*; do [ -f $$f ] && trash core* || break; done;

2s:
	@gcc *2.c -lm ../../util/util.c -g
	@./a.out || gdb a.out core*
	@for f in core*; do [ -f $$f ] && trash core* || break; done;

1i:
	@gcc *1.c -lm ../../util/util.c -g
	@./a.out arg || gdb a.out core*
	@for f in core*; do [ -f $$f ] && trash core* || break; done;

2i:
	@gcc *2.c -lm ../../util/util.c -g
	@./a.out arg || gdb a.out core*
	@for f in core*; do [ -f $$f ] && trash core* || break; done;

clean:
	trash core*
	trash a.out
