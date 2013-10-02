
default: t1.png t2.png t3.png r1.png r2.png r3.png r4.png z1a.png z1b.png z2a.png z2b.png z3a.png t0.png t5.png

t1.dot t2.dot t3.dot: BinaryTree
	./BinaryTree

r1.dot r2.dot r3.dot: RedBlack
	./RedBlack
	for x in a*.dot; do \
	  dot -Tpng $$x > `basename $$x .dot`.png; \
	done

%.png: %.dot
	dot -Tpng $< >$@

%: %.hs
	ghc -o $@ $<
