ENAME=harmonysources
ETMPPARENT=/tmp
ETMP=$(ETMPPARENT)/$(ENAME)
EZIPPED=$(ENAME).tar.gz

SRC=.

all:
	make -C $(SRC)/visual
	make -C $(SRC)/sync
	make -C $(SRC)/syncvisual

clean:
	find . -name *.bak -exec rm -rf \{\} \;
	rm -rf $(EZIPPED) 
	make -C $(SRC)/visual clean
	make -C $(SRC)/sync clean
	make -C $(SRC)/syncvisual clean

all-clean: clean
	rm -f $(SRC)/config.mk $(SRC)/config.log $(SRC)/config.status

export:
	make all-clean
	rm -rf $(ETMP)
	mkdir $(ETMP)
	cp -r * $(ETMP)
	(cd $(ETMPPARENT); \
	 tar cf - $(ENAME) | gzip -c --best > $(EZIPPED); \
         chmod 700 $(EZIPPED))
	mv $(ETMPPARENT)/$(EZIPPED) .

