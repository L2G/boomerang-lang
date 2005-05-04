all:
	$(MAKE) -C src

clean:
	$(MAKE) -C src clean
	$(MAKE) -C examples clean

all-clean:
	$(MAKE) -C src all-clean
