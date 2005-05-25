HARMONY_DIR = ../../src
VIEWERS_DIR = $(HARMONY_DIR)/lib/viewers
LIB_DIR = $(HARMONY_DIR)/lib/ubase
LIB_DIRS = -I $(HARMONY_DIR) -I $(VIEWERS_DIR) -I $(LIB_DIR)
CMX_FILES = error.cmx safelist.cmx uprintf.cmx util.cmx uarg.cmx \
	   prefs.cmx trace.cmx misc.cmx mapplus.cmx \
           info.cmx name.cmx surveyor.cmx v.cmx metal.cmx metay.cmx meta.cmx

FAVDIR = ./IE6BMSample
MOVEDDIR = ./IE6BMNew

VIEW_FILE = dumped-bkmark.meta

ie6util: ie6util.ml
	ocamlopt -o ie6util $(LIB_DIRS) str.cmxa unix.cmxa $(CMX_FILES) ie6util.ml

testieget: ie6util
	./ie6util get $(FAVDIR) $(VIEW_FILE) && echo && cat $(VIEW_FILE)

testieput: testieget
	./ie6util put $(VIEW_FILE) $(MOVEDDIR)

cleanIE:
	rm -rf $(MOVEDDIR)
	rm -rf *~ *.o *.cm* ie6util $(VIEW_FILE)
