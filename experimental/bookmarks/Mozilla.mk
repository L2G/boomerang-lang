MOZ_PREPROCESS_SRC = mozillaPreProcess.ml
MOZ_PREPROCESS_LIB = unix.cma str.cma
MOZ_PREPROCESS_TARGET = mPP

MOZ_TEST_FILE = bookmarks.html
MOZ_TMP_FILE = bookmarks.xml
MOZ_OUTPUT_FILE = output.meta

mpp: $(MOZ_PREPROCESS_SRC)
	ocamlc -o $(MOZ_PREPROCESS_TARGET) $(MOZ_PREPROCESS_LIB) $(MOZ_PREPROCESS_SRC)

tidy: mpp $(MOZ_TEST_FILE)
# /dev/null has to be removed, I was just fed up with the loooooong error output of tidy ;)
#	-tidy -f /dev/null -asxml $(MOZ_TEST_FILE) > $(MOZ_TMP_FILE)

preprocess: tidy
	./$(MOZ_PREPROCESS_TARGET) $(MOZ_TMP_FILE)

