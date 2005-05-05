#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <tidy.h>
#include <tidyenum.h>
#include <errno.h>

char *helper_tidySaveString(TidyDoc tdoc);
int helper_tidyOptSetBool(TidyDoc tdoc, TidyOptionId id, int b);

/* stub functions for calling tidy functions directly from ocaml */

value tidy_tidyCreate(value unit) {
  CAMLparam1 (unit);
  CAMLreturn ((value) tidyCreate());
}

value tidy_tidyRelease(value tdoc) {
  CAMLparam1 (tdoc);
  tidyRelease ((TidyDoc)tdoc);
  CAMLreturn (Val_unit);
}

value tidy_tidyReleaseDate(value unit) {
  CAMLparam1 (unit);
  CAMLreturn ((value) copy_string(tidyReleaseDate()) );
}

value tidy_tidyParseString(value tdoc, value content) {
  CAMLparam2 (tdoc, content);
  CAMLreturn (Val_int(tidyParseString((TidyDoc)tdoc, String_val(content))));
}

value tidy_tidyCleanAndRepair(value tdoc) {
  CAMLparam1 (tdoc);
  CAMLreturn (Val_int(tidyCleanAndRepair((TidyDoc)tdoc)));
}

value tidy_tidyRunDiagnostics(value tdoc) {
  CAMLparam1 (tdoc);
  CAMLreturn (Val_int(tidyRunDiagnostics((TidyDoc)tdoc)));
}

value tidy_tidySaveFile(value tdoc, value filename) {
  CAMLparam2 (tdoc, filename);
  CAMLreturn (Val_int(tidySaveFile((TidyDoc)tdoc, String_val(filename))));
}

/* NOTE: this is more than just a stub function; uses the helper function.. */
value tidy_tidySaveString(value tdoc) {
  CAMLparam1 (tdoc);
  CAMLreturn ((value) copy_string(helper_tidySaveString((TidyDoc)tdoc)));
}

value tidy_tidySetCharEncoding(value tdoc, value encnam) {
  CAMLparam2 (tdoc, encnam);
  CAMLreturn (Val_int(tidySetCharEncoding((TidyDoc)tdoc, String_val(encnam))));
}

value tidy_tidyDetectedHtmlVersion(value tdoc) {
  CAMLparam1 (tdoc);
  CAMLreturn (Val_int(tidyDetectedHtmlVersion((TidyDoc)tdoc)));
}

/* these need to be tested for boolean reasons */
value tidy_tidyDetectedXhtml(value tdoc) {
  CAMLparam1 (tdoc);
  CAMLreturn (tidyDetectedXhtml((TidyDoc)tdoc) ? Val_true : Val_false);
}

value tidy_tidyDetectedGenericXml(value tdoc) {
  CAMLparam1 (tdoc);
  CAMLreturn (tidyDetectedGenericXml((TidyDoc)tdoc) ? Val_true : Val_false);
}

value tidy_tidySetShowWarnings(value b, value tdoc) {
  CAMLparam2 (b, tdoc);
  CAMLreturn (Val_bool(helper_tidyOptSetBool((TidyDoc)tdoc,
                                             TidyShowWarnings,
                                             Bool_val(b))));
}

value tidy_tidySetXmlOut(value b, value tdoc) {
  CAMLparam2 (b, tdoc);
  CAMLreturn (Val_bool(helper_tidyOptSetBool((TidyDoc)tdoc,
                                             TidyXmlOut,
                                             Bool_val(b))));
}

value tidy_tidySetXhtmlOut(value b, value tdoc) {
  CAMLparam2 (b, tdoc);
  CAMLreturn (Val_bool(helper_tidyOptSetBool((TidyDoc)tdoc,
                                             TidyXhtmlOut,
                                             Bool_val(b))));
}

value tidy_tidySetHtmlOut(value b, value tdoc) {
  CAMLparam2 (b, tdoc);
  CAMLreturn (Val_bool(helper_tidyOptSetBool((TidyDoc)tdoc,
                                             TidyHtmlOut,
                                             Bool_val(b))));
}

value tidy_tidySetQuiet(value b, value tdoc) {
  CAMLparam2 (b, tdoc);
  CAMLreturn (Val_bool(helper_tidyOptSetBool((TidyDoc)tdoc,
                                             TidyQuiet,
                                             Bool_val(b))));
}

value tidy_tidySetMark(value b, value tdoc) {
  CAMLparam2 (b, tdoc);
  CAMLreturn (Val_bool(helper_tidyOptSetBool((TidyDoc)tdoc,
                                             TidyMark,
                                             Bool_val(b))));
}

value tidy_tidySetNumEntities(value b, value tdoc) {
  CAMLparam2 (b, tdoc);
  CAMLreturn (Val_bool(helper_tidyOptSetBool((TidyDoc)tdoc,
                                             TidyNumEntities,
                                             Bool_val(b))));
}


value tidy_tidySetWrapLen(value i, value tdoc) {
  CAMLparam2 (i, tdoc);
  CAMLreturn (Val_bool(helper_tidyOptSetInt((TidyDoc)tdoc,
                                            TidyWrapLen,
                                            Int_val(i))));
}


int helper_tidyOptSetInt(TidyDoc tdoc, TidyOptionId id, int i) {
  Bool ret = tidyOptSetInt(tdoc, id, i);
  return (ret ? 1 : 0);
}

int helper_tidyOptSetBool(TidyDoc tdoc, TidyOptionId id, int b) {
  Bool ret = tidyOptSetBool(tdoc, id, (b ? yes : no));
  return (ret ? 1 : 0);
}

/*
 * Grows a malloc'd buffer to fit the tidy document into, and returns 
 * said string if possible.
 */
char *helper_tidySaveString(TidyDoc tdoc) {
  int err;
  char *buf;
  int old_bufsize;
  int bufsize = 1024; // start at 1024
  buf = malloc(bufsize);
  tidySaveString (tdoc, buf, &bufsize);
  // if there wasn't enough space, tidySaveString put the right
  // size into the variable bufsize.
  if (bufsize != 1024) {
    //printf ("bufsize got changed to %d\n", bufsize);
    //printf ("not enough space first time, growing..\n");
    free(buf);
    buf = malloc(bufsize);
    old_bufsize = bufsize;
    tidySaveString (tdoc, buf, &bufsize);
    if (bufsize != old_bufsize) {
      // NOW we're really in trouble.
      return "[tidy] not enough memory!";
    }
  }
  return buf;
}
