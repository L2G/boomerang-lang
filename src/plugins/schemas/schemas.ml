(* REAL SCHEMAS *)
(* some common schemas used in the plugins *)

(* an empty type, used as a placeholder *)
let empty = Types.string2abstract_type ""
	    
let all = Types.string2abstract_type "type X = *[X]"
  
(* abstract schema for appointments *)
let appointments = 
  (* under time, the VAL should be hh:mm with hh in 00..23 and mm in 00..59
   * under notice, the VAL should be an integer
   * under freq, the VAL should be an integer
   * under monthlybyday, the VAL should be 1stSun | 1stMon | 1stTue | ... | 4thFri | 4th Sat | LastSun ...
   * under enddate, the VAL should be mm-dd-yyyy
   * under EXCLIST, the VAL should be mm-dd-yyyy
   * the type DOW should be of the form:
   *   type DOW = ONEDOW | ONEDOW.DOW
   *   type ONEDOW = Sun[{}] | Mon[{}] | Tue[{}] | Wed[{}] | Thu[{}] | Fri[{}] | Sat[{}]
   * but this does not work, not does an explicit expansion

   * JNF - you can't use recursion in the width of types as in
   *          DOW = ONEDOW | ONEDOW.DOW 
   * i.e., a use of a type variable has to be one level down.  I'm not
   * sure what you tried when you did an "explicit expansion". if you
   * merely expanded ONEDOW, then it's still not going to work because
   * DOW is being used at the same level that it's defined at. The way
   * you could get this to work is expand out all the types: all 7
   * singleton DOWs, all 21 ways of chosing two DOWs. Once we get the
   * FTTs fully implemented, you could also write the type as something like
   *     Sun?[{}].*[{}] & ... & Sat?[{}].*[{}]
   * In fact, it seems useful to have some nice sugar for writing
   * types like this.
   *)
  Types.string2abstract_type 
    "type CAL = *[ENTRY]
     type ENTRY = date[VAL].desc[VAL].OPTTIME.OPTALARM.OPTREPEAT
     type VAL = ![{}]
     type OPTTIME = time?[begin[VAL].end[VAL]]
     type OPTALARM = alarm?[units[UNITS].notice[VAL]]
     type UNITS = Minutes[{}] | Hours[{}] | Days[{}]
     type OPTREPEAT = repeat?[\"type\"[RTYPE].freq[VAL].OPTENDDATE.OPTEXCEPTIONS]
     type RTYPE = daily[{}] | weekly[DOW] | monthlybyday[VAL] | monthlybydate[{}] | yearly[{}]
     type DOW = *[{}]
     type OPTENDDATE = enddate?[VAL]
     type OPTEXCEPTIONS = exceptions?[EXCLIST]
     type EXCLIST = \"*h\"[VAL].\"*t\"[EXCLIST] | \"*nil\"[{}]"

(* BOOKMARKS *)
let bookmarks = 
  Types.string2abstract_type
    "type AContents = ListAItem
     type Val = ![{}]  
     type All = *[All]
     type ALink1 = name[Val].url[Val]
     type ALink  = link[ALink1]
     type AFolder1 = name[Val].contents[AContents]
     type AFolder = folder[AFolder1]
     type AItem = ALink | AFolder
     type ListAItem = \"*h\"[AItem].\"*t\"[ListAItem] | {}"

(* STRUCTURED - is this just a list of trees? *)
let structured = 
  Types.string2abstract_type
     "type LIST = \"*h\"[ANY] . \"*t\"[LIST] | {}
      type ANY = *[ANY]"
