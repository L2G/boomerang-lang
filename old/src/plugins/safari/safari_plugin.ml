open Pervasives
(*
let leaf_lens =
  tracepoint "leaf_lens" [
    hoist "";
    default V.empty_list (V.cons (V.field_value "PCDATA" "BLANK") V.empty_list);
    head V.empty_list;
    hoist "PCDATA";
  ]

let rec keypair_lens () =
  compose [
    map_head
      (compose [
         hoist "key";
         leaf_lens;
       ]);
    map_tail (map_head (protect plist_object_lens));
  ]

and dict_lens () =
  compose [
    hoist "";
    groupby 2;
    map_list (protect keypair_lens);
  ]

and array_lens () =
  compose [
    hoist "";
    map_list (protect plist_object_lens);
  ]

and plist_object_lens () =
  compose [
    (* I'm not confident that this is the best possible way to do this *)
    mapp (Prd.s "dict") (protect dict_lens);
    mapp (Prd.s "array") (protect array_lens);
    mapp (Prd.s "string") leaf_lens; 
  ]

let plist_lens =
  tracepoint "plist_lens" [
    hoist "";
    head V.empty;
    hoist "plist";
    focus "" V.empty;
    head V.empty;
    protect plist_object_lens;
  ]

(* ----------------------------- *)

(* N.b.: in the Safari bookmark format, the URL of a link is stored in two
   places(!) -- in the URLString field and in the key field of the
   URIDictionary subtree.  From experiments with Safari, it appears that the
   former is the real one, and even OMITTING the latter doesn't cause any harm.

   [later] Unfortunately, this is not true in later versions of Safari.  Both
   must be present. 
*)

let flatten_dict =
  tracepoint "safari_flatten_dict" [
    hoist "dict";
    map_list
      (compose [
         rename_cons "h" "t";
         pivot "h";
         mapc [focus "t" (V.atomic V.empty); head V.empty_list];
       ]);
    flatten;
  ]

let safari_link_lens =
  tracepoint "safari_link_lens" [
    rename [("WebBookmarkTypeLeaf","link")];
    mapp (Prd.s "link") 
      (compose [
         xfork (Prd.s "URIDictionary") (Prd.m ["name";"urlcopy"])
           (tracepoint "fork1" [
              hoist "URIDictionary";
              flatten_dict;
              filter (Prd.m ["title";"BLANK"]) V.empty;
              rename [("title","name");("BLANK","urlcopy")];
              map (hoist "string");
            ])
           (compose [rename [("URLString","url")]; map (hoist "string")]);
         merge "url" "urlcopy";
(*
         map (Prd.s "dict") V.empty
              (filter (Prd.s "title"));
         hoist "dict";
*)
       ]);
  ]

let safari_proxy_lens =
  tracepoint "safari_proxy_lens" [
    rename [("Title","name")];
    mapp (Prd.s "name") (hoist "string");
  ]

let rec safari_folder_lens () =
  tracepoint "safari_folder_lens" [
    rename [("WebBookmarkTypeList","folder")];
    mapc [
      mapp (Prd.s "Children")
        (compose [
           hoist "array";
           map_list (protect safari_item_lens);
         ]);
      mapp (Prd.s "Title") (hoist "string");
      rename [("Title","name"); ("Children","contents")];
      mapp (Prd.s "name")
       (rename [("BookmarksBar",Bookmarks.toolbar_folder_name);
                ("Top-level bookmark folder", Bookmarks.top_level_folder_name)]);
    ];
  ]

and safari_item_lens () =
  tracepoint "safari_item_lens" [
    flatten_dict;
    filter (Prd.neg (Prd.s "WebBookmarkUUID")) V.empty;
    mapp (Prd.s "WebBookmarkType") (hoist "string");
    pivot "WebBookmarkType";
    dispatch [
      (Prd.s "WebBookmarkTypeList"), (Prd.s "folder"),
         protect safari_folder_lens;
      (Prd.s "WebBookmarkTypeLeaf"), (Prd.s "link"),
         safari_link_lens;
      (Prd.s "WebBookmarkTypeProxy"), (Prd.s "WebBookmarkTypeProxy"),
         safari_proxy_lens;
      (* ... *)
    ]
  ]

and toplevel_lens () =
  tracepoint "safari_toplevel_lens" [
    flatten_dict;
    filter (Prd.neg (Prd.s "WebBookmarkUUID")) V.empty;
    mapp (Prd.s "WebBookmarkType") (hoist "string");
    pivot "WebBookmarkType";
    protect toplevel_folder_lens;
  ]

and toplevel_folder_lens () =
  compose [
    rename [("WebBookmarkTypeList","folder")];
    mapc [
      mapp (Prd.s "Title") (hoist "string");
      rename [("Title","name"); ("Children","contents")];
      mapp (Prd.s "name")
       (rename ["Top-level bookmark folder", Bookmarks.top_level_folder_name]);
      mapp (Prd.s "contents")
        (compose [
           hoist "array";
           map_head (protect safari_item_lens);
           map_tail (compose [
                      map_head (compose [
                        flatten_dict;
                        filter (Prd.neg (Prd.s "WebBookmarkUUID")) V.empty;
                        mapp (Prd.s "WebBookmarkType") (hoist "string");
                        pivot "WebBookmarkType";
                        protect safari_folder_lens;
                        hoist "folder";
                        filter (Prd.s "contents")
                             (V.from_desc (V.V ["name",V.Val "BookmarksMenu"]));
                        hoist "contents";
                      ]);
                      head V.empty;
                      (*map_tail (map_list (protect safari_item_lens));*)
                    ]);
         ]);
      prune "WebBookmarkFileVersion"
        (V.from_desc
          (V.V ["integer", V.V ["",V.L [V.V ["PCDATA",V.Val "1"]]]]));
    ];
  ]

let remove_proxy =
  tracepoint "Safari.remove_proxy" [
    map
     (mapp (Prd.s "contents")
           (list_filter (fun v -> Name.Set.mem "link" (V.dom v) or
                                  Name.Set.mem "folder" (V.dom v))));
  ]

(*let toolbar_folder =
  compose [
    lens;
    Bookmarks.focus_item "Bookmarks Bar"
  ]*)

let lens = protect toplevel_lens
 *)

let () =
  (* for now, just check the filename (FIXME!) *)
  let etest filename copt = Misc.filename_extension filename = "plist" in
  let encoding = {
    Surveyor.description = "Safari browser bookmarks";
    Surveyor.encoding_test = etest;
    Surveyor.reader = Xml.simple_reader;
    Surveyor.writer = Xml.simple_writer;
    Surveyor.from_string = Xml.from_string;
    Surveyor.to_string = Xml.to_string;
    Surveyor.base_type = [ "xml"; "plist" ; "safari"];
  }
  in    
    Surveyor.register_encoding "safari" encoding;
    Optometrist.register_lens ["xml"; "plist"] ["plist"] 
      Schemas.empty 
      Pervasives_plugin.id;
    Optometrist.register_lens ["plist"; "safari"] ["safari"] 
      Schemas.empty
      Pervasives_plugin.id;
    Optometrist.register_lens ["safari"] ["bookmarks"]
      Schemas.bookmarks
      Pervasives_plugin.id
