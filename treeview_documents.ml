(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008, 2009  Luca Saiu
   Copyright (C) 2009, 2010  Jean-Vincent Loddo
   Copyright (C) 2007, 2008, 2009, 2010  Université Paris 13

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

(* Authors:
 * - Luca Saiu: initial version
 * - Jean-Vincent Loddo: Unix.system calls replaced by UnixExtra's functions
     calls, and some other minor changes
 *)

open Row_item;;
open Gettext;;

class t =
fun ~packing
    ~after_user_edit_callback
    () ->
object(self)
  inherit
    Treeview.t
      ~packing
      ~hide_reserved_fields:true
      ()
  as super

  (** Display the document at the given row, in an asynchronous process: *)
  method private display row_id =
    let format = item_to_string (self#get_row_item row_id "Format") in
    let reader = self#format_to_reader format in
    let file_name = item_to_string (self#get_row_item row_id "FileName") in
    let command_line =
      Printf.sprintf "%s '%s/%s'&" reader (Option.extract directory#get) file_name in
    (* Here ~force:true would be useless, because of '&' (the shell well exit in any case). *)
    Log.system_or_ignore command_line

  val error_message =
    (s_ "You should select an existing document in PDF, Postscript, DVI, HTML or text format.")

  (** Ask the user to choose a file, and return its pathname. Fail if the user doesn't
      choose a file or cancels: *)
  method private ask_file =
    let dialog = GWindow.file_chooser_dialog
        ~icon:Icon.icon_pixbuf
        ~action:`OPEN
        ~title:((*utf8*)(s_ "Choose the document to import"))
        ~modal:true () in
    dialog#add_button_stock `CANCEL `CANCEL;
    dialog#add_button_stock `OK `OK;
    dialog#unselect_all;
    dialog#add_filter
      (GFile.filter
         ~name:(s_ "Texts (PDF, PostScript, DVI, HTML, text)")
         ~patterns:["*.pdf"; "*.ps"; "*.dvi"; "*.text"; "*.txt"; "*.html"; "*.htm"; "README";
                    (s_ "README") (* it's nice to also support something like LISEZMOI... *)]
         ());
    dialog#set_default_response `OK;
    (match dialog#run () with
      `OK ->
        (match dialog#filename with
          Some result ->
            dialog#destroy ();
            Log.printf "* Ok: \"%s\"\n" result; flush_all ();
            result
        | None -> begin
            dialog#destroy ();
            failwith "No document was selected"
          end)
    | _ ->
        dialog#destroy ();
        Log.printf "* Cancel\n"; flush_all ();
        failwith "You cancelled");


  method private file_to_format pathname =
    if Filename.check_suffix pathname ".html" or
      Filename.check_suffix pathname ".htm" or
      Filename.check_suffix pathname ".HTML" or
      Filename.check_suffix pathname ".HTM" then
      "html"
    else if Filename.check_suffix pathname ".text" or
      Filename.check_suffix pathname ".txt" or
      Filename.check_suffix pathname "readme" or
      Filename.check_suffix pathname "lisezmoi" or
      Filename.check_suffix pathname ".TEXT" or
      Filename.check_suffix pathname ".TXT" or
      Filename.check_suffix pathname "README" or
      Filename.check_suffix pathname "LISEZMOI" then
      "text"
    else if Filename.check_suffix pathname ".ps" or
      Filename.check_suffix pathname ".eps" or
      Filename.check_suffix pathname ".PS" or
      Filename.check_suffix pathname ".EPS" then
      "ps"
    else if Filename.check_suffix pathname ".dvi" or
      Filename.check_suffix pathname ".DVI" then
      "dvi"
    else if Filename.check_suffix pathname ".pdf" or
      Filename.check_suffix pathname ".PDF" then
      "pdf"
    else
      failwith ("I cannot recognize the file type of " ^ pathname);

  method private format_to_reader format =
    match format with
    | "pdf" ->
        Initialization.configuration#string "MARIONNET_PDF_READER"
    | "ps" ->
        Initialization.configuration#string "MARIONNET_POSTSCRIPT_READER"
    | "dvi" ->
        Initialization.configuration#string "MARIONNET_DVI_READER"
    | "html" -> (* 'file' may recognize (X)HTML as XML... *)
        Initialization.configuration#string "MARIONNET_HTML_READER"
    | "text" ->
        Initialization.configuration#string "MARIONNET_TEXT_EDITOR"
    | "auto" -> (* the file type in unknown: web browsers can open most everything... *)
        Initialization.configuration#string "MARIONNET_HTML_READER"
    | _ ->
      failwith ("The format \"" ^ format ^ "\" is not supported");

  (** Import the given file, copying it into the appropriate directory with a fresh name;
      return the fresh name (just the file name, not a complete pathname) and the name
      of an application suitable to read it, as a pair. In case of failure show an error
      message and raise an exception. If ~move is true then the file is moved instead of
      copied. *)
  method private import_file ?(move=false) pathname =
    try
      let file_format    = self#file_to_format pathname in
      let parent         = Option.extract directory#get in
      let fresh_pathname = UnixExtra.temp_file ~parent ~prefix:"document-" () in
      let fresh_name     = Filename.basename fresh_pathname in
      let result         = (fresh_name, file_format) in
     (try
      (match move with
      | false -> UnixExtra.file_copy pathname fresh_pathname
      | true  -> UnixExtra.file_move pathname fresh_pathname
      );
      UnixExtra.set_perm ~a:() ~w:false fresh_pathname;
      Log.Command.ll fresh_pathname;
      result
      with Unix.Unix_error (_,_, _) ->
       begin
         UnixExtra.apply_ignoring_Unix_error Unix.unlink fresh_pathname;
         let title =
           Printf.sprintf "Failed copying the file \n\"%s\"\n" pathname in
         failwith title;
       end)
     with (Failure title) as e -> begin
      Simple_dialogs.error title error_message ();
      raise e (* Re-raise *)
    end

  method import_report ~machine_or_router_name ~pathname () =
    let title = (s_ "Report on ") ^ machine_or_router_name in
    let row_id = self#import_document ~move:true pathname in
    self#set_row_item row_id "Title" (String title);
    self#set_row_item row_id "Author" (String "-");
    self#set_row_item row_id "Type" (String (s_ "Report"));
    self#set_row_item row_id "Comment" (String ((s_ "created on ") ^ (UnixExtra.date ~dot:" " ())));

  method import_history ~machine_or_router_name ~pathname () =
    let title = (s_ "History of ") ^ machine_or_router_name in
    let row_id = self#import_document ~move:true pathname in
    self#set_row_item row_id "Title" (String title);
    self#set_row_item row_id "Author" (String "-");
    self#set_row_item row_id "Type" (String (s_ "History"));
    self#set_row_item row_id "Comment" (String ((s_ "created on ") ^ (UnixExtra.date ~dot:" " ())));

  method import_document ?(move=false) user_path_name =
    let internal_file_name, format = self#import_file user_path_name in
    let row_id =
      self#add_row
        [ "FileName", String internal_file_name;
          "Format", String format ] in
    row_id

  initializer
    let _ =
      self#add_icon_column
        ~shown_header:(s_ "Icon")
        ~header:"Icon"
        ~strings_and_pixbufs:[ "text", Initialization.Path.images^"treeview-icons/text.xpm"; ]
        ~default:(fun () -> Icon "text")
        () in
    let _ =
      self#add_editable_string_column
        ~shown_header:(s_ "Title")
        ~header:"Title"
        ~italic:true
        ~default:(fun () -> String "Please edit this")
        () in
    let _ =
      self#add_editable_string_column
        ~shown_header:(s_ "Author")
        ~header:"Author"
        ~italic:false
        ~default:(fun () -> String "Please edit this")
        () in
    let _ =
      self#add_editable_string_column
        ~shown_header:(s_ "Type")
        ~header:"Type"
        ~italic:false
        ~default:(fun () -> String "Please edit this")
        () in
    let _ =
      self#add_editable_string_column
        ~shown_header:(s_ "Comment")
        ~header:"Comment"
        ~italic:true
        ~default:(fun () -> String "Please edit this")
        () in
    let _ =
      self#add_string_column
        ~header:"FileName"
        ~hidden:true
        () in
    let _ =
      self#add_string_column
        ~header:"Format"
        ~default:(fun () -> String "auto") (* unknown format; this is usefule for
                                              backward-compatibility, as this column
                                              didn't exist in older Marionnet versions *)
        ~hidden:true
        () in
    (* Make internal data structures: no more columns can be added now: *)
    self#create_store_and_view;

    (* Setup the contextual menu: *)
    self#set_contextual_menu_title "Texts operations";
    self#add_menu_item
      (s_ "Import a document")
      (fun _ -> true)
      (fun _ ->
        ignore (self#import_document self#ask_file));

    self#add_menu_item
      (s_ "Display this document")
      Option.to_bool
      (fun selected_rowid_if_any ->
        let row_id = Option.extract selected_rowid_if_any in
        self#display row_id);
    self#set_double_click_on_row_callback (fun row_id -> self#display row_id);

    self#add_menu_item
      (s_ "Remove this document")
      Option.to_bool
      (fun selected_rowid_if_any ->
        let row_id = Option.extract selected_rowid_if_any in
        let file_name = item_to_string (self#get_row_item row_id "FileName") in
        let pathname = Printf.sprintf "%s/%s" (Option.extract directory#get) file_name in
        UnixExtra.apply_ignoring_Unix_error Unix.unlink pathname;
        self#remove_row row_id;
        );

     (* J.V. *)
     self#set_after_update_callback after_user_edit_callback;

end;;

class treeview = t
module The_unique_treeview = Stateful_modules.Variable (struct
  type t = treeview
  let name = Some "treeview_documents"
  end)
let extract = The_unique_treeview.extract

let make ~packing ~after_user_edit_callback () =
  let result = new t ~packing ~after_user_edit_callback () in
  The_unique_treeview.set result;
  result
;;
