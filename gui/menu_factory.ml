(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2009, 2010  Jean-Vincent Loddo
   Copyright (C) 2009, 2010  Université Paris 13

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


(** Tools for making menus (with or without a menubar). *)

#load "include_type_definitions_p4.cmo"
;;
INCLUDE DEFINITIONS "gui/menu_factory.mli"

let fresh_path =
 let x = ref 0 in
 function () ->
  let result = "<FACTORY"^((string_of_int !x)^">/") in
  let () = incr x in
  result

(** Make a module with tools for adding and managing items to a given parent (menubar or menuitem).
   If a menubar not provided, a fresh one is created just for the factory definition.
   In this case, the connection with the menu_item_skel parent will be fixed after the
   inclusion by a calling to the function get_menu (). *)
module Make (M: Parents) = struct

 (* In the case of menu_item, this value will be defined immediately. *)
 let current_menu = ref None
 let accel_path = fresh_path ()

 let create_shell_for_simple_menu menu =
  let result = (new GMenu.factory ~accel_path menu) in
  let () = M.window#add_accel_group result#accel_group in
  let () = (current_menu := Some result)  in
  (result#menu :> GMenu.menu_shell)

 let create_subshell_for_menu_item mi =
  let simple_menu = GMenu.menu ~packing:(mi#set_submenu) () in
  create_shell_for_simple_menu simple_menu

 let shell = match M.parent with
  | Menubar  mb -> mb
  | Menuitem mi -> create_subshell_for_menu_item mi
  | Menu     m  -> create_shell_for_simple_menu m

 let factory     = new GMenu.factory ~accel_path shell
 let accel_group = factory#accel_group
 let () = M.window#add_accel_group accel_group

 (* This function is typically called only when the parent is a menu_item. *)
 let get_current_menu () = match !current_menu with
  | Some menu -> menu
  | None      -> failwith "No current menu defined in this factory."

 (* Typically used for menubars *)
 let add_menu title =
   let menu = factory#add_submenu title in
   let result = new GMenu.factory menu ~accel_path ~accel_group in
   let () = (current_menu := Some result) in
   result

 (* Useful for dynamic submenus. *)
 let recreate_subshell () = match M.parent with
  | Menuitem mi ->
     let s = match mi#get_submenu with Some x -> x | None -> assert false in
     s#destroy ();
     create_subshell_for_menu_item mi
  | Menubar  _  -> failwith "Not allowed action: this factory has been created for a menubar."
  | _  -> assert false

 (* Now tools: *)

 let not_implemented_yet _ = Log.printf "NOT IMPLEMENTED YET!!!!!\n"
 let monitor label _ = Log.printf "Menu entry with label \"%s\" selected by user\n" label

 let add_item ?(menu = get_current_menu ()) ?submenu ?(key=0) label ?(callback=(monitor label)) () =
   let result = menu#add_item label ~key ~callback in
   let () = match submenu with None -> () | Some submenu -> (result#set_submenu submenu)
   in result

 let add_stock_item ?(menu = get_current_menu ()) ?submenu ?(key=0) label ~stock ?(callback=(monitor label)) () =
   let result = menu#add_image_item ~image:(GMisc.image ~stock ())#coerce ~key ~callback ~label () in
   let () = match submenu with None -> () | Some submenu -> (result#set_submenu submenu)
   in result

 let add_imagefile_item ?(menu = get_current_menu ()) ?submenu ?(key=0) ?(label="") file ?(callback=(monitor file)) () =
   let result = menu#add_image_item ~label ~image:(GMisc.image ~file ())#coerce ~key ~callback () in
   let () = match submenu with None -> () | Some submenu -> (result#set_submenu submenu) in
   result

 let add_check_item ?(menu = get_current_menu ()) ?(active=false) ?(key=0) label ?(callback=(monitor label)) () =
   menu#add_check_item label ~key ~active ~callback


 let add_separator ?(menu = get_current_menu ()) () = ignore (menu#add_separator ())

 (* Useful shortcuts when the result of the functor is included. *)

 let parent = M.parent
 let window = M.window

end

(* Shortcuts *)
let mkenv = Environment.make_string_env

(** Useful when there is no dialog preceeding the reaction: *)
let no_dialog_but_simply_return_name = fun name () -> Some name

module Side_effects_of
  (E:sig
      type t
      val to_string : t -> string
     end)
 = struct

    let none_effect () =
      Log.printf "--- Dialog result: NOTHING TO DO (CANCELED)\n"

    let some_effect t =
      let msg =
        Printf.sprintf
          "--- Dialog result:\n%s------------------\n"
          (E.to_string t)
      in
      (Log.printf "%s" msg)
 end
 
let compose ?none_effect ?some_effect (heuristic:'a -> 'b option) (procedure:'b -> unit) =
  fun x -> match (heuristic x), none_effect, some_effect with
  | None   , None  , _      -> ()
  | None   , Some f, _      -> let () = f () in ()
  | Some y , _     , None   -> procedure y
  | Some y , _     , Some f -> let () = f y in (procedure y)


module Make_entry =
 functor (E : Entry_definition) ->
  functor (F : Factory) ->
   struct
    let item =
      let key = match E.key with None -> 0 | Some k -> k in
      F.add_stock_item ~key E.text ~stock:E.stock ()

    include Side_effects_of (E)  
    let callback = compose ~none_effect ~some_effect (E.dialog) (E.reaction)
    let connect  = item#connect#activate ~callback
   end


module Make_entry_with_children =
 functor (E : Entry_with_children_definition) ->
  functor (F : Factory) ->
   struct
    let item = F.add_stock_item E.text ~stock:E.stock ()

    (* Submenu *)
    module Submenu = Make (struct
      let parent = Menuitem (item :> GMenu.menu_item_skel)
      let window = F.window
      end)

    include Side_effects_of (E)
    let callback name = compose ~none_effect ~some_effect (E.dialog name) (E.reaction)

    let item_callback () = begin
      ignore (Submenu.recreate_subshell ());
      (List.iter
        (fun name ->
           ignore (Submenu.add_stock_item name
                      ~stock:E.stock
                      ~callback:(fun () -> callback name ())
                      ()
                   )
        )
        (E.dynlist ()))
      end

    let _ = item#connect#activate ~callback:item_callback

    let submenu = (Submenu.get_current_menu ())#menu

   end


(** {2 Examples}

include Menu_factory.Make_entry (F) (struct
   let text     = "EASY"
   let stock    = `NEW
   let dialog   = Menu_factory.no_dialog ""
   let reaction _ = ()
 end)
*)
