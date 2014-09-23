(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2014		Gianluca Guidi

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

open Gettext
open Gui_bricks

module Make (State:sig val st:State.globalState end) = struct
    open State
    type reltime_t = {freq: float; conv: int; existing: bool}
    let make_window ~title () =
        let w = GWindow.dialog ~destroy_with_parent:true ~title
                ~modal:true ~position:`CENTER () in
        set_marionnet_icon w;
        let tooltips = make_tooltips_for_container w in
        let hbox1 = GPack.hbox ~spacing:10 ~packing:(w#vbox#pack
          ~expand:true ~fill:false) () in
        let vbox1 = GPack.vbox ~spacing:10 ~packing:(hbox1#pack
            ~expand:true ~fill:false ~padding:20) () in
        let _ = GMisc.label ~xalign:0. 
                ~text:(s_ "Set default relativization parameters for new machines and routers.")
                ~packing:(vbox1#pack ~padding:20) () in
        let table = GPack.table ~rows:3 ~columns:2
                ~col_spacings:5 ~row_spacings:10
                  ~packing:(vbox1#pack ~expand:false) () in
        let label_frequency = GMisc.label ~xalign:0.
                ~text:(s_ "Frequency")
                ~packing:(table#attach ~left:0 ~top:0) () in
        let label_convergence = GMisc.label ~xalign:0.
                ~text:(s_ "Convergence")
                ~packing:(table#attach ~left:0 ~top:1) () in
        let time_frequency = spin_freq ~packing:(table#attach
                ~left:1 ~top:0) (Machine.Const.time_frequency_default ()) in
        let time_convergence = spin_seconds
                ~packing:(table#attach ~left:1 ~top:1)
                (Machine.Const.time_convergence_default ()) in
        let label_existing_machines = GMisc.label ~xalign:0.
            ~text:(s_ "Apply to existing machines") 
            ~packing:(table#attach ~left:0 ~top:2) () in
        let existing_machines = GButton.check_button 
            ~active:true ~packing:(table#attach ~left:1 ~top:2) () in
        let frequency_tooltip = (s_ "Frequency of the virtual time for this machine, in Hz.") in
        let convergence_tooltip = (s_ "Point of convergence between virtual and real time, expressed as the number of seconds since the Epoch.") in
        let existing_machines_tooltip = (s_ "Also apply these parameters to existing powered-off machines and routers.") in
        tooltips label_frequency#coerce frequency_tooltip;
        tooltips time_frequency#coerce frequency_tooltip;
        tooltips label_convergence#coerce convergence_tooltip;
        tooltips time_convergence#coerce convergence_tooltip;
        tooltips label_existing_machines#coerce existing_machines_tooltip;
        tooltips existing_machines#coerce existing_machines_tooltip;
        let get_widget_data () = {
            freq = time_frequency#value; 
            conv = int_of_float time_convergence#value;
            existing = existing_machines#active
        } in
        let ok_callback reltime = if reltime.freq <> 0. then Some reltime
        else None in
        Gui_bricks.Dialog_run.ok_or_cancel w ~get_widget_data ~ok_callback ()


    (* If the checkbox was checked, we have to iterate over the existing devices
     * and update their time_frequency and time_convergence fields (only).*)
    let update_existing_devices ~result () =
        let machines = st#network#get_devices_that_can_startup ~devkind:`Machine in
        let update_machine name =
            let m = st#network#get_device_by_name name in
            let m = ((Obj.magic m):>
            Machine.User_level_machine.machine) in

            (* Get existing values in order to reuse the existing method
             * "update_machine_with"*)
            let label = m#get_label in
            let memory = m#get_memory in
            let port_no = m#get_port_no in
            let kernel = m#get_kernel in
            let terminal = m#get_terminal in

            (* New values*)
            let time_frequency = string_of_float result.freq in
            let time_convergence = string_of_int result.conv in

            m#update_machine_with ~name ~label ~memory ~port_no ~kernel
            ~terminal ~time_frequency ~time_convergence
        in
        let routers = st#network#get_devices_that_can_startup ~devkind:`Router in
        let update_router name =
            let r = st#network#get_device_by_name name in
            let r = ((Obj.magic r):>
            Router.User_level_router.router) in

            (* Get existing values in order to reuse the existing method
             * "update_router_with"*)
            let label = r#get_label in
            let kernel = r#get_kernel in
            let show_unix_terminal = r#get_show_unix_terminal in
            let port_no = r#get_port_no in
            let port_0_ip_config = r#get_port_0_ip_config in

            (* New values*)
            let time_frequency = string_of_float result.freq in
            let time_convergence = string_of_int result.conv in

            r#update_router_with
              ~name ~label ~port_0_ip_config ~port_no ~kernel ~show_unix_terminal
              ~time_frequency ~time_convergence
        in
        let rec loop ~f ~devices ()  =
            match devices with
            | [] -> ()
            | head :: tail -> f head; loop ~f ~devices:tail ()
        in
        loop ~f:update_machine ~devices:(machines ()) ();
        loop ~f:update_router ~devices:(routers ()) ()

    let apply_changes ~result () =
        Machine.Const.set_time_frequency_default result.freq;
        Machine.Const.set_time_convergence_default result.conv;
        Router.Const.set_time_frequency_default result.freq;
        Router.Const.set_time_convergence_default result.conv;
        if result.existing then
            update_existing_devices ~result ()

end
