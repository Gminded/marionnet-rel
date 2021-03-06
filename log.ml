(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2008  Luca Saiu
   Copyright (C) 2010  Jean-Vincent Loddo
   Copyright (C) 2008, 2010  Université Paris 13

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

%str_item escape_raise_filter
;;

(* Initialized later, by Global_options, in order to break the ciclic dependency: *)
include Log_builder.Make (struct
  let debug_level () = 0           (* the debug_level must be greater or equal to the verbosity, otherwise do nothing *)
  let verbosity = 1                (* the default value of verbosity for printing functions *)
  let log_channel = `stderr        (* put messages here *)
  let synchronized = true          (* using threads *)
 end);;

(* Wrappers providing a logged version of functions defined elsewhere. *)

(** Wrapper for [UnixExtra.system_or_fail]: run system with the given argument,
    and raise exception in case of failure; return unit on success.
    Commands are automatically logged in debug mode. Furthermore, when debugging
    is not enable, a command redirection (/bin/sh compatible, i.e. 1>/dev/null
    2>/dev/null) is automatically appended to the command. In order to prevent
    this behaviour, the function provides the optional parameters ?hide_output
    and ?hide_errors: setting both these parameters to false, you ensure that
    nothing will be appended to the command (in debug mode or not). *)
let system_or_fail ?on_error ?hide_output ?hide_errors command_line =
  let extract_hide_decision h = match h with
  | None          -> not (Tuning.is_log_enabled ())
  | Some decision -> decision in
  let hide_output = extract_hide_decision hide_output in
  let hide_errors = extract_hide_decision hide_errors in
  printf "Executing: %s\n" command_line;
  try
    UnixExtra.system_or_fail ~hide_output ~hide_errors command_line
  with e ->
   begin
    (match on_error with
    | None         -> ()
    | Some command ->
        try UnixExtra.system_or_fail ~hide_output ~hide_errors command with _ -> ()
    );
    raise e
   end
(** Equivalent to [ignore (Unix.system command_line)] but with
    logging features. Notice that if the command_line terminates
    with '&' (background), no exceptions will be raised.
    Thus, using '&', there is no effect in setting [~force:true], because the
    shell well exit in any case. However, Log.system_or_ignore
    is preferable with respect to [(ignore (Unix.system command_line))]
    because it shows shell errors only in the debug mode. *)
let system_or_ignore ?on_error ?hide_output ?hide_errors command_line =
 try
  system_or_fail ?on_error ?hide_output ?hide_errors command_line
 with e ->
   begin
   let fmt = format_of_string "Ignoring exception: %s\n" in
   let msg = Printexc.to_string e in
   (match hide_errors with
    | None       -> printf fmt msg
    | Some false -> printf ~force:true fmt msg
    | Some true  -> ()
    )
    end

let print_backtrace () =
  printf
    "Backtrace:\n%s\n"
    (StringExtra.tab ~tab:2 (Printexc.get_backtrace ()))

(** Wrappers for system_or_ignore: the command is performed by Unix.system
    with logging features. In case of failure, the function doesn't produce
    any exception, but print the event on the log channel. *)
module Command = struct
 let ll pathname = system_or_ignore ("ls -l "^pathname)
end
