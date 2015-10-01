(*
  An IRC protocol analyser / synthesiser.
  Copyright (C) 2004 Samuel Mimram.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; version 2 of the License (at the exclusion
  of any other version).

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.
*)

(**
  An IRC protocol analyser / synthesiser.

  It also provides a nice OCaml module to make IRC clients or bots (see
  the Client module).

  @author Berke Durak, Samuel Mimram
*)

(* $Id$ *)

(** {1 Types} *)

(** Modes (for channels and users). *)
type mode =
  | Add_channel_mode of string * channel_mode
  | Remove_channel_mode of string * channel_mode
  | Add_user_mode of user_mode * string
  | Remove_user_mode of user_mode * string

(** Modes of channels. *)
and channel_mode =
  | Channel_mode_op of string
  | Channel_mode_private
  | Channel_mode_secret
  | Channel_mode_invite_only
  | Channel_mode_op_topic
  | Channel_mode_no_outside_messages
  | Channel_mode_moderated
  | Channel_mode_user_limit of int
  | Channel_mode_ban of string
  | Channel_mode_voice of string
  | Channel_mode_key

(** Modes of user. *)
and user_mode =
  | User_mode_invisible
  | User_mode_server_notices
  | User_mode_wallops
  | User_mode_op

(** Type of a reply from the server. *)
type reply =
  | Rpl_who
  | Rpl_end_of_who
  | Rpl_whois_channels
  | Rpl_name_reply
  | Rpl_MOTD_start
  | Rpl_MOTD
  | Rpl_end_of_MOTD

(** Sender of a message. *)
type sender

(** {1 Useful functions} *)

(** Get a string representation of a sender (it can be a servername, a nickname or an empty string). *)
val string_of_sender : sender -> string

(** Get the nick, host and user of a sender, if it is a client; otherwise, returns None *)
val nick_user_host_of_sender : sender -> (string * string * string) option

(** {2 Handling stupid mIrc color codes}
  
  The official color code list is
  - 0: white
  - 1: black
  - 2: blue     (navy)
  - 3: green
  - 4: red
  - 5: brown    (maroon)
  - 6: purple
  - 7: orange   (olive)
  - 8: yellow
  - 9: lt.green (lime)
  - 10: teal    (a kinda green/blue cyan)
  - 11: lt.cyan (cyan ?) (aqua)
  - 12: lt.blue (royal)
  - 13: pink    (light purple) (fuchsia)
  - 14: grey
  - 15: lt.grey (silver)
*)

(** Remove color codes from a string. *)
val filter_stupid_mirc_color_codes : string -> string

(** Set the foreground color of a text. *)
val color_text : int -> string -> string

(** Set the foreground and the background (arguments are in this order) colors of a text. *)
val bicolor_text : int -> int -> string -> string

(** Make a text in bold. *)
val bold_text : string -> string

(** Underline a text. *)
val underline_text : string -> string

(** Make a text in reverse colors (white becomes black, etc). *)
val reverse_text : string -> string

(** Get a text which will make a bell sound (to use with moderation). *)
val beep_text : unit -> string

(** {1 Client modules} *)

class client : string -> string -> string -> string -> string ->
object
  method set_port : int -> unit

  method connect : unit

  method disconnect : unit

  method change_user : string -> string -> string -> unit

  method change_nick : string -> unit

  method get_nick : unit -> string

  method join : string -> unit

  method part : string -> unit

  method quit : string -> unit

  method get_topic : string -> unit

  method change_topic : string -> string -> unit

  method op : string -> string -> unit

  method change_mode : mode -> unit

  method get_chan_list : unit

  method invite : string -> string -> unit

  method kick : string -> string -> string -> unit

  method get_version : unit

  method get_time : unit

  method get_info : unit

  method get_who : string -> unit

  method get_who_ircop : string -> unit

  method get_whois : string -> unit

  method get_whowas : string -> unit

  method ping : string -> unit

  method set_away : string -> unit

  method set_back : unit

  method say : string -> string -> unit

  method notice : string -> string -> unit

  method ctcp_ping : string -> float -> unit

  method on_message : sender -> string -> string -> unit

  method on_notice : sender -> string -> string -> unit

  method on_mode : sender -> mode -> unit

  method on_join : sender -> string -> unit

  method on_part : sender -> string -> string -> unit

  method on_quit : sender -> string -> unit

  method on_ping : sender -> string -> unit

  method on_reply : sender -> reply -> string array -> unit

  method on_nick : sender -> string -> unit

  method on_kick : sender -> string -> string -> string -> unit

  method on_ctcp : sender -> string -> string -> unit

  method on_ctcp_version : sender -> string -> unit

  method on_ctcp_time : sender -> string -> unit

  method on_ctcp_ping : sender -> string -> string -> unit

  method on_ctcp_ping_reply : sender -> string -> string -> unit

  method on_ctcp_action : sender -> string -> string -> unit

  method on_ctcp_reply : sender -> string -> string -> unit

  method on_raw_event : string -> unit

  method send_raw_command :string -> unit

  method event_loop : unit
  
  (* useful, if you want to replace event_loop. receives a line from the server. *)
  method private read_line : string
end

(** Parameters needed to create a client. *)
module type Client_params =
sig
  (** Address of the IRC server. *)
  val server : string

  (** Port of the IRC server. *)
  val port : int

  (** Nick to use. *)
  val nick : string

  (** Realname. *)
  val realname : string

  (** Username. *)
  val user : string

  (** Hostname. *)
  val host : string
end

(** A basic client with callbacks but no elaborate event handler. *)
module Client (P : Client_params) :
sig
  (** {2 Communication with the server} *)

  (** Connect to the server. *)
  val connect : unit -> unit

  (** Disconnect from the server. *)
  val disconnect : unit -> unit

  (** Start an event loop. This function does not return unless we are disconnected from the server. It reads messages from the server and calls the appropriate functions to handle them. *)
  val event_loop : unit -> unit

  (** {2 IRC commands} *)

  (** {3 Administrativia} *)

  (** Join a channel. *)
  val join : string -> unit

  (** Part from a channel. *)
  val part : string -> unit

  (** Quit the server. [disconnect] should be called after that. *)
  val quit : string -> unit

  (** [change_user nick host user] changes your nickname, hostname and username. *)
  val change_user : string -> string -> string -> unit

  (** Change your nickname. *)
  val change_nick : string -> unit

  (** Get current nickname *)
  val get_nick : unit -> string

  (** [kick user chan reason] kicks the user [user] from the channel [chan] giving him [reason] as explanation. *)
  val kick : string -> string -> string -> unit

  (** [invlite user chan] *)
  val invite : string -> string -> unit

  val op : string -> string -> unit

  (** Set yourself as away. *)
  val set_away : string -> unit

  (** You're not away anymore. *)
  val set_back : unit -> unit

  (** [change_topic chan topic] *)
  val change_topic : string -> string -> unit

  (** {3 Basic communication} *)

  (** Say something to someone / to a chan (private message). *)
  val say : string -> string -> unit

  (** Say something to someone / to a chan in notice. *)
  val notice : string -> string -> unit

  val ctcp_ping : string -> float -> unit  

  val get_topic : string -> unit

  val change_mode : mode -> unit

  val get_chan_list : unit -> unit

  val get_version : unit -> unit

  val get_time : unit -> unit

  val get_info : unit -> unit

  val get_who : string -> unit

  val get_who_ircop : string -> unit

  val get_whois : string -> unit

  val get_whowas : string -> unit

  val ping : string -> unit

  val on_message : (sender -> string -> string -> unit) ref

  val on_notice : (sender -> string -> string -> unit) ref

  val on_mode : (sender -> mode -> unit) ref

  val on_nick : (sender -> string -> unit) ref

  (** [on_kick sender chan user reason] *)
  val on_kick : (sender -> string -> string -> string option -> unit) ref

  val on_reply : (sender -> reply -> string array -> unit) ref

  val on_ctcp_version : (sender -> string -> unit) ref

  val on_ctcp_time : (sender -> string -> unit) ref

  val on_ctcp_ping : (sender -> string -> string -> unit) ref

  val on_ctcp_action : (sender -> string -> string -> unit) ref

  val on_ctcp : (sender -> string -> string -> unit) ref

  val on_ctcp_ping_reply : (sender -> string -> string -> unit) ref

  val on_ctcp_reply : (sender -> string -> string -> unit) ref

  val on_ping : (sender -> string -> unit) ref

  val on_join : (sender -> string -> unit) ref

  val on_part : (sender -> string -> string option -> unit) ref

  val on_quit : (sender -> string option -> unit) ref

  val on_error : (sender -> string -> unit) ref

(*
  val on_event : (message -> unit) ref
*)

  (** {2 Debugging} *)

  (** If it's set to [true], the module shows every message that is received from the server on stdout (default: [false]). *)
  val dump_everything : bool ref
end
