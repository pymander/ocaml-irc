(*pp camlp4o *)

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

(* $Id$ *)

(*
 An IRC protocol analyser / synthesiser.

 It also provides a nice OCaml module to make IRC clients or bots (see
 the Client module).

 @author Berke Durak, Samuel Mimram
 *)

(*
 NOTE: Please see the Client module at the end of the file. You shouldn't
 have to deal with low-level aspects of the IRC protocol.
 *)

type command =
  | Cmd_none
  | Cmd_password
  | Cmd_nick
  | Cmd_user
  | Cmd_server
  | Cmd_oper
  | Cmd_quit
  | Cmd_SQuit
  | Cmd_join
  | Cmd_part
  | Cmd_mode
  | Cmd_topic
  | Cmd_names
  | Cmd_list
  | Cmd_invite
  | Cmd_kick
  | Cmd_version
  | Cmd_stats
  | Cmd_links
  | Cmd_time
  | Cmd_trace
  | Cmd_admin
  | Cmd_info
  | Cmd_priv_msg
  | Cmd_notice
  | Cmd_who
  | Cmd_whois
  | Cmd_whowas
  | Cmd_kill
  | Cmd_ping
  | Cmd_pong
  | Cmd_error
  | Cmd_away
  | Cmd_reply of reply
  | Err_nickname_in_use
  | Cmd_numeric of int
  | Cmd_generic of string

and reply =
  | Rpl_who
  | Rpl_end_of_who
  | Rpl_whois_channels
  | Rpl_name_reply
  | Rpl_MOTD_start
  | Rpl_MOTD
  | Rpl_end_of_MOTD

type prefix =
  | Prefix_none
  | Prefix_server of string
  | Prefix_user of prefix_user

and prefix_user =
  {
    ipn_nick: string;
    ipn_user: string;
    ipn_host: string;
  }

type token =
  | Servername of string
  | Command of command

type mode =
  | Add_channel_mode of string * channel_mode
  | Remove_channel_mode of string * channel_mode
  | Add_user_mode of user_mode * string
  | Remove_user_mode of user_mode * string

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

and user_mode =
  | User_mode_invisible
  | User_mode_server_notices
  | User_mode_wallops
  | User_mode_op

type message =
    {
      mutable imsg_prefix: prefix;
      mutable imsg_cmd: command;
      mutable imsg_params: string array;
    }

(* <message>  ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>        *)
(* <prefix>   ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]      *)
(* <command>  ::= <letter> { <letter> } | <number> <number> <number>       *)
(* <SPACE>    ::= ' ' { ' ' }                                              *)
(* <params>   ::= <SPACE> [ ':' <trailing> | <middle> <params> ]           *)
(* <middle>   ::= <Any *non-empty* sequence of octets not including SPACE  *)
(*                or NUL or CR or LF, the first of which may not be ':'>   *)
(* <trailing> ::= <Any, possibly *empty*, sequence of octets not including *)
(*                NUL or CR or LF                                          *)
(* <crlf>     ::= CR LF                                                    *)

(* extract_word_till_space *)
(* b is buffer *)
(* s is stream of characters *)

exception Protocol_error of string
exception End_of_stream

let rec extract_word_till_space s =
  let b = Buffer.create 16 in
  let rec loop = parser
    | [< '' '; s >] -> b
    | [< ''\r'; ''\n' >] -> raise (Protocol_error "CRLF unexpected in word")
    | [< ''\n' >] -> raise (Protocol_error "LF unexpected in word")
    | [< 'c; s >] ->
        Buffer.add_char b c;
        loop s
    | [< >] -> b
  in loop s

let command_table = [|
  "PASSWORD", Cmd_password;
  "NICK",     Cmd_nick;
  "USER",     Cmd_user;
  "QUIT",     Cmd_quit;
  "JOIN",     Cmd_join;
  "PART",     Cmd_part;
  "MODE",     Cmd_mode;
  "TOPIC",    Cmd_topic;
  "LIST",     Cmd_list;
  "TOPIC",    Cmd_topic;
  "NOTICE",   Cmd_notice;
  "PRIVMSG",  Cmd_priv_msg;
  "PING",     Cmd_ping;
  "PONG",     Cmd_pong;
  "AWAY",     Cmd_away;
  "KICK",     Cmd_kick;
  "WHO",      Cmd_who;
  "WHOIS",    Cmd_whois;
  "WHOWAS",   Cmd_whowas;
  "315",      Cmd_reply Rpl_end_of_who;
  "319",      Cmd_reply Rpl_whois_channels;
  "352",      Cmd_reply Rpl_who;
  "353",      Cmd_reply Rpl_name_reply;
  "372",      Cmd_reply Rpl_MOTD;
  "375",      Cmd_reply Rpl_MOTD_start;
  "376",      Cmd_reply Rpl_end_of_MOTD;
  "433",      Err_nickname_in_use;
|]

exception Cmd_found of command
exception Str_found of string

type sender = prefix

let string_of_sender_safe = function
  | Prefix_server s -> s
  | Prefix_user u -> u.ipn_nick
  | Prefix_none -> raise (Invalid_argument "string_of_sender_safe")

let string_of_sender sender =
  try
    string_of_sender_safe sender
  with
    | Invalid_argument _ -> ""

let nick_user_host_of_sender = function
  | Prefix_user u -> Some (u.ipn_nick, u.ipn_user, u.ipn_host)
  | _ -> None

let command_of_string s =
  try
    Array.iter (function (t, c) -> if s = t then raise (Cmd_found c)) command_table;
    Cmd_generic s
  with
    | Cmd_found c -> c

let string_of_command c =
  match c with
    | Cmd_generic s -> "'" ^ s ^ "'" 
    | _ ->
        try
          Array.iter (function (s, d) -> if c = d then raise (Str_found s)) command_table;
          raise Not_found
        with
          | Str_found s -> s

let output_prefix o = function
  | Prefix_none -> ()
  | Prefix_server s -> output_string o s
  | Prefix_user u ->
      output_string o u.ipn_nick;
      if u.ipn_user != "" then
        (
          output_char o '!';
          output_string o u.ipn_user
        );
      if u.ipn_host != "" then
        (
          output_char o '@';
          output_string o u.ipn_host
        )

let output_command o c =
  output_string o (string_of_command c)

let print_prefix =
  function
    | Prefix_none -> print_string "none"
    | Prefix_server s -> print_string ("S(" ^ s ^ ")")
    | Prefix_user u -> print_string (u.ipn_nick ^ "!" ^ u.ipn_user ^ "@" ^ u.ipn_host)

let print_message m =
  (* print_string "Command "; *)
  print_string (string_of_command m.imsg_cmd);
  print_string " [";
  print_prefix m.imsg_prefix;
  print_string "] ";
  for i = 0 to (Array.length m.imsg_params) - 1 do
    if i > 0 then print_char ' ';
    print_char '"';
    print_string m.imsg_params.(i);
    print_char '"';
  done;
  print_char '\n'

let output_msg o m =
  Pervasives.flush Pervasives.stdout;
  output_prefix o m.imsg_prefix;
  output_command o m.imsg_cmd;
  output_char o ' ';
  for i = 0 to (Array.length m.imsg_params) - 2 do
    output_string o m.imsg_params.(i);
    output_char o ' ';
  done;
  output_char o ':';
  output_string o m.imsg_params.((Array.length m.imsg_params) - 1);
  output_char o '\r';
  output_char o '\n';
  flush o

(**
  [parse s] takes a stream [s] of characters and returns a stream of message structures.
  *)
let rec parse s =
  let b = Buffer.create 16 in
  let mbeg = ref 0 in
  let m =
    {
      imsg_prefix = Prefix_none;
      imsg_cmd = Cmd_none;
      imsg_params = [| |];
    }
  in
  let rec parse_message = parser
    | [< '':'; s >] ->
        m.imsg_prefix <- parse_prefix s;
        parse_message_2 s
    | [< ''\n'|'\r' >] -> raise (Protocol_error "LF or CR in message")
    | [< s >] ->
        try
          Stream.empty s;
          [< >]
        with
          | Stream.Failure -> parse_message_2 s
  and parse_command s =
    Buffer.clear b;
    let rec loop () = match s with parser
      | [< ''\n'|'\r' >] -> raise (Protocol_error "LF or CR in command")
      | [< ''0'..'9'|'a'..'z'|'A'..'Z' as c >] ->
          Buffer.add_char b c;
          loop ()
      | [< '' ' >] -> command_of_string (Buffer.contents b)
      | [< 'c >] -> raise (Protocol_error ("unexpected character '" ^ (String.make 1 c) ^ "' in command"))
    in
      loop ()
  and parse_message_2 s =
    m.imsg_cmd <- parse_command s;
      parse_params s
  and parse_params s =
    Buffer.clear b;
      let rec loop0 l n = match s with parser
        (* fin de ligne *)
        | [< ''\r'; ''\n' >] ->
            if Buffer.length b > 0 then
              loop2 ((Buffer.contents b)::l) (n + 1)
            else
              loop2 l n
        | [< ''\n' >] ->
            if Buffer.length b > 0 then
              loop2 ((Buffer.contents b)::l) (n + 1)
            else
              loop2 l n
        (* fin de paramètre : paramètre de queue ou nouveau paramètre *)
        | [< '' '|':' as c >] ->
            (* le paramètre actuel est-il vide ? *)
            if Buffer.length b > 0 then
              (* non. on doit l'ajouter à la liste des paramètres *)
              let s = Buffer.contents b in
                Buffer.clear b;
                if c = ':' then
                  loop1 (s::l) (n + 1)
                else
                  loop0 (s::l) (n + 1)
                else
                  (* le paramètre actuel est vide, on peut l'oublier *)
                  if c = ':' then
                    loop1 l n
                  else
                    loop0 l n
                    | [< 'c >] ->
                        Buffer.add_char b c;
                        loop0 l n
                    | [< >] ->
                        raise (Protocol_error "parameter")
                          and loop1 l n = match s with parser
                            (* buffer ought to be clear *)
                            | [< ''\r'; ''\n' >] -> loop2 ((Buffer.contents b) :: l) (n + 1)
                            | [< ''\n' >] -> loop2 ((Buffer.contents b) :: l) (n + 1)
                            | [< 'c >] ->
                                Buffer.add_char b c;
                                loop1 l n
                                  and loop2 l n =
                                    begin
                                      m.imsg_params <- Array.create n "";
                                      let rec loop i l =
                                        match l with
                                          | x::r ->
                                              m.imsg_params.(i) <- x;
                                              loop (i - 1) r
                                          | [] -> [< 'm ; parse s >]
                                      in loop (n - 1) l
                                    end
      in
        loop0 [] 0
  and parse_prefix s =
    let b_nick_or_server =
      Buffer.create 32 and
        b_user = Buffer.create 32 and
                   b_host = Buffer.create 32 in
    let rec loop0 () =
      match s with parser
        | [< ''\r'|'\n' >] -> raise (Protocol_error "CR or LF in prefix nick or server")
        | [< '' ' >] -> Prefix_server (Buffer.contents b_nick_or_server)
        | [< ''!' >] -> loop1 ()
        | [< 'c >] ->
            Buffer.add_char b_nick_or_server c;
            loop0 ()
    and loop1 () =
      match s with parser
        | [< ''\r'|'\n' >] -> raise (Protocol_error "CR or LF in prefix user")
        | [< '' ' >] -> loop3 ()
        | [< ''@' >] -> loop2 ()
        | [< 'c >] ->
            Buffer.add_char b_user c;
            loop1 ()
    and loop2 () =
      match s with parser
        | [< ''\r'|'\n' >] -> raise (Protocol_error "CR or LF in prefix host")
        | [< '' ' >] -> loop3 ()
        | [< 'c >] ->
            Buffer.add_char b_host c;
            loop2 ()
    and loop3 () =
      Prefix_user ({ ipn_nick = Buffer.contents b_nick_or_server;
                     ipn_user = Buffer.contents b_user;
                     ipn_host = Buffer.contents b_host })
    in loop0 ()
  in
    mbeg := Stream.count s;
    parse_message s

let filter_stupid_mirc_color_codes s =
  let b = Buffer.create (String.length s)
  and t = Stream.of_string s in
  let rec add c d =
    Buffer.add_char b c;
    d t
  and terminate () = Buffer.contents b
  and loop0 s =
    match s with parser
      | [< ''\001'|'\002'|'\026'|'\037'|'\017'|'\007'; u >] -> loop0 u
      | [< ''\003'; u >] -> loop1 0 u
      | [< 'c >] -> add c loop0
      | [< >] -> terminate ()
  and loop1 x u =
    if x > 1 then
      loop0 u
    else match u with parser
      | [< ''0'..'9'; v >] -> loop1 (x + 1) v
      | [< '','; v >] -> loop1 0 v
      | [< ''\001'|'\002'|'\026'|'\037'|'\017'|'\007'; v >] -> loop0 v 
      | [< 'c >] -> add c loop0
      | [< >] -> terminate ()
  in
    loop0 t

(*
 Color code list
 0 white
 1 black
 2 blue     (navy)
 3 green
 4 red
 5 brown    (maroon)
 6 purple
 7 orange   (olive)
 8 yellow
 9 lt.green (lime)
 10 teal    (a kinda green/blue cyan)
 11 lt.cyan (cyan ?) (aqua)
 12 lt.blue (royal)
 13 pink    (light purple) (fuchsia)
 14 grey
 15 lt.grey (silver)
 *)
let color_text col txt =
  Printf.sprintf "\003%02d%s\003" col txt

let bicolor_text colfg colbg txt =
  Printf.sprintf "\003%d,%02d%s\003" colfg colbg txt

let bold_text txt =
  Printf.sprintf "\002%s\002" txt

let underline_text txt =
  Printf.sprintf "\031%s\031" txt

let reverse_text txt =
  Printf.sprintf "\022%s\022" txt

let beep_text () = "\007"

let encode_ctcp msg =
  "\001" ^ msg ^ "\001"

class client server_ nick_ realname_ user_ host_ =
object (self)
  val mutable port = 6667
  val mutable cur_nick = nick_
  val mutable cur_user = user_
  val mutable cur_host = host_

  method set_port p = port <- p

  val mutable ic = stdin (* stub *)
  val mutable oc = stdout (* stub *)

  method private send_command cmd params =
    output_msg oc
      {
        imsg_prefix = Prefix_none;
        imsg_cmd = cmd;
        imsg_params = params
      }

  method change_user nick host user =
    self#send_command Cmd_user [|user; host; server_; realname_|];
    cur_nick <- user;
    cur_user <- user;
    cur_host <- host

  method change_nick nick =
    self#send_command Cmd_nick [|nick|];
    cur_nick <- nick

  method join chan =
    self#send_command Cmd_join [|chan|]

  method part chan =
    self#send_command Cmd_part [|chan|]

  method quit msg =
    self#send_command Cmd_quit [|msg|]

  method get_topic chan =
    self#send_command Cmd_topic [|chan|]

  method change_topic chan topic =
    self#send_command Cmd_topic [|chan; topic|]

  method op user pass =
    self#send_command Cmd_oper [|user; pass|]

  method change_mode mode =
    match mode with
      | Add_channel_mode (chan, m) ->
          (
            match m with
              | Channel_mode_voice user ->
                  self#send_command Cmd_mode [|chan; "+v"; user|]
              | _ -> () (* TODO *)
          )
      | _ -> () (* TODO *)

  (* TODO: add a command to list a chan only *)
  method get_chan_list =
    self#send_command Cmd_list [||]

  method invite user chan =
    self#send_command Cmd_invite [|user; chan|]

  method kick user chan reason =
    self#send_command Cmd_kick [|user; chan; reason|]

  method get_version =
    self#send_command Cmd_version [||]

  method get_time =
    self#send_command Cmd_time [||]

  method get_info =
    self#send_command Cmd_info [||]

  (* TODO: add an optional argument for operators *)
  method get_who name =
    self#send_command Cmd_who [|name|]

  method get_who_ircop name =
    self#send_command Cmd_who [|name; "o"|]

  method get_whois user =
    self#send_command Cmd_whois [|user|]

  method get_whowas user =
    self#send_command Cmd_whowas [|user|]

  method ping server =
    self#send_command Cmd_ping [|server|]

  method set_away msg =
    self#send_command Cmd_away [|msg|]

  method set_back =
    self#send_command Cmd_away [||]

  method say dest msg =
    self#send_command Cmd_priv_msg [|dest; msg|]

  method notice dest msg =
    self#send_command Cmd_notice [|dest; msg|]

  method private notice_ctcp dest msg =
    self#notice dest (encode_ctcp msg)

  method ctcp_ping dest timestamp =
    self#say dest (encode_ctcp ("PING " ^ Printf.sprintf "%.4f" timestamp))

  method connect =
    let host = Unix.gethostbyname server_ in
    let addr = Unix.ADDR_INET(host.Unix.h_addr_list.(0), port) in
    let i_c, o_c = Unix.open_connection addr in
      ic <- i_c;
      oc <- o_c;
      self#change_user nick_ host_ user_;
      self#change_nick nick_

  method disconnect =
    Unix.shutdown_connection ic

  method on_message sender receiver msg = ()

  method on_notice sender receiver msg = ()

  method on_mode sender mode = ()

  method on_join sender chan = ()

  method on_part sender chan reason = ()

  method on_quit sender reason = ()

  method on_ping sender server =
    self#send_command Cmd_pong [|cur_host|]

  method on_reply sender kind params = ()

  method on_nick sender nick = ()

  method on_kick sender chan user reason = ()

  method on_ctcp_version sender receiver =
    self#notice_ctcp (string_of_sender sender) ("VERSION Irc module in OCaml " ^ Sys.ocaml_version ^ " [" ^ Sys.os_type ^ "]")

  method on_ctcp_time sender receiver =
    (* TODO: make a global function *)
    let string_of_wday = function
      | 0 -> "Sun"
      | 1 -> "Mon"
      | 2 -> "Tue"
      | 3 -> "Wed"
      | 4 -> "Thu"
      | 5 -> "Fri"
      | 6 -> "Sat"
      | _ -> raise (Invalid_argument "string_of_wday")
    in
    let string_of_month = function
      | 0 -> "Jan"
      | 1 -> "Feb"
      | 2 -> "Mar"
      | 3 -> "Apr"
      | 4 -> "May"
      | 5 -> "Jun"
      | 6 -> "Jul"
      | 7 -> "Aug"
      | 8 -> "Sep"
      | 9 -> "Oct"
      | 10 -> "Nov"
      | 11 -> "Dec"
      | _ -> raise (Invalid_argument "string_of_month")
    in
    let t = Unix.localtime (Unix.time ()) in
      self#notice_ctcp (string_of_sender sender) (Printf.sprintf "TIME %s %s %.2d %.2d:%.2d:%.2d" (string_of_wday t.Unix.tm_wday) (string_of_month t.Unix.tm_mon) t.Unix.tm_mday t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec)

  method on_ctcp_ping sender receiver timestamp =
    self#notice_ctcp (string_of_sender sender) ("PING " ^ timestamp)

  method on_ctcp_action sender receiver action = ()

  (* See:
   http://www.invlogic.com/irc/ctcp.html
   http://www.irchelp.org/irchelp/rfc/ctcpspec.html *)
  method on_ctcp sender receiver msg =
    match msg with
      | "VERSION" ->
          self#on_ctcp_version sender receiver
      | "TIME" ->
          self#on_ctcp_time sender receiver
      | _ when String.length msg > 7 && String.sub msg 0 6 = "ACTION" ->
          self#on_ctcp_action sender receiver (String.sub msg 7 ((String.length msg) - 7))
      | _ when String.length msg > 5 && String.sub msg 0 5 = "PING " ->
          self#on_ctcp_ping sender receiver (String.sub msg 5 ((String.length msg) - 5))
      | _ ->
          (
            match sender with
              | Prefix_user user ->
                  self#notice_ctcp user.ipn_nick ("ERRMSG " ^ msg ^ " :Unknown CTCP query")
              | _ -> ()
          )

  method on_ctcp_ping_reply sender receiver timestamp = ()

  method on_ctcp_reply sender receiver msg =
    match msg with
      | _ when String.length msg > 5 && String.sub msg 0 5 = "PING " ->
          self#on_ctcp_ping_reply sender receiver (String.sub msg 5 ((String.length msg) - 5))
      | _ -> ()

  method private on_event =
      (fun m ->
         match m.imsg_cmd with
           | Cmd_priv_msg ->
               let msg = m.imsg_params.(1) in
                 (* TODO: filter ctcp in messages + decode ctcp *)
                 if msg.[0] = '\001' && msg.[(String.length msg) - 1] = '\001' then
                   self#on_ctcp m.imsg_prefix m.imsg_params.(0) (String.sub msg 1 ((String.length msg) - 2))
                 else
                   self#on_message m.imsg_prefix m.imsg_params.(0) msg
           | Cmd_notice ->
               let msg = m.imsg_params.(1) in
                 (* TODO: filter ctcp in messages + decode ctcp *)
                 if msg.[0] = '\001' && msg.[(String.length msg) - 1] = '\001' then
                   self#on_ctcp_reply m.imsg_prefix m.imsg_params.(0) (String.sub msg 1 ((String.length msg) - 2))
                 else
                   self#on_notice m.imsg_prefix m.imsg_params.(0) m.imsg_params.(1)
           | Cmd_join ->
               self#on_join m.imsg_prefix m.imsg_params.(0)
           | Cmd_part ->
               self#on_part m.imsg_prefix m.imsg_params.(0) (if (Array.length m.imsg_params) > 1 then m.imsg_params.(1) else "")
           | Cmd_quit ->
               self#on_quit m.imsg_prefix (if (Array.length m.imsg_params) > 0 then m.imsg_params.(0) else "")
           | Cmd_ping ->
               self#on_ping m.imsg_prefix m.imsg_params.(0)
           | Cmd_nick ->
               self#on_nick m.imsg_prefix m.imsg_params.(0)
           | Cmd_kick ->
               self#on_kick m.imsg_prefix m.imsg_params.(0) m.imsg_params.(1) (if (Array.length m.imsg_params) > 2 then m.imsg_params.(2) else "")
           | Cmd_reply r ->
               self#on_reply m.imsg_prefix r m.imsg_params
           | Cmd_mode ->
               let modes =
                 let ans = ref [] in
                   if m.imsg_params.(0).[0] = '#' then
                     (* channel mode *)
                     let mode_constr mode =
                       if m.imsg_params.(1).[0] = '+' then
                         Add_channel_mode (m.imsg_params.(0), mode)
                       else
                         Remove_channel_mode (m.imsg_params.(0), mode)
                     in
                     let cur_arg = ref 2 in
                       for i = 1 to (String.length m.imsg_params.(1)) - 1
                       do
                         match m.imsg_params.(1).[i] with
                           | 'o' ->
                               ans := mode_constr (Channel_mode_op m.imsg_params.(!cur_arg)) :: !ans;
                               incr cur_arg
                           | _ -> () (* TODO *)
                       done
                   else
                     (); (* user mode *)
                     List.rev !ans
                 in
                 List.iter (self#on_mode m.imsg_prefix) modes
                 (* TODO: other *)
           | _ -> ()
      )

  method on_raw_event event =
    Stream.iter self#on_event (parse (Stream.of_string event))

  method send_raw_command cmd =
    output_string oc (cmd ^ "\n")

  method event_loop =
    while true
      do
      let msg = self#read_line in
        self#on_raw_event msg
    done
    
  method private read_line =
    input_line ic ^ "\n"
end

module type Client_params =
sig
  val server : string
  val port : int
  val nick : string
  val realname : string
  val user : string
  val host : string
end

module Client (P : Client_params) =
struct
  let ic = ref stdin (* stub *)
  let oc = ref stdout (* stub *)
  let cur_nick = ref P.nick
  let cur_user = ref P.user
  let cur_host = ref P.host

  let send_command cmd params =
    output_msg !oc
      {
        imsg_prefix = Prefix_none;
        imsg_cmd = cmd;
        imsg_params = params
      }

  let say dest msg =
    send_command Cmd_priv_msg [|dest; msg|]

  (* TODO: really encode message *)
  let encode_ctcp msg =
    "\001" ^ msg ^ "\001"

  let notice dest msg =
    send_command Cmd_notice [|dest; msg|]

  let notice_ctcp dest msg =
    notice dest (encode_ctcp msg)

  let ctcp_ping dest timestamp =
    say dest (encode_ctcp ("PING " ^ Printf.sprintf "%.4f" timestamp))

  let change_user nick host user =
    send_command Cmd_user [|user; host; P.server; P.realname|];
    cur_nick := user;
    cur_user := user;
    cur_host := host

  let change_nick nick =
    send_command Cmd_nick [|nick|];
    cur_nick := nick

  let get_nick () =
    !cur_nick

  let join chan =
    send_command Cmd_join [|chan|]

  let join_pw chan pass =
    send_command Cmd_join [|chan; pass|]

  let part chan =
    send_command Cmd_part [|chan|]

  let connect () =
    let host = Unix.gethostbyname P.server in
    let addr = Unix.ADDR_INET(host.Unix.h_addr_list.(0), P.port) in
    let i_c, o_c = Unix.open_connection addr in
      ic := i_c;
      oc := o_c;
      change_user P.nick P.host P.user;
      change_nick P.nick

  let disconnect () =
    Unix.shutdown_connection !ic

  let quit msg =
    send_command Cmd_quit [|msg|]

  let get_topic chan =
    send_command Cmd_topic [|chan|]

  let change_topic chan topic =
    send_command Cmd_topic [|chan; topic|]

  let op user pass =
    send_command Cmd_oper [|user; pass|]

  let change_mode mode =
    match mode with
      | Add_channel_mode (chan, m) ->
          (
            match m with
              | Channel_mode_voice user -> send_command Cmd_mode [|chan; "+v"; user|]
              | _ -> () (* TODO *)
          )
      | _ -> () (* TODO *)

  (* TODO: add a command to list a chan only *)
  let get_chan_list () =
    send_command Cmd_list [||]

  let invite user chan =
    send_command Cmd_invite [|user; chan|]

  let kick user chan reason =
    send_command Cmd_kick [|user; chan; reason|]

  let get_version () =
    send_command Cmd_version [||]

  let get_time () =
    send_command Cmd_time [||]

  let get_info () =
    send_command Cmd_info [||]

  (* TODO: add an optional argument for operators *)
  let get_who name =
    send_command Cmd_who [|name|]

  let get_who_ircop name =
    send_command Cmd_who [|name; "o"|]

  let get_whois user =
    send_command Cmd_whois [|user|]

  let get_whowas user =
    send_command Cmd_whowas [|user|]

  let ping server =
    send_command Cmd_ping [|server|]

  let set_away msg =
    send_command Cmd_away [|msg|]

  let set_back () =
    send_command Cmd_away [||]

  let on_message = ref (fun sender receiver msg -> ())

  let on_notice = ref (fun sender receiver msg -> ())

  let on_mode = ref (fun sender mode -> ())

  let on_nick = ref (fun sender nick -> ())

  let on_kick = ref (fun sender chan user reason -> ())

  let on_reply = ref (fun sender kind params -> ())

  let on_ctcp_version =
    ref
      (fun sender receiver ->
         notice_ctcp (string_of_sender sender) ("VERSION Irc module in OCaml " ^ Sys.ocaml_version ^ " [" ^ Sys.os_type ^ "]")
      )

  let on_ctcp_time =
    (* TODO: make a global function *)
    let string_of_wday = function
      | 0 -> "Sun"
      | 1 -> "Mon"
      | 2 -> "Tue"
      | 3 -> "Wed"
      | 4 -> "Thu"
      | 5 -> "Fri"
      | 6 -> "Sat"
      | _ -> raise (Invalid_argument "string_of_wday")
    in
    let string_of_month = function
      | 0 -> "Jan"
      | 1 -> "Feb"
      | 2 -> "Mar"
      | 3 -> "Apr"
      | 4 -> "May"
      | 5 -> "Jun"
      | 6 -> "Jul"
      | 7 -> "Aug"
      | 8 -> "Sep"
      | 9 -> "Oct"
      | 10 -> "Nov"
      | 11 -> "Dec"
      | _ -> raise (Invalid_argument "string_of_month")
    in
    ref
      (fun sender receiver ->
         let t = Unix.localtime (Unix.time ()) in
           notice_ctcp (string_of_sender sender) (Printf.sprintf "TIME %s %s %.2d %.2d:%.2d:%.2d" (string_of_wday t.Unix.tm_wday) (string_of_month t.Unix.tm_mon) t.Unix.tm_mday t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec)
      )

  let on_ctcp_ping =
    ref
      (fun sender receiver timestamp ->
         notice_ctcp (string_of_sender sender) ("PING " ^ timestamp)
      )

  let on_ctcp_action = ref (fun sender receiver action -> ())

  (* See:
     http://www.invlogic.com/irc/ctcp.html
     http://www.irchelp.org/irchelp/rfc/ctcpspec.html *)
  let on_ctcp =
    ref
      (fun sender receiver msg ->
         match msg with
           | "VERSION" -> !on_ctcp_version sender receiver
           | "TIME" -> !on_ctcp_time sender receiver
           | _ when String.length msg > 7 && String.sub msg 0 6 = "ACTION" -> !on_ctcp_action sender receiver (String.sub msg 7 ((String.length msg) - 7))
           | _ when String.length msg > 5 && String.sub msg 0 5 = "PING " -> !on_ctcp_ping sender receiver (String.sub msg 5 ((String.length msg) - 5))
           | _ ->
               (
                 match sender with
                   | Prefix_user user ->
                       notice_ctcp user.ipn_nick ("ERRMSG " ^ msg ^ " :Unknown CTCP query")
                   | _ -> ()
               )
      )

  let on_ctcp_ping_reply = ref (fun sender receiver timestamp -> ())

  let on_ctcp_reply =
    ref
      (fun sender receiver msg ->
         match msg with
           | _ when String.length msg > 5 && String.sub msg 0 5 = "PING " -> !on_ctcp_ping_reply sender receiver (String.sub msg 5 ((String.length msg) - 5))
           | _ -> ()
      )

  let on_ping =
    ref
      (fun sender server ->
         send_command Cmd_pong [|P.host|]
      )

  let on_join = ref (fun sender chan -> ())

  let on_part = ref (fun sender chan reason -> ())

  let on_quit = ref (fun sender reason -> ())

  let on_error =
    ref
      (fun sender msg ->
         Printf.eprintf "Error from %s: %s\n%!" (string_of_sender sender) msg
      )

  let on_event =
    ref
      (fun m ->
         match m.imsg_cmd with
           | Cmd_priv_msg ->
               let msg = m.imsg_params.(1) in
                 (* TODO: filter ctcp in messages + decode ctcp *)
                 if msg.[0] = '\001' && msg.[(String.length msg) - 1] = '\001' then
                   !on_ctcp m.imsg_prefix m.imsg_params.(0) (String.sub msg 1 ((String.length msg) - 2))
                 else
                   !on_message m.imsg_prefix m.imsg_params.(0) msg
           | Cmd_notice ->
               let msg = m.imsg_params.(1) in
                 (* TODO: filter ctcp in messages + decode ctcp *)
                 if msg.[0] = '\001' && msg.[(String.length msg) - 1] = '\001' then
                   !on_ctcp_reply m.imsg_prefix m.imsg_params.(0) (String.sub msg 1 ((String.length msg) - 2))
                 else
                   !on_notice m.imsg_prefix m.imsg_params.(0) m.imsg_params.(1)
           | Cmd_join ->
               !on_join m.imsg_prefix m.imsg_params.(0)
           | Cmd_part ->
               !on_part m.imsg_prefix m.imsg_params.(0) (if (Array.length m.imsg_params) > 1 then Some m.imsg_params.(1) else None)
           | Cmd_quit ->
               !on_quit m.imsg_prefix (if (Array.length m.imsg_params) > 0 then Some m.imsg_params.(0) else None)
           | Cmd_ping ->
               !on_ping m.imsg_prefix m.imsg_params.(0)
           | Cmd_nick ->
               !on_nick m.imsg_prefix m.imsg_params.(0)
           | Cmd_kick ->
               !on_kick m.imsg_prefix m.imsg_params.(0) m.imsg_params.(1) (if (Array.length m.imsg_params) > 2 then Some m.imsg_params.(2) else None)
           | Cmd_reply r ->
               !on_reply m.imsg_prefix r m.imsg_params
           | Cmd_mode ->
               let modes =
                 let ans = ref [] in
                   if m.imsg_params.(0).[0] = '#' then
                     (* channel mode *)
                     let mode_constr =
                       (fun mode ->
                          if m.imsg_params.(1).[0] = '+' then
                            Add_channel_mode (m.imsg_params.(0), mode)
                          else
                            Remove_channel_mode (m.imsg_params.(0), mode))
                     in
                     let cur_arg = ref 2 in
                       for i = 1 to (String.length m.imsg_params.(1)) - 1
                       do
                         match m.imsg_params.(1).[i] with
                           | 'o' ->
                               ans := mode_constr (Channel_mode_op m.imsg_params.(!cur_arg)) :: !ans;
                               incr cur_arg
                           | _ -> () (* TODO *)
                       done
                       else
                         (); (* user mode *)
                       List.rev !ans
               in
                 List.iter (!on_mode m.imsg_prefix) modes
                 (* TODO: other *)
           | _ -> ()
      )

  let dump_everything = ref false

  let event_loop () =
    while true
      do
      let msg = input_line !ic ^ "\n" in
        if !dump_everything then
          Printf.printf "%s%!" msg;
          Stream.iter !on_event (parse (Stream.of_string msg))
      done
end
