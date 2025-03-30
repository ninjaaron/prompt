open StdLabels
module Unix = UnixLabels

let home_pat = Re.Perl.compile_pat ("^" ^ (Sys.getenv "HOME"))
let path_part = Re.Perl.compile_pat "(.).*?/"
let active_branch = Re.Perl.compile_pat {|\* (.*?)\n|}

let get_short_dir cwd =
  Re.replace_string home_pat ~by:"~" cwd |>
  Re.replace path_part ~f:(fun g -> Re.Group.get g 1 ^ "/")

let proc_out args =
  let sout, sin, serr = Unix.open_process_args_full args.(0) args [||] in
  let lines = In_channel.input_all sout in
  match Unix.close_process_full (sout, sin, serr) with
  | Unix.WEXITED 0 -> Some lines
  | _ -> None

let get_git_prompt () =
  let (let*) = Option.bind in
  let* branch_out = proc_out [|"git"; "branch"|] in
  let* branch_g = Re.exec_opt active_branch branch_out in
  let branch = Re.Group.get branch_g 1 in
  let* status_out = proc_out [|"git"; "status"; "-s"|] in
  let color = if status_out = "" then "green" else "red" in
  Some (color, branch)

let get_updates () =
  let fn = (Sys.getenv "HOME") ^ "/.updates" in
  if not (Sys.file_exists fn) then None else
    let fh = open_in fn in
    let f total c = int_of_char c + (256 * total) in
    Some (String.fold_left ~init:0 ~f (In_channel.input_all fh))

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "-t" then
    exit 0;

  if Sys.getenv "USER" = "root" then
    (print_endline "%F{yellow}%m%f:%F{red}%~%f# ";
     exit 0);

  let (let+) opt f = Option.map f opt in
  let dir =
    let short_dir = get_short_dir (Sys.getcwd ()) in
    Some (String.concat ~sep:"" ["%F{blue}"; short_dir; "%f> "])
  and host = let+ _ = Sys.getenv_opt "SSH_TTY" in "%F{green}%m%f:"
  and git = let+ color, branch = get_git_prompt () in
    String.concat ~sep:"" ["%F{"; color; "}"; branch; "%f|"]
  and venv = let+ venv = Sys.getenv_opt "VIRTUAL_ENV" in
    (Filename.basename venv ^ "|")
  and update = let+ n = get_updates () in
    "%F{yellow}" ^ Printf.sprintf "%x" n ^ "%f|" in
  print_endline @@ String.concat ~sep:""
  @@ List.filter_map ~f:Fun.id [venv; update; git; host; dir]
