open StdLabels
open Sys

let home = getenv "HOME"
let home_pat = Re.Perl.compile_pat ("^" ^ home)
let path_part = Re.Perl.compile_pat "(.).*?/"
let active_branch = Re.Perl.compile_pat {|\* (.*?)\n|}

let _get_short_dir cwd =
  Re.replace_string home_pat ~by:"~" cwd |>
  Re.replace path_part ~f:(fun g -> Re.Group.get g 1 ^ "/")

let get_git_prompt () =
  Result.to_option @@
  let open Subprocess.Results in
  let get_out args = cmd args |> devnull_err |> read in
  let* branch_out = get_out ["git"; "branch"] in
  let* status_out = get_out ["git"; "status"; "-s"] in
  let branch = Re.(Group.get (exec active_branch branch_out) 1) in
  let color = if status_out = "" then "green" else "red" in
  Ok (color, branch)

let get_updates () =
  let fn = home ^ "/.updates" in
  if not (file_exists fn) then None else
    let fh = open_in fn in
    let f total c = int_of_char c + (256 * total) in
    match String.fold_left ~init:0 ~f (In_channel.input_all fh) with
    | 0 -> None
    | n -> Some n

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "-t" then exit 0;
  match getenv "USER" with
  | "root" -> print_endline "%F{yellow}%m%f:%F{red}%~%f# "
  | _ ->
    let time = let tm = Unix.(localtime (time ())) in
      Some (Printf.sprintf "%02d:%02d:%02d | " tm.tm_hour tm.tm_min tm.tm_sec) in
    let (let+) opt f = Option.map f opt in
    let dir =
      let dir' =Re.replace_string ~by:"~" home_pat @@ getcwd () in
      Some (String.concat ~sep:"" ["%F{blue}"; dir'; "%f\n> "])
    and host = let+ _ = getenv_opt "SSH_TTY" in "%F{green}%m%f:"
    and git = let+ color, branch = get_git_prompt () in
      String.concat ~sep:"" ["%F{"; color; "}"; branch; "%f | "]
    and venv = let+ venv = Sys.getenv_opt "VIRTUAL_ENV" in
      Filename.basename venv ^ " | "
    and update = let+ n = get_updates () in
      "%F{yellow}" ^ Printf.sprintf "%x" n ^ "%f | " in
    print_endline @@ String.concat ~sep:""
    @@ List.filter_map ~f:Fun.id [time; venv; update; git; host; dir]
