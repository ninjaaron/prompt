open StdLabels
let (|-) = Fun.compose

let home_pat = Re.Perl.compile_pat ("^" ^ (Sys.getenv "HOME"))
let path_part = Re.Perl.compile_pat "(.).*?/"
let active_branch = Re.Perl.compile_pat {|\* (.*?)\n|}

let get_short_dir cwd =
  Re.replace_string home_pat ~by:"~" cwd |>
  Re.replace path_part ~f:(fun g -> Re.Group.get g 1 ^ "/")

let get_git_prompt branch_ic status_ic =
  let open Subprocess.Results in
  match managed_read branch_ic, managed_read status_ic with
  | Error _, _ | _, Error _ -> None
  | Ok branch_out, Ok status_out ->
    let branch = Re.(Group.get (exec active_branch branch_out) 1) in
    let color = if status_out = "" then "green" else "red" in
    Some (color, branch)

let get_git_prompt_later () =
  let open Subprocess in
  let open' = open_out |- devnull_err |- cmd in
  let branch_ic = open' ["git"; "branch"] in
  let status_ic = open' ["git"; "status"; "-s"] in
  fun () -> get_git_prompt branch_ic status_ic

let get_updates () =
  let fn = (Sys.getenv "HOME") ^ "/.updates" in
  if not (Sys.file_exists fn) then None else
    let fh = open_in fn in
    let f total c = int_of_char c + (256 * total) in
    Some (String.fold_left ~init:0 ~f (In_channel.input_all fh))

let () =
  match Sys.getenv "USER" with
  | "root" -> print_endline "%F{yellow}%m%f:%F{red}%~%f# "
  | _ ->
    let get_git_prompt = get_git_prompt_later () in
    let (let+) opt f = Option.map f opt in
    let dir = let short_dir = get_short_dir (Sys.getcwd ()) in
      Some (String.concat ~sep:"" ["%F{blue}"; short_dir; "%f> "])
    and host = let+ _ = Sys.getenv_opt "SSH_TTY" in "%F{green}%m%f:"
    and git = let+ color, branch = get_git_prompt () in
      String.concat ~sep:"" ["%F{"; color; "}"; branch; "%f|"]
    and venv = let+ venv = Sys.getenv_opt "VIRTUAL_ENV" in
      Filename.basename venv ^ "|"
    and update = let+ n = get_updates () in
      "%F{yellow}" ^ Printf.sprintf "%x" n ^ "%f|" in
    print_endline @@ String.concat ~sep:""
    @@ List.filter_map ~f:Fun.id [venv; update; git; host; dir]
