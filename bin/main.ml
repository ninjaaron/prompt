open StdLabels
let home_pat = Re.Perl.compile_pat ("^" ^ (Sys.getenv "HOME"))
let path_part = Re.Perl.compile_pat "(.).*?/"
let active_branch = Re.Perl.compile_pat {|\* (.*?)\n|}

let get_short_dir cwd =
  Re.replace_string home_pat ~by:"~" cwd |>
  Re.replace path_part ~f:(fun g -> Re.Group.get g 1 ^ "/")
    
let get_active_branch s =
  Re.(Group.get (exec active_branch s) 1)
  
let get_git_prompt () =
  let open Subprocess.Results in
  let* branch_out = cmd ["git"; "branch"] |> devnull_err |> read in
  let* status_out = cmd ["git"; "status"; "-s"] |> devnull_err |> read in
  let branch = get_active_branch branch_out in
  let color = if status_out = "" then "green" else "red" in
  Ok (color, branch)

let get_updates () =
  let fh = (Sys.getenv "HOME") ^ "/.updates" |> open_in in
  let f total c = int_of_char c + (256 * total) in
  String.fold_left ~init:0 ~f (In_channel.input_all fh)

let get_update_prompt () =
  match get_updates () with
  | exception Sys_error _ | 0 -> None
  | n -> Some ("%F{yellow}" ^ Printf.sprintf "%x" n ^ "%f|")

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "-t" then
    exit 0;

  if Sys.getenv "USER" = "root" then
    (print_endline "%F{yellow}%m%f:%F{red}%~%f# ";
     exit 0);

  let (let*) = Option.bind in
  let dir =
    let short_dir = get_short_dir (Sys.getcwd ()) in
    Some (String.concat ~sep:"" ["%F{blue}"; short_dir; "%f> "])

  and host =
    let* _ = Sys.getenv_opt "SSH_TTY" in Some "%F{green}%m%f:"

  and git =
    let* color, branch = get_git_prompt () |> Result.to_option in
    Some (String.concat ~sep:"" ["%F{"; color; "}"; branch; "%f|"])

  and venv =
    let* venv = Sys.getenv_opt "VIRTUAL_ENV" in
    Some (Filename.basename venv ^ "|")

  and update = get_update_prompt () in

  print_endline @@ String.concat ~sep:""
  @@ List.filter_map ~f:Fun.id [venv; update; git; host; dir]
