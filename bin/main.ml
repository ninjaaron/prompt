open StdLabels

let home = Re.Perl.compile_pat (Sys.getenv "HOME")

let get_short_dir cwd =
  let cwd' = Re.replace_string home ~by:"~" cwd in
  let rec loop = function
      [] -> failwith "cwd should not be empty"
    | [_] as tail -> tail
    | "" :: tail -> "" :: loop tail
    | head :: tail ->
      let len = if head.[0] = '.' then 2 else 1 in
      String.sub head ~pos:0 ~len :: loop tail in
  loop (String.split_on_char ~sep:'/' cwd')
  |> String.concat ~sep:"/"

let active_pat = Re.Perl.compile_pat {|\* (.*)|}

let rec get_active_branch = function
    [] -> None
  | hd :: tl ->
    match Re.exec_opt active_pat hd with
      Some group -> Some (Re.Group.get group 1)
    | None -> get_active_branch tl

let get_git_prompt () =
  let ( let* ) = Result.bind in
  let* branch_proc =
    Subprocess.run ~stdout:`Pipe ~stderr:`Devnull [|"git"; "branch"|] in
  let* branch = get_active_branch branch_proc.stdout
                |> Option.to_result ~none:(branch_proc) in
  let* status_proc =
    Subprocess.run ~stdout:`Pipe ~stderr:`Devnull [|"git"; "status"; "-s"|] in
  let color = match status_proc.stdout with
      [] -> "green"
    | _ -> "red" in
  Ok (color, branch)

let get_git_prompt_later () =
  let branch_proc =
    Subprocess.create ~stdout:`Pipe ~stderr:`Devnull [|"git"; "branch"|]
  and status_proc =
    Subprocess.create ~stdout:`Pipe ~stderr:`Devnull [|"git"; "status"; "-s"|] in
  let close () =
    Subprocess.close branch_proc |> ignore;
    Subprocess.close status_proc |> ignore in
  fun () ->
    match get_active_branch (Subprocess.lines branch_proc |> List.of_seq) with
      None -> close (); None
    | Some branch ->
      let color = match Subprocess.line status_proc with
          None -> "green"
        | Some _ -> "red" in
      close ();
      Some (color, branch)

let get_updates () =
  let fh = (Sys.getenv "HOME") ^ "/.updates" |> open_in in
  let rec loop total =
    try loop ((total * 256) + input_byte fh)
    with End_of_file -> total
  in loop 0

let get_update_prompt () =
  try
    match get_updates () with
      0 -> None
    | n -> Some ("%F{yellow}" ^ Printf.sprintf "%x" n ^ "%f|")
  with Sys_error _ -> None
    

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "-t" then
    exit 0;

  if Sys.getenv "USER" = "root" then
    (print_endline "%F{yellow}%m%f:%F{red}%~%f# ";
     exit 0);
  let get_git_prompt = get_git_prompt_later () in

  let dir_prompt =
    let short_dir = get_short_dir (Sys.getcwd ()) in
    Some (String.concat ~sep:"" ["%F{blue}"; short_dir; "%f> "])

  and host_prompt =
    Sys.getenv_opt "SSH_TTY"
    |> Option.map (fun _ -> "%F{green}%m%f:")

  and git_prompt =
    get_git_prompt () |> Option.map
      (fun (color, branch) ->
         String.concat ~sep:"" ["%F{"; color; "}"; branch; "%f|"])

  and venv = Sys.getenv_opt "VIRTUAL_ENV"
           |> Option.map (fun venv -> Filename.basename venv ^ "|")

  and update_prompt = get_update_prompt ()
  in
  let prompt =
    List.filter_map
      ~f:Fun.id [venv; update_prompt; git_prompt; host_prompt; dir_prompt]
    |> String.concat ~sep:"" in
  print_endline prompt;
