(* #!/home/pad/etc/commons/commons.top
#directory "/home/pad/etc/commons/"

#load "str.cma"
#load "unix.cma"

#use "common.ml"
*)
open Common
(*
ocamlc str.cma unix.cma -I ~/etc/commons/ ~/etc/commons/common.cmo xxx.ml  -o xxx
*)

(*****************************************************************************)
(*  *)
(*****************************************************************************)
(* 
 * requirements:
 *  - OCaml
 *  - lots of disk space
 *  - git, wget
 *  - tar, gzip
 * 
 * 
 * time: approximately 1 hour. This is mainly due to the date-fixing 
 * which use git-filter to rewrite the history which  takes lots of time.
 *)
(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let author_linux_up_to_2_5_3 = "linus1"

(* let author_linux_0_0_1 = "linus0" *)
(* let author_junction = author_linux_up_to_2_5 *)


let gits = [
  "history-dave";
  "history-tglx";
  "history-torvalds";
]
(* also many directory names are repeated and kind of globals *)



(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let diff_command dir1 dir2 = 
  spf "diff -x .git -x RCS -u -r %s %s " dir1 dir2

let checkout_master_command dir = 
  spf "cd %s; git checkout master" dir
let checkout_commitid_command dir commitid = 
  spf "cd %s; git checkout -b %s %s" dir commitid commitid


let get_first_last_commitid dir =
  let xs = Common.cmd_to_list (spf "cd %s; git rev-list HEAD" dir) in
  let (head_str, _, last_str) = Common.head_middle_tail xs in
  last_str, head_str


(*****************************************************************************)
(* Getting the gits *)
(*****************************************************************************)

(* time =~ 5 minute *)
let get_linux_gits () = 
  Common.command2 "git clone https://github.com/davidbau/davej.git history-dave";
  Common.command2 "git clone git://git.kernel.org/pub/scm/linux/kernel/git/tglx/history.git history-tglx";
  Common.command2 "git clone git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux-2.6.git history-torvalds";
  ()

let get_extra_files () = 
  Common.command2 "wget http://www.kernel.org/pub/linux/kernel/Historic/linux-0.01.tar.gz";
  Common.command2 "wget https://www.kernel.org/pub/linux/kernel/v2.4/old-test-kernels/prerelease-to-final.gz";
  ()

let untar_extra_files () =
  Common.command2 "gunzip prerelease-to-final.gz";
  Common.command2 "tar xvzf linux-0.01.tar.gz";
  Common.command2 "mv linux linux-0.01";
  ()


(* according to kernel.org the prerelease-to-final was made in Jan  4  2001 *)
let make_junction_dave_to_tglx () =
  Common.command2 
    "cd history-dave; patch -p1 < ../prerelease-to-final";
  let msg = "add prerelease patch to get a 2.4.0" in
  Common.command2
    (spf "cd history-dave; git commit -a -m\"%s\"" msg);
  ()




let generate_logs () = 
  raise Todo


(*****************************************************************************)
(* Checking junctions *)
(*****************************************************************************)
let generate_first_clone () =
 gits +> List.iter (fun agit -> 
   let agitfirst = agit ^ "-first" in
   let (first_commit,_) = get_first_last_commitid agit in
   Common.command2 (spf "git clone %s %s" agit agitfirst);
   Common.command2(checkout_commitid_command agitfirst first_commit);
 )
   

let junction_dirs = 
  ["linux-0.01","history-dave-first";
   "history-dave", "history-tglx-first";
   "history-tglx", "history-torvalds-first";
  ]

let check_all_junctions () =
  pr2 "checking junctions";
  junction_dirs +> List.iter (fun (dir1, dir2) -> 
    pr2 (spf "diffing: %s vs %s" dir1 dir2);
    Common.command2(diff_command dir1 dir2);
  )

(*****************************************************************************)
(* Parsing adjusted dates information *)
(*****************************************************************************)
(* ex
commit 6891759b5340b54c3408ca7d80dc722f95d6b9e0 (Apr 9 1992)
*)
let commit_regexp1 = 
  "^commit \\([a-z0-9]+\\) " ^
  "(\\([A-Za-z]+\\) +\\([0-9]+\\) +\\([0-9]+\\))" ^
  "\\(.*\\)"
let commit_regexp2 = 
  "^commit \\([a-z0-9]+\\)[ \t]*$"
let commit_regexp3 = 
 "^commit.*"

let compute_adjusted_commit_info file = 
  let xs = Common.cat file in
  let commit_only = 
    xs +> Common.map_filter (fun s -> 
      match s with
      | s when s=~ commit_regexp1 -> 
          let (commit, month_str, day, year) = Common.matched4 s in
          let date = 
            Common.mk_date_dmy 
              (s_to_i day) 
              (Common.int_of_month (Common.month_of_string month_str))
              (s_to_i year)
          in
          let (datefloat,_tm)= Common.dmy_to_unixtime date in
          (* pr2 (spf "%s:%.0f" commit datefloat); *)
          Some (Left (commit, datefloat))
      | s when s=~commit_regexp2 -> 
          let (commit) = matched1 s in
          Some (Right (commit))
      | s when s=~ commit_regexp3 -> 
          failwith ("wrong format: " ^ s)
      | _ -> None
    )
  in
  commit_only

(*****************************************************************************)
(* generate git-filter scripts for date and author adjustments *)
(*****************************************************************************)
(*
 * from git manpage
 * 'GIT_AUTHOR_NAME'::
 * 'GIT_AUTHOR_EMAIL'::
 * 'GIT_AUTHOR_DATE'::
 * 'GIT_COMMITTER_NAME'::
 * 'GIT_COMMITTER_EMAIL'::
 * 'GIT_COMMITTER_DATE'::
 * 'EMAIL'::
 * 
 * ex of script: 
 * 
 * #!/bin/bash
 * 
 * git-filter-branch -f --env-filter '
 *    echo $GIT_AUTHOR_DATE
 *    echo $GIT_AUTHOR_NAME
 *    echo $GIT_AUTHOR_EMAIL
 *    #GIT_AUTHOR_DATE="1209125145 -0500"
 *    GIT_AUTHOR_DATE="685126800"
 *    if [ "$GIT_COMMIT" = "fb863cc1c637125917da186e03a15c270d239b73"  ];
 *    then 
 *      GIT_AUTHOR_NAME="toto";
 *      echo ici
 *    fi
 *    if [ "$GIT_COMMIT" = "4d5fa2f193fc3a636dc8e9a52a4ae825eb2f1070"  ];
 *    then 
 *      GIT_AUTHOR_NAME="toto";
 *      echo ici again
 *    fi
 *  ' HEAD
 *)

let generate_prelude_shell () =
  "#!/bin/bash
git filter-branch -f --env-filter '
"

let generate_postlude_shell () =
  " ' HEAD"

(* factorizing the code for author is useful otherwise get to the 
 * args size limit of the shell quite fast. But can do it only
 * for history-dave where all authors are wrong. For history-tglx at
 * one point the author are write and those will override valid
 * authors.
 *)
let generate_single_author author =
  spf "GIT_AUTHOR_NAME=\"%s\"" author



(* note: keep space between argument in the if of the shell script *)
let generate_commit_shell commitid date optstr = 
  (spf "if [ \"$GIT_COMMIT\" = \"%s\" ];
then 
GIT_AUTHOR_DATE=\"%.0f\"
%s
fi" commitid date optstr)


type author_insert = Top | EachCommit
type author_info = string * author_insert

let generate_shell_commits adjusted (author, author_insert) = 
  Common.with_open_stringbuf (fun (pr, _buf) -> 
    pr (generate_prelude_shell ());
    if author_insert = Top 
    then pr (generate_single_author author);
    let previous_date = ref 0.0 in
    let optstr = 
      if author_insert = EachCommit
      then generate_single_author author
      else "" 
    in
    adjusted +> List.rev +> List.iter (fun x -> 
      match x with
      | Left (commitid, datefloat) -> 
          previous_date := datefloat;
          pr(generate_commit_shell commitid !previous_date optstr);
          
      | Right (commitid) -> 
          pr(generate_commit_shell commitid !previous_date optstr);
    );
    pr(generate_postlude_shell ());
    ()
  )







let generate_shell_code dir adjustfile shfile authorinfo = 
  pr2 (spf "generating: %s from %s" shfile adjustfile);
  let adjusted = compute_adjusted_commit_info adjustfile in
  let s = generate_shell_commits adjusted authorinfo in
  Common.write_file ~file:shfile s; 
  ()


  

let generate_adjust_all () = 
  (* use Top here otherwise shell do not accept it cos too long 
   * generated env-filter git script 
   *)
  generate_shell_code 
    "history-dave" "history-dave-annotated.log" 
    "history-dave-adjust.sh"
    (author_linux_up_to_2_5_3, Top) 
  ;
  generate_shell_code 
    "history-tglx" "history-tglx-annotated-smallversion.log" 
    "history-tglx-adjust.sh"
    (author_linux_up_to_2_5_3, EachCommit)
  ;
  ()


(* must be done before the gc *)
let adjust_tags dir adjustfile =
  (* do a zip2 with current log ? check same commit msg ? *)
  raise Todo


let generate_special_case_prerelease () = 
  let (_, last_commit) = get_first_last_commitid "history-dave" in
  let last_adjust_logfile = "history-dave-annotated-last.log" in
  Common.write_file ~file:last_adjust_logfile
    (spf "commit %s (Jan 4 2001)" last_commit);
  last_adjust_logfile
          





let adjust_all () =


  (* must be done here for the commitid do be the correct last one *)
  let last_adjust_logfile = generate_special_case_prerelease () in
  generate_shell_code 
    "history-dave" last_adjust_logfile 
    "history-dave-adjust-last.sh"
    (author_linux_up_to_2_5_3, EachCommit)
  ;
  Common.command2 "cd history-dave; sh ../history-dave-adjust-last.sh"; 
  
  (* time =~ 2 min *)
  Common.command2 "cd history-dave; sh ../history-dave-adjust.sh"; 



  (* time =~ 3 hour? *)
  Common.command2 "cd history-tglx; sh ../history-tglx-adjust.sh";
  ()

(* why get sometimes multiple pack  ?  why the .keep for dave jones ?
*)

let compact_all () = 
  (* Common.command2 "rm -f history-dave/.git/objects/pack/*.keep"; *)

  Common.command2 "cd history-dave; git repack -a -d";
  Common.command2 "cd history-tglx; git repack -a -d";
  ()

  

(*****************************************************************************)
(* Grafting *)
(*****************************************************************************)

let get_junctions () = 
  let (_first_dave,last_dave) = get_first_last_commitid "history-dave" in
  let (first_tglx,last_tglx) = get_first_last_commitid "history-tglx" in
  let (first_linux,_last_linux) = get_first_last_commitid "history-torvalds" in
  [(first_linux, last_tglx);
   (first_tglx, last_dave);
  ]
  

let create_graft () =
  pr2 "cp pack files";
  Common.command2 "cp history-dave/.git/objects/pack/* history-torvalds/.git/objects/pack";
  Common.command2 "cp history-tglx/.git/objects/pack/* history-torvalds/.git/objects/pack";

  let graftfile = "history-torvalds/.git/info/grafts" in
  pr2 (spf "creating graft info file: %s" graftfile);
  
  Common.with_open_outfile graftfile (fun (pr_no_nl, _chan) -> 
    get_junctions () +> List.iter (fun (c1, c2) -> 
      pr_no_nl (spf "%s %s\n" c1 c2);
    )
  )

(*****************************************************************************)
(* create tar *)
(*****************************************************************************)
  
(* then tar xvf xxx.tar; cd xxx/; git checkout -f *)

(*****************************************************************************)
(* Cleaning *)
(*****************************************************************************)
let cleaning_files () =
  [
    "rm -rf history-dave";
    "rm -rf history-tglx";
    "rm -rf history-torvalds";
    "rm -rf history-dave-first";
    "rm -rf history-tglx-first";
    "rm -rf history-torvalds-first";
    "rm -rf linux-0.01";
    "rm -f linux-0.01.tar.bz2";
    "rm -f prerelease-to-final";
    "rm -f history-dave-adjust.sh";
    "rm -f history-dave-annotated-last.log";
    "rm -f history-dave-adjust-last.sh";
    "rm -f history-tglx-adjust.sh";
  ] +> List.iter Common.command2(*_y_or_no*)
   

(*****************************************************************************)
(* Test *)
(*****************************************************************************)
  
(*****************************************************************************)
(* Main *)
(*****************************************************************************)


let main () = 
  let args = ref [] in

  let options = [
    "-build", Arg.Unit (fun () -> 
      get_linux_gits();
      get_extra_files();
      untar_extra_files();
      make_junction_dave_to_tglx ();

      (* optional *)
      generate_first_clone();
      check_all_junctions ();

      generate_adjust_all ();

      adjust_all();
      compact_all();
      create_graft();
    ),
    " wget the linux git trees and merge them together";
    "-clean", Arg.Unit (fun () ->
      cleaning_files();
    ),
    " cleanup downloaded and generated files";

    "-test", Arg.Unit (fun () ->
      create_graft();
    ),
    " internal command";
(*
    "-compile", Arg.Unit (fun () -> exit 0),
    " just compile trick";
*)
  ] in
  let usage_msg = 
    "Usage: " ^ basename Sys.argv.(0) ^ 
      " [options]" ^ "\n" ^ "Options are:"
  in

  Arg.parse (Arg.align options) (fun x -> args := x::!args) usage_msg;
  args := List.rev !args;
  ()


let _ = 
  main()
