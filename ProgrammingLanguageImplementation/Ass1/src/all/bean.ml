module P = Parser
module L = Lexer
module PPT = Pprint
(* Argument parsing code *)
let infile_name = ref None

(* --------------------------------------------- *)
(*  
    There are three type of modes.
    1. PrettyPrint, lexing and parsing then printing.
    2. Compile, not implement yet
    3. Error, output the message when arguments
       of input is invalid
*)
(* --------------------------------------------- *)
type compiler_mode = PrettyPrint | Compile | Error
let mode = ref Error

(* --------------------------------------------- *)
(*  Specification for command-line options       *)
(* --------------------------------------------- *)
    
let (speclist:(Arg.key * Arg.spec * Arg.doc) list) =
  ["-p",
     Arg.Unit(fun () -> mode := PrettyPrint),
     " Run the compiler in pretty-printer mode";
    ]

(* --------------------------------------------- *)
(*        Entrance body of the program           *)
(* --------------------------------------------- *)

let main () =
  (* Parse the command-line arguments *)
  
  Arg.parse speclist
      (begin fun fname -> infile_name := Some fname end)
      "bean [-p] [bean source]" ;
      
  if !mode == Error 
  then print_string "Sorry, cannot generate code yet\n"
  else begin      
    (* Open the input file *)
    let infile = match !infile_name with
    | None -> stdin
    | Some fname -> open_in fname in
    (* Initialize lexing buffer *)
    let lexbuf = Lexing.from_channel infile in
    (* Create a Queue to store the tuple (line_number,token) *)
    let tokens_queue = Queue.create() in
    (* Create a flag of loop *)
    let quit_loop = ref false in
    (* Get the token and line one by one *)
    while not !quit_loop do
      let line_number,token = L.token lexbuf in
      if token == P.EOF then
        quit_loop := true
      else
        Queue.add (line_number,token) tokens_queue
    done;
    
    (* We already get tokens, so create a empty lexbuf *)
    let lexbuf = Lexing.from_string "" in
    (* The flag of which line it is reading now *)
    let last_line_number = ref 1 in
    (* Take all tokens we get previous *)
    let lexer_token lb = 
      if Queue.is_empty tokens_queue then
        (* After take all tokens, add the end_of_file token to the tail *)
        P.EOF
      else begin
        let ln, next_token = Queue.take tokens_queue in
        last_line_number := ln;
        next_token
      end
    in 
    (* Get the output afer parsing *)
    let ast = 
      try 
        P.main lexer_token lexbuf
      with e -> begin
        (* Error occured and Output error msg *)
        Printf.printf "ERROR OCCURRED : line %d : Parsing error \n" !last_line_number;
        exit 0
      end
    in
    match !mode with
      | PrettyPrint -> print_string (PPT.print_program Format.std_formatter ast)
      | Compile -> print_string "Compile"
      | _ -> print_string "Unexpected"
  end
let _ = main ()


