open Lexer;;
open Parser;;
open Backend;;

print_string "PROLOG INTERPRETER!\n";;
print_string "This is a simple prolog interpreter made using OCaml.\n\n\n";;

if Array.length Sys.argv < 2 then begin
  print_string "You need to specify the name of the input file as well.\n";
  exit 0;
end;;

if Array.length Sys.argv > 2 then begin
  print_string "Only one file at a time can be loaded.\n";
  exit 0;
end;;

let fstream = open_in Sys.argv.(1);;
let init_prog = Parser.program Lexer.token (Lexing.from_channel fstream);;
let prog = unique_prog init_prog 1;;

print_string "Starting Now....\n\n";;
print_string Sys.argv.(1);;
print_string "\n\n";;

try
  while(true) do
    print_string ">> ";
    let line = read_line() in
    if line = "quit." then exit 0
    else try
      let g = Parser.goal Lexer.token (Lexing.from_string line) in
      match (interpret_goal prog g) with
          (true, _) -> print_string "true.\n"
        | (false, _) -> print_string "false.\n"
    with e -> Printf.printf "%s\n" ("Unexpected Error! Try Again.\n")
  done

with _ -> print_string "\n>> Quitting...\n"
