

let compile ch name number =
    let open Printf in
    fprintf ch ".global %s\n" name;
    fprintf ch "%s:\n" name;
    fprintf ch "movq $%s, %%rax\n" number;
    fprintf ch "ret\n"
