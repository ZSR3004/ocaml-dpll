# DPLL in OCaml

## About

This was just another neat little project I initially made for myself. This is a very barebones 
[DPLL](https://en.wikipedia.org/wiki/DPLL_algorithm) solver, which is a type of
[SAT Solver](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem). There are a ton of resources
online about SAT Solvers and DPLL, so I thought I would throw my hat into the ring and add yet another
resource for anyone trying to learn. Hopefully, this project can help someone understand the implementation
of DPLL a little better.

If you're starting from scratch (or close to it), take a look at the links above about SAT Solving and DPLL.
Also, take a look at [this link on CNF formulas](https://en.wikipedia.org/wiki/Conjunctive_normal_form) 
which will help explain why the formula and clauses are setup the way they are.

I also want to point out that this was made in wonderful OCaml, which I'm hoping to write more code in.
This may be my favorite programming language so far.

## Installation and Running

There isn't a lot to install, but you can setup the project and add new test cases to see what's happening.
If you're trying to learn, I would encourage adding print statements to see what the solver is doing at
every step.

I used [Dune](https://dune.build) to build this project, so I'll walk through what to do for that. Also, 
make sure you actually have [OCaml and Opam Insatlled](https://ocaml.org/install#linux_mac_bsd).

For reference, Dune builds and runs projects (kind of like how Rust has `cargo build` and `cargo test`) and
`Opam` is kind of like a package manager for OCaml (like `pip` for Python), but with a few more features.

Just so you know, the code itself is in `dpll.ml`. The files `dune` and `dune-project` just tell Dune
how to build the project, so I wouldn't touch those.

To start, clone this repository and `cd` into it. Now, we'll create a switch (which is kind of like a 
virtual environment)
```bash
opam switch create 5.4.0    # This was the version I ran it on, but it'll probably work on the latest
eval $(opam env --switch=5.4.0)
```

Now, we'll install the necessary packages.
```bash
opam install ppx_inline_test ppx_deriving
```

Now, we just need to eval so everything works.
```bash
eval $(opam env)
```

That's it! Now, you can run the tests with
```bash
dune test
```

Have fun!
