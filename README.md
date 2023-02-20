# AutoHyper: Explicit-State Model Checking for HyperLTL

This repository contains AutoHyper (short for either "**Auto**mata-based Model Checking of **Hyper**LTL" or "Fully **Auto**matic **Hyper**LTL Model Checking"), a tool that can model check HyperLTL properties on finite-state systems. 

## Overview

AutoHyper reads a transition system, the [HyperLTL](https://doi.org/10.1007/978-3-642-54792-8_15) property to be checked and determines if the property holds on the given system.
The system can either be provided as an explicit-state system or a symbolic system that is internally converted to an explicit-state system.
Support for symbolic systems includes a fragment of the [NuSMV specification language](https://nusmv.fbk.eu/NuSMV/userman/v26/nusmv.pdf) and programs in a simple [boolean programming language](https://doi.org/10.4230/LIPIcs.CONCUR.2021.24). 


## Structure 

- `src/` contains the source code of AutoHyper and the tool used to produce the plots in the paper (both written in F#).
- `benchmarks/` contains various HyperLTL benchmarks.
- `app/` is the target folder for the build. The final AutoHyper executable will be placed here. 

## Build AutoHyper


### Dependencies

We require the following dependencies:

- [.NET 7 SDK](https://dotnet.microsoft.com/en-us/download) (tested with version 7.0.102)
- [spot](https://spot.lrde.epita.fr/)

Install the .NET 7 SDK (see [here](https://dotnet.microsoft.com/en-us/download) for details) and make sure it is installed correctly by running `dotnet --version`.
Download and build spot (details can be found [here](https://spot.lrde.epita.fr/)). You can place the spot executables in any location of your choosing. 
To use AutoHyper you need to provide it with the *absolute* path to spot (see details below).


### Clone AutoHyper

Run 

```
git clone https://github.com/AutoHyper/autohyper
cd autohyper
git submodule init
git submodule update
```

to clone the latest version of AutoHyper.

### Build AutoHyper

To build AutoHyper run the following (when in the main directory of this repository).

```shell
cd src
dotnet build -c "release" -o ../app
cd ..
```

Afterward, the AutoHyper executable is located in the `app/` folder.
The AutoHyper executable is not standalone and requires the other files located in the `app/` folder (as is usual for .NET applications).
Moreover, it will only run on systems that have the .NET Core Runtime installed. 
Test that everything works as expected by running 

```shell
./app/AutoHyper --version
```

### Connect spot to AutoHyper

AutoHyper requires the *autfilt* and *ltl2tgba* tools from the spot library.
AutoHyper is designed such that it only needs the **absolute** path to these executables, so they can be installed and placed at whatever locations fits best.
The absolute paths are specified in a `paths.json` configuration file. 
This file must be located in the same directory as the AutoHyper executable (this convention makes it easy to find the config file, independent of the relative path AutoHyper is called from). 
We already provide a template file `app/paths.json` that *needs to be modified*. 
After having built spot, paste the absolute path to the *autfilt* and *ltl2tgba* executables to the `paths.json` file. 
For example, if `/usr/bin/autfilt` and `/usr/bin/ltl2tgba` are the *autfilt* and *ltl2tgba* executables, the content of `app/paths.json` should be

```
{
    "autfilt":"/usr/bin/autfilt",
    "ltl2tgba":"/usr/bin/ltl2tgba"
}
```

### A First Example

To test that the paths have been setup correctly, we can verify our first instance by running

```shell
app/AutoHyper -bp ./benchmarks/bp/concur_p1_1bit.txt ./benchmarks/bp/gni.txt
```

which should return `SAT`.

### Setup Additional Solver

In addition to spot, AutoHyper also supports the [rabit](https://github.com/iscas-tis/RABIT), [bait](https://github.com/parof/bait), and [forklift](https://github.com/Mazzocchi/FORKLIFT) inclusion checkers. 
These tools are optional and will only be used when the appropriate mode is set (see the Section [Command Line Arguments](#command-line-arguments))
Rabit, bait, and forklift are written in Java and AutoHyper requires the absolute path to the `.jar` file of each solver. 
The content of the `paths.json` is then 

```
{
    "autfilt":"<...>/autfilt",
    "ltl2tgba":"<...>/ltl2tgba",
    "bait":"<...>/bait.jar",
    "rabit":"<...>/rabit.jar",
    "forklift":"<...>/forklift.jar"
}
```

You can also only give some of the solver paths.
If you use a mode but have not specified a path to the corresponding solver, AutoHyper will raise an error.


## Run AutoHyper

After having built AutoHyper and all its dependencies, you are ready to use AutoHyper.
You can run AutoHyper by running `app/AutoHyper <args>` where `<args>` are the command line arguments (discussed in detail below).

AutoHyper supports different input modes:
- Explicit-state systems
- NuSMV-like models 
- Boolean programs

We give details on the command line options and the input format for each input mode in Section [Input for AutoHyper](#input-for-autohyper). 

## Input for AutoHyper

In this section, we first discuss the command-line options of AutoHyper, followed by the structure of supported input. 

### Command-line Arguments

AutoHyper supports several command-line options.
Depending on which type of system you want to check (either explicit-state systems, NuSMV-like systems, or boolean programs), use one of the following command-line options:

- `-e <systemPath(s)> <propPath>` applies AutoHyper to an explicit-state system. 
- `-nusmv <systemPath(s)> <propPath>` applies AutoHyper to a NuSMV-like model. 
- `-bp <systemPath(s)> <propPath>` applies AutoHyper to a boolean program. 

In all the above options, `<systemPath(s)>` is either a single path to the system or multiple such paths. `<propPath>` is the path to the property.
In case  `<systemPath(s)>` is only a single path, we use the system at this path to resolve all quantifiers. In case `<systemPath(s)>` are multiple paths, their number must match the quantifier prefix in the HyperLTL property (specified via `<propPath>`).
If, for example, the prefix has two quantifiers, you can call `-e ts1 ts2 prop` where `ts1` and `ts2` are (paths to) explicit-state systems and `prop` contains a HyperLTL property with two quantifiers.
If you run `-e ts1 prop` both quantifiers are resolved on `ts1`.
Exactly one of the above command line options (i.e., `-e`, `-nusmv`, or `-bp`) must be used.

For details on how the system and property are specified, we refer to the following sections.   

Additional (and optional) command-line options include

- `-m` sets the mode that AutoHyper uses for Model Checking. Available options are 
    - `comp` reduces to the emptiness of an automaton
    - `incl_spot` uses spot's inclusion check
    - `incl_rabit` uses RABIT's inclusion check (requires a working copy of rabit, see Section [])
    - `incl_bait` uses BAIT's inclusion check (requires a working copy of bait, see Section [])
    - `incl_forklift` uses FORKLIFT's inclusion check (requires a working copy of forklift, see Section [])
    If not set, AutoHyper use `incl_spot` as default.
- `-v` sets the verbosity. Available options are `0`, `1`, `2`, `3`, `4`.
    If set to `0` only the result (`SAT`, `UNSAT`, `TIMEOUT`) and potential error messages are printed. 
    The higher the verbosity, the more information is printed. 
    The default level is set to `0`.
- `-t` sets the timeout in milliseconds. If not set, we use no timeout.
- `--version` displays the AutoHyper version.
- `--license` displays information about the license of AutoHyper.
- `--help` displays a help message.


### Specifying HyperLTL Properties

The specification checked by AutoHyper is written in [HyperLTL](https://doi.org/10.1007/978-3-642-54792-8_15).
A HyperLTL formula consists of an LTL-like body, preceded by a quantifier prefix. 
Formulas have the form `<qfPrefix> <ltlBody>`.

Here `<ltlBody>` can be one of the following:
- `1`: specifies the boolean constant true
- `0`: specifies the boolean constant false
- An atomic proposition atom. Depending on which type of system (explicit-state, NuSMV,...) is considered, atomic propositions have different structures. We give details in the following sections.
- `(<ltlBody>)`: Parenthesis
- `<ltlBody> & <ltlBody>`: Conjunction
- `<ltlBody> | <ltlBody>`: Disjunction
- `<ltlBody> -> <ltlBody>`: Implication
- `<ltlBody> <-> <ltlBody>`: Equivalence
- `<ltlBody> U <ltlBody>`: Until
- `<ltlBody> W <ltlBody>`: Weak Until
- `<ltlBody> R <ltlBody>`: Release
- `F <ltlBody>`: Eventually
- `G <ltlBody>`: Globally
- `X <ltlBody>`: Next
- `! <ltlBody>`: Boolean Negation

The operators follow the usual operator precedences. To avoid ambiguity, we recommend always placing parenthesis around each construct. 

The quantifier prefix `<qfPrefix>` can be one of the following:
- The empty string
- Universal quantification `forall <VAR>. <qfPrefix>`
- Existential quantification `exists <VAR>. <qfPrefix>`

Here `<VAR>` is a trace variable, which is any non-empty sequence consisting only of letters. 

For examples of HyperLTL properties (with the atomic propositions used in either case), see the following sections. 

### Specifying Explicit-state Transition Systems

When using `-e`, AutoHyper expects an explicit-state transition system.
An explicit-state system has the form 

```
aps "<AP>" ... "<AP>"
init <stateID> ... <stateID> 
--BODY-- 
<stateDefinition>
...
<stateDefinition>
```

Here, `<AP>` is an atomic proposition. This can be any string not containing `"`. Note that all atomic propositions are escaped.
`<stateID>` is any natural number specifying a state. 
The header specifies which states are initial (there must be at least one initial state) and which APs are used in the system.

A `<stateDefinition>` has the form 
```
State: <stateID> <apEval>
<stateID> ... <stateID>
```

It specifies which state we are defining and the evaluation of the atomic propositions in that state. 
The `<apEval>` has the form `[(t|f) ... (t|f)]` and specifies if each atomic proposition holds (`t`) or does not hold `f`. The length of this list must match the number of atomic propositions listed in the header. 
The second line lists all successors of that state.
Every state must have at least one successor state.

Consider the following example:

```
aps "x" "y"
init 0 1
--BODY-- 
State: 0 [f f]
0 2 3
State: 1 [f t]
0 1 2
State: 2 [t f]
0 2 3
State: 3 [t t]
2 3
```

This specification declares states `0` and  `1` as initial states and `"x"` and `"y"` as APs.
For each state, we give the evaluation of the atomic propositions as a list of booleans (either `f`, or `t`).
For example, in state `1`, AP `"x"` does not hold but `"y"` does.
Each state lists all successors of that node. 
For example, the successor states of state `0` are states `0`, `2`, and `3`.

HyperLTL properties on explicit-state systems are specified by using atomic propositions of the form `"<AP>"_<Var>` where `<AP>` is the AP as given in the system, and `<VAR>` is a trace variable from the quantifier prefix. 
This atomic proposition holds if, in the trace bound to `<VAR>`, the AP `<AP>` holds. 

An example property on the above example system would be:

```
forall A. exists B. X (G ("x"_A <-> "y"_B))
```

### Specifying Symbolic Transition Systems

When using `-nusmv`, AutoHyper expects a (symbolic) NuSMV-like system.
See [here](https://nusmv.fbk.eu/NuSMV/userman/v26/nusmv.pdf) for details on the format.
The systems supported by AutoHyper differ slightly from the NuSMV standard.
We give details on the syntax, followed by an example.

A NuSMV-like model (as supported by AutoHyper) has the following structure:

```
MODULE <name>
VAR 
    <varTypeBlock>

ASSIGN
    <assignBlock>

DEFINE
    <defineBlock>
```

Here `<name>` is an arbitrary name (consisting only of letters) of that module. Note that AutoHyper only supports a *single* module. 

The `<varTypeBlock>` contains a sequence of type assignments of the form 
```
<varName> : <type>;
```
where `<varName>` is the name of a variable.
A valid variable name starts with a letter or `_`, followed by an arbitrary sequence of letters, digits, or the special symbols `_`, `$`, `#`, `-`, `[`, `]`, `.`.
Note that different from NuSMV, we support `[`, `]`, `.` in variable names.
`<type>` can either be `boolean` or `l..h` where `l` and `h` are two natural numbers. 
Note that we do not support arrays, but instead allow `[`, `]`, `.` in variable names.
This allows us to easily express arrays by using explicit variables for each entry.
For example, instead of declaring `var : array 0..2 of 0..3;`, we can declare three individual variables `var[0] : 0..3; var[1] : 0..3; var[2] : 0..3;` and access them in expression as usual (via `var[0]`, `var[1]`, and `var[2]`).


The `<assignBlock>` contains a sequence of assignments that either have the form 
```
init(<varName>) := <expression>;
```
or 
```
next(<varName>) := <expression>;
```
In the former case, we define all possible initial values for variable `<varName>`. Here `<expression>` is an expression that contains no variables (We define expressions below).
In this case, `<expression>` must evaluate to a value that matches the type assigned to `<varName>` in the `<varTypeBlock>`. Alternatively, it can also evaluate to a set of values, in which case there are multiple initial values for that variable.

In the latter case, we define all successor values for the variable in one step during the evaluation. `<expression>` is again a variable (which this time may contain variables).
In each step, we evaluate the expression in the current state and assign the resulting value to `<varName>` in the next state.
If `<expression>` evaluates to a set of values, we branch into all possible values. 
In this case, the `<expression>` can use both program variables (defined in the `<varTypeBlock>`) and also defined variables (from the `<defineBlock>`, see next).

The `<defineBlock>` contains variable definitions of the form 
```
<varName> := <expression>;
```
Here `<varName>` is a variable name that is *not* listed in the `<varTypeBlock>`. In each step, we can consider `<varName>` as an abbreviation for `<expression>`.
`<expression>` can use both the program variables (defined in the `<varTypeBlock>`) and also further defined variables (from the `<defineBlock>`).


An `<expression>` is either 
- `TRUE`: (boolean true).
- `FALSE`: (boolean false).
- A natural number as a constant.
- A variable (either a program variable from `<varTypeBlock>` or a defined variable from `<defineBlock>`).
- `{ <expression>, ..., <expression>}`: Set expression.
- `<expression> <opp> <expression>`: Binary operation. Here `<opp>` can be: `&` (and), `|` (or), `->` (implies), `=` (equals), `!=` (not equals), `<=` (less or equal), `>=` (greater or equal), `<` (less), `>` (greater), `+` (addition), `-` (subtraction). 
- `<opp> <expression>`: Unary operation. Here `<opp>` can be: `!` (boolean negation).
- `case <expression>:<expression>; ... <expression>:<expression>; esac`: Case expression. During evaluation we search for the first `<expression>:<expression>` pair where the first expression (the guard) evaluates to true and then compute the value of the second expression in that pair.


Consider the following example:

```
MODULE main
VAR
    mutation: boolean;
    action: 0..2;
    beverage: 0..2;
    water: 0..3;


ASSIGN
    init(mutation) := {TRUE, FALSE};
    next(mutation) := {TRUE, FALSE};

    init(action) := 0;
    next(action) := {0,1,2};

    init(beverage) :=0;
    next(beverage) :=
        case
            (action=1 & !(water=0)): 1;
            TRUE: beverage;
        esac;

    init(water) :=2;
    next(water) :=
        case
            (action=1 & !(water=0)): water - 1; -- make beverage
            (mutation & water=0): 1;
            (mutation & water=1): 2;
            (mutation & water=2): 3;
            (mutation & water=3): 3;
            (!mutation & water=0): 2;
            (!mutation & water=1): 3;
            (!mutation & water=2): 3;
            (!mutation & water=3): 3;
            TRUE: water;
        esac;

DEFINE
    NO_water := (action=1) & (water=0);
    NO_output := (action!=1)  | ((action=1)&(beverage=0));
```

HyperLTL properties on symbolic NuSMV-like systems are specified by using atomic propositions of two forms.
Either they have the form
```
*<expression>*_<VAR>
```
where `<expression>` is an expression of type bool and `<VAR>` is a trace variable.
This atomic proposition holds whenever `<expression>` evaluated in the current step of the trace bound to `<VAR>` yields `TRUE`. 


Or of the form 
```
*<expression1>*_<VAR1> = *<expression2>*_<VAR2>
```
where `<expression1>` and `<expression2>` are expressions of the same type, and `<VAR1>` and `<VAR2>` are trace variables.
This atomic proposition states that the result of evaluating `<expression1>` on trace `<VAR1>` is the same as evaluating expression `<expression2>` on trace `<VAR2>`.

An example property on the above example system would be:

```
forall A. exists B. 
(
    (*action*_A = *action*_B)  U  (*NO_water*_B)
)
```

Here `NO_water` is a boolean variable in the system (in this case a defined one) and thus also a boolean expression.
`action` is a non-boolean expression that is compared across traces `A` and `B`.

For further examples, take a look at the `benchmarks/symbolic` and `benchmarks/planning` folder.

### Specifying Boolean Programs

When using `-bp`, AutoHyper expects a system given as a boolean program.
A boolean program operates on variables that hold a (statically bounded) vector of boolean values. A boolean program therefore always denotes a finite-state system. 

A boolean program has the form 
```
dom [(<varName>, i1) ... (<varName>, in)]
<statement>
```

The header defines a bitwidth for each variable. 
A valid variable name (`<varName>`) is any non-empty sequence of letters. 
`i1`, ..., `in` are positive natural numbers. 
For example `[(h, 1) (l, 2)]` specifies that variable `h` holds a 1-bit vector and `l` holds two bits. 

Statements (`<statement>`) are formed by using expressions. 

Expressions `<expression>` have the form 
- `<varName>`: The value currently bound to a variable
- `t`: boolean true
- `f`: boolean false
- `(& <expression> <expression>)`: Pointwise conjunction. Assumes both arguments to evaluate to vectors of the same length.
- `(| <expression> <expression>)`: Pointwise disjunction. Assumes both arguments to evaluate to vectors of the same length.
- `(! <expression>)`: Pointwise negation
- `(x <expression> i)`: Duplication where `i` is a natural number. First evaluates `<expression>` and the duplicates (concats) that vector `i` times. 
- `(#<expression> i)`: Projection where `i` is a natural number. Evaluates to the `i`th value in the vector that `e` evaluates to.

Statements (`<statement>`) have the form 
- `<varName> := <expression>`: Assigns the value that `<expression>` evaluates to variable `<varName>`.
- `READ(<varName>)`:  Assigns a non-deterministic value to `<varName>`, modelling an unknown input. 
- `IF(<expression>) THEN {<statement>} ELSE {<statement>}`: Branches on whether or not `<expression>` evaluates to `[t]`.
- `[<statement>; <statement>; ....; <statement>]`: Execute all statements one after each other.
- `WHILE(<expression>) {<statement>}`: Executes `<statement>` as long as `<expression>` evaluates to `[t]`.
- `NONDET THEN {<statement>} ELSE {<statement>}`: Nondeterministic branching.


Initially, all variables are assigned the constant `f` vector (of the length specified in the domain in the first line).
For details on the semantics see ["A Temporal Logic for Strategic Hyperproperties"](https://doi.org/10.4230/LIPIcs.CONCUR.2021.24).


Consider the following example boolean program:

```
dom: [(h 2), (l 2), (o 2)]
[
    o := (x t 2);
    WHILE(t) {
        [
            READ(h);
            IF((#h 0)) THEN {
                o := (! o)
            } ELSE {
                o := (& (!o) (| h (!h)))
            }
        ]
    }
]
```

HyperLTL properties on boolean programs are specified by using atomic propositions of the form `{<varName>_j}_<VAR>` where `<varName>` is a variable in the program, `j` an index and `<VAR>` refers to a trace variable from the quantifier prefix.
The AP `{<varName>_j}_<VAR>` holds whenever the `j`th value in the vector currently assigned to `<varName>` holds on trace `<VAR>`.
For example `{h_0}_A` refers to the first position in the boolean vector value bound to `h` on the trace named `A`. 

On the above system, we can, for example, express GNI as follows:
```
forall A. forall B. exists C. (G ({h_0}_A <-> {h_0}_C)) & (G({l_0}_B <-> {l_0}_C)) & (G({o_0}_B <-> {o_0}_C))
```

For further examples, take a look at the `benchmarks/bp` folder.
