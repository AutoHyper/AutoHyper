# AutoHyper: Explicit-State Model Checking for HyperLTL

This repository contains **AutoHyper** (short for either "**Auto**mata-based Model Checking of **Hyper**LTL" or "Fully **Auto**matic **Hyper**LTL Model Checking"), a tool that can model check HyperLTL properties on finite-state systems. 


Clone this repository and **initialize all submodules** by running 

```shell
git clone https://github.com/AutoHyper/autohyper
cd autohyper
git submodule init
git submodule update
```

## Overview

AutoHyper reads a transition system and a [HyperLTL](https://doi.org/10.1007/978-3-642-54792-8_15) property and determines if the property holds on the given system.
The system can either be provided as an explicit-state system or a symbolic system that is internally converted to an explicit-state representation.
Support for symbolic systems includes a fragment of the [NuSMV specification language](https://nusmv.fbk.eu/NuSMV/userman/v26/nusmv.pdf) and programs in a simple boolean programming language [2].
You can read the details in our paper:

> AutoHyper: Explicit-State Model Checking for HyperLTL. Raven Beutner and Bernd Finkbeiner. TACAS 2023. [1]


## Structure 

- `src/` contains the source code of AutoHyper and the tool used to produce the plots in the paper (both written in F#).
- `benchmarks/` contains various HyperLTL benchmarks.
- `app/` is the target folder for the build. The final AutoHyper executable will be placed here. 

## Build AutoHyper

### Dependencies

We require the following dependencies:

- [.NET 7 SDK](https://dotnet.microsoft.com/en-us/download) (tested with version 7.0.302)
- [spot](https://spot.lrde.epita.fr/)

Install the .NET 7 SDK (see [here](https://dotnet.microsoft.com/en-us/download) for details) and make sure it is installed correctly by running `dotnet --version`.
Download and build spot (details can be found [here](https://spot.lrde.epita.fr/)). 
You can place the spot executables in any location of your choosing. 
To use AutoHyper you need to provide it with the *absolute* path to spot (see details below).

### Build AutoHyper

To build AutoHyper run the following (when in the main directory of this repository).

```shell
cd src/AutoHyper
dotnet build -c "release" -o ../../app
cd ../..
```

Afterward, the AutoHyper executable is located in the `app/` folder.
The AutoHyper executable is not standalone and requires the other files located in the `app/` folder (as is usual for .NET applications).
Moreover, it will only run on systems that have the .NET Core Runtime installed. 
Test that everything works as expected by running 

```shell
app/AutoHyper --version
```

### Connect spot to AutoHyper

AutoHyper requires the *autfilt* and *ltl2tgba* tools from the spot library.
AutoHyper is designed such that it only needs the **absolute** path to these executables, so they can be installed and placed at whatever locations fits best.
The absolute paths are specified in a `paths.json` configuration file. 
This file must be located in the same directory as the AutoHyper executable (this convention makes it easy to find the config file, independent of the relative path AutoHyper is called from). 
We already provide a template file `app/paths.json` that *needs to be modified*. 
After having built spot, paste the absolute path to the *autfilt* and *ltl2tgba* executables to the `paths.json` file. 
For example, if `/usr/bin/autfilt` and `/usr/bin/ltl2tgba` are the *autfilt* and *ltl2tgba* executables, the content of `app/paths.json` should be

```json
{
    "autfilt":"/usr/bin/autfilt",
    "ltl2tgba":"/usr/bin/ltl2tgba"
}
```

### A First Example

To test that the paths have been setup correctly, we can verify our first instance by running

```shell
app/AutoHyper --bp ./benchmarks/bp/concur_p1_1bit.txt ./benchmarks/bp/gni.txt
```

which should return `SAT`.

### Setup Additional Solver

In addition to spot, AutoHyper also supports the [rabit](https://github.com/iscas-tis/RABIT), [bait](https://github.com/parof/bait), and [forklift](https://github.com/Mazzocchi/FORKLIFT) inclusion checkers. 
These tools are optional and will only be used when the appropriate mode is set (see the Section [Command Line Arguments](#command-line-arguments))
Rabit, bait, and forklift are written in Java and AutoHyper requires the absolute path to the `.jar` file of each solver. 
The content of the `paths.json` is then 

```json
{
    "autfilt":"<...>/autfilt",
    "ltl2tgba":"<...>/ltl2tgba",
    "bait":"<...>/bait.jar",
    "rabit":"<...>/rabit.jar",
    "forklift":"<...>/forklift.jar"
}
```

You can also only provide only some of the solver paths.
If you use a mode but have not specified a path to the corresponding solver, AutoHyper will raise an error.


## Run AutoHyper

After having built AutoHyper and all its dependencies, you are ready to use AutoHyper.
You can run AutoHyper by running `app/AutoHyper <args>` where `<args>` are the command line arguments (discussed in detail below).

AutoHyper supports different input modes:
- Explicit-state systems
- (a fragment of) NuSMV models 
- Boolean programs

We give details on the command line options and the input format for each input mode in Section [Input for AutoHyper](#input-for-autohyper). 

## Input for AutoHyper

In this section, we first discuss the command-line options of AutoHyper, followed by the structure of supported input. 

### Command-line Arguments

AutoHyper supports several command-line options.
Depending on which type of system you want to check (either explicit-state systems, NuSMV-like systems, or boolean programs), use one of the following command-line options:

- `--exp <systemPath(s)> <propPath>` applies AutoHyper to an explicit-state system. 
- `--nusmv <systemPath(s)> <propPath>` applies AutoHyper to a symbolic NuSMV model. 
- `--bp <systemPath(s)> <propPath>` applies AutoHyper to a boolean program. 

In all the above options, `<systemPath(s)>` is either a single path to the system or multiple such paths. 
`<propPath>` is the path to the property.
In case  `<systemPath(s)>` is only a single path, we use the system at this path to resolve all quantifiers. In case `<systemPath(s)>` are multiple paths, their number must match the quantifier prefix in the HyperLTL property (specified via `<propPath>`).
If, for example, the prefix has two quantifiers, you can call `--exp ts1 ts2 prop` where `ts1` and `ts2` are (paths to) explicit-state systems and `prop` contains a HyperLTL property with two quantifiers.
If you run `--exp ts1 prop` both quantifiers are resolved on `ts1`.
Exactly one of the above command line options (i.e., `--exp`, `--nusmv`, or `--bp`) must be used.

For details on how the system and property are specified, we refer to the following sections.   

Additional (and optional) command-line options include

- `-m` sets the mode that AutoHyper uses for Model Checking. Available options are 
    - `comp` reduces to the emptiness of an automaton
    - `incl_spot` uses spot's inclusion check
    - `incl_rabit` uses RABIT's inclusion check (requires a working copy of rabit, see Section [])
    - `incl_bait` uses BAIT's inclusion check (requires a working copy of bait, see Section [])
    - `incl_forklift` uses FORKLIFT's inclusion check (requires a working copy of forklift, see Section [])
    If not set, AutoHyper uses `incl_spot` as default.
- `-v` sets the verbosity. Available options are `0`, `1`, `2`, `3`, `4`.
    If set to `0` only the result (`SAT`, `UNSAT`) and potential error messages are printed. 
    The higher the verbosity, the more information is printed. 
    The default level is set to `0`.
- `--version` displays the AutoHyper version.
- `--license` displays information about the license of AutoHyper.
- `--help` displays a help message.


### Specifying HyperLTL Properties

The specification checked by AutoHyper is the temporal logic [HyperLTL](https://doi.org/10.1007/978-3-642-54792-8_15).
A HyperLTL formula consists of an LTL-like body, preceded by a quantifier prefix. 
Formulas have the form `<qfPrefix> <ltlBody>`.

Here `<ltlBody>` can be one of the following:
- `1`: specifies the boolean constant true
- `0`: specifies the boolean constant false
- An atomic proposition atom. Depending on which type of system (explicit-state, NuSMV model,...) we consider, atomic propositions have different structures. We give details in the following sections.
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

Here `<VAR>` is a trace variable, which is any non-empty sequence consisting only of letters and digits (starting with a letter). 

For examples of HyperLTL properties (with the atomic propositions used in either case), see the following sections. 

### Specifying Explicit-state Transition Systems

When using option `--exp`, AutoHyper expects an *explicit-state transition system*.
An explicit-state system has the form 

```
AP: "<AP>" ... "<AP>"
Init: <stateID> ... <stateID> 
--BODY-- 
<stateDefinition>
...
<stateDefinition>
--END--
```

Here, `<AP>` is an atomic proposition that can be any *escaped* string. 
`<stateID>` is a natural number specifying a state. 
The header specifies which states are initial (there must be at least one initial state) and which APs are used in the system.

A `<stateDefinition>` has the form 
```
State: <stateID> <apEval>
<stateID> ... <stateID>
```

It specifies which state we are defining and the evaluation of the atomic propositions in that state. 
The `<apEval>` has the form `{<apIndex> ... <apIndex>}` where `<apIndex>` is a natural number that identifies one of the provided APs. 
The second line lists all successors of that state.
Every state must have at least one successor state.

Consider the following example:

```
AP: "x" "y"
Init: 0 1
--BODY-- 
State: 0 {}
0 2 3
State: 1 {1}
0 1 2
State: 2 {0}
0 2 3
State: 3 {0 1}
2 3
--END--
```

This specification declares states `0` and  `1` as initial states and `"x"` and `"y"` as APs.
For each state, we give the evaluation of the atomic propositions by listing the indices of all APs which hold in the given state.
For example, in state `1`, AP `"x"` (index 0) does not hold but `"y"` (index 1) does.
Each state lists all successors of that node. 
For example, the successor states of state `0` are states `0`, `2`, and `3`.


HyperLTL properties on explicit-state systems are specified by using atomic propositions of the form `"<AP>"_<Var>` where `<AP>` is the AP as given in the system, and `<VAR>` is a trace variable from the quantifier prefix. 
This atomic proposition holds if, in the trace bound to `<VAR>`, the AP `<AP>` holds. 

An example property on the above example system would be:

```
forall A. exists B. X (G ("x"_A <-> "y"_B))
```

### Specifying Symbolic Transition Systems

When using option `--nusmv`, AutoHyper expects a *NuSMV system* with finite variable domains (so the system denotes a finite-state transition system).
AutoHyper supports only a fragment of the NuSMV specification language.
In particular, we assume that the system consists of a *single module*. 
In Section [Convert To Single Module System](#convert-to-single-module-system) we give details on how to automatically convert a system into a single-module system. 


A single-module NuSMV model (as supported by AutoHyper) has the following structure:

```
MODULE <name>
<variableDeclarationBlock>
<bodyBlock>
```

Here `<name>` is an arbitrary name (consisting only of letters) of that module.

#### Variable Declaration Block

The `<variableDeclarationBlock>` declares all variables and assigns a type. 
The variable declaration block has the form 

```
VAR 
<varName> : <type>;
<varName> : <type>;
...
<varName> : <type>;
```

and contains a sequence of type assignments. 
Here `<varName>` is a variable name.
A valid variable name is any sequence of characters that starts with a letter or `_`, followed by an arbitrary sequence of letters, digits, or the special symbols `_`, `$`, `#`, `-`, `[`, `]`, `.` and is no reserved keyword.

`<type>` gives the type of that variable. 
Support types are:
- `boolean`: The boolean type with allowed values `TRUE` and `FALSE`
- `{n_1, ..., n_M}` where `n_1`, ..., `n_m` are integers: This is a set type that can hold any natural number contained in the set
- `l..h` where `l <= h` are integer: This range type can hold any integer between `l` and `h`. Internally, we treat it as a shorthand for `{l, l+1, ..., h-1, h}`.
- `array l..h of <type>` where `l <= h` are integer and `<type>` is an arbitrary (recursively defined) type: This array type models an array with valid indices for all inetger between `l` and `h`. Internally, we eliminate arrays and instead introduce a separate variable for each position. For example, the type declaration `x : array 0..2 of boolean;` will be reduced to the type declarations `x[0] : boolean; x[1] : boolean; x[2] : boolean;`.




#### Assignments and Definitions

The `<bodyBlock>` defines the actual behavior of the system. In it, we can pose initial conditions on all variables, describe how the variables should be updated and introduce additional (defined) shorthands. 
The `<bodyBlock>` is a sequence of assignment blocks and definition blocks, i.e., `<bodyBlock> = (<assignmentBlock> + <definitionBlock>)^*` where `<assignmentBlock>` and `<definitionBlock>` are defined in the following. 

##### Assignment Block

An assignment block (`<assignmentBlock>`) has the form 

```
ASSIGN 
<initOrNext>
<initOrNext>
...
<initOrNext>
```

where `<initOrNext>` poses an *initial* or *next* condition.
An `<initOrNext>` either has the form
```
init(<varName>) := <expression>;
```
or 
```
next(<varName>) := <expression>;
```
where `<varName>` is the name of a variable (the variable must be defined in the type declaration section) and <expression>` is a NuSMV expression (formally defined below).
In the former case, we define all possible initial values for `<varName>`. 
The expression evaluates to a set of values (in the special case, where the expression is deterministic, it evaluates to a singleton set giving the precise value) and all such values are initial states of the system. 
The expression can refer to other variables, i.e., we allow initial conditions such as `init(x) := y;`, but there can be no cyclic dependency when evaluating the initial expressions. 

In the latter case, we define all successor values for the variable in one step during the evaluation. 
In each step, we evaluate the expression in the current state and assign the resulting value to `<varName>` in the next state.
As before, the expression evaluates to a set of values (in the deterministic case a singleton set) and we consider all possible values in the next step.

In either of the two cases, the expression can refer to program variables (those defined in the type declaration section) and also *defined* words (defined below).
We always forbid cyclic dependencies of the variables.  


##### Definition Block

A definition block (`<definitionBlock>`) has the form 

```
DEFINE
<definition>
<definition>
...
<definition>
```

Each `<definition>` has the form
```
<varName> := <expression>;
```
Here `<varName>` is a variable name that is *not* listed in the `<varTypeBlock>` which we define a shorthand.
As in the assignment block, we forbid cylic depecies during the evlauation of definitions.

#### Expressions

An expression is evaluated in a state `s` (a concrete assignment of all declared variables to values) and yields a set (possibly a singleton set) of values.
An `<expression>` can have the following forms:

- `TRUE`: the boolean true constant; evaluates to singelton set `{TRUE}`
- `FALSE`: the boolean false constant; evaluates to singelton set `{TRUE}`
- `<n>` where `<n>` is any integer: an integer constant; evaluates to singelton set `{<n>}`
- `<varName>`: a variable that is either declared in the `VAR` block or defined in a `DEFINE` block. In case `<varName>` is declared, it evaluates to `{s(<varName>)}`, i.e., the singleton set containing value of `<varName>` in the current state. In case `<varName>` is defined, we (recursively) evaluate the defined expression in state `s`.
- `toInt(<expression>)`: converts a boolean value to an integer; we first (recursively) evaluate the subexpression and map all `TRUE`s to `0`s and all `FALSE`s to `1`s.
- `toBool(<expression>)`: converts a integer value to a boolean; we first (recursively) evaluate the subexpression and map all `0`s to `FALSE`s and all other values to `TRUE`s.
- `case <expression>:<expression>; ... <expression>:<expression>; esac`: a case expression; During the evaluation, we search for the first `<expression>:<expression>` pair where the first expression (the guard) evaluates to true and then compute the value of the second expression in that pair.
- `{ <expression>, ..., <expression>}`: a set expression; We (recursively) evaluate all subexpressions and take the union of all sets
- `<expression> <opp> <expression>`: Binary operation. Here `<opp>` can be: `&` (and), `|` (or), `->` (implies), `=` (equals), `!=` (not equals), `<=` (less or equal), `>=` (greater or equal), `<` (less), `>` (greater), `+` (addition), `-` (subtraction). We (recursively) evaluate both operants and apply each binary operation to all possible value combinations. 
- `<opp> <expression>`: Unary operation. Here `<opp>` can be: `!` (boolean negation). We (recursively) evaluate the operant and apply each unary operation to all possible values. 



#### Example 

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

    init(beverage) := 0;
    next(beverage) :=
        case
            (action=1 & !(water=0)): 1;
            TRUE: beverage;
        esac;

    init(water) := 2;
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

#### HyperLTL in NuSMV Systems 

HyperLTL properties on symbolic NuSMV systems are specified by using atomic propositions of two forms.
Either they have the form
```
{<expression>}_<VAR>
```
where `<expression>` is an expression of type bool and `<VAR>` is a trace variable.
This atomic proposition holds whenever `<expression>` evaluated in the current step of the trace bound to `<VAR>` evaluates to `{TRUE}`. 


Or of the form 
```
{<expression1>}_<VAR1> = {<expression2>}_<VAR2>
```
where `<expression1>` and `<expression2>` are expressions of the same type, and `<VAR1>` and `<VAR2>` are trace variables.
This atomic proposition states that the result of evaluating `<expression1>` on trace `<VAR1>` is the same as evaluating expression `<expression2>` on trace `<VAR2>`.

An example property on the above example system would be:

```
forall A. exists B. 
(
    ({action}_A = {action}_B)  U  ({NO_water}_B)
)
```

Here `NO_water` is a boolean variable in the system (in this case a defined one) and thus also a boolean expression.
`action` is a non-boolean expression that is compared across traces `A` and `B`.

For further examples, take a look at the `benchmarks/symbolic` and `benchmarks/planning` folder.



#### Convert To Single Module System 

The syntax supported by AutoHyper (and outlined above) does not include all NuSMV systems. 
Most importantly, we only support systems that consits of a **single module**.
Fortunately, the [NuSMV toolchain](https://nusmv.fbk.eu/) provides a function to flatten a system (called `flatten_hierarchy`), i.e., convert an arbitrary NuSMV model to one that consists of a single module. 
The result of this flatting does, in most cases, yield a specification that is supported by AutoHyper. 

To use this, install the [NuSMV toolchain](https://nusmv.fbk.eu/) (tested with version 2.6.0). 
For the easiest use, we recommend using the tool in *scripting* model.
Save the following script to a file (say `script.txt`)

```
read_model -i <input_path>;
flatten_hierarchy; 
write_flat_model -o <output_path>; 
quit;
```

where `<input_path>` is the path to the SMV model that should be flattened and `<output_path>` is the path the flattened model should be written to. 
When calling 

```shell
<...>/NuSMV -source script.txt
```
the script will be executed. 
Here `<...>/NuSMV` is the path to the NuSMV tool. 

##### Python Script

We provide a python script (`convertNuSMV.py`) that automates this process as much as possible. 
First, **modify** the `convertNuSMV.py` script by inserting the absolute path to the NuSMV tool.
Afterward, you can call 

```
python convertNuSMV.py <file_1> ... <file_n>
```

where `<file_1>`, ..., `<file_n>` are the NuSMV files that should be converted. 
The system will save the flattened systems in files `_<file_1>`, ..., `_<file_n>`, i.e., prepend a `_` to each file name.
You can then apply AutoHyper to the flattened model (starting with a `_`).


### Specifying Boolean Programs

When using option `--bp`, AutoHyper expects a system given as a boolean program.
A boolean program operates on variables that hold a vector of boolean values (with statically bounded length). 
A boolean program therefore always denotes a finite-state system. 

A boolean program has the form 
```
<header>
<statement>
```

where `<header>` and `<statement>` are defined below.

#### Header 

The `<header>` has the form 

```
<varName> : <bitwidth>;
<varName> : <bitwidth>;
...
<varName> : <bitwidth>;
```

and defines a bitwidth for each variable. 
A valid variable name (`<varName>`) is any non-empty sequence of letters. 
Each `<bitwidth>` is a natural number that defines the bitwidth of each variable. 

#### Expressions

Expressions (`<expression>`) have the form 
- `<varName>`: The value currently bound to a variable
- `t`: boolean true
- `f`: boolean false
- `<expression> & <expression>`: Pointwise conjunction. Assumes both arguments to evaluate to vectors of the same length.
- ` <expression> | <expression>`: Pointwise disjunction. Assumes both arguments to evaluate to vectors of the same length.
- `! <expression>`: Pointwise negation
- `<expression>[l, u]`: Evlautes `<expression>` and takes the bits ranging from position `l` (a natural number) to position `u` (also a natural number). You can write `<expression>[i]` as a shorthand for `<expression>[i, i]`.
- `(i * <expression>)`: Duplication where `i` is a natural number. First evaluates `<expression>` and the duplicates (concats) that vector `i` times. 

#### Statements

Statements (`<statement>`) have the form 
- `<varName> = <expression>;`: Assigns the value that `<expression>` evaluates to variable `<varName>`.
- `<varName> = *;`:  Assigns a non-deterministic value to `<varName>`.
- `if <expression> {<statement>} else {<statement>}`: Branches on whether or not `<expression>` evaluates to `[t]`.
- `<statement> <statement> ....; <statement>`: Execute all statements one after each other.
- `while <expression> {<statement>}`: Executes `<statement>` as long as `<expression>` evaluates to `[t]`.
- `if * {<statement>} else {<statement>}`: Nondeterministic branching.

Initially, all variables are assigned the constant `f` vector (of the length specified in the domain in the first line).
For details on the semantics see [2].

#### Example

Consider the following example Boolean program.

```
h : 1;
l : 1;
o : 1;

o = 1 * true;
while(true) {
    h = *;
    if (h[0]) {
        o = !o;
    } else {
        o = (!o) & (h | !h);
    }
}
```

#### HyperLTL in Boolean Programs

HyperLTL properties on boolean programs are specified by using atomic propositions of the form `{<varName>_j}_<VAR>` where `<varName>` is a variable in the program, `j` an index and `<VAR>` refers to a trace variable from the quantifier prefix.
The AP `{<varName>_j}_<VAR>` holds whenever the `j`th value in the vector currently assigned to `<varName>` holds on trace `<VAR>`.
For example `{h_0}_A` refers to the first position in the boolean vector value bound to `h` on the trace named `A`. 

An example property on the above system is the following

```
forall A. forall B. exists C. (G ({h_0}_A <-> {h_0}_C)) & (G({l_0}_B <-> {l_0}_C)) & (G({o_0}_B <-> {o_0}_C))
```
which always checks the first index in each boolean vector.

For further examples, take a look at the `benchmarks/bp` folder.


## References  

[1] AutoHyper: Explicit-State Model Checking for HyperLTL. Raven Beutner and Bernd Finkbeiner. TACAS 2023. 

[2] A Temporal Logic for Strategic Hyperproperties. Raven Beutner and Bernd Finkbeiner. CONCUR 2023.
