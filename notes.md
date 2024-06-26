# Phase 1 extensions
## Definition

- extend definitions to accept arguments (ie. session passing)
  => difficult (ie. makes the structure of the program more complex, especially for )

## Recursion shortcut

- todo sometime (not necessary strictly speaking)

# DOT format memo:

```
digraph G {
center = TRUE;
mclimit = 10.0;
nodesep = 0.05;
node [ width=0.25, height=0.25, label="" ];
s0 [ peripheries=2 ];
s0 [label="(1,1)"];
s1 [label="(4,1)"];
s2 [label="(1,4)"];
s0->s1[label="o(s(0), r(1), r(0), m(1), p(0))"];
s0->s2[label="o(s(0), r(0), r(1), m(0), p(0))"];
}
```

## Parsing ideas:
- search only for `state <~ "->" ~> state <~ "[label=\"" ~> action <~ "\"]"`
where action is of type `i(s(int), r(int), r(int), m(int), pType)` or 
`o(s(int), r(int), r(int), m(int), pType)` (The quotes in the label might pose
problems, test for escaping them in parser model needed)

- phase 1 is parse context file to get translation table, 
phase 2 is parse DOT file and translate 
eg. `o(s(0), r(1), r(0), m(0), pType)` to `s[p][q](+)m2(...) ...`

- need for a separate parser type from the Types and Process one.

- Once the actions are determined, we can search for them in the Types or Process
specification, and point to them.

# Corresponding context.mcrl2 translation block + specification:

```
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Beginning of autogenerated specification %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Original global type:
% (none)

%%% Original typing context:
% s[p] -> q⊕{m1(end).q&{m2(end).end}}
% s[q] -> p⊕{m2(end).p&{m1(end).end}}

%%% Adapted typing context:
% s[p] -> q⊕{m1(end).q&{m2(end).end}}
% s[q] -> p⊕{m2(end).p&{m1(end).end}}

%%% Sessions
% s(0) => s

%%% Roles
% r(0) => q
% r(1) => p

%%% Message labels
% m(0) => m2
% m(1) => m1

%%% Payload types
% (none)

%%% Processes representing typing context entries
% E0 => (s[p],q⊕{m1(end).q&{m2(end).end}})
% E1 => (s[q],p⊕{m2(end).p&{m1(end).end}})

%%% Processes generated from multiparty session types recursion variables
% (none)

proc context = (E0) || (E1);

proc E0 = (o(s(0), r(1), r(0), m(1), pEnd) . (i(s(0), r(0), r(1), m(0), pEnd)));

proc E1 = (o(s(0), r(0), r(1), m(0), pEnd) . (i(s(0), r(1), r(0), m(1), pEnd)));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% End of autogenerated specification %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
```

## Parsing ideas:
- By block, searching for block header, ie `%%% Sessions`, `%%% Roles` and 
`%%% Message labels`. Then, parse the following lines with something like `commentSymbol ~> mcrl2Name <~ "=>" ~> mpstName`
- all at once (probably hard to delimit in that case)
- other ways ?

# Work outline for phase 4

## search for:
  state ~ (commsym ~> state) ~ ("[label=" ~> dotCom <~ "];")
## translate dotCOM:
  o(s(n), r(i), r(j), m(k), p(l)) => s(n)[r(i)] : r(j)(+)m(k) ...
  i(s(n), r(i), r(j), m(k), p(l)) => s(n)[r(j)] : r(i)&m(k) ...

NOTE: order on roles in input and output
NOTE2: We do not parse transitions t, as they only would be useful for 
       a false result in "never termination" (not reversing that atm because it's lots of work)

translatedItem = mCRL2_session | mCRL2_role | mCRL2_label
mCRL2_session: s([int])
mCRL2_role:    r([int])
mCRL2_label:   m([int])

item = session | role | label
all 3 are simple identifiers

search for:
"% " ~> translatedItem ~ ("=>" ~> item)

## Parser needs to:
 - translate s(n), r(i), r(j) and m(k) into their respective names 
   (translation table in the mcrl2 file is useful for that)
 - eventually, be able to find where that action happens.
 - do that for both types and process (ie. either go from types to proc, or
   go from DOT to proc using types translation table)

## Later limitations:
 - session passing -> going back to process for passed sessions is going to be 
   difficult in case several communications have the same labels
   s[p] != xB
   xB[q](ActionSymbol){Label(payload). Cont}