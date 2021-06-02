# PDDL-DEL
A translation module from the Multi-Agent Epistemic Planning Language (MAEPL) Symbolic Model Checker for Dynamic Epistemic Logic (SMCDEL) implementation by @m4lvin.

## Usage
To compile the project, run `stack build` in the project folder. For that, [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/ "install stack") needs to be installed. 

Then execute the program with either `stack exec PDDL-DEL-exe` or something to run it.
The program takes a few command line arguments, it can parse the domain and problem file separately if you parse them as arguments (in any order) like this: `-dom exampledomain.pddl -prb exampleproblem.pddl`. If neither `-dom` or `-prb` arguments are present, the program will take the first (non-flagged) argument as its input. 

It is possible to specify maximum search depth with `-d <Int>` argument, and require implicit coordination (such that all agents are aware before they take an action that it will indeed lead to the goal) with `-ic` flag. Due to temporary SMCDEL limitations the implicit coordination works with an iterative depth first search solution, while regular coordination works with breadth first search.

In case the semantic checker is giving errors where you think your file should indeed be in a correct format, you can untoggle the semantic checking with the `-nosemantics` flag. For debugging purposes, using the `-debug` flag will print out the actions that the agents consider (though also printing useless actions, which will not be taken).

## MAEPL

A PDDL version created by Thorsten Engesser. MAEPL is an extension of PDDL version 1.2 ADL subset (the :adl requirement is built in) with additions for epistemic planning. The MAEPL definition in this module slightly modified  compared to the original MAEPL. Examples of the syntax can be found in the example/ folder. 

### Additional features compared to PDDL

The formula definition is extended by the knowledge statement. The original MAEPL implementation only allows this in goal statement. The similar `common-knowledge` statement applies to all agents, and the difference between them is that common-knowledge is transitively close: i.e. each agent knows that any other agent knows that any other agent knows ... that formula in common-knowledge is true.

```ebnf
<Form> ::= (knows <agent> <Form>)
<Form> ::= (common-knowledge <Form>)
```
Where agent is just a regular type, defined by the user in the domain's `:types` statement.

```ebnf
<agent> ::= <name>
<agent> ::= <variable>
```

Observability statements were added to tell which agents are able to distinguish between which models. An agent can have either full, partial or no observability between models. The observability can have either a list of agents for whom it applies, or none. The first non-agent-specific observability statement in the problem file (for worlds, see last block) or in an action (for events) is the default observability. In case of no observability statements all agents have full observability. 

```ebnf
<obs> ::= :observability <obs-concrete> <agent>*
<obs> ::= :observability <obs-template> <agent>*
<obs-concrete> ::= (partition <part>)
<obs-template> ::= full
<obs-template> ::= none
<part> ::= <eqc>+
<eqc> ::= (<name>+)
```

Actions have been extended with an optional `:byagent` statement after parameters, to denote which agent takes the action. If not defined, the action is viewed as a global action.

The action can either have a list of events, with their own preconditions and effects. TODO write how it works

```ebnf
<action-def> ::= (:action <action functor>
:parameters ( <typed list (variable)> )
[:byagent <agent>]
<action-def epbody>
<obs>*)
<action-def epbody> ::= <action-def body>
<action-def epbody> ::= <event>+
<event> ::= (:event-nondesignated <name>
<action-def body>)
<event> ::= (:event-designated <name>
<action-def body>)
<action-def body> ::= [:precondition <GD>]
[:effect <effect>]
```

The problem file is similarly extended with worlds, which function similarly to events. Only one world can be designated.

```ebnf
<problem> ::= (define (problem <name>)
(:domain <name>)
[<object declaration>]
[<init>]
<world>*
(<obs>)*
<goal>

<init> ::= (:init <literal(name)>+)
<world> ::= (:world-nondesignated <name>
<literal(name)>+)
<world> ::= (:world-nondesignated <name>
<literal(name)>+)
```

## SMCDEL

The symbolic model checker increases efficiency and reduces complexity in planning performance in comparison to explicit model checkers.
A good SMCDEL description is in its [github repository](https://github.com/jrclogic/SMCDEL). 

### Structure

In general, the problem file is converted into a S5 Kripke model, and the actions are converted into S5 multi-pointed action models.

S5 means that if an agent knows something, the knowledge must be true. It also means that if the agent knows something, they know that they know it, and if they don't know something, they know that they don't know it. 

A Kripke model in this case is a model with a list of worlds, which can be connected to each other or not. If a world is connected to another one, it means that the two cannot be distinguished from one another. Each agent has their own representation of what worlds are connected to one another at each point, a good representation of such a partition is any partition of any agent in any example file, like [in the three muddy children example.](https://github.com/paulsilm/PDDLtoDEL/blob/842c6cb2415eef086cf38026d16cf11fb787cdb1/examples/muddy3.pddl#L179 "\"partition (cmm mmm) (cmc mmc) (mcm ccm) (mcc ccc)\"")

A multipointed action model means that it points to several events TODO what exactly does it do?

## Translation
While the domain with its actions is converted to the actionmodels and problem to a kripke model, the two cannot be separated since the actions in DEL actionmodels are object-specific, and there is no equality test in DEL (unlike in PDDL, like (= ?a ?b)), the value of the equalities needs to be determined before translation. 

The general idea is as follows: Create a Kripke model from the problem file and the worldlist, using object and constants. Then for each action generate all permutations of possible objects/constants based on the action parameters, and translate the events to a list of models (with tuples, first value precondition and second value effect), that is then used to construct the Labelled MultipointedActionModelS5. In this way, each PDDL action results in multiple DEL actions.

## Planning
The planning is entirely taken care of by [SMCDEL](https://github.com/jrclogic/SMCDEL).