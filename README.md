
---


# Scoring effort in  Argumentation Framework (AF)

## Advance preparation
Install haskell and make sure that you can use the `ghci` command on the console.
Also, install [graphvis](https://graphviz.org/) to use the `dot` command.

## Input Graph information
Write the information about the WBAF graph you want to create in the `Input_AF.hs` file. This file already contains the information for the graphs used in our  paper. You may rewrite this file.

## How to use
Type `ghci AF.hs` and run the desired file in an interactive environment. Here are some of the functions you can use.

- `show_graph_af sample_af` - Outputs a graph (simple AF) in the form of a dot file and a diagram of it as a pdf file.

- `show_graph_baf sample_baf` - Outputs a graph (BAF) as above.

- `show_graph_exbaf sample_exbaf` - Outputs a graph (exBAF) as above.

- `cal_af sample_af "a1"` - Calculate effort scores of the given AF, e.g. "PRO=(3,1) CON=(5,4).

- `cal_exbaf sample_exbaf "生活安定"` - Calculate effort scores of the given exBAF, e.g.  "PRO=(2,4) CON=(13,8)"