14-Sets Algebra: Code Supplement 1

Dara O Shayda 

June 26 2014

dara@lossofgenerality.com 


<< "/Users/darashayda1xfer/Desktop/ComputationalAlgebra/evalCDF/evalCDF.m" 

For programmer's references use:

http://reference.wolfram.com/mathematica/guide/Mathematica.html



Kuratowski's Monoid 


(* make a random word of length n *)

randWord[gen0_, n0_] := Module[{gen = gen0, n = n0, word = {}},
  
  Table[AppendTo[word, gen[[RandomInteger[{1, Length[gen]}]]]], {i, 1, n}];
  
  StringJoin[word]
  
  ]

(* reduce the word by sequentially going down the relations list *)
(* this \
function is generic no assumptions about the rules *)

reduceWord1[w_, rules_] := Module[{word = w, steps = 1000000},
  Table[word = FixedPoint[StringReplace[#, rules[[i]]] &, word, steps], {i, 
    1, Length[rules]}];
  
  word]

(* this rewrites the string by sandwiching the StringReplace by 
replacements that reduce to unit element, before and after *)

myStringReplace[str_, rule_, Erules_] := Module[{w = str},
  w = reduceWord1[w, Erules];
  w = StringReplace[w, rule];
  w = reduceWord1[w, Erules]
  
  ]


(* This is a more practical reducer for the words. It uses the FixedPoint
up to 1000000 trials to reduce the word to irreducible form *)

reduceWord[w_, rules_, Erules_] := Module[{word = w, steps = 1000000},
  Table[word = 
    FixedPoint[myStringReplace[#, rules[[i]], Erules] &, word, steps], {i, 1, 
    Length[rules]}];
  
  word]


(* shuffle relationships, for experimentation puproses *)

randShuffle[rels0_] := Module[{index, rels = rels0},
  index = RandomSample[Range[Length[rels]]];
  Table[rels[[index[[i]]]], {i, 1, Length[rels]}]
  ]


Let's see if empirically we could experiement with the Kuratowski's 14-Sets results and verify the theorem with actual compuations and trials. 

Setup the Kuratowski's Monoid:

generators = {"c", "h"};
rules = { "hh"->"h", "hchchch"->"hch"};
Erules={"cc"->""};

Let's then make a random word of length 500:

w=randWord[generators,500];

Then reduce the word:

reduceWord[w,rules,Erules]}]]  


generators = {"c", "h"};
rules = { "hh" -> "h", "hchchch" -> "hch"};
Erules = {"cc" -> ""};
expr = Hold[
   Column[{w = randWord[generators, 500], reduceWord[w, rules, Erules]}]];
eVAL[expr, 1]

Let's make 5000 random words of length 500 each, and reduce them using the relations above and see what we get and how many new reduced words:

Table[
w=randWord[generators, RandomInteger[{1,500}]];
ws=AppendTo[ws,w];
AppendTo[words, reduceWord[w, rules, Erules]], {i, 1, 5000}];

Short[ws,20]
words=DeleteDuplicates[words];
Length[words]   


expr = Hold[Column[{words = {}; ws = {};
     "5000 random words of length 500:\n",
     Table[
      w = randWord[generators, RandomInteger[{1, 500}]];
      ws = AppendTo[ws, w];
      AppendTo[words, reduceWord[w, rules, Erules]], {i, 1, 5000}];
     Short[ws, 20]
     ,
     "\nReduced Words & Count:\n",
     words = DeleteDuplicates[words],
     Length[words]}]];
eVAL[expr, 2]

As you can see fixed number of words not to exceed 14 are always the output of the applying the Kuratowski's Monoid's reduction rules! 

This incredible result known since 1920s, and yet not fully utilized, could be used to build a multiplcation table for the above resultant output coined as Kuratowski's Operations:

Table below obtained by Plewik and Walczynska [1] 


kuraTable[] := Module[{},
  {{Subscript[\[Sigma], 0], Subscript[\[Sigma], 1], Subscript[\[Sigma], 2], 
    Subscript[\[Sigma], 3], Subscript[\[Sigma], 4], Subscript[\[Sigma], 5], 
    Subscript[\[Sigma], 6], Subscript[\[Sigma], 7], Subscript[\[Sigma], 8], 
    Subscript[\[Sigma], 9], Subscript[\[Sigma], 10], Subscript[\[Sigma], 11], 
    Subscript[\[Sigma], 12], Subscript[\[Sigma], 13]},
   {Subscript[\[Sigma], 1], Subscript[\[Sigma], 0], Subscript[\[Sigma], 4], 
    Subscript[\[Sigma], 5], Subscript[\[Sigma], 2], Subscript[\[Sigma], 3], 
    Subscript[\[Sigma], 8], Subscript[\[Sigma], 9], Subscript[\[Sigma], 6], 
    Subscript[\[Sigma], 7], Subscript[\[Sigma], 12], Subscript[\[Sigma], 13], 
    Subscript[\[Sigma], 10], Subscript[\[Sigma], 11]},
   {Subscript[\[Sigma], 2], Subscript[\[Sigma], 3], Subscript[\[Sigma], 2], 
    Subscript[\[Sigma], 3], Subscript[\[Sigma], 6], Subscript[\[Sigma], 7], 
    Subscript[\[Sigma], 6], Subscript[\[Sigma], 7], Subscript[\[Sigma], 10], 
    Subscript[\[Sigma], 11], Subscript[\[Sigma], 10], Subscript[\[Sigma], 11],
     Subscript[\[Sigma], 6], Subscript[\[Sigma], 7]},
   {Subscript[\[Sigma], 3], Subscript[\[Sigma], 2], Subscript[\[Sigma], 6], 
    Subscript[\[Sigma], 7], Subscript[\[Sigma], 2], Subscript[\[Sigma], 3], 
    Subscript[\[Sigma], 10], Subscript[\[Sigma], 11], Subscript[\[Sigma], 6], 
    Subscript[\[Sigma], 7], Subscript[\[Sigma], 6], Subscript[\[Sigma], 7], 
    Subscript[\[Sigma], 10], Subscript[\[Sigma], 11]},
   {Subscript[\[Sigma], 4], Subscript[\[Sigma], 5], Subscript[\[Sigma], 4], 
    Subscript[\[Sigma], 5], Subscript[\[Sigma], 8], Subscript[\[Sigma], 9], 
    Subscript[\[Sigma], 8], Subscript[\[Sigma], 9], Subscript[\[Sigma], 12], 
    Subscript[\[Sigma], 13], Subscript[\[Sigma], 12], Subscript[\[Sigma], 13],
     Subscript[\[Sigma], 8], Subscript[\[Sigma], 9]},
   {Subscript[\[Sigma], 5], Subscript[\[Sigma], 4], Subscript[\[Sigma], 8], 
    Subscript[\[Sigma], 9], Subscript[\[Sigma], 4], Subscript[\[Sigma], 5], 
    Subscript[\[Sigma], 12], Subscript[\[Sigma], 13], Subscript[\[Sigma], 8], 
    Subscript[\[Sigma], 9], Subscript[\[Sigma], 8], Subscript[\[Sigma], 9], 
    Subscript[\[Sigma], 12], Subscript[\[Sigma], 13]},
   {Subscript[\[Sigma], 6], Subscript[\[Sigma], 7], Subscript[\[Sigma], 6], 
    Subscript[\[Sigma], 7], Subscript[\[Sigma], 10], Subscript[\[Sigma], 11], 
    Subscript[\[Sigma], 10], Subscript[\[Sigma], 11], Subscript[\[Sigma], 6], 
    Subscript[\[Sigma], 7], Subscript[\[Sigma], 6], Subscript[\[Sigma], 7], 
    Subscript[\[Sigma], 10], Subscript[\[Sigma], 11]},
   {Subscript[\[Sigma], 7], Subscript[\[Sigma], 6], Subscript[\[Sigma], 10], 
    Subscript[\[Sigma], 11], Subscript[\[Sigma], 6], Subscript[\[Sigma], 7], 
    Subscript[\[Sigma], 6], Subscript[\[Sigma], 7], Subscript[\[Sigma], 10], 
    Subscript[\[Sigma], 11], Subscript[\[Sigma], 10], Subscript[\[Sigma], 11],
     Subscript[\[Sigma], 6], Subscript[\[Sigma], 7]},
   {Subscript[\[Sigma], 8], Subscript[\[Sigma], 9], Subscript[\[Sigma], 8], 
    Subscript[\[Sigma], 9], Subscript[\[Sigma], 12], Subscript[\[Sigma], 13], 
    Subscript[\[Sigma], 12], Subscript[\[Sigma], 13], Subscript[\[Sigma], 8], 
    Subscript[\[Sigma], 9], Subscript[\[Sigma], 8], Subscript[\[Sigma], 9], 
    Subscript[\[Sigma], 12], Subscript[\[Sigma], 13]},
   {Subscript[\[Sigma], 9], Subscript[\[Sigma], 8], Subscript[\[Sigma], 12], 
    Subscript[\[Sigma], 13], Subscript[\[Sigma], 8], Subscript[\[Sigma], 9], 
    Subscript[\[Sigma], 8], Subscript[\[Sigma], 9], Subscript[\[Sigma], 12], 
    Subscript[\[Sigma], 13], Subscript[\[Sigma], 12], Subscript[\[Sigma], 13],
     Subscript[\[Sigma], 8], Subscript[\[Sigma], 9]},
   {Subscript[\[Sigma], 10], Subscript[\[Sigma], 11], Subscript[\[Sigma], 10],
     Subscript[\[Sigma], 11], Subscript[\[Sigma], 6], Subscript[\[Sigma], 7], 
    Subscript[\[Sigma], 6], Subscript[\[Sigma], 7], Subscript[\[Sigma], 10], 
    Subscript[\[Sigma], 11], Subscript[\[Sigma], 10], Subscript[\[Sigma], 11],
     Subscript[\[Sigma], 6], Subscript[\[Sigma], 7]},
   {Subscript[\[Sigma], 11], Subscript[\[Sigma], 10], Subscript[\[Sigma], 6], 
    Subscript[\[Sigma], 7], Subscript[\[Sigma], 10], Subscript[\[Sigma], 11], 
    Subscript[\[Sigma], 10], Subscript[\[Sigma], 11], Subscript[\[Sigma], 6], 
    Subscript[\[Sigma], 7], Subscript[\[Sigma], 6], Subscript[\[Sigma], 7], 
    Subscript[\[Sigma], 10], Subscript[\[Sigma], 11]},
   {Subscript[\[Sigma], 12], Subscript[\[Sigma], 13], Subscript[\[Sigma], 12],
     Subscript[\[Sigma], 13], Subscript[\[Sigma], 8], Subscript[\[Sigma], 9], 
    Subscript[\[Sigma], 8], Subscript[\[Sigma], 9], Subscript[\[Sigma], 12], 
    Subscript[\[Sigma], 13], Subscript[\[Sigma], 12], Subscript[\[Sigma], 13],
     Subscript[\[Sigma], 8], Subscript[\[Sigma], 9]},
   {Subscript[\[Sigma], 13], Subscript[\[Sigma], 12], Subscript[\[Sigma], 8], 
    Subscript[\[Sigma], 9], Subscript[\[Sigma], 12], Subscript[\[Sigma], 13], 
    Subscript[\[Sigma], 12], Subscript[\[Sigma], 13], Subscript[\[Sigma], 8], 
    Subscript[\[Sigma], 9], Subscript[\[Sigma], 8], Subscript[\[Sigma], 9], 
    Subscript[\[Sigma], 12], Subscript[\[Sigma], 13]}}
  
  ]

(* Returns Subscript[\[Sigma], k]=Subscript[\[Sigma], i]Subscript[\[Sigma], \
j] for ith row and jth column *)

kuraProd[i0_, j0_] := Module[{M, i = i0, j = j0},
  M = kuraTable[];
  M[[i + 1]][[j + 1]]
  ]

(* just returns the index number k for the Subscript[\[Sigma], k]=Subscript[\
\[Sigma], i]Subscript[\[Sigma], j] *)

kuraProd2[i0_, j0_] := Module[{M, i = i0, j = j0, expr},
  M = kuraTable[];
  expr = M[[i + 1]][[j + 1]];
  expr = InputForm[expr];
  Extract[expr, {1, 2}]
  ]

(* PERMUTATED TABLE: Returns Subscript[\[Sigma], k]=Subscript[\[Sigma], \
i]Subscript[\[Sigma], j] for ith row and jth column, see 1.1 *)

kuraProdAUTO[i0_, j0_] := Module[{M, i = i0, j = j0},
  M = kuraTable[];
  M = M /. {Subscript[\[Sigma], 2] -> Subscript[\[Sigma], 5], 
     Subscript[\[Sigma], 3] -> Subscript[\[Sigma], 4], 
     Subscript[\[Sigma], 4] -> Subscript[\[Sigma], 3], 
     Subscript[\[Sigma], 5] -> Subscript[\[Sigma], 2], 
     Subscript[\[Sigma], 6] -> Subscript[\[Sigma], 9], 
     Subscript[\[Sigma], 7] -> Subscript[\[Sigma], 8], 
     Subscript[\[Sigma], 8] -> Subscript[\[Sigma], 7], 
     Subscript[\[Sigma], 9] -> Subscript[\[Sigma], 6], 
     Subscript[\[Sigma], 10] -> Subscript[\[Sigma], 13], 
     Subscript[\[Sigma], 11] -> Subscript[\[Sigma], 12], 
     Subscript[\[Sigma], 12] -> Subscript[\[Sigma], 11], 
     Subscript[\[Sigma], 13] -> Subscript[\[Sigma], 10]};
  
  M[[i + 1]][[j + 1]]
  ]

(* PERMUTATED TABLE: just returns the index number k for the Subscript[\
\[Sigma], k]=Subscript[\[Sigma], i]Subscript[\[Sigma], j] , see 1.1 *)

kuraProdAUTO2[i0_, j0_] := Module[{M, i = i0, j = j0, expr},
  M = kuraTable[];
  M = M /. {Subscript[\[Sigma], 2] -> Subscript[\[Sigma], 5], 
     Subscript[\[Sigma], 3] -> Subscript[\[Sigma], 4], 
     Subscript[\[Sigma], 4] -> Subscript[\[Sigma], 3], 
     Subscript[\[Sigma], 5] -> Subscript[\[Sigma], 2], 
     Subscript[\[Sigma], 6] -> Subscript[\[Sigma], 9], 
     Subscript[\[Sigma], 7] -> Subscript[\[Sigma], 8], 
     Subscript[\[Sigma], 8] -> Subscript[\[Sigma], 7], 
     Subscript[\[Sigma], 9] -> Subscript[\[Sigma], 6], 
     Subscript[\[Sigma], 10] -> Subscript[\[Sigma], 13], 
     Subscript[\[Sigma], 11] -> Subscript[\[Sigma], 12], 
     Subscript[\[Sigma], 12] -> Subscript[\[Sigma], 11], 
     Subscript[\[Sigma], 13] -> Subscript[\[Sigma], 10]};
  expr = M[[i + 1]][[j + 1]];
  expr = InputForm[expr];
  Extract[expr, {1, 2}]
  ]



This code enables us to make our own specialized binary product, in operator form, the Kuratowski's Operators:


(* This assumes the a b product is commutative, so we assume at most Complex \
numbers for coefficients *)
(* This can be changed with some effort. We used ** \
to make sure the product is non-commutative *)
<< Notation`
Off[Symbolize::rowboxh]  (* this warnning gets issued and thus suppressed *)
\
Notation[ParsedBoxWrapper[
   RowBox[{RowBox[{"(", RowBox[{"a_", " ", SubscriptBox["\[Sigma]", "i_"]}], 
       ")"}], "**", 
     RowBox[{"(", RowBox[{"b_", " ", SubscriptBox["\[Sigma]", "j_"]}], 
       ")"}]}]] \[DoubleLongRightArrow] 
  ParsedBoxWrapper[
   RowBox[{"a_", " ", "b_", " ", 
     RowBox[{"kuraProd", "[", RowBox[{"i_", ",", "j_"}], "]"}]}]]]

Notation[ParsedBoxWrapper[
   RowBox[{SubscriptBox["\[Sigma]", "i_"], "**", 
     SubscriptBox["\[Sigma]", "j_"]}]] \[DoubleLongRightArrow] 
  ParsedBoxWrapper[
   RowBox[{"kuraProd", "[", RowBox[{"i_", ",", "j_"}], "]"}]]]
(* use this or the other, but not at the same time, since there is only one ** \
noncommutative operator in Mathematica *)
(*Notation[(a_ Subscript[\[Sigma], \
i_])**(b_ Subscript[\[Sigma], j_]) \[DoubleLongRightArrow] a_ b_ \
kuraProdAUTO[i_,j_]]*)

Notation[ParsedBoxWrapper[
   RowBox[{RowBox[{"(", RowBox[{"a_", " ", SubscriptBox["\[Sigma]", "i_"]}], 
       ")"}], RowBox[{"(", 
       RowBox[{"b_", " ", SubscriptBox["\[Sigma]", "j_"]}], 
       ")"}]}]] \[DoubleLongRightArrow] 
  ParsedBoxWrapper[
   RowBox[{"a_", " ", "b_", " ", 
     RowBox[{"kuraProd", "[", RowBox[{"i_", ",", "j_"}], "]"}]}]]]

(* short-hand for testing the table and simple experssions uses * *)
Notation[
 ParsedBoxWrapper[
   RowBox[{" ", 
     RowBox[{SubscriptBox["\[Sigma]", "i_"], 
       SubscriptBox["\[Sigma]", "j_"]}]}]] \[DoubleLongRightArrow] 
  ParsedBoxWrapper[RowBox[{"kuraProd", "[", RowBox[{"i_", ",", "j_"}], "]"}]]]

(* This is necessary or Mathematica does not know how to further evaluate the \
expression *)
Notation[
 ParsedBoxWrapper[
   SubsuperscriptBox["\[Sigma]", "i_", "2"]] \[DoubleLongRightArrow] 
  ParsedBoxWrapper[
   RowBox[{"kuraProd", "[", RowBox[{"i_", ",", "i_"}], "]"}]]]
Notation[ParsedBoxWrapper[
   SuperscriptBox[
    RowBox[{"(", RowBox[{"a_", " ", SubscriptBox["\[Sigma]", "i_"]}], ")"}], 
    "2"]] \[DoubleLongRightArrow] 
  ParsedBoxWrapper[
   RowBox[{RowBox[{"(", RowBox[{"a_", "^", "2"}], ")"}], "*", 
     RowBox[{"kuraProd", "[", RowBox[{"i_", ",", "i_"}], "]"}]}]]]


Multiplication table:


kuraTable[] // MatrixForm

Let's test simple random multiplications using the table above:

Subscript[\[Sigma], i]Subscript[\[Sigma], j]        0<=i,j<=13      


basis = {Subscript[\[Sigma], 0], Subscript[\[Sigma], 1], 
   Subscript[\[Sigma], 2], Subscript[\[Sigma], 3], Subscript[\[Sigma], 4], 
   Subscript[\[Sigma], 5], Subscript[\[Sigma], 6], Subscript[\[Sigma], 7], 
   Subscript[\[Sigma], 8], Subscript[\[Sigma], 9], Subscript[\[Sigma], 10], 
   Subscript[\[Sigma], 11], Subscript[\[Sigma], 12], Subscript[\[Sigma], 13]};

expr = Hold[Column[{r1 = RandomInteger[13];
     r2 = RandomInteger[13];
     ToString[Text[Subscript[\[Sigma], r1]], StandardForm] <> 
      ToString[Text[Subscript[\[Sigma], r2]], StandardForm] <> " = " <> 
      ToString[Text[Subscript[\[Sigma], r1] Subscript[\[Sigma], r2]], 
       StandardForm],
     }]];
eVAL[expr, 3]


Imagine  Subscript[\[Sigma], i] as vector basis for a vector space over a (commutative) Field e.g. \[DoubleStruckCapitalR] or \[DoubleStruckCapitalC]:

 v=\!\( 
\*UnderoverscriptBox[\(\(\ \)\(\[Sum]\)\), \(i = 0\), \(13\)] 
\*SubscriptBox[\(a\), \(i\)] 
\*SubscriptBox[\(\[Sigma]\), \(i\)]\)          (EQ1.1  same equation indexing as the original paper)

Multiply two such vectors:


Clear[a, b, c, d, e, f, g, h, i, j, k, l, m, n];

expr = Hold[
   Column[{"v1=a \!\(\*SubscriptBox[\(\[Sigma]\), \(0\)]\)+b \
\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\)+c \!\(\*SubscriptBox[\(\[Sigma]\), \
\(2\)]\)+d \!\(\*SubscriptBox[\(\[Sigma]\), \(3\)]\)+e \!\(\*SubscriptBox[\
\(\[Sigma]\), \(4\)]\)+f \!\(\*SubscriptBox[\(\[Sigma]\), \(5\)]\)+g \
\!\(\*SubscriptBox[\(\[Sigma]\), \(6\)]\)+h \!\(\*SubscriptBox[\(\[Sigma]\), \
\(7\)]\)+i \!\(\*SubscriptBox[\(\[Sigma]\), \(8\)]\)+j \!\(\*SubscriptBox[\
\(\[Sigma]\), \(9\)]\)+k \!\(\*SubscriptBox[\(\[Sigma]\), \(10\)]\)+l \
\!\(\*SubscriptBox[\(\[Sigma]\), \(11\)]\)+m \!\(\*SubscriptBox[\(\[Sigma]\), \
\(12\)]\)+n \!\(\*SubscriptBox[\(\[Sigma]\), \(13\)]\)",
     "v2=a2 \!\(\*SubscriptBox[\(\[Sigma]\), \(0\)]\)+b2 \!\(\*SubscriptBox[\
\(\[Sigma]\), \(1\)]\)+c2 \!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\)+d2 \
\!\(\*SubscriptBox[\(\[Sigma]\), \(3\)]\)+e2 \!\(\*SubscriptBox[\(\[Sigma]\), \
\(4\)]\)+f2 \!\(\*SubscriptBox[\(\[Sigma]\), \(5\)]\)+g2 \!\(\*SubscriptBox[\
\(\[Sigma]\), \(6\)]\)+h2 \!\(\*SubscriptBox[\(\[Sigma]\), \(7\)]\)+i2 \
\!\(\*SubscriptBox[\(\[Sigma]\), \(8\)]\)+j2 \!\(\*SubscriptBox[\(\[Sigma]\), \
\(9\)]\)+k2 \!\(\*SubscriptBox[\(\[Sigma]\), \(10\)]\)+l2 \!\(\*SubscriptBox[\
\(\[Sigma]\), \(11\)]\)+m2 \!\(\*SubscriptBox[\(\[Sigma]\), \(12\)]\)+n2 \
\!\(\*SubscriptBox[\(\[Sigma]\), \(13\)]\)",
     "\nDistribute\n",
     
     Distribute[v1 ** v2]}]];
eVAL[expr, 4]

Reduce and simplify the latter:


expr = Hold[
   Column[{"Reduce using the multiplicaiton table:\n", (a Subscript[\[Sigma], 
          0]) ** (a2 Subscript[\[Sigma], 0]) + (a Subscript[\[Sigma], 
          0]) ** (b2 Subscript[\[Sigma], 1]) + (a Subscript[\[Sigma], 
          0]) ** (c2 Subscript[\[Sigma], 2]) + (a Subscript[\[Sigma], 
          0]) ** (d2 Subscript[\[Sigma], 3]) + (a Subscript[\[Sigma], 
          0]) ** (e2 Subscript[\[Sigma], 4]) + (a Subscript[\[Sigma], 
          0]) ** (f2 Subscript[\[Sigma], 5]) + (a Subscript[\[Sigma], 
          0]) ** (g2 Subscript[\[Sigma], 6]) + (a Subscript[\[Sigma], 
          0]) ** (h2 Subscript[\[Sigma], 7]) + (a Subscript[\[Sigma], 
          0]) ** (i2 Subscript[\[Sigma], 8]) + (a Subscript[\[Sigma], 
          0]) ** (j2 Subscript[\[Sigma], 9]) + (a Subscript[\[Sigma], 
          0]) ** (k2 Subscript[\[Sigma], 10]) + (a Subscript[\[Sigma], 
          0]) ** (l2 Subscript[\[Sigma], 11]) + (a Subscript[\[Sigma], 
          0]) ** (m2 Subscript[\[Sigma], 12]) + (a Subscript[\[Sigma], 
          0]) ** (n2 Subscript[\[Sigma], 13]) + (b Subscript[\[Sigma], 
          1]) ** (a2 Subscript[\[Sigma], 0]) + (b Subscript[\[Sigma], 
          1]) ** (b2 Subscript[\[Sigma], 1]) + (b Subscript[\[Sigma], 
          1]) ** (c2 Subscript[\[Sigma], 2]) + (b Subscript[\[Sigma], 
          1]) ** (d2 Subscript[\[Sigma], 3]) + (b Subscript[\[Sigma], 
          1]) ** (e2 Subscript[\[Sigma], 4]) + (b Subscript[\[Sigma], 
          1]) ** (f2 Subscript[\[Sigma], 5]) + (b Subscript[\[Sigma], 
          1]) ** (g2 Subscript[\[Sigma], 6]) + (b Subscript[\[Sigma], 
          1]) ** (h2 Subscript[\[Sigma], 7]) + (b Subscript[\[Sigma], 
          1]) ** (i2 Subscript[\[Sigma], 8]) + (b Subscript[\[Sigma], 
          1]) ** (j2 Subscript[\[Sigma], 9]) + (b Subscript[\[Sigma], 
          1]) ** (k2 Subscript[\[Sigma], 10]) + (b Subscript[\[Sigma], 
          1]) ** (l2 Subscript[\[Sigma], 11]) + (b Subscript[\[Sigma], 
          1]) ** (m2 Subscript[\[Sigma], 12]) + (b Subscript[\[Sigma], 
          1]) ** (n2 Subscript[\[Sigma], 13]) + (c Subscript[\[Sigma], 
          2]) ** (a2 Subscript[\[Sigma], 0]) + (c Subscript[\[Sigma], 
          2]) ** (b2 Subscript[\[Sigma], 1]) + (c Subscript[\[Sigma], 
          2]) ** (c2 Subscript[\[Sigma], 2]) + (c Subscript[\[Sigma], 
          2]) ** (d2 Subscript[\[Sigma], 3]) + (c Subscript[\[Sigma], 
          2]) ** (e2 Subscript[\[Sigma], 4]) + (c Subscript[\[Sigma], 
          2]) ** (f2 Subscript[\[Sigma], 5]) + (c Subscript[\[Sigma], 
          2]) ** (g2 Subscript[\[Sigma], 6]) + (c Subscript[\[Sigma], 
          2]) ** (h2 Subscript[\[Sigma], 7]) + (c Subscript[\[Sigma], 
          2]) ** (i2 Subscript[\[Sigma], 8]) + (c Subscript[\[Sigma], 
          2]) ** (j2 Subscript[\[Sigma], 9]) + (c Subscript[\[Sigma], 
          2]) ** (k2 Subscript[\[Sigma], 10]) + (c Subscript[\[Sigma], 
          2]) ** (l2 Subscript[\[Sigma], 11]) + (c Subscript[\[Sigma], 
          2]) ** (m2 Subscript[\[Sigma], 12]) + (c Subscript[\[Sigma], 
          2]) ** (n2 Subscript[\[Sigma], 13]) + (d Subscript[\[Sigma], 
          3]) ** (a2 Subscript[\[Sigma], 0]) + (d Subscript[\[Sigma], 
          3]) ** (b2 Subscript[\[Sigma], 1]) + (d Subscript[\[Sigma], 
          3]) ** (c2 Subscript[\[Sigma], 2]) + (d Subscript[\[Sigma], 
          3]) ** (d2 Subscript[\[Sigma], 3]) + (d Subscript[\[Sigma], 
          3]) ** (e2 Subscript[\[Sigma], 4]) + (d Subscript[\[Sigma], 
          3]) ** (f2 Subscript[\[Sigma], 5]) + (d Subscript[\[Sigma], 
          3]) ** (g2 Subscript[\[Sigma], 6]) + (d Subscript[\[Sigma], 
          3]) ** (h2 Subscript[\[Sigma], 7]) + (d Subscript[\[Sigma], 
          3]) ** (i2 Subscript[\[Sigma], 8]) + (d Subscript[\[Sigma], 
          3]) ** (j2 Subscript[\[Sigma], 9]) + (d Subscript[\[Sigma], 
          3]) ** (k2 Subscript[\[Sigma], 10]) + (d Subscript[\[Sigma], 
          3]) ** (l2 Subscript[\[Sigma], 11]) + (d Subscript[\[Sigma], 
          3]) ** (m2 Subscript[\[Sigma], 12]) + (d Subscript[\[Sigma], 
          3]) ** (n2 Subscript[\[Sigma], 13]) + (e Subscript[\[Sigma], 
          4]) ** (a2 Subscript[\[Sigma], 0]) + (e Subscript[\[Sigma], 
          4]) ** (b2 Subscript[\[Sigma], 1]) + (e Subscript[\[Sigma], 
          4]) ** (c2 Subscript[\[Sigma], 2]) + (e Subscript[\[Sigma], 
          4]) ** (d2 Subscript[\[Sigma], 3]) + (e Subscript[\[Sigma], 
          4]) ** (e2 Subscript[\[Sigma], 4]) + (e Subscript[\[Sigma], 
          4]) ** (f2 Subscript[\[Sigma], 5]) + (e Subscript[\[Sigma], 
          4]) ** (g2 Subscript[\[Sigma], 6]) + (e Subscript[\[Sigma], 
          4]) ** (h2 Subscript[\[Sigma], 7]) + (e Subscript[\[Sigma], 
          4]) ** (i2 Subscript[\[Sigma], 8]) + (e Subscript[\[Sigma], 
          4]) ** (j2 Subscript[\[Sigma], 9]) + (e Subscript[\[Sigma], 
          4]) ** (k2 Subscript[\[Sigma], 10]) + (e Subscript[\[Sigma], 
          4]) ** (l2 Subscript[\[Sigma], 11]) + (e Subscript[\[Sigma], 
          4]) ** (m2 Subscript[\[Sigma], 12]) + (e Subscript[\[Sigma], 
          4]) ** (n2 Subscript[\[Sigma], 13]) + (f Subscript[\[Sigma], 
          5]) ** (a2 Subscript[\[Sigma], 0]) + (f Subscript[\[Sigma], 
          5]) ** (b2 Subscript[\[Sigma], 1]) + (f Subscript[\[Sigma], 
          5]) ** (c2 Subscript[\[Sigma], 2]) + (f Subscript[\[Sigma], 
          5]) ** (d2 Subscript[\[Sigma], 3]) + (f Subscript[\[Sigma], 
          5]) ** (e2 Subscript[\[Sigma], 4]) + (f Subscript[\[Sigma], 
          5]) ** (f2 Subscript[\[Sigma], 5]) + (f Subscript[\[Sigma], 
          5]) ** (g2 Subscript[\[Sigma], 6]) + (f Subscript[\[Sigma], 
          5]) ** (h2 Subscript[\[Sigma], 7]) + (f Subscript[\[Sigma], 
          5]) ** (i2 Subscript[\[Sigma], 8]) + (f Subscript[\[Sigma], 
          5]) ** (j2 Subscript[\[Sigma], 9]) + (f Subscript[\[Sigma], 
          5]) ** (k2 Subscript[\[Sigma], 10]) + (f Subscript[\[Sigma], 
          5]) ** (l2 Subscript[\[Sigma], 11]) + (f Subscript[\[Sigma], 
          5]) ** (m2 Subscript[\[Sigma], 12]) + (f Subscript[\[Sigma], 
          5]) ** (n2 Subscript[\[Sigma], 13]) + (g Subscript[\[Sigma], 
          6]) ** (a2 Subscript[\[Sigma], 0]) + (g Subscript[\[Sigma], 
          6]) ** (b2 Subscript[\[Sigma], 1]) + (g Subscript[\[Sigma], 
          6]) ** (c2 Subscript[\[Sigma], 2]) + (g Subscript[\[Sigma], 
          6]) ** (d2 Subscript[\[Sigma], 3]) + (g Subscript[\[Sigma], 
          6]) ** (e2 Subscript[\[Sigma], 4]) + (g Subscript[\[Sigma], 
          6]) ** (f2 Subscript[\[Sigma], 5]) + (g Subscript[\[Sigma], 
          6]) ** (g2 Subscript[\[Sigma], 6]) + (g Subscript[\[Sigma], 
          6]) ** (h2 Subscript[\[Sigma], 7]) + (g Subscript[\[Sigma], 
          6]) ** (i2 Subscript[\[Sigma], 8]) + (g Subscript[\[Sigma], 
          6]) ** (j2 Subscript[\[Sigma], 9]) + (g Subscript[\[Sigma], 
          6]) ** (k2 Subscript[\[Sigma], 10]) + (g Subscript[\[Sigma], 
          6]) ** (l2 Subscript[\[Sigma], 11]) + (g Subscript[\[Sigma], 
          6]) ** (m2 Subscript[\[Sigma], 12]) + (g Subscript[\[Sigma], 
          6]) ** (n2 Subscript[\[Sigma], 13]) + (h Subscript[\[Sigma], 
          7]) ** (a2 Subscript[\[Sigma], 0]) + (h Subscript[\[Sigma], 
          7]) ** (b2 Subscript[\[Sigma], 1]) + (h Subscript[\[Sigma], 
          7]) ** (c2 Subscript[\[Sigma], 2]) + (h Subscript[\[Sigma], 
          7]) ** (d2 Subscript[\[Sigma], 3]) + (h Subscript[\[Sigma], 
          7]) ** (e2 Subscript[\[Sigma], 4]) + (h Subscript[\[Sigma], 
          7]) ** (f2 Subscript[\[Sigma], 5]) + (h Subscript[\[Sigma], 
          7]) ** (g2 Subscript[\[Sigma], 6]) + (h Subscript[\[Sigma], 
          7]) ** (h2 Subscript[\[Sigma], 7]) + (h Subscript[\[Sigma], 
          7]) ** (i2 Subscript[\[Sigma], 8]) + (h Subscript[\[Sigma], 
          7]) ** (j2 Subscript[\[Sigma], 9]) + (h Subscript[\[Sigma], 
          7]) ** (k2 Subscript[\[Sigma], 10]) + (h Subscript[\[Sigma], 
          7]) ** (l2 Subscript[\[Sigma], 11]) + (h Subscript[\[Sigma], 
          7]) ** (m2 Subscript[\[Sigma], 12]) + (h Subscript[\[Sigma], 
          7]) ** (n2 Subscript[\[Sigma], 13]) + (i Subscript[\[Sigma], 
          8]) ** (a2 Subscript[\[Sigma], 0]) + (i Subscript[\[Sigma], 
          8]) ** (b2 Subscript[\[Sigma], 1]) + (i Subscript[\[Sigma], 
          8]) ** (c2 Subscript[\[Sigma], 2]) + (i Subscript[\[Sigma], 
          8]) ** (d2 Subscript[\[Sigma], 3]) + (i Subscript[\[Sigma], 
          8]) ** (e2 Subscript[\[Sigma], 4]) + (i Subscript[\[Sigma], 
          8]) ** (f2 Subscript[\[Sigma], 5]) + (i Subscript[\[Sigma], 
          8]) ** (g2 Subscript[\[Sigma], 6]) + (i Subscript[\[Sigma], 
          8]) ** (h2 Subscript[\[Sigma], 7]) + (i Subscript[\[Sigma], 
          8]) ** (i2 Subscript[\[Sigma], 8]) + (i Subscript[\[Sigma], 
          8]) ** (j2 Subscript[\[Sigma], 9]) + (i Subscript[\[Sigma], 
          8]) ** (k2 Subscript[\[Sigma], 10]) + (i Subscript[\[Sigma], 
          8]) ** (l2 Subscript[\[Sigma], 11]) + (i Subscript[\[Sigma], 
          8]) ** (m2 Subscript[\[Sigma], 12]) + (i Subscript[\[Sigma], 
          8]) ** (n2 Subscript[\[Sigma], 13]) + (j Subscript[\[Sigma], 
          9]) ** (a2 Subscript[\[Sigma], 0]) + (j Subscript[\[Sigma], 
          9]) ** (b2 Subscript[\[Sigma], 1]) + (j Subscript[\[Sigma], 
          9]) ** (c2 Subscript[\[Sigma], 2]) + (j Subscript[\[Sigma], 
          9]) ** (d2 Subscript[\[Sigma], 3]) + (j Subscript[\[Sigma], 
          9]) ** (e2 Subscript[\[Sigma], 4]) + (j Subscript[\[Sigma], 
          9]) ** (f2 Subscript[\[Sigma], 5]) + (j Subscript[\[Sigma], 
          9]) ** (g2 Subscript[\[Sigma], 6]) + (j Subscript[\[Sigma], 
          9]) ** (h2 Subscript[\[Sigma], 7]) + (j Subscript[\[Sigma], 
          9]) ** (i2 Subscript[\[Sigma], 8]) + (j Subscript[\[Sigma], 
          9]) ** (j2 Subscript[\[Sigma], 9]) + (j Subscript[\[Sigma], 
          9]) ** (k2 Subscript[\[Sigma], 10]) + (j Subscript[\[Sigma], 
          9]) ** (l2 Subscript[\[Sigma], 11]) + (j Subscript[\[Sigma], 
          9]) ** (m2 Subscript[\[Sigma], 12]) + (j Subscript[\[Sigma], 
          9]) ** (n2 Subscript[\[Sigma], 13]) + (k Subscript[\[Sigma], 
          10]) ** (a2 Subscript[\[Sigma], 0]) + (k Subscript[\[Sigma], 
          10]) ** (b2 Subscript[\[Sigma], 1]) + (k Subscript[\[Sigma], 
          10]) ** (c2 Subscript[\[Sigma], 2]) + (k Subscript[\[Sigma], 
          10]) ** (d2 Subscript[\[Sigma], 3]) + (k Subscript[\[Sigma], 
          10]) ** (e2 Subscript[\[Sigma], 4]) + (k Subscript[\[Sigma], 
          10]) ** (f2 Subscript[\[Sigma], 5]) + (k Subscript[\[Sigma], 
          10]) ** (g2 Subscript[\[Sigma], 6]) + (k Subscript[\[Sigma], 
          10]) ** (h2 Subscript[\[Sigma], 7]) + (k Subscript[\[Sigma], 
          10]) ** (i2 Subscript[\[Sigma], 8]) + (k Subscript[\[Sigma], 
          10]) ** (j2 Subscript[\[Sigma], 9]) + (k Subscript[\[Sigma], 
          10]) ** (k2 Subscript[\[Sigma], 10]) + (k Subscript[\[Sigma], 
          10]) ** (l2 Subscript[\[Sigma], 11]) + (k Subscript[\[Sigma], 
          10]) ** (m2 Subscript[\[Sigma], 12]) + (k Subscript[\[Sigma], 
          10]) ** (n2 Subscript[\[Sigma], 13]) + (l Subscript[\[Sigma], 
          11]) ** (a2 Subscript[\[Sigma], 0]) + (l Subscript[\[Sigma], 
          11]) ** (b2 Subscript[\[Sigma], 1]) + (l Subscript[\[Sigma], 
          11]) ** (c2 Subscript[\[Sigma], 2]) + (l Subscript[\[Sigma], 
          11]) ** (d2 Subscript[\[Sigma], 3]) + (l Subscript[\[Sigma], 
          11]) ** (e2 Subscript[\[Sigma], 4]) + (l Subscript[\[Sigma], 
          11]) ** (f2 Subscript[\[Sigma], 5]) + (l Subscript[\[Sigma], 
          11]) ** (g2 Subscript[\[Sigma], 6]) + (l Subscript[\[Sigma], 
          11]) ** (h2 Subscript[\[Sigma], 7]) + (l Subscript[\[Sigma], 
          11]) ** (i2 Subscript[\[Sigma], 8]) + (l Subscript[\[Sigma], 
          11]) ** (j2 Subscript[\[Sigma], 9]) + (l Subscript[\[Sigma], 
          11]) ** (k2 Subscript[\[Sigma], 10]) + (l Subscript[\[Sigma], 
          11]) ** (l2 Subscript[\[Sigma], 11]) + (l Subscript[\[Sigma], 
          11]) ** (m2 Subscript[\[Sigma], 12]) + (l Subscript[\[Sigma], 
          11]) ** (n2 Subscript[\[Sigma], 13]) + (m Subscript[\[Sigma], 
          12]) ** (a2 Subscript[\[Sigma], 0]) + (m Subscript[\[Sigma], 
          12]) ** (b2 Subscript[\[Sigma], 1]) + (m Subscript[\[Sigma], 
          12]) ** (c2 Subscript[\[Sigma], 2]) + (m Subscript[\[Sigma], 
          12]) ** (d2 Subscript[\[Sigma], 3]) + (m Subscript[\[Sigma], 
          12]) ** (e2 Subscript[\[Sigma], 4]) + (m Subscript[\[Sigma], 
          12]) ** (f2 Subscript[\[Sigma], 5]) + (m Subscript[\[Sigma], 
          12]) ** (g2 Subscript[\[Sigma], 6]) + (m Subscript[\[Sigma], 
          12]) ** (h2 Subscript[\[Sigma], 7]) + (m Subscript[\[Sigma], 
          12]) ** (i2 Subscript[\[Sigma], 8]) + (m Subscript[\[Sigma], 
          12]) ** (j2 Subscript[\[Sigma], 9]) + (m Subscript[\[Sigma], 
          12]) ** (k2 Subscript[\[Sigma], 10]) + (m Subscript[\[Sigma], 
          12]) ** (l2 Subscript[\[Sigma], 11]) + (m Subscript[\[Sigma], 
          12]) ** (m2 Subscript[\[Sigma], 12]) + (m Subscript[\[Sigma], 
          12]) ** (n2 Subscript[\[Sigma], 13]) + (n Subscript[\[Sigma], 
          13]) ** (a2 Subscript[\[Sigma], 0]) + (n Subscript[\[Sigma], 
          13]) ** (b2 Subscript[\[Sigma], 1]) + (n Subscript[\[Sigma], 
          13]) ** (c2 Subscript[\[Sigma], 2]) + (n Subscript[\[Sigma], 
          13]) ** (d2 Subscript[\[Sigma], 3]) + (n Subscript[\[Sigma], 
          13]) ** (e2 Subscript[\[Sigma], 4]) + (n Subscript[\[Sigma], 
          13]) ** (f2 Subscript[\[Sigma], 5]) + (n Subscript[\[Sigma], 
          13]) ** (g2 Subscript[\[Sigma], 6]) + (n Subscript[\[Sigma], 
          13]) ** (h2 Subscript[\[Sigma], 7]) + (n Subscript[\[Sigma], 
          13]) ** (i2 Subscript[\[Sigma], 8]) + (n Subscript[\[Sigma], 
          13]) ** (j2 Subscript[\[Sigma], 9]) + (n Subscript[\[Sigma], 
          13]) ** (k2 Subscript[\[Sigma], 10]) + (n Subscript[\[Sigma], 
          13]) ** (l2 Subscript[\[Sigma], 11]) + (n Subscript[\[Sigma], 
          13]) ** (m2 Subscript[\[Sigma], 12]) + (n Subscript[\[Sigma], 
          13]) ** (n2 Subscript[\[Sigma], 13])}]];
eVAL[expr, "factor scalar coefficients"]

Collect the coefficients:


expr = Hold[
   Column[{"(EQ6)", 
     FullSimplify[
      Collect[a a2 Subscript[\[Sigma], 0] + b b2 Subscript[\[Sigma], 0] + 
        a2 b Subscript[\[Sigma], 1] + a b2 Subscript[\[Sigma], 1] + 
        a2 c Subscript[\[Sigma], 2] + a c2 Subscript[\[Sigma], 2] + 
        c c2 Subscript[\[Sigma], 2] + b2 d Subscript[\[Sigma], 2] + 
        b e2 Subscript[\[Sigma], 2] + d e2 Subscript[\[Sigma], 2] + 
        b2 c Subscript[\[Sigma], 3] + a2 d Subscript[\[Sigma], 3] + 
        a d2 Subscript[\[Sigma], 3] + c d2 Subscript[\[Sigma], 3] + 
        b f2 Subscript[\[Sigma], 3] + d f2 Subscript[\[Sigma], 3] + 
        b c2 Subscript[\[Sigma], 4] + a2 e Subscript[\[Sigma], 4] + 
        c2 e Subscript[\[Sigma], 4] + a e2 Subscript[\[Sigma], 4] + 
        b2 f Subscript[\[Sigma], 4] + e2 f Subscript[\[Sigma], 4] + 
        b d2 Subscript[\[Sigma], 5] + b2 e Subscript[\[Sigma], 5] + 
        d2 e Subscript[\[Sigma], 5] + a2 f Subscript[\[Sigma], 5] + 
        a f2 Subscript[\[Sigma], 5] + f f2 Subscript[\[Sigma], 5] + 
        c2 d Subscript[\[Sigma], 6] + c e2 Subscript[\[Sigma], 6] + 
        a2 g Subscript[\[Sigma], 6] + c2 g Subscript[\[Sigma], 6] + 
        a g2 Subscript[\[Sigma], 6] + c g2 Subscript[\[Sigma], 6] + 
        b2 h Subscript[\[Sigma], 6] + e2 h Subscript[\[Sigma], 6] + 
        g2 h Subscript[\[Sigma], 6] + b i2 Subscript[\[Sigma], 6] + 
        d i2 Subscript[\[Sigma], 6] + g i2 Subscript[\[Sigma], 6] + 
        e2 k Subscript[\[Sigma], 6] + g2 k Subscript[\[Sigma], 6] + 
        d k2 Subscript[\[Sigma], 6] + g k2 Subscript[\[Sigma], 6] + 
        c2 l Subscript[\[Sigma], 6] + i2 l Subscript[\[Sigma], 6] + 
        k2 l Subscript[\[Sigma], 6] + c m2 Subscript[\[Sigma], 6] + 
        h m2 Subscript[\[Sigma], 6] + k m2 Subscript[\[Sigma], 6] + 
        d d2 Subscript[\[Sigma], 7] + c f2 Subscript[\[Sigma], 7] + 
        b2 g Subscript[\[Sigma], 7] + d2 g Subscript[\[Sigma], 7] + 
        a2 h Subscript[\[Sigma], 7] + f2 h Subscript[\[Sigma], 7] + 
        a h2 Subscript[\[Sigma], 7] + c h2 Subscript[\[Sigma], 7] + 
        h h2 Subscript[\[Sigma], 7] + b j2 Subscript[\[Sigma], 7] + 
        d j2 Subscript[\[Sigma], 7] + g j2 Subscript[\[Sigma], 7] + 
        f2 k Subscript[\[Sigma], 7] + h2 k Subscript[\[Sigma], 7] + 
        d2 l Subscript[\[Sigma], 7] + j2 l Subscript[\[Sigma], 7] + 
        d l2 Subscript[\[Sigma], 7] + g l2 Subscript[\[Sigma], 7] + 
        l l2 Subscript[\[Sigma], 7] + c n2 Subscript[\[Sigma], 7] + 
        h n2 Subscript[\[Sigma], 7] + k n2 Subscript[\[Sigma], 7] + 
        e e2 Subscript[\[Sigma], 8] + c2 f Subscript[\[Sigma], 8] + 
        b g2 Subscript[\[Sigma], 8] + e g2 Subscript[\[Sigma], 8] + 
        a2 i Subscript[\[Sigma], 8] + c2 i Subscript[\[Sigma], 8] + 
        a i2 Subscript[\[Sigma], 8] + f i2 Subscript[\[Sigma], 8] + 
        i i2 Subscript[\[Sigma], 8] + b2 j Subscript[\[Sigma], 8] + 
        e2 j Subscript[\[Sigma], 8] + g2 j Subscript[\[Sigma], 8] + 
        f k2 Subscript[\[Sigma], 8] + i k2 Subscript[\[Sigma], 8] + 
        e2 m Subscript[\[Sigma], 8] + g2 m Subscript[\[Sigma], 8] + 
        e m2 Subscript[\[Sigma], 8] + j m2 Subscript[\[Sigma], 8] + 
        m m2 Subscript[\[Sigma], 8] + c2 n Subscript[\[Sigma], 8] + 
        i2 n Subscript[\[Sigma], 8] + k2 n Subscript[\[Sigma], 8] + 
        d2 f Subscript[\[Sigma], 9] + e f2 Subscript[\[Sigma], 9] + 
        b h2 Subscript[\[Sigma], 9] + e h2 Subscript[\[Sigma], 9] + 
        b2 i Subscript[\[Sigma], 9] + d2 i Subscript[\[Sigma], 9] + 
        a2 j Subscript[\[Sigma], 9] + f2 j Subscript[\[Sigma], 9] + 
        h2 j Subscript[\[Sigma], 9] + a j2 Subscript[\[Sigma], 9] + 
        f j2 Subscript[\[Sigma], 9] + i j2 Subscript[\[Sigma], 9] + 
        f l2 Subscript[\[Sigma], 9] + i l2 Subscript[\[Sigma], 9] + 
        f2 m Subscript[\[Sigma], 9] + h2 m Subscript[\[Sigma], 9] + 
        d2 n Subscript[\[Sigma], 9] + j2 n Subscript[\[Sigma], 9] + 
        l2 n Subscript[\[Sigma], 9] + e n2 Subscript[\[Sigma], 9] + 
        j n2 Subscript[\[Sigma], 9] + m n2 Subscript[\[Sigma], 9] + 
        e2 g Subscript[\[Sigma], 10] + d g2 Subscript[\[Sigma], 10] + 
        g g2 Subscript[\[Sigma], 10] + c2 h Subscript[\[Sigma], 10] + 
        c i2 Subscript[\[Sigma], 10] + h i2 Subscript[\[Sigma], 10] + 
        a2 k Subscript[\[Sigma], 10] + c2 k Subscript[\[Sigma], 10] + 
        i2 k Subscript[\[Sigma], 10] + a k2 Subscript[\[Sigma], 10] + 
        c k2 Subscript[\[Sigma], 10] + h k2 Subscript[\[Sigma], 10] + 
        k k2 Subscript[\[Sigma], 10] + b2 l Subscript[\[Sigma], 10] + 
        e2 l Subscript[\[Sigma], 10] + g2 l Subscript[\[Sigma], 10] + 
        b m2 Subscript[\[Sigma], 10] + d m2 Subscript[\[Sigma], 10] + 
        g m2 Subscript[\[Sigma], 10] + l m2 Subscript[\[Sigma], 10] + 
        f2 g Subscript[\[Sigma], 11] + d2 h Subscript[\[Sigma], 11] + 
        d h2 Subscript[\[Sigma], 11] + g h2 Subscript[\[Sigma], 11] + 
        c j2 Subscript[\[Sigma], 11] + h j2 Subscript[\[Sigma], 11] + 
        b2 k Subscript[\[Sigma], 11] + d2 k Subscript[\[Sigma], 11] + 
        j2 k Subscript[\[Sigma], 11] + a2 l Subscript[\[Sigma], 11] + 
        f2 l Subscript[\[Sigma], 11] + h2 l Subscript[\[Sigma], 11] + 
        a l2 Subscript[\[Sigma], 11] + c l2 Subscript[\[Sigma], 11] + 
        h l2 Subscript[\[Sigma], 11] + k l2 Subscript[\[Sigma], 11] + 
        b n2 Subscript[\[Sigma], 11] + d n2 Subscript[\[Sigma], 11] + 
        g n2 Subscript[\[Sigma], 11] + l n2 Subscript[\[Sigma], 11] + 
        f g2 Subscript[\[Sigma], 12] + e2 i Subscript[\[Sigma], 12] + 
        g2 i Subscript[\[Sigma], 12] + e i2 Subscript[\[Sigma], 12] + 
        c2 j Subscript[\[Sigma], 12] + i2 j Subscript[\[Sigma], 12] + 
        b k2 Subscript[\[Sigma], 12] + e k2 Subscript[\[Sigma], 12] + 
        j k2 Subscript[\[Sigma], 12] + a2 m Subscript[\[Sigma], 12] + 
        c2 m Subscript[\[Sigma], 12] + i2 m Subscript[\[Sigma], 12] + 
        k2 m Subscript[\[Sigma], 12] + a m2 Subscript[\[Sigma], 12] + 
        f m2 Subscript[\[Sigma], 12] + i m2 Subscript[\[Sigma], 12] + 
        b2 n Subscript[\[Sigma], 12] + e2 n Subscript[\[Sigma], 12] + 
        g2 n Subscript[\[Sigma], 12] + m2 n Subscript[\[Sigma], 12] + 
        f h2 Subscript[\[Sigma], 13] + f2 i Subscript[\[Sigma], 13] + 
        h2 i Subscript[\[Sigma], 13] + d2 j Subscript[\[Sigma], 13] + 
        e j2 Subscript[\[Sigma], 13] + j j2 Subscript[\[Sigma], 13] + 
        b l2 Subscript[\[Sigma], 13] + e l2 Subscript[\[Sigma], 13] + 
        j l2 Subscript[\[Sigma], 13] + b2 m Subscript[\[Sigma], 13] + 
        d2 m Subscript[\[Sigma], 13] + j2 m Subscript[\[Sigma], 13] + 
        l2 m Subscript[\[Sigma], 13] + a2 n Subscript[\[Sigma], 13] + 
        f2 n Subscript[\[Sigma], 13] + h2 n Subscript[\[Sigma], 13] + 
        a n2 Subscript[\[Sigma], 13] + f n2 Subscript[\[Sigma], 13] + 
        i n2 Subscript[\[Sigma], 13] + 
        n n2 Subscript[\[Sigma], 13], {Subscript[\[Sigma], 0], 
        Subscript[\[Sigma], 1], Subscript[\[Sigma], 2], 
        Subscript[\[Sigma], 3], Subscript[\[Sigma], 4], 
        Subscript[\[Sigma], 5], Subscript[\[Sigma], 6], 
        Subscript[\[Sigma], 7], Subscript[\[Sigma], 8], 
        Subscript[\[Sigma], 9], Subscript[\[Sigma], 10], 
        Subscript[\[Sigma], 11], Subscript[\[Sigma], 12], 
        Subscript[\[Sigma], 13]}]]}]];
eVAL[expr, "Simplify"]

Solve for a matrix representation (EQ 1.2):


expr = Hold[
   coeff = CoefficientArrays[{(a a2 + b b2) == 0 , (a2 b + a b2) == 
       0, (a2 c + a c2 + c c2 + b2 d + b e2 + d e2) == 
       0, (b2 c + a2 d + a d2 + c d2 + b f2 + d f2) == 
       0, (b c2 + a2 e + c2 e + a e2 + b2 f + e2 f) == 
       0 , (b d2 + b2 e + d2 e + a2 f + a f2 + f f2) == 
       0 , (c2 d + c e2 + a2 g + c2 g + a g2 + c g2 + b2 h + e2 h + g2 h + 
         b i2 + d i2 + g i2 + e2 k + g2 k + d k2 + g k2 + c2 l + i2 l + k2 l +
          c m2 + h m2 + k m2) == 
       0, (d d2 + c f2 + b2 g + d2 g + a2 h + f2 h + a h2 + c h2 + h h2 + 
         b j2 + d j2 + g j2 + f2 k + h2 k + d2 l + j2 l + d l2 + g l2 + l l2 +
          c n2 + h n2 + k n2) == 
       0, (e e2 + c2 f + b g2 + e g2 + a2 i + c2 i + a i2 + f i2 + i i2 + 
         b2 j + e2 j + g2 j + f k2 + i k2 + e2 m + g2 m + e m2 + j m2 + m m2 +
          c2 n + i2 n + k2 n) == 
       0, (d2 f + e f2 + b h2 + e h2 + b2 i + d2 i + a2 j + f2 j + h2 j + 
         a j2 + f j2 + i j2 + f l2 + i l2 + f2 m + h2 m + d2 n + j2 n + l2 n +
          e n2 + j n2 + m n2) == 
       0 , (e2 g + d g2 + g g2 + c2 h + c i2 + h i2 + a2 k + c2 k + i2 k + 
         a k2 + c k2 + h k2 + k k2 + b2 l + e2 l + g2 l + b m2 + d m2 + g m2 +
          l m2) == 
       0, (f2 g + d2 h + d h2 + g h2 + c j2 + h j2 + b2 k + d2 k + j2 k + 
         a2 l + f2 l + h2 l + a l2 + c l2 + h l2 + k l2 + b n2 + d n2 + g n2 +
          l n2) == 
       0 , (f g2 + e2 i + g2 i + e i2 + c2 j + i2 j + b k2 + e k2 + j k2 + 
         a2 m + c2 m + i2 m + k2 m + a m2 + f m2 + i m2 + b2 n + e2 n + g2 n +
          m2 n) == 
       0, (f h2 + f2 i + h2 i + d2 j + e j2 + j j2 + b l2 + e l2 + j l2 + 
         b2 m + d2 m + j2 m + l2 m + a2 n + f2 n + h2 n + a n2 + f n2 + i n2 +
          n n2) == 0}, {a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, 
      n2}];
   
   coeff[[2]] // MatrixForm];
eVAL[expr, "Solve for parameters a2, b2 ... in v2"]


Let's make a funcation that makes such matrix representation out of the coefficients:


(* Matrix representation of 14-Sets Algebra *)

MatrixRep14Sets[cfs0_] := 
 Module[{mat, a, b, c, d, e, f, g, h, i, j, k, l, m, n, cfs = cfs0},
  
  mat = \!\(\*
TagBox[
RowBox[{"(", GridBox[{
{"a", "b", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"},
{"b", "a", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"},
{"c", "d", 
RowBox[{"a", "+", "c"}], "0", 
RowBox[{"b", "+", "d"}], "0", "0", "0", "0", "0", "0", "0", "0", "0"},
{"d", "c", "0", 
RowBox[{"a", "+", "c"}], "0", 
RowBox[{"b", "+", "d"}], "0", "0", "0", "0", "0", "0", "0", "0"},
{"e", "f", 
RowBox[{"b", "+", "e"}], "0", 
RowBox[{"a", "+", "f"}], "0", "0", "0", "0", "0", "0", "0", "0", "0"},
{"f", "e", "0", 
RowBox[{"b", "+", "e"}], "0", 
RowBox[{"a", "+", "f"}], "0", "0", "0", "0", "0", "0", "0", "0"},
{"g", "h", 
RowBox[{"d", "+", "g", "+", "l"}], "0", 
RowBox[{"c", "+", "h", "+", "k"}], "0", 
RowBox[{"a", "+", "c", "+", "h", "+", "k"}], "0", 
RowBox[{"b", "+", "d", "+", "g", "+", "l"}], "0", 
RowBox[{"d", "+", "g", "+", "l"}], "0", 
RowBox[{"c", "+", "h", "+", "k"}], "0"},
{"h", "g", "0", 
RowBox[{"d", "+", "g", "+", "l"}], "0", 
RowBox[{"c", "+", "h", "+", "k"}], "0", 
RowBox[{"a", "+", "c", "+", "h", "+", "k"}], "0", 
RowBox[{"b", "+", "d", "+", "g", "+", "l"}], "0", 
RowBox[{"d", "+", "g", "+", "l"}], "0", 
RowBox[{"c", "+", "h", "+", "k"}]},
{"i", "j", 
RowBox[{"f", "+", "i", "+", "n"}], "0", 
RowBox[{"e", "+", "j", "+", "m"}], "0", 
RowBox[{"b", "+", "e", "+", "j", "+", "m"}], "0", 
RowBox[{"a", "+", "f", "+", "i", "+", "n"}], "0", 
RowBox[{"f", "+", "i", "+", "n"}], "0", 
RowBox[{"e", "+", "j", "+", "m"}], "0"},
{"j", "i", "0", 
RowBox[{"f", "+", "i", "+", "n"}], "0", 
RowBox[{"e", "+", "j", "+", "m"}], "0", 
RowBox[{"b", "+", "e", "+", "j", "+", "m"}], "0", 
RowBox[{"a", "+", "f", "+", "i", "+", "n"}], "0", 
RowBox[{"f", "+", "i", "+", "n"}], "0", 
RowBox[{"e", "+", "j", "+", "m"}]},
{"k", "l", 
RowBox[{"h", "+", "k"}], "0", 
RowBox[{"g", "+", "l"}], "0", 
RowBox[{"d", "+", "g", "+", "l"}], "0", 
RowBox[{"c", "+", "h", "+", "k"}], "0", 
RowBox[{"a", "+", "c", "+", "h", "+", "k"}], "0", 
RowBox[{"b", "+", "d", "+", "g", "+", "l"}], "0"},
{"l", "k", "0", 
RowBox[{"h", "+", "k"}], "0", 
RowBox[{"g", "+", "l"}], "0", 
RowBox[{"d", "+", "g", "+", "l"}], "0", 
RowBox[{"c", "+", "h", "+", "k"}], "0", 
RowBox[{"a", "+", "c", "+", "h", "+", "k"}], "0", 
RowBox[{"b", "+", "d", "+", "g", "+", "l"}]},
{"m", "n", 
RowBox[{"j", "+", "m"}], "0", 
RowBox[{"i", "+", "n"}], "0", 
RowBox[{"f", "+", "i", "+", "n"}], "0", 
RowBox[{"e", "+", "j", "+", "m"}], "0", 
RowBox[{"b", "+", "e", "+", "j", "+", "m"}], "0", 
RowBox[{"a", "+", "f", "+", "i", "+", "n"}], "0"},
{"n", "m", "0", 
RowBox[{"j", "+", "m"}], "0", 
RowBox[{"i", "+", "n"}], "0", 
RowBox[{"f", "+", "i", "+", "n"}], "0", 
RowBox[{"e", "+", "j", "+", "m"}], "0", 
RowBox[{"b", "+", "e", "+", "j", "+", "m"}], "0", 
RowBox[{"a", "+", "f", "+", "i", "+", "n"}]}
},
GridBoxAlignment->{
         "Columns" -> {{Center}}, "ColumnsIndexed" -> {}, 
          "Rows" -> {{Baseline}}, "RowsIndexed" -> {}, "Items" -> {}, 
          "ItemsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {
Offset[0.27999999999999997`], {
Offset[0.7]}, 
Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
Offset[0.2], {
Offset[0.4]}, 
Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}], 
       ")"}],
Function[BoxForm`e$, 
MatrixForm[BoxForm`e$]]]\);
  
  mat /. {a -> cfs[[1]], b -> cfs[[2]], c -> cfs[[3]], d -> cfs[[4]], 
    e -> cfs[[5]], f -> cfs[[6]], g -> cfs[[7]], h -> cfs[[8]], i -> cfs[[9]],
     j -> cfs[[10]], k -> cfs[[11]], l -> cfs[[12]], m -> cfs[[13]], 
    n -> cfs[[14]]}
  
  ]

Let's test the results by actual matrix multiplications:

MatrixRep14Sets[{a,b,c,d,e,f,g,h,i,j,k,l,m,n}].{a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2}.{Subscript[\[Sigma], 0],Subscript[\[Sigma], 1],Subscript[\[Sigma], 2],Subscript[\[Sigma], 3],Subscript[\[Sigma], 4],Subscript[\[Sigma], 5],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13]}]

expr = Hold[
   FullSimplify[
    MatrixRep14Sets[{a, b, c, d, e, f, g, h, i, j, k, l, m, n}].{a2, b2, c2, 
      d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2}.{Subscript[\[Sigma], 0], 
      Subscript[\[Sigma], 1], Subscript[\[Sigma], 2], Subscript[\[Sigma], 3], 
      Subscript[\[Sigma], 4], Subscript[\[Sigma], 5], Subscript[\[Sigma], 6], 
      Subscript[\[Sigma], 7], Subscript[\[Sigma], 8], Subscript[\[Sigma], 9], 
      Subscript[\[Sigma], 10], Subscript[\[Sigma], 11], 
      Subscript[\[Sigma], 12], Subscript[\[Sigma], 13]}]];
eVAL[expr, "Matrix Multiply"]

As you can see same as (EQ6) earlier.


Closure Properties

Double checking that the above matrix representation is closed under matrix multiplicaiton, addition and scalar multiplication (Corollary 1.1):



mat1=MatrixRep14Sets[{a,b,c,d,e,f,g,h,i,j,k,l,m,n}];
mat2=MatrixRep14Sets[{a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2}];
Expand[mat1.mat2] === Expand[MatrixRep14Sets[(mat1.mat2)[[All,1]]]]
Expand[mat1+mat2] === Expand[MatrixRep14Sets[(mat1+mat2)[[All,1]]]]
Expand[s*mat1] === Expand[MatrixRep14Sets[(s*mat1)[[All,1]]]]



expr = Hold[Column[{
     Style[
      MatrixRep14Sets[{a, b, c, d, e, f, g, h, i, j, k, l, m, n}] // 
       MatrixForm, FontSize -> 7],
     Style[
      MatrixRep14Sets[{a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, 
         n2}] // MatrixForm, FontSize -> 7],
     "\nExpand[mat1.mat2] === Expand[MatrixRep14Sets[(mat1.mat2)[[All,1]]]]",
     Expand[mat1.mat2] === Expand[MatrixRep14Sets[(mat1.mat2)[[All, 1]]]],
     "\nExpand[mat1+mat2] === \
Expand[MatrixRep14Sets[(mat1+mat2)[[All,1]]]]]",
     Expand[mat1 + mat2] === 
      Expand[MatrixRep14Sets[(mat1 + mat2)[[All, 1]]]],
     "\nExpand[s*mat1] === Expand[MatrixRep14Sets[(s*mat1)[[All,1]]]]",
     Expand[s*mat1] === Expand[MatrixRep14Sets[(s*mat1)[[All, 1]]]]
     }]
   ];
eVAL[expr, "Closure Properties"]

 2. Stochastic Matrix 


Add rows and columns:


expr = Hold[Column[{
     MatrixRep14Sets[{a, b, c, d, e, f, g, h, i, j, k, l, m, n}] // MatrixForm,
     "\nTotal columns:\n",
     Column[Total[rep]],
     "\nTotal rows:\n",
     Column[Total[Transpose[rep]]]}]
   ];
eVAL[expr, "Sum rows and columns"]

The columns add to a constant, and assume further that this constant to be 1:

a+b+c+d+e+f+g+h+i+j+k+l+m+n = 1      (EQ2 .1)

Therefore by direct computation it is proven:

Lemma 1: 14-Sets matrix representation constained by (EQ2.1) is a Left Stochastic Matrix.



This function generates a random 14-Sets Left Stochast Matrix:


rand14SetsStochastic[] := Module[{rands},
  rands = Join[Sort[ RandomReal[{0, 1}, 13], Less], {1}];
  MatrixRep14Sets[
   Join[{rands[[1]]}, Table[rands[[w]] - rands[[w - 1]], {w, 2, 14}]]]
  
  ]

Sample:


expr = Hold[Style[rand14SetsStochastic[] // MatrixForm, FontSize -> 7]];
eVAL[expr, "Sample 14-Sets Stochast Matrix"]

Powers of 14-Sets Stochastic Matrices 

Consecutive powers of 1, 2, 4, 8, and 16 of a random 14-Sets Stochastic Matrix (see Theorem 2.2):


expr = Hold[Column[{
     r = rand14SetsStochastic[];
     Style[rand14SetsStochastic[] // MatrixForm, FontSize -> 7],
     Style[Chop[MatrixPower[r, 2]] // MatrixForm, FontSize -> 7],
     Style[Chop[MatrixPower[r, 4]] // MatrixForm, FontSize -> 7],
     Style[Chop[MatrixPower[r, 8], 10^-6] // MatrixForm, FontSize -> 7],
     Style[Chop[MatrixPower[r, 16], 10^-6] // MatrixForm, FontSize -> 7]}]
   
   ];

eVAL[expr, "Powers of 14-Sets Stochastis Matrix"]

References 



[1] Plewik and Walczynska, "THE MONOID CONSISTING OF KURATOWSKI OPERATIONS". http://arxiv.org/abs/1205.3391v2 

[2] J. E. Johnson, "Markov-type Lie Gorups in GL (n, \[DoubleStruckCapitalR])". http://netmetrics.asg.sc.edu/markov-type_lie_groups_GL.pdf  

[3] Alfred North Whitehead, "Process of Reality". http://files.lossofgenerality.com/Proc_reality.pdf  

[4] M. Shah and H. Albuquerque, "QUASIALGEBRA STRUCTURE OF THE OCTONIONS". http://arxiv.org/abs/math/9802116v1 

[5] L. A. Steen, J. A. Seebach Jr, "COUNTEREXAMPLES IN TOPOLOGY". Dover



