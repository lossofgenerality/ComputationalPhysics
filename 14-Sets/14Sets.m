BeginPackage["fourteenSets`"]; (* FIXME: does not like 14 *)

kuraTable::usage = "Kuratowski monoid cayley table";
\[Sigma]::usage = "symbol for topological operator";
makeVAR::usage = "make list of an indexed var";
kuraProd::usage="product table for kuwratowski";
makeL::usage = "generate the lie algebra L";
id::usage="id";
rand14SetsStoch::usage = "make random 14-sets vector";
MatrixRep14Sets::usage = "make matrix rep for a 14-Sets vector";
\[Sigma]Expr::usage = "make an operator experssion from coefficients";
\[Sigma]Matrix::usage = "make an operator experssion from matrix rep";
sumColumn::usage = "sum the colums";
sumRow::usage = "sum the rows";
vectorProduct::usage = "product of 14-Sets vectors"
graph14Sets::usage ="analysis tool"
Begin["`Private`"];

<<Notation`

kuraTable[]:=Module[{},
{{Subscript[\[Sigma], 0],Subscript[\[Sigma], 1],Subscript[\[Sigma], 2],Subscript[\[Sigma], 3],Subscript[\[Sigma], 4],Subscript[\[Sigma], 5],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13]},
{Subscript[\[Sigma], 1],Subscript[\[Sigma], 0],Subscript[\[Sigma], 4],Subscript[\[Sigma], 5],Subscript[\[Sigma], 2],Subscript[\[Sigma], 3],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11]},
{Subscript[\[Sigma], 2],Subscript[\[Sigma], 3],Subscript[\[Sigma], 2],Subscript[\[Sigma], 3],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7]},
{Subscript[\[Sigma], 3],Subscript[\[Sigma], 2],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 2],Subscript[\[Sigma], 3],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11]},
{Subscript[\[Sigma], 4],Subscript[\[Sigma], 5],Subscript[\[Sigma], 4],Subscript[\[Sigma], 5],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9]},
{Subscript[\[Sigma], 5],Subscript[\[Sigma], 4],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 4],Subscript[\[Sigma], 5],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13]},
{Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11]},
{Subscript[\[Sigma], 7],Subscript[\[Sigma], 6],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7]},
{Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13]},
{Subscript[\[Sigma], 9],Subscript[\[Sigma], 8],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9]},
{Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7]},
{Subscript[\[Sigma], 11],Subscript[\[Sigma], 10],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11]},
{Subscript[\[Sigma], 12],Subscript[\[Sigma], 13],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9]},
{Subscript[\[Sigma], 13],Subscript[\[Sigma], 12],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13]}}

]

(* Returns Subscript[\[Sigma], k]=Subscript[\[Sigma], i]Subscript[\[Sigma], j] for ith row and jth column *)
kuraProd[i0_, j0_]:=Module[{M,i=i0,j=j0},
M = kuraTable[];
M[[i+1]][[j+1]]
]

(* just returns the index number k for the Subscript[\[Sigma], k]=Subscript[\[Sigma], i]Subscript[\[Sigma], j] *)
kuraProd2[i0_, j0_]:=Module[{M,i=i0,j=j0, expr},
M = kuraTable[];
expr=M[[i+1]][[j+1]];
expr=InputForm[expr];
Extract[expr,{1,2}]
]

(* PERMUTATED TABLE: Returns Subscript[\[Sigma], k]=Subscript[\[Sigma], i]Subscript[\[Sigma], j] for ith row and jth column, see 1.1 *)
kuraProdAUTO[i0_, j0_]:=Module[{M,i=i0,j=j0},
M = kuraTable[];
M= M/.{Subscript[\[Sigma], 2]->Subscript[\[Sigma], 5], Subscript[\[Sigma], 3]->Subscript[\[Sigma], 4],Subscript[\[Sigma], 4]->Subscript[\[Sigma], 3],Subscript[\[Sigma], 5]->Subscript[\[Sigma], 2], Subscript[\[Sigma], 6]->Subscript[\[Sigma], 9], Subscript[\[Sigma], 7]->Subscript[\[Sigma], 8],Subscript[\[Sigma], 8]->Subscript[\[Sigma], 7],Subscript[\[Sigma], 9]->Subscript[\[Sigma], 6], Subscript[\[Sigma], 10]->Subscript[\[Sigma], 13],Subscript[\[Sigma], 11]->Subscript[\[Sigma], 12], Subscript[\[Sigma], 12]->Subscript[\[Sigma], 11],Subscript[\[Sigma], 13]->Subscript[\[Sigma], 10]};

M[[i+1]][[j+1]]
]

(* PERMUTATED TABLE: just returns the index number k for the Subscript[\[Sigma], k]=Subscript[\[Sigma], i]Subscript[\[Sigma], j] , see 1.1 *)
kuraProdAUTO2[i0_, j0_]:=Module[{M,i=i0,j=j0, expr},
M = kuraTable[];
M= M/.{Subscript[\[Sigma], 2]->Subscript[\[Sigma], 5], Subscript[\[Sigma], 3]->Subscript[\[Sigma], 4],Subscript[\[Sigma], 4]->Subscript[\[Sigma], 3],Subscript[\[Sigma], 5]->Subscript[\[Sigma], 2], Subscript[\[Sigma], 6]->Subscript[\[Sigma], 9], Subscript[\[Sigma], 7]->Subscript[\[Sigma], 8],Subscript[\[Sigma], 8]->Subscript[\[Sigma], 7],Subscript[\[Sigma], 9]->Subscript[\[Sigma], 6], Subscript[\[Sigma], 10]->Subscript[\[Sigma], 13],Subscript[\[Sigma], 11]->Subscript[\[Sigma], 12], Subscript[\[Sigma], 12]->Subscript[\[Sigma], 11],Subscript[\[Sigma], 13]->Subscript[\[Sigma], 10]};
expr=M[[i+1]][[j+1]];
expr=InputForm[expr];
Extract[expr,{1,2}]
]



Off[Symbolize::rowboxh]  (* this warnning gets issued and thus suppressed *)
(* Underscript[1, 14Sets] is the multiplicative 1 of the 14-Sets algebra, and computed to be Subscript[\[Sigma], 0] *)
Symbolize[ParsedBoxWrapper[RowBox[{"14", "Sets"}]]]
Notation[ParsedBoxWrapper[UnderscriptBox["1", RowBox[{"14", "Sets"}]]] \[DoubleLongRightArrow] ParsedBoxWrapper[SubscriptBox["\[Sigma]", "0"]]]
(* This assumes the a b product is commutative, so we assume at most Complex numbers for coefficients *)
(* This can be changed with some effort. We used ** to make sure the product is non-commutative *)

Notation[ParsedBoxWrapper[RowBox[{RowBox[{"(", RowBox[{"a_", " ", SubscriptBox["\[Sigma]", "i_"]}], ")"}], "**", RowBox[{"(", RowBox[{"b_", " ", SubscriptBox["\[Sigma]", "j_"]}], ")"}]}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"a_", " ", "b_", " ", RowBox[{"kuraProd", "[", RowBox[{"i_", ",", "j_"}], "]"}]}]]]

Notation[ParsedBoxWrapper[RowBox[{SubscriptBox["\[Sigma]", "i_"], "**", SubscriptBox["\[Sigma]", "j_"]}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"kuraProd", "[", RowBox[{"i_", ",", "j_"}], "]"}]]]
(* use this or the other, but not at the same time, since there is only one ** noncommutative operator in Mathematica *)
(*Notation[(a_ Subscript[\[Sigma], i_])**(b_ Subscript[\[Sigma], j_]) \[DoubleLongRightArrow] a_ b_ kuraProdAUTO[i_,j_]]*)

Notation[ParsedBoxWrapper[RowBox[{RowBox[{"(", RowBox[{"a_", " ", SubscriptBox["\[Sigma]", "i_"]}], ")"}], RowBox[{"(", RowBox[{"b_", " ", SubscriptBox["\[Sigma]", "j_"]}], ")"}]}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"a_", " ", "b_", " ", RowBox[{"kuraProd", "[", RowBox[{"i_", ",", "j_"}], "]"}]}]]]

(* short-hand for testing the table and simple experssions uses * *)
Notation[ParsedBoxWrapper[RowBox[{" ", RowBox[{SubscriptBox["\[Sigma]", "i_"], SubscriptBox["\[Sigma]", "j_"]}]}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"kuraProd", "[", RowBox[{"i_", ",", "j_"}], "]"}]]]

(* This is necessary or Mathematica does not know how to further evaluate the expression *)
Notation[ParsedBoxWrapper[SubsuperscriptBox["\[Sigma]", "i_", "2"]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"kuraProd", "[", RowBox[{"i_", ",", "i_"}], "]"}]]]
Notation[ParsedBoxWrapper[SuperscriptBox[RowBox[{"(", RowBox[{"a_", " ", SubscriptBox["\[Sigma]", "i_"]}], ")"}], "2"]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{RowBox[{"(", RowBox[{"a_", "^", "2"}], ")"}], "*", RowBox[{"kuraProd", "[", RowBox[{"i_", ",", "i_"}], "]"}]}]]]

(* Matrix representation of 14-Sets Algebra *)
MatrixRep14Sets[cfs0_]:=Module[{mat,a,b,c,d,e,f,g,h,i,j,k,l,m,n,cfs=cfs0},

mat=(a	b	0	0	0	0	0	0	0	0	0	0	0	0
b	a	0	0	0	0	0	0	0	0	0	0	0	0
c	d	a+c	0	b+d	0	0	0	0	0	0	0	0	0
d	c	0	a+c	0	b+d	0	0	0	0	0	0	0	0
e	f	b+e	0	a+f	0	0	0	0	0	0	0	0	0
f	e	0	b+e	0	a+f	0	0	0	0	0	0	0	0
g	h	d+g+l	0	c+h+k	0	a+c+h+k	0	b+d+g+l	0	d+g+l	0	c+h+k	0
h	g	0	d+g+l	0	c+h+k	0	a+c+h+k	0	b+d+g+l	0	d+g+l	0	c+h+k
i	j	f+i+n	0	e+j+m	0	b+e+j+m	0	a+f+i+n	0	f+i+n	0	e+j+m	0
j	i	0	f+i+n	0	e+j+m	0	b+e+j+m	0	a+f+i+n	0	f+i+n	0	e+j+m
k	l	h+k	0	g+l	0	d+g+l	0	c+h+k	0	a+c+h+k	0	b+d+g+l	0
l	k	0	h+k	0	g+l	0	d+g+l	0	c+h+k	0	a+c+h+k	0	b+d+g+l
m	n	j+m	0	i+n	0	f+i+n	0	e+j+m	0	b+e+j+m	0	a+f+i+n	0
n	m	0	j+m	0	i+n	0	f+i+n	0	e+j+m	0	b+e+j+m	0	a+f+i+n

);

mat/.{a->cfs[[1]], b->cfs[[2]],c->cfs[[3]],d->cfs[[4]],e->cfs[[5]],f->cfs[[6]],g->cfs[[7]],h->cfs[[8]],i->cfs[[9]],j->cfs[[10]],k->cfs[[11]],l->cfs[[12]],m->cfs[[13]],n->cfs[[14]]}

]

Notation[ParsedBoxWrapper[SuperscriptBox["E", "x_"]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"MatrixExp", "[", "x_", "]"}]]]
Notation[ParsedBoxWrapper[RowBox[{RowBox[{"<", "x_"}], "|"}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"id", "[", "x_", "]"}]]]
(* Column use is for asthetics only, this however is not working with matrix multiplicaiton so commented out *)
(*Notation[| x_ > \[DoubleLongRightArrow] Column[x_]]*)
Notation[ParsedBoxWrapper[RowBox[{RowBox[{"<", "x_"}], "|", "M_", "|", RowBox[{"y_", ">"}]}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"Simplify", "[", RowBox[{"x_", " ", ".", " ", "M_", " ", ".", " ", "y_"}], "]"}]]]
Notation[ParsedBoxWrapper[RowBox[{RowBox[{"<", "x_"}], "|", RowBox[{"y_", ">"}]}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"Simplify", "[", RowBox[{"x_", " ", ".", " ", "y_"}], "]"}]]]
Notation[ParsedBoxWrapper[RowBox[{RowBox[{"<", "x_"}], "|", "M_"}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"Simplify", "[", RowBox[{"x_", " ", ".", " ", "M_"}], " ", "]"}]]]
Notation[ParsedBoxWrapper[RowBox[{"M_", "|", RowBox[{"y_", ">"}]}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"Simplify", "[", " ", RowBox[{"M_", " ", ".", " ", "y_"}], "]"}]]]
Notation[ParsedBoxWrapper[RowBox[{"|", RowBox[{"x_", ">"}]}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"id", "[", "x_", "]"}]]]
Notation[ParsedBoxWrapper[UnderscriptBox["a_", "n_"]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{" ", RowBox[{"ConstantArray", "[", RowBox[{"a_", ",", "n_"}], "]"}]}]]]
Notation[ParsedBoxWrapper[RowBox[{SubsuperscriptBox["", "n_", "j_"], "a_"}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"makeVAR", "[", RowBox[{"a_", ",", "n_", ",", "j_"}], "]"}]]]

makeVAR[x_,n_,j_]:=Table[Subscript[x,i],{i,j,n-(1-j)}]

id[x_]:=x


Notation[ParsedBoxWrapper[UnderscriptBox[SuperscriptBox["L", RowBox[{"i_", ",", "j_"}]], "n_"]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"makeL", "[", RowBox[{"i_", ",", "j_", ",", "n_"}], "]"}]]]

makeL[i0_,j0_,n0_]:=Module[{i=i0,j=j0,n=n0,L},
Table[KroneckerDelta[i,k]*KroneckerDelta[j,l]-KroneckerDelta[j,k]*KroneckerDelta[j,l],{k,1,n},{l,1,n}]
]



(* \[CircleDot] product in 14Sets algebra *)
Notation[ParsedBoxWrapper[RowBox[{"x_", "\[CircleDot]", "y_"}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"vectorProduct", "[", RowBox[{"x_", ",", " ", "y_"}], "]"}]]];
Notation[ParsedBoxWrapper[RowBox[{RowBox[{"(", RowBox[{"a_", " ", "x_"}], ")"}], "\[CircleDot]", RowBox[{"(", RowBox[{"b_", " ", "y_"}], ")"}]}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"a_", "*", "b_", " ", RowBox[{"vectorProduct", "[", RowBox[{"x_", ",", " ", "y_"}], "]"}]}]]];
vectorProduct[x0_,y0_]:=Module[{x=x0,y=y0,cfsx,cfsy,a,b,c,d,e,f,g,h,i,j,k,l,m,n,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2},
cfsx=Coefficient[x,{Subscript[\[Sigma], 0],Subscript[\[Sigma], 1],Subscript[\[Sigma], 2],Subscript[\[Sigma], 3],Subscript[\[Sigma], 4],Subscript[\[Sigma], 5],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13]}];
cfsy=Coefficient[y,{Subscript[\[Sigma], 0],Subscript[\[Sigma], 1],Subscript[\[Sigma], 2],Subscript[\[Sigma], 3],Subscript[\[Sigma], 4],Subscript[\[Sigma], 5],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13]}];

a=cfsx[[1]];
b=cfsx[[2]];
c=cfsx[[3]];
d=cfsx[[4]];
e=cfsx[[5]];
f=cfsx[[6]];
g=cfsx[[7]];
h=cfsx[[8]];
i=cfsx[[9]];
j=cfsx[[10]];
k=cfsx[[11]];
l=cfsx[[12]];
m=cfsx[[13]];
n=cfsx[[14]];

a2=cfsy[[1]];
b2=cfsy[[2]];
c2=cfsy[[3]];
d2=cfsy[[4]];
e2=cfsy[[5]];
f2=cfsy[[6]];
g2=cfsy[[7]];
h2=cfsy[[8]];
i2=cfsy[[9]];
j2=cfsy[[10]];
k2=cfsy[[11]];
l2=cfsy[[12]];
m2=cfsy[[13]];
n2=cfsy[[14]];

Simplify[(a a2+b b2) Subscript[\[Sigma], 0]+(a2 b+a b2) Subscript[\[Sigma], 1]+(a2 c+a c2+c c2+b2 d+b e2+d e2) Subscript[\[Sigma], 2]+(b2 c+a2 d+a d2+c d2+b f2+d f2) Subscript[\[Sigma], 3]+(b c2+a2 e+c2 e+a e2+b2 f+e2 f) Subscript[\[Sigma], 4]+(b d2+b2 e+d2 e+a2 f+a f2+f f2) Subscript[\[Sigma], 5]+(c2 d+c e2+a2 g+c2 g+a g2+c g2+b2 h+e2 h+g2 h+b i2+d i2+g i2+e2 k+g2 k+d k2+g k2+c2 l+i2 l+k2 l+c m2+h m2+k m2) Subscript[\[Sigma], 6]+(d d2+c f2+b2 g+d2 g+a2 h+f2 h+a h2+c h2+h h2+b j2+d j2+g j2+f2 k+h2 k+d2 l+j2 l+d l2+g l2+l l2+c n2+h n2+k n2) Subscript[\[Sigma], 7]+(e e2+c2 f+b g2+e g2+a2 i+c2 i+a i2+f i2+i i2+b2 j+e2 j+g2 j+f k2+i k2+e2 m+g2 m+e m2+j m2+m m2+c2 n+i2 n+k2 n) Subscript[\[Sigma], 8]+(d2 f+e f2+b h2+e h2+b2 i+d2 i+a2 j+f2 j+h2 j+a j2+f j2+i j2+f l2+i l2+f2 m+h2 m+d2 n+j2 n+l2 n+e n2+j n2+m n2) Subscript[\[Sigma], 9]+(e2 g+d g2+g g2+c2 h+c i2+h i2+a2 k+c2 k+i2 k+a k2+c k2+h k2+k k2+b2 l+e2 l+g2 l+b m2+d m2+g m2+l m2) Subscript[\[Sigma], 10]+(f2 g+d2 h+d h2+g h2+c j2+h j2+b2 k+d2 k+j2 k+a2 l+f2 l+h2 l+a l2+c l2+h l2+k l2+b n2+d n2+g n2+l n2) Subscript[\[Sigma], 11]+(f g2+e2 i+g2 i+e i2+c2 j+i2 j+b k2+e k2+j k2+a2 m+c2 m+i2 m+k2 m+a m2+f m2+i m2+b2 n+e2 n+g2 n+m2 n) Subscript[\[Sigma], 12]+(f h2+f2 i+h2 i+d2 j+e j2+j j2+b l2+e l2+j l2+b2 m+d2 m+j2 m+l2 m+a2 n+f2 n+h2 n+a n2+f n2+i n2+n n2) Subscript[\[Sigma], 13]]
]

sumColumn[mat_]:= Table[Total[mat[[All,w]]],{w,1,14}]
sumRow[mat_]:= Table[Total[mat[[w,All]]],{w,1,14}]

(* make random 14-Sets Stochastic matrices with all colums adding to 1 *)
rand14SetsStoch[]:= Module[{rands,cfs},
rands =Join[Sort[ RandomReal[{0,1},13],Less], {1}];
cfs=Join[{rands[[1]]},Table[rands[[w]]-rands[[w-1]],{w, 2,14}]]
]

(* Self-Loops are removed *)
graph14Sets [cfs0_, scale0_]:=Module[{cfs = cfs0,matREP,matREP2, matREP3, matREP4,g,gPower, scale=scale0 },

matREP=MatrixRep14Sets[cfs];
matREP2 = Table[Boole[matREP[[u]][[w]]!=0], {u, 1, 14}, {w, 1, 14}];

g=GraphPlot[Transpose[matREP2],DirectedEdges->{True,"ArrowheadsSize"->0.01},MultiedgeStyle->2,SelfLoopStyle->False,VertexRenderingFunction->({EdgeForm[Black],Yellow,Disk[#1,scale*0.18],Black,Text[Subscript[\[Sigma], (#2-1)],#1]}&)];

matREP3=Chop[N[MatrixPower[matREP, 200]], 10^-9];
matREP4 = Table[Boole[matREP3[[u]][[w]]!=0], {u, 1, 14}, {w, 1, 14}];

gPower=GraphPlot[Transpose[matREP4],DirectedEdges->{True,"ArrowheadsSize"->0.01},MultiedgeStyle->1,SelfLoopStyle->False,VertexRenderingFunction->({EdgeForm[Black],Yellow,Disk[#1,scale*0.15/2],Black,Text[Subscript[\[Sigma], (#2-1)],#1]}&)];

(* matREP = matrix representation, matREP2 the boolean, matREP3 infinite power, matREP4 boolean of the infinite power,
g graph of the first stochastic matrix, gPower the graph of the infinite power stochastic matrix *)
{matREP,matREP2, matREP3, matREP4, g, gPower}
]



\[Sigma]Expr[cfs_]:= {Subscript[\[Sigma], 0],Subscript[\[Sigma], 1],Subscript[\[Sigma], 2],Subscript[\[Sigma], 3],Subscript[\[Sigma], 4],Subscript[\[Sigma], 5],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13]}.cfs
\[Sigma]Matrix[m_]:= {Subscript[\[Sigma], 0],Subscript[\[Sigma], 1],Subscript[\[Sigma], 2],Subscript[\[Sigma], 3],Subscript[\[Sigma], 4],Subscript[\[Sigma], 5],Subscript[\[Sigma], 6],Subscript[\[Sigma], 7],Subscript[\[Sigma], 8],Subscript[\[Sigma], 9],Subscript[\[Sigma], 10],Subscript[\[Sigma], 11],Subscript[\[Sigma], 12],Subscript[\[Sigma], 13]}.m[[All,1]]
Notation[ParsedBoxWrapper[RowBox[{"\[CapitalDelta]", RowBox[{"(", "c_", ")"}]}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"Distribute", "[", RowBox[{"c_", "\[CircleTimes]", "c_"}], "]"}]]]
Notation[ParsedBoxWrapper[RowBox[{"\[CapitalDelta]", " ", "c_"}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"Distribute", "[", RowBox[{"c_", "\[CircleTimes]", "c_"}], "]"}]]]
(* \[Diamond] is left as blank in most math books but we need to have an explicit bi-product *)
Notation[ParsedBoxWrapper[RowBox[{RowBox[{"(", RowBox[{"s_", " ", RowBox[{"a_", "\[CircleTimes]", "c_"}]}], ")"}], "\[Diamond]", RowBox[{"(", RowBox[{"t_", " ", RowBox[{"b_", "\[CircleTimes]", "d_"}]}], ")"}]}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{RowBox[{"(", RowBox[{"s_", "*", "t_"}], ")"}], RowBox[{RowBox[{"(", RowBox[{"a_", "\[CircleDot]", "b_"}], ")"}], "\[CircleTimes]", RowBox[{"(", RowBox[{"c_", "\[CircleDot]", "d_"}], ")"}]}]}]]]
Notation[ParsedBoxWrapper[RowBox[{RowBox[{"(", RowBox[{"a_", " ", "x_"}], ")"}], "\[CircleTimes]", RowBox[{"(", RowBox[{"b_", " ", "y_"}], ")"}]}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"a_", "*", "b_", RowBox[{"(", RowBox[{"x_", "\[CircleTimes]", "y_"}], ")"}]}]]]
(* Kludge, Mathematica is sensitive to the format of the expressions *)
Notation[ParsedBoxWrapper[RowBox[{" ", RowBox[{FractionBox["x_", "a_"], "\[CircleTimes]", FractionBox["y_", "b_"]}]}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{RowBox[{"(", RowBox[{"1", "/", RowBox[{"(", RowBox[{"a_", "*", "b_"}], ")"}]}], ")"}], "*", RowBox[{"(", RowBox[{"x_", "\[CircleTimes]", "y_"}], ")"}]}]]]
Notation[ParsedBoxWrapper[RowBox[{" ", RowBox[{FractionBox["x_", "a_"], "\[CircleTimes]", "y_"}]}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{RowBox[{"(", RowBox[{"1", "/", RowBox[{"(", "a_", ")"}]}], ")"}], "*", RowBox[{"(", RowBox[{"x_", "\[CircleTimes]", "y_"}], ")"}]}]]]
Notation[ParsedBoxWrapper[RowBox[{" ", RowBox[{"x_", "\[CircleTimes]", FractionBox["y_", "b_"]}]}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{RowBox[{"(", RowBox[{"1", "/", RowBox[{"(", "b_", ")"}]}], ")"}], "*", RowBox[{"(", RowBox[{"x_", "\[CircleTimes]", "y_"}], ")"}]}]]]
(*Kludge: Mathematica did not multiply two Reals *)
Notation[ParsedBoxWrapper[RowBox[{RowBox[{"(", RowBox[{"a_", " ", "a2_", " ", "x_"}], ")"}], "\[CircleTimes]", RowBox[{"(", RowBox[{"b_", " ", "b2_", " ", "y_"}], ")"}]}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"a_", "*", "b_", "*", "a2_", "*", "b2_", RowBox[{"(", RowBox[{"x_", "\[CircleTimes]", "y_"}], ")"}]}]]]

End[];
EndPackage[];
