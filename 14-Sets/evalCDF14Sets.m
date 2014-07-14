(* license:

http://www.perlfoundation.org/artistic_license_2_0 

Copyrights Dara O Shayda 2014-present 
*) 
 
BeginPackage["evalCDF14Sets`"];


eVAL::usage="evaluates the Mathematica experssions in CDF";
eVAL2::usage="evaluates the Mathematica experssions in string form in CDF";
eVAL3::usage="evaluates the Mathematica experssions in CDF, the button title is formatter before argument passing";


Begin["`Private`"];




(* Matrix representation of 14-Sets Algebra *)
lib="MatrixRep14Sets[cfs0_]:=Module[{mat,a,b,c,d,e,f,g,h,i,j,k,l,m,n,cfs=cfs0},

mat={{a, b, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {b, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {c, d, a + c, 0, b + d, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {d, c, 0, a + c, 0, b + d, 0, 0, 0, 0, 0, 0, 0, 0}, {e, f, b + e, 0, a + f, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {f, e, 0, b + e, 0, a + f, 0, 0, 0, 0, 0, 0, 0, 0}, {g, h, d + g + l, 0, c + h + k, 0, a + c + h + k, 0, b + d + g + l, 0, d + g + l, 0, c + h + k, 0}, {h, g, 0, d + g + l, 0, c + h + k, 0, a + c + h + k, 0, b + d + g + l, 0, d + g + l, 0, c + h + k}, {i, j, f + i + n, 0, e + j + m, 0, b + e + j + m, 0, a + f + i + n, 0, f + i + n, 0, e + j + m, 0}, {j, i, 0, f + i + n, 0, e + j + m, 0, b + e + j + m, 0, a + f + i + n, 0, f + i + n, 0, e + j + m}, {k, l, h + k, 0, g + l, 0, d + g + l, 0, c + h + k, 0, a + c + h + k, 0, b + d + g + l, 0}, {l, k, 0, h + k, 0, g + l, 0, d + g + l, 0, c + h + k, 0, a + c + h + k, 0, b + d + g + l}, {m, n, j + m, 0, i + n, 0, f + i + n, 0, e + j + m, 0, b + e + j + m, 0, a + f + i + n, 0}, {n, m, 0, j + m, 0, i + n, 0, f + i + n, 0, e + j + m, 0, b + e + j + m, 0, a + f + i + n}};

mat/.{a->cfs[[1]], b->cfs[[2]],c->cfs[[3]],d->cfs[[4]],e->cfs[[5]],f->cfs[[6]],g->cfs[[7]],h->cfs[[8]],i->cfs[[9]],j->cfs[[10]],k->cfs[[11]],l->cfs[[12]],m->cfs[[13]],n->cfs[[14]]}

] \n";  (* leave one \n space at the end *)

eVAL[expr_,n_]:=Module[{text,t},Manipulate[Deploy[Panel[text]],{ {c,0,""},Button["Eval: "<>ToString[n],t=ReleaseHold[expr];
text=Dynamic[t]]&}, SaveDefinitions->True,AppearanceElements->None,Initialization:>(text="")]]

eVAL2[expr_,n_]:=Module[{text,t},Manipulate[Deploy[Panel[text]],{ {c,0,""},Button["Eval: "<>ToString[n],t=ToExpression[lib<>expr];
text=Dynamic[t]]&}, SaveDefinitions->True,AppearanceElements->None,Initialization:>(text="")]]

eVAL3[expr_,n_]:=Module[{text,t},Manipulate[Deploy[Panel[text]],{ {c,0,""},Button["Eval: "<>n,t=ReleaseHold[expr];
text=Dynamic[t]]&}, SaveDefinitions->True,AppearanceElements->None,Initialization:>(text="")]]

End[];
EndPackage[];
