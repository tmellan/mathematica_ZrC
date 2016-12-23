(* ::Package:: *)

(* ::Input:: *)
(*(*e0perf2={-.67369861*10^3,-.67467091*10^3,-.67528513*10^3,-.67557487*10^3,-.67539478*10^3,-.67436291*10^3,-.67314839*10^3,-.67068552*10^3,-.66688497*10^3}*)*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*m={12.0107,91.224,1};*)
(**)
(*kB=1000*8.617332478*10^\[Minus]5;K2meV=1000*8.617332478*10^\[Minus]5;kjmol2evatom=1/(96.487*64);*)
(*e0perf={*)
(*-.67550095*10^3,-.67441442*10^3,-.67323280*10^3,-.67090290*10^3,-.66733277*10^3};*)
(*volperf={766.060,778.688,791.453,808.70835,822.656,846.590,862.257,885.289,912.673};*)
(*e0perf={-.67382051*10^3,-.67479111*10^3,-.67540188*10^3,-.67568572*10^3,-.67550095*10^3,-.67441442*10^3,-.67323280*10^3,-.67090290*10^3,-.66733277*10^3};*)
(*(*Data in*)*)
(*SetDirectory["/Volumes/MicroSD/Dropbox/PostDoc_SD/ZrC/Einstein_oscillator_4685Ang_ZrC/allvols"];*)
(*filesC={"C_4575","C_4600","C_4625","C_46583","C_4685_760","C_4730_1900","C_4759_2500","C_4801_3200","C_4850_3805","C_4575_110","C_4600_110","C_4625_110","C_46583_110","C_4685_760_110","C_4730_1900_110","C_4759_2500_110","C_4801_3200_110","C_4850_3805_110","C_4575_111","C_4600_111","C_4625_111","C_46583_111","C_4685_760_111","C_4730_1900_111","C_4759_2500_111","C_4801_3200_111","C_4850_3805_111"};*)
(*filesZr={"Zr_4575","Zr_4600","Zr_4625","Zr_46583","Zr_4685_760","Zr_4730_1900","Zr_4759_2500","Zr_4801_3200","Zr_4850_3805","Zr_4575_110","Zr_4600_110","Zr_4625_110","Zr_46583_110","Zr_4685_760_110","Zr_4730_1900_110","Zr_4759_2500_110","Zr_4801_3200_110","Zr_4850_3805_110","Zr_4575_111","Zr_4600_111","Zr_4625_111","Zr_46583_111","Zr_4685_760_111","Zr_4730_1900_111","Zr_4759_2500_111","zr_4801_3200_111","Zr_4850_3805_111"};*)
(*filesCh={"1.harm.C_4575","1.harm.C_4600","1.harm.C_4625","1.harm.C_46583","1.harm.C_4685_760","1.harm.C_4730_1900","1.harm.C_4759_2500","1.harm.C_4801_3200","1.harm.C_4850_3805","1.harm.C_4575_110","1.harm.C_4600_110","1.harm.C_4625_110","1.harm.C_46583_110","1.harm.C_4685_760_110","1.harm.C_4730_1900_110","1.harm.C_4759_2500_110","1.harm.C_4801_3200_110","1.harm.C_4850_3805_110","1.harm.C_4575_111","1.harm.C_4600_111","1.harm.C_4625_111","1.harm.C_46583_111","1.harm.C_4685_760_111","1.harm.C_4730_1900_111","1.harm.C_4759_2500_111","1.harm.C_4801_3200_111","1.harm.C_4850_3805_111"};*)
(*filesZrh={"1.harm.Zr_4575","1.harm.Zr_4600","1.harm.Zr_4625","1.harm.Zr_46583","1.harm.Zr_4685_760","1.harm.Zr_4730_1900","1.harm.Zr_4759_2500","1.harm.Zr_4801_3200","1.harm.Zr_4850_3805","1.harm.Zr_4575_110","1.harm.Zr_4600_110","1.harm.Zr_4625_110","1.harm.Zr_46583_110","1.harm.Zr_4685_760_110","1.harm.Zr_4730_1900_110","1.harm.Zr_4759_2500_110","1.harm.Zr_4801_3200_110","1.harm.Zr_4850_3805_110","1.harm.Zr_4575_111","1.harm.Zr_4600_111","1.harm.Zr_4625_111","1.harm.Zr_46583_111","1.harm.Zr_4685_760_111","1.harm.Zr_4730_1900_111","1.harm.Zr_4759_2500_111","1.harm.Zr_4801_3200_111","1.harm.Zr_4850_3805_111"};*)
(*Cdata=Table[ReadList[filesC[[i]],{Number, Number}],{i,1,27}];*)
(*Zrdata=Table[ReadList[filesZr[[i]],{Number, Number}],{i,1,27}];*)
(*hCdata=Table[ReadList[filesCh[[i]],{Number, Number}],{i,1,27}];*)
(*hZrdata=Table[ReadList[filesZrh[[i]],{Number, Number}],{i,1,27}];*)
(*Show[ListPlot@hCdata,ListPlot@hZrdata,PlotRange->All]*)
(*Show[ListPlot@Cdata,ListPlot@Zrdata,PlotRange->All]*)


(* ::Input:: *)
(*ListPlot[{Cdata[[1]],Cdata[[10]],Cdata[[19]],hCdata}]*)


(* ::Input:: *)
(*Plot[Evaluate@{V[1,6,x],V[1,2,x][[1]],V[2,2,x][[1]]},{x,-1,1},PlotLegends->SwatchLegend[{1,2,3,harC,harZ}]]*)


(* ::Input:: *)
(*Define and fit potentials;*)
(*(*Carbon*)*)
(*V[1,2,x_]:=Table[Fit[hCdata[[i]],{x^2},x],{i,1,27}]*)
(*V[1,4,x_]:=Table[Fit[Cdata[[i]],{x^2,x^4},x],{i,1,27,9}]*)
(*V[1,6,x_]:=Table[Fit[Cdata[[i]],{x^2,x^4,x^6},x],{i,1,27}]*)
(*V[1,8,x_]:=Table[Fit[Cdata[[i]],{x^2,x^4,x^6,x^8},x],{i,1,27,9}]*)
(*V[1,10,x_]:=Table[Fit[Cdata[[i]],{x^2,x^4,x^6,x^8,x^10},x],{i,1,27,9}]*)
(*V[1,12,x_]:=Table[Fit[Cdata[[i]],{x^2,x^4,x^6,x^8,x^10,x^12},x],{i,1,27}]*)
(**)
(*V[2,2,x_]:=Table[Fit[hZrdata[[i]],{x^2},x],{i,1,27}]*)
(*V[2,4,x_]:=Table[Fit[Zrdata[[i]],{x^2,x^4},x],{i,1,27,9}]*)
(*V[2,6,x_]:=Table[Fit[Zrdata[[i]],{x^2,x^4,x^6},x],{i,1,27}]*)
(*V[2,8,x_]:=Table[Fit[Zrdata[[i]],{x^2,x^4,x^6,x^8},x],{i,1,27,9}]*)
(*V[2,10,x_]:=Table[Fit[Zrdata[[i]],{x^2,x^4,x^6,x^8,x^10},x],{i,1,27,9}]*)
(*V[2,12,x_]:=Table[Fit[Zrdata[[i]],{x^2,x^4,x^6,x^8,x^10,x^12},x],{i,1,27}]*)
(*GraphicsGrid[{{Show[Plot[Evaluate@{V[1,2,x][[1]],V[1,12,x][[1]],V[1,12,x][[10]],V[1,12,x][[11]]},{x,-1.2,1.2},PlotLegends->SwatchLegend[{"harmonic","100 anharmonic","110 anharmonic","111 anharmonic"}],PlotLabel->"Carbon potentials at 4.685Angstrom",AxesLabel->{"Angstrom","meV/atom"}],ImageSize->500],Show[Plot[Evaluate@{V[2,2,x][[1]],V[2,12,x][[1]],V[2,12,x][[10]],V[2,12,x][[11]]},{x,-1.2,1.2},PlotLegends->SwatchLegend[{"harmonic","100 anharmonic","110 anharmonic","111 anharmonic"}],PlotLabel->"Zirconium potentials at 4.685Angstrom",AxesLabel->{"Angstrom","meV/atom"}],ImageSize->500]}},ImageSize->1300]*)


(* ::Input:: *)
(*V[1,2,x][[5]]*)


(* ::Input:: *)
(*ang2bohr=1/0.529177208;*)
(*ev2har=1/27.211396;*)
(*eVperAng2harperbohr=ev2har/ang2bohr;*)
(*eVperAng22harperbohr2=ev2har/ang2bohr/ang2bohr;*)
(*HarBohr=meVperAng22harperbohr2=0.001*ev2har/ang2bohr/ang2bohr;*)
(*cHess=0.5Sqrt[2Table[V[1,2,x][[i]][[1]],{i,1,9}]/m[[1]]]*)
(*zHess=0.5Sqrt[2Table[V[2,2,x][[i]][[1]],{i,1,9}]/m[[2]]]*)
(*cHess*HarBohr*)
(*zHess*HarBohr*)
(*HarBohr*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*"Carbon harmonic polynomial fit prefactor 'A' in meV/Ang**2 is"*)
(*Acarbon=Table[V[1,2,x][[i]][[1]],{i,1,1}]*)
(*"cOmega0 in meV/Ang**2/AMU is approximately"*)
(*cOmega0=Sqrt[2Acarbon/m[[1]]]*)
(*"Zr harmonic polynomial fit prefactor 'A' in meV/Ang**2 is"*)
(*Azirconium=Table[V[2,2,x][[i]][[1]],{i,1,1}]*)
(*"Z_Omega_0 in meV/Ang**2/AMU is approximately"*)
(*zOmega0=Sqrt[2Azirconium/m[[2]]] *)


(* ::Input:: *)
(*Carbon and zirconium - calculate eigenvalues and vectors;*)
(*(*Find the first x eigenvalues and eigenvectors*)*)
(*Neigenvalues=280;*)
(*(*Define bounds for plotting and solving eigenproblem*)*)
(*B=1.1;H=2000;*)
(*power={2,6,12};*)
(*(*Carbon Hamiltonians - from harmonic, even symmetry to 12th order*)*)
(*\[ScriptCapitalL]allc=Table[-(1/(2m[[1]]))*u''[x]+V[1,i,x]*u[x]/.i->power[[j]],{j,1,3,1}];*)
(*(*Zirconium Hamiltonians - harmonic, even symmetry to 12th order*)*)
(*\[ScriptCapitalL]allz=Table[-(1/(2m[[2]]))*u''[x]+V[2,i,x]*u[x]/.i->power[[j]],{j,1,3,1}];*)
(*solverOptions={Method->{"SpatialDiscretization"->{"FiniteElement",{"MeshOptions"->{MaxCellMeasure->0.0005}}},"Eigensystem"->{"Arnoldi",MaxIterations->Infinity}}};*)


(* ::Input:: *)
(*test=Table[NDEigensystem[\[ScriptCapitalL]allc[[1]][[i]],u[x],{x,-B,B},Neigenvalues,Evaluate@solverOptions],{i,1,1}];*)
(*Plot[test[[1]][[2]][[200]],{x,-B,B},PlotRange->All]*)


(* ::Input:: *)
(*(*\[ScriptCapitalL]allc[[1=harmonic]]\[ScriptCapitalL]allc[[2=anharmonic 6th order,3=anharmonic 12th order]]*)*)
(*(*char1d=Table[NDEigensystem[\[ScriptCapitalL]allc[[1]][[i]],u[x],{x,-B,B},Neigenvalues,Evaluate@solverOptions],{i,1,27}];*)
(*c6anh1d=Table[NDEigensystem[\[ScriptCapitalL]allc[[2]][[i]],u[x],{x,-B,B},Neigenvalues,Evaluate@solverOptions],{i,1,27}];*)
(*c12anh1d=Table[NDEigensystem[\[ScriptCapitalL]allc[[3]][[i]],u[x],{x,-B,B},Neigenvalues,Evaluate@solverOptions],{i,1,27}];*)
(*DumpSave["dump.DEC14char1d.mx",char1d]*)
(*DumpSave["dump.DEC14c6anh1d.mx",c6anh1d]*)
(*DumpSave["dump.DEC14c12anh1d.mx",c12anh1d]*)*)


(* ::Input:: *)
(*(*zhar1d=Table[NDEigensystem[\[ScriptCapitalL]allz[[1]][[i]],u[x],{x,-B,B},Neigenvalues,Evaluate@solverOptions],{i,1,27}];*)
(*z6anh1d=Table[NDEigensystem[\[ScriptCapitalL]allz[[2]][[i]],u[x],{x,-B,B},Neigenvalues,Evaluate@solverOptions],{i,1,27}];*)
(*z12anh1d=Table[NDEigensystem[\[ScriptCapitalL]allz[[3]][[i]],u[x],{x,-B,B},Neigenvalues,Evaluate@solverOptions],{i,1,27}];*)
(*DumpSave["dump.DEC14zhar1d.mx",zhar1d]*)
(*DumpSave["dump.DEC14z6anh1d.mx",z6anh1d]*)
(*DumpSave["dump.DEC14z12anh1d.mx",z12anh1d]*)*)


(* ::Input:: *)
(*(*Get["dump.DEC14char1d.mx"]*)
(*Get["dump.DEC14c6anh1d.mx"]*)
(*Get["dump.DEC14c12anh1d.mx"]*)
(*Get["dump.DEC14zhar1d.mx"]*)
(*Get["dump.DEC14z6anh1d.mx"]*)
(*Get["dump.DEC14z12anh1d.mx"]*)*)


(* ::Input:: *)
(*ListPlot@Table[char1d[[i]][[1]][[1]],{i,1,9}]*)
(*ListPlot@Table[c12anh1d[[i]][[1]][[1]],{i,1,9}]*)
(*ListPlot@Table[zhar1d[[i]][[1]][[1]],{i,1,9}]*)
(*ListPlot@Table[z12anh1d[[i]][[1]][[1]],{i,1,9}]*)


(* ::Input:: *)
(*(*NOTE also rescale the anharmonic frequencies to tend exactly to the harmonic freqs in the limit of low T ie the first mode*)*)
(*(*... there is a small random error 0.05% at for omega0, but over 200 eigenvalues gets amplied so messes up volume order of distributions*)*)
(*(*Exmaple:*)*)
(*c12anh1d[[1]][[1]][[1;;20]]*)
(*c12anh1d[[1]][[1]][[1;;20]]/c12anh1d[[1]][[1]][[1]]*)
(*char1d[[1]][[1]][[1]]*c12anh1d[[1]][[1]][[1;;20]]/c12anh1d[[1]][[1]][[1]]*)


(* ::Input:: *)
(*maxval=250;*)
(*(*Fix harmonic to use perfect summation from omega0*)*)
(*char1de=Table[Table[char1d[[i]][[1]][[1]]*2(j-0.5),{j,1,maxval}],{i,1,27}];*)
(*c6anh1de=Table[char1d[[i]][[1]][[1]]*c6anh1d[[i]][[1]][[1;;maxval]]/c6anh1d[[i]][[1]][[1]],{i,1,27}];*)
(*c12anh1de=Table[char1d[[i]][[1]][[1]]*c12anh1d[[i]][[1]][[1;;maxval]]/c12anh1d[[i]][[1]][[1]],{i,1,27}];*)
(*zhar1de=Table[Table[zhar1d[[i]][[1]][[1]]*2(j-0.5),{j,1,maxval}],{i,1,27}];*)
(*z6anh1de=Table[zhar1d[[i]][[1]][[1]]*z6anh1d[[i]][[1]][[1;;maxval]]/z6anh1d[[i]][[1]][[1]],{i,1,27}];*)
(*z12anh1de=Table[zhar1d[[i]][[1]][[1]]*z12anh1d[[i]][[1]][[1;;maxval]]/z12anh1d[[i]][[1]][[1]],{i,1,27}];*)


(* ::Input:: *)
(*GraphicsGrid[{{ListPlot[char1de],ListPlot[c12anh1de[[1;;27]]]},{ListPlot[zhar1de],ListPlot[z12anh1de[[1;;27]]]}},ImageSize->1200]*)


(* ::Input:: *)
(*GraphicsGrid[{Table[Show[ListPlot[char1de[[i]],PlotStyle->{Red},PlotLegends->SwatchLegend[{"Harmonic"}],ImageSize->400],ListPlot[{c12anh1de[[i]],c12anh1de[[9+i]],c12anh1de[[18+i]]},PlotLegends->SwatchLegend[{"Zr 001 anharmonic","Zr 011 anharmonic","Zr 111 anharmonic"}]]],{i,1,9}]},ImageSize->5000]*)
(*GraphicsGrid[{Table[ListPlot[{c12anh1de[[i]]/char1de[[i]],c12anh1de[[9+i]]/char1de[[i]],c12anh1de[[18+i]]/char1de[[i]]},ImageSize->400,PlotLegends->SwatchLegend[{"Zr 001 anharmonic","Zr 011 anharmonic","Zr 111 anharmonic"}]],{i,1,9}]},ImageSize->5000]*)


(* ::Input:: *)
(*GraphicsGrid[{{Show@Table[ListPlot[{c12anh1de[[i]]/char1de[[i]],c12anh1de[[9+i]]/char1de[[i]],c12anh1de[[18+i]]/char1de[[i]]},ImageSize->400],{i,1,9}],Show@Table[ListPlot[{z12anh1de[[i]]/zhar1de[[i]],z12anh1de[[9+i]]/zhar1de[[i]],z12anh1de[[18+i]]/zhar1de[[i]]},ImageSize->400],{i,1,9}]}}]*)


(* ::Input:: *)
(*fr[T_,spectrumZ_,spectrumC_,maxEigen_,xFactor_,pFactor_]:=-K2meV*(pFactor)*T*Log[{Total[Exp[-xFactor*spectrumZ/(K2meV*T)]],Total[Exp[-xFactor*spectrumC/(K2meV*T)]]}]*)


(* ::Input:: *)
(*Table[{t,Total@fr[t,z12anh1de[[1]],c12anh1de[[1]],150,2,1.5]},{t,4000,4000}]*)
(*Table[{t,Total@fr[t,z12anh1de[[1]],c12anh1de[[1]],275,2,1.5]},{t,4000,4000}]*)


(* ::Input:: *)
(*Show[ListPlot[Table[{t,Total@fr[t,zhar1de[[1]],char1de[[1]],150,2,1.5]},{t,1,4000,50}]],ListPlot[Table[{t,Total@fr[t,z12anh1de[[1]],c12anh1de[[1]],150,2,1.5]},{t,1,4000,50}]]]*)


(* ::Input:: *)
(*vols={"4.575\[CapitalARing]","4.600\[CapitalARing]","4.625\[CapitalARing]","4.658\[CapitalARing]","4.685\[CapitalARing]","4.730\[CapitalARing]","4.759\[CapitalARing]","4.801\[CapitalARing]","4.850\[CapitalARing]"};*)
(*dirs={" \[LeftAngleBracket]001\[RightAngleBracket]"," \[LeftAngleBracket]011\[RightAngleBracket]"," \[LeftAngleBracket]111\[RightAngleBracket]"};maxT=4000;minT=300;dT=100;*)
(*plab=Flatten@Table[StringJoin[vols[[i]],dirs[[j]]],{j,1,3},{i,1,9}]*)
(*ListPlot[Table[Table[{t,Total@fr[t,z12anh1de[[i]],c12anh1de[[i]],150,2,1.5]-Total@fr[t,zhar1de[[i]],char1de[[i]],150,2,1.5]},{t,1,4000,50}],{i,1,27}],PlotRange->{{minT,maxT},{0,24}},PlotLegends->SwatchLegend[plab],ImageSize->800,AxesLabel->{"T (K)","\!\(\*SuperscriptBox[SubscriptBox[\(F\), \(ah\)], \(Einstein\)]\) (meV/atom)"},PlotStyle->"Thick"]*)


(* ::Input:: *)
(*Show[ListPlot[Table[{T,3kB},{T,minT,maxT,dT}],PlotRange->{All,{0,0.265}}],ListPlot[Table[ Table[{T,t*D[-D[Total@fr[t,z12anh1de[[vol]],c12anh1de[[vol]],150,4,1.5],t],t]}/.t->T,{T,minT,maxT,dT}],{vol,1,9}]]]*)


(* ::Input:: *)
(*Show[ListPlot[Table[{T,3kB},{T,minT,maxT,dT}],PlotRange->{All,{0,0.265}}],ListPlot[Table[ Table[{T,t*D[-D[Total@fr[t,z12anh1de[[vol]],c12anh1de[[vol]],150,2,1.5],t],t]}/.t->T,{T,minT,maxT,dT}],{vol,1,9}]]]*)


(* ::Input:: *)
(*e0perf*)


(* ::Input:: *)
(*volperf*)


(* ::Input:: *)
(*e0volfit=Fit[Transpose[{volperf,e0perf}],{1,x,x*x,x*x*x},x];*)
(*e0afit=Fit[Transpose[{0.5(volperf^(1/3)),e0perf}],{1,x,x*x,x*x*x},x];*)
(*e0volfit*)
(*e0afit*)
(*GraphicsGrid[{{Show[ListPlot[Transpose[{volperf,e0perf}]],Plot[e0volfit,{x,750,900}]],Show[ListPlot[Transpose[{0.5(volperf^(1/3)),e0perf}]],Plot[e0afit,{x,4.5,4.9}]]}},ImageSize->1200]*)


(* ::Input:: *)
(*e0afit*)
(*eafitMin=FindMinimum[Evaluate@e0afit,x][[1]]*)
(*e0=e0perf[[1;;9]]-eafitMin*)
(*a0=0.5(volperf^(1/3))*)


(* ::Input:: *)
(*GraphicsGrid[{{Show[Plot[Evaluate@Fit[Transpose[{a0,e0}],{1,x,x*x,x*x*x},x],{x,4.5,5}],ListPlot[Transpose[{a0,e0}]]],Plot[e0afit*1000/64,{x,4.5,4.9}]}},ImageSize->900]*)


(* ::Input:: *)
(*Table[Table[{t,1000/64*e0[[i]]+Total@fr[t,zhar1de[[i]],char1de[[i]],150,2,1.5]},{t,1,10,5}],{i,4,4}]*)
(*Table[Table[{t,1000/64*e0[[i]]+Total@fr[t,zhar1de[[i]],char1de[[i]],150,1,3]},{t,1,10,5}],{i,4,4}]*)
(*ListLinePlot[Table[Table[{t,1000/64*e0[[i]]+Total@fr[t,zhar1de[[i]],char1de[[i]],150,2,1.5]},{t,1,4000,5}],{i,1,9}],PlotLegends->SwatchLegend[plab],ImageSize->800,AxesLabel->{"T (K)","\!\(\*SuperscriptBox[SubscriptBox[\(F\), \(ah\)], \(Einstein\)]\) (meV/atom)"},PlotStyle->PointSize[0.2]]*)


(* ::Input:: *)
(*fit=Fit[Table[{a0[[i]],1000/64*e0[[i]]+Total@fr[t,zhar1de[[i]],char1de[[i]],150,2,1.5]},{i,1,9}]/.t->4200,{1,x,x*x,x*x*x},x]*)
(*Minimize[e0afit,x]*)
(*Plot[fit,{x,4.5,5.2}]*)
(*Minimize[{fit,4.5<x<4.9},x]*)
(*Plot[fit,{x,4.5,5.2}]*)


(* ::Input:: *)
(*temps[t_]:=1+9.6t^2*)
(*tlist=Table[temps[t],{t,0,22}]*)
(*fitT=Table[Fit[Table[{a0[[i]],1000/64*e0[[i]]+Total@fr[t,zhar1de[[i]],char1de[[i]],150,2,1.5]},{i,1,9}]/.t->tlist[[j]],{1,x,x*x,x*x*x},x],{j,1,22}];*)
(*fitTanh=Table[Fit[Table[{a0[[i]],1000/64*e0[[i]]+Total@fr[t,z12anh1de[[i]],c12anh1de[[i]],150,2,1.5]},{i,1,9}]/.t->tlist[[j]],{1,x,x*x,x*x*x},x],{j,1,22}];*)


(* ::Input:: *)
(*minaData=Table[Minimize[{fitT[[i]],4.575<x<4.9},x],{i,1,22}]*)
(*minaDataanh=Table[Minimize[{fitTanh[[i]],4.575<x<4.9},x],{i,1,22}]*)
(*minA=Table[minaData[[i]][[2]][[1]][[2]],{i,1,22}]*)
(*mine0=Table[minaData[[i]][[1]],{i,1,22}]*)
(*minAanh=Table[minaDataanh[[i]][[2]][[1]][[2]],{i,1,22}]*)
(*mine0anh=Table[minaDataanh[[i]][[1]],{i,1,22}]*)


(* ::Subsubsection:: *)
(*Gruneisen parameters;	*)


(* ::Input:: *)
(*grunHar=ListPlot@Transpose[{minA,mine0}];*)
(*grunAnh=ListPlot@Transpose[{minAanh,mine0anh}];*)
(*GraphicsGrid[{{Show[Plot[fitT,{x,4.6,4.9}],grunHar],Show[Plot[fitTanh,{x,4.6,4.9}],grunAnh]}},ImageSize->800]*)


(* ::Input:: *)
(*tempsDens[t_]:=1+t^2*)
(*tlistDens=Table[tempsDens[t],{t,0,65}]*)


(* ::Input:: *)
(*fitTDens=Table[Fit[Table[{a0[[i]],1000/64*e0[[i]]+Total@fr[t,zhar1de[[i]],char1de[[i]],150,2,1.5]},{i,1,9}]/.t->tlistDens[[j]],{1,x,x*x,x*x*x},x],{j,1,65}];*)
(*minaDataDens=Table[Minimize[{fitTDens[[i]],4.575<x<4.9},x],{i,1,65}]*)
(*(*minADens=Table[{minaDataDens[[i]][[2]][[1]][[2]],temps[i]},{i,1,22}]*)*)
(*minADens=Table[{tempsDens[i],minaDataDens[[i]][[2]][[1]][[2]]},{i,1,65}]*)


(* ::Subsubsection:: *)
(*Thermal expansion via minimum Helmholtz variation*)


(* ::Input:: *)
(*Plot[fitTDens,{x,4.5,4.9},FrameLabel->{"a (\[CapitalARing])","Helmholtz (meV/atom"},Frame->True]*)


(* ::Subsubsection:: *)
(*Thermal expansion at 65 points*)


(* ::Input:: *)
(*ListPlot[minADens,FrameLabel->{"T (K)","a (\[CapitalARing])"},Frame->True]*)
(*VT=Fit[minADens,{t^3,t^2,t,1},t]*)
(*(*Thermal expansivity - VdV/dT - http://journals.aps.org/prb/pdf/10.1103/PhysRevB.76.064116*)*)
(*D[minADensFitT,t]*)
(*f[T_]:=VT[T]->T*)
(*f[10]//N*)
(*dVdT[T_]=D[VT,t]//Simplify*)
(*VdVdT[T_]=(1/VT)*D[VT,t]//Simplify*)
(**)


(* ::Subsubsection:: *)
(*Percentage Thermal Expansion;*)


(* ::Input:: *)
(*percentThermalExpan=Evaluate[100*VT/(Evaluate[VT/.t->0])-100]*)
(*Plot[percentThermalExpan/.t->T,{T,1,3800},FrameLabel->{"T (K)","Thermal expansion (%)"},Frame->True]*)


(* ::Subsubsection:: *)
(*Thermal expansion 0-400 and 0-4000, dVdT and thermal expansion coefficient 1/V*dV/dT;*)


(* ::Input:: *)
(*(*alpha = (1/V)*dV/dT - https://en.wikipedia.org/wiki/Thermal_expansion*)*)
(*(*The zero point volume correction has a tempertaure of 450K*)*)
(*GraphicsGrid[{{Plot[VT,{t,1,450}],Plot[VT,{t,1,4500}],Plot[dVdT[T]/.T->t,{t,1,4050}],Plot[VdVdT[T]/.T->t,{t,1,4050}]}},ImageSize->1200]*)


(* ::Input:: *)
(*Plot[fitT,{x,4.6,4.8},PlotRange->{Automatic,{-200,0}}]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*(*Maxwell identity:*)
(*G(T)=F(V,T)-V*dF/dV*)
(*http://www.tandfonline.com/doi/pdf/10.1080/01418638108222168?needAccess=true  J Harding paper*)
(**)*)
(*Plot[Evaluate[x*D[fitT[[1]],x]],{x,4.4,4.9}]*)
(*Table[Evaluate[x*D[fitT[[i]],x]]/.x->minA[[i]],{i,1,22}]*)
(*minA[[1]]*)


(* ::Subsubsection:: *)
(*Thermal expansion via the temperature modified Helmholtz free energy-volume surface - pristine quasiharmonic ZrC;*)


(* ::Input:: *)
(*(*Note - alatt at zero temperature with and without ZPE energy correction is 4.66667 and 4.65794 *)*)
(*Show[Plot[Evaluate[fitT-mine0],{x,4.575,4.89},PlotLegends->SwatchLegend[Round[tlist]"K"],PlotRange->{Automatic,{-1,65}},AxesLabel->{"a (\[CapitalARing])","\!\(\*SuperscriptBox[\(F\), \(qha\)]\)(V,T)-min[\!\(\*SuperscriptBox[\(F\), \(qha\)]\)(V,T),V]\n (meV/atom)"},ImageSize->600],ListPlot[{{4.6583,61}},Filling->Bottom],Graphics[{Text["\!\(\*SubscriptBox[\(a\), \(0\)]\)=4.658 \[CapitalARing]",{4.677,61}]}],ListPlot[{{4.6672,55}},Filling->Bottom],Graphics[{Text["\!\(\*SubscriptBox[\(a\), \(ZP\)]\)=4.667 \[CapitalARing]",{4.687,55}]}],Plot[Evaluate[(e0afit-Minimize[e0afit,x][[1]])*1000/64],{x,4.575,4.85},PlotStyle->Dashed,PlotLegends->{"\!\(\*SubscriptBox[\(E\), \(0\)]\)(V)"}]]*)


(* ::Subsubsection:: *)
(*Thermal expansion via the temperature modified Helmholtz free energy-volume surface - pristine anharmonic ZrC;*)


(* ::Input:: *)
(*Show[Plot[Evaluate[fitTanh-mine0anh],{x,4.575,4.89},PlotLegends->SwatchLegend[tlist "K"],PlotRange->{Automatic,{-1,75}},AxesLabel->{"a (\[CapitalARing])","\!\(\*SuperscriptBox[\(F\), \(anh\)]\)(V,T)-min[\!\(\*SuperscriptBox[\(F\), \(anh\)]\)(V,T),V]\n (meV/atom)"},ImageSize->600],ListPlot[{{4.6583,74}},Filling->Bottom],Graphics[{Text["\!\(\*SubscriptBox[\(a\), \(0\)]\)=4.658 \[CapitalARing]",{4.679,74}]}],ListPlot[{{4.6672,70}},Filling->Bottom],Graphics[{Text["\!\(\*SubscriptBox[\(a\), \(ZP\)]\)=4.667 \[CapitalARing]",{4.6897,70}]}],Plot[Evaluate[(e0afit-Minimize[e0afit,x][[1]])*1000/64],{x,4.575,4.89},PlotStyle->Dashed,PlotLegends->{"\!\(\*SubscriptBox[\(E\), \(0\)]\)(V)"}]]*)


(* ::Input:: *)
(*Show[Plot[Evaluate[(fitTanh)-(fitT)],{x,4.575,4.89},PlotLegends->SwatchLegend[Round[tlist]"K"],PlotRange->{Automatic,{-8,25}},AxesLabel->{"a (\[CapitalARing])","\!\(\*SuperscriptBox[\(F\), \(anh\)]\)(V,T)-\!\(\*SuperscriptBox[\(F\), \(qha\)]\)(V,T)\n (meV/atom)"},ImageSize->600],Plot[Evaluate[(e0afit-Minimize[e0afit,x][[1]])*1000/64],{x,4.575,4.89},PlotStyle->Dashed,PlotLegends->{"\!\(\*SubscriptBox[\(E\), \(0\)]\)(V)"}]]*)
(*Show[Plot[Evaluate[(mine0anh)-(mine0)],{x,4.575,4.89},PlotLegends->SwatchLegend[Round[tlist]"K"],PlotRange->{Automatic,{-8,25}},AxesLabel->{"a (\[CapitalARing])","min[\!\(\*SuperscriptBox[\(F\), \(anh\)]\)(V,T),V]-min[\!\(\*SuperscriptBox[\(F\), \(qha\)]\)(V,T),V],V]\n (meV/atom)"},ImageSize->600],Plot[Evaluate[(e0afit-Minimize[e0afit,x][[1]])*1000/64],{x,4.575,4.89},PlotStyle->Dashed,PlotLegends->{"\!\(\*SubscriptBox[\(E\), \(0\)]\)(V)"}]]*)


(* ::Input:: *)
(*tmax=4010;*)
(*tlistall=Table[t,{t,1,tmax}];*)
(*fitTall=Table[Fit[Table[{a0[[i]],1000/64*e0[[i]]+Total@fr[t,zhar1de[[i]],char1de[[i]],150,2,1.5]},{i,1,9}]/.t->T,{1,x,x*x,x*x*x},x],{T,1,tmax}];*)
(*interTall=Table[Interpolation[Table[{a0[[i]],1000/64*e0[[i]]+Total@fr[t,zhar1de[[i]],char1de[[i]],150,2,1.5]},{i,1,9}]/.t->T],{T,3700,tmax}];*)


(* ::Input:: *)
(*Plot[fitTall[[1]],{x,4.5,5.2}]*)
(*Plot[fitTall[[3750]],{x,4.75,4.9}]*)
(*Minimize[{fitTall[[3750]],4.64<x<4.89},x]*)
(*NMinimize[{fitTall[[3800]],4.64<x<4.89},x]*)
(*Minimize[{interTall[[50]][x],4.64<x<4.89},x]*)
(*NMinimize[{interTall[[100]][x],4.64<x<4.89},x]*)


(* ::Input:: *)
(*fitTallanh=Table[Fit[Table[{a0[[i]],1000/64*e0[[i]]+Total@fr[t,z12anh1de[[i]],c12anh1de[[i]],150,2,1.5]},{i,1,9}]/.t->T,{1,x,x*x,x*x*x},x],{T,1,tmax}];*)
(*minaDataTall=Table[Minimize[{fitTall[[i]],4.64<x<4.89},x,WorkingPrecision->20],{i,1,tmax}];*)
(*minaDataTallanh=Table[Minimize[{fitTallanh[[i]],4.64<x<4.89},x,WorkingPrecision->20],{i,1,tmax}];*)
(*minAall=Table[minaDataTall[[i]][[2]][[1]][[2]],{i,1,tmax}];*)
(*mine0all=Table[minaDataTall[[i]][[1]],{i,1,tmax}];*)
(*minAallanh=Table[minaDataTallanh[[i]][[2]][[1]][[2]],{i,1,tmax}];*)
(*mine0allanh=Table[minaDataTallanh[[i]][[1]],{i,1,tmax}];*)


(* ::Input:: *)
(*Plot[{kB*3,Evaluate[-x*D[D[Interpolation[Transpose[{tlistall,mine0all}]][x],x],x]],Evaluate[-x*D[D[Interpolation[Transpose[{tlistall,mine0allanh}]][x],x],x]]},{x,100,3950},PlotRange->All]*)


(* ::Input:: *)
(*minty=1;maxty1=4500;maxty2=4550;*)
(*Show[Plot[{kB*3,Evaluate[-x*D[D[Interpolation[Transpose[{tlistall,mine0all}]][x],x],x]]},{x,minty,maxty1},PlotRange->{{minty,maxty1},{0.00,0.4}},AxesLabel->{"T (K)","Fah (meV/atom)"}],*)
(*Plot[{Evaluate[-x*D[D[Interpolation[Transpose[{tlistall,mine0allanh}]][x],x],x]]},{x,minty,maxty2},PlotRange->{{minty,maxty2},{0.00,0.4}}],ImageSize->800]*)


(* ::Input:: *)
(*SetDirectory["/Volumes/MicroSD/Dropbox/PostDoc_SD/ZrC/output_qha_700eV_666kp/QHA_PERF"];*)
(*qhaCp=ReadList["Cp-temperature.dat",{Number, Number}];*)
(*cpQHAnnormal=ListLinePlot[Transpose[{Transpose[qhaCp][[1]],kjmol2evatom*Transpose[qhaCp][[2]]}],PlotStyle->Green]*)


(* ::Input:: *)
(*Show[Plot[{kB*3,Evaluate[-x*D[D[Interpolation[Transpose[{tlistall,mine0all}]][x],x],x]]},{x,minty,maxty1},PlotRange->{{minty,maxty1},{0.00,0.4}},AxesLabel->{"T (K)","Fah (meV/atom)"}],*)
(*Plot[{Evaluate[-x*D[D[Interpolation[Transpose[{tlistall,mine0allanh}]][x],x],x]]},{x,minty,maxty2},PlotRange->{{minty,maxty2},{0.00,0.4}}],cpQHAnnormal,ImageSize->800]*)


(* ::Input:: *)
(*kB*)


(* ::Subsubsection:: *)
(*Cp/Cv ratios*)
(*Einstein model*)


(* ::Input:: *)
(*cp4000=Evaluate[-x*D[D[Interpolation[Transpose[{tlistall,mine0all}]][x],x],x]]/.x->4000*)
(*cv4000=3kB*)
(*cp4000/cv4000*)


(* ::Input:: *)
(*Transpose[qhaCp][[1]][[-1]]*)


(* ::Subsubsection:: *)
(**)


(* ::Input:: *)
(*kjmol2evatom*Transpose[qhaCp][[2]][[-1]]/(3kB)*)
(**)


(* ::Input:: *)
(*kjmol2evatom*)


(* ::Subsubsection:: *)
(*Cp heat capacity in terms of kB *)
(*(Cv is 3kB at MP, Cp should a little over  4 kB)*)


(* ::Input:: *)
(*(*Phonopy*)*)
(*(*at 3800K, Cp for QHA ZrC perfect says (T,Cp) = 3800.000000000000000 2246.737466752529144 - convert to kB*)*)
(*2246.737466752529144*kjmol2evatom/kB*)
(*(*at 3960K, Cp for QHA ZrC perfect says (T,Cp) = 3960.000000000000000 2361.999360322952271 - convert to kB**)*)
(*2361.999360322952271*kjmol2evatom/kB*)


(* ::Input:: *)
(*(*MPI script - 2nd order fit*)*)
(*(*at 3800K, Cp for QHA ZrC perfect says (T,Cp) = 3800.00000000000 4.19835895965954 - convert to kB*)*)
(*4.19835895965954*)
(*(*at 3960, cp for QHA ZrC perfect says (T,Cp) = 3960.00000000000 4.37959364456706 - convert to kB*)*)
(*4.37959364456706*)
(**)


(* ::Input:: *)
(*(*MPI script - 3rd order fit*)*)
(*(*at 3800K, Cp for QHA ZrC perfect is 3800.00000000000 4.41904477773773 - convert to kB *)*)
(*4.41904477773773*)
(*(*at 3960, cp for QHA ZrC perfect is 3960.00000000000 4.73500757130209 - convert to kB*)*)
(*4.73500757130209*)


(* ::Input:: *)
(*K2meV*1900/1000*)
(**)


(* ::Input:: *)
(*0.00086*)
