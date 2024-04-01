(* 2024-03-28 *)

(* ::Package:: *)

BeginPackage["Transform2DPlot`"] ;

Transform2DPlot::"usage"="Transform2DPlot[ f[x,y], {x,xMin,xMax}, {y,yMin,yMax}, (options)] plots the image of a rectangular grid under the transformation function f:R^2->R^2 or f:C^1->C^1. f must return a 2D real vector or a complex number.
Example:

Transform2DPlot[(a + b*I)^2, {a, 0, 1}, {b, 0, .5}]

Transform2DPlot[RotationMatrix[30 Degree] . {x, y}, {x, -3, 3}, {y, -3, 3}, ColorFunction -> Hue]

Special Options include:

PlotPoints->{m,n} controls the density of the grid.

ColorFunction->Hue is the standard option that controls the color of the plot.
The argument to the color function is the distance of {x,y} to f[x,y], scaled so it runs from 0 to 1.";

Transform2DPlot::"func"="Warning: The function `` does not evaluate to a pair of numbers or a complex number at the test point {1.,1.}.";
Transform2DGraphicsPlot::"usage"="Transform2DGraphicsPlot[ 2dGraphics, f, (options)] returns graphics under the transformation function f:R^2->R^2 or f:C^1->C^1. The function f must return a 2D real vector of the form {a,b} or a Complex number of the form a+b*I.

Special options include: ResolutionLength, AdaptiveSampling (True/False), and MaxDistance.
Example:

Transform2DGraphicsPlot[Plot[Sin[x],{x,0,2*Pi},AspectRatio->Automatic],Function[{x,y},{x + 1.1 y, y}],AspectRatio->Automatic,Axes->True]";

AdaptiveSampling::usage="AdaptiveSampling is a option for Transform2DGraphicsPlot, StereographicProjectionPlot3D, and StereographicProjectionPlot3D. AdaptiveSampling->True produce smoother plots. It tries to make the final image so that any neighboring points has a distance less than ResolutionLength. The option MaxRecursion->n limits the refining process to n repetitions. The default is AdaptiveSampling->True and MaxRecursion->10.";

ResolutionLength::"usage"="ResolutionLength is an option for Transform2DGraphicsPlot, StereographicProjectionPlot3D, and StereographicProjectionPlot3D. ResolutionLength->maxLength sets a resolution of the image under f. 0 < maxLength <= Infinity. If AdaptiveSampling->True, than neighboring points in the result image have a distance less or equal to ResolutionLength with MaxRecursion->n limit. If AdaptiveSampling->False, then the input graphics be broken so that all segments have length less than ResolutionLength before the transformation function is appled. The smaller maxLength, the more accurate the image under f. The default is ResolutionLength->Automatic.

With ResolutionLength->Automatic, its value is computed based on ResolutionDefaultDivision.";

ResolutionDefaultDivision::"usage"="ResolutionDefaultDivision is an option for Transform2DGraphicsPlot, StereographicProjectionPlot3D, and StereographicProjectionPlot3D. It is used to set the ResolutionLength when ResolutionLength->Automatic. ResolutionDefaultDivision->n set ResolutionLength to graphicsBondary/n, where graphicsBondary is the maximum distance in the graphics measured along the axes. Default is ResolutionDefaultDivision->50.";

ResolutionLength::"zero"="ResolutionLength must be a real number greater than 0.";

IncreaseGraphicsResolution::"usage"="IncreaseGraphicsResolution[graphics,maxDistance] returns a modified graphics such that any neighboring points has distance less or equal to maxDistance. Graphic Primitives Rectangle or Circle is replaced by Polygon and Line respectively. The purpose of IncreaseGraphicsResolution is that transformation function can be applied to intermediate points in g instead on extremities only.

IncreaseGraphicsResolution[g,maxDistance,f,maxRecursion] break the graphics so that the distance between any two neighboring points f[pt1] and f[pt2] is less than maxDistance. This is useful for adaptive sampling when doing transformations. maxRecursion sets a limit in this process. Note that the graphics g can be 2d Graphics or Graphics3D.
Example:

IncreaseGraphicsResolution[ {Circle[{0,0},1],Line[{{-2,0},{2,0}}], Rectangle[{0,0},{1,1}]},0.5 ]";

PolarGrid::"usage"="PolarGrid[{rMin, rMax,(rStep)},{thetaMin, thetaMax, (thetaStep)},({ColorFunction1, ColorFunction2})] generates a list of Graphic Primitives that represents a polar grid.
The optional color functions are used to determine the color of the r and \[Theta] grids respectively.
The arguments provided for the specified function are always in the range 0 to 1.
The specified function must return a one of the color directive such as Hue[].

Example:

Show[ Graphics[ PolarGrid[{1, 5, 3/4},{0, 4, 4/20},{Hue, GrayLevel}]], AspectRatio->Automatic]";

RectangularGrid::"usage"="RectangularGrid[ {xMin, xMax, xStep}, {yMin, yMax, yStep}, ({ColorFunction1, ColorFunction2 })] generates a list of Graphic Primitives that represents a recangular grid.
The optional pair of color functions are used to determine the color of the x and y grids respectively.
The arguments provided for the specified function are always in the range 0 to 1.
The specified function must return a color directive, e.g. GrayLevel, Hue.

Example:

Show[ Graphics[ RectangularGrid[{1,5,4/20},{2,4,2/4},{Hue,GrayLevel}]], AspectRatio->Automatic]";

TriangularGrid::"usage"="TriangularGrid[ r, n, ({c1, c2}), ({ColorFunction1, ColorFunction2, ColorFunction3 })] generates a list of Graphic Primitives that represents a triangular grid with hexagonal boundary. r is the radius of an inscribed circle. n is the number of parallel lines in each direction. {c1,c2} is an optional argument for center. Default is the origin. The optional color functions are used to determine the color of the grids. The arguments provided for the specified function are always in the range 0 to 1. The specified function must return a CMYKColor, GrayLevel, Hue or RGBColor directive.
Example:

Show[Graphics@TriangularGrid[5,19,{0,0},{Hue,Hue,Hue}],AspectRatio->Automatic]";

RectangularPatchGraphics::usage="RectangularPatchGraphics[ {xMin, xMax, xNumber,(xGapScale)}, {yMin, yMax, yNumber,(yGapScale)}] generates a grid of rectangular patches. xNumber is the number of rectangles. xGapScale is the gap between rectangles. The default is 1/10.

RectangularPatchGraphics[{...}, {...},cf] applies the color function cf to the center of the patches. cf must take two arguments cf[x,y] and return a color directive such as Hue. The default cf varies hue by x and brightness by y.
Example:

Show[RectangularPatchGraphics[{-5,5,5,.2}, {-5,5,4,.2}],Frame->True,AspectRatio->Automatic,PlotRange->All]";

PolarPatchGraphics::usage="PolarPatchGraphics[{rMin, rMax,rNumber,(rGap)},{\[Theta]Min, \[Theta]Max, \[Theta]Number, (\[Theta]Gap)}] generates a polar grid of patches. rNumber is the number of patches. rGap is the gap between rectangles as a percentage of the patch radial length. The default is 1/10.

PolarPatchGraphics[{...}, {...},(resolutionLength),(cf)] uses a resolutionLength to determine how close are neighboring points in the generated graphics. Default is 1/10. cf applies the color function cf[r,\[Theta]] to the patches. cf must return a color directive such as Hue. The default cf varies hue by \[Theta] and brightness r.
Example:

Show[PolarPatchGraphics[{1,2,5}, {0,6,15}],Frame->True,AspectRatio->Automatic,PlotRange->All]";

StereographicProject::"usage"="StereographicProject[{p1,p2}] returns a 3D coordinate {q1,q2,q3} that is the point {p1,p2} stereographically projected to a sphere of radius 1/2 sitting on the origin. StereographicProject[{p1,p2},r] assumes the sphere has radius r. StereographicProject[{p1,p2},r,c] assumes the sphere has radius r and its center is above the origin by c units.";

StereographicProjectionPlot3D::usage="StereographicProjectionPlot3D[planeGraphics,(r),(c)] shows a Graphics3D object that is the input 2D graphics steoreographically projected to a sphere of radius r, and above the Origin by c. The default r is 1/2 and default c is 1/2. StereographicProjectionPlot3D takes all options of Graphics3D, and AdaptiveSampling->True,ResolutionLength->0.05, and MaxRecursion->10.
Example:

StereographicProjectionPlot3D[ParametricPlot[t*{Cos@t,Sin@t}*.04,{t,0,99},AspectRatio->Automatic]]
";

MobiusTransform::usage="MobiusTransform[a,b,c,d] returns a function that is the Mobius Transformation defined as f[z]:=(a*z + b)/(c*z + d), where all variables are complex numbers. MobiusTransform[a,b,c,d][z] return a expression evaluated at z. For example:

MobiusTransform[1 + 1*I, 2 + 2*I, 3 + 3*I, 4 + 4*I][5 + 5*I] returns 104/293-(5*I)/293";

MobiusTransform2::usage="MobiusTransform2[{a1,a2},{b1,b2},{c1,c2},{d1,d2}] returns a function that is the Mobius Transformation defined as f[Z]:=(A*Z + B)/(C*Z + D), where all variables are complex numbers. MobiusTransform[{a1,a2},{b1,b2},{c1,c2},{d1,d2}][{z1,z2}] return a expression evaluated at {z1,z2}. For
Example:

MobiusTransform2[{1,1},{2,2},{3,3},{4,4}][{5,5}] returns {104/293,-(5/293)}";

(* 2024-03-31
todo
vectorFunctionCanonicalForm.
why is this needed.
rephrase doc. it's unclear
*)

vectorFunctionCanonicalForm::usage="vectorFunctionCanonicalForm[f] returns a function f2 that takes a 2D vector as input and returns a 2D vector as output.
The argument f can be a function of these forms
f[_Real,_Real],
f[{_Real,_Real}],
f[_Complex].
The first two returns a 2D Real vector {_Real,_Real}, and the last returns a complex number _Complex.
f2 have this form: f2[{_Real,_Real}] and its output is this form {_Real,_Real}.
";

Begin["`Private`"];

Clear[graphicsBoundary];
graphicsBoundary::usage="graphicsBoundary[2dGraphics] returns {{xMin,xMax},{yMin,yMax}} of the coordinates of points in the graphics. graphicsBoundary[3dGraphics] returns {{xMin,xMax},{yMin,yMax},{zMin,zMax}}. Only Polygon,Line, Rectanle graphics primitives are considered.
Example: graphicsBoundary@ParametricPlot3D[{u,v,Sin[u*v]},{u,0,3},{v,0,3}]";

graphicsBoundary[gra_]:=({Min@#,Max@#}&)/@Transpose@(#/.Point->List&)@Flatten@Cases[gra/.Rectangle[{xm_,ym_},{xM_,yM_}]:>Polygon[{{xm,ym},{xM,ym},{xM,yM},{xm,yM},{xm,ym}}],Polygon[pts_]|Line[pts_]:>(Point@@#&)/@pts,Infinity]

Clear[boundaryPoints];
boundaryPoints::usage="boundaryPoints[{{xMin,xMax},{yMin,yMax}}] returns the boundary points {{xMin,yMin},{xMin,yMax},{xMax,yMin},{xMax,yMax}}. boundaryPoints[{{xMin,xMax},{yMin,yMax},{zMin,zMax}}] returns the 8 boundary points in 3D. boundaryPoints returns 2^n vectors, each of length n, if there are n minmax pairs in the input.";

boundaryPoints[li_List]:=Flatten@Outer[Point,Sequence@@li]/.Point->List

Clear[insertMiddlePoint];

insertMiddlePoint::usage="insertMiddlePoint[pt1,pt2,maxDist,f,maxRecursion] returns a recursive list of points that lies between pt1 and pt2. maxDist is the length between the neighbor points. pt1 and pt2 are n-dimensional vectors. maxRecursion is a integer that limit the repetition of recursion.

In general, {Point[pt1],Flatten@insertMiddlePoint[pt1,pt2,maxDist,f,maxRecursion]} return a list {Point[pt1],..., Point[pt2]} of interpolated points.";

insertMiddlePoint[a_,b_,maxDist_,f_,0]=Point@b;

insertMiddlePoint[a_,b_,maxDist_,f_,n_]:=
If[Sqrt[# . #]&[f[a]-f[b]]>maxDist,{insertMiddlePoint[a,(a+b)/2,maxDist,f,n-1],insertMiddlePoint[(a+b)/2,b,maxDist,f,n-1]},Point@b]

Clear[linearInterpolate];

linearInterpolate::"usage"="linearInterpolate[{point1,point2,...}, maxDist] returns the list of points with new points inserted in between existing points. The added points lie on the line between neighboring points. New points are added if the distance between neighboring points are greater than maxDist.

linearInterpolate[{point1,point2,...}, maxDist,f,maxRecursion] use the distance between f[point1] and f[point2] in comparison to maxDist as a crition for whether to add a new point between point1 and point2.  maxRecursion is a integer that limit the repetition of recursion process.
Example:

linearInterpolate[{{0},{1}},.2]

(*2D example*)
linearInterpolate[{{0,0},{0,1}},.3]

(*3D example*)
linearInterpolate[{{0,0,0},{1,1,1}},.3]

(*linear interpolate the real line between 0 and 4, so that the square function applied to it comes out smooth. *)

Show@Graphics[{PointSize[.05],Point/@(({First@#,First@#^2}&)/@linearInterpolate[{{0},{4}},1,Function[#^2],9])},Frame->True,AspectRatio->Automatic]";

linearInterpolate[pts_List,maxDist_]:=Module[{res={},p1,p2,n},
Do[{p1,p2}=pts[[{i,i+1}]];
If[(n=Ceiling[(Sqrt[#1 . #1]&)[p2-p1]/maxDist])>1,res={res,Table[Point[p1+(p2-p1) j],{j,0,1-1/(2 n),1/n}]},res={res,Point[p1]}],{i,1,Length[pts]-1}];(#1/. Point[t_]->t&)[Flatten[{res,Point[Last[pts]]}]]]

linearInterpolate[pts_List,maxDist_,f_,n_]:=(#1/. Point[t_]->t&)[Flatten[{Point[First[pts]],(insertMiddlePoint[Sequence@@#1,maxDist,f,n]&)/@Partition[pts,2,1]}]]

Clear[breakCircleDiskRectangle];
breakCircleDiskRectangle[gra_,maxDist_/;N[maxDist]!=0]:=With[{md=maxDist},gra/. {(Circle|Disk)[{c1_,c2_},0|0.,___]:>Point[{c1,c2}],Circle[{c1_,c2_},r_?NumberQ]:>With[{iStep=(2 Pi)/Ceiling[(2 r Pi)/md]},Line[Table[r {Cos[t],Sin[t]}+{c1,c2},{t,0,2 Pi,iStep}]]],Circle[{c1_,c2_},{a_,b_}]:>With[{iStep=(2 Pi)/Ceiling[(2 Pi Max[a,b])/md]},Line[Table[{a Cos[t],b Sin[t]}+{c1,c2},{t,0,2 Pi,iStep}]]],Circle[{c1_,c2_},r_,{a_,b_}]:>With[{iStep=(b-a)/Ceiling[(2 r Pi)/md]},Line[Table[r {Cos[t],Sin[t]}+{c1,c2},{t,a,b,iStep}]]],Disk[{c1_,c2_},r_?NumberQ]:>With[{iStep=(2 Pi)/Ceiling[(2 r Pi)/md]},Polygon[Table[r {Cos[t],Sin[t]}+{c1,c2},{t,0,2 Pi,iStep}]]],Disk[{c1_,c2_},{a_,b_}]:>With[{iStep=(2 Pi)/Ceiling[(2 Pi Max[a,b])/md]},Polygon[Table[{a Cos[t],b Sin[t]}+{c1,c2},{t,0,2 Pi,iStep}]]],Disk[{c1_,c2_},r_,{a_,b_}]:>With[{iStep=(b-a)/Ceiling[(2 r Pi)/md]},Polygon[Table[r {Cos[t],Sin[t]}+{c1,c2},{t,a,b,iStep}]]],Rectangle[{xm_,ym_},{xM_,yM_}]:>Polygon[{{xm,ym},{xM,ym},{xM,yM},{xm,yM}}]}]

(* N[gra_,Infinity]:=gra; *)

IncreaseGraphicsResolution[gra_,maxDist_/;(N@maxDist!=0)]:=With[{md=maxDist},(breakCircleDiskRectangle[gra,maxDist]/.{Polygon[pts_]:>Polygon@(Drop[#,-1]&)@(linearInterpolate[#,md]&)@Append[pts,First@pts],Line[pts_]:>Line[linearInterpolate[pts,md]]})]

IncreaseGraphicsResolution[gra_,maxDist_/;(N@maxDist!=0),f_,n_]:=breakCircleDiskRectangle[gra,maxDist]/.{Polygon[points_]:>Polygon@linearInterpolate[Append[points,First@points],maxDist,f,n],Line[points_]:>Line[linearInterpolate[points,maxDist,f,n]]}

PolarGrid[{rMin_,rMax_,rStep_:Automatic},{thetaMin_,thetaMax_,thetaStep_:Automatic}]:=PolarGrid[{rMin,rMax,rStep},{thetaMin,thetaMax,thetaStep},{Hue[0.7,#1,0.8]&,Hue[0,#1,0.8]&}];

PolarGrid[{rMin_,rMax_,rStep_:Automatic},{thetaMin_,thetaMax_,thetaStep_:Automatic},{cf1_,cf2_}]:=
Module[{thetaRange=N[thetaMax-thetaMin],rRange=N[rMax-rMin],thetaStep2,rStep2},
		If[thetaStep===Automatic,thetaStep2 = thetaRange/14,thetaStep2=thetaStep];
		If[rStep===Automatic,rStep2 = rRange/14,rStep2=rStep];
		N@{Table[{cf2[(i - thetaMin)/thetaRange],Line[{rMin {Cos[i],Sin[i]},rMax {Cos[i],Sin[i]}}]},{i,thetaMin,thetaMax,N[thetaStep2]}],Table[{cf1[(i - rMin)/rRange],Circle[{0,0},i,{thetaMin,thetaMax}]},{i,rMin,rMax,N[rStep2]}]}];

RectangularGrid[{xMin_,xMax_,xStep_:Automatic},{yMin_,yMax_,yStep_:Automatic}]:=RectangularGrid[{xMin,xMax,xStep},{yMin,yMax,yStep},{Hue[0,#1,0.8]&,Hue[0.7,#1,0.8]&}];

RectangularGrid[{xMin_,xMax_,xStep_:Automatic},{yMin_,yMax_,yStep_:Automatic},{cf1_,cf2_}]:=
Module[{xRange=N[xMax-xMin],yRange=N[yMax-yMin],xStep2,yStep2},
		If[xStep===Automatic,xStep2=xRange/First@Flatten@{PlotPoints /. Options[Plot3D, N[PlotPoints]]},xStep2=xStep];

		If[yStep===Automatic,yStep2=yRange/Last@Flatten@{PlotPoints /. Options[Plot3D, N[PlotPoints]]},yStep2=yStep];

		N@{Table[{cf1[(i - xMin)/xRange],Line[{{i,yMin},{i,yMax}}]},{i,xMin,xMax,N[xStep2]}],Table[{cf2[(i - yMin)/yRange],Line[{{xMin,i},{xMax,i}}]},{i,yMin,yMax,N[yStep2]}]}
];

TriangularGrid[d_,n_]:=TriangularGrid[d,n,{0,0},{GrayLevel,Hue[0,#1,0.8]&,Hue[0.7,#1,0.8]&}]/;NumberQ[N[d]]&&NumberQ[N[n]];

TriangularGrid[d_,n_,{c1_,c2_}]:=TriangularGrid[d,n,{c1,c2},{GrayLevel,Hue[0,#1,0.8]&,Hue[0.7,#1,0.8]&}]/;And@@NumberQ/@N[{d,n,c1,c2}];

TriangularGrid[d_,n_,{cf1_,cf2_,cf3_}]:=TriangularGrid[d,n,{0,0},{cf1,cf2,cf3}]/;NumberQ[N[d]]&&NumberQ[N[n]];

TriangularGrid[d_,n_,{c1_,c2_},{cf1_,cf2_,cf3_}]:=Module[{colorList,points1,cos=N[Cos[Pi/3]],sin=N[Sin[Pi/3]]},colorList=N[Transpose[({cf1[#1],cf2[#1],cf3[#1]}&)/@N[Range[0,1,1/(n-1)]]]];points1=Line/@N[Table[{{-((2 d-Abs[i])/Sqrt[3.]),i},{(2 d-Abs[i])/Sqrt[3.],i}},{i,N[-d],N[d],N[(2 d)/(n-1)]}]];points1={Transpose[{colorList[[1]],points1}],Transpose[{colorList[[2]],points1/.line:Line[_]:>Map[{{cos,-sin},{sin,cos}} . #1&,line,{2}]}],Transpose[{colorList[[3]],points1/.line:Line[_]:>Map[{{cos,sin},{-sin,cos}} . #1&,line,{2}]}]};N[If[N[{c1,c2}]===N[{0,0}],points1,points1=points1/.line:Line[_]:>Map[N[#1+{c1,c2}]&,line,{2}]]]]/;And@@NumberQ/@N[{d,n,c1,c2}];

RectangularPatchGraphics[{xMin_, xMax_, xNumber_,xGapScale2_:Automatic}, {yMin_,yMax_,yNumber_,yGapScale2_:Automatic},cf2_:Automatic]:=Module[{xStep,xGapWidth,xSideLength,yStep,yGapWidth,ySideLength,gp,maxLength},
If[cf2===Automatic,cf=(Hue[(#1-xMin)/(xMax-xMin),1-(#2-yMin)/(yMax-yMin),.9]&),cf=cf2];
If[xGapScale2===Automatic,xGapScale=1/10,xGapScale=xGapScale2];
If[yGapScale2===Automatic,yGapScale=1/10,yGapScale=yGapScale2];
xStep=(xMax-xMin)/(xNumber);
yStep=(yMax-yMin)/(yNumber);
xGapWidth=xStep*xGapScale;
yGapWidth=yStep*yGapScale;
xSideLength=xStep-xGapWidth;
ySideLength=yStep-yGapWidth;
gp=Table[Rectangle[{x-xSideLength/2,y-ySideLength/2},{x+xSideLength/2,y+ySideLength/2}],{x, xMin + xStep/2, xMax - xStep/2 + xStep/4, xStep},{y,yMin+yStep/2,yMax- yStep/2 + yStep/4,yStep}];
Graphics@gp/.rect:Rectangle[{a_,b_},{c_,d_}]:>{cf[(a+c)/2,(b+d)/2],rect}
]

PolarPatchGraphics[{rMin_,rMax_,rNumber_,rGapScale2_:Automatic},{thetaMin_, thetaMax_, thetaNumber_,thetaGapScale2_:Automatic},resolution2_:Automatic,cf2_:Automatic]:=Module[{rStep,rGapSweep,rSideSweep,thetaStep,thetaGapSweep,thetaSideSweep,radialBeginEndPairs,sweepBeginEndPairs,sectorsArray,gp,center,sector,resolution},

If[cf2===Automatic,cf=(Hue[#2/(thetaMax-thetaMin),1-(#1-rMin)/(rMax-rMin),.9]&),cf=cf2];If[rGapScale2===Automatic,rGapScale=1/10,rGapScale=rGapScale2];If[thetaGapScale2===Automatic,thetaGapScale=1/10,thetaGapScale=thetaGapScale2];
If[resolution2===Automatic,resolution=1/10,resolution=resolution2];

rStep=(rMax-rMin)/rNumber;
rGapSweep=rStep*rGapScale;
rSideSweep=rStep-rGapSweep;
thetaStep=(thetaMax-thetaMin)/thetaNumber;
thetaGapSweep=thetaStep*thetaGapScale;
thetaSideSweep=thetaStep-thetaGapSweep;

radialBeginEndPairs=Table[{r - rSideSweep/2,r + rSideSweep/2},{r,rMin + rStep/2, rMax - rStep/2 + rStep/4, rStep}];
sweepBeginEndPairs=Table[{theta - thetaSideSweep/2,theta + thetaSideSweep/2},{theta,thetaMin + thetaStep/2, thetaMax - thetaStep/2 + thetaStep/4, thetaStep}];

sectorsArray=Function[{s},sector[#,s]&/@radialBeginEndPairs]/@sweepBeginEndPairs;

gp=sectorsArray/.{sector[{rA_,rB_},{tA_,tB_}]:>sector[center[(rA+rB)/2,(tA+tB)/2],

Polygon@Flatten[{({Cos@#,Sin@#}rA&)/@Range[tA,tB,(tB-tA)/(Ceiling[(rA (-tA+tB))/resolution])],

({Cos@#,Sin@#}rB&)/@Range[tB,tA,(tA-tB)/(Ceiling[(rA (-tA+tB))/resolution])]},1]

]};
Graphics@{gp/.{sector->List,center[r_,theta_]:>cf[r,theta]}}

]

vectorFunctionCanonicalForm[fun_]:=Module[{fun2},
Off[First::"normal",Last::"normal",Function::"slotn",Function::"fpct"];
fun2=Which[((VectorQ@#)&&(And@@(NumberQ/@#))&)@N@fun[3.,4.],fun@@#&,
NumberQ@N@fun[3.,4.]&&Not@NumberQ@N@fun[3.],(({Re[#],Im[#]}&)@(fun@@#))&,
NumberQ@N@fun[3.],({Re[#],Im[#]}&)@fun[First@#+I*Last@#]&,
((VectorQ@#)&&(And@@(NumberQ/@#))&)@N@fun[{3.,4.}],fun@#&];
On[First::"normal",Last::"normal",Function::"slotn",Function::"fpct"];
fun2
]

Options[Transform2DPlot]=Union[{ColorFunction->(Hue[0,#1,0.8]&),PlotPoints->{24,24}},
  Options[Graphics]/.{(AspectRatio->x_):>AspectRatio->Automatic,(Axes->False):>Axes->True}];

Transform2DPlot[expr_,{x_,xMin_,xMax_},{y_,yMin_,yMax_},opts___Rule]:=Transform2DPlot[Function[{x,y},expr],{xMin,xMax},{yMin,yMax},opts];
Transform2DPlot[fun_,{xMin_,xMax_},{yMin_,yMax_},opts___Rule]:=Module[{plotPoints,colorFunction,coordArray,coordArray2,colorInfoArray,minColor,maxColor,vLineGP,hLineGP},plotPoints=PlotPoints/. {opts}/. Options[Transform2DPlot];
plotPoints={First[Flatten[{plotPoints}]],Last[Flatten[{plotPoints}]]};
colorFunction=ColorFunction/. {opts}/. Options[Transform2DPlot];
coordArray=Table[{x,y},{x,N[xMin],N[xMax],N[(xMax-xMin)/(First[plotPoints]-1)]},{y,N[yMin],N[yMax],N[(yMax-yMin)/(Last[plotPoints]-1)]}];
coordArray2=Map[vectorFunctionCanonicalForm[fun],coordArray,{-2}];
colorInfoArray=MapThread[Sqrt[Plus@@((#1-#2)^2)]&,{coordArray,coordArray2},2];
minColor=Min[colorInfoArray];
maxColor=Max[colorInfoArray];
colorInfoArray=Map[colorFunction,(colorInfoArray-minColor)/(maxColor-minColor+0.0001),{-1}];
vLine=Map[Line,(Partition[#1,2,1]&)/@coordArray2,{-3}];
hLine=Map[Line,(Partition[#1,2,1]&)/@Transpose[coordArray2],{-3}];
vLine=Transpose[{Transpose[Drop[Transpose[colorInfoArray],-1]],vLine},{3,2,1}];
hLine=Transpose[{Transpose[Drop[colorInfoArray,-1]],hLine},{3,2,1}];
Show[Graphics[{vLine,hLine},Sequence@@FilterRules[Flatten[{opts,Options[Transform2DPlot]}],Options[Graphics]]]]]/;
And@@NumberQ/@N[{xMin,xMax,yMin,yMax}]

Options[Transform2DGraphicsPlot]=Union[{ResolutionLength ->Automatic,ResolutionDefaultDivision ->50,AdaptiveSampling->True,MaxRecursion->10}, Options[Graphics]];

Transform2DGraphicsPlot[gra_,fun_,opts___Rule]:=Module[{gp,maxLength,rdDivision,as,fun2,maxRecurse},maxLength=ResolutionLength/. {opts}/. Options[Transform2DGraphicsPlot];rdDivision=ResolutionDefaultDivision/. {opts}/. Options[Transform2DGraphicsPlot];as=AdaptiveSampling/. {opts}/. Options[Transform2DGraphicsPlot];maxRecurse=MaxRecursion/. {opts}/. Options[Transform2DGraphicsPlot];fun2=vectorFunctionCanonicalForm[fun];If[Head[gra]===Graphics,gp=N[First[gra]],gp=N[gra]];If[maxLength===Automatic,maxLength=1/rdDivision Max[(Last[#1]-First[#1]&)/@graphicsBoundary[gp]]];(Show[Graphics[#1,Sequence@@FilterRules[Flatten[{opts,Options[Transform2DGraphicsPlot]}],Options[Graphics]]]]&)[If[as,IncreaseGraphicsResolution[gp,maxLength,fun2,maxRecurse],IncreaseGraphicsResolution[gp,maxLength]]/. {poly:Polygon[_]:>Map[fun2,poly,{2}],li:Line[_]:>Map[fun2,li,{2}],pt:Point[_]:>Map[fun2,pt,{1}]}]]/;If[(ResolutionLength/. {opts}/. Options[Transform2DGraphicsPlot])=!=Automatic&&N[ResolutionLength/. {opts}/. Options[Transform2DGraphicsPlot]]==0,Message[ResolutionLength::"zero",fun];False,True]

StereographicProject[{p1_,p2_}]:=With[{cR1=1+p1^2+p2^2},{p1/cR1,p2/cR1,1-1/cR1}]

StereographicProject[{p1_,p2_},r_]:=With[{cR1=4 r^2+4 Sqrt[r^4],cR2=p1^2+p2^2+4 r^2},{(cR1 p1)/(2 cR2),(cR1 p2)/(2 cR2),2 r-(cR1 r)/cR2}]

StereographicProject[{p1_,p2_},1/2,1/2]:=StereographicProject[{p1,p2}]

StereographicProject[{p1_, p2_}, r_, c3_] :=
   With[{n3 = r + c3},
     With[{cR1 = Sqrt[4*(c3 - n3)^2*n3^2 -
               4*(n3^2 + p1^2 + p2^2)*(c3^2 -
                    2*c3*n3 + n3^2 - r^2)],
         cR2 = n3^2 + p1^2 + p2^2},
       {((cR1 - 2*c3*n3 + 2*n3^2)*p1)/(2*cR2),
         ((cR1 - 2*c3*n3 + 2*n3^2)*p2)/(2*cR2),
         n3 - (n3*(cR1 - 2*c3*n3 + 2*n3^2))/
             (2*cR2)}]]

Options[StereographicProjectionPlot3D] =
     Union[{ResolutionLength -> 0.07,
         AdaptiveSampling -> True, MaxRecursion ->
           10}, Options[Graphics3D] /.
         {(Axes -> False) -> Axes -> True,
           (Lighting -> Automatic) -> Lighting ->
               "Neutral"}];

StereographicProjectionPlot3D[gra_Graphics,opts___Rule]:=StereographicProjectionPlot3D[gra,1/2,1/2,opts];

StereographicProjectionPlot3D[gra_Graphics,r2_/;NumberQ[r2],opts___Rule]:=StereographicProjectionPlot3D[gra,r2,1/2,opts];

StereographicProjectionPlot3D[gra_Graphics,r_,c_,opts___Rule]:=Module[{gp},maxLength=ResolutionLength/. {opts}/. Options[StereographicProjectionPlot3D];as=AdaptiveSampling/. {opts}/. Options[Transform2DGraphicsPlot];Show[Graphics3D[If[as,IncreaseGraphicsResolution[List@@gra,maxLength,StereographicProject[#1,r,c]&,maxRecurse],IncreaseGraphicsResolution[List@@gra,maxLength]]/. {p_Polygon:>Map[StereographicProject[#1,r,c]&,p,{2}],li_Line:>Map[StereographicProject[#1,r,c]&,li,{2}]},Sequence@@FilterRules[Flatten[{opts,Options[StereographicProjectionPlot3D]}],Options[Graphics3D]]]]]/;If[(ResolutionLength/. {opts}/. Options[Transform2DGraphicsPlot])=!=Automatic&&N[ResolutionLength/. {opts}/. Options[Transform2DGraphicsPlot]]==0,Message[ResolutionLength::"zero",fun];False,True]&&NumberQ[r]&&NumberQ[c]

MobiusTransform[a_Complex,b_Complex,c_Complex,d_Complex]=Function[(a*#+b)/(c*#+d)];

complexPlus[{a_,b_},{c_,d_}]:={a+c,b+d}
complexTimes[{a_,b_},{c_,d_}]:={a*c-b*d,b*c+a*d}
complexInversion[{a_,b_}]:=1/(a^2+b^2)*{a,-b}

MobiusTransform2[{a1_,a2_},{b1_,b2_},{c1_,c2_},{d1_,d2_}][{z1_,z2_}]:=complexTimes[complexPlus[complexTimes[{a1,a2},{z1,z2}],{b1,b2}],complexInversion[complexPlus[complexTimes[{c1,c2},{z1,z2}],{d1,d2}]]]

End[];

(*
Protect[vectorFunctionCanonicalForm,
Transform2DPlot,
Transform2DGraphicsPlot,
StereographicProjectionPlot3D,
AdaptiveSampling,
ResolutionLength,
ResolutionDefaultDivision,
IncreaseGraphicsResolution,
PolarGrid,
RectangularGrid,
TriangularGrid,
RectangularPatchGraphics,
PolarPatchGraphics,
StereographicProject,
MobiusTransform,
MobiusTransform2];
*)

EndPackage[];
