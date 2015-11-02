cut_plan(CutPlanRet,PartsSorted) :-
  house(Xco, Yco),
  cut_plan_inner([part(0,0,Xco,Yco)], [], CuttingPlan, [], Parts),
  reverse(CuttingPlan, CutPlanRet),
  sort(Parts,PartsSorted).

cut_plan_inner([], Cuts, Cuts,Parts,Parts).
cut_plan_inner([CurrPart|RestParts], Cuts, CutsRet,CurrParts,PartsRet) :-
  \+ part_has_wall(CurrPart),
  single_color_present(CurrPart),
  cut_plan_inner(RestParts, Cuts, CutsRet, [CurrPart|CurrParts], PartsRet).
cut_plan_inner([CurrPart|RestParts], Cuts, CutsRet, CurrParts, PartsRet) :-
  part_has_wall(CurrPart),
  cut_part(CurrPart, NewParts, Cut),
  append(NewParts, RestParts, RecParts),
  cut_plan_inner(RecParts, [Cut|Cuts], CutsRet, CurrParts, PartsRet).

part_has_wall(Part) :- wall_in_part(Part,_), !.

wall_in_part(part(PX1,PY1,PX2,PY2),wall(v,WX,WY1,WY2)) :-
  wall(v,WX,WY1,WY2),
  InnerX1 is PX1 + 1,
  InnerX2 is PX2 - 1,
  between(InnerX1,InnerX2,WX),
  intersect((WY1,WY2),(PY1,PY2),O),
  O \== 0.
wall_in_part(part(PX1,PY1,PX2,PY2),wall(h,WY,WX1,WX2)) :-
  wall(h,WY,WX1,WX2),
  InnerY1 is PY1 + 1,
  InnerY2 is PY2 - 1,
  between(InnerY1,InnerY2,WY),
  intersect((WX1,WX2),(PX1,PX2),O),
  O \== 0.

intersect((A1,A2),(B1,B2),I) :-
  MaxBegin is max(A1,B1),
  MinEnd is min(A2,B2),
  I is max(0,MinEnd - MaxBegin).

single_color_present(Part) :-
  findall(C,color_in_part(Part,C),ColorList),
  list_to_set(ColorList,Cset),
  length(Cset,NrColors),
  NrColors =< 1.

color_in_part(part(Xstart,Ystart,PX2,PY2),Color) :-
  Xend is PX2 - 1,
  Yend is PY2 - 1,
  color(X,Y,Color),
  between(Xstart,Xend,X),
  between(Ystart,Yend,Y).

cut_part(part(X1,Y1,X2,Y2),NewParts,Cut) :-
  get_poss_cuts(part(X1,Y1,X2,Y2),PossCuts),
  member(Cut,PossCuts),
  perform_cut(part(X1,Y1,X2,Y2),Cut,NewParts).

% Perform vertical cut
perform_cut(part(PX1,PY1,PX2,PY2),cut(v,X,PY1,PY2),[part(PX1,PY1,X,PY2),part(X,PY1,PX2,PY2)]).
% Perform horizontal cut
perform_cut(part(PX1,PY1,PX2,PY2),cut(h,Y,PX1,PX2),[part(PX1,PY1,PX2,Y),part(PX1,Y,PX2,PY2)]).

get_poss_cuts(Part,Cuts) :-
  Part = part(X1,Y1,X2,Y2),
  findall(cut(v,WX,Y1,Y2),wall_in_part(Part,wall(v,WX,_,_)),VCuts),
  findall(cut(h,WY,X1,X2),wall_in_part(Part,wall(h,WY,_,_)),HCuts),
  append(VCuts,HCuts,Cuts).

color_in(Parts, CarpetOrderSorted) :-
  color_in_inner(Parts,CarpetOrder,[]),
  blue_green_check(CarpetOrder),
  sort(CarpetOrder,CarpetOrderSorted).

color_in_inner([],CarpetOrder,CarpetOrder).
color_in_inner([part(X1,Y1,X2,Y2)|Rest],CarpetOrder,CurrCOrder) :-
  color_in_part(part(X1,Y1,X2,Y2),Color),
  !,
  color_in_inner(Rest,CarpetOrder,[carpet(X1,Y1,X2,Y2,Color)|CurrCOrder]).

color_in_inner([part(X1,Y1,X2,Y2)|Rest],CarpetOrder,CurrCOrder) :-
  all_colors(Colors),
  member(Color,Colors),
  color_in_inner(Rest,CarpetOrder,[carpet(X1,Y1,X2,Y2,Color)|CurrCOrder]).

all_colors(Colors) :-
  findall(C,color(_,_,C),CList),
  list_to_set(CList,Colors).

blue_green_check(CarpetOrder) :-
  blue_green_check_inner(CarpetOrder,0,0,NrBlue,NrGreen),
  NrBlue > NrGreen.

blue_green_check_inner([],NrBlue,NrGreen,NrBlue,NrGreen).
blue_green_check_inner([carpet(X1,Y1,X2,Y2,green)|Rest],CurrBlue,CurrGreen,NrBlue,NrGreen) :-
  !,
  rectangle_size(X1,Y1,X2,Y2,Size),
  NewGreen is CurrGreen + Size,
  blue_green_check_inner(Rest,CurrBlue,NewGreen,NrBlue,NrGreen).
blue_green_check_inner([carpet(X1,Y1,X2,Y2,blue)|Rest],CurrBlue,CurrGreen,NrBlue,NrGreen) :-
  !,
  rectangle_size(X1,Y1,X2,Y2,Size),
  NewBlue is CurrBlue + Size,
  blue_green_check_inner(Rest,NewBlue,CurrGreen,NrBlue,NrGreen).
blue_green_check_inner([carpet(X1,Y1,X2,Y2,_)|Rest],CurrBlue,CurrGreen,NrBlue,NrGreen) :-
  blue_green_check_inner(Rest,CurrBlue,CurrGreen,NrBlue,NrGreen).

rectangle_size(X1,Y1,X2,Y2,Size) :-
  Xlength is X2 - X1,
  Ylength is Y2 - Y1,
  Size is Xlength * Ylength.