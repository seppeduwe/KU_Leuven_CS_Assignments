

% Assignment 1
% Given ListPossNumbers (list of possible digits), MaxNr (maximum number),
% generate all possible RelNr ('relevant numbers') such that 
%      - RelNr =< MaxNr
%      - RelNr only contains digits in ListPossDigits 

% PREDICATE: relevantNumber(ListPossDigits,MaxNr,RelNr)
% ----------
%            the predicate relevantNumber as specified in Assignment 1.
%
% APPROACH: let NumLen iterate between 1 and the number of digits in MaxNr
%           and write a predicate relevantNumberWithLength/4 generates a relevant number of length NumbLen
%           Lines 14 and 15 serve to iterate over all possible NumLen values.
%           Next task: write nrDigits/2 and relevantNumberWithLength/4.
relevantNumber(ListPossDigits,MaxNr,RelNr) :-
	nrDigits(MaxNr,NrDigitsInMaxNr),
	between(1,NrDigitsInMaxNr,NrDigitsToGenerate),
	relevantNumberWithLength(ListPossDigits,MaxNr,NrDigitsToGenerate,RelNr).


% PREDICATE: nrDigits(Nr,NrDigits)
% ----------
%            given a positive integer Nr, NrDigits is the amount of digits in Nr
%
% APPROACH: (CASE 1) Nr is a number < 10, then NrDigits is 1
%           (CASE 2) Nr is a number >= 10, then NrDigits is 1 + the number of digits in Nr/10
% note: this solution does not use tail recursion, but that's ok (Nr wil never by very high, # of recursion steps is pretty low)
nrDigits(Nr,1) :- Nr < 10.
nrDigits(Nr,NrDigits) :- 
	Nr >= 10,
	NewNr is Nr // 10, % integer division
	nrDigits(NewNr,RecNrDigits),
	NrDigits is 1 + RecNrDigits.


% PREDICATE: relevantNumber(ListPossDigits,MaxNr,NrDigitsToGenerate,RelNr)
% ----------
%            Given the first three arguments, RelNr unifies with all possible
%            relevant numbers (see Assignment 1) whose number of digits is equal
%            to NrDigitsToGenerate.
%
% APPROACH: Given NrDigitsToGenerate (abbreviate with NRD), we construct the NRD-th digit our result number
%           So NRD = 3 with ListPossDigits = [1,3] means we "add" either 100 or 300 to our end solution.
%
%           If we want to use tail recursion, we'll need an accumulator (the constructed number so far). 
%           This is added as 5th argument and we initialise it as 0 in line 53.
relevantNumberWithLength(ListPossDigits,MaxNr,NrDigitsToGenerate,RelNr) :-
	relevantNumberWithLength(ListPossDigits,MaxNr,NrDigitsToGenerate,RelNr,0).
%           
%           (CASE 1): NrDigitsToGenerate = 0, then CurrNr is our completely constructed 
%                     relevant number and we can return it by unifying it with the return argument (4th one)
%                     if the number is smaller than MaxNr.
relevantNumberWithLength(_,MaxNr,0,CurrNr,CurrNr) :- CurrNr =< MaxNr.
%% Longer version (does the exact same thing, but more verbose):
%% relevantNumberWithLength(ListPossDigits,MaxNr,NrDigitsToGenerate,RelNr,CurrNr) :- 
%%     NrDigitsToGenerate = 0,
%%     RelNr = CurrNr,
%%     CurrNr =< MaxNr.
%
%           (CASE 2): NrDigitsToGenerate > 0, then construct N*10^(NRD-1) and add the recursive (decrease NRD) answer to it)
relevantNumberWithLength(ListPossDigits,MaxNr,NrDigitsToGenerate,RelNr,CurrNr) :- 
	NrDigitsToGenerate > 0,
	member(Digit,ListPossDigits),
	NewNr is CurrNr + (10 ** (NrDigitsToGenerate-1)) * Digit,
	NewNrDigitsToGenerate is NrDigitsToGenerate - 1,
	relevantNumberWithLength(ListPossDigits,MaxNr,NewNrDigitsToGenerate,RelNr,NewNr).



% Assignment 2
% Given ListPossNumbers (list of possible digits), TargetNr (target number),
% CurrDisplay (current number displayed on the calculator), generate the list
% of operations that is needed to get the target number on the display
%
%
% PREDICATE: calcul(ListPossNumbers,TargetNr,CurrDisplay,Operations)
% ----------
%            behaviour as depicted in Assignment 2.
%            
% APPROACH: (given in Hint:) 
%           step 1: if you've reached the target number then you can stop with Operations = []
%           step 2: randomly select a relevant number and an operation to perform
%           step 3: check whether is valid to perform
%           step 4: perform the operation and store the new current display
%           step 5: call recursively if the new display is smaller than or equal to the target number
%           
%           Step 1 means we have a base case (TargetNr = CurrDisplay)
%           Step 2 is translated the creation of 2 recursive cases; one where we do +, one where we do *
%           
%           
%           (CASE 1): Base case, TargetNr = CurrDisplay. Return with Operations = []
calcul(_,TargetNr,TargetNr,[]).
%           (CASE 2): Addition recursive case. Take note of how we recursively construct our
%                     operations to return: we're adding it in the head so that is returned here
%                     is the recursive answer with 2 new elements (+ and the relevant number) added in front
calcul(ListPossDigits,TargetNr,CurrDisplay,[+,RelNr|RestOp]) :-
	relevantNumber(ListPossDigits,TargetNr,RelNr),
	RelNr \== 0,
	NewDisplay is CurrDisplay + RelNr,
	NewDisplay =< TargetNr,
	calcul(ListPossDigits,TargetNr,NewDisplay,RestOp).
%           (CASE 3): Multiplication recursive case. 
%                     Very similar to the previous case: the only difference are the 
%                     requirements for an operation to be valid
calcul(ListPossDigits,TargetNr,CurrDisplay,[*,RelNr|RestOp]) :-
	relevantNumber(ListPossDigits,TargetNr,RelNr),
	CurrDisplay \== 0,
	RelNr > 1,
	NewDisplay is CurrDisplay * RelNr,
	NewDisplay =< TargetNr,
	calcul(ListPossDigits,TargetNr,NewDisplay,RestOp).
	


% Assignment 3
% Given ListPossNumbers (list of possible digits), TargetNr (target number),
% CurrDisplay (current number displayed on the calculator) and Cost (cost of the solution),
% generate a list of operations that is needed to get the target number on the display with exact cost.
% Cost is defined by 1 for each + or * sign and defined as N for integer INT with N the number of digits in INT.
%
%
% PREDICATE: calculCost(ListPossNumbers,TargetNr,CurrDisplay,Cost,Operations)
% ----------
%            behaviour as depicted in Assignment 3.
%
% APPROACH: use calcul/4 to generate possible Operations and filter out the ones that don't
%           adhere to the cost constraint
calculCost(ListPossDigits,TargetNr,CurrDisplay,Cost,Operations) :-
	calcul(ListPossDigits,TargetNr,CurrDisplay,Operations),
	operationsCost(Operations,Cost).

% PREDICATE: operationsCost(Operations,Cost)
% ----------
%            given a list of operations Operations, calculate it cost as described
%            in Assignment 3
%            
% APPROACH: do recursion on the list of operations, processing one element at a time + base case is Operations = [] and Cost = 0.
%           In order to do tail recursion we'd need an extra accumulator argument (current cost).
%           I will not write out a tail recursion solution. This is left as excercise for the reader.
operationsCost([],0).
operationsCost([+|RestOp],C) :-
	operationsCost(RestOp,RestC),
	C is RestC + 1.
operationsCost([*|RestOp],C) :-
	operationsCost(RestOp,RestC),
	C is RestC + 1.
operationsCost([Int|RestOp],C) :-
	Int \== *,
	Int \== +,
	nrDigits(Int,NrDigits),
	operationsCost(RestOp,RestC),
	C is RestC + NrDigits.

% Assignment 4
% Given ListPossNumbers (list of possible digits) and TargetNr (target number),
% generate lists of the operations that are needed to get the target number on the display with increasing cost.
% Display implicitly starts at 0 (see introduction text)
% Cost is as defined in Assignment 3
%
%
% PREDICATE: sortedCalcul(ListPossNumbers,TargetNr,Operations)
% ----------
%            behaviour as depicted in Assignment 4.
%            
% APPROACH: Generate all numbers from 1 to TargetNr*2 and call calculCost with that exact cost.
sortedCalcul(ListPossDigits,TargetNr,Operations) :-
	MaxCost is TargetNr * 2,
	between(1,MaxCost,Cost),
	calculCost(ListPossDigits,TargetNr,0,Cost,Operations).

% Assignment 5
% Given ListPossNumbers (list of possible digits), give the list of all numbers between
% 1 and 30 that cannot be produced on the calculator.
%
%
% PREDICATE: unreachable(ListPossNumbers,ListNrNotReachable)
% ----------
%            behaviour as depicted in Assignment 5.
%            
% APPROACH: Generate all numbers from 1 to 30 and filter out the numbers that are reachable.
%           i.e. FIND ALL numbers between 1 and 30 not reachable ...
unreachable(ListPossDigits,ListNrNotReachable) :-
	findall(UnreachableNr,(between(1,30,UnreachableNr), \+ calcul(ListPossDigits,UnreachableNr,0,_)), ListNrNotReachable).