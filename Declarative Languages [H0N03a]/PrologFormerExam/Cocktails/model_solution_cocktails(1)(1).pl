:- use_module(library(lists)).

% Name and Surname:
% Course (3bach/schakel/cw):
% Student Number:


% Construct shopping lists (and select a store), such that all guests needs are 
% satisfied
shopping_lists(DO,X) :-
    guestList(GL),
    drink_order(DO),
    all_needed_ingredients(DO,I),
    shopping_list_rec(DO,X,GL,I,[]).

drink_order(DO) :-
    basic_drink_order(DO),
    dislikes_orders_check(DO),
    carry_capacity_check(DO).

shopping_list_rec(_,Ret,_,[], Ret).
shopping_list_rec(DO,Ret,[Guest|R],I, CurrLists) :-
    storeList(SL),
    member(S,SL),
    visit_store(DO,Guest,S,I,GuestShopList,NewIngredients),
    shopping_list_rec(DO,Ret,R,NewIngredients,[GuestShopList|CurrLists]).

visit_store(DO,Guest,Store,Ilist,shopat(Guest,Store,BuyList),IReduced) :-
    buyable_ingredients_at_shop(Guest,Store,Ilist,DO,BuyableSet),
    guest_carry_capacity(Guest,CC),
    select_items_to_buy(BuyableSet,CC,BuyList),
    buy_elements(Ilist,BuyList,IReduced).

buyable_ingredients_at_shop(Guest,Store,Ilist,DO,BuyableSet) :-
    findall(I, (member(I,Ilist), sells(Store,I), dislike_shop_check(Guest,I,DO)), BuyableList),
    list_to_set(BuyableList,BuyableSet).

guestList(GL) :-
    findall(X, guest(X), GL).

storeList(SL) :-
    findall(X, store(X), SL).

select_items_to_buy(List,MaxSize,List) :-
    length(List,LL),
    LL =< MaxSize, !.
select_items_to_buy(List,MaxSize,NewList) :-
    select(_,List,TmpList),
    select_items_to_buy(TmpList,MaxSize,NewList).

buy_elements(InList, [], InList).
buy_elements([X|R], [X|R2], Ret) :-
    buy_elements(R,R2,Ret).
buy_elements([X|R], [Y|R2], Ret) :-
    X \== Y,
    buy_elements(R,[Y],NewRest),!,
    buy_elements([X|NewRest],R2,Ret).

basic_drink_order(DO) :-
    guestList(GL),
    basic_drink_order(DO,GL,[]).

basic_drink_order(DO,[],DO).
basic_drink_order(DO,[Guest|R],CurrOrders) :-
    drinks_minimum(Guest,MinNr),
    drinks_maximum(Guest,MaxNr),
    get_needed_drinks(Guest,Drinks,0,[],MinNr,MaxNr,CurrOrders),
    basic_drink_order(DO,R,[order(Guest,Drinks)|CurrOrders]).
    
get_needed_drinks(Guest,Drinks,CurrentDrinksForGuest,Drinks,MinNr,MaxNr,CurrOrders) :-
    CurrentDrinksForGuest >= MinNr,
    CurrentDrinksForGuest =< MaxNr,
    dislikecheck(Guest,Drinks,CurrOrders).
get_needed_drinks(Guest,Drinks,CurrentDrinksForGuest,Acc,MinNr,MaxNr,CurrOrders) :-
    CurrentDrinksForGuest =< MaxNr,
    drinks(Guest,Drink),
    NewNrDrinks is CurrentDrinksForGuest + 1,
    get_needed_drinks(Guest,Drinks,NewNrDrinks,[Drink|Acc],MinNr,MaxNr,CurrOrders).

all_needed_ingredients(Orders,Ingredients) :-
    all_needed_ingredients_rec(Orders,Ingredients,[]).

all_needed_ingredients_rec([],SetRet,Ret) :- list_to_set(Ret,SetRet).
all_needed_ingredients_rec([order(_,Drinks)|R],Ret,CurrIngr) :-
    findall(I,(member(Drink,Drinks), ingredient(Drink,I)),IngredientsForDrink),
    append(CurrIngr,IngredientsForDrink,NewIngredients),
    all_needed_ingredients_rec(R,Ret,NewIngredients).

dislikes_orders_check(DO) :-
    findall(((G1,Drinks1),(G2,Drinks2)),(member(order(G1,Drinks1),DO), member(order(G2,Drinks2),DO), dislikes(G1,G2), length(Drinks1,L1), length(Drinks2,L2), L1 =< L2), ViolList),
    length(ViolList,0).

carry_capacity_check(DO) :-
    guestList(GL),
    findall(Cc, (member(G,GL), guest_carry_capacity(G,Cc)), CCL),
    sumlist(CCL,Total_CC),
    all_needed_ingredients(DO,Ingredients),
    length(Ingredients,Total_CC_needed),
    Total_CC >= Total_CC_needed.

dislikecheck(Guest,Drinks,CurrOrders) :-
    length(Drinks,DL),
    findall(DislikedDrinkLists,(
        dislikes(Guest,G2), 
        member(order(G2,DislikedDrinkLists),CurrOrders),
        length(DislikedDrinkLists,LDDL),
        DL =< LDDL),[]).

dislike_shop_check(Guest,Ingredient,DO) :-
    needs_ingredient(Guest,Ingredient,DO),
    !.

dislike_shop_check(Guest,Ingredient,DO) :-
    findall(G2,(dislikes(Guest,G2),needs_ingredient(G2,Ingredient,DO)),[]).
        
needs_ingredient(Guest,Ingredient,DO) :-
    member(order(Guest,Cocktails),DO),
    member(Cocktail,Cocktails),
    ingredient(Cocktail,Ingredient).