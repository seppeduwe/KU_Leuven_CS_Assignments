% Input predicate: guests
%     guest(guestID).
guest(stephen).
guest(sandra).
guest(bruno).

guest_carry_capacity(stephen,1).
guest_carry_capacity(sandra,1).
guest_carry_capacity(bruno,2).

% Input predicate: what guests drink
%     drinks(guestID,drinkID).
drinks(stephen,tequila_sunrise).
drinks(stephen,long_island_ice_tea).
drinks(stephen,bloody_mary).

drinks(sandra,bloody_mary).
drinks(sandra,mojito).

drinks(bruno,tequila_sunrise).
drinks(bruno,long_island_ice_tea).
drinks(bruno,margarita).

% Input predicate: how much drinks each guest want (min).
%     drinks_minimum(guestID,number).
drinks_minimum(stephen,1).
drinks_minimum(sandra,1).
drinks_minimum(bruno,1).

% Input predicate: how much drinks each guest can tolerate (max).
%     drinks_maximum(guestID,number).
drinks_maximum(stephen,2).
drinks_maximum(sandra,2).
drinks_maximum(bruno,2).

% Input predicate: guests disliking eachother
%     dislikes(guestID, guestID)
dislikes(bruno,stephen).

% Input predicate: ingredients
%     ingredient(ingredientID).
ingredient(mojito,limes).
ingredient(mojito,mint).
ingredient(mojito,rum).

ingredient(tequila_sunrise,grenadine).
ingredient(tequila_sunrise,limes).
ingredient(tequila_sunrise,tequila).

ingredient(margarita,limes).
ingredient(margarita,tequila).

ingredient(bloody_mary,vodka).
ingredient(bloody_mary,tomatoes).
ingredient(bloody_mary,limes).

ingredient(long_island_ice_tea,vodka).
ingredient(long_island_ice_tea,rum).
ingredient(long_island_ice_tea,tequila).
ingredient(long_island_ice_tea,limes).

% Input predicate: stores
%     store(storeID).

store(delhaize).
store(carrefour).
store(aldi).

% Input predicate: stores selling ingredients
%     sells(storeID, ingredientID).
sells(delhaize,rum).
sells(delhaize,limes).
sells(delhaize,mint).
sells(delhaize,vodka).


sells(carrefour,rum).
sells(carrefour,limes).
sells(carrefour,grenadine).
sells(carrefour,tequila).


sells(aldi,tomatoes).