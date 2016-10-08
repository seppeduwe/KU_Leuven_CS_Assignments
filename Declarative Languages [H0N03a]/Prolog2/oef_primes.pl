% The implementation of all_primes/2.
% all_primes(N,L): L is the list of prime numbers smaller than N.
% The first argument needs to be instantiated.
all_primes(UpperBound,AllPrimes) :-
    numlist(2,UpperBound,Candidates),
    Limit = sqrt(UpperBound),
    eratosthenes([],Candidates,Limit,AllPrimes).

remove_multiples([],_,[]).
remove_multiples([X|Xs],Candidate,Result) :-
    (
        0 is X mod Candidate
    ->
        Result = NResult
    ;
        Result = [X|NResult]
    ),
    remove_multiples(Xs,Candidate,NResult).

eratosthenes(PartialResult,[],_,Result) :-
    reverse(PartialResult,Result).
eratosthenes(PartialResult,[Candidate|Candidates],Limit,Result) :-
    (
        Candidate >= Limit
    ->
        reverse(PartialResult,RPResult),
        append(RPResult,[Candidate|Candidates],Result)
    ;
        remove_multiples(Candidates,Candidate,NCandidates),
        eratosthenes([Candidate|PartialResult],NCandidates,Limit,Result)
    ).

% An alternative, less efficient implementation of all_primes/2.
% all_primes2(N,L): L is the list of prime numbers smaller than N.
% The first argument needs to be instantiated.
all_primes2(UpperBound,AllPrimes) :-
    (
        UpperBound == 2
    ->
        AllPrimes = [2]
    ;
        Limit is sqrt(UpperBound),
        (
            prime2(2,UpperBound,Limit)
        ->
            AllPrimes = [UpperBound|NAllPrimes]
        ;
            AllPrimes = NAllPrimes
        ),
        NUpperBound is UpperBound - 1,
        all_primes2(NUpperBound,NAllPrimes)
    ).

prime2(Current,UpperBound,Limit) :-
    (
        Current > Limit
    ->
        true
    ;
        Rest is UpperBound mod Current,
        Rest =\= 0,
        NCurrent is Current + 1,
        prime2(NCurrent,UpperBound,Limit)
    ).
