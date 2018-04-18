function [uitvoer1, uitvoer2] = doeietsmindernuttigs(invoer1, invoer2, invoer3)
    uitvoer1 = invoer1;
    uitvoer1 = uitvoer1+invoer2;
    uitvoer1 = uitvoer1+invoer3;
    
    uitvoer2 = invoer1;
    uitvoer2 = uitvoer2*invoer2;
    uitvoer2 = uitvoer2*invoer3;
end