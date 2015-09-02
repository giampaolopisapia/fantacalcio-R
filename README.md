# fantacalcio-R
Set of R functions to manage a fantacalcio.

A project that collects a set of functions useful to manage a fantacalcio and analyze relevant data.

Up to now you can handle the market ("asta"), loading the "Gazzetta dello Sport" list of players with their value from the csv file available in the project, recording each player purchase, seeing a table view with all team purchases, checking the remaining credits, and so on. 

You can also see the list of players, ordered by their value, and filtered with the already purchased ones.

The idea is to discover some key factor to make a winner team by analyzing the right data.

```
> asta <- addAcquisto(asta, "Team A", "DE SANC", 120)
Team A 
  1560 
> asta
  Fantasquadra Ruolo  Calciatore  Squadra Prezzo Quota Prog
1       Team A     P      BUFFON JUVENTUS    320    17    1
2       Team B     P    SZCZESNY     ROMA    320    16    1
3       Team C     P DIEGO LOPEZ    MILAN    320    13    1
4       Team A     P  DE SANCTIS     ROMA    120     3    2
> viewTabellone(asta)
  Ruolo           Team A         Team B            Team C
1     P     BUFFON - 320 SZCZESNY - 320 DIEGO LOPEZ - 320
2     P DE SANCTIS - 120           <NA>              <NA>
3     P              440            320               320
4  <NA>              440            320               320
> bestQuote(q, ruolo="P", 5, asta)
    Calciatore Ruolo   Squadra Quota
240 HANDANOVIC     P     INTER    15
55    BIZZARRI     P    CHIEVO    14
418      PERIN     P     GENOA    14
489 SORRENTINO     P   PALERMO    14
530    VIVIANO     P SAMPDORIA    14
```

