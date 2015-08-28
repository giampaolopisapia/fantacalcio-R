# fantacalcio-R
R set of functions to manage a fantacalcio

A project that collects a set of functions useful to manage a fantacalcio and analyze relevant data.

For example you can load the list of players with their value from csv file (previously downloaded fromweb and adjusted).

Then you can record the market ("asta"), by recording each player purchase, seeing a table view with all team purchases, checking the remaining credits, and so on. 

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
```

