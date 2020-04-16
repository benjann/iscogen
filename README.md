# iscogen
Stata module to translate ISCO codes

`iscogen` is a Stata command to translate ISCO codes (International
Standard Classification of Occupations). Various transformations such as 
recoding between ISCO-08, ISCO-88, and ISCO-68 or generating prestige scores 
and occupational classes are supported. A command to label ISCO codes is
also provided (`iscolbl`).

`iscogen` uses an indexing approach to translate the ISCO codes and is thus
fast also in very large datasets.

To install the `iscogen` package, type

    . ssc install iscogen, replace

in Stata. Stata version 13 or newer is required.

---

Installation from GitHub:

    . net install mindex, from(https://raw.githubusercontent.com/benjann/iscogen/master/)

---

Main changes:

    16apr2020
    - added an ISCO-88 to ISCO-88(COM) translator
    
    12apr2020
    - added column 'seno' to esec translators even though it is identical to se<10
    - esec for ISCO-88 now applies the simplified ESEC if sempl is missing
    - improved description of treatment of sempl and supvis in help file
    
    10apr2020
    - ISCO-88 -> esec added
    - ISCO-08 -> esec now also covers ISCO minor groups 960, 961, and 962
    
    26jul2019
    - egp11() added
    - ISCO-88 -> MPS added
