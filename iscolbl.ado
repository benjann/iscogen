*! version 1.0.0  04jul2019  Ben Jann

program iscolbl
    version 13
    gettoken lbl 0 : 0, parse(", ")
    if `"`lbl'"'=="" {
        di as err "{it:lbl} required"
        exit 198
    }
    syntax [ varlist(numeric default=none) ] [, * ]
    iscogen, lbl(`lbl') makelabels(`varlist') `options'
    di as txt _n "variable name" _col(34) "label name"
    di as txt "{hline 65}"
    local lblnames `"`r(lblnames)'"'
    local varlist `"`r(varlist)'"'
    foreach v of local varlist {
        if `"`lblnames'"'=="" {
            local lblnames `"`r(lblnames)'"'
        }
        gettoken lblname lblnames : lblnames
        di as res "`v'" _col(34) "`lblname'"
    }
    if `"`lblnames'"'!="" {
        di as txt "(not specified)" as res _col(34) "`lblnames'"
    }
end
