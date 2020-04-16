*! version 1.0.5  16apr2020  Ben Jann

program iscogen
    version 13
    if replay() {
        Label `0'
        exit
    }
    Recode `0'
end

program Label, rclass
    syntax, lbl(name) [ makelabels(str) name(name) MAJor SUBmajor MINor MODify MINImal ]
    if `"`makelabels'"'=="" {
        if "`minimal'"!="" {
            di as err "{bf:minimal} only allowed if {it:varlist} is specified"
            exit 198
        }
        if "`name'"=="" local name "`lbl'"
    }
    local select `major' `submajor' `minor'
    if `: list sizeof select'>1 {
        di as err "only one of {bf:major}, {bf:submajor}, and {bf:minor} allowed"
        exit 198
    }
    local LBL = strlower("`lbl'")
    local 0 ", `LBL'"
    capt syntax [, isco08 isco88 isco88com isco88a isco88b isco68 egp egp11 esec oesch oesch8 oesch5 ]
    if _rc==1 exit 1 // user hit -break-
    else if _rc {
        di as err `"{bf:`lbl'} not allowed"'
        exit 198
    }
    if "`select'"!="" {
        if !inlist(substr("`LBL'",1,6), "isco08", "isco88") {
            di as err "option {bf:`select'} not allowed with {bf:`lbl'}"
            exit 198
        }
    }
    mata: define_labels("`name'", tokens(st_local("makelabels")), ///
        "`LBL'", "`select'", "`modify'"!="", "`minimal'"!="") // returns local lblnames
    return local lblnames "`lblnames'"
    return local varlist "`makelabels'"
end

program Recode, rclass
    // syntax: iscogen newvar = function(varlist) [if] [in] [, options]
    // - parse "newvar"
    gettoken generate rest : 0, parse("=(, ")
    capt n confirm name `generate'
    if _rc==1 exit 1 // user hit -break-
    else if _rc exit 198
    // - parse "function" from "= function(varlist)"
    gettoken to rest : rest, parse("=")
    if `"`to'"'!="=" error 198
    gettoken to rest : rest, parse("(")
    parse_to `to'
    local labelok 1
    if      "`to'"=="isei"   local labelok
    else if "`to'"=="siops"  local labelok
    else if "`to'"=="mps"    local labelok
    // - parse "varlist" from "= function(varlist)"
    gettoken par rest : rest, parse("(")
    if `"`par'"'!="(" error 198
    gettoken varlist rest : rest, parse("),")
    gettoken par rest : rest, parse("),")
    if `"`par'"'!=")" error 198
    local 0 `"`varlist'"'
    syntax varlist(min=1 max=3)
    gettoken isco   sempl  : varlist
    gettoken sempl  supvis : sempl
    gettoken supvis        : supvis

    // - parse "[if] [in] [, options]"
    local 0 `"`rest'"'
    syntax [if] [in] [, From(str) String Replace noLabel INValid EMissing ]
    if "`string'"!="" local label nolabel
    if "`replace'"=="" confirm new variable `generate'
    local label0 `label'
    if "`labelok'"=="" local label nolabel
    parse_from `from'
    if ("`from'"=="`to'") {
        di as error "{bf:`to'()} not allowed with {bf:from(`from')}"
        exit 198
    }
    if inlist("`to'","major","submajor","minor") {
        if !inlist("`from'","isco88","isco08") {
            di as err "{bf:`to'())} not supported with {bf:from(`from')}"
            exit 198
        }
    }
    if "`emissing'"!="" {
        if "`string'"!="" local emissing
        else if substr("`: type ``isco''",1,3)=="str" local emissing
    }
    if inlist(substr("`to'",1,5), "egp", "egp11", "esec", "oesch") {
        if "`to'"=="esec" & "`from'"=="isco88" {
            if "`sempl'"=="" di as txt "({it:sempl} not provided; applying simplified ESEC)"
            else {
                confirm numeric variable `sempl'
                if "`supvis'"=="" di as txt "({it:supvis} not provided; assumed 0)"
                else confirm numeric variable `supvis'
            }
        }
        else {
            if "`sempl'"==""  di as txt "({it:sempl} not provided; assumed 0)"
            else confirm numeric variable `sempl'
            if "`supvis'"=="" di as txt "({it:supvis} not provided; assumed 0)"
            else confirm numeric variable `supvis'
        }
    }
    else {
        if "`sempl'"!="" {
            di as err "{it:sempl} not allowed with {bf:`to'()}"
            exit 198
        }
        if "`supvis'"!="" {
            di as err "{it:supvis} not allowed with {bf:`to'()}"
            exit 198
        }
    }
    
    // mark sample
    marksample touse, novarlist
    qui count if `touse' & missing(`isco')
    local N_missing = r(N)
    if `N_missing'==0 local emissing
    if "`emissing'"!="" {
        // preserve touse
        tempvar touse0
        qui gen byte `touse0' = `touse'
        // collect labels of extended missing values
        if "`label0'"=="" {
            local emvallab: value label `isco'
            if "`emvallab'"!="" {
                qui levelsof `isco' if `isco'>. & `touse0', missing local(emvals)
            }
        }
    }
    markout `touse' `isco', strok
    
    // translate
    tempvar vnew
    if inlist("`to'", "major", "submajor", "minor") {
        _truncate `from' `to' `isco' `vnew' `touse' "`string'" "`label'"
    }
    else {
        _recode `from' `to' `isco' `vnew' `touse' "`string'" "`label'" "`sempl'" "`supvis'"
    }
    
    // carry forward extended missing values
    if "`emissing'"!="" {
        qui replace `vnew' = `isco' if `isco'>. & `touse0'
        if "`emvallab'"!="" {
            foreach emval of local emvals {
                local emlbl: label `emvallab' `emval', strict
                if `"`emlbl'"'!="" {
                    label define `vnew' `emval' `"`emlbl'"', nofix add
                    local label
                }
            }
        }
    }
    
    // count matched/unmatched observations
    qui count if `touse'
    local N_matched = r(N)
    qui count if missing(`vnew') & `touse'
    local N_unmatched = r(N)
    local N_matched = `N_matched' - r(N)
    
    // get list of invalid (unmatched) codes
    if "`invalid'"!="" {
        if r(N) {
            qui levelsof `isco' if missing(`vnew') & `touse', local(invalid_codes)
            local N_invalid = r(r)
            local invalid_codes: list clean invalid_codes
        }
        else {
            local N_invalid = 0
            local invalid_codes ""
        }
    }
    
    // assign variable name and labels
    capt confirm new variable `generate'
    if _rc==1 exit 1 // user hit -break-
    else if _rc drop `generate'
    rename `vnew' `generate'
    lab var `generate' "Recode of `isco': `from' to `to'"
    if "`label'"=="" {
        label copy `vnew' `generate', replace
        label drop `vnew'
        label values `generate' `generate', nofix
    }
    
    // display
    di
    di as txt _col(5) "variable name:"   _col(29) as res `"`generate'"'
    di as txt _col(5) "variable label:"  _col(29) as res `"`:var lab `generate''"'
    di as txt _col(5) "storage type:"    _col(29) as res `"`:type `generate''"'
    di as txt _col(5) "matched observations:"   _col(29) as res `N_matched' 
    di as txt _col(5) "unmatched observations:" _col(29) as res `N_unmatched'
    di as txt _col(5) "missing observations:"   _col(29) as res `N_missing' 
    if "`invalid'"!="" {
        if `N_invalid'==0 {
            di as txt _col(5) "(no invalid codes)"
        }
        else {
            if `N_invalid'==1 local tmp "invalid code:"
            else              local tmp "invalid codes:"
            di as txt _col(5) as res `N_invalid' as txt " `tmp'" _c
            di _col(29) as res `"`invalid_codes'"'
        }
    }
    
    // returns
    return scalar string = ("`string'"!="")
    return scalar labeled = ("`label'"=="")
    if "`invalid'"!="" {
        return scalar N_invalid = `N_invalid'
        return local invalid `"`invalid_codes'"'
    }
    return scalar N_missing = `N_missing'
    return scalar N_unmatched = `N_unmatched'
    return scalar N_matched = `N_matched'
    return scalar N = `N_missing' + `N_matched' + `N_unmatched'
    return local to "`to'"
    return local from "`from'"
    return local supvis "`supvis'"
    return local sempl "`sempl'"
    return local isco "`isco'"
    return local varname "`generate'"
end

program parse_to
    if `: list sizeof 0'>1 {
        di as err `"{bf:`0'()} not allowed"'
        exit 198
    }
    local to0 `"`0'"'
    local 0 = strlower(`"`0'"')
    local 0 `", `0'"'
    capt syntax [, isco08 isco88 isco88com isco68 MAJor SUBmajor MINor ///
        egp egp11 isei siops TREIman ESEC oesch oesch8 oesch5 mps ]
    if _rc==1 exit 1 // user hit -break-
    else if _rc {
        di as err `"{bf:`to0'()} not allowed"'
        exit 198
    }
    if "`treiman'"!="" local siops siops
    c_local to `isco08' `isco88' `isco88com' `isco68' `major' `submajor' `minor' ///
        `egp' `egp11' `isei' `siops' `esec' `oesch' `oesch8' `oesch5' `mps'
end

program parse_from
    if `"`0'"'=="" {
        c_local from isco08     // default is isco08
        exit
    }
    if `: list sizeof 0'>1 {
        di as err `"'`0'' not allowed in {bf:from()}"'
        exit 198
    }
    local FROM isco08 isco88 isco68
    if `: list 0 in FROM'==0 {
        di as err `"'`0'' not allowed in {bf:from()}"'
        exit 198
    }
    c_local from `0'
end

program _recode
    args from to vfrom vto touse string label sempl supvis

    // if EGP, ESEC, OESCH: determine relevant column of codelist
    local to0 "`to'"
    if substr("`to'",1,5)=="oesch" local to "oesch"
    local ncols = 1 + 1
    if "`sempl'"!="" {
        qui count if `sempl'>=. & `touse'
        if r(N) {
            if "`to'"=="esec" & "`from'"=="isco88" {
                di as txt "({cmd:`sempl'} has missings; applying simplified ESEC to corresponding observations)"
            }
            else {
                di as txt "({cmd:`sempl'} has missings; assumed 0)"
            }
        }
        if "`supvis'"!="" {
            qui count if `supvis'>=. & `touse'
            if r(N) di as txt "({cmd:`supvis'} has missings; assumed 0)"
            qui count if `supvis'<0 & `touse'
            if r(N) di as txt "({cmd:`supvis'} has negative values; assumed 0)"
        }
    }
    if "`to'"=="esec" & "`from'"=="isco88" {
        local ncols = 1 + 6
        tempvar COL
        qui gen byte `COL' = 6 if `touse' // simplified ESEC
        if "`sempl'"!="" {
            qui replace `COL' = 1 if (`sempl'==0) & `touse' // employees
            qui replace `COL' = 3 if (`sempl'!=0) & (`sempl'<.) & `touse' // selfemp
            if "`supvis'"!="" {
                qui replace `COL' = 2 if (`supvis'>=1)  & (`supvis'<.)  & (`COL'==1) & `touse'
                qui replace `COL' = 4 if (`supvis'>=1)  & (`supvis'<10) & (`COL'==3) & `touse'
                qui replace `COL' = 5 if (`supvis'>=10) & (`supvis'<.)  & (`COL'==3) & `touse'
            }
        }
    }
    else if "`to'"=="esec" {
        local ncols = 1 + 5
        tempvar COL
        qui gen byte `COL' = 1 if `touse'   // employees
        if "`sempl'"!="" {
            qui replace `COL' = 3 if (`sempl'!=0) & (`sempl'<.) & `touse' // selfemp
            if "`supvis'"!="" {
                qui replace `COL' = 2 if (`supvis'>=1)  & (`supvis'<.)  & (`COL'==1) & `touse'
                qui replace `COL' = 4 if (`supvis'>=1)  & (`supvis'<10) & (`COL'==3) & `touse'
                qui replace `COL' = 5 if (`supvis'>=10) & (`supvis'<.)  & (`COL'==3) & `touse'
            }
        }
    }
    else if "`to'"=="oesch" {
        local ncols = 1 + 4
        tempvar COL
        qui gen byte `COL' = 1 if `touse'
        if "`sempl'"!="" {
            qui replace `COL' = 2 if (`sempl'!=0) & (`sempl'<.) & `touse'
            if "`supvis'"!="" {
                qui replace `COL' = 3 + (`supvis'>=10) if (`supvis'>=1) & (`supvis'<.) & (`COL'==2) & `touse'
            }
        }
    }
    else if inlist("`to'", "egp", "egp11") & "`from'"=="isco68" {
        local ncols = 1 + 6
        tempvar COL
        qui gen byte `COL' = 1 if `touse'
        if "`sempl'"!="" {
            qui replace `COL' = 4 if (`sempl'!=0) & (`sempl'<.) & `touse'
            if "`supvis'"!="" {
                qui replace `COL' = 2 if (`supvis'>=1)  & (`supvis'<10) & (`COL'==1) & `touse'
                qui replace `COL' = 3 if (`supvis'>=10) & (`supvis'<.)  & (`COL'==1) & `touse'
                qui replace `COL' = 5 if (`supvis'>=1)  & (`supvis'<10) & (`COL'==4) & `touse'
                qui replace `COL' = 6 if (`supvis'>=10) & (`supvis'<.)  & (`COL'==4) & `touse'
            }
        }
    }
    else if inlist("`to'", "egp", "egp11") & "`from'"=="isco88" {
        local ncols = 1 + 8
        tempvar COL
        qui gen byte `COL' = 1 if `touse'
        if "`sempl'"!="" {
            qui replace `COL' = 5 if (`sempl'!=0) & (`sempl'<.) & `touse'
            if "`supvis'"!="" {
                qui replace `COL' = 2 if (`supvis'>=1)  & (`supvis'<2)  & (`COL'==1) & `touse'
                qui replace `COL' = 3 if (`supvis'>=2)  & (`supvis'<10) & (`COL'==1) & `touse'
                qui replace `COL' = 4 if (`supvis'>=10) & (`supvis'<.)  & (`COL'==1) & `touse'
                qui replace `COL' = 6 if (`supvis'>=1)  & (`supvis'<2)  & (`COL'==5) & `touse'
                qui replace `COL' = 7 if (`supvis'>=2)  & (`supvis'<10) & (`COL'==5) & `touse'
                qui replace `COL' = 8 if (`supvis'>=10) & (`supvis'<.)  & (`COL'==5) & `touse'
            }
        }
    }
    
    // apply mapping
    mata: merge_codelist("`from'", "`to0'", st_varindex("`vfrom'"), "`vto'", ///
        "`string'"!="", `ncols', _st_varindex("`COL'"), st_varindex("`touse'"))
    if "`string'"=="" qui compress `vto'
    
    // define labels
    if "`label'"=="" {
        mata: set_labels("`vto'", "`to0'")
    }
end
program Merge, sortpreserve
    merge `0'
end

program _truncate
    args from to vfrom vto touse string label
    
    // generate real if input is string
    if substr("`: type `vfrom''",1,3)=="str" {
        tempvar tmp
        qui gen double `tmp' = real(`vfrom') if `touse'
        local vfrom `tmp'
    }
    
    // truncate
    if      "`to'"=="major"    local digits 1
    else if "`to'"=="submajor" local digits 2
    else if "`to'"=="minor"    local digits 3
    else                       local digits 4
    local divide = 10^(4 - `digits')
    if "`string'"=="" {
        qui gen double `vto' = trunc(`vfrom'/`divide') ///
            if `vfrom'>=0 & `vfrom'<10000 & `touse'
        qui compress `vto'
    }
    else {
        qui gen str `vto' = string(trunc(`vfrom'/`divide')) ///
            if `vfrom'>=0 & `vfrom'<10000 & `touse'
    }
    
    // define labels
    if "`label'"=="" {
        mata: set_labels("`vto'", "`from'", "`to'")
    }
end

version 11
mata:
mata set matastrict on

void merge_codelist(string scalar from, string scalar to, 
    real scalar vfrom, string scalar vto, real scalar tostr, 
    real scalar ncols, real scalar col, real scalar touse)
{
    real scalar            offset, n
    real rowvector         minmax
    real colvector         isco, p
    transmorphic colvector res
    transmorphic matrix    codes
    pragma unset isco
    pragma unset codes
    
    // get mapping info and put into matrix indexed by isco
    read_codelist(isco, codes, from, 0, to, tostr, ncols)
    minmax = minmax(isco)
    offset = minmax[1] - 1; n = minmax[2] - minmax[1] + 1
    isco = isco :- offset // so that isco in {1,...,n}
    codes = expand_matrix(n, isco, codes)
    
    // map codes
    isco = (st_isstrvar(vfrom)
            ? strtoreal(st_sdata(., vfrom, touse))
            : st_data(., vfrom, touse))
    if (to=="esec") isco = isco/10 // ESEC is defined at 3-digit level
    isco = trunc(isco) :- offset
    res  = J(rows(isco), 1, missingof(codes))
    p = selectindex(isco:>=1 :& isco:<=n :& isco:==trunc(isco))
    if (col<.) res[p] = select_cols(codes[isco[p],], st_data(., col, touse)[p])
    else       res[p] = codes[isco[p]]
    if (tostr) st_sstore(., st_addvar(max(strlen(res)), vto), touse, res)
    else       st_store(., st_addvar("double", vto), touse, res)
}

transmorphic matrix expand_matrix(real scalar n, real colvector index, 
    transmorphic matrix M0)
{
    transmorphic matrix M
    
    M = J(n, cols(M0), missingof(M0))
    M[index,] = M0
    return(M)
}

transmorphic colvector select_cols(transmorphic matrix M, real colvector cols)
{
    real scalar            i
    transmorphic colvector C
    
    C = J(rows(cols), 1, missingof(M))
    for (i=rows(cols); i; i--) C[i] = M[i, cols[i]]  // is there a faster way to do this?
    return(C)
}

void read_codelist(transmorphic colvector isco, transmorphic matrix codes, 
    string scalar from, real scalar fromstr, 
    string scalar to0, real scalar tostr, real scalar ncols)
{
    string scalar to
    
    to = (substr(to0,1,5)=="oesch" ? "oesch" : to0)
    codes = read_list("% CODELIST-" + from + "-" + to, ncols)
    isco = (fromstr ? codes[,1] : strtoreal(codes[,1]))
    codes = codes[|1,2 \ .,.|]
    if (to=="oesch") {
        if (to0=="oesch8") {
            _editvalue(codes, "2","1")
            _editvalue(codes, "3","2")
            _editvalue(codes, "4","2")
            _editvalue(codes, "5","3")
            _editvalue(codes, "6","3")
            _editvalue(codes, "7","4")
            _editvalue(codes, "8","4")
            _editvalue(codes, "9","5")
            _editvalue(codes,"10","5")
            _editvalue(codes,"11","6")
            _editvalue(codes,"12","6")
            _editvalue(codes,"13","7")
            _editvalue(codes,"14","7")
            _editvalue(codes,"15","8")
            _editvalue(codes,"16","8")
        }
        else if (to0=="oesch5") {
            _editvalue(codes, "2","1")
            _editvalue(codes, "3","3")
            _editvalue(codes, "4","3")
            _editvalue(codes, "5","1")
            _editvalue(codes, "6","2")
            _editvalue(codes, "7","4")
            _editvalue(codes, "8","5")
            _editvalue(codes, "9","1")
            _editvalue(codes,"10","2")
            _editvalue(codes,"11","4")
            _editvalue(codes,"12","5")
            _editvalue(codes,"13","1")
            _editvalue(codes,"14","2")
            _editvalue(codes,"15","4")
            _editvalue(codes,"16","5")
        }
    }
    if (tostr) _editvalue(codes, ".", "") // replace missings
    else       codes = strtoreal(codes)
}

void define_labels(string scalar lblname, string rowvector vnames,
    string scalar list, string scalar select, real scalar modify,
    real scalar minimal)
{
    real scalar      i, n
    real colvector   values
    real colvector   p
    string colvector labels
    pragma unset values
    pragma unset labels
    
    // get label definitions
    read_labellist(values, labels, list, select)
    
    // case 1: no variables specified
    n = length(vnames)
    if (n==0) {
        if (modify==0) {
            if (st_vlexists(lblname)) st_vldrop(lblname)
        }
        st_vlmodify(lblname, values, labels)
        st_local("lblnames", lblname)
        return
    }
    // case 2: variables specified, but no lblname
    else if (lblname=="") {
        for (i=1; i<=n; i++) {
            if (modify==0) {
                if (st_vlexists(vnames[i])) st_vldrop(vnames[i])
            }
            if (minimal) {
                p = selectindex_labels(values, vnames[i])
                st_vlmodify(vnames[i], values[p], labels[p])
            }
            else st_vlmodify(vnames[i], values, labels)
            st_varvaluelabel(vnames[i], vnames[i])
        }
        st_local("lblnames", invtokens(vnames))
        return
    }
    // case 3: both specified (use same set for all variables)
    if (minimal) {
        p = selectindex_labels(values, vnames)
        values = values[p]
        labels = labels[p]
    }
    if (modify==0) {
        if (st_vlexists(lblname)) st_vldrop(lblname)
    }
    st_vlmodify(lblname, values, labels)
    for (i=1; i<=n; i++) {
        st_varvaluelabel(vnames[i], lblname)
    }
    st_local("lblnames", lblname)
}

real colvector selectindex_labels(real colvector values, string rowvector vnames)
{
    real scalar    i, j, jj, r, v, offset, n
    real rowvector minmax
    real colvector idx, p
    
    // set up permutation vector to select relevant values
    r = rows(values)
    minmax = minmax(values)
    offset = minmax[1] - 1; n = minmax[2] - minmax[1] + 1
    idx = values :- offset // so that idx in {1,...,n}
    p = J(n, 1, .)
    p[idx] = J(r, 1, 0)
    
    // go through data and mark existing values
    for (j=length(vnames); j; j--) {
        jj = st_varindex(vnames[j])
        for (i=st_nobs(); i; i--) {
            v = _st_data(i,jj) - offset
            if (v<1) continue
            if (v>n) continue
            if (v!=trunc(v)) continue
            if (p[v]!=0) continue
            p[v] = 1
            r--
            if (!r) break // done (all values exist in data)
        }
        if (!r) break
    }
    return(selectindex(p[idx]))
}

void set_labels(string scalar lblname, string scalar list,
    | string scalar select)
{
    real colvector   values
    string colvector labels
    pragma unset values
    pragma unset labels
    
    read_labellist(values, labels, list, select)
    if (st_vlexists(lblname)) st_vldrop(lblname)
    st_vlmodify(lblname, values, labels)
}

void read_labellist(real colvector values, string colvector labels, 
    string scalar list, | string scalar select)
{
    real scalar    i, n
    string scalar  s
    string matrix  S
    real colvector p
    
    // get labellist
    S = read_list("% LABELLIST-" + list, 2)
    values = strtoreal(S[,1])
    labels = S[,2]
    
    // select
    if (select!="") {
        if (select=="major") {
            p = selectindex(mod(values, 1000):==0)
            values = values[p]/1000
        }
        else if (select=="submajor") {
            p = selectindex(mod(values, 100):==0)
            values = values[p]/100
        }
        else if (select=="minor") {
            p = selectindex(mod(values, 10):==0)
            values = values[p]/10
        }
        else _error(3498, "{bf:" + select + "}" + " not allowed")
        labels = labels[p]
    }

    // strip quotes
    n = rows(values)
    for (i=1; i<=n; i++) {
        s = labels[i]
        if (substr(s, 1, 1)=="'") {                 // '...'
            if (substr(s, -1, 1)=="'") {
                labels[i] = substr(s, 2, strlen(s)-2)
            }
        }
        else if (substr(s, 1, 1)==`"""') {          // "..."
            if (substr(s, -1, 1)==`"""') {
                labels[i] = substr(s, 2, strlen(s)-2)
            }
        }
        else if (substr(s, 1, 2)=="`" + `"""') {   // `"..."'
            if (substr(s, -2, 2)==`"""' + "'") {
                labels[i] = substr(s, 3, strlen(s)-4)
            }
        }
    }
}

string matrix read_list(string scalar list, real scalar ncols)
{
    real scalar   fh, fpos, i, j
    string scalar fn, stop, s
    transmorphic  t
    string matrix S
    string matrix EOF

    // setup
    fn = findfile("iscogen.ado")
    if (fn=="") {
        display("{err}iscogen.ado not found")
        exit(111)
    }
    stop = "% END"
    EOF = J(0, 0, "")
    fh = fopen(findfile("iscogen.ado"), "r")

    // seek start of code list
    while ((s = fget(fh))!=EOF) {
        if (strtrim(s)==list) break
    }
    fpos = ftell(fh)
    
    // dictionary not found
    if (s==EOF) {
        display("{err}" + substr(list,3,.) + " not found")
        exit(111)
    }
    
    // count lines
    i = 0
    while ((s = fget(fh))!=EOF) {
        s = strtrim(s)
        if (s==stop) break // end of list
        if (substr(s, 1, 1)=="%") continue // skip comments
        if (s=="") continue // empty line
        i++
    }

    // empty dictionary
    if (i==0) {
        display("{err}unexpected error; " + substr(list,3,.) + " is empty")
        exit(499)
    }
    
    // read codes and close file
    fseek(fh, fpos, -1)
    S = J(i, ncols, "")
    t = tokeninit()
    tokenqchars(t, ( "''", `""""', `"`""'"'))
    i = 0
    while ((s = fget(fh))!=EOF) {
        s = strtrim(s)
        if (s==stop) break // end of list
        if (substr(s, 1, 1)=="%") continue // skip comments
        if (s=="") continue // empty line
        i++
        tokenset(t, s)
        for (j=1; j<=ncols; j++) S[i,j] = tokenget(t)
    }
    fclose(fh)

    // return result
    return(S)
}

end

exit

% CODELIST-isco68-isco88
% source: isco6888.sps from http://www.harryganzeboom.nl/isco08/
% note: alternative would be isco6888.sps from http://www.harryganzeboom.nl/isco68/; 
%       the version from .../isco08/ has some additional codes
% note: using last mapping in case of duplicates (repeated mappings); this leads
%       to differences to the source because in SPSS later mappings are ignored;
%       using the last mapping is consistent with isco6888.ado for Stata
% variables: ISCO-68 ISCO-88
0100 2110
0110 2113
0120 2111
0131 2114
0132 2111
0133 2112
0140 3111
0200 2140
0210 2141
0220 2142
0230 2143
0240 2145
0250 2146
0260 2147
0270 2147
0280 2149
0290 2149
0300 3110
0310 2148
0320 3118
0321 3118
0329 3118
0330 3112
% 0340 3113 (duplicate)
0340 3114
0350 3115
0360 3116
0370 3117
0380 3117
0390 3119
0400 3140
0410 3143
0411 3143
0419 3143
0420 3142
0421 3142
0429 3142
0430 3141
0500 2210
0510 2211
0520 2212
0521 2212
0529 2212
0530 2213
0531 3213
0539 2213
0540 3211
0541 3212
0549 3211
0600 2221
0610 2221
0611 2221
0619 2221
0620 3221
0630 2222
0640 3225
0650 2223
0660 3227
0670 2224
0680 3228
0690 3223
0700 2230
0710 2230
0711 2230
0719 2230
0720 3231
0730 2230
0740 3232
0750 3224
0751 3224
0759 3224
0760 3226
0761 3226
0762 3226
0769 3226
0770 3133
0780 3220
0790 3229
0791 3226
0792 3241
0793 3222
% 0799 3229 (duplicate)
0800 2120
0810 2122
0820 2121
0830 2131
0840 3434
0849 2132
0900 2441
1100 2411
1101 2411
1109 2411
1200 2420
1210 2421
1211 2421
1219 2421
1220 2422
1221 2422
1222 2422
1229 2422
1290 2429
1291 2429
1299 2429
0130 2110
1300 2300
1310 2310
1311 1229
1319 2310
1320 2321
1321 2323
1329 2321
1330 2331
1340 2332
1350 2340
1390 2359
1391 1229
1392 2352
1393 3300
1394 1229
1399 2322
1400 2460
1410 2460
1411 2460
1412 3480
1413 2460
1414 2460
1415 2460
1416 3480
1419 2460
1490 3480
1491 3242
1499 3480
1500 2451
1510 2451
1511 2451
1519 2451
1590 2451
1591 2451
1592 2451
1593 2419
1599 2451
1600 2450
1610 2452
1620 3471
1621 3471
1622 3471
1629 3471
1630 3131
1631 3131
1639 3131
1700 3470
1710 3473
1711 2453
1712 3473
1713 3473
1719 2453
1720 3473
1721 2454
1729 3473
1730 2455
1731 2455
1732 2455
1739 2455
1740 1229
1749 1229
1750 3474
1790 3470
1791 3473
1799 3472
1800 3475
1801 3475
1809 3475
1900 2400
1910 2432
1920 2442
1921 2445
1922 2443
1923 2443
1924 2442
1929 2442
1930 2446
1931 3460
1939 2446
1940 2412
1941 2412
1949 1232
1950 2444
1951 2444
1959 2444
1960 2000
1990 3400
1991 5150
1992 3450
1993 5113
1994 3340
1995 3429
1999 3100
2000 1110
2010 1100
2011 1120
2012 1120
2013 1120
2014 1120
2015 1130
2020 1110
2021 1110
2022 1110
2023 1110
2024 1110
2029 1110
2030 1120
2031 1120
2032 1120
2033 1120
2034 1120
2035 1120
2036 1130
2039 1120
2100 1300
% 2110 1200 (duplicate)
2110 1200
2111 1210
2112 1210
2113 1319
2114 1227
2115 1227
2116 1313
% 2119 1210 (duplicate)
2119 1210
2120 1220
2190 1220
2191 1230
2192 1230
2193 1230
2194 1230
2195 1141
2196 1142
2197 1142
2199 1227
3000 1231
3009 1240
3100 3440
3101 3440
3102 3444
3103 3441
3104 3442
3109 3440
3200 4110
3210 4111
3211 4115
3219 4111
3220 4113
3300 3430
3310 3433
3311 4211
3312 4211
3313 4212
3314 4212
3315 4211
3319 3433
3390 4120
3391 4215
3399 4121
3400 3120
3410 4114
3420 3122
3500 4133
3510 1226
3520 1226
3590 4133
3600 5112
3601 5112
3602 5112
3609 5112
3700 4142
3701 9151
3709 4142
3800 4223
3801 3132
3802 3132
3809 4223
3900 4100
3910 4131
3911 4131
3919 4131
3920 4132
3930 4100
3931 4100
3932 3432
3939 4100
3940 4222
3941 4133
3942 4131
3943 3414
3944 4221
3949 4222
3950 4141
3951 4141
3959 4141
3990 4190
3991 4143
3992 1141
3993 9153
3999 4143
4000 1314
4001 1317
4002 1231
4009 1314
4100 1314
4101 1224
4102 5230
4103 1224
4104 3421
4105 3421
4106 1314
4107 9333
4108 3423
4109 1314
4200 1233
4210 1233
4220 3416
4221 3416
4222 3416
4229 3416
4300 3415
4310 3415
4311 3415
4319 3415
4320 3415
4400 3410
4410 3412
4411 3413
4412 3411
4419 3412
4420 3429
4430 3417
4431 3417
4432 3417
4500 5200
4510 5220
4511 5220
4512 5220
4513 5210
4514 5220
4519 5220
4520 9110
4521 9110
4522 9113
4523 9112
4524 9113
4525 9110
4529 5230
4900 5200
5000 1315
5001 1315
5002 1225
5009 1225
% 5100 1315 (duplicate)
5100 5000
5101 1315
5102 1315
5103 1315
5104 1315
5109 1315
5200 5121
5201 5121
5209 5121
5300 5120
5310 5122
5311 5122
5312 9132
5319 5122
5320 5123
5321 5123
5322 5123
5329 5123
5400 9130
5401 5131
5402 9132
5403 9152
5409 5142
5500 9140
5510 9141
5511 9141
5512 9141
5519 9141
5520 9132
5521 9142
5522 7143
5529 9132
5600 8264
5700 5141
5701 5141
5702 5141
5703 5141
5709 5141
5800 5160
5810 5161
5820 5162
5821 3451
5822 1229
5823 1229
5829 5162
5830 5164
5831 1229
5832 3452
5833 5164
5839 5164
5890 5169
5891 5163
5892 3432
5899 9152
5900 5100
5910 5113
5920 5143
5990 5100
5991 9152
5992 9151
5993 9151
5994 9152
5995 9120
5996 5111
5997 4213
5998 9151
5999 5132
6000 1311
6001 6132
6009 1311
6100 6133
6110 6133
6111 1311
6112 6210
6113 6133
6114 6133
6115 6130
6116 6130
6117 9211
6119 6133
6120 6130
6200 9211
6210 9211
6211 9211
6219 9211
6220 9211
6230 9211
6239 9211
% 6240 9211 (duplicate)
6240 9211
6250 9211
6260 9211
6270 9211
6280 8331
% 6290 9211 (duplicate)
6290 9200
6291 9211
6299 6100
6300 9212
6310 6141
6311 6141
6319 6141
6320 9142
6329 6141
6400 6150
6410 6150
6411 6150
6419 6150
6490 6150
6491 6154
6499 9213
7000 7510
7001 7510
7009 7510
7100 7110
7110 7111
7111 7111
7112 7111
7113 7111
7119 7111
7120 8112
7130 8113
7139 8113
7200 7220
7210 8121
7220 8122
7230 8123
7240 8122
7250 7211
7260 8123
7270 8124
7280 8223
7290 8120
7300 8140
7310 8141
7320 8141
7321 7421
7329 8141
7330 8142
7340 8143
7400 8150
7410 8151
7420 8152
7430 8153
7440 8154
7450 8155
7490 8159
7491 6142
7499 8159
7500 8260
7510 8261
7520 8261
7530 7432
7540 8262
7541 7432
7549 8262
7550 8262
7560 8264
7590 8260
7600 7441
7610 7441
7620 7441
7700 8270
7710 8273
7711 8273
7719 8273
7720 8276
7730 7411
7731 8271
7739 7411
7740 8270
7750 8272
7760 7412
7761 7412
7769 7412
7770 8277
7780 8278
7790 8270
7799 7411
7800 8279
7810 8279
7820 8279
7830 8279
7890 8279
7900 7433
7910 7433
7911 7433
7919 7433
7920 7434
7930 7433
7940 7435
7950 7436
7960 7437
0799 2221
7990 8269
8000 7442
8010 7442
8020 7442
8030 7442
8100 7420
8110 7422
8120 8240
8190 7420
8191 7422
8199 7422
8200 7113
8300 7220
8310 7221
8311 7221
8319 7221
8320 7222
8321 7222
8329 7222
8330 7223
8331 7223
8339 7223
8340 8211
% 8350 7224 (duplicate)
8350 8211
8351 8223
8359 7224
8390 7220
8400 7230
8410 7230
8411 8281
8412 7233
8419 7230
8420 7311
8421 7311
8422 7311
8429 7311
8430 7231
8431 1314
8439 7231
8440 7232
8490 7230
8491 7231
8492 7233
8493 8280
8494 7234
8499 7230
8500 7240
8510 7241
8520 7242
8530 8282
8540 7243
8550 7137
8551 7137
8559 7137
8560 7244
8570 7245
8590 7240
8600 3130
8610 3132
8620 3139
8700 7210
8710 7136
8711 7136
8719 7136
8720 7212
8730 7213
8731 7213
8732 7213
8733 7213
8739 7213
8740 7214
8800 7313
8801 7313
8809 7313
8900 7320
8910 7322
8911 7322
8919 7322
8920 7321
8930 8131
8940 7323
8950 7324
8990 8130
9000 8230
9010 8230
9020 8231
9100 8253
9200 7340
9210 7341
9211 7341
9219 7341
9220 8251
9230 7342
9240 7343
9250 7343
9260 7345
9270 8224
9290 7340
9300 7241
9310 7141
9311 7141
9319 7141
9390 7142
9400 7520
9410 7312
9420 7424
9430 8212
9490 7331
9491 7331
9492 7331
9493 7331
9499 8400
9500 7120
9510 7122
9520 7123
9530 7131
9540 7124
9541 7124
9542 7124
9549 7124
9550 7133
9551 7133
9559 7133
9560 7134
9570 7135
9590 7129
9591 7129
9592 7129
9593 7129
9594 7129
9595 9313
9596 7129
9599 7141
9600 8160
9610 8161
9690 8160
9700 9330
9710 9333
9711 9333
9712 9151
9713 9151
9714 9322
9719 9333
9720 7215
9730 8333
9731 8333
9739 8333
9740 8332
9790 8334
9800 8320
9810 8340
9811 8340
9819 8340
9820 8162
9830 8311
9831 8311
9832 8311
% 9839 8311 (duplicate)
9839 8311
9840 8312
9850 8320
9851 8323
9852 8324
9853 8324
9854 9333
9855 3340
9859 8322
9860 9332
9861 9332
9869 9332
9890 8320
9891 8312
9899 9331
9900 9300
9950 7520
9951 7520
9959 7520
9970 8290
9971 7530
9979 8280
9990 9300
9991 9320
9992 9320
9993 9320
9994 9312
9995 9162
9996 9161
9997 9312
9999 9300
% 12000 0000 (unclassifiable occupation)
10000 3452
10001 1229
10002 3452
10003 5164
10009 1229
7899 7416
7732 7411
6322 6112
6321 6141
4439 3417
2019 1210
0339 3112
0139 2120
% END

% CODELIST-isco68-isco08
% source: isco6808.sps from http://www.harryganzeboom.nl/isco08/
% note: using first mapping in case of duplicates (repeated mappings); this is
%       consistent with the source because in SPSS later mappings are ignored
% variables: ISCO-68 ISCO-08
0100 2110
0110 2113
0120 2111
0130 2110
0131 2114
0132 2111
0133 2112
0139 2110
0140 3111
0200 2140
0210 2160
0220 2142
0230 2151
0240 2144
0250 2145
0260 2146
0270 2146
0280 2141
0290 2149
0300 3110
0310 2165
0320 3118
0321 3118
0329 3118
0330 3112
0339 3119
0340 3113
0350 3115
0360 3116
0370 3117
0380 3117
0390 3119
0400 3150
0410 3153
0411 3153
0419 3153
0420 3152
0421 3152
0429 3152
0430 3151
0500 2130
0510 2131
0520 2131
0521 2130
0529 2131
0530 2131
% 0530 2131 (duplicate)
0531 3142
0540 3141
0541 3142
0549 3210
0600 2200
0610 2210
0611 2210
0619 2210
0620 3256
0630 2261
0640 3251
0650 2250
0660 3240
0670 2262
0680 3213
0690 2265
0700 2220
0710 2221
0711 2221
0719 2221
0720 3221
0730 2222
0740 3222
0750 2267
0751 3254
0759 2267
0760 2264
0761 2264
0762 3259
0769 2264
0770 3211
0780 2240
0790 3259
0791 3259
0792 3230
0793 3257
0799 3259
0800 2120
0810 2120
0820 2120
0830 2511
0840 3314
0849 2514
0900 2631
1100 2411
1101 2411
1109 2411
1200 2610
1210 2611
1211 2611
1219 1220
% 1219 2611 (duplicate)
1220 2612
1221 2612
1222 2612
1229 2612
1290 2619
1291 2619
1299 2611
1300 2300
1310 2310
1311 2310
1319 2310
1320 2330
1321 2330
1329 2330
1330 2341
1340 2342
1350 2352
1390 2350
1391 1345
1392 2359
1393 5312
1394 1345
1399 2320
1400 2636
1410 2636
1411 2636
1412 3413
1413 2636
1414 2636
1415 3413
1416 2636
1419 2636
1490 3413
1491 5161
1499 3413
1500 2641
1510 2641
1511 2641
1519 2641
1590 2641
1591 2642
1592 2431
1593 2432
1599 2642
1600 2650
1610 2651
1620 3435
1621 3435
1622 3432
1629 3435
1630 3431
1631 3521
1639 3431
1700 2650
1710 2652
1711 2652
1712 2652
1713 2652
1719 2652
1720 2653
1721 2653
1729 2653
1730 2655
1731 2655
1732 2654
1739 2655
1740 2654
1749 2654
1750 2659
1790 2650
1791 2652
1799 2656
1800 3421
1801 3422
1809 3421
1900 2000
1910 2622
1920 2632
1921 2634
1922 2633
1923 2633
1924 2632
1929 2632
1930 2635
1931 3412
1939 2635
1940 2423
1941 2423
1949 1212
1950 2643
1951 2643
1959 2643
1960 2000
1990 3000
1991 5161
1992 3100
1993 3421
1994 2635
1995 3339
1999 3100
2000 1112
2010 1112
2011 1112
2012 1112
2013 1112
2014 1112
2015 1113
2019 1112
2020 1111
2021 1111
2022 1111
2023 1111
2024 1111
2029 1111
2030 1112
2031 1112
2032 1112
2033 1112
2034 1112
2035 1112
2036 1113
2039 1112
2100 1200
2110 1120
2111 1120
2112 1120
2113 1200
2114 1210
2115 1210
2116 1323
2119 1120
2120 1321
2190 1200
2191 1200
2192 1200
2193 1200
2194 1200
2195 1114
2196 1114
2197 1114
2199 1210
3000 4000
3009 3341
3100 3350
3101 3350
3102 3112
3103 3351
3104 3352
3109 3350
3200 4131
3210 4120
3211 4120
3219 4131
3220 4132
3300 3313
3310 3313
3311 5230
3312 5230
3313 4211
3314 4211
3315 5230
3319 3313
3390 4311
3391 4214
3399 4310
3400 3500
3410 4132
3420 3511
3500 1324
3510 1324
3520 1324
3590 4323
3600 5112
3601 5112
3602 5111
3609 5112
3700 4412
3701 9621
3709 4412
3800 4223
3801 3521
3802 3521
3809 4223
3900 4100
3910 4321
3911 4323
3919 4321
3920 4322
3930 4100
3931 4100
3932 3411
3939 4100
3940 4226
3941 4323
3942 9621
3943 4221
3944 4226
3949 4226
3950 4411
3951 4415
3959 4411
3990 4419
3992 4419
3993 9623
3999 4413
4000 1420
4001 5222
4002 1211
4009 1420
4100 5221
4101 1420
4102 5211
4103 1420
4104 3324
4105 3324
4106 1420
4107 9629
4108 3333
4109 5221
4200 1221
4210 1221
4220 3323
4221 3323
4222 3323
4229 3323
4300 2433
4310 2433
4311 3322
4319 2433
4320 3322
4400 3321
4410 3321
4411 3334
4412 3311
4419 3321
4420 3339
4430 3339
4431 3315
4432 3315
4439 3339
4500 5200
4510 5220
4511 5220
4512 5245
4513 5241
4514 5220
4519 5220
4520 9520
4521 9520
4522 5244
4523 9520
4524 5243
4525 9520
4529 5211
4900 5200
5000 1410
5001 1411
5002 1411
5009 1412
5100 1411
5101 1412
5102 1411
5103 1411
5104 1412
5109 1412
5200 5151
5201 5151
5209 5151
5300 5120
5310 5120
5311 3434
5312 9412
5319 5120
5320 5131
5321 5132
5322 5246
5329 5131
5400 9100
5401 5311
5402 9112
5403 4224
5409 5162
5500 5153
5510 5153
5511 5151
5512 5153
5519 5153
5520 9112
5521 9123
5522 7133
5529 9112
5600 8154
5700 5141
5701 5141
5702 5141
5703 1439
5709 5141
5800 5410
5810 5411
5820 5412
5821 5412
5822 1349
5823 5412
5829 5412
5830 0000
5831 0100
5832 0200
5833 0300
5839 0300
5890 5414
5891 5413
5892 3411
5899 5414
5900 5100
5910 5414
5920 5163
5990 5329
% 5990 5100 (duplicate)
5991 5414
5992 9629
5993 9621
5994 5414
5995 9510
5996 5111
5997 4212
5998 9621
5999 5321
0539 2131
6000 6100
6001 6100
6009 1311
6100 6100
6110 6100
6111 1311
6112 6100
6113 6100
6114 6100
6115 6100
6116 6100
6117 9210
6119 6100
6120 6100
6200 9210
6210 9210
6211 9210
6219 9210
6220 9211
6230 9214
6239 9214
6240 9212
6250 9212
6260 9212
6270 9214
6280 8341
6290 9210
6291 6340
6299 6100
6300 6210
6310 6210
6311 6210
6319 6210
6320 9215
6321 6210
6322 6113
6329 6210
6400 6220
6410 6220
6411 6220
6419 6220
6490 6220
6491 6224
6499 6223
7000 3120
7001 3120
7009 3120
7100 8110
7110 8111
7111 8111
7112 8111
7113 8111
7119 8111
7120 8112
7130 8113
7139 8113
7200 8120
7210 8121
7220 8121
7230 8121
7240 7211
7250 7211
7260 8121
7270 8121
7280 8122
7290 8120
7300 8172
7310 8172
7320 8172
7321 7543
7329 8172
7330 8171
7340 8171
7400 8130
7410 8131
7420 8131
7430 8131
7440 3134
7450 8131
7490 8131
7491 6210
7499 8131
7500 8150
7510 8151
7520 8151
7530 8152
7540 8152
7541 7543
7549 8152
7550 8152
7560 8154
7590 8150
7600 7535
7610 7535
7620 7535
7700 7515
7710 8160
7711 8160
7719 8160
7720 8160
7730 7511
7731 7511
7732 7511
7739 7511
7740 8160
7750 7513
7760 7512
7761 7512
7769 7512
7770 7515
7780 7515
7790 7515
7799 7511
7800 7516
7810 7516
7820 7516
7830 7516
7890 7516
7899 7516
7900 7531
7910 7531
7911 7531
7919 7531
7920 7531
7930 7531
7940 7532
7950 7533
7960 7534
7990 7531
8000 7536
8010 7536
8020 7536
8030 7536
8100 7520
8110 7522
8120 7523
8190 7520
8191 7522
8199 7522
8200 7113
8300 7220
8310 7221
8311 7221
8319 7221
8320 7222
8321 7222
8329 7222
8330 7223
8331 7223
8339 7223
8340 8100
8350 7224
8351 7224
8359 7224
8390 7222
8400 7230
8410 7230
8411 7232
8412 7223
8419 7230
8420 7311
8421 7311
8422 3214
8429 7311
8430 7231
8431 1439
8439 7231
8440 7232
8490 7230
8491 7234
8492 7230
8493 8210
8494 9620
8499 7230
8500 7420
8510 7412
8520 7421
8530 8212
8540 7421
8550 7411
8551 7411
8559 7411
8560 7422
8570 7413
8590 7420
8600 3521
8610 3521
8620 3521
8700 7126
8710 7126
8711 7126
8719 7126
8720 7212
8730 7213
8731 7213
8732 7213
8733 7213
8739 7213
8740 7214
8800 7313
8801 7313
8809 7313
8900 7315
8910 7315
8911 7315
8919 7549
8920 7314
8930 7315
8940 7316
8950 7316
8990 7315
9000 8140
9010 8140
9020 8141
9100 8143
9200 7320
9210 7321
9211 7322
9219 7322
9220 7322
9230 7321
9240 7321
9250 7321
9260 7323
9270 8132
9290 7320
9300 7131
9310 7131
9311 7131
9319 7131
9390 7132
9400 8000
9410 7312
9420 7317
9430 8114
9490 7549
9491 7317
9492 7319
9493 7317
9499 7543
9500 7100
9510 7112
9520 7114
9530 7121
9540 7115
9541 7115
9542 7115
9549 7115
9550 7123
9551 7123
9559 7123
9560 7124
9570 7125
9590 7119
9591 7131
9592 7119
9593 7119
9594 7119
9595 9313
9596 7119
9599 7131
9600 3131
9610 3131
9690 3131
9700 8340
9710 9333
9711 9333
9712 9621
9713 9621
9714 9321
9719 9333
9720 7215
9730 8343
9731 8343
9739 8343
9740 8342
9790 8344
9800 8300
9810 8350
9811 8350
9819 8350
9820 8182
9830 8311
9831 8311
9832 8311
9839 8311
9840 8312
9850 8320
9851 8331
9852 8332
9853 8322
9854 9333
9855 5165
9859 8320
9860 9332
9861 9332
9869 9332
9890 8320
9891 8312
9899 9331
9900 9300
9950 7000
9951 7000
9959 7000
9970 8000
9971 7000
9979 8000
9990 9300
9991 9320
9992 9300
9993 9300
9994 9312
9995 9613
9996 9611
9997 9312
9999 9300
10000 0000
10001 0100
10002 0200
10003 0300
10009 0100
% 12000 9999 (unclassifiable occupation)
% END

% CODELIST-isco68-isei
% source: iscoisei.sps from http://www.harryganzeboom.nl/isco68/
% note: leading zeros added to ISCO-68 codes with less than 4 digits
% note: source contains range mappings that have been expanded to one-to-one
%       mappings based on the set of ISCO-68 codes found in iscolab.sps (plus
%       code 3991 found in isco6888.sps)
% note: using mappings consistent with Ganzeboom et al. (1992: Appendix B) in
%       case of duplicates (repeated mappings); this leads to inconsistencies
%       with SPSS, but is consistent with iscoisei.ado for Stata
% variables: ISCO-68 ISEI
0100 62
0110 73
0120 79
0130 79
0131 79
0132 79
0133 79
0139 79
0140 47
0200 71
0210 77
0220 73
0230 69
0240 68
0250 73
0260 70
0270 65
0280 65
0290 76
0300 53
0310 58
0320 53
0321 53
0329 53
0330 50
0339 50
0340 48
0350 52
0360 57
0370 56
0380 56
0390 56
0400 59
0410 71
0411 71
0419 71
0420 53
0421 53
0429 53
0430 53
0500 65
0510 77
0520 77
0521 77
0529 77
0530 77
0531 77
0539 77
0540 52
0541 59
0549 49
0600 85
0610 88
0611 88
0619 88
0630 86
0650 84
0670 81
0700 49
0710 42
0711 42
0715 42
0719 42
0720 39
0730 51
0740 39
0750 58
0751 58
0759 58
0760 58
0761 58
0762 58
0769 58
0770 58
0680 52
0780 52
0620 52
0640 52
0660 52
% 0670 52 (duplicate)
0690 52
0790 58
0791 58
0792 58
0793 58
0799 58
0800 67
0810 71
0820 71
0830 71
0840 64
0849 64
0900 80
1100 69
1101 75
1109 68
1200 85
1210 85
1211 85
1219 85
1220 90
1221 90
1222 90
1229 90
1290 82
1300 71
1310 78
1311 78
1319 78
1320 71
1321 71
1329 71
1330 69
1340 65
1350 65
1390 65
1391 65
1392 65
1393 65
1394 65
1399 65
1400 55
1410 55
1411 55
1412 55
1413 55
1414 55
1415 55
1416 55
1419 55
1490 55
1491 55
1499 55
1500 66
1510 66
1511 66
1519 66
1590 66
1591 66
1592 66
1593 66
1599 66
1600 55
1610 57
1620 55
1621 60
1622 44
1629 44
1630 50
1631 50
1639 50
1700 59
1710 54
1711 54
1712 54
1713 54
1719 54
1720 64
1721 64
1729 64
1730 64
1731 64
1732 64
1739 64
1740 64
1749 64
1750 54
1790 64
1791 64
1799 64
1800 55
1801 55
1809 55
1900 65
1910 59
1920 72
1921 72
1922 72
1923 72
1924 72
1929 72
1930 54
1931 54
1939 54
1940 59
1941 59
1949 59
1950 54
1951 54
1959 54
1960 82
1990 61
1991 61
1992 61
1993 61
1994 61
1995 61
1999 61
2000 72
2010 72
2011 72
2012 72
2013 72
2014 72
2015 72
2019 72
2020 73
2021 73
2022 73
2023 73
2024 73
2029 73
2030 72
2031 72
2032 72
% 2033 72 (duplicate)
2034 72
% 2035 72 (duplicate)
2036 72
2039 72
2033 81
2035 60
2100 67
2110 66
2111 69
2114 69
2115 69
2119 69
2112 65
2113 65
2116 47
2120 67
2190 67
2191 65
2192 67
2193 67
2195 59
2196 59
2197 59
2199 67
2194 67
3000 60
3009 60
3100 58
3102 48
3103 48
3104 53
3101 59
3109 59
3200 54
3210 55
3211 58
3219 48
3220 48
3300 54
3310 54
3312 58
3311 47
3313 47
3314 47
3315 46
3319 56
3390 44
3391 44
3399 44
3400 51
3410 50
3420 54
3500 49
3510 56
3520 56
3590 48
3600 37
3601 37
3602 37
3609 37
3700 36
3701 34
3709 37
3800 43
3801 63
3802 63
3809 43
3900 48
3910 35
3911 35
3919 35
3920 45
3930 58
3931 58
3932 58
3939 58
3940 51
3941 51
3942 51
3943 51
3944 51
3949 51
3950 51
3951 51
3959 51
3990 45
3991 45
3992 45
3993 45
3999 45
4000 54
4001 54
4002 54
4009 54
4100 53
4101 64
4104 64
4105 64
4108 64
4102 35
4107 35
4103 52
4106 60
4109 47
4200 54
4210 57
4220 52
4221 52
4222 52
4229 52
4300 58
4310 55
4311 55
4319 55
4320 58
4400 59
4410 59
4411 61
4412 64
4419 58
4420 60
4430 56
4431 56
4432 56
4500 42
4510 45
4512 17
4513 41
4514 41
4511 46
4519 46
4520 35
4521 35
4522 35
4523 35
4524 35
4525 35
4529 35
4900 35
5000 41
5001 41
5002 41
5009 41
5100 48
5101 48
5102 48
5103 48
5104 48
5109 49
5200 33
5201 33
5209 33
5300 29
5310 27
5311 30
5319 30
5312 10
5320 30
5321 28
5322 28
5329 32
5400 24
5401 24
5402 24
5403 24
5409 24
5500 25
5510 26
5511 26
5512 26
5519 26
5520 22
5521 22
5522 22
5529 22
5600 24
5700 32
5701 32
5702 32
5703 32
5709 32
5800 48
5810 44
5820 54
5821 75
5822 75
5823 53
5829 53
5830 60
5831 83
5832 58
5833 58
5890 35
5891 35
5892 35
5899 35
5900 40
5910 39
5920 58
5990 39
5991 44
5997 44
5992 37
5993 37
5994 37
5995 37
5998 37
5996 34
5999 28
6000 46
6001 32
6009 49
6100 26
6110 26
6111 49
6112 18
6113 27
6114 27
6115 27
6116 26
6119 26
6120 29
6200 17
6210 16
6211 16
6219 16
6117 16
6220 16
6230 16
6239 16
6240 20
6250 20
6260 20
6270 21
6280 28
6290 10
6291 10
6299 10
6300 25
6310 19
6311 19
6319 19
6320 32
6321 32
6322 32
6329 32
6400 30
6410 30
6411 30
6419 30
6490 32
6491 32
6499 32
7000 44
7001 44
7009 44
7100 32
7110 32
7112 29
7111 32
7113 32
7119 32
7120 26
7130 31
7139 31
7200 34
7210 34
7220 31
7329 31
7230 31
7240 31
7250 34
7260 34
7270 34
7280 34
7290 37
7300 26
7310 24
7320 24
7321 24
7330 36
7340 36
7400 36
7410 43
7420 43
7430 43
7440 43
7450 43
7491 29
7490 36
7499 36
7500 34
7510 35
7520 35
7530 30
7540 34
7541 34
7549 34
7550 31
7560 31
7590 37
7600 38
7610 38
7620 38
7700 32
7710 22
7711 22
7719 22
7720 22
7730 32
7731 32
7732 32
7739 32
7740 28
7750 33
7760 33
7761 33
7769 33
7770 33
7780 33
7790 36
7799 36
7800 37
7810 37
7820 37
7830 37
7890 37
7900 40
7910 46
7919 46
7920 43
7930 43
7940 43
7950 23
7911 23
7960 30
7990 37
8000 33
8010 33
8020 32
8030 32
8100 35
8110 36
8120 32
8190 32
8191 32
8199 32
8200 29
8300 40
8310 37
8311 34
8319 45
8320 41
8321 44
8329 41
8330 36
8331 39
8339 35
8340 39
8350 28
8351 28
8390 43
8400 35
8410 36
8411 36
8412 36
8419 36
8420 39
8421 39
8422 39
8429 39
8430 31
8439 31
8431 52
8440 44
8490 36
8491 26
8492 26
8494 26
8493 28
8499 37
8500 41
8510 39
8520 41
8530 40
8540 43
8550 40
8551 40
8559 40
8560 43
8570 41
8590 46
8600 46
8610 46
8620 46
8700 35
8710 36
8711 36
8719 36
8720 33
8730 36
8731 32
8732 37
8733 41
8739 37
8740 33
8800 43
8801 43
8809 43
8900 29
8910 33
8911 33
8919 33
8920 27
8930 25
8940 25
8950 25
8990 25
9000 33
9010 33
9020 33
9100 34
9200 42
9210 41
9211 41
9219 41
9220 41
9230 43
9240 43
9250 43
9260 39
9270 43
9290 46
9300 32
9310 32
9311 32
9319 32
9390 30
9400 29
9410 29
9420 29
9430 29
9490 29
9491 29
9492 29
9493 29
9499 39
9500 32
9510 32
9520 29
9530 22
9540 31
9541 31
9542 31
9549 31
9550 33
9551 33
9559 33
9560 36
9570 30
9590 31
9591 39
9592 39
9593 39
9594 39
9596 39
9599 39
9595 24
9600 33
9610 34
9690 33
9700 31
9710 31
9711 32
9712 21
9713 21
9714 27
9719 34
9720 28
9730 29
9731 29
9739 29
9740 30
9790 30
9800 36
9810 36
9811 36
9819 36
% 9820 36 (duplicate)
% 9830 36 (duplicate)
% 9831 36 (duplicate)
% 9832 36 (duplicate)
% 9839 36 (duplicate)
9820 36
9830 45
9831 45
9832 45
9839 45
9840 35
9850 37
9851 33
9852 37
9853 37
9855 37
9854 18
9859 33
9860 20
9861 20
9890 35
9891 35
9899 35
9900 25
9950 43
9951 43
9959 43
9970 28
9971 28
9979 28
9990 23
9991 24
9994 28
9995 26
9996 26
9997 28
9992 22
9993 22
9999 22
% END

% CODELIST-isco68-siops
% source: iscotrei.sps from http://www.harryganzeboom.nl/isco68/
% note: leading zeros added to ISCO-68 codes with less than 4 digits
% note: source contains uninformative duplicates; these have been commented out
% variables: ISCO-68 SIOPS
0400 59
2193 63
% 4319 51 (dublicate)
% 6119 47 (dublicate)
% 6291 5  (duplicate)
0100 66
0110 69
0120 76
0130 72
0131 67
0132 71
0133 49
0139 78
0140 46
0200 56
0210 72
0220 70
0230 65
0240 66
0250 66
0260 60
0270 63
0280 54
0290 55
0300 56
0310 58
0320 55
0321 26
0329 55
0330 39
0339 39
0340 46
0350 46
0360 46
0370 46
0380 54
0390 46
0410 66
0411 80
0419 66
0420 50
0421 36
0429 63
0430 60
0500 61
0510 69
0520 68
0521 56
0529 79
0530 56
0531 55
0539 58
0540 52
0541 47
0549 58
0600 60
0610 78
0611 80
0619 78
0620 50
0630 70
0640 44
0650 61
0660 48
0670 64
0680 44
0690 52
0700 50
0710 54
0711 58
0719 54
0720 44
0730 46
0740 42
0750 60
0751 57
0759 62
0760 51
0761 57
0762 30
0769 67
0770 58
0790 50
0791 62
0792 29
0793 48
0799 62
0800 56
0810 55
0820 69
0830 51
0840 51
0849 51
0900 60
1100 62
1101 68
1109 55
1200 73
1210 73
1211 75
1219 71
1220 76
1221 82
1222 73
1229 78
1290 71
1291 52
1299 71
1300 61
1310 78
1311 86
1319 78
1320 60
1321 57
1329 64
1330 57
1340 49
1350 62
1390 62
1391 66
1392 68
1393 50
1394 72
1399 57
1400 46
1410 54
1411 83
1412 46
1413 50
1414 49
1415 56
1416 61
1419 60
1490 39
1491 22
1499 56
1500 58
1510 62
1511 35
1519 62
1590 56
1591 65
1592 47
1593 57
1599 55
1600 51
1610 57
1620 49
1621 56
1622 38
1629 54
1630 46
1631 47
1639 45
1700 48
1710 45
1711 38
1712 32
1713 53
1719 56
1720 40
1721 36
1729 45
1730 57
1731 63
1732 62
1739 52
1740 68
1749 68
1750 33
1790 42
1791 33
1799 50
1800 49
1801 50
1809 48
1900 57
1910 54
1920 68
1921 66
1922 69
1923 67
1924 69
1929 67
1930 52
1931 49
1939 56
1940 56
1941 55
1949 58
1950 62
1951 69
1959 54
1990 51
1991 37
1992 54
1993 49
1994 53
1995 57
1999 58
2000 64
2010 63
2011 82
2012 66
2013 75
2014 68
2015 42
2019 90
2020 64
2021 85
2022 72
2023 66
2024 55
2029 86
2030 66
2031 87
2032 73
2033 71
2034 74
2035 63
2036 50
2039 79
2100 63
2110 65
2111 70
2112 63
2113 52
2114 67
2115 76
2116 53
2119 75
2120 64
2190 60
2191 52
2192 60
2194 67
2195 63
2196 50
2197 63
2199 58
3000 55
3100 55
3101 54
3102 61
3103 44
3104 52
3109 66
3200 46
3210 48
3211 53
3219 42
3220 45
3300 38
3310 41
3311 31
3312 65
3313 48
3314 39
3315 36
3319 49
3390 34
3391 27
3399 42
3410 45
3420 53
3500 50
3510 56
3520 58
3590 37
3600 32
3601 26
3602 30
3609 39
3700 30
3701 26
3709 33
3800 44
3801 45
3802 49
3809 38
3900 38
3910 30
3911 29
3919 32
3920 44
3930 44
3931 44
3932 59
3939 43
3940 34
3941 37
3942 23
3943 43
3944 27
3949 38
3950 36
3951 31
3959 41
3990 37
3991 41
3992 48
3993 21
3999 41
4000 45
4001 38
4002 49
4009 47
4100 48
4101 58
4102 38
4103 44
4104 55
4105 40
4106 58
4108 49
4109 42
4200 49
4210 52
4220 46
4221 51
4222 39
4229 49
4300 46
4310 46
4311 42
4319 51
4320 47
4400 46
4410 50
4411 49
4412 56
4419 44
4420 42
4430 45
4431 48
4432 49
4439 39
4500 28
4510 32
4511 36
4512 25
4513 36
4514 28
4519 34
4520 24
4521 22
4522 26
4523 14
4524 24
4529 36
4900 15
5000 40
5001 53
5002 47
5009 32
5100 37
5101 35
5102 46
5103 22
5104 33
5109 48
5200 37
5201 28
5209 46
5300 26
5310 31
5311 38
5312 22
5319 31
5320 21
5321 23
5322 16
5329 23
5400 22
5401 23
5402 14
5403 33
5409 17
5500 22
5510 25
5511 24
5512 30
5519 21
5520 20
5521 19
5522 25
5529 16
5600 22
5700 32
5701 37
5702 35
5703 45
5709 30
5800 35
5810 35
5820 40
5821 60
5822 75
5823 52
5829 40
5830 42
5831 63
5832 44
5833 39
5890 30
5891 39
5892 47
5899 22
5900 31
5910 29
5920 34
5990 29
5991 20
5992 24
5993 14
5994 27
5995 12
5996 50
5997 34
5998 41
5999 42
6000 48
6001 41
6009 54
6100 40
6110 40
6111 63
6112 38
6113 30
6114 32
6115 35
6116 39
6117 34
6119 47
6120 55
6200 22
6210 20
6211 18
6219 23
6220 21
6230 21
6239 21
6240 26
6250 23
6260 21
6270 21
6280 31
6290 14
6291 05
6299 30
6300 24
6310 18
6311 18
6319 19
6320 42
6321 38
6322 40
6329 48
6400 28
6410 32
6411 37
6419 28
6490 23
6491 06
6499 40
7000 46
7001 52
7009 39
7100 32
7110 34
7111 36
7112 24
7113 44
7119 32
7120 32
7130 31
7139 31
7200 38
7210 45
7220 36
7230 38
7240 33
7250 38
7260 38
7270 38
7280 28
7290 38
7300 29
7310 29
7319 29
7320 30
7321 31
7329 30
7330 28
7340 28
7400 40
7410 43
7420 43
7430 43
7440 43
7450 37
7490 30
7491 16
7499 43
7500 29
7510 29
7520 34
7530 30
7540 32
7541 33
7549 30
7550 29
7560 25
7590 26
7600 22
7610 22
7620 22
7700 34
7710 33
7711 42
7719 33
7720 45
7730 24
7731 18
7732 45
7739 31
7740 35
7750 34
7760 33
7761 48
7769 33
7770 34
7780 34
7790 34
7799 34
7800 34
7810 34
7820 28
7830 34
7890 39
7899 39
7900 34
7910 40
7911 39
7919 40
7920 35
7930 32
7940 41
7950 26
7960 31
7990 34
8000 26
8010 28
8020 28
8030 22
8100 36
8110 40
8120 36
8190 31
8191 34
8199 28
8200 38
8300 36
8310 35
8311 36
8319 34
8320 40
8321 39
8329 40
8330 38
8331 37
8339 40
8340 38
8350 27
8351 35
8359 19
8390 40
8400 43
8410 42
8411 42
8412 40
8419 43
8420 47
8421 42
8422 60
8429 40
8430 44
8431 47
8439 43
8440 50
8490 30
8491 28
8492 31
8493 30
8494 18
8499 43
8500 41
8510 38
8520 48
8530 48
8540 42
8550 44
8551 48
8559 44
8560 35
8570 36
8590 40
8600 44
8610 53
8620 34
8700 38
8710 34
8711 45
8719 34
8720 39
8730 34
8731 32
8732 31
8733 36
8739 36
8740 44
8800 43
8801 57
8809 43
8900 31
8910 37
8911 33
8919 41
8920 25
8930 31
8940 31
8950 31
8990 31
9000 30
9010 30
9020 30
9100 28
9200 41
9210 42
9211 51
9219 42
9220 41
9230 41
9240 41
9250 46
9260 32
9270 36
9290 52
9300 30
9310 31
9311 39
9319 31
9390 29
9400 31
9410 33
9420 21
9430 30
9490 41
9491 33
9492 50
9493 23
9499 39
9500 31
9510 34
9520 34
9530 31
9540 37
9541 48
9542 23
9549 37
9550 31
9551 39
9559 31
9560 28
9570 26
9590 28
9591 38
9592 28
9593 46
9594 26
9595 15
9596 36
9599 24
9600 38
9610 42
9690 34
9700 22
9710 20
9711 20
9712 17
9713 18
9714 22
9719 21
9720 32
9730 32
9731 25
9739 39
9740 32
9790 28
9800 28
9810 29
9811 23
9819 35
9820 25
9830 34
9831 33
9832 27
9839 43
9840 29
9850 31
9851 32
9852 33
9853 39
9854 15
9855 41
9859 28
9860 22
9861 26
9869 18
9890 24
9891 30
9899 17
9900 32
9950 46
9951 50
9959 42
9970 33
9971 37
9979 29
9990 18
9991 18
9992 8
9993 20
9994 33
9995 13
9996 13
9997 20
9999 19
10000 42
10001 63
10002 44
10003 39
10009 73
% END

% CODELIST-isco68-egp
% source: iscoegp.ado (version 1.0 19jun2001 John_Hendrickx@yahoo.com) which is
%       based on iscoegp.sps from http://www.harryganzeboom.nl/isco68/
% note: the codelist has been generated automatically by applying iscoegp.ado
%       to all relevant combinations of isco codes, selfemployment, supervision
% variables: ISCO-68 EGP(0,0) EGB(0,1-9) EGP(0,10+) EGP(1,0) EGB(1,1-9) 
%       EGP(1,10+), where the first argument is selfemployment status
%       and the second argument is the number if subordinates/employees
0100 1 1 1 1 1 1
0110 1 1 1 1 1 1
0120 1 1 1 1 1 1
0130 1 1 1 1 1 1
0131 1 1 1 1 1 1
0132 1 1 1 1 1 1
0133 1 1 1 1 1 1
0139 1 1 1 1 1 1
0140 2 2 2 2 2 2
0200 1 1 1 1 1 1
0210 1 1 1 1 1 1
0220 1 1 1 1 1 1
0230 1 1 1 1 1 1
0240 1 1 1 1 1 1
0250 1 1 1 1 1 1
0260 1 1 1 1 1 1
0270 1 1 1 1 1 1
0280 1 1 1 1 1 1
0290 1 1 1 1 1 1
0300 2 2 2 2 2 2
0310 2 2 2 2 2 2
0320 2 2 2 2 2 2
0321 3 3 3 3 3 3
0329 2 2 2 2 2 2
0330 2 2 2 2 2 2
0339 2 2 2 2 2 2
0340 2 2 2 2 2 2
0350 2 2 2 2 2 2
0360 2 2 2 2 2 2
0370 2 2 2 2 2 2
0380 2 2 2 2 2 2
0390 2 2 2 2 2 2
0400 1 1 1 1 1 1
0409 1 1 1 1 1 1
0410 1 1 1 1 1 1
0411 1 1 1 1 1 1
0419 1 1 1 1 1 1
0420 1 1 1 1 1 1
0421 2 2 1 5 4 1
0429 1 1 1 1 1 1
0430 1 1 1 1 1 1
0500 1 1 1 1 1 1
0510 1 1 1 1 1 1
0520 1 1 1 1 1 1
0521 1 1 1 1 1 1
0529 1 1 1 1 1 1
0530 1 1 1 1 1 1
0531 1 1 1 1 1 1
0539 1 1 1 1 1 1
0540 2 2 2 2 2 2
0541 2 2 2 2 2 2
0549 2 2 2 2 2 2
0600 1 1 1 1 1 1
0610 1 1 1 1 1 1
0611 1 1 1 1 1 1
0619 1 1 1 1 1 1
0620 2 2 2 2 2 2
0630 1 1 1 1 1 1
0640 2 2 2 2 2 2
0650 1 1 1 1 1 1
0660 2 2 2 2 2 2
0670 1 1 1 1 1 1
0680 2 2 2 2 2 2
0690 2 2 2 2 2 2
0700 2 2 2 2 2 2
0710 2 2 2 2 2 2
0711 2 2 1 2 2 1
0715 2 2 2 2 2 2
0719 2 2 2 2 2 2
0720 3 3 3 3 3 3
0730 2 2 2 2 2 2
0740 2 2 2 2 2 2
0750 2 2 2 2 2 2
0751 2 2 2 2 2 2
0759 2 2 2 2 2 2
0760 2 2 2 2 2 2
0761 2 2 2 2 2 2
0762 8 8 7 5 4 1
0769 2 2 2 2 2 2
0770 2 2 2 2 2 2
0780 2 2 2 2 2 2
0790 2 2 2 2 2 2
0791 2 2 2 2 2 2
0792 2 2 2 2 2 2
0793 2 2 1 2 2 1
0799 2 2 2 2 2 2
0800 1 1 1 1 1 1
0810 1 1 1 1 1 1
0820 1 1 1 1 1 1
0830 2 2 2 2 2 2
0840 2 2 2 2 2 2
0849 2 2 2 2 2 2
0900 1 1 1 1 1 1
1100 1 1 1 1 1 1
1101 1 1 1 1 1 1
1109 1 1 1 1 1 1
1200 1 1 1 1 1 1
1210 1 1 1 1 1 1
1211 1 1 1 1 1 1
1219 1 1 1 1 1 1
1220 1 1 1 1 1 1
1221 1 1 1 1 1 1
1222 1 1 1 1 1 1
1229 1 1 1 1 1 1
1290 1 1 1 1 1 1
1291 2 2 2 2 2 2
1299 1 1 1 1 1 1
1300 2 2 2 2 2 2
1310 1 1 1 1 1 1
1311 1 1 1 1 1 1
1319 1 1 1 1 1 1
1320 2 2 2 2 2 2
1321 2 2 2 2 2 2
1329 2 2 2 2 2 2
1330 2 2 2 2 2 2
1340 2 2 2 2 2 2
1350 2 2 2 2 2 2
1390 2 2 2 2 2 2
1391 2 2 1 2 2 1
1392 1 1 1 1 1 1
1393 3 3 3 3 3 3
1394 1 1 1 1 1 1
1399 2 2 2 2 2 2
1400 2 2 2 2 2 2
1410 2 2 2 2 2 2
1411 1 1 1 1 1 1
1412 2 2 2 2 2 2
1413 2 2 2 2 2 2
1414 2 2 2 2 2 2
1415 2 2 2 2 2 2
1416 2 2 2 2 2 2
1419 2 2 2 2 2 2
1490 2 2 2 2 2 2
1491 0 0 0 0 0 0
1499 2 2 2 2 2 2
1500 2 2 2 2 2 2
1510 2 2 2 2 2 2
1511 2 2 2 2 2 2
1519 2 2 2 2 2 2
1590 2 2 2 2 2 2
1591 2 2 1 2 2 1
1592 2 2 2 2 2 2
1593 2 2 2 2 2 2
1599 2 2 2 2 2 2
1600 2 2 2 2 2 2
1610 2 2 2 2 2 2
1620 2 2 2 2 2 2
1621 2 2 2 2 2 2
1622 2 2 2 2 2 2
1629 2 2 2 2 2 2
1630 2 2 2 5 4 1
1631 2 2 2 5 4 1
1639 2 2 2 2 2 2
1700 2 2 2 2 2 2
1710 2 2 2 2 2 2
1711 2 2 2 2 2 2
1712 2 2 2 2 2 2
1713 2 2 2 2 2 2
1719 2 2 2 2 2 2
1720 2 2 2 2 2 2
1721 2 2 2 2 2 2
1729 2 2 2 2 2 2
1730 2 2 2 2 2 2
1731 2 2 2 2 2 2
1732 2 2 2 2 2 2
1739 2 2 2 2 2 2
1740 1 1 1 1 1 1
1749 2 2 2 2 2 2
1750 2 2 2 2 2 2
1790 2 2 2 2 2 2
1791 2 2 2 2 2 2
1799 2 2 2 2 2 2
1800 2 2 2 2 2 2
1801 2 2 2 2 2 2
1809 2 2 2 2 2 2
1900 1 1 1 1 1 1
1910 2 2 2 2 2 2
1920 1 1 1 1 1 1
1921 1 1 1 1 1 1
1922 1 1 1 1 1 1
1923 1 1 1 1 1 1
1924 1 1 1 1 1 1
1929 1 1 1 1 1 1
1930 2 2 2 2 2 2
1931 2 2 2 2 2 2
1939 2 2 2 2 2 2
1940 2 2 2 2 2 2
1941 2 2 2 2 2 2
1949 2 2 2 2 2 2
1950 2 2 2 2 2 2
1951 1 1 1 1 1 1
1959 2 2 2 2 2 2
1960 1 1 1 1 1 1
1990 2 2 2 2 2 2
1991 2 2 2 2 2 2
1992 2 2 2 2 2 2
1993 2 2 2 2 2 2
1994 2 2 2 2 2 2
1995 2 2 2 2 2 2
1999 2 2 2 2 2 2
2000 1 1 1 1 1 1
2010 1 1 1 1 1 1
2011 1 1 1 1 1 1
2012 1 1 1 1 1 1
2013 1 1 1 1 1 1
2014 1 1 1 1 1 1
2015 1 1 1 1 1 1
2019 1 1 1 1 1 1
2020 1 1 1 1 1 1
2021 1 1 1 1 1 1
2022 1 1 1 1 1 1
2023 1 1 1 1 1 1
2024 1 1 1 1 1 1
2029 1 1 1 1 1 1
2030 1 1 1 1 1 1
2031 1 1 1 1 1 1
2032 1 1 1 1 1 1
2033 1 1 1 1 1 1
2034 1 1 1 1 1 1
2035 1 1 1 1 1 1
2036 1 1 1 1 1 1
2039 1 1 1 1 1 1
2100 2 2 1 2 2 1
2110 2 2 1 2 2 1
2111 1 1 1 1 1 1
2112 2 2 1 2 2 1
2113 2 2 1 2 2 1
2114 1 1 1 1 1 1
2115 1 1 1 1 1 1
2116 2 2 1 2 2 1
2119 1 1 1 1 1 1
2120 2 2 1 2 2 1
2190 2 2 1 2 2 1
2191 2 2 1 2 2 1
2192 1 1 1 1 1 1
2193 1 1 1 1 1 1
2194 1 1 1 1 1 1
2195 1 1 1 1 1 1
2196 2 2 1 2 2 1
2197 1 1 1 1 1 1
2199 2 2 2 2 2 2
3000 2 2 1 5 4 1
3009 2 2 2 2 2 2
3100 2 2 1 2 2 1
3101 2 2 1 2 2 1
3102 2 2 1 2 2 1
3103 2 2 1 2 2 1
3104 2 2 1 2 2 1
3109 2 2 2 2 2 2
3200 3 3 3 3 3 3
3210 3 3 3 3 3 3
3211 3 3 3 3 3 3
3219 3 3 3 3 3 3
3220 3 3 3 3 3 3
3300 3 3 3 5 4 1
3310 3 3 3 5 4 1
3311 3 3 3 5 4 1
3312 3 2 1 5 4 1
3313 3 3 3 5 4 1
3314 3 3 3 5 4 1
3315 3 3 3 5 4 1
3319 3 3 3 5 4 1
3390 3 3 3 5 4 1
3391 3 3 3 5 4 1
3399 3 3 3 3 3 3
3400 3 3 3 3 3 3
3410 3 3 3 3 3 3
3420 3 3 3 3 3 3
3500 2 2 1 2 2 1
3510 2 2 1 2 2 1
3520 2 2 1 5 4 1
3590 3 2 1 3 2 1
3600 3 3 3 3 3 3
3601 3 3 3 3 3 3
3602 3 3 3 3 3 3
3609 3 3 3 3 3 3
3700 9 9 9 5 4 1
3701 9 9 9 5 4 1
3709 9 9 9 5 4 1
3800 3 3 3 3 3 3
3801 3 3 3 3 3 3
3802 3 3 3 3 3 3
3809 3 3 3 3 3 3
3900 3 3 3 3 3 3
3910 3 3 3 3 3 3
3911 3 3 3 3 3 3
3919 3 3 3 3 3 3
3920 3 3 3 3 3 3
3930 3 3 3 3 3 3
3931 3 3 3 3 3 3
3932 3 3 3 3 3 3
3939 3 3 3 3 3 3
3940 3 3 3 3 3 3
3941 3 3 3 3 3 3
3942 3 3 3 3 3 3
3943 3 3 3 3 3 3
3944 3 3 3 3 3 3
3949 3 3 3 3 3 3
3950 3 3 3 3 3 3
3951 3 3 3 3 3 3
3959 3 3 3 3 3 3
3990 3 3 3 3 3 3
3991 3 3 3 3 3 3
3992 3 3 3 3 3 3
3993 3 3 3 3 3 3
3999 3 3 3 3 3 3
4000 2 2 1 5 4 1
4001 2 2 1 5 4 1
4002 2 2 1 5 4 1
4009 2 2 1 2 2 1
4100 5 4 1 5 4 1
4101 5 4 1 5 4 1
4102 5 4 1 5 4 1
4103 5 4 1 5 4 1
4104 5 4 1 5 4 1
4105 5 4 1 5 4 1
4106 5 4 1 5 4 1
4107 0 0 0 0 0 0
4108 5 4 1 5 4 1
4109 5 4 1 5 4 1
4200 2 2 1 2 2 1
4210 2 2 1 5 4 1
4220 2 2 1 2 2 1
4221 2 2 2 2 2 2
4222 2 2 2 2 2 2
4229 2 2 2 2 2 2
4300 2 2 2 5 4 1
4310 2 2 2 5 4 1
4311 3 3 3 5 4 1
4319 2 2 2 5 4 1
4320 3 3 3 5 4 1
4400 2 2 2 5 4 1
4410 2 2 1 2 2 1
4411 2 2 1 2 2 1
4412 2 2 1 2 2 1
4419 2 2 2 2 2 2
4420 2 2 2 5 4 1
4430 2 2 2 5 4 1
4431 2 2 2 5 4 1
4432 2 2 2 5 4 1
4439 2 2 2 5 4 1
4500 3 3 3 5 4 1
4510 3 3 3 5 4 1
4511 3 3 3 5 4 1
4512 9 9 9 5 4 1
4513 3 3 3 5 4 1
4514 3 3 3 5 4 1
4519 3 3 3 5 4 1
4520 5 4 1 5 4 1
4521 5 4 1 5 4 1
4522 3 3 3 5 4 1
4523 3 3 3 5 4 1
4524 9 9 9 5 4 1
4525 0 0 0 5 4 1
4529 5 4 1 5 4 1
4900 3 3 3 5 4 1
5000 2 2 1 5 4 1
5001 2 2 1 5 4 1
5002 2 2 1 5 4 1
5009 2 2 1 2 2 1
5100 5 4 1 5 4 1
5101 5 4 1 5 4 1
5102 5 4 1 5 4 1
5103 5 4 1 5 4 1
5104 5 4 1 5 4 1
5109 5 4 1 5 4 1
5200 3 2 1 3 2 1
5201 3 2 1 3 2 1
5209 3 3 3 3 3 3
5300 8 8 7 5 4 1
5310 8 8 7 5 4 1
5311 8 8 7 5 4 1
5312 9 9 9 5 4 1
5319 8 8 7 5 4 1
5320 3 3 3 5 4 1
5321 3 3 3 5 4 1
5322 3 3 3 5 4 1
5329 3 3 3 5 4 1
5400 9 9 9 5 4 1
5401 9 9 9 5 4 1
5402 9 9 9 5 4 1
5403 9 9 9 5 4 1
5409 9 9 9 5 4 1
5500 9 9 9 5 4 1
5510 9 9 9 5 4 1
5511 9 9 9 5 4 1
5512 9 9 9 5 4 1
5519 9 9 9 5 4 1
5520 9 9 9 5 4 1
5521 9 9 9 5 4 1
5522 9 9 9 5 4 1
5529 9 9 9 5 4 1
5600 9 9 9 5 4 1
5700 8 8 7 5 4 1
5701 8 8 7 5 4 1
5702 8 8 7 5 4 1
5703 8 8 7 5 4 1
5709 8 8 7 5 4 1
5800 8 8 7 5 4 1
5810 8 8 7 5 4 1
5820 2 2 2 5 4 1
5821 2 2 1 5 4 1
5822 1 1 1 5 4 1
5823 2 2 1 5 4 1
5829 2 2 2 5 4 1
5830 7 7 7 5 4 1
5831 1 1 1 5 4 1
5832 7 7 7 5 4 1
5833 8 8 7 5 4 1
5839 . . . 5 4 1
5890 9 9 9 5 4 1
5891 9 9 9 5 4 1
5892 9 9 9 5 4 1
5899 9 9 9 5 4 1
5900 3 3 3 5 4 1
5910 3 3 3 5 4 1
5920 3 3 3 5 4 1
5990 9 9 9 5 4 1
5991 3 3 3 5 4 1
5992 9 9 9 5 4 1
5993 9 9 9 5 4 1
5994 9 9 9 5 4 1
5995 9 9 9 5 4 1
5996 3 3 3 5 4 1
5997 3 3 3 5 4 1
5998 3 3 3 5 4 1
5999 9 9 9 5 4 1
6000 11 11 11 11 11 11
6001 11 11 11 11 11 11
6009 11 11 11 11 11 11
6100 11 11 11 11 11 11
6110 11 11 11 11 11 11
6111 11 11 11 11 11 11
6112 11 11 11 11 11 11
6113 11 11 11 11 11 11
6114 11 11 11 11 11 11
6115 11 11 11 11 11 11
6116 11 11 11 11 11 11
6117 11 11 11 11 11 11
6119 11 11 11 11 11 11
6120 11 11 11 11 11 11
6200 10 10 10 11 11 11
6210 10 10 10 11 11 11
6211 10 10 10 11 11 11
6219 10 10 10 11 11 11
6220 10 10 10 11 11 11
6230 10 10 10 11 11 11
6239 10 10 10 11 11 11
6240 10 10 10 11 11 11
6250 10 10 10 11 11 11
6260 10 10 10 11 11 11
6270 10 10 10 11 11 11
6280 10 10 10 11 11 11
6290 10 10 10 11 11 11
6291 10 10 10 11 11 11
6299 10 10 10 11 11 11
6300 10 10 10 11 11 11
6310 10 10 10 11 11 11
6311 10 10 10 11 11 11
6319 10 10 10 11 11 11
6320 10 10 10 11 11 11
6321 10 10 10 11 11 11
6322 10 10 10 11 11 11
6329 10 10 10 11 11 11
6400 10 10 10 11 11 11
6410 10 10 10 11 11 11
6411 11 11 11 11 11 11
6419 10 10 10 11 11 11
6490 10 10 10 11 11 11
6491 10 10 10 11 11 11
6499 10 10 10 11 11 11
7000 7 7 7 5 4 1
7001 7 7 7 5 4 1
7009 7 7 7 5 4 1
7100 8 8 7 5 4 1
7110 8 8 7 5 4 1
7111 8 8 7 5 4 1
7112 9 9 9 5 4 1
7113 8 8 7 5 4 1
7119 8 8 7 5 4 1
7120 9 9 9 5 4 1
7130 8 8 7 5 4 1
7139 8 8 7 5 4 1
7200 8 8 7 5 4 1
7210 8 8 7 5 4 1
7220 8 8 7 5 4 1
7230 8 8 7 5 4 1
7240 8 8 7 5 4 1
7250 8 8 7 5 4 1
7260 8 8 7 5 4 1
7270 8 8 7 5 4 1
7280 8 8 7 5 4 1
7290 9 9 9 5 4 1
7300 8 8 7 5 4 1
7310 8 8 7 5 4 1
7319 8 8 7 5 4 1
7320 8 8 7 5 4 1
7321 8 8 7 5 4 1
7329 8 8 7 5 4 1
7330 9 9 9 5 4 1
7340 9 9 9 5 4 1
7400 9 9 9 5 4 1
7410 9 9 9 5 4 1
7420 9 9 9 5 4 1
7430 9 9 9 5 4 1
7440 9 9 9 5 4 1
7450 9 9 9 5 4 1
7490 9 9 9 5 4 1
7491 9 9 9 5 4 1
7499 9 9 9 5 4 1
7500 9 9 9 5 4 1
7510 9 9 9 5 4 1
7520 9 9 9 5 4 1
7530 8 8 7 5 4 1
7540 8 8 7 5 4 1
7541 8 8 7 5 4 1
7549 8 8 7 5 4 1
7550 8 8 7 5 4 1
7560 8 8 7 5 4 1
7590 9 9 9 5 4 1
7600 8 8 7 5 4 1
7610 8 8 7 5 4 1
7620 8 8 7 5 4 1
7700 9 9 9 5 4 1
7710 9 9 9 5 4 1
7711 5 4 1 5 4 1
7719 9 9 9 5 4 1
7720 8 8 7 5 4 1
7730 8 8 7 5 4 1
7731 9 9 9 5 4 1
7732 8 8 7 5 4 1
7739 8 8 7 5 4 1
7740 9 9 9 5 4 1
7750 9 9 9 5 4 1
7760 8 8 7 5 4 1
7761 8 8 7 5 4 1
7769 8 8 7 5 4 1
7770 8 8 7 5 4 1
7780 8 8 7 5 4 1
7790 9 9 9 5 4 1
7799 9 9 9 5 4 1
7800 9 9 9 5 4 1
7810 9 9 9 5 4 1
7820 9 9 9 5 4 1
7830 9 9 9 5 4 1
7890 9 9 9 5 4 1
7900 8 8 7 5 4 1
7910 8 8 7 5 4 1
7911 8 8 7 5 4 1
7919 8 8 7 5 4 1
7920 8 8 7 5 4 1
7930 8 8 7 5 4 1
7940 8 8 7 5 4 1
7950 9 9 9 5 4 1
7960 9 9 9 5 4 1
7990 9 9 9 5 4 1
8000 8 8 7 5 4 1
8010 8 8 7 5 4 1
8020 9 9 9 5 4 1
8030 9 9 9 5 4 1
8100 8 8 7 5 4 1
8110 8 8 7 5 4 1
8120 9 9 9 5 4 1
8190 8 8 7 5 4 1
8191 8 8 7 5 4 1
8199 8 8 7 5 4 1
8200 8 8 7 5 4 1
8300 8 8 7 5 4 1
8310 8 8 7 5 4 1
8311 8 8 7 5 4 1
8319 8 8 7 5 4 1
8320 8 8 7 5 4 1
8321 8 8 7 5 4 1
8329 8 8 7 5 4 1
8330 8 8 7 5 4 1
8331 8 8 7 5 4 1
8339 8 8 7 5 4 1
8340 9 9 9 5 4 1
8350 8 8 7 5 4 1
8351 8 8 7 5 4 1
8359 8 8 7 5 4 1
8390 8 8 7 5 4 1
8400 8 8 7 5 4 1
8410 8 8 7 5 4 1
8411 8 8 7 5 4 1
8412 8 8 7 5 4 1
8419 8 8 7 5 4 1
8420 8 8 7 5 4 1
8421 8 8 7 5 4 1
8422 8 8 7 5 4 1
8429 8 8 7 5 4 1
8430 8 8 7 5 4 1
8431 8 8 7 5 4 1
8439 8 8 7 5 4 1
8440 8 8 7 5 4 1
8490 8 8 7 5 4 1
8491 9 9 9 5 4 1
8492 9 9 9 5 4 1
8493 9 9 9 5 4 1
8494 9 9 9 5 4 1
8499 8 8 7 5 4 1
8500 8 8 7 5 4 1
8510 8 8 7 5 4 1
8520 8 8 7 5 4 1
8530 9 9 9 5 4 1
8540 8 8 7 5 4 1
8550 8 8 7 5 4 1
8551 5 4 1 5 4 1
8559 8 8 7 5 4 1
8560 8 8 7 5 4 1
8570 8 8 7 5 4 1
8590 8 8 7 5 4 1
8600 8 8 7 5 4 1
8610 8 8 7 5 4 1
8620 3 3 3 3 3 3
8700 8 8 7 5 4 1
8710 8 8 7 5 4 1
8711 5 4 1 5 4 1
8719 8 8 7 5 4 1
8720 8 8 7 5 4 1
8730 8 8 7 5 4 1
8731 8 8 7 5 4 1
8732 8 8 7 5 4 1
8733 8 8 7 5 4 1
8739 8 8 7 5 4 1
8740 8 8 7 5 4 1
8800 8 8 7 5 4 1
8801 8 8 7 5 4 1
8809 8 8 7 5 4 1
8900 8 8 7 5 4 1
8910 8 8 7 5 4 1
8911 8 8 7 5 4 1
8919 8 8 7 5 4 1
8920 8 8 7 5 4 1
8930 9 9 9 5 4 1
8940 9 9 9 5 4 1
8950 9 9 9 5 4 1
8990 9 9 9 5 4 1
9000 9 9 9 5 4 1
9010 9 9 9 5 4 1
9020 9 9 9 5 4 1
9100 9 9 9 5 4 1
9200 8 8 7 5 4 1
9210 8 8 7 5 4 1
9211 8 8 7 5 4 1
9219 8 8 7 5 4 1
9220 8 8 7 5 4 1
9230 8 8 7 5 4 1
9240 8 8 7 5 4 1
9250 8 8 7 5 4 1
9260 8 8 7 5 4 1
9270 8 8 7 5 4 1
9290 8 8 7 5 4 1
9300 8 8 7 5 4 1
9310 8 8 7 5 4 1
9311 8 8 7 5 4 1
9319 8 8 7 5 4 1
9390 9 9 9 5 4 1
9400 9 9 9 5 4 1
9410 8 8 7 5 4 1
9420 9 9 9 5 4 1
9430 8 8 7 5 4 1
9490 8 8 7 5 4 1
9491 8 8 7 5 4 1
9492 8 8 7 5 4 1
9493 9 9 9 5 4 1
9499 8 8 7 5 4 1
9500 8 8 7 5 4 1
9510 8 8 7 5 4 1
9520 8 8 7 5 4 1
9530 9 9 9 5 4 1
9540 8 8 7 5 4 1
9541 8 8 7 5 4 1
9542 9 9 9 5 4 1
9549 8 8 7 5 4 1
9550 8 8 7 5 4 1
9551 8 8 7 5 4 1
9559 8 8 7 5 4 1
9560 9 9 9 5 4 1
9570 9 9 9 5 4 1
9590 8 8 7 5 4 1
9591 8 8 7 5 4 1
9592 8 8 7 5 4 1
9593 8 8 7 5 4 1
9594 9 9 9 5 4 1
9595 9 9 9 5 4 1
9596 8 8 7 5 4 1
9599 8 8 7 5 4 1
9600 9 9 9 5 4 1
9610 9 9 9 5 4 1
9690 9 9 9 5 4 1
9700 9 9 9 5 4 1
9710 9 9 9 5 4 1
9711 9 9 9 5 4 1
9712 9 9 9 5 4 1
9713 9 9 9 5 4 1
9714 9 9 9 5 4 1
9719 9 9 9 5 4 1
9720 8 8 7 5 4 1
9730 8 8 7 5 4 1
9731 9 9 9 5 4 1
9739 8 8 7 5 4 1
9740 8 8 7 5 4 1
9790 9 9 9 5 4 1
9800 9 9 9 5 4 1
9810 9 9 9 5 4 1
9811 9 9 9 5 4 1
9819 9 9 9 5 4 1
9820 9 9 9 5 4 1
9830 9 9 9 5 4 1
9831 9 9 9 5 4 1
9832 9 9 9 5 4 1
9839 8 8 7 5 4 1
9840 9 9 9 5 4 1
9850 9 9 9 5 4 1
9851 9 9 9 5 4 1
9852 9 9 9 5 4 1
9853 9 9 9 5 4 1
9854 9 9 9 5 4 1
9855 3 3 3 3 3 3
9859 9 9 9 5 4 1
9860 9 9 9 5 4 1
9861 9 9 9 5 4 1
9869 9 9 9 5 4 1
9890 9 9 9 5 4 1
9891 9 9 9 5 4 1
9899 9 9 9 5 4 1
9900 9 9 9 5 4 1
9950 8 8 7 5 4 1
9951 5 4 1 5 4 1
9959 8 8 7 5 4 1
9970 9 9 9 5 4 1
9971 9 9 9 5 4 1
9979 9 9 9 5 4 1
9990 9 9 9 5 4 1
9991 9 9 9 5 4 1
9992 9 9 9 5 4 1
9993 9 9 9 5 4 1
9994 9 9 9 5 4 1
9995 9 9 9 5 4 1
9996 9 9 9 5 4 1
9997 9 9 9 5 4 1
9999 9 9 9 5 4 1
10000 7 7 7 5 4 1
10001 1 1 1 1 1 1
10002 7 7 7 5 4 1
10003 8 8 7 5 4 1
% END

% CODELIST-isco68-egp11
% source: iscoegp.sps from http://www.harryganzeboom.nl/isco68/
% note: the codelist has been generated automatically by applying iscoegp.sps
%       to all relevant combinations of isco codes, selfemployment, supervision
% variables: ISCO-68 EGP(0,0) EGB(0,1-9) EGP(0,10+) EGP(1,0) EGB(1,1-9) 
%       EGP(1,10+), where the first argument is selfemployment status
%       and the second argument is the number if subordinates/employees
0100 1 1 1 1 1 1
0110 1 1 1 1 1 1
0120 1 1 1 1 1 1
0130 1 1 1 1 1 1
0131 1 1 1 1 1 1
0132 1 1 1 1 1 1
0133 1 1 1 1 1 1
0139 1 1 1 1 1 1
0140 2 2 2 2 2 2
0200 1 1 1 1 1 1
0210 1 1 1 1 1 1
0220 1 1 1 1 1 1
0230 1 1 1 1 1 1
0240 1 1 1 1 1 1
0250 1 1 1 1 1 1
0260 1 1 1 1 1 1
0270 1 1 1 1 1 1
0280 1 1 1 1 1 1
0290 1 1 1 1 1 1
0300 2 2 2 2 2 2
0310 2 2 2 2 2 2
0320 2 2 2 2 2 2
0321 3 3 3 3 3 3
0329 2 2 2 2 2 2
0330 2 2 2 2 2 2
0339 2 2 2 2 2 2
0340 2 2 2 2 2 2
0350 2 2 2 2 2 2
0360 2 2 2 2 2 2
0370 2 2 2 2 2 2
0380 2 2 2 2 2 2
0390 2 2 2 2 2 2
0400 1 1 1 1 1 1
0409 1 1 1 1 1 1
0410 1 1 1 1 1 1
0411 1 1 1 1 1 1
0419 1 1 1 1 1 1
0420 1 1 1 1 1 1
0421 2 2 1 6 5 1
0429 1 1 1 1 1 1
0430 1 1 1 1 1 1
0500 1 1 1 1 1 1
0510 1 1 1 1 1 1
0520 1 1 1 1 1 1
0521 1 1 1 1 1 1
0529 1 1 1 1 1 1
0530 1 1 1 1 1 1
0531 1 1 1 1 1 1
0539 1 1 1 1 1 1
0540 2 2 2 2 2 2
0541 2 2 2 2 2 2
0549 2 2 2 2 2 2
0600 1 1 1 1 1 1
0610 1 1 1 1 1 1
0611 1 1 1 1 1 1
0619 1 1 1 1 1 1
0620 2 2 2 2 2 2
0630 1 1 1 1 1 1
0640 2 2 2 2 2 2
0650 1 1 1 1 1 1
0660 2 2 2 2 2 2
0670 1 1 1 1 1 1
0680 2 2 2 2 2 2
0690 2 2 2 2 2 2
0700 2 2 2 2 2 2
0710 2 2 2 2 2 2
0711 2 2 1 2 2 1
0715 2 2 2 2 2 2
0719 2 2 2 2 2 2
0720 4 4 4 4 4 4
0730 2 2 2 2 2 2
0740 2 2 2 2 2 2
0750 2 2 2 2 2 2
0751 2 2 2 2 2 2
0759 2 2 2 2 2 2
0760 2 2 2 2 2 2
0761 2 2 2 2 2 2
0762 8 8 7 6 5 1
0769 2 2 2 2 2 2
0770 2 2 2 2 2 2
0780 2 2 2 2 2 2
0790 2 2 2 2 2 2
0791 2 2 2 2 2 2
0792 2 2 2 2 2 2
0793 2 2 1 2 2 1
0799 2 2 2 2 2 2
0800 1 1 1 1 1 1
0810 1 1 1 1 1 1
0820 1 1 1 1 1 1
0830 2 2 2 2 2 2
0840 2 2 2 2 2 2
0849 2 2 2 2 2 2
0900 1 1 1 1 1 1
1100 1 1 1 1 1 1
1101 1 1 1 1 1 1
1109 1 1 1 1 1 1
1200 1 1 1 1 1 1
1210 1 1 1 1 1 1
1211 1 1 1 1 1 1
1219 1 1 1 1 1 1
1220 1 1 1 1 1 1
1221 1 1 1 1 1 1
1222 1 1 1 1 1 1
1229 1 1 1 1 1 1
1290 1 1 1 1 1 1
1291 2 2 2 2 2 2
1299 1 1 1 1 1 1
1300 2 2 2 2 2 2
1310 1 1 1 1 1 1
1311 1 1 1 1 1 1
1319 1 1 1 1 1 1
1320 2 2 2 2 2 2
1321 2 2 2 2 2 2
1329 2 2 2 2 2 2
1330 2 2 2 2 2 2
1340 2 2 2 2 2 2
1350 2 2 2 2 2 2
1390 2 2 2 2 2 2
1391 2 2 1 2 2 1
1392 1 1 1 1 1 1
1393 3 3 3 3 3 3
1394 1 1 1 1 1 1
1399 2 2 2 2 2 2
1400 2 2 2 2 2 2
1410 2 2 2 2 2 2
1411 1 1 1 1 1 1
1412 2 2 2 2 2 2
1413 2 2 2 2 2 2
1414 2 2 2 2 2 2
1415 2 2 2 2 2 2
1416 2 2 2 2 2 2
1419 2 2 2 2 2 2
1490 2 2 2 2 2 2
1491 0 0 0 0 0 0
1499 2 2 2 2 2 2
1500 2 2 2 2 2 2
1510 2 2 2 2 2 2
1511 2 2 2 2 2 2
1519 2 2 2 2 2 2
1590 2 2 2 2 2 2
1591 2 2 1 2 2 1
1592 2 2 2 2 2 2
1593 2 2 2 2 2 2
1599 2 2 2 2 2 2
1600 2 2 2 2 2 2
1610 2 2 2 2 2 2
1620 2 2 2 2 2 2
1621 2 2 2 2 2 2
1622 2 2 2 2 2 2
1629 2 2 2 2 2 2
1630 2 2 2 6 5 1
1631 2 2 2 6 5 1
1639 2 2 2 2 2 2
1700 2 2 2 2 2 2
1710 2 2 2 2 2 2
1711 2 2 2 2 2 2
1712 2 2 2 2 2 2
1713 2 2 2 2 2 2
1719 2 2 2 2 2 2
1720 2 2 2 2 2 2
1721 2 2 2 2 2 2
1729 2 2 2 2 2 2
1730 2 2 2 2 2 2
1731 2 2 2 2 2 2
1732 2 2 2 2 2 2
1739 2 2 2 2 2 2
1740 1 1 1 1 1 1
1749 2 2 2 2 2 2
1750 2 2 2 2 2 2
1790 2 2 2 2 2 2
1791 2 2 2 2 2 2
1799 2 2 2 2 2 2
1800 2 2 2 2 2 2
1801 2 2 2 2 2 2
1809 2 2 2 2 2 2
1900 1 1 1 1 1 1
1910 2 2 2 2 2 2
1920 1 1 1 1 1 1
1921 1 1 1 1 1 1
1922 1 1 1 1 1 1
1923 1 1 1 1 1 1
1924 1 1 1 1 1 1
1929 1 1 1 1 1 1
1930 2 2 2 2 2 2
1931 2 2 2 2 2 2
1939 2 2 2 2 2 2
1940 2 2 2 2 2 2
1941 2 2 2 2 2 2
1949 2 2 2 2 2 2
1950 2 2 2 2 2 2
1951 1 1 1 1 1 1
1959 2 2 2 2 2 2
1960 1 1 1 1 1 1
1990 2 2 2 2 2 2
1991 2 2 2 2 2 2
1992 2 2 2 2 2 2
1993 2 2 2 2 2 2
1994 2 2 2 2 2 2
1995 2 2 2 2 2 2
1999 2 2 2 2 2 2
2000 1 1 1 1 1 1
2010 1 1 1 1 1 1
2011 1 1 1 1 1 1
2012 1 1 1 1 1 1
2013 1 1 1 1 1 1
2014 1 1 1 1 1 1
2015 1 1 1 1 1 1
2019 1 1 1 1 1 1
2020 1 1 1 1 1 1
2021 1 1 1 1 1 1
2022 1 1 1 1 1 1
2023 1 1 1 1 1 1
2024 1 1 1 1 1 1
2029 1 1 1 1 1 1
2030 1 1 1 1 1 1
2031 1 1 1 1 1 1
2032 1 1 1 1 1 1
2033 1 1 1 1 1 1
2034 1 1 1 1 1 1
2035 1 1 1 1 1 1
2036 1 1 1 1 1 1
2039 1 1 1 1 1 1
2100 2 2 1 2 2 1
2110 2 2 1 2 2 1
2111 1 1 1 1 1 1
2112 2 2 1 2 2 1
2113 2 2 1 2 2 1
2114 1 1 1 1 1 1
2115 1 1 1 1 1 1
2116 2 2 1 2 2 1
2119 1 1 1 1 1 1
2120 2 2 1 2 2 1
2190 2 2 1 2 2 1
2191 2 2 1 2 2 1
2192 1 1 1 1 1 1
2193 1 1 1 1 1 1
2194 1 1 1 1 1 1
2195 1 1 1 1 1 1
2196 2 2 1 2 2 1
2197 1 1 1 1 1 1
2199 2 2 2 2 2 2
3000 2 2 1 6 5 1
3009 2 2 2 2 2 2
3100 2 2 1 2 2 1
3101 2 2 1 2 2 1
3102 2 2 1 2 2 1
3103 2 2 1 2 2 1
3104 2 2 1 2 2 1
3109 2 2 2 2 2 2
3200 3 3 3 3 3 3
3210 3 3 3 3 3 3
3211 3 3 3 3 3 3
3219 3 3 3 3 3 3
3220 3 3 3 3 3 3
3300 3 3 3 6 5 1
3310 3 3 3 6 5 1
3311 3 3 3 6 5 1
3312 3 2 1 6 5 1
3313 3 3 3 6 5 1
3314 3 3 3 6 5 1
3315 3 3 3 6 5 1
3319 3 3 3 6 5 1
3390 3 3 3 6 5 1
3391 3 3 3 6 5 1
3399 3 3 3 3 3 3
3400 3 3 3 3 3 3
3410 3 3 3 3 3 3
3420 3 3 3 3 3 3
3500 2 2 1 2 2 1
3510 2 2 1 2 2 1
3520 2 2 1 6 5 1
3590 3 2 1 3 2 1
3600 3 3 3 3 3 3
3601 3 3 3 3 3 3
3602 3 3 3 3 3 3
3609 3 3 3 3 3 3
3700 4 4 4 6 5 1
3701 4 4 4 4 4 4
3709 4 4 4 4 4 4
3800 3 3 3 3 3 3
3801 3 3 3 3 3 3
3802 3 3 3 3 3 3
3809 3 3 3 3 3 3
3900 3 3 3 3 3 3
3910 3 3 3 3 3 3
3911 3 3 3 3 3 3
3919 4 4 4 4 4 4
3920 3 3 3 3 3 3
3930 3 3 3 3 3 3
3931 3 3 3 3 3 3
3932 3 3 3 3 3 3
3939 3 3 3 3 3 3
3940 3 3 3 3 3 3
3941 3 3 3 3 3 3
3942 3 3 3 3 3 3
3943 3 3 3 3 3 3
3944 3 3 3 3 3 3
3949 3 3 3 3 3 3
3950 3 3 3 3 3 3
3951 3 3 3 3 3 3
3959 3 3 3 3 3 3
3990 3 3 3 3 3 3
3991 3 3 3 3 3 3
3992 3 3 3 3 3 3
3993 3 3 3 3 3 3
3999 3 3 3 3 3 3
4000 2 2 1 6 5 1
4001 2 2 1 6 5 1
4002 2 2 1 6 5 1
4009 2 2 1 2 2 1
4100 6 5 1 6 5 1
4101 6 5 1 6 5 1
4102 6 5 1 6 5 1
4103 6 5 1 6 5 1
4104 6 5 1 6 5 1
4105 6 5 1 6 5 1
4106 6 5 1 6 5 1
4107 0 0 0 0 0 0
4108 6 5 1 6 5 1
4109 6 5 1 6 5 1
4200 2 2 1 2 2 1
4210 2 2 1 6 5 1
4220 2 2 1 2 2 1
4221 2 2 2 2 2 2
4222 2 2 2 2 2 2
4229 2 2 2 2 2 2
4300 2 2 2 6 5 1
4310 2 2 2 6 5 1
4311 3 3 3 6 5 1
4319 2 2 2 6 5 1
4320 3 3 3 6 5 1
4400 2 2 2 6 5 1
4410 2 2 1 2 2 1
4411 2 2 1 2 2 1
4412 2 2 1 2 2 1
4419 2 2 2 2 2 2
4420 2 2 2 6 5 1
4430 2 2 2 6 5 1
4431 2 2 2 6 5 1
4432 2 2 2 6 5 1
4439 2 2 2 6 5 1
4500 4 4 4 6 5 1
4510 4 4 4 6 5 1
4511 4 4 4 6 5 1
4512 9 9 9 6 5 1
4513 4 4 4 6 5 1
4514 4 4 4 6 5 1
4519 4 4 4 6 5 1
4520 6 5 1 6 5 1
4521 6 5 1 6 5 1
4522 4 4 4 6 5 1
4523 4 4 4 6 5 1
4524 9 9 9 6 5 1
4525 0 0 0 6 5 1
4529 6 5 1 6 5 1
4900 4 4 4 6 5 1
5000 2 2 1 6 5 1
5001 2 2 1 6 5 1
5002 2 2 1 6 5 1
5009 2 2 1 2 2 1
5100 6 5 1 6 5 1
5101 6 5 1 6 5 1
5102 6 5 1 6 5 1
5103 6 5 1 6 5 1
5104 6 5 1 6 5 1
5109 6 5 1 6 5 1
5200 3 2 1 3 2 1
5201 3 2 1 3 2 1
5209 3 3 3 3 3 3
5300 8 8 7 6 5 1
5310 8 8 7 6 5 1
5311 8 8 7 6 5 1
5312 9 9 9 6 5 1
5319 8 8 7 6 5 1
5320 4 4 4 6 5 1
5321 4 4 4 6 5 1
5322 4 4 4 6 5 1
5329 4 4 4 6 5 1
5400 9 9 9 6 5 1
5401 9 9 9 6 5 1
5402 9 9 9 6 5 1
5403 9 9 9 6 5 1
5409 9 9 9 6 5 1
5500 9 9 9 6 5 1
5510 9 9 9 6 5 1
5511 9 9 9 6 5 1
5512 9 9 9 6 5 1
5519 9 9 9 6 5 1
5520 9 9 9 6 5 1
5521 9 9 9 6 5 1
5522 9 9 9 6 5 1
5529 9 9 9 6 5 1
5600 9 9 9 6 5 1
5700 8 8 7 6 5 1
5701 8 8 7 6 5 1
5702 8 8 7 6 5 1
5703 8 8 7 6 5 1
5709 8 8 7 6 5 1
5800 8 8 7 6 5 1
5810 8 8 7 6 5 1
5820 2 2 2 6 5 1
5821 2 2 1 6 5 1
5822 1 1 1 6 5 1
5823 2 2 1 6 5 1
5829 2 2 2 6 5 1
5830 7 7 7 6 5 1
5831 1 1 1 6 5 1
5832 7 7 7 6 5 1
5833 8 8 7 6 5 1
5839 . . . 6 5 1
5890 9 9 9 6 5 1
5891 9 9 9 6 5 1
5892 9 9 9 6 5 1
5899 9 9 9 6 5 1
5900 4 4 4 6 5 1
5910 4 4 4 6 5 1
5920 3 3 3 6 5 1
5990 9 9 9 6 5 1
5991 4 4 4 6 5 1
5992 9 9 9 6 5 1
5993 9 9 9 6 5 1
5994 9 9 9 6 5 1
5995 9 9 9 6 5 1
5996 3 3 3 6 5 1
5997 3 3 3 6 5 1
5998 3 3 3 6 5 1
5999 9 9 9 6 5 1
6000 11 11 11 11 11 11
6001 11 11 11 11 11 11
6009 11 11 11 11 11 11
6100 11 11 11 11 11 11
6110 11 11 11 11 11 11
6111 11 11 11 11 11 11
6112 11 11 11 11 11 11
6113 11 11 11 11 11 11
6114 11 11 11 11 11 11
6115 11 11 11 11 11 11
6116 11 11 11 11 11 11
6117 11 11 11 11 11 11
6119 11 11 11 11 11 11
6120 11 11 11 11 11 11
6200 10 10 10 11 11 11
6210 10 10 10 11 11 11
6211 10 10 10 11 11 11
6219 10 10 10 11 11 11
6220 10 10 10 11 11 11
6230 10 10 10 11 11 11
6239 10 10 10 11 11 11
6240 10 10 10 11 11 11
6250 10 10 10 11 11 11
6260 10 10 10 11 11 11
6270 10 10 10 11 11 11
6280 10 10 10 11 11 11
6290 10 10 10 11 11 11
6291 10 10 10 11 11 11
6299 10 10 10 11 11 11
6300 10 10 10 11 11 11
6310 10 10 10 11 11 11
6311 10 10 10 11 11 11
6319 10 10 10 11 11 11
6320 10 10 10 11 11 11
6321 10 10 10 11 11 11
6322 10 10 10 11 11 11
6329 10 10 10 11 11 11
6400 10 10 10 11 11 11
6410 10 10 10 11 11 11
6411 11 11 11 11 11 11
6419 10 10 10 11 11 11
6490 10 10 10 11 11 11
6491 10 10 10 11 11 11
6499 10 10 10 11 11 11
7000 7 7 7 6 5 1
7001 7 7 7 6 5 1
7009 7 7 7 6 5 1
7100 8 8 7 6 5 1
7110 8 8 7 6 5 1
7111 8 8 7 6 5 1
7112 9 9 9 6 5 1
7113 8 8 7 6 5 1
7119 8 8 7 6 5 1
7120 9 9 9 6 5 1
7130 8 8 7 6 5 1
7139 8 8 7 6 5 1
7200 8 8 7 6 5 1
7210 8 8 7 6 5 1
7220 8 8 7 6 5 1
7230 8 8 7 6 5 1
7240 8 8 7 6 5 1
7250 8 8 7 6 5 1
7260 8 8 7 6 5 1
7270 8 8 7 6 5 1
7280 8 8 7 6 5 1
7290 9 9 9 6 5 1
7300 8 8 7 6 5 1
7310 8 8 7 6 5 1
7319 8 8 7 6 5 1
7320 8 8 7 6 5 1
7321 8 8 7 6 5 1
7329 8 8 7 6 5 1
7330 9 9 9 6 5 1
7340 9 9 9 6 5 1
7400 9 9 9 6 5 1
7410 9 9 9 6 5 1
7420 9 9 9 6 5 1
7430 9 9 9 6 5 1
7440 9 9 9 6 5 1
7450 9 9 9 6 5 1
7490 9 9 9 6 5 1
7491 9 9 9 6 5 1
7499 9 9 9 6 5 1
7500 9 9 9 6 5 1
7510 9 9 9 6 5 1
7520 9 9 9 6 5 1
7530 8 8 7 6 5 1
7540 8 8 7 6 5 1
7541 8 8 7 6 5 1
7549 8 8 7 6 5 1
7550 8 8 7 6 5 1
7560 8 8 7 6 5 1
7590 9 9 9 6 5 1
7600 8 8 7 6 5 1
7610 8 8 7 6 5 1
7620 8 8 7 6 5 1
7700 9 9 9 6 5 1
7710 9 9 9 6 5 1
7711 6 5 1 6 5 1
7719 9 9 9 6 5 1
7720 8 8 7 6 5 1
7730 8 8 7 6 5 1
7731 9 9 9 6 5 1
7732 8 8 7 6 5 1
7739 8 8 7 6 5 1
7740 9 9 9 6 5 1
7750 9 9 9 6 5 1
7760 8 8 7 6 5 1
7761 8 8 7 6 5 1
7769 8 8 7 6 5 1
7770 8 8 7 6 5 1
7780 8 8 7 6 5 1
7790 9 9 9 6 5 1
7799 9 9 9 6 5 1
7800 9 9 9 6 5 1
7810 9 9 9 6 5 1
7820 9 9 9 6 5 1
7830 9 9 9 6 5 1
7890 9 9 9 6 5 1
7900 8 8 7 6 5 1
7910 8 8 7 6 5 1
7911 8 8 7 6 5 1
7919 8 8 7 6 5 1
7920 8 8 7 6 5 1
7930 8 8 7 6 5 1
7940 8 8 7 6 5 1
7950 9 9 9 6 5 1
7960 9 9 9 6 5 1
7990 9 9 9 6 5 1
8000 8 8 7 6 5 1
8010 8 8 7 6 5 1
8020 9 9 9 6 5 1
8030 9 9 9 6 5 1
8100 8 8 7 6 5 1
8110 8 8 7 6 5 1
8120 9 9 9 6 5 1
8190 8 8 7 6 5 1
8191 8 8 7 6 5 1
8199 8 8 7 6 5 1
8200 8 8 7 6 5 1
8300 8 8 7 6 5 1
8310 8 8 7 6 5 1
8311 8 8 7 6 5 1
8319 8 8 7 6 5 1
8320 8 8 7 6 5 1
8321 8 8 7 6 5 1
8329 8 8 7 6 5 1
8330 8 8 7 6 5 1
8331 8 8 7 6 5 1
8339 8 8 7 6 5 1
8340 9 9 9 6 5 1
8350 8 8 7 6 5 1
8351 8 8 7 6 5 1
8359 8 8 7 6 5 1
8390 8 8 7 6 5 1
8400 8 8 7 6 5 1
8410 8 8 7 6 5 1
8411 8 8 7 6 5 1
8412 8 8 7 6 5 1
8419 8 8 7 6 5 1
8420 8 8 7 6 5 1
8421 8 8 7 6 5 1
8422 8 8 7 6 5 1
8429 8 8 7 6 5 1
8430 8 8 7 6 5 1
8431 8 8 7 6 5 1
8439 8 8 7 6 5 1
8440 8 8 7 6 5 1
8490 8 8 7 6 5 1
8491 9 9 9 6 5 1
8492 9 9 9 6 5 1
8493 9 9 9 6 5 1
8494 9 9 9 6 5 1
8499 8 8 7 6 5 1
8500 8 8 7 6 5 1
8510 8 8 7 6 5 1
8520 8 8 7 6 5 1
8530 9 9 9 6 5 1
8540 8 8 7 6 5 1
8550 8 8 7 6 5 1
8551 6 5 1 6 5 1
8559 8 8 7 6 5 1
8560 8 8 7 6 5 1
8570 8 8 7 6 5 1
8590 8 8 7 6 5 1
8600 8 8 7 6 5 1
8610 8 8 7 6 5 1
8620 3 3 3 3 3 3
8700 8 8 7 6 5 1
8710 8 8 7 6 5 1
8711 6 5 1 6 5 1
8719 8 8 7 6 5 1
8720 8 8 7 6 5 1
8730 8 8 7 6 5 1
8731 8 8 7 6 5 1
8732 8 8 7 6 5 1
8733 8 8 7 6 5 1
8739 8 8 7 6 5 1
8740 8 8 7 6 5 1
8800 8 8 7 6 5 1
8801 8 8 7 6 5 1
8809 8 8 7 6 5 1
8900 8 8 7 6 5 1
8910 8 8 7 6 5 1
8911 8 8 7 6 5 1
8919 8 8 7 6 5 1
8920 8 8 7 6 5 1
8930 9 9 9 6 5 1
8940 9 9 9 6 5 1
8950 9 9 9 6 5 1
8990 9 9 9 6 5 1
9000 9 9 9 6 5 1
9010 9 9 9 6 5 1
9020 9 9 9 6 5 1
9100 9 9 9 6 5 1
9200 8 8 7 6 5 1
9210 8 8 7 6 5 1
9211 8 8 7 6 5 1
9219 8 8 7 6 5 1
9220 8 8 7 6 5 1
9230 8 8 7 6 5 1
9240 8 8 7 6 5 1
9250 8 8 7 6 5 1
9260 8 8 7 6 5 1
9270 8 8 7 6 5 1
9290 8 8 7 6 5 1
9300 8 8 7 6 5 1
9310 8 8 7 6 5 1
9311 8 8 7 6 5 1
9319 8 8 7 6 5 1
9390 9 9 9 6 5 1
9400 9 9 9 6 5 1
9410 8 8 7 6 5 1
9420 9 9 9 6 5 1
9430 9 9 9 6 5 1
9490 8 8 7 6 5 1
9491 8 8 7 6 5 1
9492 8 8 7 6 5 1
9493 9 9 9 6 5 1
9499 8 8 7 6 5 1
9500 8 8 7 6 5 1
9510 8 8 7 6 5 1
9520 8 8 7 6 5 1
9530 9 9 9 6 5 1
9540 8 8 7 6 5 1
9541 8 8 7 6 5 1
9542 9 9 9 6 5 1
9549 8 8 7 6 5 1
9550 8 8 7 6 5 1
9551 8 8 7 6 5 1
9559 8 8 7 6 5 1
9560 9 9 9 6 5 1
9570 9 9 9 6 5 1
9590 8 8 7 6 5 1
9591 8 8 7 6 5 1
9592 8 8 7 6 5 1
9593 8 8 7 6 5 1
9594 9 9 9 6 5 1
9595 9 9 9 6 5 1
9596 8 8 7 6 5 1
9599 8 8 7 6 5 1
9600 9 9 9 6 5 1
9610 9 9 9 6 5 1
9690 9 9 9 6 5 1
9700 9 9 9 6 5 1
9710 9 9 9 6 5 1
9711 9 9 9 6 5 1
9712 9 9 9 6 5 1
9713 9 9 9 6 5 1
9714 9 9 9 6 5 1
9719 9 9 9 6 5 1
9720 8 8 7 6 5 1
9730 8 8 7 6 5 1
9731 9 9 9 6 5 1
9739 8 8 7 6 5 1
9740 8 8 7 6 5 1
9790 9 9 9 6 5 1
9800 9 9 9 6 5 1
9810 9 9 9 6 5 1
9811 9 9 9 6 5 1
9819 9 9 9 6 5 1
9820 9 9 9 6 5 1
9830 9 9 9 6 5 1
9831 9 9 9 6 5 1
9832 9 9 9 6 5 1
9839 8 8 7 6 5 1
9840 9 9 9 6 5 1
9850 9 9 9 6 5 1
9851 9 9 9 6 5 1
9852 9 9 9 6 5 1
9853 9 9 9 6 5 1
9854 9 9 9 6 5 1
9855 3 3 3 3 3 3
9859 9 9 9 6 5 1
9860 9 9 9 6 5 1
9861 9 9 9 6 5 1
9869 9 9 9 6 5 1
9890 9 9 9 6 5 1
9891 9 9 9 6 5 1
9899 9 9 9 6 5 1
9900 9 9 9 6 5 1
9950 8 8 7 6 5 1
9951 6 5 1 6 5 1
9959 8 8 7 6 5 1
9970 9 9 9 6 5 1
9971 9 9 9 6 5 1
9979 9 9 9 6 5 1
9990 9 9 9 6 5 1
9991 9 9 9 6 5 1
9992 9 9 9 6 5 1
9993 9 9 9 6 5 1
9994 9 9 9 6 5 1
9995 9 9 9 6 5 1
9996 9 9 9 6 5 1
9997 9 9 9 6 5 1
9999 9 9 9 6 5 1
10000 7 7 7 6 5 1
10001 1 1 1 1 1 1
10002 7 7 7 6 5 1
10003 8 8 7 6 5 1
% END

% CODELIST-isco88-isco68
% source: isco8868.sps from http://www.harryganzeboom.nl/isco68/
% variables: ISCO-88 ISCO-68
8400 9970
7234 8494
7520 9950
1000 2000
1100 2010
1110 2020
1120 2030
1130 2015
1140 2110
1141 2195
1142 2196
1143 2190
1200 2110
1210 2119
1220 2120
1221 6000
1222 2120
1223 2120
1224 4000
1225 5000
1226 3500
1227 2199
1228 2120
1229 1740
1230 2192
1231 3009
1232 1949
1233 4200
1234 2192
1235 2192
1236 2192
1237 2192
1239 2192
1240 2192
1250 5832
1300 2110
1310 2115
1311 6009
1312 2113
1313 2116
1314 4000
1315 5000
1316 2113
1317 2113
1318 2113
1319 2113
2000 1960
2100 0100
2110 0100
2111 0120
2112 0133
2113 0110
2114 0131
2120 0800
2121 0820
2122 0810
2130 0840
2131 0830
2132 0849
2139 0840
2140 0200
2141 0210
2142 0220
2143 0230
2144 0230
2145 0240
2146 0250
2147 0260
2148 0310
2149 0290
2200 0500
2210 0500
2211 0510
2212 0520
2213 0530
2220 0600
2221 0610
2222 0630
2223 0650
2224 0670
2229 0790
2230 0710
2300 1300
2310 1310
2320 1320
2321 1329
2322 1321
2323 1399
2330 1330
2331 1330
2332 1340
2340 1350
2350 1390
2351 1924
2352 1392
2359 1390
2400 1990
2410 1100
2411 1100
2412 1940
2419 1593
2420 1200
2421 1210
2422 1220
2429 1290
2430 1910
2431 1910
2432 1910
2440 1920
2441 0900
2442 1920
2443 1923
2444 1950
2445 1921
2446 1930
2450 1600
2451 1510
2452 1610
2453 1710
2454 1720
2455 1730
2460 1400
3000 1990
3100 1999
3110 0140
3111 0140
3112 0330
3113 0340
3114 0340
3115 0350
3116 0360
3117 0370
3118 0320
3119 0390
3120 3400
3121 3420
3122 3420
3123 0390
3130 8600
3131 1630
3132 3800
3133 0770
3139 8620
3140 0400
3141 0430
3142 0420
3143 0410
3144 3590
3145 3590
3150 3102
3151 5890
3152 9499
3200 0700
3210 0540
3211 0540
3212 0541
3213 0531
3220 0790
3221 0620
3222 0793
3223 0690
3224 0750
3225 0640
3226 0760
3227 0660
3228 0680
3229 0790
3230 0720
3231 0720
3232 0740
3240 0792
3241 0792
3242 1491
3300 1393
3310 1391
3320 1993
3330 1393
3340 1994
3400 1990
3410 4400
3411 4410
3412 4419
3413 4411
3414 1990
3415 4300
3416 4220
3417 4430
3419 3390
3420 4400
3422 4410
3421 4104
3423 4108
3429 4420
3430 3300
3431 3210
3432 3932
3433 3310
3434 0840
3439 3211
3440 3100
3441 3103
3442 3104
3443 3109
3444 3102
3449 3100
3450 5820
3451 5820
3452 5832
3460 1930
3470 1700
3471 1620
3472 1790
3473 1712
3474 1750
3475 1800
3480 1490
4000 3000
4100 3930
4110 3200
4111 3210
4112 3219
4113 3220
4114 3410
4115 3211
4120 3390
4121 3399
4122 3391
4130 3920
4131 3910
4132 3920
4133 3911
4140 3950
4141 3950
4142 3709
4143 3999
4144 3930
4190 3990
4200 3310
4210 3310
4211 3311
4212 3313
4213 5997
4214 4900
4215 3391
4220 3940
4221 3943
4222 3949
4223 3809
5000 5990
5100 5900
5111 5996
5112 3600
5113 5910
5110 3600
5120 5300
5121 5201
5122 5310
5123 5320
5130 5400
5131 5401
5132 5999
5133 5409
5139 5990
5140 5990
5141 5700
5142 5409
5143 5920
5149 5990
5150 1991
5151 1991
5152 1991
5160 5800
5161 5810
5162 5829
5163 5891
5164 5833
5169 5890
5200 4500
5210 4513
5220 4510
5230 4529
6000 6220
6100 6220
6110 6220
6111 6220
6112 6230
6113 6270
6114 6110
6120 6240
6121 6240
6122 6260
6123 6290
6124 6240
6129 6240
6130 6100
6131 6119
6133 6119
6140 6300
6141 6310
6142 7491
6150 6400
6151 6490
6152 6410
6153 6410
6154 6491
6200 6290
6210 6290
7000 9950
7100 9500
7110 7100
7111 7110
7112 7110
7113 8200
7120 9500
7121 9590
7122 9510
7123 9520
7124 9540
7129 9590
7130 9500
7131 9530
7132 9540
7133 9550
7134 9560
7135 9570
7136 8710
7137 8550
7140 9300
7141 9310
7142 9390
7143 5520
7200 7250
7210 7200
7211 7250
7212 8720
7213 8730
7214 8740
7215 9720
7216 9594
7220 8300
7221 8310
7222 8320
7223 8330
7224 8350
7230 8400
7231 8410
7232 8440
7233 8490
7240 8500
7241 8510
7242 8520
7243 8540
7244 8560
7245 8570
7300 8420
7310 8420
7311 8420
7312 9410
7313 8800
7320 8900
7321 8920
7322 8910
7323 8940
7324 8950
7330 9420
7331 9420
7332 9420
7340 9200
7341 9210
7342 9230
7343 9240
7344 9270
7345 9260
7346 9290
7400 9950
7410 7700
7411 7730
7412 7760
7413 7750
7414 7740
7415 7790
7416 7800
7420 8100
7421 7310
7422 8110
7423 8120
7424 9420
7430 7500
7431 7510
7432 7540
7433 7900
7434 7920
7435 7940
7436 7950
7437 7960
7440 7600
7441 7610
7442 8000
7500 9950
7510 7009
7530 9971
8000 8490
8100 9690
8110 9690
8111 7110
8112 7120
8113 7130
8120 7200
8121 7210
8122 7220
8123 7260
8124 7270
8130 8930
8131 8930
8139 8990
8140 7300
8141 7320
8142 7330
8143 7340
8150 7400
8151 7410
8152 7420
8153 7430
8154 7440
8155 7450
8159 7490
8160 9600
8161 9610
8162 9690
8163 9690
8170 8530
8171 8530
8172 0390
8200 8340
8210 8340
8211 8340
8212 9430
8220 7440
8221 7440
8222 7440
8223 7290
8224 9270
8229 7490
8230 9000
8231 9010
8232 9010
8240 8120
8250 9210
8251 9220
8252 9260
8253 9100
8260 7590
8261 7510
8262 7530
8263 7950
8264 7560
8265 7610
8266 8010
8269 7930
8270 7790
8271 7790
8272 7750
8273 7710
8274 7760
8275 7740
8276 7720
8277 7770
8278 7780
8279 7890
8280 8410
8281 8493
8282 8530
8283 8430
8284 8493
8285 8190
8286 9100
8290 9970
8300 9970
8310 9830
8311 9839
8312 9840
8320 9850
8321 9850
8322 9859
8323 9851
8324 9852
8330 6280
8331 6280
8332 9740
8333 9730
8334 9790
8340 9810
9000 9990
9100 4520
9110 4520
9111 4521
9112 4521
9113 4521
9120 5995
9130 5400
9131 5400
9132 5520
9133 5600
9140 5500
9141 5510
9142 5520
9150 3700
9151 9713
9152 5994
9153 3993
9160 9990
9161 9996
9162 9995
9200 6290
9210 6290
9211 6210
9212 6230
9213 6490
9300 9990
9310 9990
9311 9990
9312 9994
9313 9595
9320 9990
9321 9979
9322 9714
9330 9710
9331 9899
9332 9860
9333 9710
% END

% CODELIST-isco88-isco88com
% source: Translation based on suggestions in Appendix 7.12 in Geis (2011)
%         http://www.gesis.org/fileadmin/upload/dienstleistung/tools_standards/handbuch_der_berufscodierung_110304.pdf
% variables: ICSO88 ISCO88(COM)
1000 1000
1100 1100
1110 1110
1120 1110
1130 1110
1140 1140
1141 1141
1142 1142
1143 1143
1200 1200
1210 1210
1220 1220
1221 1221
1222 1222
1223 1223
1224 1224
1225 1225
1226 1226
1227 1227
1228 1228
1229 1229
1230 1230
1231 1231
1232 1232
1233 1233
1234 1234
1235 1235
1236 1236
1237 1237
1239 1239
1300 1300
1310 1310
1311 1311
1312 1312
1313 1313
1314 1314
1315 1315
1316 1316
1317 1317
1318 1318
1319 1319
2000 2000
2100 2100
2110 2110
2111 2111
2112 2112
2113 2113
2114 2114
2120 2120
2121 2121
2122 2122
2130 2130
2131 2131
2132 2131
2139 2139
2140 2140
2141 2141
2142 2142
2143 2143
2144 2144
2145 2145
2146 2146
2147 2147
2148 2148
2149 2149
2200 2200
2210 2210
2211 2211
2212 2212
2213 2213
2220 2220
2221 2221
2222 2222
2223 2223
2224 2224
2229 2229
2230 2230
2300 2300
2310 2310
2320 2320
2330 2330
2331 2331
2332 2332
2340 2340
2350 2350
2351 2351
2352 2352
2359 2359
2400 2400
2410 2410
2411 2411
2412 2412
2419 2419
2420 2420
2421 2421
2422 2422
2429 2429
2430 2430
2431 2431
2432 2432
2440 2440
2441 2441
2442 2442
2443 2443
2444 2444
2445 2445
2446 2446
2450 2450
2451 2451
2452 2452
2453 2453
2454 2454
2455 2455
2460 2460
3000 3000
3100 3100
3110 3110
3111 3111
3112 3112
3113 3113
3114 3114
3115 3115
3116 3116
3117 3117
3118 3118
3119 3119
3120 3120
3121 3121
3122 3122
3123 3123
3130 3130
3131 3131
3132 3132
3133 3133
3139 3139
3140 3140
3141 3141
3142 3142
3143 3143
3144 3144
3145 3145
3150 3150
3151 3151
3152 3152
3200 3200
3210 3210
3211 3211
3212 3212
3213 3213
3220 3220
3221 3221
3222 3222
3223 3223
3224 3224
3225 3225
3226 3226
3227 3227
3228 3228
3229 3229
3230 3230
3231 3231
3232 3232
3240 3229
3241 3229
3242 3229
3300 3300
3310 3310
3320 3320
3330 3330
3340 3340
3400 3400
3410 3410
3411 3411
3412 3412
3413 3413
3414 3414
3415 3415
3416 3416
3417 3417
3419 3419
3420 3420
3421 3421
3422 3422
3423 3423
3429 3429
3430 3430
3431 3431
3432 3432
3433 3433
3434 3434
3439 3431
3440 3440
3441 3441
3442 3442
3443 3443
3444 3444
3449 3449
3450 3450
3460 3460
3470 3470
3471 3471
3472 3472
3473 3473
3474 3474
3475 3475
3480 3480
4000 4000
4100 4100
4110 4110
4111 4111
4112 4112
4113 4113
4114 4114
4115 4115
4120 4120
4121 4121
4122 4122
4130 4130
4131 4131
4132 4132
4133 4133
4140 4140
4141 4141
4142 4142
4143 4143
4144 4144
4190 4190
4200 4200
4210 4210
4211 4211
4212 4212
4213 4213
4214 4214
4215 4215
4220 4220
4221 4221
4222 4222
4223 4223
5000 5000
5100 5100
5110 5110
5111 5111
5112 5112
5113 5113
5120 5120
5121 5121
5122 5122
5123 5123
5130 5130
5131 5131
5132 5132
5133 5133
5139 5139
5140 5140
5141 5141
5142 5142
5143 5143
5149 5149
5150 5149
5151 5149
5152 5149
5160 5160
5161 5161
5162 5162
5163 5163
5169 5169
5200 5200
5210 5210
5220 5220
5230 5220
6000 6000
6100 6100
6110 6110
6111 6111
6112 6111
6113 6112
6114 6111
6120 6120
6121 6121
6122 6122
6123 6129
6124 6129
6129 6129
6130 6130
6140 6140
6141 6141
6142 6142
6150 6150
6151 6151
6152 6152
6153 6153
6154 6154
6200 6100
6210 6100
7000 7000
7100 7100
7110 7110
7111 7111
7112 7112
7113 7113
7120 7120
7121 7121
7122 7122
7123 7123
7124 7124
7129 7129
7130 7130
7131 7131
7132 7132
7133 7133
7134 7134
7135 7135
7136 7136
7137 7137
7140 7140
7141 7141
7142 7141
7143 7143
7200 7200
7210 7210
7211 7211
7212 7212
7213 7213
7214 7214
7215 7215
7216 7216
7220 7220
7221 7221
7222 7222
7223 7223
7224 7224
7230 7230
7231 7231
7232 7232
7233 7233
7240 7240
7241 7241
7242 7242
7243 7242
7244 7244
7245 7245
7300 7300
7310 7310
7311 7311
7312 7312
7313 7313
7320 7320
7321 7321
7322 7322
7323 7323
7324 7324
7330 7330
7331 7331
7332 7332
7340 7340
7341 7341
7342 7342
7343 7343
7344 7344
7345 7345
7346 7346
7400 7400
7410 7410
7411 7411
7412 7412
7413 7413
7414 7414
7415 7415
7416 7416
7420 7420
7421 7421
7422 7422
7423 7423
7424 7424
7430 7430
7431 7431
7432 7432
7433 7433
7434 7434
7435 7435
7436 7436
7437 7437
7440 7440
7441 7441
7442 7442
8000 8000
8100 8100
8110 8110
8111 8111
8112 8112
8113 8113
8120 8120
8121 8121
8122 8122
8123 8123
8124 8124
8130 8130
8131 8131
8139 8139
8140 8140
8141 8141
8142 8142
8143 8143
8150 8150
8151 8151
8152 8152
8153 8153
8154 8154
8155 8155
8159 8159
8160 8160
8161 8161
8162 8162
8163 8163
8170 8170
8171 8280
8172 8170
8200 8200
8210 8210
8211 8211
8212 8212
8220 8220
8221 8221
8222 8222
8223 8223
8224 8224
8229 8229
8230 8230
8231 8231
8232 8232
8240 8240
8250 8250
8251 8251
8252 8252
8253 8253
8260 8260
8261 8261
8262 8262
8263 8263
8264 8264
8265 8265
8266 8266
8269 8269
8270 8270
8271 8271
8272 8272
8273 8273
8274 8274
8275 8275
8276 8276
8277 8277
8278 8278
8279 8279
8280 8280
8281 8281
8282 8282
8283 8283
8284 8284
8285 8285
8286 8286
8290 8290
8300 8300
8310 8310
8311 8311
8312 8312
8320 8320
8321 8321
8322 8322
8323 8323
8324 8324
8330 8330
8331 8331
8332 8332
8333 8333
8334 8334
8340 8340
9000 9000
9100 9100
9110 9110
9111 9111
9112 9111
9113 9113
9120 9120
9130 9130
9131 9131
9132 9132
9133 9133
9140 9140
9141 9141
9142 9142
9150 9150
9151 9151
9152 9152
9153 9153
9160 9160
9161 9161
9162 9162
9200 9200
9210 9210
9211 9211
9212 9212
9213 9213
9300 9300
9310 9310
9311 9311
9312 9312
9313 9313
9320 9320
9321 8280
9322 9320
9330 9330
9331 9330
9332 9330
9333 9330
0000 0000
0100 0100
0110 0100
% END

% CODELIST-isco88-isco08
% source: isco8808.sps from http://www.harryganzeboom.nl/isco08/
% note: leading zeros added to ICSO88 codes with less than 4 digits
% variables: ICSO88 ISCO08
0100 0300
0110 0300
1250 0100
3452 0200
1000 1000
1100 1110
1110 1111
1120 1112
1130 1113
1140 1114
1141 1114
1142 1114
1143 1114
1200 1200
1210 1120
1220 1300
1221 1311
1222 1321
1223 1323
1224 1420
1225 1410
1226 1324
1227 1219
1228 1219
1229 1300
1230 1200
1231 1211
1232 1212
1233 1221
1234 1222
1235 1324
1236 1330
1237 1223
1239 1213
1240 3341
1300 1400
1310 1400
1311 6130
1312 1321
1313 1323
1314 1420
1315 1410
1316 1324
1317 1210
1318 1219
1319 1340
2000 2000
2100 2100
2110 2110
2111 2111
2112 2112
2113 2113
2114 2114
2120 2120
2121 2120
2122 2120
2130 2500
2131 2500
2132 2510
2139 2519
2140 2140
2141 2160
2142 2142
2143 2151
2144 2150
2145 2144
2146 2145
2147 2146
2148 2165
2149 2149
2200 2200
2210 2130
2211 2130
2212 2212
2213 2132
2220 2200
2221 2210
2222 2261
2223 2250
2224 2262
2229 2260
2230 2220
2300 2300
2310 2310
2320 2330
2321 2330
2322 2320
2323 2330
2330 2340
2331 2341
2332 2342
2340 2352
2350 2350
2351 2351
2352 2351
2359 2359
2400 2400
2410 2410
2411 2411
2412 2423
2419 2400
2420 2610
2421 2611
2422 2612
2429 2619
2430 2620
2431 2621
2432 2622
2440 2630
2441 2631
2442 2632
2443 2633
2444 2643
2445 2634
2446 2635
2450 2640
2451 2640
2452 2651
2453 2652
2454 2653
2455 2655
2460 2636
2470 2422
3000 3000
3100 3100
3110 3110
3111 3111
3112 3112
3113 3113
3114 3114
3115 3115
3116 3116
3117 3117
3118 3118
3119 3119
3120 3510
3121 3510
3122 3511
3123 3139
3130 3520
3131 3521
3132 3521
3133 3211
3139 3211
3140 3150
3141 3151
3142 3152
3143 3153
3144 3154
3145 3155
3150 7544
3151 3359
3152 3110
3200 3200
3210 3140
3211 3212
3212 3142
3213 2132
3220 3250
3221 3256
3222 3257
3223 2265
3224 3254
3225 3251
3226 2264
3227 3240
3228 3213
3229 3259
3230 3220
3231 3221
3232 3222
3240 3230
3241 3230
3242 5161
3300 2359
3310 2341
3320 2342
3330 2352
3340 2359
3400 3300
3410 3310
3411 3311
3412 3321
3413 3334
3414 4221
3415 3322
3416 3323
3417 3315
3419 3312
3420 3320
3421 3324
3422 3331
3423 3333
3429 3339
3430 3330
3431 3343
3432 3411
3433 3313
3434 3314
3439 3340
3440 3350
3441 3351
3442 3352
3443 3353
3444 3354
3449 3359
3450 3355
3451 3355
3460 3412
3470 3430
3471 3432
3472 2656
3473 2652
3474 2659
3475 3420
3480 3413
4000 4000
4100 4100
4110 4120
4111 4131
4112 4131
4113 4132
4114 4132
4115 4120
4120 4310
4121 4311
4122 4312
4130 4320
4131 4321
4132 4322
4133 4323
4140 4410
4141 4410
4142 4412
4143 4413
4144 4414
4190 4419
4200 4200
4210 4210
4211 5230
4212 4211
4213 4212
4214 4213
4215 4214
4220 4220
4221 4221
4222 4220
4223 4223
5000 5000
5100 5100
5110 5110
5111 5111
5112 5112
5113 5113
5120 5150
5121 5152
5122 5120
5123 5130
5130 5300
5131 5311
5132 5320
5133 5322
5139 5320
5140 5160
5141 5140
5142 5162
5143 5163
5149 5169
5150 5161
5151 5161
5152 5161
5160 5410
5161 5411
5162 5412
5163 5413
5164 0000
5169 5419
5200 5200
5210 5241
5220 5223
5230 5211
6000 6000
6100 6100
6110 6110
6111 6111
6112 6112
6113 6113
6114 6114
6120 6120
6121 6121
6122 6122
6123 6123
6124 6120
6129 6129
6130 6130
6133 6130
6140 6210
6141 6210
6142 6210
6150 6220
6151 6221
6152 6222
6153 6223
6154 6224
6200 6300
6210 6300
7000 7000
7100 7100
7110 8110
7111 8111
7112 7542
7113 7113
7120 7110
7121 7111
7122 7112
7123 7114
7124 7115
7129 7119
7130 7120
7131 7121
7132 7122
7133 7123
7134 7124
7135 7125
7136 7126
7137 7411
7140 7130
7141 7131
7142 7132
7143 7133
7200 7200
7210 7210
7211 7211
7212 7212
7213 7213
7214 7214
7215 7215
7216 7541
7220 7220
7221 7221
7222 7222
7223 7223
7224 7224
7230 7230
7231 7231
7232 7232
7233 7233
7234 9620
7240 7400
7241 7412
7242 7421
7243 7421
7244 7422
7245 7413
7300 7300
7310 7311
7311 7311
7312 7312
7313 7313
7320 7314
7321 7314
7322 7315
7323 7316
7324 7316
7330 7310
7331 7317
7332 7318
7340 7320
7341 7320
7342 7321
7343 7321
7344 8132
7345 7323
7346 7322
7400 7540
7410 7510
7411 7511
7412 7512
7413 7513
7414 7514
7415 7515
7416 7516
7420 7520
7421 7521
7422 7522
7423 7523
7424 7317
7430 7530
7431 7318
7432 8152
7433 7531
7434 7531
7435 7532
7436 7533
7437 7534
7440 7536
7441 7535
7442 7536
7510 3120
7520 7000
7530 7000
8000 8000
8100 8100
8110 8110
8111 8111
8112 8112
8113 8113
8120 8120
8121 8121
8122 8121
8123 8121
8124 8121
8130 8181
8131 8181
8139 8181
8140 8170
8141 8172
8142 8171
8143 8171
8150 8131
8151 8131
8152 8131
8153 8131
8154 8131
8155 8131
8159 8131
8160 8180
8161 3131
8162 8182
8163 3132
8170 3130
8171 3139
8172 3139
8200 8100
8210 8120
8211 7223
8212 8114
8220 8130
8221 8131
8222 8131
8223 8122
8224 8132
8229 8131
8230 8140
8231 8141
8232 8142
8240 7523
8250 7323
8251 7322
8252 7323
8253 8143
8260 8150
8261 8151
8262 8152
8263 8153
8264 8154
8265 8155
8266 8156
8269 8159
8270 8160
8271 8160
8272 8160
8273 8160
8274 8160
8275 8160
8276 8160
8277 8160
8278 8160
8279 8160
8280 8210
8281 8211
8282 8212
8283 8212
8284 8219
8285 8219
8286 8219
8290 8189
8300 8300
8310 8310
8311 8311
8312 8312
8320 8330
8321 8321
8322 8322
8323 8331
8324 8332
8330 8340
8331 8341
8332 8342
8333 8343
8334 8344
8340 8350
9000 9000
9100 9620
9110 9520
9111 5212
9112 9520
9113 5244
9120 9510
9130 9110
9131 9111
9132 9112
9133 9121
9140 9120
9141 5153
9142 9120
9150 9621
9151 9621
9152 5414
9153 9623
9160 9610
9161 9610
9162 9613
9200 9200
9210 9210
9211 9210
9212 9215
9213 9216
9300 9300
9310 9310
9311 9311
9312 9312
9313 9313
9320 9320
9321 9329
9322 9329
9330 9330
9331 9331
9332 9332
9333 9333
% END

% CODELIST-isco88-isei
% source: http://www.harryganzeboom.nl/ismf/scaleapp.htm
% variables: ISCO-88 ISEI
1000 55
1100 70
1110 77
1120 77
1130 66
1140 58
1141 58
1142 58
1143 58
1200 68
1210 70
1220 67
1221 67
1222 67
1223 67
1224 59
1225 59
1226 59
1227 87
1228 59
1229 67
1230 61
1231 69
1232 69
1233 56
1234 69
1235 69
1236 69
1237 69
1239 69
1240 58
1250 64
1251 70
1252 60
1300 51
1310 51
1311 43
1312 56
1313 51
1314 49
1315 44
1316 51
1317 51
1318 51
1319 51
2000 70
2100 69
2110 74
2111 74
2112 74
2113 74
2114 74
2120 71
2121 71
2122 71
2130 71
2131 71
2132 71
2139 71
2140 73
2141 69
2142 69
2143 68
2144 68
2145 67
2146 71
2147 67
2148 56
2149 69
2200 80
2210 78
2211 77
2212 77
2213 79
2220 85
2221 88
2222 85
2223 83
2224 74
2229 85
2230 43
2300 69
2310 77
2320 69
2321 70
2322 66
2330 66
2331 66
2332 43
2340 66
2350 66
2351 70
2352 70
2359 65
2400 68
2410 69
2411 69
2412 69
2419 69
2420 85
2421 85
2422 90
2429 82
2430 65
2431 65
2432 65
2440 65
2441 78
2442 71
2443 71
2444 65
2445 71
2446 51
2450 61
2451 65
2452 54
2453 64
2454 64
2455 64
2460 53
3000 54
3100 50
3110 49
3111 45
3112 45
3113 46
3114 46
3115 54
3116 54
3117 54
3118 51
3119 53
3120 52
3121 52
3122 52
3123 52
3130 52
3131 48
3132 57
3133 57
3139 52
3140 57
3141 52
3142 52
3143 69
3144 69
3145 50
3150 50
3151 50
3152 50
3200 48
3210 50
3211 50
3212 50
3213 50
3220 55
3221 51
3222 51
3223 51
3224 60
3225 51
3226 60
3227 51
3228 51
3229 51
3230 38
3231 38
3232 38
3240 49
3241 51
3242 38
3300 38
3310 38
3320 38
3330 38
3340 38
3400 55
3410 55
3411 61
3412 54
3413 59
3414 56
3415 56
3416 50
3417 56
3419 55
3420 55
3421 55
3422 55
3423 55
3429 55
3430 54
3431 54
3432 59
3433 51
3434 61
3439 54
3440 56
3441 56
3442 57
3443 56
3444 46
3449 56
3450 56
3451 55
3452 56
3460 43
3470 52
3471 53
3472 64
3473 50
3474 50
3475 54
3480 38
4000 45
4100 45
4110 51
4111 51
4112 50
4113 50
4114 51
4115 53
4120 51
4121 51
4122 51
4130 36
4131 32
4132 43
4133 45
4140 39
4141 39
4142 39
4143 39
4144 39
4190 39
4200 49
4210 48
4211 53
4212 46
4213 40
4214 40
4215 40
4220 52
4221 52
4222 52
4223 52
5000 40
5100 38
5110 34
5111 34
5112 34
5113 34
5120 32
5121 30
5122 30
5123 34
5130 25
5131 25
5132 25
5133 25
5139 25
5140 30
5141 29
5142 19
5143 54
5149 19
5150 43
5151 43
5152 43
5160 47
5161 42
5162 50
5163 40
5164 40
5169 40
5200 43
5210 43
5220 43
5230 37
6000 23
6100 23
6110 23
6111 23
6112 23
6113 23
6114 23
6120 23
6121 23
6122 23
6123 23
6124 23
6129 23
6130 23
6131 23
6132 27
6133 28
6134 23
6140 22
6141 22
6142 22
6150 28
6151 28
6152 28
6153 28
6154 28
6200 16
6210 16
7000 34
7100 31
7110 30
7111 30
7112 30
7113 27
7120 30
7121 29
7122 29
7123 26
7124 29
7129 30
7130 34
7131 19
7132 30
7133 31
7134 34
7135 26
7136 33
7137 37
7140 29
7141 29
7142 32
7143 29
7200 34
7210 31
7211 29
7212 30
7213 33
7214 30
7215 30
7216 30
7220 35
7221 33
7222 40
7223 34
7224 24
7230 34
7231 34
7232 42
7233 33
7234 23
7240 40
7241 40
7242 39
7243 41
7244 40
7245 38
7300 34
7310 38
7311 38
7312 38
7313 38
7320 28
7321 27
7322 29
7323 29
7324 29
7330 29
7331 29
7332 29
7340 40
7341 40
7342 40
7343 42
7344 40
7345 37
7346 38
7400 33
7410 30
7411 30
7412 31
7413 30
7414 30
7415 30
7416 30
7420 33
7421 33
7422 33
7423 33
7424 33
7430 36
7431 29
7432 29
7433 45
7434 36
7435 36
7436 33
7437 28
7440 31
7441 31
7442 31
7500 42
7510 42
7520 38
7530 26
8000 31
8100 30
8110 35
8111 35
8112 35
8113 35
8120 30
8121 31
8122 30
8123 28
8124 30
8130 22
8131 22
8139 22
8140 27
8141 27
8142 27
8143 27
8150 35
8151 35
8152 35
8153 35
8154 35
8155 35
8159 35
8160 32
8161 33
8162 27
8163 33
8170 26
8171 26
8172 26
8200 32
8210 36
8211 36
8212 30
8220 30
8221 30
8222 30
8223 30
8224 30
8229 30
8230 30
8231 30
8232 30
8240 29
8250 38
8251 38
8252 38
8253 38
8260 30
8261 29
8262 29
8263 32
8264 24
8265 32
8266 32
8269 32
8270 29
8271 29
8272 29
8273 29
8274 29
8275 29
8276 29
8277 29
8278 29
8279 29
8280 31
8281 30
8282 34
8283 34
8284 30
8285 30
8286 30
8290 26
8300 32
8310 36
8311 41
8312 32
8320 34
8321 30
8322 30
8323 30
8324 34
8330 26
8331 26
8332 26
8333 28
8334 28
8340 32
8400 24
9000 20
9100 25
9110 29
9111 29
9112 28
9113 29
9120 28
9130 16
9131 16
9132 16
9133 16
9140 23
9141 23
9142 23
9150 27
9151 25
9152 27
9153 27
9160 23
9161 23
9162 23
9200 16
9210 16
9211 16
9212 16
9213 16
9300 23
9310 21
9311 21
9312 21
9313 21
9320 20
9321 20
9322 24
9330 29
9331 22
9332 22
9333 30
% END

% CODELIST-isco88-siops
% source: http://www.harryganzeboom.nl/ismf/scaleapp.htm
% variables: ISCO-88 SIOPS
1000 51
1100 67
1110 64
1120 71
1130 63
1140 63
1141 63
1142 63
1143 63
1200 60
1210 70
1220 63
1221 60
1222 60
1223 60
1224 60
1225 60
1226 60
1227 60
1228 60
1229 60
1230 60
1231 60
1232 60
1233 60
1234 60
1235 60
1236 60
1237 60
1239 60
1240 55
1250 65
1251 73
1252 63
1300 50
1310 50
1311 47
1312 52
1313 52
1314 46
1315 38
1316 52
1317 52
1318 52
1319 52
2000 62
2100 63
2110 69
2111 75
2112 72
2113 69
2114 67
2120 56
2121 69
2122 55
2130 51
2131 51
2132 51
2139 51
2140 63
2141 72
2142 70
2143 65
2144 65
2145 66
2146 66
2147 61
2148 58
2149 56
2200 70
2210 62
2211 69
2212 68
2213 56
2220 73
2221 78
2222 70
2223 61
2224 64
2229 73
2230 54
2300 61
2310 78
2320 60
2321 60
2322 57
2330 57
2331 57
2332 49
2340 62
2350 62
2351 68
2352 68
2359 62
2400 60
2410 57
2411 62
2412 56
2419 57
2420 73
2421 73
2422 76
2429 71
2430 54
2431 54
2432 54
2440 58
2441 60
2442 67
2443 67
2444 62
2445 67
2446 52
2450 57
2451 58
2452 57
2453 45
2454 40
2455 57
2460 60
3000 48
3100 48
3110 47
3111 46
3112 39
3113 46
3114 46
3115 46
3116 46
3117 53
3118 55
3119 46
3120 53
3121 53
3122 53
3123 53
3130 46
3131 46
3132 49
3133 58
3139 44
3140 57
3141 60
3142 55
3143 60
3144 50
3145 46
3150 54
3151 54
3152 54
3200 51
3210 52
3211 52
3212 47
3213 55
3220 51
3221 53
3222 48
3223 52
3224 60
3225 44
3226 51
3227 48
3228 44
3229 45
3230 44
3231 44
3232 44
3240 29
3241 29
3242 22
3300 50
3310 50
3320 50
3330 50
3340 50
3400 48
3410 47
3411 50
3412 44
3413 49
3414 43
3415 46
3416 49
3417 46
3419 46
3420 42
3421 55
3422 50
3423 49
3429 42
3430 49
3431 53
3432 49
3433 49
3434 51
3439 53
3440 52
3441 44
3442 52
3443 55
3444 54
3449 55
3450 45
3451 60
3452 44
3460 49
3470 45
3471 49
3472 50
3473 32
3474 33
3475 49
3480 50
4000 37
4100 37
4110 45
4111 42
4112 42
4113 45
4114 45
4115 53
4120 44
4121 45
4122 36
4130 32
4131 30
4132 44
4133 37
4140 37
4141 36
4142 33
4143 41
4144 37
4190 37
4200 39
4210 37
4211 34
4212 42
4213 34
4214 15
4215 27
4220 38
4221 38
4222 38
4223 38
5000 32
5100 32
5110 32
5111 50
5112 32
5113 29
5120 26
5121 37
5122 31
5123 21
5130 27
5131 23
5132 42
5133 17
5139 29
5140 29
5141 32
5142 17
5143 34
5149 29
5150 37
5151 37
5152 37
5160 37
5161 35
5162 40
5163 39
5164 39
5169 30
5200 31
5210 28
5220 32
5230 24
6000 37
6100 38
6110 40
6111 40
6112 40
6113 40
6114 40
6120 40
6121 40
6122 40
6123 40
6124 40
6129 40
6130 38
6131 40
6132 41
6133 40
6134 30
6140 24
6141 24
6142 16
6150 28
6151 23
6152 23
6153 28
6154  6
6200 38
6210 38
7000 38
7100 34
7110 34
7111 34
7112 36
7113 34
7120 34
7121 36
7122 34
7123 34
7124 37
7129 28
7130 37
7131 31
7132 31
7133 31
7134 28
7135 26
7136 34
7137 44
7140 31
7141 31
7142 29
7143 20
7200 40
7210 38
7211 38
7212 39
7213 34
7214 44
7215 32
7216 26
7220 37
7221 35
7222 40
7223 38
7224 27
7230 43
7231 43
7232 50
7233 42
7234 20
7240 38
7241 38
7242 48
7243 42
7244 35
7245 36
7300 39
7310 45
7311 47
7312 33
7313 43
7320 28
7321 25
7322 37
7323 31
7324 31
7330 31
7331 31
7332 31
7340 42
7341 42
7342 41
7343 41
7344 42
7345 32
7346 52
7400 33
7410 28
7411 24
7412 33
7413 34
7414 35
7415 34
7416 34
7420 29
7421 29
7422 40
7423 36
7424 21
7430 34
7431 29
7432 32
7433 40
7434 35
7435 40
7436 26
7437 31
7440 27
7441 22
7442 27
7500 48
7510 46
7520 46
7530 37
8000 34
8100 36
8110 31
8111 34
8112 32
8113 31
8120 40
8121 45
8122 36
8123 38
8124 28
8130 31
8131 31
8139 31
8140 28
8141 29
8142 28
8143 28
8150 42
8151 43
8152 43
8153 43
8154 43
8155 37
8159 43
8160 38
8161 42
8162 35
8163 34
8170 30
8171 30
8172 30
8200 34
8210 37
8211 38
8212 30
8220 43
8221 43
8222 43
8223 28
8224 43
8229 43
8230 30
8231 30
8232 30
8240 31
8250 41
8251 41
8252 32
8253 28
8260 28
8261 29
8262 29
8263 25
8264 25
8265 26
8266 28
8269 26
8270 33
8271 31
8272 34
8273 33
8274 33
8275 35
8276 45
8277 34
8278 34
8279 39
8280 33
8281 30
8282 48
8283 48
8284 30
8285 31
8286 28
8290 33
8300 33
8310 36
8311 43
8312 29
8320 32
8321 31
8322 31
8323 32
8324 33
8330 32
8331 31
8332 32
8333 33
8334 28
8340 29
8400 33
9000 21
9100 23
9110 25
9111 24
9112 24
9113 26
9120 12
9130 21
9131 22
9132 21
9133 22
9140 23
9141 25
9142 19
9150 20
9151 22
9152 20
9153 21
9160 13
9161 13
9162 13
9200 23
9210 23
9211 23
9212 18
9213 23
9300 18
9310 16
9311 18
9312 15
9313 15
9320 19
9321 18
9322 22
9330 20
9331 17
9332 22
9333 20
% END

% CODELIST-isco88-mps
% source: bernhard2005.xlsx from https://github.com/dirtyhawk/stata-derivescores/
%         (should be equivalent to the appendix table in https://nbn-resolving.org/urn:nbn:de:0168-ssoar-207543)
% note: sorted by ISCO code; leading zero added to ISCO code "110"
% variables: ISCO-88 MPS88
0110 85.3
1110 160.3
1120 160.3
1130 131.1
1141 119.2
1142 121.3
1143 119.2
1210 150.8
1221 112.3
1222 93.9
1223 112.3
1224 112.3
1225 112.3
1226 112.3
1227 153.5
1228 112.3
1229 153.8
1231 131.6
1232 145.2
1233 125.4
1234 135.8
1235 160.5
1236 149.6
1237 135.8
1239 134.8
1311 104.4
1312 108.7
1313 101.1
1314 105.6
1315 81.6
1316 81.3
1317 144.5
1318 104.4
1319 124.1
2111 141.9
2112 141.9
2113 142.5
2114 141.9
2121 129.6
2122 129.6
2131 113.8
2132 119.9
2139 128
2141 133.7
2142 129.1
2143 126.2
2144 129.5
2145 130.4
2146 133.4
2147 131.5
2148 120.2
2149 135.1
2211 138.6
2212 134.6
2213 134.6
2221 179.6
2222 160.5
2223 160.5
2224 173.3
2229 160.5
2230 70.9
2310 159.8
2320 149.2
2331 130.3
2332 67.8
2340 149.2
2351 149.2
2352 149.2
2359 149.2
2411 129.2
2412 96.5
2419 126.7
2421 170.9
2422 186.8
2429 140.9
2431 143.2
2432 143.2
2441 132
2442 117.2
2443 117.2
2444 117.2
2445 147.1
2446 91.5
2451 113
2452 110.7
2453 101.4
2454 110.7
2455 110.7
2460 142.2
2470 130.4
3111 69.3
3112 78.8
3113 83.9
3114 77.3
3115 83.2
3116 79.1
3117 79.1
3118 71.7
3119 78.3
3121 115.8
3122 88.9
3123 106
3131 93.6
3132 93.6
3133 93.6
3139 93.6
3141 127.8
3142 127.8
3143 127.8
3144 127.8
3145 127.8
3151 67.6
3152 66
3211 82.9
3212 82.9
3213 82.9
3221 86.1
3222 86.1
3223 86.1
3224 86.1
3225 86.1
3226 87.8
3227 86.1
3228 86.1
3229 86.1
3231 70.9
3232 70.2
3241 77
3242 77
3310 82.9
3320 67.8
3330 82.9
3340 87.3
3411 88.6
3412 95.4
3413 100.3
3414 88.6
3415 90.5
3416 76.9
3417 88.6
3419 78.6
3421 93.9
3422 93.9
3423 93.9
3429 93.9
3431 73.2
3432 87.6
3433 90.2
3434 82.9
3439 69.9
3441 89.8
3442 86.1
3443 79.7
3444 89.8
3449 88.9
3450 107.4
3460 88.2
3471 91.9
3472 86.1
3473 86.1
3474 86.1
3475 86.1
3480 88.2
4111 73.1
4112 73.1
4113 73.1
4114 73.1
4115 73.1
4121 93.6
4122 92.1
4131 46.7
4132 80.1
4133 76.6
4141 47.9
4142 45.1
4143 47.9
4144 47.9
4190 73.1
4211 67.4
4212 67.1
4213 67.4
4214 67.4
4215 67.4
4221 60.2
4222 60.2
4223 60.2
5111 57.3
5112 53.1
5113 57.3
5121 55.4
5122 49.8
5123 55.4
5131 56.9
5132 57.3
5133 56.9
5139 56.9
5141 77.9
5142 75.7
5143 75.7
5149 75.7
5151 68.3
5152 68.3
5161 73.1
5162 80
5163 70.1
5169 56.1
5210 53.8
5220 53.8
5230 53.8
6111 41.9
6112 36.6
6113 45.8
6114 41.9
6121 38.4
6122 39.2
6123 39.2
6124 39.2
6129 39.2
6130 46.3
6141 60
6142 60
6151 44.2
6152 44.2
6153 44.2
6154 44.2
6210 44
7111 45.9
7112 42.9
7113 42.9
7121 47.1
7122 45.3
7123 40.3
7124 48.7
7129 53.4
7131 47.2
7132 56.8
7133 51.5
7134 45.6
7135 52.4
7136 51
7137 56
7139 52.4
7141 52.5
7142 40.3
7143 41.2
7211 44.5
7212 38.3
7213 47.1
7214 45.4
7215 44.5
7216 44.5
7221 49.6
7222 52.6
7223 48.5
7224 49.6
7231 52.9
7232 58.2
7233 47.4
7241 49.9
7242 62.3
7243 65.4
7244 60.5
7245 55
7311 62.5
7312 63
7313 63
7321 36.1
7322 38.1
7323 36.1
7324 36.1
7331 58.8
7332 58.8
7341 63.1
7342 64.2
7343 60.7
7344 64.2
7345 64.2
7346 64.2
7411 49.9
7412 55
7413 53.1
7414 53.1
7415 53.1
7416 53.1
7421 52.1
7422 53.1
7423 42.1
7424 52.1
7431 41.5
7432 41.5
7433 41.5
7434 41.5
7435 41.5
7436 41.5
7437 35.6
7441 51.1
7442 51.1
8111 45.6
8112 45.6
8113 45.6
8121 43.7
8122 33.9
8123 43.7
8124 43.7
8131 45.6
8139 45.6
8141 31.6
8142 31.6
8143 31.6
8151 44.8
8152 44.8
8153 44.8
8154 44.8
8155 44.8
8159 46
8161 51.6
8162 45.8
8163 49
8171 45.6
8172 45.6
8211 42.7
8212 43.3
8221 46.8
8222 46.8
8223 37.6
8224 46.8
8229 46.8
8231 41.4
8232 39.9
8240 29.3
8251 55
8252 44.2
8253 36.1
8261 44.2
8262 44.2
8263 44.2
8264 50.9
8265 44.2
8266 44.2
8269 44.2
8271 48.3
8272 48.3
8273 48.3
8274 48.3
8275 48.3
8276 48.3
8277 48.3
8278 48.3
8279 48.3
8281 31.9
8282 31.9
8283 31.9
8284 31.9
8285 31.9
8286 31.9
8287 31.9
8290 31.8
8311 55.7
8312 57
8321 39.5
8322 38.3
8323 40.5
8324 40.7
8331 34.7
8332 36.8
8333 41.5
8334 26.7
8340 40
9111 38.3
9112 38.3
9113 38.3
9120 40.1
9131 31.2
9132 30
9133 31.2
9141 44.7
9142 40.6
9151 32.4
9152 36.8
9153 35.6
9161 30.3
9162 28.6
9211 23.9
9212 23.9
9213 23.9
9311 20
9312 20
9313 24.7
9321 32.4
9322 32.4
9331 26.9
9332 26.9
9333 26.9
% END

% CODELIST-isco88-egp
% source: iskoegp.ado (version 1.2 14Oct2004 John_Hendrickx@yahoo.com) which is
%       based on iskoegp.sps, iskoroot.sps, and iskopromo.sps from
%       http://www.harryganzeboom.nl/isco88/
% note: the codelist has been generated automatically by applying iskoegp.ado
%       to all relevant combinations of isco codes, selfemployment, supervision
% variables: ISCO-88 EGP(0,0) EGB(0,1) EGP(0,2-9) EGP(0,10+) EGP(1,0) EGB(1,1) 
%       EGP(1,2-9) EGP(1,10+), where the first argument is selfemployment status
%       and the second argument is the number if subordinates/employees
1000 1 1 1 1 1 1 1 1
1100 1 1 1 1 1 1 1 1
1110 1 1 1 1 1 1 1 1
1120 1 1 1 1 1 1 1 1
1130 2 2 2 1 2 2 2 1
1140 2 2 2 1 2 2 2 1
1141 2 2 2 1 2 2 2 1
1142 2 2 2 1 2 2 2 1
1143 2 2 2 1 2 2 2 1
1200 1 2 2 1 1 4 4 1
1210 1 2 2 1 1 4 4 1
1220 1 2 2 1 1 4 4 1
1221 11 11 11 11 11 11 11 11
1222 1 2 2 1 1 4 4 1
1223 1 2 2 1 1 4 4 1
1224 1 2 2 1 1 4 4 1
1225 1 2 2 1 1 4 4 1
1226 1 2 2 1 1 4 4 1
1227 1 2 2 1 1 4 4 1
1228 1 2 2 1 1 4 4 1
1229 1 2 2 1 1 4 4 1
1230 1 1 1 1 1 1 1 1
1231 1 1 1 1 1 1 1 1
1232 1 1 1 1 1 1 1 1
1233 1 1 1 1 1 1 1 1
1234 1 1 1 1 1 1 1 1
1235 1 1 1 1 1 1 1 1
1236 1 1 1 1 1 1 1 1
1237 1 1 1 1 1 1 1 1
1239 1 1 1 1 1 1 1 1
1240 2 2 2 1 2 2 2 1
1250 1 1 1 1 1 1 1 1
1251 1 1 1 1 1 1 1 1
1252 2 2 2 1 2 2 2 1
1300 2 2 2 1 5 4 4 1
1310 2 2 2 1 5 4 4 1
1311 11 11 11 11 11 11 11 11
1312 2 2 2 1 5 4 4 1
1313 2 2 2 1 5 4 4 1
1314 2 2 2 1 5 4 4 1
1315 2 2 2 1 5 4 4 1
1316 2 2 2 1 5 4 4 1
1317 2 2 2 1 5 4 4 1
1318 2 2 2 1 5 4 4 1
1319 2 2 2 1 5 4 4 1
2000 1 1 1 1 1 1 1 1
2100 1 1 1 1 1 1 1 1
2110 1 1 1 1 1 1 1 1
2111 1 1 1 1 1 1 1 1
2112 1 1 1 1 1 1 1 1
2113 1 1 1 1 1 1 1 1
2114 1 1 1 1 1 1 1 1
2120 1 1 1 1 1 1 1 1
2121 1 1 1 1 1 1 1 1
2122 1 1 1 1 1 1 1 1
2130 1 1 1 1 1 1 1 1
2131 1 1 1 1 1 1 1 1
2132 2 2 2 1 2 2 2 1
2139 2 2 2 1 2 2 2 1
2140 1 1 1 1 1 1 1 1
2141 1 1 1 1 1 1 1 1
2142 1 1 1 1 1 1 1 1
2143 1 1 1 1 1 1 1 1
2144 1 1 1 1 1 1 1 1
2145 1 1 1 1 1 1 1 1
2146 1 1 1 1 1 1 1 1
2147 1 1 1 1 1 1 1 1
2148 2 2 2 1 2 2 2 1
2149 1 1 1 1 1 1 1 1
2200 1 1 1 1 1 1 1 1
2210 1 1 1 1 1 1 1 1
2211 1 1 1 1 1 1 1 1
2212 1 1 1 1 1 1 1 1
2213 1 1 1 1 1 1 1 1
2220 1 1 1 1 1 1 1 1
2221 1 1 1 1 1 1 1 1
2222 1 1 1 1 1 1 1 1
2223 1 1 1 1 1 1 1 1
2224 1 1 1 1 1 1 1 1
2229 1 1 1 1 1 1 1 1
2230 2 2 2 1 2 2 2 1
2300 2 2 2 1 2 2 2 1
2310 1 1 1 1 1 1 1 1
2320 2 2 2 1 2 2 2 1
2321 2 2 2 1 2 2 2 1
2322 2 2 2 1 2 2 2 1
2323 2 2 2 1 2 2 2 1
2330 2 2 2 1 2 2 2 1
2331 2 2 2 1 2 2 2 1
2332 2 2 2 1 2 2 2 1
2340 2 2 2 1 2 2 2 1
2350 1 1 1 1 1 1 1 1
2351 1 1 1 1 1 1 1 1
2352 1 1 1 1 1 1 1 1
2359 2 2 2 1 2 2 2 1
2400 1 1 1 1 1 1 1 1
2410 2 2 2 1 2 2 2 1
2411 1 1 1 1 1 1 1 1
2412 2 2 2 1 2 2 2 1
2419 2 2 2 1 2 2 2 1
2420 1 1 1 1 1 1 1 1
2421 1 1 1 1 1 1 1 1
2422 1 1 1 1 1 1 1 1
2429 1 1 1 1 1 1 1 1
2430 2 2 2 1 2 2 2 1
2431 2 2 2 1 2 2 2 1
2432 2 2 2 1 2 2 2 1
2440 1 1 1 1 1 1 1 1
2441 1 1 1 1 1 1 1 1
2442 1 1 1 1 1 1 1 1
2443 1 1 1 1 1 1 1 1
2444 2 2 2 1 2 2 2 1
2445 1 1 1 1 1 1 1 1
2446 2 2 2 1 2 2 2 1
2450 2 2 2 1 2 2 2 1
2451 2 2 2 1 2 2 2 1
2452 2 2 2 1 2 2 2 1
2453 2 2 2 1 2 2 2 1
2454 2 2 2 1 2 2 2 1
2455 2 2 2 1 2 2 2 1
2460 2 2 2 1 2 2 2 1
3000 2 2 2 1 2 2 2 1
3100 2 2 2 1 2 2 2 1
3110 2 2 2 1 2 2 2 1
3111 2 2 2 1 2 2 2 1
3112 2 2 2 1 2 2 2 1
3113 2 2 2 1 2 2 2 1
3114 2 2 2 1 2 2 2 1
3115 2 2 2 1 2 2 2 1
3116 2 2 2 1 2 2 2 1
3117 2 2 2 1 2 2 2 1
3118 2 2 2 1 2 2 2 1
3119 2 2 2 1 2 2 2 1
3120 2 2 2 1 2 2 2 1
3121 2 2 2 1 2 2 2 1
3122 2 2 2 1 2 2 2 1
3123 2 2 2 1 2 2 2 1
3130 2 2 2 1 2 2 2 1
3131 2 2 2 1 2 2 2 1
3132 2 2 2 1 2 2 2 1
3133 2 2 2 1 2 2 2 1
3139 2 2 2 1 2 2 2 1
3140 2 2 2 1 2 2 2 1
3141 2 2 2 1 2 2 2 1
3142 2 2 2 1 2 2 2 1
3143 1 1 1 1 1 1 1 1
3144 1 1 1 1 1 1 1 1
3145 2 2 2 1 2 2 2 1
3150 2 2 2 1 2 2 2 1
3151 2 2 2 1 2 2 2 1
3152 2 2 2 1 2 2 2 1
3200 2 2 2 1 2 2 2 1
3210 2 2 2 1 2 2 2 1
3211 2 2 2 1 2 2 2 1
3212 2 2 2 1 2 2 2 1
3213 2 2 2 1 2 2 2 1
3220 2 2 2 1 2 2 2 1
3221 2 2 2 1 2 2 2 1
3222 2 2 2 1 2 2 2 1
3223 2 2 2 1 2 2 2 1
3224 2 2 2 1 2 2 2 1
3225 2 2 2 1 2 2 2 1
3226 2 2 2 1 2 2 2 1
3227 2 2 2 1 2 2 2 1
3228 2 2 2 1 2 2 2 1
3229 2 2 2 1 2 2 2 1
3230 3 2 2 1 3 2 2 1
3231 3 2 2 1 3 2 2 1
3232 3 2 2 1 3 2 2 1
3240 2 2 2 1 2 2 2 1
3241 2 2 2 1 2 2 2 1
3242 2 2 2 1 2 2 2 1
3300 3 2 2 1 3 2 2 1
3310 3 2 2 1 3 2 2 1
3320 3 2 2 1 3 2 2 1
3330 3 2 2 1 3 2 2 1
3340 3 2 2 1 3 2 2 1
3400 2 2 2 1 5 4 4 1
3410 2 2 2 1 5 4 4 1
3411 2 2 2 1 5 4 4 1
3412 2 2 2 1 5 4 4 1
3413 2 2 2 1 5 4 4 1
3414 2 2 2 1 5 4 4 1
3415 2 2 2 1 5 4 4 1
3416 2 2 2 1 5 4 4 1
3417 2 2 2 1 5 4 4 1
3419 2 2 2 1 5 4 4 1
3420 2 2 2 1 5 4 4 1
3421 2 2 2 1 5 4 4 1
3422 2 2 2 1 5 4 4 1
3423 2 2 2 1 5 4 4 1
3429 2 2 2 1 5 4 4 1
3430 3 2 2 1 5 4 4 1
3431 2 2 2 1 5 4 4 1
3432 2 2 2 1 5 4 4 1
3433 3 2 2 1 5 4 4 1
3434 2 2 2 1 5 4 4 1
3439 3 2 2 1 5 4 4 1
3440 2 2 2 1 2 2 2 1
3441 2 2 2 1 2 2 2 1
3442 2 2 2 1 2 2 2 1
3443 2 2 2 1 2 2 2 1
3444 2 2 2 1 2 2 2 1
3449 2 2 2 1 2 2 2 1
3450 2 2 2 1 2 2 2 1
3451 2 2 2 1 2 2 2 1
3452 7 7 7 7 5 4 4 1
3460 3 2 2 1 3 2 2 1
3470 2 2 2 1 2 2 2 1
3471 2 2 2 1 2 2 2 1
3472 2 2 2 1 2 2 2 1
3473 2 2 2 1 2 2 2 1
3474 2 2 2 1 2 2 2 1
3475 2 2 2 1 2 2 2 1
3480 3 2 2 1 3 2 2 1
4000 3 2 2 1 5 4 4 1
4100 3 2 2 1 5 4 4 1
4110 3 2 2 1 5 4 4 1
4111 3 2 2 1 5 4 4 1
4112 3 2 2 1 5 4 4 1
4113 3 2 2 1 5 4 4 1
4114 3 2 2 1 5 4 4 1
4115 3 2 2 1 5 4 4 1
4120 3 2 2 1 5 4 4 1
4121 3 2 2 1 5 4 4 1
4122 3 2 2 1 5 4 4 1
4130 3 2 2 1 5 4 4 1
4131 3 2 2 1 5 4 4 1
4132 3 2 2 1 5 4 4 1
4133 3 2 2 1 5 4 4 1
4140 3 2 2 1 5 4 4 1
4141 3 2 2 1 5 4 4 1
4142 9 9 9 9 5 4 4 1
4143 3 2 2 1 5 4 4 1
4144 3 2 2 1 5 4 4 1
4190 3 2 2 1 5 4 4 1
4200 3 2 2 1 5 4 4 1
4210 3 2 2 1 5 4 4 1
4211 3 2 2 1 5 4 4 1
4212 3 2 2 1 5 4 4 1
4213 3 2 2 1 5 4 4 1
4214 3 2 2 1 5 4 4 1
4215 3 2 2 1 5 4 4 1
4220 3 2 2 1 5 4 4 1
4221 3 2 2 1 5 4 4 1
4222 3 2 2 1 5 4 4 1
4223 3 2 2 1 5 4 4 1
5000 3 2 2 1 5 4 4 1
5100 3 2 2 1 5 4 4 1
5110 3 2 2 1 5 4 4 1
5111 3 2 2 1 5 4 4 1
5112 3 2 2 1 5 4 4 1
5113 3 2 2 1 5 4 4 1
5120 3 2 2 1 5 4 4 1
5121 2 2 2 1 5 4 4 1
5122 8 7 7 7 5 4 4 1
5123 9 9 9 9 5 4 4 1
5130 9 9 9 9 5 4 4 1
5131 3 2 2 1 5 4 4 1
5132 9 9 9 9 5 4 4 1
5133 3 2 2 1 5 4 4 1
5139 9 9 9 9 5 4 4 1
5140 8 7 7 7 5 4 4 1
5141 8 7 7 7 5 4 4 1
5142 9 9 9 9 5 4 4 1
5143 8 7 7 7 5 4 4 1
5149 9 9 9 9 5 4 4 1
5150 2 2 2 1 5 4 4 1
5151 2 2 2 1 5 4 4 1
5152 2 2 2 1 5 4 4 1
5160 9 9 9 9 5 4 4 1
5161 8 7 7 7 5 4 4 1
5162 8 7 7 7 5 4 4 1
5163 9 9 9 9 5 4 4 1
5164 8 7 7 7 5 4 4 1
5169 9 9 9 9 5 4 4 1
5200 3 2 2 1 5 4 4 1
5210 3 2 2 1 5 4 4 1
5220 3 2 2 1 5 4 4 1
5230 3 2 2 1 5 4 4 1
6000 10 10 10 10 11 11 11 11
6100 10 11 11 11 11 11 11 11
6110 10 11 11 11 11 11 11 11
6111 10 11 11 11 11 11 11 11
6112 10 11 11 11 11 11 11 11
6113 10 11 11 11 11 11 11 11
6114 10 11 11 11 11 11 11 11
6120 10 11 11 11 11 11 11 11
6121 10 11 11 11 11 11 11 11
6122 10 11 11 11 11 11 11 11
6123 10 11 11 11 11 11 11 11
6124 10 11 11 11 11 11 11 11
6129 10 11 11 11 11 11 11 11
6130 10 11 11 11 11 11 11 11
6131 11 11 11 11 11 11 11 11
6132 11 11 11 11 11 11 11 11
6133 11 11 11 11 11 11 11 11
6134 10 10 10 10 11 11 11 11
6140 10 10 10 10 11 11 11 11
6141 10 10 10 10 11 11 11 11
6142 10 10 10 10 11 11 11 11
6150 10 10 10 10 11 11 11 11
6151 10 10 10 10 11 11 11 11
6152 10 10 10 10 11 11 11 11
6153 10 10 10 10 11 11 11 11
6154 10 10 10 10 11 11 11 11
6200 11 11 11 11 11 11 11 11
6210 11 11 11 11 11 11 11 11
7000 8 7 7 7 5 4 4 1
7100 8 7 7 7 5 4 4 1
7110 8 7 7 7 5 4 4 1
7111 8 7 7 7 5 4 4 1
7112 8 7 7 7 5 4 4 1
7113 8 7 7 7 5 4 4 1
7120 8 7 7 7 5 4 4 1
7121 9 9 9 9 5 4 4 1
7122 9 9 9 9 5 4 4 1
7123 9 9 9 9 5 4 4 1
7124 8 7 7 7 5 4 4 1
7129 8 7 7 7 5 4 4 1
7130 8 7 7 7 5 4 4 1
7131 9 9 9 9 5 4 4 1
7132 8 7 7 7 5 4 4 1
7133 8 7 7 7 5 4 4 1
7134 8 7 7 7 5 4 4 1
7135 9 9 9 9 5 4 4 1
7136 8 7 7 7 5 4 4 1
7137 8 7 7 7 5 4 4 1
7140 8 7 7 7 5 4 4 1
7141 8 7 7 7 5 4 4 1
7142 9 9 9 9 5 4 4 1
7143 9 9 9 9 5 4 4 1
7200 8 7 7 7 5 4 4 1
7210 8 7 7 7 5 4 4 1
7211 8 7 7 7 5 4 4 1
7212 8 7 7 7 5 4 4 1
7213 8 7 7 7 5 4 4 1
7214 8 7 7 7 5 4 4 1
7215 8 7 7 7 5 4 4 1
7216 8 7 7 7 5 4 4 1
7220 8 7 7 7 5 4 4 1
7221 8 7 7 7 5 4 4 1
7222 8 7 7 7 5 4 4 1
7223 8 7 7 7 5 4 4 1
7224 8 7 7 7 5 4 4 1
7230 8 7 7 7 5 4 4 1
7231 8 7 7 7 5 4 4 1
7232 8 7 7 7 5 4 4 1
7233 8 7 7 7 5 4 4 1
7234 9 9 9 9 5 4 4 1
7240 8 7 7 7 5 4 4 1
7241 8 7 7 7 5 4 4 1
7242 8 7 7 7 5 4 4 1
7243 8 7 7 7 5 4 4 1
7244 8 7 7 7 5 4 4 1
7245 8 7 7 7 5 4 4 1
7300 8 7 7 7 5 4 4 1
7310 8 7 7 7 5 4 4 1
7311 8 7 7 7 5 4 4 1
7312 8 7 7 7 5 4 4 1
7313 8 7 7 7 5 4 4 1
7320 9 9 9 9 5 4 4 1
7321 9 9 9 9 5 4 4 1
7322 9 9 9 9 5 4 4 1
7323 8 7 7 7 5 4 4 1
7324 8 7 7 7 5 4 4 1
7330 9 9 9 9 5 4 4 1
7331 9 9 9 9 5 4 4 1
7332 9 9 9 9 5 4 4 1
7340 8 7 7 7 5 4 4 1
7341 8 7 7 7 5 4 4 1
7342 8 7 7 7 5 4 4 1
7343 8 7 7 7 5 4 4 1
7344 8 7 7 7 5 4 4 1
7345 8 7 7 7 5 4 4 1
7346 8 7 7 7 5 4 4 1
7400 8 7 7 7 5 4 4 1
7410 8 7 7 7 5 4 4 1
7411 8 7 7 7 5 4 4 1
7412 8 7 7 7 5 4 4 1
7413 8 7 7 7 5 4 4 1
7414 8 7 7 7 5 4 4 1
7415 8 7 7 7 5 4 4 1
7416 8 7 7 7 5 4 4 1
7420 8 7 7 7 5 4 4 1
7421 9 9 9 9 5 4 4 1
7422 8 7 7 7 5 4 4 1
7423 8 7 7 7 5 4 4 1
7424 9 9 9 9 5 4 4 1
7430 8 7 7 7 5 4 4 1
7431 9 9 9 9 5 4 4 1
7432 9 9 9 9 5 4 4 1
7433 8 7 7 7 5 4 4 1
7434 8 7 7 7 5 4 4 1
7435 8 7 7 7 5 4 4 1
7436 8 7 7 7 5 4 4 1
7437 8 7 7 7 5 4 4 1
7440 8 7 7 7 5 4 4 1
7441 8 7 7 7 5 4 4 1
7442 8 7 7 7 5 4 4 1
7500 8 7 7 7 5 4 4 1
7510 7 7 7 7 4 4 4 1
7520 8 7 7 7 5 4 4 1
7530 9 9 9 9 5 4 4 1
8000 9 9 9 9 5 4 4 1
8100 9 9 9 9 5 4 4 1
8110 8 7 7 7 5 4 4 1
8111 8 7 7 7 5 4 4 1
8112 8 7 7 7 5 4 4 1
8113 8 7 7 7 5 4 4 1
8120 8 7 7 7 5 4 4 1
8121 8 7 7 7 5 4 4 1
8122 8 7 7 7 5 4 4 1
8123 8 7 7 7 5 4 4 1
8124 8 7 7 7 5 4 4 1
8130 9 9 9 9 5 4 4 1
8131 9 9 9 9 5 4 4 1
8139 9 9 9 9 5 4 4 1
8140 9 9 9 9 5 4 4 1
8141 9 9 9 9 5 4 4 1
8142 9 9 9 9 5 4 4 1
8143 9 9 9 9 5 4 4 1
8150 8 7 7 7 5 4 4 1
8151 8 7 7 7 5 4 4 1
8152 8 7 7 7 5 4 4 1
8153 8 7 7 7 5 4 4 1
8154 8 7 7 7 5 4 4 1
8155 8 7 7 7 5 4 4 1
8159 8 7 7 7 5 4 4 1
8160 8 7 7 7 5 4 4 1
8161 8 7 7 7 5 4 4 1
8162 8 7 7 7 5 4 4 1
8163 8 7 7 7 5 4 4 1
8170 8 7 7 7 5 4 4 1
8171 8 7 7 7 5 4 4 1
8172 8 7 7 7 5 4 4 1
8200 9 9 9 9 5 4 4 1
8210 8 7 7 7 5 4 4 1
8211 8 7 7 7 5 4 4 1
8212 9 9 9 9 5 4 4 1
8220 9 9 9 9 5 4 4 1
8221 9 9 9 9 5 4 4 1
8222 9 9 9 9 5 4 4 1
8223 9 9 9 9 5 4 4 1
8224 9 9 9 9 5 4 4 1
8229 9 9 9 9 5 4 4 1
8230 9 9 9 9 5 4 4 1
8231 9 9 9 9 5 4 4 1
8232 9 9 9 9 5 4 4 1
8240 9 9 9 9 5 4 4 1
8250 9 9 9 9 5 4 4 1
8251 9 9 9 9 5 4 4 1
8252 9 9 9 9 5 4 4 1
8253 9 9 9 9 5 4 4 1
8260 9 9 9 9 5 4 4 1
8261 9 9 9 9 5 4 4 1
8262 9 9 9 9 5 4 4 1
8263 9 9 9 9 5 4 4 1
8264 9 9 9 9 5 4 4 1
8265 9 9 9 9 5 4 4 1
8266 9 9 9 9 5 4 4 1
8269 9 9 9 9 5 4 4 1
8270 9 9 9 9 5 4 4 1
8271 9 9 9 9 5 4 4 1
8272 9 9 9 9 5 4 4 1
8273 9 9 9 9 5 4 4 1
8274 9 9 9 9 5 4 4 1
8275 9 9 9 9 5 4 4 1
8276 9 9 9 9 5 4 4 1
8277 9 9 9 9 5 4 4 1
8278 9 9 9 9 5 4 4 1
8279 9 9 9 9 5 4 4 1
8280 9 9 9 9 5 4 4 1
8281 9 9 9 9 5 4 4 1
8282 9 9 9 9 5 4 4 1
8283 9 9 9 9 5 4 4 1
8284 9 9 9 9 5 4 4 1
8285 9 9 9 9 5 4 4 1
8286 9 9 9 9 5 4 4 1
8290 9 9 9 9 5 4 4 1
8300 9 9 9 9 5 4 4 1
8310 9 9 9 9 5 4 4 1
8311 8 7 7 7 5 4 4 1
8312 9 9 9 9 5 4 4 1
8320 9 9 9 9 5 4 4 1
8321 9 9 9 9 5 4 4 1
8322 9 9 9 9 5 4 4 1
8323 9 9 9 9 5 4 4 1
8324 9 9 9 9 5 4 4 1
8330 9 9 9 9 5 4 4 1
8331 10 10 10 10 11 11 11 11
8332 8 7 7 7 5 4 4 1
8333 8 7 7 7 5 4 4 1
8334 9 9 9 9 5 4 4 1
8340 9 9 9 9 5 4 4 1
8400 9 9 9 9 5 4 4 1
9000 9 9 9 9 5 4 4 1
9100 3 2 2 1 3 2 2 1
9110 3 2 2 1 3 2 2 1
9111 3 2 2 1 3 2 2 1
9112 3 2 2 1 3 2 2 1
9113 3 2 2 1 3 2 2 1
9120 9 9 9 9 5 4 4 1
9130 9 9 9 9 5 4 4 1
9131 9 9 9 9 5 4 4 1
9132 9 9 9 9 5 4 4 1
9133 9 9 9 9 5 4 4 1
9140 9 9 9 9 5 4 4 1
9141 9 9 9 9 5 4 4 1
9142 9 9 9 9 5 4 4 1
9150 9 9 9 9 5 4 4 1
9151 9 9 9 9 5 4 4 1
9152 9 9 9 9 5 4 4 1
9153 9 9 9 9 5 4 4 1
9160 9 9 9 9 5 4 4 1
9161 9 9 9 9 5 4 4 1
9162 9 9 9 9 5 4 4 1
9200 9 9 11 11 5 4 11 11
9210 10 10 11 11 11 11 11 11
9211 10 10 11 11 11 11 11 11
9212 10 10 11 11 11 11 11 11
9213 10 10 11 11 11 11 11 11
9300 9 9 9 9 9 9 9 9
9310 9 9 9 9 9 9 9 9
9311 9 9 9 9 9 9 9 9
9312 9 9 9 9 9 9 9 9
9313 9 9 9 9 9 9 9 9
9320 9 9 9 9 9 9 9 9
9321 9 9 9 9 9 9 9 9
9322 9 9 9 9 9 9 9 9
9330 9 9 9 9 9 9 9 9
9331 9 9 9 9 9 9 9 9
9332 9 9 9 9 9 9 9 9
9333 9 9 9 9 9 9 9 9
% END

% CODELIST-isco88-egp11
% source: iskoegp.sps, iskoroot.sps, and iskopromo.sps from
%       http://www.harryganzeboom.nl/isco88/
% note: the codelist has been generated automatically by applying iskoegp.sps
%       to all relevant combinations of isco codes, selfemployment, supervision
% variables: ISCO-88 EGP(0,0) EGB(0,1) EGP(0,2-9) EGP(0,10+) EGP(1,0) EGB(1,1) 
%       EGP(1,2-9) EGP(1,10+), where the first argument is selfemployment status
%       and the second argument is the number if subordinates/employees
1000 1 1 1 1 1 1 1 1
1100 1 1 1 1 1 1 1 1
1110 1 1 1 1 1 1 1 1
1120 1 1 1 1 1 1 1 1
1130 2 2 2 1 2 2 2 1
1140 2 2 2 1 2 2 2 1
1141 2 2 2 1 2 2 2 1
1142 2 2 2 1 2 2 2 1
1143 2 2 2 1 2 2 2 1
1200 1 2 2 1 1 5 5 1
1210 1 2 2 1 1 5 5 1
1220 1 2 2 1 1 5 5 1
1221 11 11 11 11 11 11 11 11
1222 1 2 2 1 1 5 5 1
1223 1 2 2 1 1 5 5 1
1224 1 2 2 1 1 5 5 1
1225 1 2 2 1 1 5 5 1
1226 1 2 2 1 1 5 5 1
1227 1 2 2 1 1 5 5 1
1228 1 2 2 1 1 5 5 1
1229 1 2 2 1 1 5 5 1
1230 1 1 1 1 1 1 1 1
1231 1 1 1 1 1 1 1 1
1232 1 1 1 1 1 1 1 1
1233 1 1 1 1 1 1 1 1
1234 1 1 1 1 1 1 1 1
1235 1 1 1 1 1 1 1 1
1236 1 1 1 1 1 1 1 1
1237 1 1 1 1 1 1 1 1
1239 1 1 1 1 1 1 1 1
1240 2 2 2 1 2 2 2 1
1250 1 1 1 1 1 1 1 1
1251 1 1 1 1 1 1 1 1
1252 2 2 2 1 2 2 2 1
1300 2 2 2 1 6 5 5 1
1310 2 2 2 1 6 5 5 1
1311 11 11 11 11 11 11 11 11
1312 2 2 2 1 6 5 5 1
1313 2 2 2 1 6 5 5 1
1314 2 2 2 1 6 5 5 1
1315 2 2 2 1 6 5 5 1
1316 2 2 2 1 6 5 5 1
1317 2 2 2 1 6 5 5 1
1318 2 2 2 1 6 5 5 1
1319 2 2 2 1 6 5 5 1
2000 1 1 1 1 1 1 1 1
2100 1 1 1 1 1 1 1 1
2110 1 1 1 1 1 1 1 1
2111 1 1 1 1 1 1 1 1
2112 1 1 1 1 1 1 1 1
2113 1 1 1 1 1 1 1 1
2114 1 1 1 1 1 1 1 1
2120 1 1 1 1 1 1 1 1
2121 1 1 1 1 1 1 1 1
2122 1 1 1 1 1 1 1 1
2130 1 1 1 1 1 1 1 1
2131 1 1 1 1 1 1 1 1
2132 2 2 2 1 2 2 2 1
2139 2 2 2 1 2 2 2 1
2140 1 1 1 1 1 1 1 1
2141 1 1 1 1 1 1 1 1
2142 1 1 1 1 1 1 1 1
2143 1 1 1 1 1 1 1 1
2144 1 1 1 1 1 1 1 1
2145 1 1 1 1 1 1 1 1
2146 1 1 1 1 1 1 1 1
2147 1 1 1 1 1 1 1 1
2148 2 2 2 1 2 2 2 1
2149 1 1 1 1 1 1 1 1
2200 1 1 1 1 1 1 1 1
2210 1 1 1 1 1 1 1 1
2211 1 1 1 1 1 1 1 1
2212 1 1 1 1 1 1 1 1
2213 1 1 1 1 1 1 1 1
2220 1 1 1 1 1 1 1 1
2221 1 1 1 1 1 1 1 1
2222 1 1 1 1 1 1 1 1
2223 1 1 1 1 1 1 1 1
2224 1 1 1 1 1 1 1 1
2229 1 1 1 1 1 1 1 1
2230 2 2 2 1 2 2 2 1
2300 2 2 2 1 2 2 2 1
2310 1 1 1 1 1 1 1 1
2320 2 2 2 1 2 2 2 1
2321 2 2 2 1 2 2 2 1
2322 2 2 2 1 2 2 2 1
2323 2 2 2 1 2 2 2 1
2330 2 2 2 1 2 2 2 1
2331 2 2 2 1 2 2 2 1
2332 2 2 2 1 2 2 2 1
2340 2 2 2 1 2 2 2 1
2350 1 1 1 1 1 1 1 1
2351 1 1 1 1 1 1 1 1
2352 1 1 1 1 1 1 1 1
2359 2 2 2 1 2 2 2 1
2400 1 1 1 1 1 1 1 1
2410 2 2 2 1 2 2 2 1
2411 1 1 1 1 1 1 1 1
2412 2 2 2 1 2 2 2 1
2419 2 2 2 1 2 2 2 1
2420 1 1 1 1 1 1 1 1
2421 1 1 1 1 1 1 1 1
2422 1 1 1 1 1 1 1 1
2429 1 1 1 1 1 1 1 1
2430 2 2 2 1 2 2 2 1
2431 2 2 2 1 2 2 2 1
2432 2 2 2 1 2 2 2 1
2440 1 1 1 1 1 1 1 1
2441 1 1 1 1 1 1 1 1
2442 1 1 1 1 1 1 1 1
2443 1 1 1 1 1 1 1 1
2444 2 2 2 1 2 2 2 1
2445 1 1 1 1 1 1 1 1
2446 2 2 2 1 2 2 2 1
2450 2 2 2 1 2 2 2 1
2451 2 2 2 1 2 2 2 1
2452 2 2 2 1 2 2 2 1
2453 2 2 2 1 2 2 2 1
2454 2 2 2 1 2 2 2 1
2455 2 2 2 1 2 2 2 1
2460 2 2 2 1 2 2 2 1
3000 2 2 2 1 2 2 2 1
3100 2 2 2 1 2 2 2 1
3110 2 2 2 1 2 2 2 1
3111 2 2 2 1 2 2 2 1
3112 2 2 2 1 2 2 2 1
3113 2 2 2 1 2 2 2 1
3114 2 2 2 1 2 2 2 1
3115 2 2 2 1 2 2 2 1
3116 2 2 2 1 2 2 2 1
3117 2 2 2 1 2 2 2 1
3118 2 2 2 1 2 2 2 1
3119 2 2 2 1 2 2 2 1
3120 2 2 2 1 2 2 2 1
3121 2 2 2 1 2 2 2 1
3122 2 2 2 1 2 2 2 1
3123 2 2 2 1 2 2 2 1
3130 2 2 2 1 2 2 2 1
3131 2 2 2 1 2 2 2 1
3132 2 2 2 1 2 2 2 1
3133 2 2 2 1 2 2 2 1
3139 2 2 2 1 2 2 2 1
3140 2 2 2 1 2 2 2 1
3141 2 2 2 1 2 2 2 1
3142 2 2 2 1 2 2 2 1
3143 1 1 1 1 1 1 1 1
3144 1 1 1 1 1 1 1 1
3145 2 2 2 1 2 2 2 1
3150 2 2 2 1 2 2 2 1
3151 2 2 2 1 2 2 2 1
3152 2 2 2 1 2 2 2 1
3200 2 2 2 1 2 2 2 1
3210 2 2 2 1 2 2 2 1
3211 2 2 2 1 2 2 2 1
3212 2 2 2 1 2 2 2 1
3213 2 2 2 1 2 2 2 1
3220 2 2 2 1 2 2 2 1
3221 2 2 2 1 2 2 2 1
3222 2 2 2 1 2 2 2 1
3223 2 2 2 1 2 2 2 1
3224 2 2 2 1 2 2 2 1
3225 2 2 2 1 2 2 2 1
3226 2 2 2 1 2 2 2 1
3227 2 2 2 1 2 2 2 1
3228 2 2 2 1 2 2 2 1
3229 2 2 2 1 2 2 2 1
3230 3 2 2 1 3 2 2 1
3231 3 2 2 1 3 2 2 1
3232 3 2 2 1 3 2 2 1
3240 2 2 2 1 2 2 2 1
3241 2 2 2 1 2 2 2 1
3242 2 2 2 1 2 2 2 1
3300 3 2 2 1 3 2 2 1
3310 3 2 2 1 3 2 2 1
3320 3 2 2 1 3 2 2 1
3330 3 2 2 1 3 2 2 1
3340 3 2 2 1 3 2 2 1
3400 2 2 2 1 6 5 5 1
3410 2 2 2 1 6 5 5 1
3411 2 2 2 1 6 5 5 1
3412 2 2 2 1 6 5 5 1
3413 2 2 2 1 6 5 5 1
3414 2 2 2 1 6 5 5 1
3415 2 2 2 1 6 5 5 1
3416 2 2 2 1 6 5 5 1
3417 2 2 2 1 6 5 5 1
3419 2 2 2 1 6 5 5 1
3420 2 2 2 1 6 5 5 1
3421 2 2 2 1 6 5 5 1
3422 2 2 2 1 6 5 5 1
3423 2 2 2 1 6 5 5 1
3429 2 2 2 1 6 5 5 1
3430 3 2 2 1 6 5 5 1
3431 2 2 2 1 6 5 5 1
3432 2 2 2 1 6 5 5 1
3433 3 2 2 1 6 5 5 1
3434 2 2 2 1 6 5 5 1
3439 3 2 2 1 6 5 5 1
3440 2 2 2 1 2 2 2 1
3441 2 2 2 1 2 2 2 1
3442 2 2 2 1 2 2 2 1
3443 2 2 2 1 2 2 2 1
3444 2 2 2 1 2 2 2 1
3449 2 2 2 1 2 2 2 1
3450 2 2 2 1 2 2 2 1
3451 2 2 2 1 2 2 2 1
3452 7 7 7 7 6 5 5 1
3460 3 2 2 1 3 2 2 1
3470 2 2 2 1 2 2 2 1
3471 2 2 2 1 2 2 2 1
3472 2 2 2 1 2 2 2 1
3473 2 2 2 1 2 2 2 1
3474 2 2 2 1 2 2 2 1
3475 2 2 2 1 2 2 2 1
3480 3 2 2 1 3 2 2 1
4000 3 2 2 1 6 5 5 1
4100 3 2 2 1 6 5 5 1
4110 3 2 2 1 6 5 5 1
4111 3 2 2 1 6 5 5 1
4112 3 2 2 1 6 5 5 1
4113 3 2 2 1 6 5 5 1
4114 3 2 2 1 6 5 5 1
4115 3 2 2 1 6 5 5 1
4120 3 2 2 1 6 5 5 1
4121 3 2 2 1 6 5 5 1
4122 3 2 2 1 6 5 5 1
4130 3 2 2 1 6 5 5 1
4131 3 2 2 1 6 5 5 1
4132 3 2 2 1 6 5 5 1
4133 3 2 2 1 6 5 5 1
4140 3 2 2 1 6 5 5 1
4141 3 2 2 1 6 5 5 1
4142 9 9 9 9 6 5 5 1
4143 3 2 2 1 6 5 5 1
4144 3 2 2 1 6 5 5 1
4190 4 2 2 1 6 5 5 1
4200 4 2 2 1 6 5 5 1
4210 4 2 2 1 6 5 5 1
4211 4 2 2 1 6 5 5 1
4212 4 2 2 1 6 5 5 1
4213 4 2 2 1 6 5 5 1
4214 4 2 2 1 6 5 5 1
4215 4 2 2 1 6 5 5 1
4220 3 2 2 1 6 5 5 1
4221 3 2 2 1 6 5 5 1
4222 3 2 2 1 6 5 5 1
4223 3 2 2 1 6 5 5 1
5000 4 2 2 1 6 5 5 1
5100 4 2 2 1 6 5 5 1
5110 4 2 2 1 6 5 5 1
5111 4 2 2 1 6 5 5 1
5112 4 2 2 1 6 5 5 1
5113 4 2 2 1 6 5 5 1
5120 4 2 2 1 6 5 5 1
5121 4 2 2 1 6 5 5 1
5122 8 7 7 7 6 5 5 1
5123 4 2 2 1 6 5 5 1
5130 4 2 2 1 6 5 5 1
5131 4 2 2 1 6 5 5 1
5132 4 2 2 1 6 5 5 1
5133 4 2 2 1 6 5 5 1
5139 4 2 2 1 6 5 5 1
5140 8 7 7 7 6 5 5 1
5141 8 7 7 7 6 5 5 1
5142 4 2 2 1 6 5 5 1
5143 8 7 7 7 6 5 5 1
5149 4 2 2 1 6 5 5 1
5150 2 2 2 1 6 5 5 1
5151 2 2 2 1 6 5 5 1
5152 2 2 2 1 6 5 5 1
5160 9 9 9 9 6 5 5 1
5161 8 7 7 7 6 5 5 1
5162 8 7 7 7 6 5 5 1
5163 9 9 9 9 6 5 5 1
5164 8 7 7 7 6 5 5 1
5169 9 9 9 9 6 5 5 1
5200 4 2 2 1 6 5 5 1
5210 4 2 2 1 6 5 5 1
5220 4 2 2 1 6 5 5 1
5230 4 2 2 1 6 5 5 1
6000 10 10 10 10 11 11 11 11
6100 10 11 11 11 11 11 11 11
6110 10 11 11 11 11 11 11 11
6111 10 11 11 11 11 11 11 11
6112 10 11 11 11 11 11 11 11
6113 10 11 11 11 11 11 11 11
6114 10 11 11 11 11 11 11 11
6120 10 11 11 11 11 11 11 11
6121 10 11 11 11 11 11 11 11
6122 10 11 11 11 11 11 11 11
6123 10 11 11 11 11 11 11 11
6124 10 11 11 11 11 11 11 11
6129 10 11 11 11 11 11 11 11
6130 10 11 11 11 11 11 11 11
6131 11 11 11 11 11 11 11 11
6132 11 11 11 11 11 11 11 11
6133 11 11 11 11 11 11 11 11
6134 10 10 10 10 11 11 11 11
6140 10 10 10 10 11 11 11 11
6141 10 10 10 10 11 11 11 11
6142 10 10 10 10 11 11 11 11
6150 10 10 10 10 11 11 11 11
6151 10 10 10 10 11 11 11 11
6152 10 10 10 10 11 11 11 11
6153 10 10 10 10 11 11 11 11
6154 10 10 10 10 11 11 11 11
6200 11 11 11 11 11 11 11 11
6210 11 11 11 11 11 11 11 11
7000 8 7 7 7 6 5 5 1
7100 8 7 7 7 6 5 5 1
7110 8 7 7 7 6 5 5 1
7111 8 7 7 7 6 5 5 1
7112 8 7 7 7 6 5 5 1
7113 8 7 7 7 6 5 5 1
7120 8 7 7 7 6 5 5 1
7121 9 9 9 9 6 5 5 1
7122 9 9 9 9 6 5 5 1
7123 9 9 9 9 6 5 5 1
7124 8 7 7 7 6 5 5 1
7129 8 7 7 7 6 5 5 1
7130 8 7 7 7 6 5 5 1
7131 9 9 9 9 6 5 5 1
7132 8 7 7 7 6 5 5 1
7133 8 7 7 7 6 5 5 1
7134 8 7 7 7 6 5 5 1
7135 9 9 9 9 6 5 5 1
7136 8 7 7 7 6 5 5 1
7137 8 7 7 7 6 5 5 1
7140 8 7 7 7 6 5 5 1
7141 8 7 7 7 6 5 5 1
7142 9 9 9 9 6 5 5 1
7143 9 9 9 9 6 5 5 1
7200 8 7 7 7 6 5 5 1
7210 8 7 7 7 6 5 5 1
7211 8 7 7 7 6 5 5 1
7212 8 7 7 7 6 5 5 1
7213 8 7 7 7 6 5 5 1
7214 8 7 7 7 6 5 5 1
7215 8 7 7 7 6 5 5 1
7216 8 7 7 7 6 5 5 1
7220 8 7 7 7 6 5 5 1
7221 8 7 7 7 6 5 5 1
7222 8 7 7 7 6 5 5 1
7223 8 7 7 7 6 5 5 1
7224 8 7 7 7 6 5 5 1
7230 8 7 7 7 6 5 5 1
7231 8 7 7 7 6 5 5 1
7232 8 7 7 7 6 5 5 1
7233 8 7 7 7 6 5 5 1
7234 9 9 9 9 6 5 5 1
7240 8 7 7 7 6 5 5 1
7241 8 7 7 7 6 5 5 1
7242 8 7 7 7 6 5 5 1
7243 8 7 7 7 6 5 5 1
7244 8 7 7 7 6 5 5 1
7245 8 7 7 7 6 5 5 1
7300 8 7 7 7 6 5 5 1
7310 8 7 7 7 6 5 5 1
7311 8 7 7 7 6 5 5 1
7312 8 7 7 7 6 5 5 1
7313 8 7 7 7 6 5 5 1
7320 9 9 9 9 6 5 5 1
7321 9 9 9 9 6 5 5 1
7322 9 9 9 9 6 5 5 1
7323 8 7 7 7 6 5 5 1
7324 8 7 7 7 6 5 5 1
7330 9 9 9 9 6 5 5 1
7331 9 9 9 9 6 5 5 1
7332 9 9 9 9 6 5 5 1
7340 8 7 7 7 6 5 5 1
7341 8 7 7 7 6 5 5 1
7342 8 7 7 7 6 5 5 1
7343 8 7 7 7 6 5 5 1
7344 8 7 7 7 6 5 5 1
7345 8 7 7 7 6 5 5 1
7346 8 7 7 7 6 5 5 1
7400 8 7 7 7 6 5 5 1
7410 8 7 7 7 6 5 5 1
7411 8 7 7 7 6 5 5 1
7412 8 7 7 7 6 5 5 1
7413 8 7 7 7 6 5 5 1
7414 8 7 7 7 6 5 5 1
7415 8 7 7 7 6 5 5 1
7416 8 7 7 7 6 5 5 1
7420 8 7 7 7 6 5 5 1
7421 9 9 9 9 6 5 5 1
7422 8 7 7 7 6 5 5 1
7423 8 7 7 7 6 5 5 1
7424 9 9 9 9 6 5 5 1
7430 8 7 7 7 6 5 5 1
7431 9 9 9 9 6 5 5 1
7432 9 9 9 9 6 5 5 1
7433 8 7 7 7 6 5 5 1
7434 8 7 7 7 6 5 5 1
7435 8 7 7 7 6 5 5 1
7436 8 7 7 7 6 5 5 1
7437 8 7 7 7 6 5 5 1
7440 8 7 7 7 6 5 5 1
7441 8 7 7 7 6 5 5 1
7442 8 7 7 7 6 5 5 1
7500 8 7 7 7 6 5 5 1
7510 7 7 7 7 5 5 5 1
7520 8 7 7 7 6 5 5 1
7530 9 9 9 9 6 5 5 1
8000 9 9 9 9 6 5 5 1
8100 9 9 9 9 6 5 5 1
8110 8 7 7 7 6 5 5 1
8111 8 7 7 7 6 5 5 1
8112 8 7 7 7 6 5 5 1
8113 8 7 7 7 6 5 5 1
8120 8 7 7 7 6 5 5 1
8121 8 7 7 7 6 5 5 1
8122 8 7 7 7 6 5 5 1
8123 8 7 7 7 6 5 5 1
8124 8 7 7 7 6 5 5 1
8130 9 9 9 9 6 5 5 1
8131 9 9 9 9 6 5 5 1
8139 9 9 9 9 6 5 5 1
8140 9 9 9 9 6 5 5 1
8141 9 9 9 9 6 5 5 1
8142 9 9 9 9 6 5 5 1
8143 9 9 9 9 6 5 5 1
8150 8 7 7 7 6 5 5 1
8151 8 7 7 7 6 5 5 1
8152 8 7 7 7 6 5 5 1
8153 8 7 7 7 6 5 5 1
8154 8 7 7 7 6 5 5 1
8155 8 7 7 7 6 5 5 1
8159 8 7 7 7 6 5 5 1
8160 8 7 7 7 6 5 5 1
8161 8 7 7 7 6 5 5 1
8162 8 7 7 7 6 5 5 1
8163 8 7 7 7 6 5 5 1
8170 8 7 7 7 6 5 5 1
8171 8 7 7 7 6 5 5 1
8172 8 7 7 7 6 5 5 1
8200 9 9 9 9 6 5 5 1
8210 8 7 7 7 6 5 5 1
8211 8 7 7 7 6 5 5 1
8212 9 9 9 9 6 5 5 1
8220 9 9 9 9 6 5 5 1
8221 9 9 9 9 6 5 5 1
8222 9 9 9 9 6 5 5 1
8223 9 9 9 9 6 5 5 1
8224 9 9 9 9 6 5 5 1
8229 9 9 9 9 6 5 5 1
8230 9 9 9 9 6 5 5 1
8231 9 9 9 9 6 5 5 1
8232 9 9 9 9 6 5 5 1
8240 9 9 9 9 6 5 5 1
8250 9 9 9 9 6 5 5 1
8251 9 9 9 9 6 5 5 1
8252 9 9 9 9 6 5 5 1
8253 9 9 9 9 6 5 5 1
8260 9 9 9 9 6 5 5 1
8261 9 9 9 9 6 5 5 1
8262 9 9 9 9 6 5 5 1
8263 9 9 9 9 6 5 5 1
8264 9 9 9 9 6 5 5 1
8265 9 9 9 9 6 5 5 1
8266 9 9 9 9 6 5 5 1
8269 9 9 9 9 6 5 5 1
8270 9 9 9 9 6 5 5 1
8271 9 9 9 9 6 5 5 1
8272 9 9 9 9 6 5 5 1
8273 9 9 9 9 6 5 5 1
8274 9 9 9 9 6 5 5 1
8275 9 9 9 9 6 5 5 1
8276 9 9 9 9 6 5 5 1
8277 9 9 9 9 6 5 5 1
8278 9 9 9 9 6 5 5 1
8279 9 9 9 9 6 5 5 1
8280 9 9 9 9 6 5 5 1
8281 9 9 9 9 6 5 5 1
8282 9 9 9 9 6 5 5 1
8283 9 9 9 9 6 5 5 1
8284 9 9 9 9 6 5 5 1
8285 9 9 9 9 6 5 5 1
8286 9 9 9 9 6 5 5 1
8290 9 9 9 9 6 5 5 1
8300 9 9 9 9 6 5 5 1
8310 9 9 9 9 6 5 5 1
8311 8 7 7 7 6 5 5 1
8312 9 9 9 9 6 5 5 1
8320 9 9 9 9 6 5 5 1
8321 9 9 9 9 6 5 5 1
8322 9 9 9 9 6 5 5 1
8323 9 9 9 9 6 5 5 1
8324 9 9 9 9 6 5 5 1
8330 9 9 9 9 6 5 5 1
8331 10 10 10 10 11 11 11 11
8332 8 7 7 7 6 5 5 1
8333 8 7 7 7 6 5 5 1
8334 9 9 9 9 6 5 5 1
8340 9 9 9 9 6 5 5 1
8400 9 9 9 9 6 5 5 1
9000 9 9 9 9 6 5 5 1
9100 3 2 2 1 3 2 2 1
9110 3 2 2 1 3 2 2 1
9111 3 2 2 1 3 2 2 1
9112 3 2 2 1 3 2 2 1
9113 3 2 2 1 3 2 2 1
9120 9 9 9 9 6 5 5 1
9130 9 9 9 9 6 5 5 1
9131 9 9 9 9 6 5 5 1
9132 9 9 9 9 6 5 5 1
9133 9 9 9 9 6 5 5 1
9140 9 9 9 9 6 5 5 1
9141 9 9 9 9 6 5 5 1
9142 9 9 9 9 6 5 5 1
9150 9 9 9 9 6 5 5 1
9151 9 9 9 9 6 5 5 1
9152 9 9 9 9 6 5 5 1
9153 9 9 9 9 6 5 5 1
9160 9 9 9 9 6 5 5 1
9161 9 9 9 9 6 5 5 1
9162 9 9 9 9 6 5 5 1
9200 10 10 11 11 11 11 11 11
9210 10 10 11 11 11 11 11 11
9211 10 10 11 11 11 11 11 11
9212 10 10 11 11 11 11 11 11
9213 10 10 11 11 11 11 11 11
9300 9 9 9 9 9 9 9 9
9310 9 9 9 9 9 9 9 9
9311 9 9 9 9 9 9 9 9
9312 9 9 9 9 9 9 9 9
9313 9 9 9 9 9 9 9 9
9320 9 9 9 9 9 9 9 9
9321 9 9 9 9 9 9 9 9
9322 9 9 9 9 9 9 9 9
9330 9 9 9 9 9 9 9 9
9331 9 9 9 9 9 9 9 9
9332 9 9 9 9 9 9 9 9
9333 9 9 9 9 9 9 9 9
% END

% CODELIST-isco88-esec
% source: Euroesec matrix.xls obtained on 10apr2020 from 
%         https://www.iser.essex.ac.uk/archives/esec/matrices-and-syntax
% variables: ISCO-88(3 digit) ESEC-employee ESEC-supervisor
%            ESEC-selfemp(0) ESEC-selfemp(1-9) ESEC-selfemp(10+)
%            ESEC-simplified
010 1 1 1 1 1 1
011 3 2 3 3 3 3
100 1 1 4 4 1 1
110 1 1 1 1 1 1
111 1 1 1 1 1 1
114 1 1 1 1 1 1
120 1 1 4 4 1 1
121 1 1 4 4 1 1
122 2 2 4 4 1 2
123 1 1 4 4 1 1
130 2 2 4 4 1 4
131 2 2 4 4 1 4
200 1 1 1 1 1 1
210 1 1 1 1 1 1
211 1 1 1 1 1 1
212 1 1 1 1 1 1
213 1 1 1 1 1 1
214 1 1 1 1 1 1
220 1 1 1 1 1 1
221 1 1 1 1 1 1
222 1 1 1 1 1 1
223 2 2 2 2 1 2
230 2 2 2 2 1 2
231 1 1 1 1 1 1
232 2 2 2 2 1 2
233 2 2 2 2 1 2
234 2 2 2 2 1 2
235 1 1 1 1 1 1
240 1 1 1 1 1 1
241 1 1 1 1 1 1
242 1 1 1 1 1 1
243 2 2 2 2 1 2
244 2 2 2 2 1 2
245 2 2 2 2 1 2
246 2 2 2 2 1 2
247 2 2 2 2 1 2
300 3 2 4 4 1 3
310 2 2 2 2 1 2
311 2 2 2 2 1 2
312 2 2 2 2 1 2
313 6 2 4 4 1 6
314 2 2 2 2 1 2
315 6 6 4 4 1 6
320 2 2 2 2 1 2
321 2 2 2 2 1 2
322 2 2 2 2 1 2
323 2 2 2 2 1 2
330 3 2 4 4 1 3
331 3 2 4 4 1 3
332 3 2 4 4 1 3
333 3 2 4 4 1 3
334 2 2 2 2 1 2
340 3 2 4 4 1 3
341 3 2 4 4 1 3
342 2 2 2 2 1 2
343 3 2 4 4 1 3
344 2 2 2 2 2 2
345 2 2 2 2 2 2
346 3 2 4 4 1 3
347 3 2 4 4 1 3
348 2 2 2 2 1 2
400 3 2 4 4 1 3
410 3 2 4 4 1 3
411 3 2 4 4 1 3
412 3 2 4 4 1 3
413 7 6 4 4 1 7
414 9 6 4 4 1 9
419 3 2 4 4 1 3
420 3 2 4 4 1 3
421 7 6 4 4 1 7
422 7 6 4 4 1 7
500 7 6 4 4 1 7
510 7 6 4 4 1 7
511 7 6 4 4 1 7
512 9 6 4 4 1 9
513 7 6 4 4 1 7
514 7 6 4 4 1 7
516 7 6 3 3 3 7
520 7 6 4 4 1 7
521 2 2 4 4 1 2
522 7 6 4 4 1 7
600 8 6 5 5 1 5
610 8 6 5 5 1 5
611 8 6 5 5 1 5
612 8 6 5 5 1 5
613 8 6 5 5 1 5
614 8 6 5 5 1 8
615 8 6 5 5 1 8
621 5 5 5 5 5 5
700 8 6 4 4 1 8
710 8 6 4 4 1 8
711 8 6 4 4 1 8
712 8 6 4 4 1 8
713 8 6 4 4 1 8
714 8 6 4 4 1 8
720 8 6 4 4 1 8
721 8 6 4 4 1 8
722 8 6 4 4 1 8
723 8 6 4 4 1 8
724 8 6 4 4 1 8
730 6 6 4 4 1 6
731 6 6 4 4 1 6
732 8 6 4 4 1 8
733 8 6 4 4 1 8
734 8 6 4 4 1 8
740 8 6 4 4 1 8
741 8 6 4 4 1 8
742 8 6 4 4 1 8
743 8 6 4 4 1 8
744 8 6 4 4 1 8
800 9 6 4 4 1 9
810 9 6 4 4 1 9
811 9 6 4 4 1 9
812 9 6 4 4 1 9
813 9 6 4 4 1 9
814 9 6 4 4 1 9
815 9 6 4 4 1 9
816 9 6 4 4 1 9
817 9 6 4 4 1 9
820 9 6 4 4 1 9
821 9 6 4 4 1 9
822 9 6 4 4 1 9
823 9 6 4 4 1 9
824 9 6 4 4 1 9
825 8 6 4 4 1 8
826 9 6 4 4 1 9
827 9 6 4 4 1 9
828 9 6 4 4 1 9
829 9 6 4 4 1 9
830 9 6 4 4 1 9
831 8 6 4 4 1 8
832 9 6 4 4 1 9
833 9 6 4 4 1 9
834 8 6 4 4 1 8
900 9 6 4 4 1 9
910 9 6 4 4 1 9
911 7 6 4 4 1 4
912 9 6 4 4 1 9
913 9 6 4 4 1 9
914 9 6 4 4 1 9
915 9 6 4 4 1 9
916 9 6 4 4 1 9
920 9 6 5 5 1 9
921 9 6 5 5 1 9
930 9 6 4 4 1 9
931 9 6 4 4 1 9
932 9 6 4 4 1 9
933 9 6 4 4 1 9
% END

% CODELIST-isco88-oesch
% source: iskooesch.ado (May 2018) by Simon Kaiser
% note: the codelist has been generated automatically by applying iskooesch.ado
%       to all relevant combinations of isco codes, selfemployment, supervision
% variables: ISCO-88 OESCH-employee OESCH-selfemp(0) OESCH-selfemp(1-9)
%            OESCH-selfemp(10+)
1000 9 4 3 1
1100 9 4 3 1
1110 9 4 3 1
1120 9 4 3 1
1130 9 4 3 1
1140 9 4 3 1
1141 9 4 3 1
1142 9 4 3 1
1143 9 4 3 1
1200 9 4 3 1
1210 9 4 3 1
1220 9 4 3 1
1221 9 4 3 1
1222 9 4 3 1
1223 9 4 3 1
1224 9 4 3 1
1225 9 4 3 1
1226 9 4 3 1
1227 9 4 3 1
1228 9 4 3 1
1229 9 4 3 1
1230 9 4 3 1
1231 9 4 3 1
1232 9 4 3 1
1233 9 4 3 1
1234 9 4 3 1
1235 9 4 3 1
1236 9 4 3 1
1237 9 4 3 1
1239 9 4 3 1
1240 . 4 3 1
1250 . 4 3 1
1251 . 4 3 1
1252 . 4 3 1
1300 10 4 3 1
1310 10 4 3 1
1311 10 4 3 1
1312 10 4 3 1
1313 10 4 3 1
1314 10 4 3 1
1315 10 4 3 1
1316 10 4 3 1
1317 10 4 3 1
1318 10 4 3 1
1319 10 4 3 1
2000 . 2 2 1
2100 5 2 2 1
2110 5 2 2 1
2111 5 2 2 1
2112 5 2 2 1
2113 5 2 2 1
2114 5 2 2 1
2120 5 2 2 1
2121 5 2 2 1
2122 5 2 2 1
2130 5 2 2 1
2131 5 2 2 1
2132 5 2 2 1
2139 5 2 2 1
2140 5 2 2 1
2141 5 2 2 1
2142 5 2 2 1
2143 5 2 2 1
2144 5 2 2 1
2145 5 2 2 1
2146 5 2 2 1
2147 5 2 2 1
2148 5 2 2 1
2149 5 2 2 1
2200 5 2 2 1
2210 5 2 2 1
2211 5 2 2 1
2212 5 2 2 1
2213 5 2 2 1
2220 13 2 2 1
2221 13 2 2 1
2222 13 2 2 1
2223 13 2 2 1
2224 13 2 2 1
2229 13 2 2 1
2230 14 4 3 1
2300 13 2 2 1
2310 13 2 2 1
2320 13 2 2 1
2321 . 2 2 1
2322 . 2 2 1
2323 . 2 2 1
2330 14 2 2 1
2331 14 2 2 1
2332 14 2 2 1
2340 13 2 2 1
2350 13 2 2 1
2351 13 2 2 1
2352 13 2 2 1
2359 13 2 2 1
2400 9 2 2 1
2410 9 2 2 1
2411 9 2 2 1
2412 9 2 2 1
2419 9 2 2 1
2420 9 2 2 1
2421 9 2 2 1
2422 9 2 2 1
2429 9 2 2 1
2430 13 2 2 1
2431 13 2 2 1
2432 13 2 2 1
2440 13 2 2 1
2441 9 2 2 1
2442 13 2 2 1
2443 13 2 2 1
2444 14 2 2 1
2445 13 2 2 1
2446 14 2 2 1
2450 14 2 2 1
2451 13 2 2 1
2452 14 2 2 1
2453 14 2 2 1
2454 14 2 2 1
2455 14 2 2 1
2460 13 2 2 1
2470 9 2 2 1
3000 . 4 3 1
3100 6 4 3 1
3110 6 4 3 1
3111 6 4 3 1
3112 6 4 3 1
3113 6 4 3 1
3114 6 4 3 1
3115 6 4 3 1
3116 6 4 3 1
3117 6 4 3 1
3118 6 4 3 1
3119 6 4 3 1
3120 6 4 3 1
3121 6 4 3 1
3122 6 4 3 1
3123 6 4 3 1
3130 6 4 3 1
3131 6 4 3 1
3132 6 4 3 1
3133 6 4 3 1
3139 6 4 3 1
3140 6 4 3 1
3141 6 4 3 1
3142 6 4 3 1
3143 6 4 3 1
3144 6 4 3 1
3145 6 4 3 1
3150 6 4 3 1
3151 6 4 3 1
3152 6 4 3 1
3200 14 4 3 1
3210 6 4 3 1
3211 6 4 3 1
3212 6 4 3 1
3213 6 4 3 1
3220 14 4 3 1
3221 14 4 3 1
3222 14 4 3 1
3223 14 4 3 1
3224 14 4 3 1
3225 15 4 3 1
3226 14 4 3 1
3227 15 4 3 1
3228 15 4 3 1
3229 14 4 3 1
3230 14 4 3 1
3231 14 4 3 1
3232 14 4 3 1
3240 14 4 3 1
3241 14 4 3 1
3242 14 4 3 1
3300 14 4 3 1
3310 14 4 3 1
3320 14 4 3 1
3330 14 4 3 1
3340 14 4 3 1
3400 10 4 3 1
3410 10 4 3 1
3411 10 4 3 1
3412 10 4 3 1
3413 10 4 3 1
3414 10 4 3 1
3415 10 4 3 1
3416 10 4 3 1
3417 10 4 3 1
3419 10 4 3 1
3420 10 4 3 1
3421 10 4 3 1
3422 10 4 3 1
3423 10 4 3 1
3429 10 4 3 1
3430 10 4 3 1
3431 10 4 3 1
3432 10 4 3 1
3433 10 4 3 1
3434 6 4 3 1
3439 . 4 3 1
3440 10 4 3 1
3441 10 4 3 1
3442 10 4 3 1
3443 10 4 3 1
3444 10 4 3 1
3449 10 4 3 1
3450 10 4 3 1
3451 . 4 3 1
3452 . 4 3 1
3460 14 4 3 1
3470 14 4 3 1
3471 14 4 3 1
3472 14 4 3 1
3473 15 4 3 1
3474 15 4 3 1
3475 15 4 3 1
3480 14 4 3 1
4000 11 4 3 1
4100 11 4 3 1
4110 11 4 3 1
4111 11 4 3 1
4112 11 4 3 1
4113 12 4 3 1
4114 11 4 3 1
4115 11 4 3 1
4120 11 4 3 1
4121 11 4 3 1
4122 11 4 3 1
4130 11 4 3 1
4131 11 4 3 1
4132 11 4 3 1
4133 11 4 3 1
4140 11 4 3 1
4141 11 4 3 1
4142 11 4 3 1
4143 11 4 3 1
4144 11 4 3 1
4190 11 4 3 1
4200 11 4 3 1
4210 11 4 3 1
4211 12 4 3 1
4212 11 4 3 1
4213 11 4 3 1
4214 11 4 3 1
4215 11 4 3 1
4220 11 4 3 1
4221 11 4 3 1
4222 11 4 3 1
4223 12 4 3 1
5000 15 4 3 1
5100 15 4 3 1
5110 15 4 3 1
5111 15 4 3 1
5112 15 4 3 1
5113 15 4 3 1
5120 16 4 3 1
5121 16 4 3 1
5122 15 4 3 1
5123 16 4 3 1
5130 16 4 3 1
5131 15 4 3 1
5132 15 4 3 1
5133 16 4 3 1
5139 16 4 3 1
5140 15 4 3 1
5141 15 4 3 1
5142 16 4 3 1
5143 15 4 3 1
5149 16 4 3 1
5150 . 4 3 1
5151 . 4 3 1
5152 . 4 3 1
5160 15 4 3 1
5161 15 4 3 1
5162 15 4 3 1
5163 15 4 3 1
5164 15 4 3 1
5169 15 4 3 1
5200 15 4 3 1
5210 15 4 3 1
5220 15 4 3 1
5230 16 4 3 1
6000 7 4 3 1
6100 7 4 3 1
6110 7 4 3 1
6111 7 4 3 1
6112 7 4 3 1
6113 7 4 3 1
6114 7 4 3 1
6120 7 4 3 1
6121 7 4 3 1
6122 7 4 3 1
6123 7 4 3 1
6124 7 4 3 1
6129 7 4 3 1
6130 7 4 3 1
6131 7 4 3 1
6132 7 4 3 1
6133 7 4 3 1
6134 7 4 3 1
6140 7 4 3 1
6141 7 4 3 1
6142 7 4 3 1
6150 7 4 3 1
6151 7 4 3 1
6152 7 4 3 1
6153 7 4 3 1
6154 7 4 3 1
6200 7 4 3 1
6210 7 4 3 1
7000 7 4 3 1
7100 7 4 3 1
7110 7 4 3 1
7111 7 4 3 1
7112 7 4 3 1
7113 7 4 3 1
7120 7 4 3 1
7121 7 4 3 1
7122 7 4 3 1
7123 7 4 3 1
7124 7 4 3 1
7129 7 4 3 1
7130 7 4 3 1
7131 7 4 3 1
7132 7 4 3 1
7133 7 4 3 1
7134 7 4 3 1
7135 7 4 3 1
7136 7 4 3 1
7137 7 4 3 1
7139 7 4 3 1
7140 7 4 3 1
7141 7 4 3 1
7142 7 4 3 1
7143 7 4 3 1
7200 7 4 3 1
7210 7 4 3 1
7211 7 4 3 1
7212 7 4 3 1
7213 7 4 3 1
7214 7 4 3 1
7215 7 4 3 1
7216 7 4 3 1
7220 7 4 3 1
7221 7 4 3 1
7222 7 4 3 1
7223 7 4 3 1
7224 7 4 3 1
7230 7 4 3 1
7231 7 4 3 1
7232 7 4 3 1
7233 7 4 3 1
7234 7 4 3 1
7240 7 4 3 1
7241 7 4 3 1
7242 7 4 3 1
7243 7 4 3 1
7244 7 4 3 1
7245 7 4 3 1
7300 7 4 3 1
7310 7 4 3 1
7311 7 4 3 1
7312 7 4 3 1
7313 7 4 3 1
7320 7 4 3 1
7321 7 4 3 1
7322 7 4 3 1
7323 7 4 3 1
7324 7 4 3 1
7330 7 4 3 1
7331 7 4 3 1
7332 7 4 3 1
7340 7 4 3 1
7341 7 4 3 1
7342 7 4 3 1
7343 7 4 3 1
7344 7 4 3 1
7345 7 4 3 1
7346 7 4 3 1
7400 7 4 3 1
7410 7 4 3 1
7411 7 4 3 1
7412 7 4 3 1
7413 7 4 3 1
7414 7 4 3 1
7415 7 4 3 1
7416 7 4 3 1
7420 7 4 3 1
7421 7 4 3 1
7422 7 4 3 1
7423 7 4 3 1
7424 7 4 3 1
7430 7 4 3 1
7431 7 4 3 1
7432 7 4 3 1
7433 7 4 3 1
7434 7 4 3 1
7435 7 4 3 1
7436 7 4 3 1
7437 7 4 3 1
7440 7 4 3 1
7441 7 4 3 1
7442 7 4 3 1
7500 . 4 3 1
7510 . 4 3 1
7520 . 4 3 1
7530 . 4 3 1
8000 8 4 3 1
8100 8 4 3 1
8110 8 4 3 1
8111 8 4 3 1
8112 8 4 3 1
8113 8 4 3 1
8120 8 4 3 1
8121 8 4 3 1
8122 8 4 3 1
8123 8 4 3 1
8124 8 4 3 1
8130 8 4 3 1
8131 8 4 3 1
8139 8 4 3 1
8140 8 4 3 1
8141 8 4 3 1
8142 8 4 3 1
8143 8 4 3 1
8150 8 4 3 1
8151 8 4 3 1
8152 8 4 3 1
8153 8 4 3 1
8154 8 4 3 1
8155 8 4 3 1
8159 8 4 3 1
8160 8 4 3 1
8161 8 4 3 1
8162 8 4 3 1
8163 8 4 3 1
8170 8 4 3 1
8171 8 4 3 1
8172 8 4 3 1
8200 8 4 3 1
8210 8 4 3 1
8211 8 4 3 1
8212 8 4 3 1
8220 8 4 3 1
8221 8 4 3 1
8222 8 4 3 1
8223 8 4 3 1
8224 8 4 3 1
8229 8 4 3 1
8230 8 4 3 1
8231 8 4 3 1
8232 8 4 3 1
8240 8 4 3 1
8250 8 4 3 1
8251 8 4 3 1
8252 8 4 3 1
8253 8 4 3 1
8260 8 4 3 1
8261 8 4 3 1
8262 8 4 3 1
8263 8 4 3 1
8264 8 4 3 1
8265 8 4 3 1
8266 8 4 3 1
8269 8 4 3 1
8270 8 4 3 1
8271 8 4 3 1
8272 8 4 3 1
8273 8 4 3 1
8274 8 4 3 1
8275 8 4 3 1
8276 8 4 3 1
8277 8 4 3 1
8278 8 4 3 1
8279 8 4 3 1
8280 8 4 3 1
8281 8 4 3 1
8282 8 4 3 1
8283 8 4 3 1
8284 8 4 3 1
8285 8 4 3 1
8286 8 4 3 1
8287 8 4 3 1
8290 8 4 3 1
8300 8 4 3 1
8310 7 4 3 1
8311 7 4 3 1
8312 7 4 3 1
8320 8 4 3 1
8321 8 4 3 1
8322 16 4 3 1
8323 15 4 3 1
8324 7 4 3 1
8330 7 4 3 1
8331 8 4 3 1
8332 7 4 3 1
8333 7 4 3 1
8334 7 4 3 1
8340 7 4 3 1
8400 . 4 3 1
9000 . 4 3 1
9100 16 4 3 1
9110 16 4 3 1
9111 16 4 3 1
9112 16 4 3 1
9113 16 4 3 1
9120 16 4 3 1
9130 16 4 3 1
9131 16 4 3 1
9132 16 4 3 1
9133 16 4 3 1
9140 16 4 3 1
9141 16 4 3 1
9142 16 4 3 1
9150 16 4 3 1
9151 16 4 3 1
9152 16 4 3 1
9153 8 4 3 1
9160 8 4 3 1
9161 8 4 3 1
9162 8 4 3 1
9200 8 4 3 1
9210 8 4 3 1
9211 8 4 3 1
9212 8 4 3 1
9213 8 4 3 1
9300 8 4 3 1
9310 8 4 3 1
9311 8 4 3 1
9312 8 4 3 1
9313 8 4 3 1
9320 8 4 3 1
9321 8 4 3 1
9322 8 4 3 1
9330 8 4 3 1
9331 8 4 3 1
9332 8 4 3 1
9333 8 4 3 1
% END

% CODELIST-isco08-isco88
% source: isco0888.sps from http://www.harryganzeboom.nl/isco08/
% note: using first mapping in case of duplicates (repeated mappings); this is
%       consistent with the source because in SPSS later mappings are ignored
% variables: ICSO08 ISCO88
0110 0110
0210 0110
0310 0110
0100 0110
0200 0110
0300 0110
1000 1000
1100 1100
1110 1100
1111 1110
1112 1120
1113 1130
1114 1140
1120 1210
1200 1200
1210 1230
1211 1231
1212 1232
1213 1239
1219 1229
1220 1230
1221 1233
1222 1234
1223 1237
1300 1220
1310 1221
1311 1221
1312 1221
1320 1220
1321 1222
1322 1222
1323 1223
1324 1235
1330 1236
1340 1229
1341 1229
1342 1229
1343 1229
1344 1229
1345 1229
1346 1227
1349 1229
1400 1310
1410 1315
1411 1315
1412 1315
1420 1314
1430 1319
1431 1319
1439 1319
2000 2000
2100 2100
2110 2110
2111 2111
2112 2112
2113 2113
2114 2114
2120 2120
2130 2210
2131 2211
2132 2213
2133 2200
2140 2140
2141 2149
2142 2142
2143 2149
2144 2145
2145 2146
2146 2147
2149 2149
2150 2140
2151 2143
2152 2144
2153 2144
2160 2140
2161 2141
2162 2141
2163 3471
2164 2141
2165 2148
2166 3471
2200 2200
2210 2220
2211 2221
2212 2221
2220 2230
2221 2230
2222 2230
2230 3229
2240 3221
2250 2223
2260 3210
2261 2222
2262 2224
2263 2229
2264 3226
2265 3223
2266 3229
2267 3224
2269 2229
2310 2310
2320 2320
2330 2320
2300 2300
2340 2330
2341 2331
2342 2332
2350 2350
2351 2351
% 2351 2352 (duplicate)
2352 2340
2353 2359
2354 2359
2355 2359
2356 2359
2359 2359
2400 2400
2410 2410
2411 2411
2412 2419
2413 2419
2420 2419
2421 2419
2422 2419
2423 2412
2424 2412
2430 2410
2431 2419
2432 2419
2433 3415
2434 3415
2500 2100
2510 2130
2511 2131
2512 2131
2513 2131
% 2513 2139  (duplicate)
2514 2132
2519 2131
2520 2131
2521 2131
2522 2131
2523 2131
2529 2139
2600 2400
2610 2420
2611 2421
2612 2422
2619 2429
2620 2430
2621 2431
2622 2432
2630 2440
2631 2441
2632 2442
2633 2443
2634 2445
2635 2446
2636 2460
2640 2450
2641 2451
2642 2451
2643 2444
2650 2450
2651 2452
2652 2453
2653 2454
2654 2455
2655 2455
2656 3472
2659 3474
3000 3000
3100 3100
3110 3110
3111 3111
3112 3112
3113 3113
3114 3114
3115 3115
3116 3116
3117 3117
3118 3118
3119 3119
3120 1220
3121 1229
3122 1222
3123 1223
3130 8160
3131 8161
3132 8163
3133 8150
3134 8155
3135 8120
3139 8290
3140 3210
3141 3211
3142 3212
3143 3212
3150 3140
3151 3141
3152 3142
3153 3143
3154 3144
3155 3145
3200 3100
3210 3130
3211 3133
3212 3211
3213 3228
3214 7311
3220 3230
3221 3231
3222 3232
3230 3241
3240 3227
3250 3220
3251 3225
3252 3229
3253 3229
3254 3224
3255 3226
3256 3221
3257 3152
3258 3152
3259 3229
3300 3300
3310 3410
3311 3411
3312 3419
3313 3433
3314 3434
3315 3417
3320 3410
3321 3412
3322 3415
3323 3416
3324 3421
3330 3420
3331 3422
3332 3414
% 3332 3439 (duplicate)
3333 3423
3334 3413
3339 3429
3340 3430
3341 3431
3342 3431
3343 3431
3344 3431
3350 3440
3351 3441
3352 3442
3353 3443
3354 3444
3355 3450
3359 3449
3400 3400
3410 3430
3411 3432
3412 3460
3413 3480
3420 3470
3421 3475
3422 3475
3423 3340
% 3423 3475 (duplicate)
3430 3470
3431 3131
3432 3471
3433 3470
3434 5122
3435 3470
3500 3100
3510 3120
3511 3122
3512 3121
3513 2139
3514 3121
3520 3130
3521 3130
3522 3114
% 3522 3132 (duplicate)
4000 4000
4110 4100
4120 4115
4100 4100
4130 4110
4131 4112
4132 4113
4200 4200
4210 4210
4211 4211
4212 4213
4213 4214
4214 4215
4220 4220
4221 3414
4222 4222
4223 4223
4224 4222
4225 4222
4226 4222
4227 4190
4229 4222
4300 4100
4310 4120
4311 4121
4312 4122
4313 4121
4320 4130
4321 4131
4322 4132
4323 4133
4400 4100
4410 4140
4411 4141
4412 4142
4413 4143
4414 4144
4415 4141
4416 4190
4419 4190
5000 5000
5100 5100
5110 5110
5111 5111
5112 5112
5113 5113
5120 5122
5130 5120
5131 5123
5132 5123
5140 5140
5141 5141
5142 5141
5150 5120
5151 5121
5152 5121
5153 9141
5160 5140
5161 5152
5162 5142
5163 5143
5164 5149
5165 3340
5169 5149
5200 5200
5210 5230
5211 5230
5212 9111
5220 1314
5221 1314
5222 1314
5223 5220
5230 4211
5240 5220
5241 5210
5242 5220
5243 9113
5244 9113
5245 5220
5246 5123
5249 5220
5300 5100
5310 5130
5311 5131
5312 5131
5320 5130
5321 5132
5322 5133
5329 5139
5400 5100
5410 5160
5411 5161
5412 5162
5413 5163
5414 5169
5419 5169
6000 6000
6100 6100
6110 6110
6111 6111
6112 6112
6113 6113
6114 6114
6120 6120
6121 6124
6122 6122
6123 6123
6129 6129
6130 6130
6210 6141
6200 6150
6220 6150
6221 6151
6222 6152
6223 6153
6224 6154
6300 6200
6310 6210
6320 6210
6330 6210
6340 6210
7000 7000
7100 7100
7110 7120
7111 7129
7112 7122
7113 7113
7114 7123
7115 7124
7119 7129
7120 7130
7121 7131
7122 7132
7123 7133
7124 7134
7125 7135
7126 7136
7127 7240
7130 7140
7131 7141
7132 7142
7133 7143
7200 7200
7210 7210
7211 7211
7212 7212
7213 7213
7214 7214
7215 7215
7220 7220
7221 7221
7222 7222
7223 7223
7224 7224
7230 7230
7231 7231
7232 7232
7233 7233
7234 7231
7300 7300
7310 7310
7311 7311
7312 7312
7313 7313
7314 7321
7315 7322
7316 3471
7317 7331
7318 7332
7319 7330
7320 7340
7321 7340
7322 7340
7323 7345
7400 7200
7410 7240
7411 7137
7412 7241
7413 7245
7420 7240
7421 7242
% 7421 7243 (duplicate)
7422 7243
7500 7400
7510 7410
7511 7411
7512 7412
7513 7413
7514 7414
7515 7415
7516 7416
7520 7420
7521 7421
7522 7422
7523 7423
7530 7430
7531 7434
7532 7435
7533 7436
7534 7437
7535 7441
7536 7442
7540 7200
7541 7216
7542 7112
7543 3152
7544 7143
7549 7000
8000 8000
8100 8100
8110 8110
8111 7111
8112 8112
8113 8113
8114 8212
8120 8120
8121 8120
8122 8223
8130 8220
8131 8220
8132 8224
8140 8230
8141 8231
8142 8232
8143 8253
8150 8260
8151 8261
8152 8262
8153 8263
8154 8264
8155 8265
8156 8266
8157 8264
8159 8269
8160 8270
8170 8140
8171 8140
8172 8141
8180 8290
8181 8131
8182 8162
8183 8290
8189 8290
8200 8200
8210 8280
8211 8281
8212 8283
8219 8290
8300 8300
8310 8310
8311 8311
8312 8312
8320 8320
8321 8321
8322 8322
8330 8320
8331 8323
8332 8324
8340 8330
8341 8331
8342 8332
8343 8333
8344 8334
8350 8340
9000 9000
9100 9100
9110 9130
9111 9131
9112 9132
9120 9140
9121 9133
9122 9142
9123 9142
9129 9140
9200 9200
9210 9210
9211 9211
9212 9211
9213 9211
9214 9211
9215 9212
9216 9213
9300 9300
9310 9310
9311 9311
9312 9312
9313 9313
9320 9320
9321 9322
9329 9320
9330 9330
9331 9331
9332 9332
9333 9333
9334 9333
9400 9100
9410 9130
9411 5122
9412 9132
9500 9100
9510 9120
9520 9112
9600 9100
9610 9160
9611 9161
9612 9161
9613 9162
9620 9140
9621 9151
9622 9160
9623 9153
9624 9160
9629 9100
% 9999 0000 (unclassifiable occupation)
% END

% CODELIST-isco08-isei
% source: isco08_with_isei.pdf from http://www.harryganzeboom.nl/isco08/
% variables: ISCO-08 ISEI-08
0000 53
0100 65
0110 65
0200 53
0210 53
0300 30
0310 30
1000 62
1100 69
1110 68
1111 66
1112 70
1113 57
1114 68
1120 70
1200 68
1210 68
1211 68
1212 68
1213 68
1219 63
1220 68
1221 66
1222 67
1223 79
1300 60
1310 49
1311 60
1312 60
1320 60
1321 63
1322 60
1323 59
1324 57
1330 77
1340 59
1341 59
1342 59
1343 59
1344 59
1345 59
1346 59
1349 59
1400 53
1410 43
1411 43
1412 47
1420 56
1430 59
1431 59
1439 59
2000 65
2100 69
2110 77
2111 79
2112 70
2113 76
2114 80
2120 73
2130 67
2131 71
2132 64
2133 67
2140 72
2141 65
2142 76
2143 72
2144 69
2145 71
2146 74
2149 70
2150 74
2151 74
2152 75
2153 74
2160 60
2161 71
2162 60
2163 51
2164 60
2165 67
2166 60
2200 66
2210 89
2211 89
2212 89
2220 54
2221 42
2222 52
2230 49
2240 51
2250 71
2260 66
2261 86
2262 69
2263 66
2264 55
2265 53
2266 51
2267 58
2269 64
2300 63
2310 76
2320 65
2330 71
2340 57
2341 61
2342 47
2350 56
2351 67
2352 58
2353 54
2354 54
2355 54
2356 54
2359 54
2400 64
2410 66
2411 66
2412 66
2413 66
2420 59
2421 59
2422 62
2423 58
2424 59
2430 64
2431 64
2432 64
2433 64
2434 64
2500 69
2510 70
2511 70
2512 70
2513 70
2514 70
2519 70
2520 68
2521 68
2522 68
2523 68
2529 68
2600 66
2610 81
2611 85
2612 88
2619 72
2620 55
2621 55
2622 55
2630 65
2631 72
2632 75
2633 76
2634 74
2635 59
2636 53
2640 65
2641 65
2642 65
2643 68
2650 53
2651 51
2652 50
2653 53
2654 63
2655 64
2656 47
2659 41
3000 51
3100 51
3110 52
3111 49
3112 55
3113 51
3114 53
3115 52
3116 52
3117 59
3118 49
3119 50
3120 49
3121 49
3122 49
3123 49
3130 37
3131 41
3132 38
3133 37
3134 37
3135 37
3139 35
3140 47
3141 47
3142 48
3143 47
3150 59
3151 55
3152 47
3153 74
3154 67
3155 66
3200 46
3210 45
3211 51
3212 45
3213 40
3214 45
3220 48
3221 48
3222 42
3230 42
3240 30
3250 45
3251 43
3252 45
3253 45
3254 48
3255 40
3256 46
3257 50
3258 45
3259 45
3300 53
3310 51
3311 67
3312 51
3313 47
3314 63
3315 52
3320 55
3321 57
3322 55
3323 52
3324 54
3330 56
3331 54
3332 56
3333 55
3334 57
3339 57
3340 49
3341 57
3342 47
3343 49
3344 49
3350 55
3351 63
3352 61
3353 50
3354 52
3355 54
3359 55
3400 45
3410 45
3411 52
3412 42
3413 31
3420 46
3421 46
3422 46
3423 46
3430 47
3431 50
3432 47
3433 47
3434 47
3435 45
3500 57
3510 58
3511 56
3512 60
3513 50
3514 50
3520 46
3521 46
3522 46
4000 41
4100 41
4110 41
4120 42
4130 39
4131 42
4132 36
4200 40
4210 44
4211 44
4212 46
4213 70
4214 46
4220 37
4221 42
4222 37
4223 34
4224 37
4225 37
4226 37
4227 37
4229 37
4300 43
4310 47
4311 45
4312 52
4313 47
4320 38
4321 36
4322 41
4323 41
4400 40
4410 40
4411 42
4412 32
4413 42
4414 45
4415 40
4416 40
4419 40
5000 31
5100 30
5110 42
5111 44
5112 40
5113 41
5120 27
5130 29
5131 28
5132 30
5140 32
5141 32
5142 32
5150 29
5151 33
5152 33
5153 26
5160 33
5161 43
5162 24
5163 37
5164 33
5165 33
5169 34
5200 33
5210 28
5211 31
5212 23
5220 33
5221 45
5222 40
5223 31
5230 31
5240 35
5241 37
5242 41
5243 34
5244 35
5245 17
5246 25
5249 25
5300 26
5310 26
5311 26
5312 38
5320 27
5321 28
5322 24
5329 26
5400 40
5410 40
5411 49
5412 53
5413 49
5414 27
5419 38
6000 18
6100 18
6110 18
6111 16
6112 21
6113 24
6114 14
6120 24
6121 23
6122 20
6123 29
6129 27
6130 18
6200 24
6210 26
6220 21
6221 18
6222 19
6223 35
6224 10
6300 10
6310 10
6320 10
6330 10
6340 10
7000 35
7100 34
7110 34
7111 40
7112 32
7113 31
7114 32
7115 33
7119 35
7120 36
7121 36
7122 35
7123 29
7124 39
7125 33
7126 38
7127 38
7130 34
7131 33
7132 34
7133 48
7200 38
7210 37
7211 38
7212 37
7213 36
7214 38
7215 26
7220 38
7221 34
7222 40
7223 36
7224 41
7230 39
7231 38
7232 54
7233 38
7234 26
7300 33
7310 31
7311 38
7312 42
7313 36
7314 29
7315 35
7316 30
7317 33
7318 25
7319 34
7320 36
7321 38
7322 37
7323 34
7400 43
7410 43
7411 43
7412 42
7413 43
7420 44
7421 45
7422 41
7500 27
7510 29
7511 29
7512 29
7513 34
7514 24
7515 32
7516 10
7520 32
7521 27
7522 34
7523 21
7530 25
7531 24
7532 27
7533 24
7534 29
7535 30
7536 27
7540 32
7541 32
7542 49
7543 32
7544 32
7549 32
8000 32
8100 29
8110 39
8111 40
8112 39
8113 46
8114 33
8120 33
8121 35
8122 31
8130 35
8131 35
8132 34
8140 31
8141 29
8142 31
8143 36
8150 21
8151 27
8152 20
8153 18
8154 19
8155 22
8156 18
8157 24
8159 27
8160 22
8170 29
8171 33
8172 27
8180 29
8181 25
8182 26
8183 27
8189 30
8200 29
8210 29
8211 33
8212 27
8219 28
8300 36
8310 44
8311 52
8312 35
8320 36
8321 33
8322 36
8330 36
8331 37
8332 36
8340 31
8341 22
8342 35
8343 35
8344 29
8350 44
9000 20
9100 17
9110 17
9111 17
9112 16
9120 20
9121 19
9122 20
9123 20
9129 20
9200 14
9210 15
9211 16
9212 20
9213 18
9214 16
9215 19
9216 19
9300 24
9310 22
9311 24
9312 23
9313 22
9320 23
9321 23
9329 21
9330 27
9331 21
9332 31
9333 28
9334 20
9400 15
9410 15
9411 20
9412 10
9500 25
9510 22
9520 26
9600 26
9610 17
9611 18
9612 17
9613 17
9620 29
9621 30
9622 20
9623 34
9624 20
9629 20
% END

% CODELIST-isco08-siops
% source: isqotrei08.sps from http://www.harryganzeboom.nl/isco08/
% note: leading zeros added to ISCO-08 codes with less than 4 digits
% variables: ISCO-08 SIOPS-08
0000 42.88
0100 48.68
0110 48.68
0200 39.00
0210 39.00
0300 43.23
0310 43.23
1000 48.87
1100 64.92
1110 65.38
1111 64.00
1112 66.84
1113 42.00
1114 54.88
1120 64.51
1200 56.60
1210 53.28
1211 49.00
1212 58.00
1213 53.28
1219 53.28
1220 54.61
1221 52.00
1222 52.36
1223 52.36
1300 61.15
1310 61.15
1311 61.15
1312 61.15
1320 60.67
1321 64.00
1322 60.67
1323 53.00
1324 57.21
1330 61.15
1340 73.51
1341 73.51
1342 73.51
1343 73.51
1344 73.51
1345 72.00
1346 73.51
1349 75.00
1400 44.83
1410 39.28
1411 37.52
1412 39.91
1420 45.13
1430 47.00
1431 47.00
1439 47.00
2000 62.42
2100 63.15
2110 69.75
2111 75.68
2112 69.51
2113 69.00
2114 67.00
2120 58.16
2130 62.66
2131 62.66
2132 62.66
2133 62.66
2140 61.39
2141 54.00
2142 70.00
2143 61.69
2144 66.00
2145 66.00
2146 61.16
2149 55.00
2150 65.00
2151 65.00
2152 65.00
2153 65.00
2160 66.52
2161 63.19
2162 63.19
2163 63.19
2164 63.19
2165 58.00
2166 63.19
2200 69.03
2210 78.01
2211 78.01
2212 78.01
2220 53.82
2221 54.05
2222 46.00
2230 69.03
2240 69.03
2250 61.00
2260 65.80
2261 70.00
2262 64.00
2263 65.80
2264 56.21
2265 52.00
2266 65.80
2267 60.15
2269 65.80
2300 63.18
2310 78.16
2320 57.00
2330 62.63
2340 56.97
2341 57.00
2342 49.00
2350 62.15
2351 62.28
2352 62.00
2353 62.28
2354 62.28
2355 62.28
2356 62.28
2359 68.00
2400 57.18
2410 58.45
2411 58.45
2412 58.45
2413 58.45
2420 55.80
2421 55.80
2422 55.80
2423 55.80
2424 55.80
2430 50.09
2431 50.09
2432 57.00
2433 48.37
2434 50.09
2500 51.00
2510 51.00
2511 51.00
2512 51.00
2513 51.00
2514 51.00
2519 51.00
2520 51.00
2521 51.00
2522 51.00
2523 51.00
2529 51.00
2600 59.97
2610 73.14
2611 73.10
2612 76.11
2619 69.40
2620 54.00
2621 54.00
2622 54.00
2630 56.30
2631 60.00
2632 68.51
2633 56.30
2634 66.00
2635 53.89
2636 54.00
2640 57.40
2641 56.92
2642 62.99
2643 65.66
2650 52.40
2651 57.00
2652 46.09
2653 40.00
2654 66.78
2655 56.79
2656 50.00
2659 33.00
3000 48.11
3100 46.74
3110 49.72
3111 46.00
3112 52.72
3113 46.00
3114 49.65
3115 46.00
3116 46.00
3117 54.00
3118 55.00
3119 46.00
3120 45.94
3121 45.94
3122 45.94
3123 45.94
3130 34.91
3131 34.76
3132 34.91
3133 34.91
3134 43.00
3135 34.91
3139 34.91
3140 49.00
3141 52.00
3142 48.57
3143 49.00
3150 59.00
3151 60.00
3152 55.44
3153 66.00
3154 59.00
3155 59.00
3200 50.27
3210 55.90
3211 58.00
3212 54.58
3213 44.00
3214 60.00
3220 44.00
3221 44.00
3222 44.00
3230 50.27
3240 48.00
3250 49.87
3251 49.87
3252 49.87
3253 49.87
3254 57.00
3255 49.87
3256 50.00
3257 48.00
3258 49.87
3259 49.89
3300 48.81
3310 45.93
3311 56.00
3312 45.93
3313 43.81
3314 51.00
3315 48.97
3320 47.20
3321 46.00
3322 47.00
3323 48.86
3324 55.00
3330 47.61
3331 47.61
3332 47.61
3333 47.61
3334 49.00
3339 42.15
3340 48.81
3341 48.81
3342 48.81
3343 48.81
3344 48.81
3350 54.81
3351 54.64
3352 52.00
3353 54.64
3354 54.64
3355 54.64
3359 54.64
3400 50.11
3410 49.84
3411 54.17
3412 49.00
3413 39.00
3420 49.63
3421 48.62
3422 50.00
3423 49.63
3430 50.38
3431 45.69
3432 38.00
3433 50.38
3434 38.00
3435 54.59
3500 47.24
3510 53.00
3511 53.00
3512 53.00
3513 53.00
3514 53.00
3520 44.87
3521 44.87
3522 44.87
4000 38.54
4100 41.81
4110 42.09
4120 50.87
4130 44.16
4131 42.00
4132 45.00
4200 38.19
4210 38.53
4211 40.28
4212 38.53
4213 38.53
4214 27.00
4220 37.83
4221 43.00
4222 37.83
4223 42.15
4224 33.00
4225 37.83
4226 34.24
4227 37.83
4229 37.83
4300 32.37
4310 34.00
4311 34.00
4312 34.00
4313 34.00
4320 31.89
4321 30.28
4322 44.00
4323 32.49
4400 34.44
4410 34.44
4411 36.00
4412 30.05
4413 41.00
4414 34.44
4415 31.00
4416 34.44
4419 37.00
5000 35.11
5100 25.92
5110 31.07
5111 50.00
5112 30.93
5113 31.07
5120 30.98
5130 22.11
5131 21.67
5132 23.00
5140 32.00
5141 32.00
5142 32.00
5150 24.00
5151 37.03
5152 24.00
5153 23.27
5160 22.90
5161 22.90
5162 17.00
5163 34.00
5164 22.90
5165 41.00
5169 22.90
5200 36.95
5210 36.69
5211 36.69
5212 36.69
5220 38.84
5221 48.00
5222 42.78
5223 42.78
5230 32.16
5240 24.09
5241 24.09
5242 24.09
5243 24.00
5244 26.00
5245 25.00
5246 16.00
5249 24.09
5300 30.21
5310 47.66
5311 23.00
5312 50.00
5320 29.00
5321 29.00
5322 29.00
5329 29.00
5400 33.71
5410 33.80
5411 35.00
5412 40.89
5413 39.00
5414 27.89
5419 33.71
6000 42.02
6100 43.53
6110 43.53
6111 43.53
6112 43.53
6113 43.53
6114 43.53
6120 43.53
6121 43.53
6122 43.53
6123 43.53
6129 43.53
6130 43.53
6200 26.11
6210 22.69
6220 27.24
6221 27.24
6222 27.24
6223 27.24
6224 27.24
6300  5.00
6310  5.00
6320  5.00
6330  5.00
6340  5.00
7000 37.00
7100 33.06
7110 34.10
7111 34.10
7112 34.00
7113 38.00
7114 34.00
7115 36.76
7119 29.83
7120 32.27
7121 31.00
7122 32.27
7123 31.00
7124 28.00
7125 26.00
7126 34.01
7127 32.27
7130 30.48
7131 30.92
7132 29.00
7133 25.00
7200 40.26
7210 37.72
7211 37.45
7212 39.00
7213 33.02
7214 44.00
7215 32.00
7220 37.84
7221 35.42
7222 39.96
7223 37.87
7224 27.62
7230 41.37
7231 44.00
7232 49.89
7233 42.17
7234 28.00
7300 38.51
7310 35.06
7311 43.52
7312 33.00
7313 43.00
7314 25.00
7315 32.48
7316 31.00
7317 21.00
7318 35.06
7319 35.06
7320 41.46
7321 42.31
7322 41.08
7323 32.00
7400 41.08
7410 41.17
7411 44.01
7412 38.00
7413 36.00
7420 40.69
7421 44.15
7422 35.00
7500 34.69
7510 30.11
7511 23.79
7512 33.00
7513 34.00
7514 30.11
7515 34.00
7516 34.18
7520 38.08
7521 39.41
7522 39.99
7523 36.00
7530 32.23
7531 38.57
7532 41.00
7533 26.00
7534 31.00
7535 22.00
7536 27.37
7540 39.22
7541 39.22
7542 39.22
7543 38.81
7544 39.22
7549 41.00
8000 32.45
8100 32.77
8110 32.11
8111 32.25
8112 32.00
8113 31.00
8114 30.00
8120 38.86
8121 41.13
8122 28.00
8130 36.54
8131 31.55
8132 36.00
8140 29.61
8141 30.00
8142 29.30
8143 28.00
8150 27.78
8151 32.64
8152 30.20
8153 27.86
8154 23.32
8155 27.86
8156 27.86
8157 27.86
8159 27.86
8160 36.28
8170 29.55
8171 28.00
8172 29.95
8180 25.00
8181 25.00
8182 25.00
8183 25.00
8189 25.00
8200 39.34
8210 35.52
8211 39.34
8212 48.00
8219 39.34
8300 31.89
8310 34.28
8311 39.41
8312 29.09
8320 30.76
8321 30.76
8322 30.76
8330 32.91
8331 32.00
8332 33.00
8340 31.60
8341 31.00
8342 32.00
8343 32.39
8344 28.00
8350 28.69
9000 20.76
9100 20.26
9110 20.00
9111 20.00
9112 20.00
9120 20.03
9121 20.03
9122 20.03
9123 20.03
9129 20.03
9200 22.28
9210 22.16
9211 21.00
9212 26.00
9213 22.28
9214 21.00
9215 42.00
9216 22.28
9300 18.96
9310 18.02
9311 18.02
9312 27.60
9313 15.00
9320 20.09
9321 22.00
9329 21.08
9330 20.39
9331 17.00
9332 23.97
9333 20.30
9334 20.39
9400 22.00
9410 22.00
9411 22.00
9412 22.00
9500 21.72
9510 12.00
9520 21.80
9600 20.08
9610 13.00
9611 13.00
9612 13.00
9613 13.00
9620 21.89
9621 23.13
9622 22.88
9623 21.00
9624 22.88
9629 24.00
% END

% CODELIST-isco08-esec
% source: esec_08_3_digit_public.xlsx obtained on 27jun2019 from 
%         https://ekharrison.weebly.com/european-socio-economic-classification-esec.html
% note: codes 960/961/962 are not covered in the 3-digit list in 
%       esec_08_3_digit_public.xlsx; because ESEC classes are the same for 95 
%       and 96 in the 2-digit list (also in esec_08_3_digit_public.xlsx), codes
%       960/961/962 are added to the list below using the same classes as for
%       950/951/952
% variables: ISCO-08(3 digit) ESEC-employee ESEC-supervisor
%            ESEC-selfemp(0) ESEC-selfemp(1-9) ESEC-selfemp(10+)
011 1 1 1 1 1
021 3 2 3 3 3
031 3 2 3 3 3
100 1 1 4 4 1
110 1 1 1 1 1
111 1 1 1 1 1
112 1 1 1 1 1
120 1 1 4 4 1
121 1 1 4 4 1
122 1 1 4 4 1
130 1 1 4 4 1
131 2 2 5 5 1
132 2 2 4 4 1
133 1 1 4 4 1
134 1 1 4 4 1
140 2 2 4 4 1
141 2 2 4 4 1
142 2 2 4 4 1
143 2 2 4 4 1
200 1 1 1 1 1
210 1 1 1 1 1
211 1 1 1 1 1
212 1 1 1 1 1
213 1 1 1 1 1
214 1 1 1 1 1
215 1 1 1 1 1
216 1 1 1 1 1
220 1 1 1 1 1
221 1 1 1 1 1
222 2 2 2 2 1
223 2 2 2 2 1
224 6 6 4 4 1
225 1 1 1 1 1
226 1 1 1 1 1
230 1 1 1 1 1
231 1 1 1 1 1
232 1 1 1 1 1
233 2 2 2 2 1
234 2 2 2 2 1
235 1 1 1 1 1
240 1 1 1 1 1
241 1 1 1 1 1
242 2 2 2 2 1
243 1 1 1 1 1
250 1 1 1 1 1
251 1 1 1 1 1
252 2 2 2 2 1
260 2 2 2 2 1
261 1 1 1 1 1
262 2 2 2 2 1
263 1 1 1 1 1
264 2 2 2 2 1
265 2 2 2 2 1
300 3 2 4 4 1
310 2 2 2 2 1
311 2 2 2 2 1
312 2 2 2 2 1
313 6 2 4 4 1
314 2 2 2 2 1
315 2 2 2 2 1
320 6 6 4 4 1
321 2 2 2 2 1
322 2 2 2 2 1
323 2 2 2 2 1
324 3 6 4 4 1
325 3 2 4 4 1
330 1 1 1 1 1
331 1 1 1 1 1
332 1 1 1 1 1
333 2 2 2 2 1
334 3 2 4 4 1
335 2 2 2 2 2
340 3 2 4 4 1
341 3 2 2 2 1
342 3 2 4 4 1
343 3 2 4 4 1
350 3 2 4 4 1
351 3 2 4 4 1
352 3 2 4 4 1
400 3 2 4 4 1
410 3 2 4 4 1
411 3 2 4 4 1
412 3 2 4 4 1
413 3 2 4 4 1
420 7 6 4 4 1
421 7 6 4 4 1
422 7 6 4 4 1
430 3 2 4 4 1
431 3 2 4 4 1
432 7 2 4 4 1
440 3 6 4 4 1
441 3 6 4 4 1
500 7 6 4 4 1
510 7 6 4 4 1
511 7 6 4 4 1
512 7 6 4 4 1
513 7 6 4 4 1
514 7 6 4 4 1
515 7 6 4 4 1
516 7 6 4 4 1
520 7 6 4 4 1
521 9 7 4 4 1
522 7 6 4 4 1
523 7 6 4 4 1
524 7 6 4 4 1
530 7 6 4 4 1
531 7 6 4 4 1
532 7 6 4 4 1
540 7 6 3 3 3
541 7 6 3 3 3
600 8 6 5 5 1
610 8 6 5 5 1
611 8 6 5 5 1
612 8 6 5 5 1
613 8 6 5 5 1
620 8 6 5 5 1
621 8 6 5 5 1
622 8 6 5 5 1
630 5 5 5 5 5
631 5 5 5 5 5
632 5 5 5 5 5
633 5 5 5 5 5
634 5 5 5 5 5
700 8 6 4 4 1
710 8 6 4 4 1
711 8 6 4 4 1
712 8 6 4 4 1
713 8 6 4 4 1
720 8 6 4 4 1
721 8 6 4 4 1
722 8 6 4 4 1
723 8 6 4 4 1
730 8 6 4 4 1
731 8 6 4 4 1
732 8 6 4 4 1
740 8 6 4 4 1
741 8 6 4 4 1
742 6 6 4 4 1
750 8 6 4 4 1
751 8 6 4 4 1
752 8 6 4 4 1
753 8 6 4 4 1
754 8 6 4 4 1
800 9 6 4 4 1
810 9 6 4 4 1
811 9 6 4 4 1
812 9 6 4 4 1
813 9 6 4 4 1
814 9 6 4 4 1
815 9 6 4 4 1
816 9 6 4 4 1
817 9 6 4 4 1
818 9 6 4 4 1
820 9 6 4 4 1
821 9 6 4 4 1
830 8 6 4 4 1
831 8 6 4 4 1
832 9 6 4 4 1
833 8 6 4 4 1
834 9 6 4 4 1
835 8 6 4 4 1
900 9 6 4 4 1
910 9 6 4 4 1
911 9 6 4 4 1
912 9 6 4 4 1
920 9 6 5 5 1
921 9 6 5 5 1
930 9 6 5 5 1
931 9 6 4 4 1
932 9 6 4 4 1
933 9 6 4 4 1
940 9 6 4 4 1
941 9 6 4 4 1
950 9 6 4 4 1
951 9 6 4 4 1
952 9 6 4 4 1
960 9 6 4 4 1  % see note above
961 9 6 4 4 1  % see note above
962 9 6 4 4 1  % see note above
% END

% CODELIST-isco08-oesch
% source: iscooesch.ado (May 2018) by Simon Kaiser
% note: the codelist has been generated automatically by applying iskooesch.ado
%       to all relevant combinations of isco codes, selfemployment, supervision
% variables: ISCO-88 OESCH-employee OESCH-selfemp(0) OESCH-selfemp(1-9)
%            OESCH-selfemp(10+)
0100 9 . . 1
0110 9 . . 1
0200 10 . . 1
0210 10 . . 1
1000 9 4 3 1
1100 9 4 3 1
1110 9 4 3 1
1111 9 4 3 1
1112 9 4 3 1
1113 9 4 3 1
1114 9 4 3 1
1120 9 4 3 1
1200 9 4 3 1
1210 9 4 3 1
1211 9 4 3 1
1212 9 4 3 1
1213 9 4 3 1
1219 9 4 3 1
1220 9 4 3 1
1221 9 4 3 1
1222 9 4 3 1
1223 9 4 3 1
1300 9 4 3 1
1310 10 4 3 1
1311 10 4 3 1
1312 10 4 3 1
1320 9 4 3 1
1321 9 4 3 1
1322 9 4 3 1
1323 9 4 3 1
1324 9 4 3 1
1330 9 4 3 1
1340 9 4 3 1
1341 9 4 3 1
1342 9 4 3 1
1343 9 4 3 1
1344 9 4 3 1
1345 9 4 3 1
1346 9 4 3 1
1349 9 4 3 1
1400 10 4 3 1
1410 10 4 3 1
1411 10 4 3 1
1412 10 4 3 1
1420 10 4 3 1
1430 10 4 3 1
1431 10 4 3 1
1439 10 4 3 1
2000 . 2 2 1
2100 5 2 2 1
2110 5 2 2 1
2111 5 2 2 1
2112 5 2 2 1
2113 5 2 2 1
2114 5 2 2 1
2120 5 2 2 1
2130 5 2 2 1
2131 5 2 2 1
2132 5 2 2 1
2133 5 2 2 1
2140 5 2 2 1
2141 5 2 2 1
2142 5 2 2 1
2143 5 2 2 1
2144 5 2 2 1
2145 5 2 2 1
2146 5 2 2 1
2149 5 2 2 1
2150 5 2 2 1
2151 5 2 2 1
2152 5 2 2 1
2153 5 2 2 1
2160 5 2 2 1
2161 5 2 2 1
2162 5 2 2 1
2163 14 4 3 1
2164 5 2 2 1
2165 5 2 2 1
2166 14 4 3 1
2200 13 2 2 1
2210 13 2 2 1
2211 13 2 2 1
2212 13 2 2 1
2220 14 4 3 1
2221 14 4 3 1
2222 14 4 3 1
2230 14 4 3 1
2240 14 4 3 1
2250 13 2 2 1
2260 14 4 3 1
2261 13 2 2 1
2262 13 2 2 1
2263 14 4 3 1
2264 14 4 3 1
2265 14 4 3 1
2266 14 4 3 1
2267 14 4 3 1
2269 14 4 3 1
2300 13 2 2 1
2310 13 2 2 1
2320 13 2 2 1
2330 13 2 2 1
2340 14 4 3 1
2341 14 4 3 1
2342 14 4 3 1
2350 13 2 2 1
2351 13 2 2 1
2352 13 2 2 1
2353 14 4 3 1
2354 14 4 3 1
2355 14 4 3 1
2356 14 4 3 1
2359 13 2 2 1
2400 9 2 2 1
2410 9 2 2 1
2411 9 2 2 1
2412 9 2 2 1
2413 9 2 2 1
2420 9 2 2 1
2421 9 2 2 1
2422 9 2 2 1
2423 9 2 2 1
2424 9 2 2 1
2430 9 2 2 1
2431 9 2 2 1
2432 9 2 2 1
2433 10 4 3 1
2434 10 4 3 1
2500 5 2 2 1
2510 5 2 2 1
2511 5 2 2 1
2512 5 2 2 1
2513 5 2 2 1
2514 5 2 2 1
2519 5 2 2 1
2520 5 2 2 1
2521 5 2 2 1
2522 5 2 2 1
2523 5 2 2 1
2529 5 2 2 1
2600 13 2 2 1
2610 9 2 2 1
2611 9 2 2 1
2612 9 2 2 1
2619 9 2 2 1
2620 14 4 3 1
2621 13 2 2 1
2622 14 4 3 1
2630 13 2 2 1
2631 9 2 2 1
2632 13 2 2 1
2633 13 2 2 1
2634 13 2 2 1
2635 14 4 3 1
2636 13 2 2 1
2640 13 2 2 1
2641 14 4 3 1
2642 13 2 2 1
2643 13 2 2 1
2650 14 4 3 1
2651 14 4 3 1
2652 14 4 3 1
2653 14 4 3 1
2654 14 4 3 1
2655 14 4 3 1
2656 14 4 3 1
2659 14 4 3 1
3000 . 4 3 1
3100 6 4 3 1
3110 6 4 3 1
3111 6 4 3 1
3112 6 4 3 1
3113 6 4 3 1
3114 6 4 3 1
3115 6 4 3 1
3116 6 4 3 1
3117 6 4 3 1
3118 6 4 3 1
3119 6 4 3 1
3120 6 4 3 1
3121 6 4 3 1
3122 6 4 3 1
3123 6 4 3 1
3130 6 4 3 1
3131 6 4 3 1
3132 6 4 3 1
3133 6 4 3 1
3134 6 4 3 1
3135 6 4 3 1
3139 6 4 3 1
3140 6 4 3 1
3141 6 4 3 1
3142 6 4 3 1
3143 6 4 3 1
3150 6 4 3 1
3151 6 4 3 1
3152 6 4 3 1
3153 6 4 3 1
3154 6 4 3 1
3155 6 4 3 1
3200 14 4 3 1
3210 6 4 3 1
3211 6 4 3 1
3212 6 4 3 1
3213 6 4 3 1
3214 6 4 3 1
3220 14 4 3 1
3221 14 4 3 1
3222 14 4 3 1
3230 14 4 3 1
3240 15 4 3 1
3250 14 4 3 1
3251 15 4 3 1
3252 6 4 3 1
3253 14 4 3 1
3254 14 4 3 1
3255 14 4 3 1
3256 14 4 3 1
3257 14 4 3 1
3258 15 4 3 1
3259 14 4 3 1
3300 10 4 3 1
3310 10 4 3 1
3311 10 4 3 1
3312 10 4 3 1
3313 10 4 3 1
3314 10 4 3 1
3315 10 4 3 1
3320 10 4 3 1
3321 10 4 3 1
3322 10 4 3 1
3323 10 4 3 1
3324 10 4 3 1
3330 10 4 3 1
3331 10 4 3 1
3332 10 4 3 1
3333 10 4 3 1
3334 10 4 3 1
3339 10 4 3 1
3340 11 4 3 1
3341 11 4 3 1
3342 11 4 3 1
3343 10 4 3 1
3344 11 4 3 1
3350 10 4 3 1
3351 10 4 3 1
3352 10 4 3 1
3353 10 4 3 1
3354 10 4 3 1
3355 10 4 3 1
3359 10 4 3 1
3400 14 4 3 1
3410 14 4 3 1
3411 10 4 3 1
3412 14 4 3 1
3413 14 4 3 1
3420 15 4 3 1
3421 15 4 3 1
3422 15 4 3 1
3423 15 4 3 1
3430 14 4 3 1
3431 14 4 3 1
3432 14 4 3 1
3433 14 4 3 1
3434 15 4 3 1
3435 14 4 3 1
3500 6 4 3 1
3510 6 4 3 1
3511 6 4 3 1
3512 6 4 3 1
3513 6 4 3 1
3514 6 4 3 1
3520 6 4 3 1
3521 6 4 3 1
3522 6 4 3 1
4000 11 4 3 1
4100 11 4 3 1
4110 11 4 3 1
4120 11 4 3 1
4130 11 4 3 1
4131 11 4 3 1
4132 12 4 3 1
4200 11 4 3 1
4210 11 4 3 1
4211 11 4 3 1
4212 11 4 3 1
4213 11 4 3 1
4214 11 4 3 1
4220 11 4 3 1
4221 11 4 3 1
4222 12 4 3 1
4223 12 4 3 1
4224 11 4 3 1
4225 11 4 3 1
4226 11 4 3 1
4227 11 4 3 1
4229 11 4 3 1
4300 11 4 3 1
4310 11 4 3 1
4311 11 4 3 1
4312 11 4 3 1
4313 11 4 3 1
4320 11 4 3 1
4321 11 4 3 1
4322 11 4 3 1
4323 11 4 3 1
4400 11 4 3 1
4410 11 4 3 1
4411 11 4 3 1
4412 11 4 3 1
4413 11 4 3 1
4414 14 4 3 1
4415 11 4 3 1
4416 11 4 3 1
4419 11 4 3 1
5000 15 4 3 1
5100 15 4 3 1
5110 15 4 3 1
5111 15 4 3 1
5112 15 4 3 1
5113 15 4 3 1
5120 15 4 3 1
5130 16 4 3 1
5131 16 4 3 1
5132 16 4 3 1
5140 15 4 3 1
5141 15 4 3 1
5142 15 4 3 1
5150 16 4 3 1
5151 16 4 3 1
5152 16 4 3 1
5153 16 4 3 1
5160 16 4 3 1
5161 16 4 3 1
5162 16 4 3 1
5163 15 4 3 1
5164 16 4 3 1
5165 15 4 3 1
5169 16 4 3 1
5200 15 4 3 1
5210 16 4 3 1
5211 16 4 3 1
5212 16 4 3 1
5220 15 4 3 1
5221 10 4 3 1
5222 15 4 3 1
5223 15 4 3 1
5230 12 4 3 1
5240 16 4 3 1
5241 15 4 3 1
5242 15 4 3 1
5243 16 4 3 1
5244 16 4 3 1
5245 16 4 3 1
5246 16 4 3 1
5249 16 4 3 1
5300 15 4 3 1
5310 15 4 3 1
5311 15 4 3 1
5312 15 4 3 1
5320 15 4 3 1
5321 15 4 3 1
5322 16 4 3 1
5329 16 4 3 1
5400 15 4 3 1
5410 15 4 3 1
5411 15 4 3 1
5412 15 4 3 1
5413 15 4 3 1
5414 16 4 3 1
5419 15 4 3 1
6000 7 4 3 1
6100 7 4 3 1
6110 7 4 3 1
6111 7 4 3 1
6112 7 4 3 1
6113 7 4 3 1
6114 7 4 3 1
6120 7 4 3 1
6121 7 4 3 1
6122 7 4 3 1
6123 7 4 3 1
6129 7 4 3 1
6130 7 4 3 1
6200 7 4 3 1
6210 7 4 3 1
6220 7 4 3 1
6221 7 4 3 1
6222 7 4 3 1
6223 7 4 3 1
6224 7 4 3 1
6300 7 4 3 1
6310 7 4 3 1
6320 7 4 3 1
6330 7 4 3 1
6340 7 4 3 1
7000 7 4 3 1
7100 7 4 3 1
7110 7 4 3 1
7111 7 4 3 1
7112 7 4 3 1
7113 7 4 3 1
7114 7 4 3 1
7115 7 4 3 1
7119 7 4 3 1
7120 7 4 3 1
7121 7 4 3 1
7122 7 4 3 1
7123 7 4 3 1
7124 7 4 3 1
7125 7 4 3 1
7126 7 4 3 1
7127 7 4 3 1
7130 7 4 3 1
7131 7 4 3 1
7132 7 4 3 1
7133 7 4 3 1
7200 7 4 3 1
7210 7 4 3 1
7211 7 4 3 1
7212 7 4 3 1
7213 7 4 3 1
7214 7 4 3 1
7215 7 4 3 1
7220 7 4 3 1
7221 7 4 3 1
7222 7 4 3 1
7223 7 4 3 1
7224 7 4 3 1
7230 7 4 3 1
7231 7 4 3 1
7232 7 4 3 1
7233 7 4 3 1
7234 7 4 3 1
7300 7 4 3 1
7310 7 4 3 1
7311 7 4 3 1
7312 7 4 3 1
7313 7 4 3 1
7314 7 4 3 1
7315 7 4 3 1
7316 7 4 3 1
7317 7 4 3 1
7318 7 4 3 1
7319 7 4 3 1
7320 7 4 3 1
7321 7 4 3 1
7322 7 4 3 1
7323 7 4 3 1
7400 7 4 3 1
7410 7 4 3 1
7411 7 4 3 1
7412 7 4 3 1
7413 7 4 3 1
7420 7 4 3 1
7421 7 4 3 1
7422 7 4 3 1
7500 7 4 3 1
7510 7 4 3 1
7511 7 4 3 1
7512 7 4 3 1
7513 7 4 3 1
7514 7 4 3 1
7515 7 4 3 1
7516 7 4 3 1
7520 7 4 3 1
7521 7 4 3 1
7522 7 4 3 1
7523 7 4 3 1
7530 7 4 3 1
7531 7 4 3 1
7532 7 4 3 1
7533 7 4 3 1
7534 7 4 3 1
7535 7 4 3 1
7536 7 4 3 1
7540 7 4 3 1
7541 7 4 3 1
7542 7 4 3 1
7543 7 4 3 1
7544 7 4 3 1
7549 7 4 3 1
8000 8 4 3 1
8100 8 4 3 1
8110 8 4 3 1
8111 8 4 3 1
8112 8 4 3 1
8113 8 4 3 1
8114 8 4 3 1
8120 8 4 3 1
8121 8 4 3 1
8122 8 4 3 1
8130 8 4 3 1
8131 8 4 3 1
8132 8 4 3 1
8140 8 4 3 1
8141 8 4 3 1
8142 8 4 3 1
8143 8 4 3 1
8150 8 4 3 1
8151 8 4 3 1
8152 8 4 3 1
8153 8 4 3 1
8154 8 4 3 1
8155 8 4 3 1
8156 8 4 3 1
8157 8 4 3 1
8159 8 4 3 1
8160 8 4 3 1
8170 8 4 3 1
8171 8 4 3 1
8172 8 4 3 1
8180 8 4 3 1
8181 8 4 3 1
8182 8 4 3 1
8183 8 4 3 1
8189 8 4 3 1
8200 8 4 3 1
8210 8 4 3 1
8211 8 4 3 1
8212 8 4 3 1
8219 8 4 3 1
8300 8 4 3 1
8310 7 4 3 1
8311 7 4 3 1
8312 7 4 3 1
8320 8 4 3 1
8321 8 4 3 1
8322 16 4 3 1
8330 7 4 3 1
8331 15 4 3 1
8332 7 4 3 1
8340 7 4 3 1
8341 8 4 3 1
8342 7 4 3 1
8343 7 4 3 1
8344 7 4 3 1
8350 8 4 3 1
9000 . 4 3 1
9100 16 4 3 1
9110 16 4 3 1
9111 16 4 3 1
9112 16 4 3 1
9120 16 4 3 1
9121 16 4 3 1
9122 16 4 3 1
9123 16 4 3 1
9129 16 4 3 1
9200 8 4 3 1
9210 8 4 3 1
9211 8 4 3 1
9212 8 4 3 1
9213 8 4 3 1
9214 8 4 3 1
9215 8 4 3 1
9216 8 4 3 1
9300 8 4 3 1
9310 8 4 3 1
9311 8 4 3 1
9312 8 4 3 1
9313 8 4 3 1
9320 8 4 3 1
9321 8 4 3 1
9329 8 4 3 1
9330 8 4 3 1
9331 8 4 3 1
9332 8 4 3 1
9333 8 4 3 1
9334 8 4 3 1
9400 16 4 3 1
9410 16 4 3 1
9411 16 4 3 1
9412 16 4 3 1
9500 16 4 3 1
9510 16 4 3 1
9520 16 4 3 1
9600 8 4 3 1
9610 8 4 3 1
9611 8 4 3 1
9612 8 4 3 1
9613 8 4 3 1
9620 8 4 3 1
9621 12 4 3 1
9622 8 4 3 1
9623 8 4 3 1
9624 8 4 3 1
9629 8 4 3 1
% END

% LABELLIST-isco68
% source: iscolab.sps from http://www.harryganzeboom.nl/isco68/
% variables: ICSO68 ISCO68-label-E
0100 'PHYSIC SCIENTIST'
0110 'CHEMIST'
0120 'PHYSICIST'
0130 'PHYS SCIENTIST'
0131 'GEOLOGIST'
0132 'ASTRONOMIST'
0133 'WEATHERMAN'
0139 'SCIENTIST'
0140 'PHYS SCIENCE TECHN'
0200 'ARCHTCTS,ENGINEERS'
0210 'ARCHITECTS'
0220 'CIVIL ENGINEERS'
0230 'ELECTRICAL ENGINEERS'
0240 'MECHANICAL ENGINEERS'
0250 'CHEMICAL ENGINEERS'
0260 'METALLURGIST'
0270 'MINING ENGINEER'
0280 'INDUSTRIAL ENGINEER'
0290 'ENGINEER NEC'
0300 'ENGIN TECHNICIANS'
0310 'SURVEYOR'
0320 'DRAFTSMEN'
0321 'TRACER'
0329 'DRAFTSMAN'
0330 'CIVIL ENG TECHNICIAN'
0339 'SURVEYOR ASSISTENT'
0340 'ELECTR ENGIN TECHN'
0350 'MECH ENGIN TECHCN'
0360 'CHEM ENGIN TECHCN'
0370 'METALLURG TECHCN'
0380 'MINING TECHNICIAN'
0390 'ENGIN TECHNCN NEC'
0409 'AIRCR SHIP OFFICRS'
0400 'PILOTS-SHIP CAPTAINS'
0410 'PILOTS ARW'
0411 'ASTRONAUT'
0419 'AIRCRAFT PILOT'
0420 'SHIP OFFCR ARW'
0421 'SMALL BOAT OFFICER'
0429 'SHIPS OFFICER'
0430 'SHIPS ENGINEER'
0500 'LIFE SCIENTIST'
0510 'BIOLOGIST'
0520 'BACTERIOLOGISTS'
0521 'DAIRY SCIENTIST'
0529 'MEDICAL RESEARCHER'
0530 'AGRONOMISTS'
0531 'AGRICULTURAL AGENT'
0539 'AGRONOMIST'
0540 'LIFE SC TECHNICIANS'
0541 'AGRICULT TECHNCN'
0549 'MEDICAL TECHNICIAN'
0600 'MEDICAL ARW WRKRS'
0610 'MEDICAL DOCTOR'
0611 'CHF PHYSIC IN HOSPITAL'
0619 'PHYSICIAN'
0620 'MEDICAL ASSISTANT'
0630 'DENTIST'
0640 'DENTAL ASSISTANT'
0650 'VETERINARIAN'
0660 'VETERINARY ASSISTANT'
0670 'PHARMACIST'
0680 'PHARM ASSISTANT'
0690 'DIETITIAN'
0700 'LOWER MEDCL PROFS'
0710 'PROF NURSES ARW'
0711 'HEADNURSE'
0715 'OTHER LOWER MEDCL PROFS'
0719 'PROFESSIONAL NURSE'
0720 'UNCERTIFIED NURSE'
0730 'PROFES MIDWIFE'
0740 'MIDWIFE PERSNL NEC'
0750 'OPTOMETRIST'
0751 'OPTICIAN'
0759 'OPTOMETRIST'
0760 'PHYSIOTHERAPISTS'
0761 'OCCUPATIONL THERAPST'
0762 'MASSEUR'
0769 'PHYSIOTHERAPISTS'
0770 'MEDCL X-RAY TECHNCN'
0780 'LOWER MEDCL PROFS'
0790 'MEDICAL WORKER NEC'
0791 'CHIROPRACTOR'
0792 'HERBALIST'
0793 'SANITARY OFFICER'
0799 'OSTEOPATH'
0800 'MATHEMATICNS ARW'
0810 'STATISTICIAN'
0820 'MATHEMATICIAN'
0830 'SYSTEM ANALYST'
0840 'STAT TECHNICN'
0849 'COMPUTER PROGRAMMER'
0900 'ECONOMIST'
1100 'ACCOUNTANT'
1101 'PROFESSNL ACCOUNTNT'
1109 'ACCOUNTANT'
1200 'JURIST'
1210 'LAWYERS'
1211 'PUBLIC PROSECUTOR'
1219 'TRIAL LAWYER'
1220 'JUDGES'
1221 'SUPREME COURT JUSTICE'
1222 'LOCAL COURT JUSTICE'
1229 'JUDGE'
1290 'JURIST NEC'
1291 'LEGL ADVISR WOUT DEGREE'
1299 'NON-TRIAL LAWYER'
1300 'TEACHER'
1304 'SECOND SCHOOL PRINCIPAL'
1310 'UNIVERS TEACHER'
1311 'UNIVERS PROFESSOR,DEAN'
1319 'UNIVERSITY TEACHER'
1320 'SECONDARY SCHOOL TEACHER'
1321 'MIDDLE SCHOOL TEACHER'
1329 'HIGH SCHOOL TEACHER'
1330 'PRIMARY TEACHER'
1340 'PREPRIMARY TEACHER'
1350 'SPEC EDUCYR TEACHER'
1390 'TEACHERS NEC'
1391 'PRIMARY PRINCIPAL'
1392 'EDUCYRATION OFFICER'
1393 'TEACHERS AIDE'
1394 'SEC SCHOOL PRINCIPAL'
1399 'VOCATIONAL TEACHER'
1400 'RELIGIOUS WRKRS'
1410 'RELI MINISTER ARW'
1411 'HIGH CHURCH OFFICER'
1412 'RELIGIOUS RECITER'
1413 'EVANGELIST'
1414 'MISSIONARY'
1415 'MEMB OF RELIGS ORDER'
1416 'ASSISTANT PRIEST'
1419 'CLERGYMAN'
1490 'RELI WORKERS'
1491 'FAITH HEALER'
1499 'RELIGIOUS TEACHER'
1500 'AUTHORS ARW'
1510 'AUTHORS'
1511 'PULPWRITER'
1519 'AUTHOR'
1590 'AUTHORS ARW'
1591 'NEWSPAPER EDITOR'
1592 'ADVERTISING WRITER'
1593 'PUBLIC RELATIONS MAN'
1599 'JOURNALIST'
1600 'CREATIVE ARTISTS'
1610 'ARTIST'
1620 'COMMERC ARTIST ARW'
1621 'DESIGNER'
1622 'WINDOW DISPLAY ARTST'
1629 'COMMERCIAL ARTIST'
1630 'PHOTOGRAPHER ARW'
1631 'TV CAMERAMAN'
1639 'PHOTOGRAPHER'
1700 'PERFORMING ARTISTS'
1710 'MUSICIANS ARW'
1711 'JAZZ MUSICIAN'
1712 'MUSICAL ENTERTAINER'
1713 'MUSIC TEACHER'
1719 'CLASSICAL MUSICIAN'
1720 'DANCERS ARW'
1721 'DANCING TEACHER'
1729 'DANCER'
1730 'ACTORS ARW'
1731 'STAR ACTOR'
1732 'DRAMATIC DIRECTOR'
1739 'ACTOR'
1740 'PRODUCER PERF ARTS'
1749 'DRAMATIC PRODUCER'
1750 'CIRCUS PERFORMER'
1790 'PERFORM ARTIST NEC'
1791 'ENTERTAINER'
1799 'RADIO TV ANNOUNCER'
1800 'SPORTSMEN ARW'
1801 'COACH-MANAGER'
1809 'PROFESSIONAL ATHLETES'
1900 'PROF WRKRS NEC'
1910 'LIBRARIAN'
1920 'SOCIOLOGIST ARW'
1921 'PSYCHOLOGIST'
1922 'ARCHEOLOGIST'
1923 'HISTORIAN'
1924 'SOCIAL SCIENTIST NEC'
1929 'SOCIOLOGIST'
1930 'SOCIAL WORKER ARW'
1931 'GROUP WRKR'
1939 'SOCIAL WRKR'
1940 'OCCUP SPECIALIST ARW'
1941 'JOB COUNSELOR'
1949 'PERSONNEL DIRECTOR'
1950 'PHILOLOGISTS'
1951 'PHILOLSTS,TRANSLATORS'
1959 'TRANSLATOR'
1960 'GENERIC PROFESSIONAL'
1990 'OTHER PROF WRKR'
1991 'DIVINER'
1992 'FINGERPRINT EXPERT'
1993 'EXPLORER'
1994 'PEACE CORPS MEMBER'
1995 'ADVERTISING EXECUTIVE'
1999 'TECHNICIAN'
2000 'GOVERNMNT ADMINISTRTORS'
2010 'GOV JURISDIC HEADS'
2011 'PROVINCIAL GOVERNOR'
2012 'DISTRICT HEAD'
2013 'HEAD LARGE CITY'
2014 'HEAD SMALL CITY'
2015 'VILLAGE HEAD'
2019 'CHIEF OF STATE'
2020 'LEGISLTV BODY MEMBER'
2021 'MEMBER UPPER HOUSE'
2022 'MEMBER LOWER HOUSE'
2023 'MEMBER PROVINCIAL HOUSE'
2024 'MEMBER LOCAL COUNCIL'
2029 'LEADER OF HOUSE'
2030 'HIGHER ADMINISTRATIVE OFF'
2031 'AMBASSADOR'
2032 'DIPLOMAT'
2033 'HIGH CIVIL SERVANT DEPT HEAD'
2034 'DEPT HEAD LOCAL GOVERNMENT'
2035 'DEPHEAD LOC GOVRNMT'
2036 'CHIEFS COUNSELOR'
2039 'GOVERNMENT MINISTER'
2100 'MANAGERS'
2110 'GENERAL MANAGER'
2111 'HEAD OF LARGE FIRM'
2112 'HEAD OF FIRM'
2113 'HEAD OF SMALL FIRM'
2114 'BANKER'
2115 'BANKER LARGE BANK'
2116 'BUILDING CONTRACTOR'
2119 'MEMBER BOARD OF DIRECTORS'
2120 'FACTORY MANAGER'
2190 'MANAGERS NEC'
2191 'BRANCH MANAGER'
2192 'DEPARTMENT MANAGER'
2193 'DEP MANAGER LARGE FRM'
2194 'BUSINESS EXECUTIVE'
2195 'POLITICN-PARTY OFFICL'
2196 'UNION OFFICIAL'
2197 'HIGH UNION OFFICIAL'
3000 'CLERICAL WORKERS'
2199 'BUSINESS MANAGER'
3009 'OFFICE MANAGER'
3100 'GOV EXEC OFFICL'
3101 'MINOR CIVIL SERVANT'
3102 'GOVERNMENT INSPECTOR'
3103 'COSTUMS INSPECTOR'
3104 'TAX COLLECTOR'
3109 'MIDDL RANGE CVL SRVNT'
3200 'TYPIST ARW'
3210 'SECRETARIES ARW'
3211 'SECRETARY'
3219 'TYPIST-STENOGRAPHER'
3220 'KEYPUNCH OPRTR'
3300 'BOOKKEEPER ARW'
3310 'BOOKKEEPERS ARW'
3311 'CASHIER'
3312 'HEAD CASHIER'
3313 'BANK TELLER'
3314 'POST OFFICE CLERK'
3315 'TICKET SELLER'
3319 'BOOKKEEPER'
3390 'BOOKKEEPERS ARW'
3391 'BILL COLLECTOR'
3399 'FINANCIAL CLERK'
3400 'COMP MACH OPRTR'
3410 'BKKEEPNG MACH OPRTR'
3420 'COMPUTOR OPRTR'
3500 'TRANSPORT SUPERVISOR'
3510 'RAILW STATNMSTR'
3520 'POSTMASTER'
3590 'DISPATCHER-EXPEDITOR'
3600 'TRANSPORT CONDUCTORS'
3601 'BUS-STREET CONDUCTOR'
3602 'SLEEPING CAR PORTER'
3609 'RAILROAD CONDUCTOR'
3700 'MAIL DIST CLERKS'
3701 'OFFICE BOY-MESSENGER'
3709 'MAIL CARRIER'
3800 'TELE OPERATOR'
3801 'TELEGRAPH OPRTR'
3802 'RADIO OPRTR'
3809 'TELEPHONE OPRTR'
3900 'CLERKS ARW'
3910 'STOCK CLERKS'
3911 'SHIPPING CLERK'
3919 'STOCKROOM ATTENDANT'
3920 'PLANNING CLERKS'
3930 'CLERKS'
3931 'GOVERNMENT OFFICE CLERK'
3932 'LAW CLERK'
3939 'OFFICE CLERK'
3940 'RECEPTIONIST ARW'
3941 'TRANSPORTATION AGENT'
3942 'RAILWAY BAGGAGEMEN'
3943 'TRAVEL AGENT'
3944 'FLOOR WALKER'
3949 'RECEPTIONIST'
3950 'LIBRARY CLERKS ARW'
3951 'FILING CLERK'
3959 'LIBRARY ASSISTANT'
3990 'CLERKS NEC'
3992 'POLITICAL PARTY WRKR'
3993 'METER READER'
3999 'PROOFREADER'
4000 'WHOLE-RETAIL MANAGR'
4001 'SERVICE STATION MANAGER'
4002 'CREDIT MANAGER'
4009 'RETAIL MANAGER'
4100 'WHOLE-RETAIL OWNERS'
4101 'LARGE SHOP OWNER'
4102 'ONE-MAN STAND OPRTR'
4103 'AUTOMOBILE DEALER'
4104 'BROKER'
4105 'LIVESTOCK BROKER'
4106 'WHOLESALE DISTRIBUTR'
4107 'SMUGGLER'
4108 'LABOR CONTRACTORS'
4109 'SHOP KEEPER'
4200 'SALES SUPERVISOR'
4210 'SALES MANAGER'
4220 'BUYER'
4221 'PURCHASING AGENT'
4222 'AGRICULTURAL BUYER'
4229 'BUYER'
4300 'TECNH SALESMEN ARW'
4310 'TECHN SALESMEN'
4311 'UTILITY CO SALESMEN'
4319 'SALES ENGINEER'
4320 'TRAVELING SALESMEN'
4400 'INSURNC SALESMN ARW'
4410 'INSURE ARW SALESMN'
4411 'REAL ESTATE AGENT'
4412 'STOCK BROKER'
4419 'INSURANCE AGENT'
4420 'ADVERTISING SALESMAN'
4430 'AUCTIONEERS'
4431 'APPRAISER'
4432 'INSURNCE CLMS INVESTIGATOR'
4439 'AUCTIONEER'
4500 'SALESMEN'
4510 'SALESMEN'
4511 'AUTOMOBILE SALESMEN'
4512 'GAS STATION ATTENDNT'
4513 'MODEL'
4514 'SALES DEMONSTRATOR'
4519 'SALES CLERK'
4520 'VENDORS'
4521 'STREET VENDOR-PEDDLR'
4522 'TELEPHONE SOLICITOR'
4523 'NEWSPAPER SELLER'
4524 'ROUTEMAN'
4525 'NARCOTICS PEDDLR'
4529 'MARKET VENDOR'
4900 'SALESMEN NEC'
5000 'MANAGERS CATER&LODGE'
5001 'HOTEL MAMAGER'
5002 'APARTMENT MANAGER'
5009 'BAR MANAGER'
5100 'CATER-LOG OWNERS'
5101 'LUNCHROOM-CFFSHP OPRTR'
5102 'HOTEL OPRTR'
5103 'BOARDINGHOUSE KEEPER'
5104 'PUB KEEPER'
5109 'RESTAURANT OWNER'
5200 'HOUSEKEEPING SUPERVISORS'
5201 'HOUSEKEEPER'
5209 'STEWARDS'
5300 'COOKS ARW'
5310 'COOKS ARW'
5311 'MASTER COOK'
5312 'COOKS HELPER'
5319 'COOK'
5320 'WAITERS ARW'
5321 'BARTENDER'
5322 'SODA FOUNTAIN CLERK'
5329 'WAITER'
5400 'MAID ARW'
5401 'NURSEMAID'
5402 'HOTEL CHAMBERMAID'
5403 'HOTEL CONCIERGE'
5409 'SERVANT'
5500 'CARETAKERS ARW'
5510 'BUILD CARETAKER'
5511 'CONCRGE APPRTMNT HSE'
5512 'SEXTON'
5519 'JANITOR'
5520 'CHARWRKR ARW'
5521 'WINDOW WASHER'
5522 'CHIMNEY SWEEP'
5529 'CHARWRKR'
5600 'LAUNDERER'
5700 'BARBER ARW'
5701 'MASTER BARBER'
5702 'BEAUTICIAN'
5703 'OPERTR HAIRDRSS SALON'
5709 'BARBER'
5800 'PROTCTV SERVC WRKR'
5810 'FIREMAN'
5820 'POLICE ARW'
5821 'POLICE OFFICER'
5822 'HIGH POLICE OFFICIAL'
5823 'SPECIALIZED LAW OFFICER'
5829 'POLICEMAN'
5830 'ARMED FORCES'
5831 'ARMY OFFICERS'
5832 'NON COMMISND OFFICERS'
5833 'SOLDIER'
5890 'PROT SERV WRKRS NEC'
5891 'PRISON GUARD'
5892 'BAILIFF'
5899 'WATCHMAN'
5900 'SERVICE WRKRS NEC'
5910 'MUSEUM ATTENDANT'
5920 'UNDERTAKER'
5990 'OTHR SERVC WRKRS'
5991 'ENTERTAINMENT ATTENDANT'
5992 'ELEVATOR OPRTR'
5993 'HOTEL BELL BOY'
5994 'DOORKEEPER'
5995 'SHOE SHINER'
5996 'AIRLINE STEWARDESS'
5997 'BOOKMAKER'
5998 'BELL CAPTAIN IN HOTEL'
5999 'MEDICAL ATTENDANT'
6000 'FARM MANAGERS'
6001 'FARM FOREMAN'
6009 'FARM MANAGER'
6100 'FARMERS'
6110 'FARMERS'
6111 'LARGE FARMER'
6112 'SMALL FARMER'
6113 'TENANT FARMER'
6114 'SHARE CROPPER'
6115 'COLLECTIVE FARMER'
6116 'SETTLER'
6117 'UNPD FAM FARM WRKR'
6119 'FARMER'
6120 'SPECIALIZED FARMER'
6200 'AGRIC WRKR'
6210 'GENERAL FARM WRKR'
6211 'MIGRANT WRKR'
6219 'FARM HAND'
6220 'FIELD CROP WRKR'
6230 'ORCHARD-VINYAR WRK'
6239 'PALMWINE HARVESTER'
6240 'LIVESTOCK WRKR'
6250 'MILKER'
6260 'POULTRY FARM WRKR'
6270 'GARDENER'
6280 'TRACTOR DRIVER'
6290 'AGRICULT WORKER'
6291 'GATHERER'
6299 'SKILLED FARM WRKR'
6300 'FORESTRY WRKRS'
6310 'LOGGERS'
6311 'WHISTLE PUNK'
6319 'LOGGER'
6320 'FOREST WRKRS'
6321 'TIMBER CRUISER'
6322 'TREE SURGEON'
6329 'FORESTER'
6400 'FISHERMEN ARW'
6410 'FISHERMEN'
6411 'FISHERMAN,OWN BOAT'
6419 'FISHERMAN'
6490 'FISHERMEN ARW'
6491 'HUNTER'
6499 'WHALER'
7000 'PROD SUPERVSR'
7001 'SUPERVISOR'
7009 'FOREMAN'
7100 'MINER ARW'
7110 'MINERS QUARRYMEN'
7111 'SPECLZD MINE WRKR'
7112 'QUARRY WRKR'
7113 'INSTRUCTOR IN MINE'
7119 'MINER'
7120 'MINERAL-STONE TREATERS'
7130 'WELL DRILLERS ARW'
7139 'OIL FIELD WORKERS'
7200 'METAL PROCESSORS'
7210 'STEEL MILL WRKR'
7220 'ROLLING MILL OPRTR'
7230 'METAL MELTERS-REHEATERS'
7240 'METAL CASTER'
7250 'METAL MOULDER-COREMAKER'
7260 'METAL ANNEALER ARW'
7270 'METAL DRAWER-EXTRUDER'
7280 'GALVANIZER'
7290 'METAL PROCESSER NEC'
7300 'WOOD PREP WRKR'
7310 'WOOD TREATERS'
7320 'SAYWER ARW'
7321 'LUMBER GRADER'
7329 'SAWYER IN SAW MILL'
7330 'PAPER PULP PREPARER'
7340 'PAPER MAKER'
7400 'CHEM PROCESS ARW'
7410 'CRUSHER,GRINDER,MIXR'
7420 'COOKER,ROASTER ARW'
7430 'FILTER-SEPARATOR OPRTRS'
7440 'STILL & REACTOR OPRTRS'
7450 'PETROLEUM WRKR'
7490 'CHEM PROC ARW NEC'
7491 'CHARCOAL BURNER'
7499 'CHEMICAL WRKR'
7500 'SPINNER ARW'
7510 'FIBER PREPARER'
7520 'SPINNER'
7530 'WEAV MACH SETTER ARW'
7540 'WEAVERS ARW'
7541 'CLOTH GRADER'
7549 'WEAVER'
7550 'KNITTING MACHINE OPRTR'
7560 'CLOTH DYER'
7590 'TEXTILE MILL WRKR'
7600 'TANNERS ARW'
7610 'TANNER-FELLMONGER'
7620 'PELT DRESSER'
7700 'FOOD BEVERG WRKR'
7710 'GRAIN MILLER ARW'
7711 'GRAIN MILL OWNER OPRTR'
7719 'GRAIN MILLER'
7720 'SUGAR BOILER'
7730 'BUTCHER'
7731 'PACKING HOUSE BUTCHER'
7732 'MASTER BUTCHER'
7739 'BUTCHER'
7740 'CANNERY WRKR'
7750 'DAIRY PRODUCT WRKRS'
7760 'BAKERS-CONFECTIONERS'
7761 'MASTER BAKER'
7769 'BAKERS'
7770 'TEA-COFFEE-COCOA PREPRR'
7780 'BEVERAGE MAKER'
7790 'FOOD-BEV NEC'
7799 'FISH BUTCHER'
7800 'TOBACC WRK'
7810 'TOBACCO PREPARER'
7820 'CIGAR MAKER'
7830 'CIGARETTE MAKER'
7890 'TOBACCO NEC'
7899 'TOBACCO FACTORY WRKR'
7900 'TAILOR ARW'
7910 'TAILORS DRESSMAKERS'
7911 'CUSTOM SEAMSTRESS'
7919 'TAILOR'
7920 'FUR COAT TAILOR'
7930 'MILLINER'
7940 'GARMENT CUTTER'
7950 'SEWING MACHINE OPRTR'
7960 'UPHOLSTERER'
7990 'TAILORS ARW NEC'
8000 'SHOEMAKER ARW'
8010 'SHOEMAKER,REPAIRER'
8020 'SHOECUTTER ARW'
8030 'LEATHER WRKR'
8100 'CABINETMAKER ARW'
8110 'CABINETMAKER'
8120 'WOODWORKING MACHINE OPRTR'
8190 'WOODWRKRS NEC'
8191 'WOOD VEHICLE BUILDER'
8199 'COOPER'
8200 'TOMBSTONE CARVER'
8300 'SMITH,TOOLMAKER ARW'
8310 'SMITHS'
8311 'FORGING PRESS OPRTR'
8319 'BLACKSMITH'
8320 'TOOLMAKERS ARW'
8321 'METAL PATTERNMAKER'
8329 'TOOL-DIE MAKER'
8330 'MACH TOOL SETTER'
8331 'TURNER'
8339 'MACHINE SETUP MAN'
8340 'MACHINE OPRTR IN FACTORY'
8350 'METAL GRINDERS ARW'
8351 'POLISHNG MACH OPRTR'
8359 'SAW SHARPENER'
8390 'LOCKSMITH'
8400 'MACHINE FITTR ARW'
8410 'MACHINE ASSEMBL ARW'
8411 'AIRCRAFT WRKR'
8412 'MILLWRIGHT'
8419 'MACHINIST-FITTER'
8420 'PREC INSTR MAKERS'
8421 'FINE FITTER'
8422 'DENTAL MECHANIC'
8429 'WATCH MAKER-REPAIRMN'
8430 'MOTOR VEH MECHANCS'
8431 'GARAGE OPRTR'
8439 'GARAGE MECHANIC'
8440 'AIRPLANE MECHANIC'
8490 'MACH FITTERS NEC'
8491 'BICYCLE REPAIRMAN'
8492 'MECHANICS HELPER'
8493 'ASSEMBLY LINE WRKR'
8494 'UNSKILLED GARAGE WRKR'
8499 'MECHANIC,REPAIRMAN'
8500 'ELCTR FITTER ARW'
8510 'ELECTRICAL FITTER'
8520 'ELECTRONICS FITTER'
8530 'ELCTRONIC ASSEMBLER'
8540 'RADIO-TV REPAIRMAN'
8550 'ELEC WIREMEN'
8551 'MASTER-ELCTRCN-OWN SHOP'
8559 'ELECTRICIANS'
8560 'TELPHONE INSTALLER'
8570 'POWER LINEMAN'
8590 'ELECTR WRKR NEC'
8600 'BROADCSTNG STATN OPRT ARW'
8610 'BROADCSTNG STATN OPRTR'
8620 'MOTION PICT PROJECTNST'
8700 'PLUMBER ARW'
8710 'PLUMBERS TEC'
8711 'MASTER PLUMBR-OWN SHP'
8719 'PLUMBER'
8720 'WELDER'
8730 'SHEET METAL WRKR'
8731 'COPPER-TIN SMITH'
8732 'BOILERMAKER'
8733 'VEHICLE BODY BUILDER'
8739 'SHEETMETAL WRKR'
8740 'STRUCTURAL STEEL WRKR'
8800 'JEWELRY ARW'
8801 'MASTER JEWELER,GOLDSMTH'
8809 'JEWELER,GOLDSMITH'
8900 'GLASS FORMER ARW'
8910 'GLASS FORMERS ARW'
8911 'GLASS BOWLER'
8919 'LENS GRINDER'
8920 'POTTER'
8930 'GLASS-KERAMIC KILNMN'
8940 'GLASS ENGRAVER-ETCHER'
8950 'GLASS-CERAMICS PAINTERS ARW'
8990 'GLASS FORMERS ARW NEC'
9000 'RUBBER-PLASTICS MAKER'
9010 'RUBBR-PLASTC PR MAKRS'
9020 'TIRE MAKERS-VULCANIZERS'
9100 'PAPER-PAPRBRD PR MAKRS'
9200 'PRINTER ARW'
9210 'COMPOSITORS ARW'
9211 'MASTER PRINTER'
9219 'PRINTERS'
9220 'PRINTING PRESSMAN'
9230 'STEREOTYPRS-ELCTRTYPRS'
9240 'METAL ENGRAVER'
9250 'PHOTOENGRAVER'
9260 'BOOKBINDER'
9270 'PHOTOGRAPH DEVELOPER'
9290 'PRINTERS NEC'
9300 'PAINTER ARW'
9310 'CONSTR PAINTERS'
9311 'MASTER BUILDING PAINTER'
9319 'BUILDING PAINTER'
9390 'PAINTERS NEC'
9400 'PRODUCTN WORKER ARW'
9410 'PIANO TUNER'
9420 'BASKET WEAVER'
9430 'NONMET MINERL PR MAKR'
9490 'OTH PROC ARW'
9491 'IVORY CARVER'
9492 'TAXIDERMIST'
9493 'CALABASH MAKER'
9499 'QUALITY CHECKER'
9500 'CONSTRCTN WRKR ARW'
9510 'MASON'
9520 'CEMENT FINISHER'
9530 'ROOFER'
9540 'CARPENTERS-JOINERS'
9541 'MASTER CARPENTER'
9542 'CARPENTERS NEC'
9549 'CARPENTER'
9550 'PLASTERERS'
9551 'MASTER PLASTERER'
9559 'PLASTERER'
9560 'INSULATION INSTALLER'
9570 'GLAZIER'
9590 'CONSTRUCTION WRK NEC'
9591 'MASTER PAPERHANGER'
9592 'MAINTENANCE MAN'
9593 'SKILLED CONSR WRKR'
9594 'CONSTRCTN WRKR NEC'
9595 'UNSKLLD CONSTRCT WRKR'
9596 'HOUSE BUILDER'
9599 'PAPERHANGER'
9600 'STAT EQUIPM OPRTR ARW'
9610 'POWER STATION OPRTR'
9690 'STATIONARY ENGINEER'
9700 'MAT-HANDLNG EQPM OPRTR ARW'
9710 'DOCKERS'
9711 'WAREHOUSE HAND'
9712 'PORTER'
9713 'RAILWAY-AIRPORT PORTER'
9714 'PACKER'
9719 'LONGSHOREMAN'
9720 'RIGGER-CABLE SPLICER'
9730 'CRANE-HOIST OPERATR'
9731 'DRAWBRIDGE TENDER'
9739 'POWER CRANE OPRTR'
9740 'ROAD MACHINERY OPRTR'
9790 'MATER-HANDLNG EQUIPM OPRTR'
9800 'TRANSP EQUIPM OPRTR ARW'
9810 'SHIP DECKS RATINGS'
9811 'BOATMEN'
9819 'SEAMAN'
9820 'SHIP ENGINE-ROOM HAND'
9830 'RAILW ENGN DRIVER'
9831 'LOCOMOTIVE FIREMEN'
9832 'ORETRAIN MOTERMAN IN MINE'
9839 'LOCOMOTIVE ENGINEER'
9840 'RAILW SWITCHMN,BRAKEMN'
9850 'MOT VEH DRIVER'
9851 'BUS-TRAM DRIVER'
9852 'TRUCK DRIVER'
9853 'SMALL TRANSPORT OPRTR'
9854 'TRUCK DRIVERS HELPER'
9855 'DRIVING TEACHER'
9859 'DRIVER'
9860 'ANIMAL DRIVER'
9861 'WAGONEER'
9869 'ANIMAL DRIVER'
9890 'TRANSP EQ OPER NEC'
9891 'RAILW CROSSNG GUARD'
9899 'PEDAL-VEHICLE DRIVER'
9900 'MANUAL WORKERS NEC'
9950 'SKLD MANUAL WRKR NEC'
9951 'INDEPENDENT ARTISAN'
9959 'SKILLED MANUAL WRKR'
9970 'SEMSK WRKR NEC'
9971 'APPRENTICE'
9979 'SEMISK FACTORY WRKR'
9990 'LABORER NEC'
9991 'UNSKILLED FACTORY WRKR'
9992 'CONTRACT LABORER'
9993 'ITINERANT WRKR'
9994 'RAILWAY TRACK WORKER'
9995 'STREET SWEEPER'
9996 'GARBAGE COLLECTOR'
9997 'ROAD CONSTRUCTION WRKR'
9999 'LABORER'
10000 'ARMED FORCES'
10001 'ARMY OFFICERS'
10002 'NON COMMISND OFFICERS'
10003 'SOLDIER'
10009 'HIGH ARMY OFFICERS'
% 12000 'UNCLASSIFIABLE OCCUPATION'
% END

% LABELLIST-isco88
% source: https://www.ilo.org/public/english/bureau/stat/isco/isco88/major.htm
% note: uppercase spelling of major, submajor, and minor groups changed to
%       lowercase
% variables: ICSO88 ICSO88-label-E
1000 "Legislators, senior officials and managers"
1100 "Legislators and senior officials"
1110 "Legislators"
1120 "Senior government officials"
1130 "Traditional chiefs and heads of villages"
1140 "Senior officials of special-interest organisations"
1141 "Senior officials of political-party organisations"
1142 "Senior officials of employers', workers' and other economic-interest organisations"
1143 "Senior officials of humanitarian and other special-interest organisations"
1200 "Corporate managers"
1210 "Directors and chief executives"
1220 "Production and operations department managers"
1221 "Production and operations department managers in agriculture, hunting, forestry and fishing"
1222 "Production and operations department managers in manufacturing"
1223 "Production and operations department managers in construction"
1224 "Production and operations department managers in wholesale and retail trade"
1225 "Production and operations department managers in restaurants and hotels"
1226 "Production and operations department managers in transport, storage and communications"
1227 "Production and operations department managers in business services"
1228 "Production and operations department managers in personal care, cleaning and related services"
1229 "Production and operations department managers not elsewhere classified"
1230 "Other department managers"
1231 "Finance and administration department managers"
1232 "Personnel and industrial relations department managers"
1233 "Sales and marketing department managers"
1234 "Advertising and public relations department managers"
1235 "Supply and distribution department managers"
1236 "Computing services department managers"
1237 "Research and development department managers"
1239 "Other department managers not elsewhere classified"
1300 "General managers"
1310 "General managers"
1311 "General managers in agriculture, hunting, forestry/ and fishing"
1312 "General managers in manufacturing"
1313 "General managers in construction"
1314 "General managers in wholesale and retail trade"
1315 "General managers of restaurants and hotels"
1316 "General managers in transport, storage and communications"
1317 "General managers of business services"
1318 "General managers in personal care, cleaning and related services"
1319 "General managers not elsewhere classified"
2000 "Professionals"
2100 "Physical, mathematical and engineering science professionals"
2110 "Physicists, chemists and related professionals"
2111 "Physicists and astronomers"
2112 "Meteorologists"
2113 "Chemists"
2114 "Geologists and geophysicists"
2120 "Mathematicians, statisticians and related professionals"
2121 "Mathematicians and related professionals"
2122 "Statisticians"
2130 "Computing professionals"
2131 "Computer systems designers and analysts"
2132 "Computer programmers"
2139 "Computing professionals not elsewhere classified"
2140 "Architects, engineers and related professionals"
2141 "Architects, town and traffic planners"
2142 "Civil engineers"
2143 "Electrical engineers"
2144 "Electronics and telecommunications engineers"
2145 "Mechanical engineers"
2146 "Chemical engineers"
2147 "Mining engineers, metallurgists and related professionals"
2148 "Cartographers and surveyors"
2149 "Architects, engineers and related professionals not elsewhere classified"
2200 "Life science and health professionals"
2210 "Life science professionals"
2211 "Biologists, botanists, zoologists and related professionals"
2212 "Pharmacologists, pathologists and related professionals"
2213 "Agronomists and related professionals"
2220 "Health professionals (except nursing)"
2221 "Medical doctors"
2222 "Dentists"
2223 "Veterinarians"
2224 "Pharmacists"
2229 "Health professionals (except nursing) not elsewhere classified"
2230 "Nursing and midwifery professionals"
2300 "Teaching professionals"
2310 "College, university and higher education teaching professionals"
2320 "Secondary education teaching professionals"
2330 "Primary and pre-primary education teaching professionals"
2331 "Primary education teaching professionals"
2332 "Pre-primary education teaching professionals"
2340 "Special education teaching professionals"
2350 "Other teaching professionals"
2351 "Education methods specialists"
2352 "School inspectors"
2359 "Other teaching professionals not elsewhere classified"
2400 "Other professionals"
2410 "Business professionals"
2411 "Accountants"
2412 "Personnel and careers professionals"
2419 "Business professionals not elsewhere classified"
2420 "Legal professionals"
2421 "Lawyers"
2422 "Judges"
2429 "Legal professionals not elsewhere classified"
2430 "Archivists, librarians and related information professionals"
2431 "Archivists and curators"
2432 "Librarians and related information professionals"
2440 "Social science and related professionals"
2441 "Economists"
2442 "Sociologists, anthropologists and related professionals"
2443 "Philosophers, historians and political scientists"
2444 "Philologists, translators and interpreters"
2445 "Psychologists"
2446 "Social work professionals"
2450 "Writers and creative or performing artists"
2451 "Authors, journalists and other writers"
2452 "Sculptors, painters and related artists"
2453 "Composers, musicians and singers"
2454 "Choreographers and dancers"
2455 "Film, stage and related actors and directors"
2460 "Religious professionals"
3000 "Technicians and associate professionals"
3100 "Physical and engineering science associate professionals"
3110 "Physical and engineering science technicians"
3111 "Chemical and physical science technicians"
3112 "Civil engineering technicians"
3113 "Electrical engineering technicians"
3114 "Electronics and telecommunications engineering technicians"
3115 "Mechanical engineering technicians"
3116 "Chemical engineering technicians"
3117 "Mining and metallurgical technicians"
3118 "Draughtspersons"
3119 "Physical and engineering science technicians not elsewhere classified"
3120 "Computer associate professionals"
3121 "Computer assistants"
3122 "Computer equipment operators"
3123 "Industrial robot controllers"
3130 "Optical and electronic equipment operators"
3131 "Photographers and image and sound recording equipment operators"
3132 "Broadcasting and telecommunications equipment operators"
3133 "Medical equipment operators"
3139 "Optical and electronic equipment operators not elsewhere classified"
3140 "Ship and aircraft controllers and technicians"
3141 "Ships' engineers"
3142 "Ships' deck officers and pilots"
3143 "Aircraft pilots and related associate professionals"
3144 "Air traffic controllers"
3145 "Air traffic safety technicians"
3150 "Safety and quality inspectors"
3151 "Building and fire inspectors"
3152 "Safety, health and quality inspectors"
3200 "Life science and health associate professionals"
3210 "Life science technicians and related associate professionals"
3211 "Life science technicians"
3212 "Agronomy and forestry technicians"
3213 "Farming and forestry advisers"
3220 "Modern health associate professionals (except nursing)"
3221 "Medical assistants"
3222 "Sanitarians"
3223 "Dieticians and nutritionists"
3224 "Optometrists and opticians"
3225 "Dental assistants"
3226 "Physiotherapists and related associate professionals"
3227 "Veterinary assistants"
3228 "Pharmaceutical assistants"
3229 "Modern health associate professionals (except nursing) not elsewhere classified"
3230 "Nursing and midwifery associate professionals"
3231 "Nursing associate professionals"
3232 "Midwifery associate professionals"
3240 "Traditional medicine practitioners and faith healers"
3241 "Traditional medicine practitioners"
3242 "Faith healers"
3300 "Teaching associate professionals"
3310 "Primary education teaching associate professionals"
3320 "Pre-primary education teaching associate professionals"
3330 "Special education teaching associate professionals"
3340 "Other teaching associate professionals"
3400 "Other associate professionals"
3410 "Finance and sales associate professionals"
3411 "Securities and finance dealers and brokers"
3412 "Insurance representatives"
3413 "Estate agents"
3414 "Travel consultants and organisers"
3415 "Technical and commercial sales representatives"
3416 "Buyers"
3417 "Appraisers, valuers and auctioneers"
3419 "Finance and sales associate professionals not elsewhere classified"
3420 "Business services agents and trade brokers"
3421 "Trade brokers"
3422 "Clearing and forwarding agents"
3423 "Employment agents and labour contractors"
3429 "Business services agents and trade brokers not elsewhere classified"
3430 "Administrative associate professionals"
3431 "Administrative secretaries and related associate professionals"
3432 "Legal and related business associate professionals"
3433 "Bookkeepers"
3434 "Statistical, mathematical and related associate professionals"
3439 "Administrative associate professionals not elsewhere classified"
3440 "Customs, tax and related government associate professionals"
3441 "Customs and border inspectors"
3442 "Government tax and excise officials"
3443 "Government social benefits officials"
3444 "Government licensing officials"
3449 "Customs, tax and related government associate professionals not elsewhere classified"
3450 "Police inspectors and detectives"
3460 "Social work associate professionals"
3470 "Artistic, entertainment and sports associate professionals"
3471 "Decorators and commercial designers"
3472 "Radio, television and other announcers"
3473 "Street, night-club and related musicians, singers and dancers"
3474 "Clowns, magicians, acrobats and related associate professionals"
3475 "Athletes, sportspersons and related associate professionals"
3480 "Religious associate professionals"
4000 "Clerks"
4100 "Office clerks"
4110 "Secretaries and keyboard-operating clerks"
4111 "Stenographers and typists"
4112 "Word-processor and related operators"
4113 "Data entry operators"
4114 "Calculating-machine operators"
4115 "Secretaries"
4120 "Numerical clerks"
4121 "Accounting and bookkeeping clerks"
4122 "Statistical and finance clerks"
4130 "Material-recording and transport clerks"
4131 "Stock clerks"
4132 "Production clerks"
4133 "Transport clerks"
4140 "Library, mail and related clerks"
4141 "Library and filing clerks"
4142 "Mail carriers and sorting clerks"
4143 "Coding, proof-reading and related clerks"
4144 "Scribes and related workers"
4190 "Other office clerks"
4200 "Customer services clerks"
4210 "Cashiers, tellers and related clerks"
4211 "Cashiers and ticket clerks"
4212 "Tellers and other counter clerks"
4213 "Bookmakers and croupiers"
4214 "Pawnbrokers and money-lenders"
4215 "Debt-collectors and related workers"
4220 "Client information clerks"
4221 "Travel agency and related clerks"
4222 "Receptionists and information clerks"
4223 "Telephone switchboard operators"
5000 "Service workers and shop and market sales workers"
5100 "Personal and protective services workers"
5110 "Travel attendants and related workers"
5111 "Travel attendants and travel stewards"
5112 "Transport conductors"
5113 "Travel guides"
5120 "Housekeeping and restaurant services workers"
5121 "Housekeepers and related workers"
5122 "Cooks"
5123 "Waiters, waitresses and bartenders"
5130 "Personal care and related workers"
5131 "Child-care workers"
5132 "Institution-based personal care workers"
5133 "Home-based personal care workers"
5139 "Personal care and related workers not elsewhere classified"
5140 "Other personal services workers"
5141 "Hairdressers, barbers, beauticians and related workers"
5142 "Companions and valets"
5143 "Undertakers and embalmers"
5149 "Other personal services workers not elsewhere classified"
5150 "Astrologers, fortune-tellers and related workers"
5151 "Astrologers and related workers"
5152 "Fortune-tellers, palmists and related workers"
5160 "Protective services workers"
5161 "Fire-fighters"
5162 "Police officers"
5163 "Prison guards"
5169 "Protective services workers not elsewhere classified"
5200 "Models, salespersons and demonstrators"
5210 "Fashion and other models"
5220 "Shop salespersons and demonstrators"
5230 "Stall and market salespersons"
6000 "Skilled agricultural and fishery workers"
6100 "Market-oriented skilled agricultural and fishery workers"
6110 "Market gardeners and crop growers"
6111 "Field crop and vegetable growers"
6112 "Tree and shrub crop growers"
6113 "Gardeners, horticultural and nursery growers"
6114 "Mixed-crop growers"
6120 "Market-oriented animal producers and related workers"
6121 "Dairy and livestock producers"
6122 "Poultry producers"
6123 "Apiarists and sericulturists"
6124 "Mixed-animal producers"
6129 "Market-oriented animal producers and related workers not elsewhere classified"
6130 "Market-oriented crop and animal producers"
6140 "Forestry and related workers"
6141 "Forestry workers and loggers"
6142 "Charcoal burners and related workers"
6150 "Fishery workers, hunters and trappers"
6151 "Aquatic-life cultivation workers"
6152 "Inland and coastal waters fishery workers"
6153 "Deep-sea fishery workers"
6154 "Hunters and trappers"
6200 "Subsistence agricultural and fishery workers"
6210 "Subsistence agricultural and fishery workers"
7000 "Craft and related trades workers"
7100 "Extraction and building trades workers"
7110 "Miners, shotfirers, stone cutters and carvers"
7111 "Miners and quarry workers"
7112 "Shotfirers and blasters"
7113 "Stone splitters, cutters and carvers"
7120 "Building frame and related trades workers"
7121 "Builders, traditional materials"
7122 "Bricklayers and stonemasons"
7123 "Concrete placers, concrete finishers and related workers"
7124 "Carpenters and joiners"
7129 "Building frame and related trades workers not elsewhere classified"
7130 "Building finishers and related trades workers"
7131 "Roofers"
7132 "Floor layers and tile setters"
7133 "Plasterers"
7134 "Insulation workers"
7135 "Glaziers"
7136 "Plumbers and pipe fitters"
7137 "Building and related electricians"
7140 "Painters, building structure cleaners and related trades workers"
7141 "Painters and related workers"
7142 "Varnishers and related painters"
7143 "Building structure cleaners"
7200 "Metal, machinery and related trades workers"
7210 "Metal moulders, welders, sheet-metal workers, structural- metal preparers, and related trades workers"
7211 "Metal moulders and coremakers"
7212 "Welders and flamecutters"
7213 "Sheet metal workers"
7214 "Structural-metal preparers and erectors"
7215 "Riggers and cable splicers"
7216 "Underwater workers"
7220 "Blacksmiths, tool-makers and related trades workers"
7221 "Blacksmiths, hammer-smiths and forging-press workers"
7222 "Tool-makers and related workers"
7223 "Machine-tool setters and setter-operators"
7224 "Metal wheel-grinders, polishers and tool sharpeners"
7230 "Machinery mechanics and fitters"
7231 "Motor vehicle mechanics and fitters"
7232 "Aircraft engine mechanics and fitters"
7233 "Agricultural- or industrial-machinery mechanics and fitters"
7240 "Electrical and electronic equipment mechanics and fitters"
7241 "Electrical mechanics and fitters"
7242 "Electronics fitters"
7243 "Electronics mechanics and servicers"
7244 "Telegraph and telephone installers and servicers"
7245 "Electrical line installers, repairers and cable jointers"
7300 "Precision, handicraft, printing and related trades workers"
7310 "Precision workers in metal and related materials"
7311 "Precision-instrument makers and repairers"
7312 "Musical instrument makers and tuners"
7313 "Jewellery and precious-metal workers"
7320 "Potters, glass-makers and related trades workers"
7321 "Abrasive wheel formers, potters and related workers"
7322 "Glass makers, cutters, grinders and finishers"
7323 "Glass engravers and etchers"
7324 "Glass, ceramics and related decorative painters"
7330 "Handicraft workers in wood,textile, leather and related materials"
7331 "Handicraft workers in wood and related materials"
7332 "Handicraft workers in textile, leather and related materials"
7340 "Printing and related trades workers"
7341 "Compositors, typesetters and related workers"
7342 "Stereotypers and electrotypers"
7343 "Printing engravers and etchers"
7344 "Photographic and related workers"
7345 "Bookbinders and related workers"
7346 "Silk-screen, block and textile printers"
7400 "Other craft and related trades workers"
7410 "Food processing and related trades workers"
7411 "Butchers, fishmongers and related food preparers"
7412 "Bakers, pastry-cooks and confectionery makers"
7413 "Dairy-products makers"
7414 "Fruit, vegetable and related preservers"
7415 "Food and beverage tasters and graders"
7416 "Tobacco preparers and tobacco products makers"
7420 "Wood treaters, cabinet-makers and related trades workers"
7421 "Wood treaters"
7422 "Cabinet makers and related workers"
7423 "Woodworking machine setters and setter-operators"
7424 "Basketry weavers, brush makers and related workers"
7430 "Textile, garment and related trades workers"
7431 "Fibre preparers"
7432 "Weavers, knitters and related workers"
7433 "Tailors, dressmakers and hatters"
7434 "Furriers and related workers"
7435 "Textile, leather and related pattern-makers and cutters"
7436 "Sewers, embroiderers and related workers"
7437 "Upholsterers and related workers"
7440 "Pelt, leather and shoemaking trades workers"
7441 "Pelt dressers, tanners and fellmongers"
7442 "Shoe-makers and related workers"
8000 "Plant and machine operators and assemblers"
8100 "Stationary-plant and related operators"
8110 "Mining- and mineral-processing-plant operators"
8111 "Mining-plant operators"
8112 "Mineral-ore- and stone-processing-plant operators"
8113 "Well drillers and borers and related workers"
8120 "Metal-processing-plant operators"
8121 "Ore and metal furnace operators"
8122 "Metal melters, casters and rolling-mill operators"
8123 "Metal-heat-treating-plant operators"
8124 "Metal drawers and extruders"
8130 "Glass, ceramics and related plant operators"
8131 "Glass and ceramics kiln and related machine operators"
8139 "Glass, ceramics and related plant operators not elsewhere classified"
8140 "Wood-processing- and papermaking-plant operators"
8141 "Wood-processing-plant operators"
8142 "Paper-pulp plant operators"
8143 "Papermaking-plant operators"
8150 "Chemical-processing-plant operators"
8151 "Crushing-, grinding- and chemical-mixing-machinery operators"
8152 "Chemical-heat-treating-plant operators"
8153 "Chemical-filtering- and separating-equipment operators"
8154 "Chemical-still and reactor operators (except petroleum and natural gas)"
8155 "Petroleum- and natural-gas-refining-plant operators"
8159 "Chemical-processing-plant operators not elsewhere classified"
8160 "Power-production and related plant operators"
8161 "Power-production plant operators"
8162 "Steam-engine and boiler operators"
8163 "Incinerator, water-treatment and related plant operators"
8170 "Automated-assembly-line and industrial-robot operators"
8171 "Automated-assembly-line operators"
8172 "Industrial-robot operators"
8200 "Machine operators and assemblers"
8210 "Metal- and mineral-products machine operators"
8211 "Machine-tool operators"
8212 "Cement and other mineral products machine operators"
8220 "Chemical-products machine operators"
8221 "Pharmaceutical- and toiletry-products machine operators"
8222 "Ammunition- and explosive-products machine operators"
8223 "Metal finishing-, plating- and coating-machine operators"
8224 "Photographic-products machine operators"
8229 "Chemical-products machine operators not elsewhere classified"
8230 "Rubber- and plastic-products machine operators"
8231 "Rubber-products machine operators"
8232 "Plastic-products machine operators"
8240 "Wood-products machine operators"
8250 "Printing-, binding- and paper-products machine operators"
8251 "Printing-machine operators"
8252 "Bookbinding-machine operators"
8253 "Paper-products machine operators"
8260 "Textile-, fur- and leather-products machine operators"
8261 "Fibre-preparing-, spinning- and winding-machine operators"
8262 "Weaving- and knitting-machine operators"
8263 "Sewing-machine operators"
8264 "Bleaching-, dyeing- and cleaning-machine operators"
8265 "Fur and leather-preparing-machine operators"
8266 "Shoemaking- and related machine operators"
8269 "Textile-, fur- and leather-products machine operators not elsewhere classified"
8270 "Food and related products machine operators"
8271 "Meat- and fish-processing-machine operators"
8272 "Dairy-products machine operators"
8273 "Grain- and spice-milling-machine operators"
8274 "Baked-goods, cereal and chocolate-products machine operators"
8275 "Fruit-, vegetable- and nut-processing-machine operators"
8276 "Sugar production machine operators"
8277 "Tea-, coffee-, and cocoa-processing-machine operators"
8278 "Brewers, wine and other beverage machine operators"
8279 "Tobacco production machine operators"
8280 "Assemblers"
8281 "Mechanical-machinery assemblers"
8282 "Electrical-equipment assemblers"
8283 "Electronic-equipment assemblers"
8284 "Metal-, rubber- and plastic-products assemblers"
8285 "Wood and related products assemblers"
8286 "Paperboard, textile and related products assemblers"
8290 "Other machine operators and assemblers"
8300 "Drivers and mobile-plant operators"
8310 "Locomotive-engine drivers and related workers"
8311 "Locomotive-engine drivers"
8312 "Railway brakers, signallers and shunters"
8320 "Motor-vehicle drivers"
8321 "Motor-cycle drivers"
8322 "Car, taxi and van drivers"
8323 "Bus and tram drivers"
8324 "Heavy-truck and lorry drivers"
8330 "Agricultural and other mobile-plant operators"
8331 "Motorised farm and forestry plant operators"
8332 "Earth-moving- and related plant operators"
8333 "Crane, hoist and related plant operators"
8334 "Lifting-truck operators"
8340 "Ships' deck crews and related workers"
9000 "Elementary occupations"
9100 "Sales and services elementary occupations"
9110 "Street vendors and related workers"
9111 "Street food vendors"
9112 "Street vendors, non-food products"
9113 "Door-to-door and telephone salespersons"
9120 "Shoe cleaning and other street services elementary occupations"
9130 "Domestic and related helpers, cleaners and launderers"
9131 "Domestic helpers and cleaners"
9132 "Helpers and cleaners in offices, hotels and other establishments"
9133 "Hand-launderers and pressers"
9140 "Building caretakers, window and related cleaners"
9141 "Building caretakers"
9142 "Vehicle, window and related cleaners"
9150 "Messengers, porters, doorkeepers and related workers"
9151 "Messengers, package and luggage porters and deliverers"
9152 "Doorkeepers, watchpersons and related workers"
9153 "Vending-machine money collectors, meter readers and related workers"
9160 "Garbage collectors and related labourers"
9161 "Garbage collectors"
9162 "Sweepers and related labourers"
9200 "Agricultural, fishery and related labourers"
9210 "Agricultural, fishery and related labourers"
9211 "Farm-hands and labourers"
9212 "Forestry labourers"
9213 "Fishery, hunting and trapping labourers"
9300 "Labourers in mining, construction, manufacturing and transport"
9310 "Mining and construction labourers"
9311 "Mining and quarrying labourers"
9312 "Construction and maintenance labourers: roads, dams and similar constructions"
9313 "Building construction labourers"
9320 "Manufacturing labourers"
9321 "Assembling labourers"
9322 "Hand packers and other manufacturing labourers"
9330 "Transport labourers and freight handlers"
9331 "Hand or pedal vehicle drivers"
9332 "Drivers of animal-drawn vehicles and machinery"
9333 "Freight handlers"
0000 "Armed forces"
0100 "Armed forces"
0110 "Armed forces"
% END

% LABELLIST-isco88com
% source: ISCO88.xls from https://warwick.ac.uk/fac/soc/ier/research/classification/isco88
% variables: ICSO88COM ICSO88COM-label-E
1000 "Legislators, senior officials and managers"
1100 "Legislators and senior officials"
1110 "Legislators and senior government officials"
1140 "Senior officials of special-interest organisations"
1141 "Senior officials of political party organisations"
1142 "Senior officials of employers', workers' and other economic-interest organisations"
1143 "Senior officials of humanitarian and other special-interest organisations"
1200 "Corporate managers"
1210 "Directors and chief executives"
1220 "Production and operations managers"
1221 "Production and operations managers in agriculture, hunting, forestry and fishing"
1222 "Production and operations managers in manufacturing"
1223 "Production and operations managers in construction"
1224 "Production and operations managers in wholesale and retail trade"
1225 "Production and operations managers in restaurants and hotels"
1226 "Production and operations managers in transport, storage and communications"
1227 "Production and operations managers in business services enterprises"
1228 "Production and operations managers in personal care, cleaning and related services"
1229 "Production and operations managers not elsewhere classified"
1230 "Other specialist managers"
1231 "Finance and administration managers"
1232 "Personnel and industrial relations managers"
1233 "Sales and marketing managers"
1234 "Advertising and public relations managers"
1235 "Supply and distribution managers"
1236 "Computing services managers"
1237 "Research and development managers"
1239 "Other specialist managers not elsewhere classified"
1300 "Managers of small enterprises"
1310 "Managers of small enterprises"
1311 "Managers of small enterprises in agriculture, hunting,  forestry and fishing"
1312 "Managers of small enterprises in manufacturing"
1313 "Managers of small enterprises in construction"
1314 "Managers of small enterprises in wholesale and retail trade"
1315 "Managers of small enterprises of restaurants and hotels"
1316 "Managers of small enterprises in transport, storage and communications"
1317 "Managers of small enterprises in business services enterprises"
1318 "Managers of small enterprises in personal care, cleaning and related services"
1319 "Managers of small enterprises not elsewhere classifie"
2000 "Professionals"
2100 "Physical, mathematical and engineering science professionals"
2110 "Physicists, chemists and related professionals"
2111 "Physicists and astronomers"
2112 "Meteorologists"
2113 "Chemists"
2114 "Geologists and geophysicists"
2120 "Mathematicians, statisticians and related professionals"
2121 "Mathematicians and related professionals"
2122 "Statisticians"
2130 "Computing professionals"
2131 "Computer systems designers, analysts and programmers"
2139 "Computing professionals not elsewhere classified"
2140 "Architects, engineers and related professionals"
2141 "Architects, town and traffic planners"
2142 "Civil engineers"
2143 "Electrical engineers"
2144 "Electronics and telecommunications engineers"
2145 "Mechanical engineers"
2146 "Chemical engineers"
2147 "Mining engineers, metallurgists and related professionals"
2148 "Cartographers and surveyors"
2149 "Architects, engineers and related professionals not elsewhere classified"
2200 "Life science and health professionals"
2210 "Life science professionals"
2211 "Biologists, botanists, zoologists and related professionals"
2212 "Pharmacologists, pathologists and related professionals"
2213 "Agronomists and related professionals"
2220 "Health professionals (except nursing)"
2221 "Medical doctors"
2222 "Dentists"
2223 "Veterinarians"
2224 "Pharmacists"
2229 "Health professionals (except nursing) not elsewhere classified"
2230 "Nursing and midwifery professionals"
2300 "Teaching professionals"
2310 "College, university and higher education teaching professionals"
2320 "Secondary education teaching professionals"
2330 "Primary and pre-primary education teaching professionals"
2331 "Primary education teaching professionals"
2332 "Pre-primary education teaching professionals"
2340 "Special education teaching professionals"
2350 "Other teaching professionals"
2351 "Education methods specialists"
2352 "School inspectors"
2359 "Other teaching professionals not elsewhere classified"
2400 "Other professionals"
2410 "Business professionals"
2411 "Accountants"
2412 "Personnel and careers professionals"
2419 "Business professionals not elsewhere classified"
2420 "Legal professionals"
2421 "Lawyers"
2422 "Judges"
2429 "Legal professionals not elswhere classified"
2430 "Archivists, librarians and related information professionals"
2431 "Archivists and curators"
2432 "Librarians and related information professionals"
2440 "Social science and related professionals"
2441 "Economists"
2442 "Sociologists, anthropologists and related professionals"
2443 "Philosophers, historians and political scientists"
2444 "Philologists, translators and interpreters"
2445 "Psychologists"
2446 "Social work professionals"
2450 "Writers and creative or performing artists"
2451 "Authors, journalists and other writers"
2452 "Sculptors, painters and related artists"
2453 "Composers, musicians and singers"
2454 "Choreographers and dancers"
2455 "Film, stage and related actors and directors"
2460 "Religious professionals"
2470 "Public service administrative professionals"
3000 "Technicians and associate professionals"
3100 "Physical and engineering science associate professionals"
3110 "Physical and engineering science technicians"
3111 "Chemical and physical science technicians"
3112 "Civil engineering technicians"
3113 "Electrical engineering technicians"
3114 "Electronics and telecommunications engineering technicians"
3115 "Mechanical engineering technicians"
3116 "Chemical engineering technicians"
3117 "Mining and metallurgical technicians"
3118 "Draughtspersons"
3119 "Physical and engineering science technicians not elsewhere classified"
3120 "Computer associate professionals"
3121 "Computer assistants"
3122 "Computer equipment operators"
3123 "Industrial robot controllers"
3130 "Optical and electronic equipment operators"
3131 "Photographers and image and sound recording equipment operators"
3132 "Broadcasting and telecommunications equipment operators"
3133 "Medical equipment operators"
3139 "Optical and electronic equipment operators not elsewhere classified"
3140 "Ship and aircraft controllers and technicians"
3141 "Ships' engineers"
3142 "Ships' deck officers and pilots"
3143 "Aircraft pilots and related associate professionals"
3144 "Air traffic controllers"
3145 "Air traffic safety technicians"
3150 "Safety and quality inspectors"
3151 "Building and fire inspectors"
3152 "Safety, health and quality inspectors"
3200 "Life science and health associate professionals"
3210 "Life science technicians and related associate professional"
3211 "Life science technicians"
3212 "Agronomy and forestry technicians"
3213 "Farming and forestry advisers"
3220 "Health associate professionals (except nursing)"
3221 "Medical assistants"
3222 "Hygienists, health and environmental officers"
3223 "Dieticians and nutritionists"
3224 "Optometrists and opticians"
3225 "Dental assistants"
3226 "Physiotherapists and related associate professionals"
3227 "Veterinary assistants"
3228 "Pharmaceutical assistants"
3229 "Health associate professionals (except nursing) not elsewhere classified"
3230 "Nursing and midwifery associate professionals"
3231 "Nursing associate professionals"
3232 "Midwifery associate professionals"
3300 "Teaching associate professionals"
3310 "Primary education teaching associate professionals"
3320 "Pre-primary education teaching associate professionals"
3330 "Special education teaching associate professionals"
3340 "Other teaching associate professionals"
3400 "Other associate professionals"
3410 "Finance and sales associate professionals"
3411 "Securities and finance dealers and brokers"
3412 "Insurance representatives"
3413 "Estate agents"
3414 "Travel consultants and organisers"
3415 "Technical and commercial sales representatives"
3416 "Buyers"
3417 "Appraisers, valuers and auctioneers"
3419 "Finance and sales associate professionals not elsewhere classified"
3420 "Business services agents and trade brokers"
3421 "Trade brokers"
3422 "Clearing and forwarding agents"
3423 "Employment agents and labour contractors"
3429 "Business services agents and trade brokers not elsewhere classified"
3430 "Administrative associate professionals"
3431 "Administrative secretaries and related associate professionals"
3432 "Legal and related business associate professionals"
3433 "Bookkeepers"
3434 "Statistical, mathematical and related associate professionals"
3440 "Customs, tax and related government associate professionals"
3441 "Customs and border inspectors"
3442 "Government tax and excise officials"
3443 "Government social benefits officials"
3444 "Government licensing officials"
3449 "Customs, tax and related government associate professionals not elsewhere classified"
3450 "Police inspectors and detectives"
3460 "Social work associate professionals"
3470 "Artistic, entertainment and sports associate professionals"
3471 "Decorators and commercial designers"
3472 "Radio, television and other announcers"
3473 "Street, night-club and related musicians, singers and dancers"
3474 "Clowns, magicians, acrobats and related associate professionals"
3475 "Athletes, sports persons and related associate professionals"
3480 "Religious associate professionals"
4000 "Clerks"
4100 "Office clerks"
4110 "Secretaries and keyboard-operating clerks"
4111 "Stenographers and typists"
4112 "Word-processor and related operators"
4113 "Data entry operators"
4114 "Calculating-machine operators"
4115 "Secretaries"
4120 "Numerical clerks"
4121 "Accounting and book-keeping clerks"
4122 "Statistical and finance clerks"
4130 "Material-recording and transport clerks"
4131 "Stock clerks"
4132 "Production clerks"
4133 "Transport clerks"
4140 "Library, mail and related clerks"
4141 "Library and filing clerks"
4142 "Mail carriers and sorting clerks"
4143 "Coding, proof-reading and related clerks"
4144 "Scribes and related workers"
4190 "Other office clerks"
4200 "Customer services clerks"
4210 "Cashiers, tellers and related clerks"
4211 "Cashiers and ticket clerks"
4212 "Tellers and other counter clerks"
4213 "Bookmakers and croupiers"
4214 "Pawnbrokers and money-lenders"
4215 "Debt-collectors and related workers"
4220 "Client information clerks"
4221 "Travel agency and related clerks"
4222 "Receptionists and information clerks"
4223 "Telephone switchboard operators"
5000 "Service workers and shop and market sales workers"
5100 "Personal and protective services workers"
5110 "Travel attendants and related workers"
5111 "Travel attendants and travel stewards"
5112 "Transport conductors"
5113 "Travel guides"
5120 "Housekeeping and restaurant services workers"
5121 "Housekeepers and related workers"
5122 "Cooks"
5123 "Waiters, waitresses and bartenders"
5130 "Personal care and related workers"
5131 "Child-care workers"
5132 "Institution-based personal care workers"
5133 "Home-based personal care workers"
5139 "Personal care and related workers not elsewhere classified"
5140 "Other personal services workers"
5141 "Hairdressers, barbers, beauticians and related workers"
5142 "Companions and valets"
5143 "Undertakers and embalmers"
5149 "Other personal services workers not elsewhere classified"
5160 "Protective services workers"
5161 "Fire-fighters"
5162 "Police officers"
5163 "Prison guards"
5169 "Protective services workers not elsewhere classified"
5200 "Models, salespersons and demonstrators"
5210 "Fashion and other models"
5220 "Shop, stall and market salespersons and demonstrators"
6000 "Skilled agricultural and fishery workers"
6100 "Skilled agricultural and fishery workers"
6110 "Market gardeners and crop growers"
6111 "Field crop and vegetable growers"
6112 "Gardeners, horticultural and nursery growers"
6120 "Animal producers and related workers"
6121 "Dairy and livestock producers"
6122 "Poultry producers"
6129 "Animal producers and related workers not elsewhere classified"
6130 "Crop and animal producers"
6140 "Forestry and related workers"
6141 "Forestry workers and loggers"
6142 "Charcoal burners and related workers"
6150 "Fishery workers, hunters and trappers"
6151 "Aquatic life cultivation workers"
6152 "Inland and coastal waters fishery workers"
6153 "Deep-sea fishery workers"
6154 "Hunters and trappers"
7000 "Craft and related trades workers"
7100 "Extraction and building trades workers"
7110 "Miners, shotfirers, stone cutters and carvers"
7111 "Miners and quarry workers"
7112 "Shotfirers and blasters"
7113 "Stone splitters, cutters and carvers"
7120 "Building frame and related trades workers"
7121 "Builders"
7122 "Bricklayers and stonemasons"
7123 "Concrete placers, concrete finishers and related workers"
7124 "Carpenters and joiners"
7129 "Building frame and related trades workers not elsewhere classified"
7130 "Building finishers and related trades workers"
7131 "Roofers"
7132 "Floor layers and tile setters"
7133 "Plasterers"
7134 "Insulation workers"
7135 "Glaziers"
7136 "Plumbers and pipe fitters"
7137 "Building and related electricians"
7139 "Building finishers and related trade workers not elsewhere classified"
7140 "Painters, building structure cleaners and related trades workers"
7141 "Painters and related workers"
7143 "Building structure cleaners"
7200 "Metal, machinery and related trades workers"
7210 "Metal moulders, welders, sheet-metal workers, structural-metal preparers, and related trades workers"
7211 "Metal moulders and coremakers"
7212 "Welders and flame cutters"
7213 "Sheet-metal workers"
7214 "Structural-metal preparers and erectors"
7215 "Riggers and cable splicers"
7216 "Underwater workers"
7220 "Blacksmiths, tool-makers and related trades workers"
7221 "Blacksmiths, hammer-smiths and forging-press workers"
7222 "Tool-makers and related workers"
7223 "Machine-tool setters and setter-operators"
7224 "Metal wheel-grinders, polishers and tool sharpeners"
7230 "Machinery mechanics and fitters"
7231 "Motor vehicle mechanics and fitters"
7232 "Aircraft engine mechanics and fitters"
7233 "Agricultural- or industrial-machinery mechanics and fitters"
7240 "Electrical and electronic equipment mechanics and fitters"
7241 "Electrical mechanics fitters and services"
7242 "Electronics mechanics, fitters and servicers"
7244 "Telegraph and telephone installers and servicers"
7245 "Electrical line installers, repairers and cable jointers"
7300 "Precision, handicraft, craft printing and related trades workers"
7310 "Precision workers in metal and related materials"
7311 "Precision-instrument makers and repairers"
7312 "Musical-instrument makers and tuners"
7313 "Jewellery and precious-metal workers"
7320 "Potters, glass-makers and related trades workers"
7321 "Abrasive wheel formers, potters and related workers"
7322 "Glass-makers, cutters, grinders and finishers"
7323 "Glass engravers and etchers"
7324 "Glass, ceramics and related decorative painters"
7330 "Handicraft workers in wood, textile, leather and related materials"
7331 "Handicraft workers in wood and related materials"
7332 "Handicraft workers in textile, leather and related materials"
7340 "Craft printing and related trades workers"
7341 "Compositors, typesetters and related workers"
7342 "Stereotypers and electrotypers"
7343 "Printing engravers and etchers"
7344 "Photographic and related workers"
7345 "Bookbinders and related workers"
7346 "Silk-screen, block and craft textile printers"
7400 "Other craft and related trades workers"
7410 "Food processing and related trades workers"
7411 "Butchers, fishmongers and related food preparers"
7412 "Bakers, pastry-cooks and confectionery makers"
7413 "Dairy-products workers"
7414 "Fruit, vegetable and related preservers"
7415 "Food and beverage tasters and graders"
7416 "Tobacco preparers and tobacco products makers"
7420 "Wood treaters, cabinet-makers and related trades workers"
7421 "Wood treaters"
7422 "Cabinetmakers and related workers"
7423 "Woodworking machine setters and setter-operators"
7424 "Basketry weavers, brush makers and related workers"
7430 "Textile, garment and related trades workers"
7431 "Fibre preparers"
7432 "Weavers, knitters and related workers"
7433 "Tailors, dressmakers and hatters"
7434 "Furriers and related workers"
7435 "Textile, leather and related pattern-makers and cutters"
7436 "Sewers, embroiderers and related workers"
7437 "Upholsterers and related workers"
7440 "Pelt, leather and shoemaking trades workers"
7441 "Pelt dressers, tanners and fellmongers"
7442 "Shoe-makers and related workers"
8000 "Plant and machine operators and assemblers"
8100 "Stationary plant and related operators"
8110 "Mining and mineral-processing-plant operators"
8111 "Mining plant operators"
8112 "Mineral-ore and stone-processing-plant operators"
8113 "Well drillers and borers and related workers"
8120 "Metal-processing plant operators"
8121 "Ore and metal furnace operators"
8122 "Metal melters, casters and rolling-mill operators"
8123 "Metal heat-treating-plant operators"
8124 "Metal drawers and extruders"
8130 "Glass, ceramics and related plant operators"
8131 "Glass and ceramics kiln and related machine operators"
8139 "Glass, ceramics and related plant operators not elsewhere classified"
8140 "Wood-processing- and papermaking-plant operators"
8141 "Wood-processing-plant operators"
8142 "Paper-pulp plant operators"
8143 "Papermaking-plant operators"
8150 "Chemical-processing-plant operators"
8151 "Crushing-, grinding- and chemical-mixing-machinery operators"
8152 "Chemical-heat-treating-plant operators"
8153 "Chemical-filtering- and separating-equipment operators"
8154 "Chemical-still and reactor operators (except petroleum and natural gas)"
8155 "Petroleum- and natural-gas-refining-plant operators"
8159 "Chemical-processing-plant operators not elsewhere classified"
8160 "Power-production and related plant operators"
8161 "Power-production plant operators"
8162 "Steam-engine and boiler operators"
8163 "Incinerator, water-treatment and related plant operators"
8170 "Industrial robot operators"
8200 "Machine operators and assemblers"
8210 "Metal- and mineral-products machine operators"
8211 "Machine-tool operators"
8212 "Cement and other mineral products machine operators"
8220 "Chemical-products machine operators"
8221 "Pharmaceutical-and toiletry-products machine operators"
8222 "Ammunition- and explosive-products machine operators"
8223 "Metal finishing-, plating- and coating-machine operators"
8224 "Photographic-products machine operators"
8229 "Chemical-products machine operators not elsewhere classified"
8230 "Rubber- and plastic-products machine operators"
8231 "Rubber-products machine operators"
8232 "Plastic-products machine operators"
8240 "Wood-products machine operators"
8250 "Printing-, binding- and paper-products machine operators"
8251 "Printing-machine operators"
8252 "Book-binding-machine operators"
8253 "Paper-products machine operators"
8260 "Textile-, fur- and leather-products machine operators"
8261 "Fibre-preparing-, spinning- and winding-machine operators"
8262 "Weaving- and knitting-machine operators"
8263 "Sewing-machine operators"
8264 "Bleaching-, dyeing- and cleaning-machine operators"
8265 "Fur- and leather-preparing-machine operators"
8266 "Shoemaking- and related machine operators"
8269 "Textile-, fur- and leather-products machine operators not elsewhere classified"
8270 "Food and related products machine operators"
8271 "Meat- and fish-processing-machine operators"
8272 "Dairy-products machine operators"
8273 "Grain- and spice-milling-machine operators"
8274 "Baked-goods, cereal- and chocolate-products machine operators"
8275 "Fruit-, vegetable- and nut-processing-machine operators"
8276 "Sugar production machine operators"
8277 "Tea-, coffee- and cocoa-processing-machine operators"
8278 "Brewers, wine and other beverage machine operators"
8279 "Tobacco production machine operators"
8280 "Assemblers"
8281 "Mechanical-machinery assemblers"
8282 "Electrical-equipment assemblers"
8283 "Electronic-equipment assemblers"
8284 "Metal-, rubber- and plastic-products assemblers"
8285 "Wood and related products assemblers"
8286 "Paperboard, textile and related products assemblers"
8287 "Composite products assemblers"
8290 "Other machine operators not elsewhere classified"
8300 "Drivers and mobile plant operators"
8310 "Locomotive engine drivers and related workers"
8311 "Locomotive engine drivers"
8312 "Railway brakers, signallers and shunters"
8320 "Motor vehicle drivers"
8321 "Motorcycle drivers"
8322 "Car, taxi and van drivers"
8323 "Bus and tram drivers"
8324 "Heavy truck and lorry drivers"
8330 "Agricultural and other mobile plant operators"
8331 "Motorised farm and forestry plant operators"
8332 "Earth-moving and related plant operators"
8333 "Crane, hoist and related plant operators"
8334 "Lifting-truck operators"
8340 "Ships' deck crews and related workers"
9000 "Elementary occupations"
9100 "Sales and services elementary occupations"
9110 "Street vendors and related workers"
9111 "Street vendors"
9113 "Door-to-door and telephone salespersons"
9120 "Shoe cleaning and other street services elementary occupations"
9130 "Domestic and related helpers, cleaners and launderers"
9131 "Domestic helpers and cleaners"
9132 "Helpers and cleaners in offices, hotels and other establishments"
9133 "Hand-launderers and pressers"
9140 "Building caretakers, window and related cleaners"
9141 "Building caretakers"
9142 "Vehicle, window and related cleaners"
9150 "Messengers, porters, doorkeepers and related workers"
9151 "Messengers, package and luggage porters and deliverers"
9152 "Doorkeepers, watchpersons and related workers"
9153 "Vending-machine money collectors, meter readers and related workers"
9160 "Garbage collectors and related labourers"
9161 "Garbage collectors"
9162 "Sweepers and related labourers"
9200 "Agricultural, fishery and related labourers"
9210 "Agricultural, fishery and related labourers"
9211 "Farm-hands and labourers"
9212 "Forestry labourers"
9213 "Fishery, hunting and trapping labourers"
9300 "Labourers in mining, construction, manufacturing and transport"
9310 "Mining and construction labourers"
9311 "Mining and quarrying labourers"
9312 "Construction and maintenance labourers: roads, dams and similar constructions"
9313 "Building construction labourers"
9320 "Manufacturing labourers"
9330 "Transport labourers and freight handlers"
0000 "Armed forces"
0100 "Armed forces"
% END

% LABELLIST-isco88a
% source: iskolab.sps from http://www.harryganzeboom.nl/isco88/
% variables: ISCO88 ISCO88-label-E
0110 '[armed forces]'
6131 '[mixed farmer]'
6132 '[farm foreman]'
6133 '[farmer nfs]'
6134 '[skilled farm worker nfs]'
7510 '[non farm foremen nfs]'
7520 '[skilled manual nfs]'
3451 '[police inspectors-detectives]'
3452 '[armed forces non-commissioned officers + army nfs]'
2322 '[second voc teacher]'
2321 '[second academ teacher]'
2323 '[middle school teacher]'
8400 '[semiskld worker nfs]'
7530 '[apprentice skilled work nfs]'
7234 '[unskl garage worker, oiler]'
1000 'legislators, senior officials & managers'
1100 'legislators & senior officials'
1110 'legislators'
1120 'senior government officials'
1130 'traditional chiefs & heads of villages'
1140 'senior officials special-interest organisations'
1141 'senior officials political-party organisations'
1142 'senior officials economic-interest organisations'
1143 'senior officials special-interest organisations'
1200 'corporate managers [large enterprise]'
1210 'directors & chief executives [large enterprise]'
1220 'department managers [large enterprise][production-operation]'
1221 'department managers agriculture, hunting, forestry & fishlng'
1222 'department managers manufacturing'
1223 'department managers construction'
1224 'department managers wholesale & retail trade'
1225 'department managers restaurants & hotels'
1226 'department managers transport, storage & communications'
1227 'department managers business services'
1228 'department managers personal care, cleaning etc'
1229 'operations department managers nec'
1230 'other department managers [large enterprise]'
1231 'finance & administration department managers'
1232 'personnel & industrial relations department managers'
1233 'sales & marketing department managers'
1234 'advertising & public relations department managers'
1235 'supply & distribution department managers'
1236 'computing services department managers'
1237 'research & development department managers'
1239 'other department managers nec'
1240 '[office manager]'
1250 '[military officers]'
1251 '[high military officers]'
1252 '[lower military officers]'
1300 'general managers [small enterprise]'
1310 'general managers [small enterprise]'
1311 'general managers agriculture, hunting forestry & fishing'
1312 'general managers manufacturing'
1313 'general managers construction'
1314 'general managers wholesale & retail trade'
1315 'general managers restaurants & hotels'
1316 'general managers transport, storage & communications'
1317 'general managers business services'
1318 'general managers personal care, cleaning etc services'
1319 'general managers nec'
2000 'professionals'
2100 'physical, mathematical & engineering science professionals'
2110 'physicists, chemists & related professionals'
2111 'physicists & astronomers'
2112 'meteorologists'
2113 'chemists'
2114 'geologists & geophysicists'
2120 'mathematicians, statisticians etc professionals'
2121 'mathematicians etc professionals'
2122 'statisticians'
2130 'computing professionals'
2131 'computer systems designers & analysts'
2132 'computer programmers'
2139 'computing professionals nec'
2140 'architects, engineers etc professionals'
2141 'architects town & traffic planners'
2142 'civil engineers'
2143 'electrical engineers'
2144 'electronics & telecommunications engineers'
2145 'mechanical engineers'
2146 'chemical engineers'
2147 'mining engineers metallurgists etc professionals'
2148 'cartographers & surveyors'
2149 'architects engineers etc professionals nec'
2200 'life science & health professionals'
2210 'life science professionals'
2211 'biologists, botanists zoologists etc professionals'
2212 'pharmacologists, pathologists etc professlonals'
2213 'agronomists etc professionals'
2220 'health professionals (except nursing)'
2221 'medical doctors'
2222 'dentists'
2223 'veterinarians'
2224 'pharmacists'
2229 'health professionals except nursing nec'
2230 'nursing & midwifery professionals'
2300 'teaching professionals'
2310 'higher education teaching professionals'
2320 'secondary education teaching professionals'
2330 'primary & pre-primary education teaching professionals'
2331 'primary education teaching professionals'
2332 'pre-primary education teaching professionals'
2340 'special education teaching professionals'
2350 'other teaching professionals'
2351 'education methods specialists'
2352 'school inspectors'
2359 'other teaching professionals nec'
2400 'other professionals'
2410 'business professionals'
2411 'accountants'
2412 'personnel & careers professionals'
2419 'business professionals nec'
2420 'legal professionals'
2421 'lawyers'
2422 'judges'
2429 'legal professionals nec'
2430 'archivists, librarians etc information professionals'
2431 'archivists & curators'
2432 'librarians etc information professionals'
2440 'social science etc professionals'
2441 'economists'
2442 'sociologists anthropologists etc professionals'
2443 'philosophers, historians & political scientists'
2444 'philologists translators & interpreters'
2445 'psychologists'
2446 'social work professionals'
2450 'writers & creative or performing artists'
2451 'authors journalists & other writers'
2452 'sculptors, painters etc artists'
2453 'composers musicians & singers'
2454 'choreographers & dancers'
2455 'film, stage etc actors & directors'
2460 'religious professionals'
3000 'technicians and associated professionals'
3100 'physical & engineering science associate professionals'
3110 'physical & engineering science technicians'
3111 'chemical & physical science technicians'
3112 'civil engineering technicians'
3113 'electrical engineering technicians'
3114 'electronics & telecommunications engineering technicians'
3115 'mechanical engineering technicians'
3116 'chemical engineering technicians'
3117 'mining & metallurgical technicians'
3118 'draughtspersons'
3119 'physical & engineering science technicians nec'
3120 'computer associate professionals'
3121 'computer assistants'
3122 'computer equipment operators'
3123 'industrial robot controllers'
3130 'optical & electronic equipment operators'
3131 'photographers & elctr equipment operators'
3132 'broadcasting & telecommunications equipment operators'
3133 'medical equipment operators'
3139 'optical & electronic equipment operators nec'
3140 'ship & aircraft controllers & technicians'
3141 'ships engineers'
3142 'ships deck officers & pilots'
3143 'aircraft pilots etc associate professionals'
3144 'air traffic controllers'
3145 'air traffic safety technicians'
3150 'safety & ouality inspectors'
3151 'building & fire inspectors'
3152 'safety, health & quality inspectors'
3200 'life science & health associate professionals'
3210 'life science technicians etc associate professionals'
3211 'life science technicians'
3212 'agronomy & forestry technicians'
3213 'farming & forestry advisers'
3220 'modern health associate professionals except nursing'
3221 'medical assistants'
3222 'sanitarians'
3223 'dieticians & nutritionists'
3224 'optometrists & opticians'
3225 'dental assistants'
3226 'physiotherapists etc associate professionals'
3227 'veterinary assistants'
3228 'pharmaceutical assistants'
3229 'modern health associate professionals except nursing nec'
3230 'nursing & midwifery associate professionals'
3231 'nursing associate professionals'
3232 'midwifery associate professionals'
3240 'traditional medicine practitioners & faith healers'
3241 'traditional medicine practitioners'
3242 'faith healers'
3300 'teaching associate professionals'
3310 'primary education teaching associate professionals'
3320 'pre-primary education teaching associate professionals'
3330 'special education teaching associate professionals'
3340 'other teaching associate professionals'
3400 'other associate professionals'
3410 'finance & sales associate professionals'
3411 'securities & finance dealers & brokers'
3412 'insurance representatives'
3413 'estate agents'
3414 'travel consultants & organisers'
3415 'technical & commercial sales representatives'
3416 'buyers'
3417 'appraisers, valuers & auctioneers'
3419 'finance & sales associate professionals nec'
3420 'business services agents and trade brokers'
3421 'trade brokers'
3422 'clearing & forwarding agents'
3423 'employment agents & labour contractors'
3429 'business services agents & trade brokers nec'
3430 'administrative associate professionals'
3431 'administrative secretaries etc associate professionals'
3432 'legal etc business associate professionals'
3433 'bookkeepers'
3434 'statistical, mathematical etc associate professionals'
3439 'administrative associate professionals nec'
3440 'customs, tax etc government associate professionals'
3441 'customs & border inspectors'
3442 'government tax & excise officials'
3443 'government social benefits officials'
3444 'government licensing officials'
3449 'customs tax etc government associate professionals nec'
3450 '[police and army officers]'
3460 'social work associate professionals'
3470 'artistic, entertainment & sports associate professionals'
3471 'decorators & commercial designers'
3472 'radio, television & other announcers'
3473 'street night-club etc musicians singers & dancers'
3474 'clowns magicians acrobats etc associate professionals'
3475 'athletes sportspersons etc associate professionals'
3480 'religious associate professionals'
4000 'clerks'
4100 'office clerks'
4110 'secretaries & keyboard-operating clerks'
4111 'stenographers & typists'
4112 'word-processor etc operators'
4113 'data entry operators'
4114 'calculating-machine operators'
4115 'secretaries'
4120 'numerical clerks'
4121 'accounting & bookkeeping clerks'
4122 'statistical & finance clerks'
4130 'material-recording & transport clerks'
4131 'stock clerks'
4132 'production clerks'
4133 'transport clerks'
4140 'library, mail etc clerks'
4141 'library & filing clerks'
4142 'mail carriers & sorting clerks'
4143 'coding proof-reading etc clerks'
4144 'scribes etc workers'
4190 'other office clerks'
4200 'customer services clerks'
4210 'cashiers, tellers etc clerks'
4211 'cashiers & ticket clerks'
4212 'tellers & other counter clerks'
4213 'bookmakers & croupiers'
4214 'pawnbrokers & money-lenders'
4215 'debt-collectors etc workers'
4220 'client information clerks'
4221 'travel agency etc clerks'
4222 'receptionists & information clerks'
4223 'telephone switchboard operators'
5000 'service workers & shop & market sales workers'
5100 'personal & protective services workers'
5110 'travel attendants etc'
5111 'travel attendants & travel stewards'
5112 'transport conductors'
5113 'travel, museum guides'
5120 'housekeeping & restaurant services workers'
5121 'housekeepers etc workers'
5122 'cooks'
5123 'waiters waitresses & bartenders'
5130 'personal care etc work'
5131 'child-care workers'
5132 'institution-based personal care workers'
5133 'home based personal care workers'
5139 'personal care etc workers nec'
5140 'other personal services workers'
5141 'hairdressers barbers beauticians etc workers'
5142 'companions & valets '
5143 'undertakers & embalmers'
5149 'other personal services workers nec'
5150 'astrologers, fortune-tellers etc workers'
5151 'astrologers etc workers'
5152 'fortune-tellers, palmists etc workers'
5160 'protective services workers'
5161 'fire-fighters'
5162 'police officers'
5163 'prison guards'
5164 '[army: enlisted men - soldiers]'
5169 'protective services workers nec'
5200 'models, salespersons & demonstrators'
5210 'fashion & other models'
5220 'shop salespersons & demonstrators'
5230 'stall & market salespersons'
6000 'skilled agricultural & fishery workers'
6100 'market-oriented skilled agricultural & fishery workers'
6110 'market gardeners & cropgrowers'
6111 'field crop & vegetable growers'
6112 'tree & shrub crop growers'
6113 'gardeners, horticultural & nursery growers'
6114 'mixed-crop growers'
6120 'market-oriented animal producers etc workers'
6121 'dairy & livestock producers'
6122 'poultry producers'
6123 'apiarists & sericulturists'
6124 'mixed -animal producers'
6129 'market-oriented animal producers etc workers nec'
6130 'market-oriented crop & animal producers'
6140 'forestry etc workers'
6141 'forestry workers & loggers'
6142 'charcoal burners etc workers'
6150 'fishery workers, hunters & trappers'
6151 'aquatic-life cultivation workers'
6152 'inland & coastal waters fishery workers'
6153 'deep-sea fishery workers'
6154 'hunters & trappers'
6200 'subsistence agricultural & fishery workers'
6210 'subsistence agricultural & fishery workers'
7000 'craft etc trades workers'
7100 'extraction & building trades workers'
7110 'miners, shotfirers, stone cutters & carvers'
7111 'miners & quarry workers'
7112 'shotfirers & blasters'
7113 'stone splitters cutters & carvers'
7120 '[builders] & ar trades workers'
7121 'builders traditional materials'
7122 'bricklayers & stonemasons'
7123 'concrete placers concrete finishers etc workers'
7124 'carpenters & joiners'
7129 '[builders] ar trades workers nec'
7130 'building finishers etc trades workers'
7131 'roofers'
7132 'floor layers & tile setters'
7133 'plasterers'
7134 'insulation workers'
7135 'glaziers'
7136 'plumbers & pipe fitters'
7137 'building etc electricians'
7140 'painters, building structure cleaners etc trades workers'
7141 'painters etc workers'
7142 'varnishers etc painters'
7143 'building structure cleaners'
7200 'metal, machinery etc trades workers'
7210 'metal moulders, welders, sheetmetal workers structural metal'
7211 'metal moulders & coremakers'
7212 'welders & flamecutters'
7213 'sheet-metal workers'
7214 'structural-metal preparers & erectors'
7215 'riggers & cable splicers'
7216 'underwater workers'
7220 'blacksmiths, tool-makers etc trades workers'
7221 'blacksmiths hammer-smiths & forgingpress workers'
7222 'tool-makers etc workers'
7223 'machine-tool setters & setter-operators'
7224 'metal wheel-grinders polishers & tool sharpeners'
7230 'machinery mechanics & fitters'
7231 'motor vehicle mechanics & fitters'
7232 'aircraft engine mechanics & fitters'
7233 'agricultural- or industrial-machinery mechanics & fitters'
7240 'electrical & electronic equipment mechanics & fitters'
7241 'electrical mechanics & fitters'
7242 'electronics fitters'
7243 'electronics mechanics & servicers'
7244 'telegraph & telephone installers & servlcers'
7245 'electrical line installers repairers & cable jointers'
7300 'precision, handicraft, printing etc trades workers'
7310 'precision workers in metal etc materials'
7311 'precision-instrument makers & repairers'
7312 'musical-instrument makers & tuners'
7313 'jewellery & precious-metal workers'
7320 'potters, glass-makers etc trades workers'
7321 'abrasive wheel formers potters etc workers'
7322 'glass-makers cutters grinders & finishers'
7323 'glass engravers & etchers'
7324 'glass ceramics etc decorative painters'
7330 'handicraft workers in wood,textile, leather etc'
7331 'handicraft workers in wood etc materials'
7332 'handicraft workers in textile leather etc materials'
7340 'printing etc trades workers'
7341 'compositors typesetters etc workers'
7342 'stereotypers & electrotypers'
7343 'printing engravers & etchers'
7344 'photographic etc workers'
7345 'bookbinders etc workers'
7346 'silk-screen, block & textile printers'
7400 'other craft etc trades workers'
7410 'food processing etc trades workers'
7411 'butchers fishmongers etc food preparers'
7412 'bakers-pastry cooks & confectionery makers'
7413 'dairy-products makers'
7414 'fruit, vegetable etc preservers'
7415 'food & beverage tasters & graders'
7416 'tobacco preparers & tobacco products makers'
7420 'wood treaters, cabinet-makers etc trades workers'
7421 'wood treaters'
7422 'cabinet-makers etc workers'
7423 'woodworking-machine setters & setteroperators'
7424 'basketry weavers brush makers etc workers'
7430 'textile, garment etc trades workers'
7431 'fibre preparers'
7432 'weavers knitters etc workers'
7433 'tailors dressmakers & hatters'
7434 'furriers etc workers'
7435 'textile leather etc pattern-makers & cutters'
7436 'sewers embroiderers etc workers'
7437 'upholsterers etc workers'
7440 'pelt, leather & shoemaking trades workers'
7441 'pelt dressers tanners & fellmongers'
7442 'shoe-makers etc workers'
7500 '[skilled manual worker nfs]'
8000 'plant & machine operators & assemblers'
8100 'stationary-plant etc operators'
8110 'mining- & mineral-processing plant operators'
8111 'mining-plant operators'
8112 'mineral-ore- & stone-processing-plant operators'
8113 'well drillers & borers etc workers'
8120 'metal-processing-plant operators'
8121 'ore & metal furnace operators'
8122 'metal melters casters & rolling-mill operators'
8123 'metal-heat-treating-plant operators'
8124 'metal drawers & extruders'
8130 'glass, ceramics etc plant operators'
8131 'glass & ceramics kiln etc machine operators'
8139 'glass, ceramics etc plant operators nec'
8140 'wood-processing- & papermaking-plant operators'
8141 'wood-processing-plant operators'
8142 'paper-pulp plant operators'
8143 'papermaking-plant operators'
8150 'chemical-processing-plant operators'
8151 'crushing- grinding- & chemical-mixing machinery operators'
8152 'chemical-heat-treating-plant operators'
8153 'chemical-filtering- & separating-equipment operators'
8154 'chemical-still & reactor operators '
8155 'petroleum- & natural-gas-refining-plant operators'
8159 'chemical-processing-plant operators nec'
8160 'power-production etc plant operators'
8161 'power-production plant operators'
8162 'steam-engine & boiler operators'
8163 'incinerator water-treatment etc plant operators'
8170 'automated-assembly-line & industrial-robot operators'
8171 'automated-assembly- line operators'
8172 'industrial-robot operators'
8200 'machine operators & assemblers'
8210 'metal- & mineral-products machine operators'
8211 'machine-tool operators'
8212 'cement & other mineral products machine operators'
8220 'chemical-products machine operators'
8221 'pharmaceutical- & toiletry-products machine operators'
8222 'ammunition- & explosive-products machine operators'
8223 'metal finishing- plating- & coating-machlne operators'
8224 'photographic-products machine operators'
8229 'chemical-products machine operators nec'
8230 'rubber- & plastic-products machine operators'
8231 'rubber-products machine operators'
8232 'plastic-products machine operators'
8240 'wood-products machine operators'
8250 'printing- binding- & paper-products machine operators'
8251 'printing-machine operators'
8252 'bookbinding-machine operators'
8253 'paper-products machine operators'
8260 'textile- fur- & leather-products machine operators'
8261 'fibre-preparing-, spinning- & windingmachine operators'
8262 'weaving- & knitting-machine operators'
8263 'sewing-machine operators'
8264 'bleaching- dyeing- & cleaning-machine operators'
8265 'fur- & leather-preparing-machine operators'
8266 'shoemaking- etc machine operators'
8269 'textile- fur- & leather-products machine operators nec'
8270 'food etc products machine operators'
8271 'meat- & fish-processing-machine operators'
8272 'dairy-products machine operators'
8273 'grain- & spice-milling-machine operators'
8274 'baked-goods cereal & chocolate-products machine operators'
8275 'fruit- vegetable- & nut-processing-machine operators'
8276 'sugar production machine operators'
8277 'tea- coffee- & cocoa-processing-machine operators'
8278 'brewers- wine & other beverage machine operators'
8279 'tobacco production machine operators'
8280 'assemblers'
8281 'mechanical-machinery assemblers'
8282 'electrical-equipment assemblers'
8283 'electronic-equipment assemblers'
8284 'metal- rubber- & plastic-products assemblers'
8285 'wood etc products assemblers'
8286 'paperboard textile etc products assemblers'
8290 'other machine operators & assemblers'
8300 'drivers & mobile-plant operators'
8310 'locomotive-engine drivers etc workers'
8311 'locomotive-engine drivers'
8312 'railway brakers signallers & shunters'
8320 'motor-vehicle drivers'
8321 'motor-cycle drivers'
8322 'car taxi & van drivers'
8323 'bus & tram drivers'
8324 'heavy truck & lorry drivers'
8330 'agricultural & other mobile-plant operators'
8331 'motorised farm & forestry plant operators'
8332 'earth-moving- etc plant operators'
8333 'crane hoist etc plant operators'
8334 'lifting-truck operators'
8340 'ships deck crews etc workers'
9000 'elementary occupations'
9100 'sales & services elementary occupations'
9110 'street vendors etc workers'
9111 'street food vendors'
9112 'street vendors non-food products'
9113 'door-to-door & telephone salespersons'
9120 'street services elementary occupations'
9130 'domestic etc helpers cleaners & launderers'
9131 'domestic helpers & cleaners'
9132 'helpers & cleaners in establishments'
9133 'hand-launderers & pressers'
9140 'building caretakers, window etc cleaners'
9141 'building caretakers'
9142 'vehicle, window etc cleaners'
9150 'messengers, porters, doorkeepers etc workers'
9151 'messengers package & luggage porters & deliverers'
9152 'doorkeepers watchpersons etc workers'
9153 'vending-machine money collectors meter readers etc workers'
9160 'garbage collectors etc labourers'
9161 'garbage collectors'
9162 'sweepers etc labourers'
9200 'agricultural, fishery etc labourers'
9210 'agricultural, fishery etc labourers'
9211 'farm-hands & labourers'
9212 'forestry labourers'
9213 'fishery hunting & trapping labourers'
9300 'labourers in mining, construction, manufacturing & transport'
9310 'mining & construction labourers'
9311 'mining & quarrying labourers'
9312 'construction & maintenance labourers: roads dams etc'
9313 'building construction labourers'
9320 'manufacturing labourers'
9321 'assembling labourers'
9322 'hand packers & other manufacturing labourers'
9330 'transport labourers & freight handlers'
9331 'hand or pedal vehicle drivers'
9332 'drivers of animal-drawn vehicles & machinery'
9333 'freight handlers'
% END

% LABELLIST-isco88b
% source: http://www.harryganzeboom.nl/ismf/scaleapp.htm
% variables: ISCO88 ISCO88-label-E
1000 "LEGISLATORS, SENIOR OFFICIALS & MANAGERS"
1100 "LEGISLATORS & SENIOR OFFICIALS"
1110 "LEGISLATORS"
1120 "SENIOR [NATIONAL] GOVERNMENT OFFICIALS"
1130 "[SENIOR LOCAL GOVERNMENT OFFICIALS]"
1140 "SENIOR OFFICIALS SPECIAL-INTEREST ORGANISATIONS"
1141 "Senior officials political-party organisations"
1142 "Senior officials economic-interest organisations"
1143 "Senior officials special-interest organisations"
1200 "CORPORATE MANAGERS [LARGE ENTERPRISES]"
1210 "[LARGE ENTERPRISES] DIRECTORS & CHIEF EXECUTIVES"
1220 "[LARGE ENTERPRISE OPERATION] DEPARTMENT MANAGERS"
1221 "Production department managers agriculture & fishing"
1222 "Production department managers manufacturing"
1223 "Production department managers construction"
1224 "Production department managers wholesale & retail trade"
1225 "Production department managers restaurants & hotels"
1226 "Production department managers transp., storage & communic."
1227 "Production department managers business services"
1228 "Production department managers personal care, cleaning etc"
1229 "Production department managers nec"
1230 "[LARGE ENTERPRISES] OTHER DEPARTMENT MANAGERS"
1231 "Finance & administration department managers"
1232 "Personnel & industrial relations department managers"
1233 "Sales & marketing department managers"
1234 "Advertising & public relations department managers"
1235 "Supply & distribution department managers"
1236 "Computing services department managers"
1237 "Research & development department managers"
1239 "Other department managers nec"
1240 "OFFICE MANAGERS"
1250 "MILITARY OFFICERS"
1251 "Higher military officers"
1252 "Lower grade commissioned officers"
1300 "[SMALL ENTERPRISE] GENERAL MANAGERS"
1310 "[SMALL ENTERPRISE] GENERAL MANAGERS"
1311 "[Small enterprise] General managers agriculture, forestry & fishing"
1312 "[Small enterprise] General managers manufacturing"
1313 "[Small enterprise] General managers construction"
1314 "[Small enterprise] General managers wholesale & retail trade"
1315 "[Small enterprise] General managers restaurants & hotels"
1316 "[Small enterprise] General managers transp., storage & communications"
1317 "[Small enterprise] General managers business services"
1318 "[Small enterprise] General managers personal care, cleaning etc. services"
1319 "[Small enterprise] General managers nec"
2000 "PROFESSIONALS"
2100 "PHYSICAL, MATHEMATICAL & ENGINEERING SCIENCE PROFESSIONALS"
2110 "PHYSICISTS, CHEMISTS & RELATED PROFESSIONALS"
2111 "Physicists & astronomers"
2112 "Meteorologists"
2113 "Chemists"
2114 "Geologists & geophysicists"
2120 "MATHEMATICIANS, STATISTICIANS ETC PROFESSIONALS"
2121 "Mathematicians etc professionals"
2122 "Statisticians"
2130 "COMPUTING PROFESSIONALS"
2131 "Computer systems designers & analysts"
2132 "Computer programmers"
2139 "Computing professionals nec"
2140 "ARCHITECTS, ENGINEERS ETC PROFESSIONALS"
2141 "Architects town & traffic planners"
2142 "Civil engineers"
2143 "Electrical engineers"
2144 "Electronics & telecommunications engineers"
2145 "Mechanical engineers"
2146 "Chemical engineers"
2147 "Mining engineers, metallurgists etc professionals"
2148 "Cartographers & surveyors"
2149 "Architects engineers etc professionals nec"
2200 "LIFE SCIENCE & HEALTH PROFESSIONALS"
2210 "LIFE SCIENCE PROFESSIONALS"
2211 "Biologists, botanists zoologists etc professionals"
2212 "Pharmacologists, pathologists etc professlonals"
2213 "Agronomists etc professionals"
2220 "HEALTH PROFESSIONALS (EXCEPT NURSING)"
2221 "Medical doctors"
2222 "Dentists"
2223 "Veterinarians"
2224 "Pharmacists"
2229 "Health professionals except nursing nec"
2230 "NURSING & MIDWIFERY PROFESSIONALS"
2300 "TEACHING PROFESSIONALS"
2310 "HIGHER EDUCATION TEACHING PROFESSIONALS"
2320 "SECONDARY EDUCATION TEACHING PROFESSIONALS"
2321 "[Secondary teachers, academic track]"
2322 "[Secondary teachers, vocational track]"
2330 "PRIMARY & PRE-PRIMARY EDUCATION TEACHING PROFESSIONALS"
2331 "Primary education teaching professionals"
2332 "Pre-primary education teaching professionals"
2340 "SPECIAL EDUCATION TEACHING PROFESSIONALS"
2350 "OTHER TEACHING PROFESSIONALS"
2351 "Education methods specialists"
2352 "School inspectors"
2359 "Other teaching professionals nec"
2400 "OTHER PROFESSIONALS"
2410 "BUSINESS PROFESSIONALS"
2411 "Accountants"
2412 "Personnel & careers professionals"
2419 "Business professionals nec"
2420 "LEGAL PROFESSIONALS"
2421 "Lawyers"
2422 "Judges"
2429 "Legal professionals nec"
2430 "ARCHIVISTS, LIBRARIANS ETC INFORMATION PROFESSIONALS"
2431 "Archivists & curators"
2432 "Librarians etc information professionals"
2440 "SOCIAL SCIENCE ETC PROFESSIONALS"
2441 "Economists"
2442 "Sociologists, anthropologists etc professionals"
2443 "Philosophers, historians & political scientists"
2444 "Philologists, translators & interpreters"
2445 "Psychologists"
2446 "Social work professionals"
2450 "WRITERS & CREATIVE OR PERFORMING ARTISTS"
2451 "Authors journalists & other writers"
2452 "Sculptors, painters etc artists"
2453 "Composers, musicians & singers"
2454 "Choreographers & dancers"
2455 "Film, stage etc actors & directors"
2460 "RELIGIOUS PROFESSIONALS"
3000 "TECHNICIANS AND ASSOCIATE PROFESSIONALS"
3100 "PHYSICAL & ENGINEERING SCIENCE ASSOCIATE PROFESSIONALS"
3110 "PHYSICAL & ENGINEERING SCIENCE TECHNICIANS"
3111 "Chemical & physical science technicians"
3112 "Civil engineering technicians"
3113 "Electrical engineering technicians"
3114 "Electronics & telecommunications engineering technicians"
3115 "Mechanical engineering technicians"
3116 "Chemical engineering technicians"
3117 "Mining & metallurgical technicians"
3118 "Draughtspersons"
3119 "Physical & engineering science technicians nec"
3120 "COMPUTER ASSOCIATE PROFESSIONALS"
3121 "Computer assistants"
3122 "Computer equipment operators"
3123 "Industrial robot controllers"
3130 "OPTICAL & ELECTRONIC EQUIPMENT OPERATORS"
3131 "Photographers & electronic equipment operators"
3132 "Broadcasting & telecommunications equipment operators"
3133 "Medical equipment operators"
3139 "Optical & electronic equipment operators nec"
3140 "SHIP & AIRCRAFT CONTROLLERS & TECHNICIANS"
3141 "Ships engineers"
3142 "Ships deck officers & pilots"
3143 "Aircraft pilots etc associate professionals"
3144 "Air traffic controllers"
3145 "Air traffic safety technicians"
3150 "SAFETY & QUALITY INSPECTORS"
3151 "Building & fire inspectors"
3152 "Safety, health & quality inspectors"
3200 "LIFE SCIENCE & HEALTH ASSOCIATE PROFESSIONALS"
3210 "LIFE SCIENCE TECHNICIANS ETC ASSOCIATE PROFESSIONALS"
3211 "Life science technicians"
3212 "Agronomy & forestry technicians"
3213 "Farming & forestry advisers"
3220 "MODERN HEALTH ASSOCIATE PROFESSIONALS EXCEPT NURSING"
3221 "Medical assistants"
3222 "Sanitarians"
3223 "Dieticians & nutritionists"
3224 "Optometrists & opticians"
3225 "Dental assistants"
3226 "Physiotherapists etc associate professionals"
3227 "Veterinary assistants"
3228 "Pharmaceutical assistants"
3229 "Modern health associate professionals except nursing nec"
3230 "NURSING & MIDWIFERY ASSOCIATE PROFESSIONALS"
3231 "Nursing associate professionals"
3232 "Midwifery associate professionals"
3240 "TRADITIONAL MEDICINE PRACTITIONERS & FAITH HEALERS"
3241 "Traditional medicine practitioners"
3242 "Faith healers"
3300 "TEACHING ASSOCIATE PROFESSIONALS"
3310 "PRIMARY EDUCATION TEACHING ASSOCIATE PROFESSIONALS"
3320 "PRE-PRIMARY EDUCATION TEACHING ASSOCIATE PROFESSIONALS"
3330 "SPECIAL EDUCATION TEACHING ASSOCIATE PROFESSIONALS"
3340 "OTHER TEACHING ASSOCIATE PROFESSIONALS"
3400 "OTHER ASSOCIATE PROFESSIONALS"
3410 "FINANCE & SALES ASSOCIATE PROFESSIONALS"
3411 "Securities & finance dealers & brokers"
3412 "Insurance representatives"
3413 "[Real] estate agents"
3414 "Travel consultants & organisers"
3415 "Technical & commercial sales representatives"
3416 "Buyers"
3417 "Appraisers, valuers & auctioneers"
3419 "Finance & sales associate professionals nec"
3420 "BUSINESS SERVICES AGENTS AND TRADE BROKERS"
3421 "Trade brokers"
3422 "Clearing & forwarding agents"
3423 "Employment agents & labour contractors"
3429 "Business services agents & trade brokers nec"
3430 "ADMINISTRATIVE ASSOCIATE PROFESSIONALS"
3431 "Administrative secretaries etc associate professionals"
3432 "Legal etc business associate professlonals"
3433 "Bookkeepers"
3434 "Statistical, mathematical etc associate professionals"
3439 "Administrative associate professionals nec"
3440 "CUSTOMS, TAX ETC GOVERNMENT ASSOCIATE PROFESSIONALS"
3441 "Customs & border inspectors"
3442 "Government tax & excise officials"
3443 "Government social benefits officials"
3444 "Government licensing officials"
3449 "Customs tax etc government associate professionals nec"
3450 "POLICE INSPECTORS & DETECTIVES / [ARMY]"
3451 "Police inspectors & detectives"
3452 "[Armed forces non-commissioned officers]"
3460 "SOCIAL WORK ASSOCIATE PROFESSIONALS"
3470 "ARTISTIC, ENTERTAINMENT & SPORTS ASSOCIATE PROFESSIONALS"
3471 "Decorators & commercial designers"
3472 "Radio, television & other announcers"
3473 "Street night-club etc musicians, singers & dancers"
3474 "Clowns, magicians, acrobats etc associate professionals"
3475 "Athletes, sportspersons etc associate professionals"
3480 "RELIGIOUS ASSOCIATE PROFESSIONALS"
4000 "CLERKS"
4100 "OFFICE CLERKS"
4110 "SECRETARIES & KEYBOARD-OPERATING CLERKS"
4111 "Stenographers & typists"
4112 "Word-processor etc operators"
4113 "Data entry operators"
4114 "Calculating-machine operators"
4115 "Secretaries"
4120 "NUMERICAL CLERKS"
4121 "Accounting & bookkeeping clerks"
4122 "Statistical & finance clerks"
4130 "MATERIAL-RECORDING & TRANSPORT CLERKS"
4131 "Stock clerks"
4132 "Production clerks"
4133 "Transport clerks"
4140 "LIBRARY, MAIL ETC CLERKS"
4141 "Library & filing clerks"
4142 "Mail carriers & sorting clerks"
4143 "Coding proof-reading etc clerks"
4144 "Scribes etc workers"
4190 "OTHER OFFICE CLERKS"
4200 "CUSTOMER SERVICES CLERKS"
4210 "CASHIERS, TELLERS ETC CLERKS"
4211 "Cashiers & ticket clerks"
4212 "Tellers & other counter clerks"
4213 "Bookmakers & croupiers"
4214 "Pawnbrokers & money-lenders"
4215 "Debt-collectors etc workers"
4220 "CLIENT INFORMATION CLERKS"
4221 "Travel agency etc clerks"
4222 "Receptionists & information clerks"
4223 "Telephone switchboard operators"
5000 "SERVICE WORKERS & SHOP & MARKET SALES WORKERS"
5100 "PERSONAL & PROTECTIVE SERVICES WORKERS"
5110 "TRAVEL ATTENDANTS ETC"
5111 "Travel attendants & travel stewards"
5112 "Transport conductors"
5113 "Travel, museum guides"
5120 "HOUSEKEEPING & RESTAURANT SERVICES WORKERS"
5121 "Housekeepers etc workers"
5122 "Cooks"
5123 "Waiters, waitresses & bartenders"
5130 "PERSONAL CARE ETC WORK"
5131 "Child-care workers"
5132 "Institution-based personal care workers"
5133 "Home based personal care workers"
5139 "[Other] care etc workers nec"
5140 "OTHER PERSONAL SERVICES WORKERS"
5141 "Hairdressers, barbers, beauticians etc workers"
5142 "Companions & valets"
5143 "Undertakers & embalmers"
5149 "Other personal services workers nec"
5150 "ASTROLOGERS, FORTUNE-TELLERS ETC WORKERS"
5151 "Astrologers etc workers"
5152 "Fortune-tellers, palmists etc workers"
5160 "PROTECTIVE SERVICES WORKERS"
5161 "Fire-fighters"
5162 "Police officers"
5163 "Prison guards"
5164 "[Armed forces, soldiers]"
5169 "Protective services workers nec"
5200 "[SALESPERSONS, MODELS & DEMONSTRATORS]"
5210 "FASHION & OTHER MODELS"
5220 "SHOP SALESPERSONS & DEMONSTRATORS"
5230 "STALL & MARKET SALESPERSONS"
6000 "SKILLED AGRICULTURAL & FISHERY WORKERS"
6100 "MARKET-ORIENTED SKILLED AGRICULTURAL & FISHERY WORKERS"
6110 "MARKET GARDENERS & CROPGROWERS"
6111 "Field crop & vegetable growers"
6112 "Tree & shrub crop growers"
6113 "Gardeners, horticultural & nursery growers"
6114 "Mixed-crop growers"
6120 "MARKET-ORIENTED ANIMAL PRODUCERS ETC WORKERS"
6121 "Dairy & livestock producers"
6122 "Poultry producers"
6123 "Apiarists & sericulturists"
6124 "Mixed-animal producers"
6129 "Market-oriented animal producers etc workers nec"
6130 "MARKET-ORIENTED CROP & ANIMAL PRODUCERS"
6131 "[Mixed farmers]"
6132 "[Farm foremen/supervisor]"
6133 "[Farmers nfs]"
6134 "[Skilled farm workers nfs]"
6140 "FORESTRY ETC WORKERS"
6141 "Forestry workers & loggers"
6142 "Charcoal burners etc workers"
6150 "FISHERY WORKERS, HUNTERS & TRAPPERS"
6151 "Aquatic-life cultivation workers"
6152 "Inland & coastal waters fishery workers"
6153 "Deep-sea fishery workers"
6154 "Hunters & trappers"
6200 "SUBSISTENCE AGRICULTURAL & FISHERY WORKERS"
6210 "SUBSISTENCE AGRICULTURAL & FISHERY WORKERS"
7000 "CRAFT ETC TRADES WORKERS"
7100 "EXTRACTION & BUILDING TRADES WORKERS"
7110 "MINERS, SHOTFIRERS, STONE CUTTERS & CARVERS"
7111 "Miners & quarry workers"
7112 "Shotfirers & blasters"
7113 "Stone splitters, cutters & carvers"
7120 "BUILDING FRAME ETC TRADES WORKERS"
7121 "Builders traditional materials"
7122 "Bricklayers & stonemasons"
7123 "Concrete placers, concrete finishers etc workers"
7124 "Carpenters & joiners"
7129 "Building frame etc trades workers nec"
7130 "BUILDING FINISHERS ETC TRADES WORKERS"
7131 "Roofers"
7132 "Floor layers & tile setters"
7133 "Plasterers"
7134 "Insulation workers"
7135 "Glaziers"
7136 "Plumbers & pipe fitters"
7137 "Building etc electricians"
7140 "PAINTERS, BUILDING STRUCTURE CLEANERS ETC TRADES WORKERS"
7141 "Painters etc workers"
7142 "Varnishers etc painters"
7143 "Building structure cleaners"
7200 "METAL, MACHINERY ETC TRADES WORKERS"
7210 "METAL MOULDERS, WELDERS, SHEETMETAL WORKERS STRUCTURAL METAL"
7211 "Metal moulders & coremakers"
7212 "Welders & flamecutters"
7213 "Sheet-metal workers"
7214 "Structural-metal preparers & erectors"
7215 "Riggers & cable splicers"
7216 "Underwater workers"
7220 "BLACKSMITHS, TOOL-MAKERS ETC TRADES WORKERS"
7221 "Blacksmiths, hammer-smiths & forging press workers"
7222 "Tool-makers etc workers"
7223 "Machine-tool setters & setter-operators"
7224 "Metal wheel-grinders, polishers & tool sharpeners"
7230 "MACHINERY MECHANICS & FITTERS"
7231 "Motor vehicle mechanics & fitters"
7232 "Aircraft engine mechanics & fitters"
7233 "[Industrial & agricultural] machinery mechanics & fitters"
7234 "[Unskilled garage worker]"
7240 "ELECTRICAL & ELECTRONIC EQUIPMENT MECHANICS & FITTERS"
7241 "Electrical mechanics & fitters"
7242 "Electronics fitters"
7243 "Electronics mechanics & servicers"
7244 "Telegraph & telephone installers & servicers"
7245 "Electrical line installers, repairers & cable jointers"
7300 "PRECISION, HANDICRAFT, PRINTING ETC TRADES WORKERS"
7310 "PRECISION WORKERS IN METAL ETC MATERIALS"
7311 "Precision-instrument makers & repairers"
7312 "Musical-instrument makers & tuners"
7313 "Jewellery & precious-metal workers"
7320 "POTTERS, GLASS-MAKERS ETC TRADES WORKERS"
7321 "Abrasive wheel formers, potters etc workers"
7322 "Glass-makers, cutters, grinders & finishers"
7323 "Glass engravers & etchers"
7324 "Glass ceramics etc decorative painters"
7330 "HANDICRAFT WORKERS IN WOOD,TEXTILE, LEATHER ETC"
7331 "Handicraft workers in wood etc materials"
7332 "Handicraft workers in textile leather etc materials"
7340 "PRINTING ETC TRADES WORKERS"
7341 "Compositors typesetters etc workers"
7342 "Stereotypers & electrotypers"
7343 "Printing engravers & etchers"
7344 "Photographic etc workers"
7345 "Bookbinders etc workers"
7346 "Silk-screen, block & textile printers"
7400 "OTHER CRAFT ETC TRADES WORKERS"
7410 "FOOD PROCESSING ETC TRADES WORKERS"
7411 "Butchers, fishmongers etc food preparers"
7412 "Bakers, pastry-cooks & confectionery makers"
7413 "Dairy-products makers"
7414 "Fruit, vegetable etc preservers"
7415 "Food & beverage tasters & graders"
7416 "Tobacco preparers & tobacco products makers"
7420 "WOOD TREATERS, CABINET-MAKERS ETC TRADES WORKERS"
7421 "Wood treaters"
7422 "Cabinet-makers etc workers"
7423 "Woodworking-machine setters & setter-operators"
7424 "Basketry weavers, brush makers etc workers"
7430 "TEXTILE, GARMENT ETC TRADES WORKERS"
7431 "Fibre preparers"
7432 "Weavers, knitters etc workers"
7433 "Tailors, dressmakers & hatters"
7434 "Furriers etc workers"
7435 "Textile, leather etc pattern-makers & cutters"
7436 "Sewers, embroiderers etc workers"
7437 "Upholsterers etc workers"
7440 "PELT, LEATHER & SHOEMAKING TRADES WORKERS"
7441 "Pelt dressers, tanners & fellmongers"
7442 "Shoe-makers etc workers"
7500 "[SKILLED WORKERS NFS]"
7510 "[MANUAL FOREMEN NFS --NON-FARM--]"
7520 "[SKILLED WORKERS NFS]"
7530 "[APPRENTICE SKILLED WORK NFS]"
8000 "PLANT & MACHINE OPERATORS & ASSEMBLERS"
8100 "STATIONARY-PLANT ETC OPERATORS"
8110 "MINING- & MINERAL-PROCESSING PLANT OPERATORS"
8111 "Mining-plant operators"
8112 "Mineral-ore- & stone-processing-plant operators"
8113 "Well drillers & borers etc workers"
8120 "METAL-PROCESSING-PLANT OPERATORS"
8121 "Ore & metal furnace operators"
8122 "Metal melters, casters & rolling-mill operators"
8123 "Metal-heat-treating-plant operators"
8124 "Metal drawers & extruders"
8130 "GLASS, CERAMICS ETC PLANT OPERATORS"
8131 "Glass & ceramics kiln etc machine operators"
8139 "Glass, ceramics etc plant operators nec"
8140 "WOOD-PROCESSING- & PAPERMAKING-PLANT OPERATORS"
8141 "Wood-processing-plant operators"
8142 "Paper-pulp plant operators"
8143 "Papermaking-plant operators"
8150 "CHEMICAL-PROCESSING-PLANT OPERATORS"
8151 "Crushing- grinding- & chemical-mixing machinery operators"
8152 "Chemical-heat-treating-plant operators"
8153 "Chemical-filtering- & separating-equipment operators"
8154 "Chemical-still & reactor operators"
8155 "Petroleum- & natural-gas-refining-plant operators"
8159 "Chemical-processing-plant operators nec"
8160 "POWER-PRODUCTION ETC PLANT OPERATORS"
8161 "Power-production plant operators"
8162 "Steam-engine & boiler operators"
8163 "Incinerator water-treatment etc plant operators"
8170 "AUTOMATED-ASSEMBLY-LINE & INDUSTRIAL-ROBOT OPERATORS"
8171 "Automated-assembly-line operators"
8172 "Industrial-robot operators"
8200 "MACHINE OPERATORS & ASSEMBLERS"
8210 "METAL- & MINERAL-PRODUCTS MACHINE OPERATORS"
8211 "Machine-tool operators"
8212 "Cement & other mineral products machine operators"
8220 "CHEMICAL-PRODUCTS MACHINE OPERATORS"
8221 "Pharmaceutical- & toiletry-products machine operators"
8222 "Ammunition- & explosive-products machine operators"
8223 "Metal finishing- plating- & coating-machine operators"
8224 "Photographic-products machine operators"
8229 "Chemical-products machine operators nec"
8230 "RUBBER- & PLASTIC-PRODUCTS MACHINE OPERATORS"
8231 "Rubber-products machine operators"
8232 "Plastic-products machine operators"
8240 "WOOD-PRODUCTS MACHINE OPERATORS"
8250 "PRINTING-, BINDING- & PAPER-PRODUCTS MACHINE OPERATORS"
8251 "Printing-machine operators"
8252 "Bookbinding-machine operators"
8253 "Paper-products machine operators"
8260 "TEXTILE-, FUR- & LEATHER-PRODUCTS MACHINE OPERATORS"
8261 "Fibre-preparing-, spinning- & winding machine operators"
8262 "Weaving- & knitting-machine operators"
8263 "Sewing-machine operators"
8264 "Bleaching-, dyeing- & cleaning-machine operators"
8265 "Fur- & leather-preparing-machine operators"
8266 "Shoemaking- etc machine operators"
8269 "Textile-, fur- & leather-products machine operators nec"
8270 "FOOD ETC PRODUCTS MACHINE OPERATORS"
8271 "Meat- & fish-processing-machine operators"
8272 "Dairy-products machine operators"
8273 "Grain- & spice-milling-machine operators"
8274 "Baked-goods cereal & chocolate-products machine operators"
8275 "Fruit-, vegetable- & nut-processing-machine operators"
8276 "Sugar production machine operators"
8277 "Tea-, coffee- & cocoa-processing-machine operators"
8278 "Brewers- wine & other beverage machine operators"
8279 "Tobacco production machine operators"
8280 "ASSEMBLERS"
8281 "Mechanical-machinery assemblers"
8282 "Electrical-equipment assemblers"
8283 "Electronic-equipment assemblers"
8284 "Metal-, rubber- & plastic-products assemblers"
8285 "Wood etc products assemblers"
8286 "Paperboard, textile etc products assemblers"
8290 "OTHER MACHINE OPERATORS & ASSEMBLERS"
8300 "DRIVERS & MOBILE-PLANT OPERATORS"
8310 "LOCOMOTIVE-ENGINE DRIVERS ETC WORKERS"
8311 "Locomotive-engine drivers"
8312 "Railway brakers signallers & shunters"
8320 "MOTOR-VEHICLE DRIVERS"
8321 "Motor-cycle drivers"
8322 "Car, taxi & van drivers"
8323 "Bus & tram drivers"
8324 "Heavy truck & lorry drivers"
8330 "AGRICULTURAL & OTHER MOBILE PLANT OPERATORS"
8331 "Motorised farm & forestry plant operators"
8332 "Earth-moving- etc plant operators"
8333 "Crane, hoist etc plant operators"
8334 "Lifting-truck operators"
8340 "SHIPS DECK CREWS ETC WORKERS"
8400 "SEMI-SKILLED WORKERS NFS"
9000 "ELEMENTARY OCCUPATIONS"
9100 "SALES & SERVICES ELEMENTARY OCCUPATIONS"
9110 "STREET VENDORS ETC WORKERS"
9111 "Street food vendors"
9112 "Street vendors non-food products"
9113 "Door-to-door & telephone salespersons"
9120 "STREET SERVICES ELEMENTARY OCCUPATIONS"
9130 "DOMESTIC ETC HELPERS CLEANERS & LAUNDERERS"
9131 "Domestic helpers & cleaners"
9132 "Helpers & cleaners in establishments"
9133 "Hand-launderers & pressers"
9140 "BUILDING CARETAKERS, WINDOW ETC CLEANERS"
9141 "Building caretakers"
9142 "Vehicle, window etc cleaners"
9150 "MESSENGERS, PORTERS, DOORKEEPERS ETC WORKERS"
9151 "Messengers, package & luggage porters & deliverers"
9152 "Doorkeepers, watchpersons etc workers"
9153 "Vending-machine money collectors, meter readers etc workers"
9160 "GARBAGE COLLECTORS ETC LABOURERS"
9161 "Garbage collectors"
9162 "Sweepers etc labourers"
9200 "AGRICULTURAL, FISHERY ETC LABOURERS"
9210 "AGRICULTURAL, FISHERY ETC LABOURERS"
9211 "Farm-hands & labourers"
9212 "Forestry labourers"
9213 "Fishery, hunting & trapping labourers"
9300 "LABOURERS IN MINING, CONSTRUCTION, MANUFACTURING & TRANSPORT"
9310 "MINING & CONSTRUCTION LABOURERS"
9311 "Mining & quarrying labourers"
9312 "Construction & maintenance labourers: roads dams etc"
9313 "Building construction labourers"
9320 "MANUFACTURING LABOURERS"
9321 "Assembling labourers"
9322 "Handpackers & other manufacturing labourers"
9330 "TRANSPORT LABOURERS & FREIGHT HANDLERS"
9331 "Hand or pedal vehicle drivers"
9332 "Drivers of animal-drawn vehicles & machinery"
9333 "Freight handlers"
% END

% LABELLIST-isco08
% source: structure08.docx from https://www.ilo.org/public/english/bureau/stat/isco/isco08/index.htm
% note: source uses titlecase spelling; this has been transformed to lowercase spelling
% variables: ISCO08 ISCO08-label-E
1000 "Managers"
1100 "Chief executives, senior officials and legislators"
1110 "Legislators and senior officials"
1111 "Legislators"
1112 "Senior government officials"
1113 "Traditional chiefs and heads of villages"
1114 "Senior officials of special-interest organizations"
1120 "Managing directors and chief executives"
1200 "Administrative and commercial managers"
1210 "Business services and administration managers"
1211 "Finance managers"
1212 "Human resource managers"
1213 "Policy and planning managers"
1219 "Business services and administration managers not elsewhere classified"
1220 "Sales, marketing and development managers"
1221 "Sales and marketing managers"
1222 "Advertising and public relations managers"
1223 "Research and development managers"
1300 "Production and specialized services managers"
1310 "Production managers in agriculture, forestry and fisheries"
1311 "Agricultural and forestry production managers"
1312 "Aquaculture and fisheries production managers"
1320 "Manufacturing, mining, construction and distribution managers"
1321 "Manufacturing managers"
1322 "Mining managers"
1323 "Construction managers"
1324 "Supply, distribution and related managers"
1330 "Information and communications technology services managers"
1340 "Professional services managers"
1341 "Child care services managers"
1342 "Health services managers"
1343 "Aged care services managers"
1344 "Social welfare managers"
1345 "Education managers"
1346 "Financial and insurance services branch managers"
1349 "Professional services managers not elsewhere classified"
1400 "Hospitality, retail and other services managers"
1410 "Hotel and restaurant managers"
1411 "Hotel managers"
1412 "Restaurant managers"
1420 "Retail and wholesale trade managers"
1430 "Other services managers"
1431 "Sports, recreation and cultural centre managers"
1439 "Services managers not elsewhere classified"
2000 "Professionals"
2100 "Science and engineering professionals"
2110 "Physical and earth science professionals"
2111 "Physicists and astronomers"
2112 "Meteorologists"
2113 "Chemists"
2114 "Geologists and geophysicists"
2120 "Mathematicians, actuaries and statisticians"
2130 "Life science professionals"
2131 "Biologists, botanists, zoologists and related professionals"
2132 "Farming, forestry and fisheries advisers"
2133 "Environmental protection professionals"
2140 "Engineering professionals (excluding electrotechnology)"
2141 "Industrial and production engineers"
2142 "Civil engineers"
2143 "Environmental engineers"
2144 "Mechanical engineers"
2145 "Chemical engineers"
2146 "Mining engineers, metallurgists and related professionals"
2149 "Engineering professionals not elsewhere classified"
2150 "Electrotechnology engineers"
2151 "Electrical engineers"
2152 "Electronics engineers"
2153 "Telecommunications engineers"
2160 "Architects, planners, surveyors and designers"
2161 "Building architects"
2162 "Landscape architects"
2163 "Product and garment designers"
2164 "Town and traffic planners"
2165 "Cartographers and surveyors"
2166 "Graphic and multimedia designers"
2200 "Health professionals"
2210 "Medical doctors"
2211 "Generalist medical practitioners"
2212 "Specialist medical practitioners"
2220 "Nursing and midwifery professionals"
2221 "Nursing professionals"
2222 "Midwifery professionals"
2230 "Traditional and complementary medicine professionals"
2240 "Paramedical practitioners"
2250 "Veterinarians"
2260 "Other health professionals"
2261 "Dentists"
2262 "Pharmacists"
2263 "Environmental and occupational health and hygiene professionals"
2264 "Physiotherapists"
2265 "Dieticians and nutritionists"
2266 "Audiologists and speech therapists"
2267 "Optometrists and ophthalmic opticians"
2269 "Health professionals not elsewhere classified"
2300 "Teaching professionals"
2310 "University and higher education teachers"
2320 "Vocational education teachers"
2330 "Secondary education teachers"
2340 "Primary school and early childhood teachers"
2341 "Primary school teachers"
2342 "Early childhood educators"
2350 "Other teaching professionals"
2351 "Education methods specialists"
2352 "Special needs teachers"
2353 "Other language teachers"
2354 "Other music teachers"
2355 "Other arts teachers"
2356 "Information technology trainers"
2359 "Teaching professionals not elsewhere classified"
2400 "Business and administration professionals"
2410 "Finance professionals"
2411 "Accountants"
2412 "Financial and investment advisers"
2413 "Financial analysts"
2420 "Administration professionals"
2421 "Management and organization analysts"
2422 "Policy administration professionals"
2423 "Personnel and careers professionals"
2424 "Training and staff development professionals"
2430 "Sales, marketing and public relations professionals"
2431 "Advertising and marketing professionals"
2432 "Public relations professionals"
2433 "Technical and medical sales professionals (excluding ict)"
2434 "Information and communications technology sales professionals"
2500 "Information and communications technology professionals"
2510 "Software and applications developers and analysts"
2511 "Systems analysts"
2512 "Software developers"
2513 "Web and multimedia developers"
2514 "Applications programmers"
2519 "Software and applications developers and analysts not elsewhere classified"
2520 "Database and network professionals"
2521 "Database designers and administrators"
2522 "Systems administrators"
2523 "Computer network professionals"
2529 "Database and network professionals not elsewhere classified"
2600 "Legal, social and cultural professionals"
2610 "Legal professionals"
2611 "Lawyers"
2612 "Judges"
2619 "Legal professionals not elsewhere classified"
2620 "Librarians, archivists and curators"
2621 "Archivists and curators"
2622 "Librarians and related information professionals"
2630 "Social and religious professionals"
2631 "Economists"
2632 "Sociologists, anthropologists and related professionals"
2633 "Philosophers, historians and political scientists"
2634 "Psychologists"
2635 "Social work and counselling professionals"
2636 "Religious professionals"
2640 "Authors, journalists and linguists"
2641 "Authors and related writers"
2642 "Journalists"
2643 "Translators, interpreters and other linguists"
2650 "Creative and performing artists"
2651 "Visual artists"
2652 "Musicians, singers and composers"
2653 "Dancers and choreographers"
2654 "Film, stage and related directors and producers"
2655 "Actors"
2656 "Announcers on radio, television and other media"
2659 "Creative and performing artists not elsewhere classified"
3000 "Technicians and associate professionals"
3100 "Science and engineering associate professionals"
3110 "Physical and engineering science technicians"
3111 "Chemical and physical science technicians"
3112 "Civil engineering technicians"
3113 "Electrical engineering technicians"
3114 "Electronics engineering technicians"
3115 "Mechanical engineering technicians"
3116 "Chemical engineering technicians"
3117 "Mining and metallurgical technicians"
3118 "Draughtspersons"
3119 "Physical and engineering science technicians not elsewhere classified"
3120 "Mining, manufacturing and construction supervisors"
3121 "Mining supervisors"
3122 "Manufacturing supervisors"
3123 "Construction supervisors"
3130 "Process control technicians"
3131 "Power production plant operators"
3132 "Incinerator and water treatment plant operators"
3133 "Chemical processing plant controllers"
3134 "Petroleum and natural gas refining plant operators"
3135 "Metal production process controllers"
3139 "Process control technicians not elsewhere classified"
3140 "Life science technicians and related associate professionals"
3141 "Life science technicians (excluding medical)"
3142 "Agricultural technicians"
3143 "Forestry technicians"
3150 "Ship and aircraft controllers and technicians"
3151 "Ships’ engineers"
3152 "Ships’ deck officers and pilots"
3153 "Aircraft pilots and related associate professionals"
3154 "Air traffic controllers"
3155 "Air traffic safety electronics technicians"
3200 "Health associate professionals"
3210 "Medical and pharmaceutical technicians"
3211 "Medical imaging and therapeutic equipment technicians"
3212 "Medical and pathology laboratory technicians"
3213 "Pharmaceutical technicians and assistants"
3214 "Medical and dental prosthetic technicians"
3220 "Nursing and midwifery associate professionals"
3221 "Nursing associate professionals"
3222 "Midwifery associate professionals"
3230 "Traditional and complementary medicine associate professionals"
3240 "Veterinary technicians and assistants"
3250 "Other health associate professionals"
3251 "Dental assistants and therapists"
3252 "Medical records and health information technicians"
3253 "Community health workers"
3254 "Dispensing opticians"
3255 "Physiotherapy technicians and assistants"
3256 "Medical assistants"
3257 "Environmental and occupational health inspectors and associates"
3258 "Ambulance workers"
3259 "Health associate professionals not elsewhere classified"
3300 "Business and administration associate professionals"
3310 "Financial and mathematical associate professionals"
3311 "Securities and finance dealers and brokers"
3312 "Credit and loans officers"
3313 "Accounting associate professionals"
3314 "Statistical, mathematical and related associate professionals"
3315 "Valuers and loss assessors"
3320 "Sales and purchasing agents and brokers"
3321 "Insurance representatives"
3322 "Commercial sales representatives"
3323 "Buyers"
3324 "Trade brokers"
3330 "Business services agents"
3331 "Clearing and forwarding agents"
3332 "Conference and event planners"
3333 "Employment agents and contractors"
3334 "Real estate agents and property managers"
3339 "Business services agents not elsewhere classified"
3340 "Administrative and specialized secretaries"
3341 "Office supervisors"
3342 "Legal secretaries"
3343 "Administrative and executive secretaries"
3344 "Medical secretaries"
3350 "Government regulatory associate professionals"
3351 "Customs and border inspectors"
3352 "Government tax and excise officials"
3353 "Government social benefits officials"
3354 "Government licensing officials"
3355 "Police inspectors and detectives"
3359 "Government regulatory associate professionals not elsewhere classified"
3400 "Legal, social, cultural and related associate professionals"
3410 "Legal, social and religious associate professionals"
3411 "Legal and related associate professionals"
3412 "Social work associate professionals"
3413 "Religious associate professionals"
3420 "Sports and fitness workers"
3421 "Athletes and sports players"
3422 "Sports coaches, instructors and officials"
3423 "Fitness and recreation instructors and programme leaders"
3430 "Artistic, cultural and culinary associate professionals"
3431 "Photographers"
3432 "Interior designers and decorators"
3433 "Gallery, museum and library technicians"
3434 "Chefs"
3435 "Other artistic and cultural associate professionals"
3500 "Information and communications technicians"
3510 "Information and communications technology operations and user support technicians"
3511 "Information and communications technology operations technicians"
3512 "Information and communications technology user support technicians"
3513 "Computer network and systems technicians"
3514 "Web technicians"
3520 "Telecommunications and broadcasting technicians"
3521 "Broadcasting and audiovisual technicians"
3522 "Telecommunications engineering technicians"
4000 "Clerical support workers"
4100 "General and keyboard clerks"
4110 "General office clerks"
4120 "Secretaries (general)"
4130 "Keyboard operators"
4131 "Typists and word processing operators"
4132 "Data entry clerks"
4200 "Customer services clerks"
4210 "Tellers, money collectors and related clerks"
4211 "Bank tellers and related clerks"
4212 "Bookmakers, croupiers and related gaming workers"
4213 "Pawnbrokers and money-lenders"
4214 "Debt collectors and related workers"
4220 "Client information workers"
4221 "Travel consultants and clerks"
4222 "Contact centre information clerks"
4223 "Telephone switchboard operators"
4224 "Hotel receptionists"
4225 "Inquiry clerks"
4226 "Receptionists (general)"
4227 "Survey and market research interviewers"
4229 "Client information workers not elsewhere classified"
4300 "Numerical and material recording clerks"
4310 "Numerical clerks"
4311 "Accounting and bookkeeping clerks"
4312 "Statistical, finance and insurance clerks"
4313 "Payroll clerks"
4320 "Material recording and transport clerks"
4321 "Stock clerks"
4322 "Production clerks"
4323 "Transport clerks"
4400 "Other clerical support workers"
4410 "Other clerical support workers"
4411 "Library clerks"
4412 "Mail carriers and sorting clerks"
4413 "Coding, proofreading and related clerks"
4414 "Scribes and related workers"
4415 "Filing and copying clerks"
4416 "Personnel clerks"
4419 "Clerical support workers not elsewhere classified"
5000 "Services and sales workers"
5100 "Personal services workers"
5110 "Travel attendants, conductors and guides"
5111 "Travel attendants and travel stewards"
5112 "Transport conductors"
5113 "Travel guides"
5120 "Cooks"
5130 "Waiters and bartenders"
5131 "Waiters"
5132 "Bartenders"
5140 "Hairdressers, beauticians and related workers"
5141 "Hairdressers"
5142 "Beauticians and related workers"
5150 "Building and housekeeping supervisors"
5151 "Cleaning and housekeeping supervisors in offices, hotels and other establishments"
5152 "Domestic housekeepers"
5153 "Building caretakers"
5160 "Other personal services workers"
5161 "Astrologers, fortune-tellers and related workers"
5162 "Companions and valets"
5163 "Undertakers and embalmers"
5164 "Pet groomers and animal care workers"
5165 "Driving instructors"
5169 "Personal services workers not elsewhere classified"
5200 "Sales workers"
5210 "Street and market salespersons"
5211 "Stall and market salespersons"
5212 "Street food salespersons"
5220 "Shop salespersons"
5221 "Shopkeepers"
5222 "Shop supervisors"
5223 "Shop sales assistants"
5230 "Cashiers and ticket clerks"
5240 "Other sales workers"
5241 "Fashion and other models"
5242 "Sales demonstrators"
5243 "Door-to-door salespersons"
5244 "Contact centre salespersons"
5245 "Service station attendants"
5246 "Food service counter attendants"
5249 "Sales workers not elsewhere classified"
5300 "Personal care workers"
5310 "Child care workers and teachers’ aides"
5311 "Child care workers"
5312 "Teachers’ aides"
5320 "Personal care workers in health services"
5321 "Health care assistants"
5322 "Home-based personal care workers"
5329 "Personal care workers in health services not elsewhere classified"
5400 "Protective services workers"
5410 "Protective services workers"
5411 "Firefighters"
5412 "Police officers"
5413 "Prison guards"
5414 "Security guards"
5419 "Protective services workers not elsewhere classified"
6000 "Skilled agricultural, forestry and fishery workers"
6100 "Market-oriented skilled agricultural workers"
6110 "Market gardeners and crop growers"
6111 "Field crop and vegetable growers"
6112 "Tree and shrub crop growers"
6113 "Gardeners; horticultural and nursery growers"
6114 "Mixed crop growers"
6120 "Animal producers"
6121 "Livestock and dairy producers"
6122 "Poultry producers"
6123 "Apiarists and sericulturists"
6129 "Animal producers not elsewhere classified"
6130 "Mixed crop and animal producers"
6200 "Market-oriented skilled forestry, fishery and hunting workers"
6210 "Forestry and related workers"
6220 "Fishery workers, hunters and trappers"
6221 "Aquaculture workers"
6222 "Inland and coastal waters fishery workers"
6223 "Deep-sea fishery workers"
6224 "Hunters and trappers"
6300 "Subsistence farmers, fishers, hunters and gatherers"
6310 "Subsistence crop farmers"
6320 "Subsistence livestock farmers"
6330 "Subsistence mixed crop and livestock farmers"
6340 "Subsistence fishers, hunters, trappers and gatherers"
7000 "Craft and related trades workers"
7100 "Building and related trades workers (excluding electricians)"
7110 "Building frame and related trades workers"
7111 "House builders"
7112 "Bricklayers and related workers"
7113 "Stonemasons, stone cutters, splitters and carvers"
7114 "Concrete placers, concrete finishers and related workers"
7115 "Carpenters and joiners"
7119 "Building frame and related trades workers not elsewhere classified"
7120 "Building finishers and related trades workers"
7121 "Roofers"
7122 "Floor layers and tile setters"
7123 "Plasterers"
7124 "Insulation workers"
7125 "Glaziers"
7126 "Plumbers and pipe fitters"
7127 "Air conditioning and refrigeration mechanics"
7130 "Painters, building structure cleaners and related trades workers"
7131 "Painters and related workers"
7132 "Spray painters and varnishers"
7133 "Building structure cleaners"
7200 "Metal, machinery and related trades workers"
7210 "Sheet and structural metal workers, moulders and welders, and related workers"
7211 "Metal moulders and coremakers"
7212 "Welders and flame cutters"
7213 "Sheet metal workers"
7214 "Structural metal preparers and erectors"
7215 "Riggers and cable splicers"
7220 "Blacksmiths, toolmakers and related trades workers"
7221 "Blacksmiths, hammersmiths and forging press workers"
7222 "Toolmakers and related workers"
7223 "Metal working machine tool setters and operators"
7224 "Metal polishers, wheel grinders and tool sharpeners"
7230 "Machinery mechanics and repairers"
7231 "Motor vehicle mechanics and repairers"
7232 "Aircraft engine mechanics and repairers"
7233 "Agricultural and industrial machinery mechanics and repairers"
7234 "Bicycle and related repairers"
7300 "Handicraft and printing workers"
7310 "Handicraft workers"
7311 "Precision-instrument makers and repairers"
7312 "Musical instrument makers and tuners"
7313 "Jewellery and precious metal workers"
7314 "Potters and related workers"
7315 "Glass makers, cutters, grinders and finishers"
7316 "Signwriters, decorative painters, engravers and etchers"
7317 "Handicraft workers in wood, basketry and related materials"
7318 "Handicraft workers in textile, leather and related materials"
7319 "Handicraft workers not elsewhere classified"
7320 "Printing trades workers"
7321 "Pre-press technicians"
7322 "Printers"
7323 "Print finishing and binding workers"
7400 "Electrical and electronics trades workers"
7410 "Electrical equipment installers and repairers"
7411 "Building and related electricians"
7412 "Electrical mechanics and fitters"
7413 "Electrical line installers and repairers"
7420 "Electronics and telecommunications installers and repairers"
7421 "Electronics mechanics and servicers"
7422 "Information and communications technology installers and servicers"
7500 "Food processing, woodworking, garment and other craft and related trades workers"
7510 "Food processing and related trades workers"
7511 "Butchers, fishmongers and related food preparers"
7512 "Bakers, pastry-cooks and confectionery makers"
7513 "Dairy products makers"
7514 "Fruit, vegetable and related preservers"
7515 "Food and beverage tasters and graders"
7516 "Tobacco preparers and tobacco products makers"
7520 "Wood treaters, cabinet-makers and related trades workers"
7521 "Wood treaters"
7522 "Cabinet-makers and related workers"
7523 "Woodworking machine tool setters and operators"
7530 "Garment and related trades workers"
7531 "Tailors, dressmakers, furriers and hatters"
7532 "Garment and related patternmakers and cutters"
7533 "Sewing, embroidery and related workers"
7534 "Upholsterers and related workers"
7535 "Pelt dressers, tanners and fellmongers"
7536 "Shoemakers and related workers"
7540 "Other craft and related workers"
7541 "Underwater divers"
7542 "Shotfirers and blasters"
7543 "Product graders and testers (excluding foods and beverages)"
7544 "Fumigators and other pest and weed controllers"
7549 "Craft and related workers not elsewhere classified"
8000 "Plant and machine operators and assemblers"
8100 "Stationary plant and machine operators"
8110 "Mining and mineral processing plant operators"
8111 "Miners and quarriers"
8112 "Mineral and stone processing plant operators"
8113 "Well drillers and borers and related workers"
8114 "Cement, stone and other mineral products machine operators"
8120 "Metal processing and finishing plant operators"
8121 "Metal processing plant operators"
8122 "Metal finishing, plating and coating machine operators"
8130 "Chemical and photographic products plant and machine operators"
8131 "Chemical products plant and machine operators"
8132 "Photographic products machine operators"
8140 "Rubber, plastic and paper products machine operators"
8141 "Rubber products machine operators"
8142 "Plastic products machine operators"
8143 "Paper products machine operators"
8150 "Textile, fur and leather products machine operators"
8151 "Fibre preparing, spinning and winding machine operators"
8152 "Weaving and knitting machine operators"
8153 "Sewing machine operators"
8154 "Bleaching, dyeing and fabric cleaning machine operators"
8155 "Fur and leather preparing machine operators"
8156 "Shoemaking and related machine operators"
8157 "Laundry machine operators"
8159 "Textile, fur and leather products machine operators not elsewhere classified"
8160 "Food and related products machine operators"
8170 "Wood processing and papermaking plant operators"
8171 "Pulp and papermaking plant operators"
8172 "Wood processing plant operators"
8180 "Other stationary plant and machine operators"
8181 "Glass and ceramics plant operators"
8182 "Steam engine and boiler operators"
8183 "Packing, bottling and labelling machine operators"
8189 "Stationary plant and machine operators not elsewhere classified"
8200 "Assemblers"
8210 "Assemblers"
8211 "Mechanical machinery assemblers"
8212 "Electrical and electronic equipment assemblers"
8219 "Assemblers not elsewhere classified"
8300 "Drivers and mobile plant operators"
8310 "Locomotive engine drivers and related workers"
8311 "Locomotive engine drivers"
8312 "Railway brake, signal and switch operators"
8320 "Car, van and motorcycle drivers"
8321 "Motorcycle drivers"
8322 "Car, taxi and van drivers"
8330 "Heavy truck and bus drivers"
8331 "Bus and tram drivers"
8332 "Heavy truck and lorry drivers"
8340 "Mobile plant operators"
8341 "Mobile farm and forestry plant operators"
8342 "Earthmoving and related plant operators"
8343 "Crane, hoist and related plant operators"
8344 "Lifting truck operators"
8350 "Ships’ deck crews and related workers"
9000 "Elementary occupations"
9100 "Cleaners and helpers"
9110 "Domestic, hotel and office cleaners and helpers"
9111 "Domestic cleaners and helpers"
9112 "Cleaners and helpers in offices, hotels and other establishments"
9120 "Vehicle, window, laundry and other hand cleaning workers"
9121 "Hand launderers and pressers"
9122 "Vehicle cleaners"
9123 "Window cleaners"
9129 "Other cleaning workers"
9200 "Agricultural, forestry and fishery labourers"
9210 "Agricultural, forestry and fishery labourers"
9211 "Crop farm labourers"
9212 "Livestock farm labourers"
9213 "Mixed crop and livestock farm labourers"
9214 "Garden and horticultural labourers"
9215 "Forestry labourers"
9216 "Fishery and aquaculture labourers"
9300 "Labourers in mining, construction, manufacturing and transport"
9310 "Mining and construction labourers"
9311 "Mining and quarrying labourers"
9312 "Civil engineering labourers"
9313 "Building construction labourers"
9320 "Manufacturing labourers"
9321 "Hand packers"
9329 "Manufacturing labourers not elsewhere classified"
9330 "Transport and storage labourers"
9331 "Hand and pedal vehicle drivers"
9332 "Drivers of animal-drawn vehicles and machinery"
9333 "Freight handlers"
9334 "Shelf fillers"
9400 "Food preparation assistants"
9410 "Food preparation assistants"
9411 "Fast food preparers"
9412 "Kitchen helpers"
9500 "Street and related sales and services workers"
9510 "Street and related services workers"
9520 "Street vendors (excluding food)"
9600 "Refuse workers and other elementary workers"
9610 "Refuse workers"
9611 "Garbage and recycling collectors"
9612 "Refuse sorters"
9613 "Sweepers and related labourers"
9620 "Other elementary workers"
9621 "Messengers, package deliverers and luggage porters"
9622 "Odd-job persons"
9623 "Meter readers and vending-machine collectors"
9624 "Water and firewood collectors"
9629 "Elementary workers not elsewhere classified"
0000 "Armed forces occupations"
0100 "Commissioned armed forces officers"
0110 "Commissioned armed forces officers"
0200 "Non-commissioned armed forces officers"
0210 "Non-commissioned armed forces officers"
0300 "Armed forces occupations, other ranks"
0310 "Armed forces occupations, other ranks"
% END

% LABELLIST-egp
% source: iskoegp.ado (version 1.2 14Oct2004 John_Hendrickx@yahoo.com)
% variables: EGP EGP-label-E
1  "higher controllers"
2  "lo controllers"
3  "routine nonmanual"
4  "sempl with emp"
5  "sempl without empl"
7  "manual supervisor"
8  "skilled manual"
9  "semi-unskilld manual"
10 "farm labor"
11 "selfempl farm"
% END

% LABELLIST-egp11
% source: iskoegp.sps from http://www.harryganzeboom.nl/isco88/
% note: slighly edited (spaces)
% variables: EGP EGP-label-E
1  "I: Higher Controllers"
2  "II: Lower Controllers"
3  "IIIa: Routine Nonmanual"
4  "IIIb: Lower Sales-Service"
5  "IVa: Selfempl with empl"
6  "IVb: Selfempl no empl"
7  "V: Manual Supervisors"
8  "VI: Skilled Worker"
9  "VIIa: Unskilled Worker"
10 "VIIb: Farm Labor"
11 "IVc: Selfempl Farmer"
% END

% LABELLIST-esec
% source: esec083digit.sps from https://ekharrison.weebly.com/european-socio-economic-classification-esec.html
% variables: ESEC ESEC-label-E
1 'Large employers, higher mgrs/professionals'
2 'Lower mgrs/professionals, higher supervisory/technicians'
3 'Intermediate occupations'
4 'Small employers and self-employed (non-agriculture)'
5 'Small employers and self-employed (agriculture)'
6 'Lower supervisors and technicians'
7 'Lower sales and service'
8 'Lower technical'
9 'Routine'
% END

% LABELLIST-oesch
% source: iskooesch.ado (May 2018) by Simon Kaiser
% variables: OESCH OESCH-label-E
1  "Large employers"
2  "Self-employed professionals"
3  "Small business owners with employees"
4  "Small business owners without employees"
5  "Technical experts"
6  "Technicians"
7  "Skilled manual"
8  "Low-skilled manual"
9  "Higher-grade managers and administrators"
10 "Lower-grade managers and administrators"
11 "Skilled clerks"
12 "Unskilled clerks"
13 "Socio-cultural professionals"
14 "Socio-cultural semi-professionals"
15 "Skilled service"
16 "Low-skilled service"
% END

% LABELLIST-oesch8
% source: iskooesch.ado (May 2018) by Simon Kaiser
% variables: OESCH8 OESCH8-label-E
1 "Self-employed professionals and large employers"
2 "Small business owners"
3 "Technical (semi-)professionals"
4 "Production workers"
5 "(Associate) managers"
6 "Clerks"
7 "Socio-cultural (semi-)professionals"
8 "Service workers"
% END

% LABELLIST-oesch5
% source: iskooesch.ado (May 2018) by Simon Kaiser
% variables: OESCH8 OESCH8-label-E
1 "Higher-grade service class"
2 "Lower-grade service class"
3 "Small business owners"
4 "Skilled workers"
5 "Unskilled workers"
% END
