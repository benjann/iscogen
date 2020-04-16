{smcl}
{* *! version 1.0.5  16apr2020  Ben Jann}{...}
{vieweralsosee "[D] generate" "help generate"}{...}
{vieweralsosee "[D] label" "help label"}{...}
{viewerjumpto "Syntax" "iscogen##syntax"}{...}
{viewerjumpto "Description" "iscogen##description"}{...}
{viewerjumpto "Options" "iscogen##options"}{...}
{viewerjumpto "Examples" "iscogen##examples"}{...}
{viewerjumpto "Sources" "iscogen##sources"}{...}
{viewerjumpto "Stored results" "iscogen##stored"}{...}
{viewerjumpto "References" "iscogen##references"}{...}
{viewerjumpto "Author" "iscogen##author"}{...}
{hi:help iscogen}
{hline}

{title:Title}

{pstd}{hi:iscogen} {hline 2} Utility to translate ISCO codes


{marker syntax}{...}
{title:Syntax}

{pstd}
    Translate ISCO codes

{p 8 15 2}
    {cmd:iscogen} {newvar} {cmd:=} {it:fcn}{cmd:(}{help varname:{it:isco}}
    [{help varname:{it:sempl}} {help varname:{it:supvis}}]{cmd:)}
    {ifin}
    [{cmd:,}
    {cmdab:f:rom(}{help iscogen##from:{it:from}}{cmd:)}
    {opt r:eplace}
    {opt em:isssing}
    {opt s:tring}
    {opt nol:abel}
    {opt inv:alid}
    ]

{pmore}
    where {it:fnc}{cmd:()} is

{p2colset 13 25 27 2}{...}
{p2col:{opt maj:or()}}generate major ISCO groups (1-digit)
    {p_end}
{p2col:{opt sub:major()}}generate submajor ISCO groups (2-digit)
    {p_end}
{p2col:{opt min:or()}}generate minor ISCO groups (3-digit)
    {p_end}
{p2col:{opt isco08()}}generate 4-digit ISCO-08 codes
    {p_end}
{p2col:{opt isco88()}}generate 4-digit ISCO-88 codes
    {p_end}
{p2col:{opt isco88com()}}generate 4-digit ISCO-88(COM) codes
    {p_end}
{p2col:{opt isco68()}}generate 4-digit ISCO-68 codes
    {p_end}
{p2col:{opt isei()}}generate ISEI scores
    {p_end}
{p2col:{opt siops()}}generate SIOPS scores
    {p_end}
{p2col:{opt trei:man()}}synonym for {cmd:siops()}
    {p_end}
{p2col:{opt mps()}}generate MPS scores
    {p_end}
{p2col:{opt egp()}}generate EGP classes (Hendrickx variant)
    {p_end}
{p2col:{opt egp11()}}generate EGP classes (Ganzeboom variant)
    {p_end}
{p2col:{opt esec()}}generate ESEC classes
    {p_end}
{p2col:{opt oesch()}}generate 16 OESCH classes
    {p_end}
{p2col:{opt oesch8()}}generate 8 OESCH classes
    {p_end}
{p2col:{opt oesch5()}}generate 5 OESCH classes
    {p_end}

{pmore}
    The function names are case-insensitive. Not all functions are supported
    for all input variables. See the list of supported translations below.

{pstd}
    Assign value labels

{p 8 15 2}
    {cmd:iscolbl} {help iscogen##lbl:{it:lbl}} [{varlist}]
    [{cmd:,} {cmdab:n:ame(}{help iscogen##name:{it:name}}{cmd:)} {opt mini:mal}
    {opt mod:ify} {opt maj:or} {opt sub:major} {opt min:or} ]


{marker description}{...}
{title:Description}

{pstd}
    {cmd:iscogen} is a tool to translate unit-group ISCO codes
    ({browse "http://www.ilo.org/public/english/bureau/stat/isco/":International Standard Classification of Occupations}). The
    following translations are currently supported (see {help iscogen##sources:Sources} below for details
    on the different translators):

{p2colset 9 20 22 2}{...}
{p2col:ISCO-08 ->}major (1 digit), submajor (2 digit), minor (3 digit) groups{p_end}
{p2col:ISCO-08 ->}ISCO-88{p_end}
{p2col:ISCO-08 ->}ISEI (International Socio-economic Index of Occupational Status; Ganzeboom et al. 1992){p_end}
{p2col:ISCO-08 ->}SIOPS (Standard International Occupational Prestige Scale; Treiman 1977){p_end}
{p2col:ISCO-08 ->}ESEC (European Socio-economic Classification; Harrison/Rose 2006){p_end}
{p2col:ISCO-08 ->}OESCH classes (16, 8 or 5 classes; Oesch 2006a,b){p_end}

{p2col:ISCO-88 ->}major (1 digit), submajor (2 digit), minor (3 digit) groups{p_end}
{p2col:ISCO-88 ->}ISCO-08{p_end}
{p2col:ISCO-88 ->}ISCO-88(COM) (European Union variant of the ISCO-88){p_end}
{p2col:ISCO-88 ->}ISCO-68{p_end}
{p2col:ISCO-88 ->}ISEI (International Socio-economic Index of Occupational Status; Ganzeboom et al. 1992){p_end}
{p2col:ISCO-88 ->}SIOPS (Standard International Occupational Prestige Scale; Treiman 1977){p_end}
{p2col:ISCO-88 ->}MPS (German Magnitude Prestige Scale; Christoph 2005){p_end}
{p2col:ISCO-88 ->}EGP classes (Erikson et al. 1979, 1983){p_end}
{p2col:ISCO-88 ->}ESEC (European Socio-economic Classification; Harrison/Rose 2006){p_end}
{p2col:}{space 2}Note: ESEC is based on ISCO-88(COM); consider converting ISCO-88 to ISCO-88(COM) before
    translating to ESEC{p_end}
{p2col:ISCO-88 ->}OESCH classes (16, 8 or 5 classes; Oesch 2006a,b){p_end}

{p2col:ISCO-68 ->}ISCO-08{p_end}
{p2col:ISCO-68 ->}ISCO-88{p_end}
{p2col:ISCO-68 ->}ISEI (International Socio-economic Index of Occupational Status; Ganzeboom et al. 1992){p_end}
{p2col:ISCO-68 ->}SIOPS (Standard International Occupational Prestige Scale; Treiman 1977){p_end}
{p2col:ISCO-68 ->}EGP classes (Erikson et al. 1979, 1983){p_end}

{pstd}
    Variable {it:isco} specifies the input ISCO variable that is to be
    translated. It can be numeric or string. The default is to assume {it:isco}
    to contain 4-digit ISCO-08 codes; for ISCO-88 or ISCO-68 you must specify option
    {cmd:from(isco88)} or {cmd:from(isco68)}, respectively. In any case, {it:isco}
    is assumed to contain 4-digit codes. Decimal places will be ignored (that is,
    for example, 1000.5 will be treated as 1000).

{pstd}
    With some translators you may additionally specify variables {it:sempl} and
    {it:supvis}. {it:sempl}!=0 indicates that a respondent is self-employed;
    {it:supvis} indicates that a respondent has supervisory status and, possibly,
    specifies the number of subordinates or employees. In general, omitted or missing
    {it:sempl} will be treated as {it:sempl}=0 (unless noted otherwise), and omitted, missing, or negative
    {it:supvis} will be treated as {it:supvis}=0. Details
    are as follows:

{phang}
o {opt egp()} and {cmd:egp11()}
    {p_end}
{phang2}
    - {cmd:from(isco88)}: relevant distinctions for {it:supvis} are 0 vs. 1 vs. 2-9 vs. 10 or more
    {p_end}
{phang2}
    - {cmd:from(isco68)}: relevant distinctions for {it:supvis} are 0 vs. 1-9 vs. 10 or more
    {p_end}

{phang}
o {opt esec()}
    {p_end}
{phang2}
    - if {it:sempl}=0: the relevant distinction is {it:supvis}=0 (no supervisory status)
    vs. {it:supvis}>0 (supervisory status); supervisors are employees
    who have formal responsibility for supervising the work of other employees; if the data
    does not contain a direct measure of supervisory status, the
    {browse "https://www.iser.essex.ac.uk/archives/esec/user-guide":ESEC User Guide}
    (section 4.7)
    suggests coding employees as supervisors if they are supervising at least three people.
    {p_end}
{phang2}
    - if {it:sempl}!=0: relevant distinctions for {it:supvis} are 0 vs. 1-9 vs. 10 or more
    (although note that the resulting ESEC classes will be the same for 0 and 1-9)
    {p_end}
{phang2}
    - {cmd:from(isco88)}: the "simplified" ESEC (see
    {browse "https://www.iser.essex.ac.uk/archives/esec/user-guide":ESEC User Guide})
    will be applied if {it:sempl} is omitted or missing
    {p_end}

{phang}
o {cmd:oesch()}, {cmd:oesch8()}, and {cmd:oesch5()}
    {p_end}
{phang2}
    - if {it:sempl}=0: {it:supvis} is ignored
    {p_end}
{phang2}
    - if {it:sempl}!=0: relevant distinctions for {it:supvis} are 0 vs. 1-9 vs. 10 or
    more
    {p_end}
{phang2}
    - helping family members should be coded as {it:sempl}!=0 with {it:supvis}=0

{marker lbl}{...}
{pstd}
    {cmd:iscolbl} creates value labels and, if {it:varlist} is provided,
    assigns them to the specified variables. {it:lbl} is one of the following
    (case-insensitive):

{p2colset 9 20 22 2}{...}
{p2col:{opt isco08}}ISCO-08 labels{p_end}
{p2col:{opt isco88}}ISCO-88 labels{p_end}
{p2col:{opt isco88com}}ISCO-88(COM) labels{p_end}
{p2col:{opt isco88a}}Ganzeboom/Treiman variant of ISCO-88 labels (brief labels){p_end}
{p2col:{opt isco88b}}Ganzeboom/Treiman variant of ISCO-88 labels (verbose labels){p_end}
{p2col:{opt isco68}}ISCO-68 labels{p_end}
{p2col:{opt egp}}EGP labels (Hendrickx variant){p_end}
{p2col:{opt egp11}}EGP labels (Ganzeboom variant){p_end}
{p2col:{opt esec}}ESEC labels{p_end}
{p2col:{opt oesch}}16-class OESCH labels{p_end}
{p2col:{opt oesch8}}8-class OESCH labels{p_end}
{p2col:{opt oesch5}}5-class OESCH labels{p_end}


{marker options}{...}
{title:Options for iscogen}

{marker from}{...}
{phang}
    {opt from(from)} declares the type of input variable. {it:from}
    can be one of the following:

{p2colset 9 20 22 2}{...}
{p2col:{opt isco08}}4-digit ISCO-08 codes; the default{p_end}
{p2col:{opt isco88}}4-digit ISCO-88 codes{p_end}
{p2col:{opt isco68}}4-digit ISCO-68 codes{p_end}

{phang}
    {opt replace} allows overwriting an existing variable.

{phang}
    {opt emissing} requests that extended missing values ({cmd:.a}, {cmd:.b},
    ...) be carried forward to the generated variable (including their labels,
    unless {cmd:nolabel} is specified). The default is to use
    system missing ({cmd:.}) for these observations. {cmd:emissing} has no
    effect if {it:isco} is a string variable or if {cmd:string} has been
    specified. In any case, extended missing values will only be carried forward
    within the sample satisfying the {it:if} and {it:in} qualifiers.

{phang}
    {opt string} enforces string format for the generated variable. The default
    is to use a numeric format.

{phang}
    {opt nolabel} suppresses creating value labels for the generated
    variable. By default, unless {cmd:string} is specified, the generated codes
    will be labelled (if labels are available).

{phang}
    {opt invalid} requests that a list of invalid codes (i.e. codes
    that could not be translated) is returned in {cmd:r(invalid)}.


{title:Options for iscolbl}

{marker name}{...}
{phang}
    {opt name(name)} provides a name to be used for the value labels. The
    default is to use the name(s) of the variable(s). If {cmd:name()} is
    specified, only one label set is added to the data; by default, each
    variable receives its own set. If  {it:varlist} is
    omitted, {it:lbl} is used as label name.

{phang}
    {opt minimal} requests that only values be labeled that exist in the
    data. The default is to include all defined labels. {cmd:minimal} is only
    allowed if {it:varlist} is specified.

{phang}
    {opt modify} causes existing labels to be modified instead of replaced.

{phang}
    {opt major}, {opt submajor}, and {opt minor} can be used with ISCO-08 and
    ISCO-88 to request labels for major groups (1-digit),
    submajor groups (2-digit), or minor groups (3-digit) instead
    of the default unit groups (4-digit). Only one of {opt major},
    {opt submajor}, and {opt minor} is allowed.


{marker sources}{...}
{title:Sources}

{pstd}
    The various mappings and labels supported by {cmd:iscogen} have been
    obtained from the following  sources.

{dlgtab:ISCO-08 mappings}

{phang}
    ISCO-08  ->  major (1 digit), submajor (2 digit), minor (3 digit) groups
    {p_end}
{pmore}
    No source. The target values are generated by truncating the input
    values. Only values between 0 and 9999 will be translated.

{phang}
    ISCO-08  ->  ISCO-88
    {p_end}
{pmore}
    SPSS script {cmd:isco0888.sps} provided by Harry Ganzeboom at
    {browse "http://www.harryganzeboom.nl/isco08/"}. The first mapping is
    used in cases where the source contains repeated mappings; this is consistent
    with the source since repeated mappings are ignored by the used SPSS command.

{phang}
    ISCO-08  ->  ISEI
    {p_end}
{pmore}
    File {bf:{browse "http://www.harryganzeboom.nl/isco08/isco08_with_isei.pdf":isco08_with_isei.pdf}}
    (Ganzeboom 2010) provided by Harry
    Ganzeboom at {browse "http://www.harryganzeboom.nl/isco08/"}.

{phang}
    ISCO-08  ->  SIOPS
    {p_end}
{pmore}
    SPSS script {cmd:isqotrei08.sps} provided by Harry Ganzeboom at
    {browse "http://www.harryganzeboom.nl/isco08/"}.

{phang}
    ISCO-08  ->  ESEC
    {p_end}
{pmore}
    File {bf:{browse "http://ekharrison.weebly.com/uploads/2/3/9/9/23996844/esec_08_3_digit_public.xlsx":esec_08_3_digit_public.xlsx}}
    provided by Eric Harrison at
    {browse "http://ekharrison.weebly.com/european-socio-economic-classification-esec.html"}. For
    information on the ESEC classification also see
    {browse "http://www.iser.essex.ac.uk/archives/esec"}. Note that ESEC is defined
    at the level of minor groups (3 digit) (i.e. all unit groups within a minor group
    map into the same class). ISCO minor groups 960, 961, and 962 are missing in the ESEC
    list provided in {bf:esec_08_3_digit_public.xlsx}; {cmd:iscogen} applies the same ESEC classes as
    for minor groups 950, 951, and 952.

{phang}
    ISCO-08  ->  OESCH
    {p_end}
{pmore}
    Stata script {cmd:iscooesch.ado} (version May 2018) by
    {browse "http://ideas.repec.org/c/boc/bocode/s458490.html":Kaiser (2018)}. The mapping has been
    generated automatically by applying {cmd:iskooesch.ado} to all relevant
    combinations of ISCO codes, self-employment status, and number
    of subordinates or employees.

{dlgtab:ISCO-88 mappings}

{phang}
    ISCO-88  ->  major (1 digit), submajor (2 digit), minor (3 digit) groups
    {p_end}
{pmore}
    No source. The target values are generated by truncating the input
    values. Only values between 0 and 9999 will be translated.

{phang}
    ISCO-88  ->  ISCO-08
    {p_end}
{pmore}
    SPSS script {cmd:isco8808.sps} provided by Harry Ganzeboom at
    {browse "http://www.harryganzeboom.nl/isco08/"}.

{phang}
    ISCO-88  ->  ISCO-08(COM)
    {p_end}
{pmore}
    Translation based on Appendix 7.12 in
    {browse "http://www.gesis.org/fileadmin/upload/dienstleistung/tools_standards/handbuch_der_berufscodierung_110304.pdf":Geis (2011)}, covering
    the {browse "http://www.ilo.org/public/english/bureau/stat/isco/isco88/major.htm":ISCO-88}
    codes provided by the ILO. The
    {browse "http://warwick.ac.uk/fac/soc/ier/research/classification/isco88":ISCO-88(COM)}
    is the European Union variant of the ISCO-88. A number
    of ISCO-88 codes do not exist in the ISCO-88(COM)
    (0110, 1120, 1130, 2132, 3240, 3241, 3242, 3439, 5150, 5151, 5152, 5230,
    6113, 6114, 6123, 6124, 6200, 6210, 7142, 7243, 8171, 8172, 9112, 9321,
    9322, 9331, 9332, 9333) or have a different meaning (6112). These codes are remapped
    as suggested by Geis (2011). Note that the ISCO-88(COM) also has three additional
    codes (2470, 7139, 8287); these codes have no counterpart in the ISCO-88 and, hence,
    will be left empty by the translator. Furthermore, a number of
    ISCO-88(COM) labels are somewhat different from the ISCO-88 labels; the translator will
    assign the ISCO-88(COM) labels in these cases.

{phang}
    ISCO-88  ->  ISCO-68
    {p_end}
{pmore}
    SPSS script {cmd:isco8868.sps} provided by Harry Ganzeboom at
    {browse "http://www.harryganzeboom.nl/isco68/"}. The mapping is consistent with
    Stata script {cmd:isko8868.ado} (version 1.0 15jun2001) by
    {browse "http://ideas.repec.org/c/boc/bocode/s425802.html":Hendrickx (2002b)}.

{phang}
    ISCO-88  ->  ISEI
    {p_end}
{pmore}
    File {bf:{browse "http://www.harryganzeboom.nl/ismf/scaleapp.htm":scaleapp.htm}}
    provided by Harry Ganzeboom at {browse "http://www.harryganzeboom.nl/isco88/"} (equivalent
    to the Appendix in Ganzeboom/Treiman 1996). Apart from two exceptions,
    the mapping is consistent with Stata script {cmd:iskoisei.ado} (version 1.0 15jun2001) by
    {browse "http://ideas.repec.org/c/boc/bocode/s425802.html":Hendrickx (2002b)}, which is based on
    SPSS script {cmd:iskoisei.sps} provided by Harry Ganzeboom at
    {browse "http://www.harryganzeboom.nl/isco88/"}. The two exceptions are ISCO codes
    6134 (score 23 by {cmd:iscogen} vs. score 28 by
    {cmd:iskoisei.ado}/{cmd:iskoisei.sps}) and 7520 (score 38 by {cmd:iscogen} vs. score 39 by
    {cmd:iskoisei.ado}/{cmd:iskoisei.sps}); the scores used by {cmd:iscogen} are consistent with
    the values printed in Ganzeboom/Treiman (1996).

{phang}
    ISCO-88  ->  SIOPS
    {p_end}
{pmore}
    File {bf:{browse "http://www.harryganzeboom.nl/ismf/scaleapp.htm":bernhard2005.xlsx}}
    provided by Harry Ganzeboom at {browse "http://www.harryganzeboom.nl/isco88/"} (equivalent
    to the Appendix in Ganzeboom/Treiman 1996). The mapping is consistent with Stata
    script {cmd:iskotrei.ado} (version 1.0 15jun2001) by
    {browse "http://ideas.repec.org/c/boc/bocode/s425802.html":Hendrickx (2002b)}, which is based on
    SPSS script {cmd:iskotrei.sps} provided by Harry Ganzeboom at
    {browse "http://www.harryganzeboom.nl/isco88/"}.

{phang}
    ISCO-88  ->  MPS
    {p_end}
{pmore}
    File {bf:{browse "http://github.com/dirtyhawk/stata-derivescores/blob/master/create_tables/proprietary/ISCO-88--MPS88/bernhard2005.xlsx":bernhard2005.xlsx}}
    provided by Daniel Bela at {browse "http://github.com/dirtyhawk/stata-derivescores/"} (should be equivalent
    to Appendix Table A5 in Christoph 2005).

{phang}
    ISCO-88  ->  EGP
    {p_end}
{phang2}
    {opt egp()}: Stata script {cmd:iskoegp.ado} (version 1.2 14Oct2004) by
    {browse "http://ideas.repec.org/c/boc/bocode/s425802.html":Hendrickx (2002b)}. The mapping
    has been generated automatically by applying {cmd:iskoegp.ado} to all relevant
    combinations of ISCO codes, self-employment status, and number
    of subordinates or employees.
    {p_end}
{phang2}
    {opt egp11()}: SPSS scripts {cmd:iskoegp.sps}, {cmd:iskoroot.sps}, and {cmd:iskopromo.sps}
    provided by Harry Ganzeboom at {browse "http://www.harryganzeboom.nl/isco88/"}. The mapping
    has been generated automatically by applying the scripts to all relevant
    combinations of ISCO codes, self-employment status, and number
    of subordinates or employees.
    {p_end}
{pmore}
    The main difference between {cmd:egp()} and {cmd:egp11()} is that
    {cmd:egp11()} distinguishes between classes IIIa and IIIb whereas
    {cmd:egp()} makes no such distinction; also note that different numeric codes
    are used for classes IVa and IVb.

{phang}
    ISCO-88  ->  ESEC
    {p_end}
{pmore}
    File {bf:{browse "http://www.iser.essex.ac.uk/files/esec/nsi/matrices/Euroesec%20matrix.xls":Euroesec matrix.xls}}
    from
    {browse "http://www.iser.essex.ac.uk/archives/esec"}. Note that ESEC is defined
    at the level of minor groups (3 digit) (i.e. all unit groups within a minor group
    map into the same class). Furthermore, note that the ESEC translator is
    based on the {browse "http://warwick.ac.uk/fac/soc/ier/research/classification/isco88":ISCO-88(COM)},
    the European Union variant of the ISCO-88. If your data contains ISCO-88 codes
    you might want to translate these codes to ISCO-88(COM) using
    {cmd:isco88com()} before applying {cmd:esec()}.

{phang}
    ISCO-88  ->  OESCH
    {p_end}
{pmore}
    Stata script {cmd:iskooesch.ado} (version May 2018) by
    {browse "http://ideas.repec.org/c/boc/bocode/s458490.html":Kaiser (2018)}. The
    mapping has been generated automatically by applying {cmd:iskooesch.ado} to all relevant
    combinations of ISCO codes, self-employment status, and number
    of subordinates or employees.

{dlgtab:ISCO-68 mappings}

{phang}
    ISCO-68  ->  ISCO-08
    {p_end}
{pmore}
    SPSS script {cmd:isco6808.sps} provided by Harry Ganzeboom at
    {browse "http://www.harryganzeboom.nl/isco08/"}. The first mapping is
    used in cases where the source contains repeated mappings; this is consistent
    with the source since repeated mappings are ignored by the used SPSS command.

{phang}
    ISCO-68  ->  ISCO-88
    {p_end}
{pmore}
    SPSS script {cmd:isco6888.sps} provided by Harry Ganzeboom at
    {browse "http://www.harryganzeboom.nl/isco08/"}. The last mapping is
    used in cases where the source contains repeated mappings; this is
    inconsistent with the source since repeated mappings are ignored by the
    used SPSS command. However, the mapping used by {cmd:iscogen} is consistent
    with Stata script {cmd:isko6888.ado} (version 1.0 15jun2001) by
    {browse "http://ideas.repec.org/c/boc/bocode/s425801.html":Hendrickx (2002a)}.

{phang}
    ISCO-68  ->  ISEI
    {p_end}
{pmore}
    SPSS script {cmd:iscoisei.sps} provided by Harry Ganzeboom at
    {browse "http://www.harryganzeboom.nl/isco68/"}. The source contains
    range mappings that have been expanded to one-to-one mappings based on the
    set of ISCO-68 codes found in {cmd:iscolab.sps} (plus code 3991 found in
    {cmd:isco6888.sps}). Furthermore, ISEI values as printed in Ganzeboom et al. (1992)
    are used in case of conflict due to repeated mappings in the source (affected
    ISCO codes are: 2033, 2035, 9830, 9831, 9832, 9839); this is consistent with
    Stata script {cmd:iscoisei.ado} (version 1.0 18jun2001) by
    {browse "http://ideas.repec.org/c/boc/bocode/s425801.html":Hendrickx (2002a)}.

{phang}
    ISCO-68  ->  SIOPS
    {p_end}
{pmore}
    SPSS script {cmd:iscotrei.sps} provided by Harry Ganzeboom at
    {browse "http://www.harryganzeboom.nl/isco68/"}. The mapping is consistent
    with Stata script {cmd:iscotrei.ado} (version 1.0 15jun2001) by
    {browse "http://ideas.repec.org/c/boc/bocode/s425801.html":Hendrickx (2002a)}.

{phang}
    ISCO-68  ->  EGP
    {p_end}
{phang2}
    {opt egp()}: Stata script {cmd:iscoegp.ado} (version 1.0 19jun2001) by
    {browse "http://ideas.repec.org/c/boc/bocode/s425801.html":Hendrickx (2002a)}. The mapping
    has been generated automatically by applying {cmd:iscoegp.ado} to all relevant
    combinations of ISCO codes, self-employment status, and number
    of subordinates or employees.
    {p_end}
{phang2}
    {opt egp11()}: SPSS script {cmd:iscoegp.sps} provided by Harry Ganzeboom at
    {browse "http://www.harryganzeboom.nl/isco68/"}. The mapping
    has been generated automatically by applying {cmd:iscoegp.sps} to all relevant
    combinations of ISCO codes, self-employment status, and number
    of subordinates or employees.
    {p_end}
{pmore}
    The main difference between {cmd:egp()} and {cmd:egp11()} is that
    {cmd:egp11()} distinguishes between classes IIIa and IIIb whereas
    {cmd:egp()} makes no such distinction; also note that different numeric codes
    are used for classes IVa and IVb.

{dlgtab:Labels}

{phang}
    ISCO-08 labels
    {p_end}
{pmore}
    File {bf:{browse "http://www.ilo.org/public/english/bureau/stat/isco/docs/groupdefn08.docx":structure08.docx}}
    provided by the ILO at {browse "http://www.ilo.org/public/english/bureau/stat/isco/isco08/"}. The
    source uses titlecase spelling; this has been changed to lowercase spelling.

{phang}
    ISCO-88 labels
    {p_end}
{phang2}
    {com:isco88}: ISCO-88 labels provided by the ILO at
    {browse "http://www.ilo.org/public/english/bureau/stat/isco/isco88/major.htm"}. The
    uppercase spelling of major, submajor, and minor groups has been changed to
    lowercase spelling
    {p_end}
{phang2}
    {opt isco88com}: ISCO-88(COM) labels from file
    {bf:ISCO88.xls} provided by the Warwick Institute for Employment Research at
    {browse "http://warwick.ac.uk/fac/soc/ier/research/classification/isco88"}; also see
    Elias/Birch (1994).
    {p_end}
{phang2}
    {opt isco88a}: Variant of ISCO-88 labels by Ganzeboom/Treiman (1996)
    obtained from SPSS script {cmd:iskolab.sps} available from
    {browse "http://www.harryganzeboom.nl/isco88/"}. The Ganzeboom/Treiman
    variant of ISCO-88 contains a few extra occupational codes compared to
    the official variant by the ILO.
    {p_end}
{phang2}
    {opt isco88b}: Variant of ISCO-88 labels by Ganzeboom/Treiman (1996)
    obtained from file {bf:{browse "http://www.harryganzeboom.nl/ismf/scaleapp.htm":scaleapp.htm}}
    available from {browse "http://www.harryganzeboom.nl/isco88/"}. The Ganzeboom/Treiman
    variant of ISCO-88 contains a few extra occupational codes compared to
    the official variant by the ILO.

{phang}
    ISCO-68 labels
    {p_end}
{pmore}
    SPSS script {bf:iscolab.sps} provided by Harry Ganzeboom at
    {browse "http://www.harryganzeboom.nl/isco68/"}.

{phang}
    EGP labels
    {p_end}
{phang2}
    {opt egp}: Stata script {cmd:iskoegp.ado} (version 1.2 14Oct2004) by
    {browse "http://ideas.repec.org/c/boc/bocode/s425802.html":Hendrickx (2002b)}.
    {p_end}
{phang2}
    {opt egp11}: SPSS script {cmd:iskoegp.sps} provided by Harry Ganzeboom at
    {browse "http://www.harryganzeboom.nl/isco88/"}.

{phang}
    ESEC labels
    {p_end}
{pmore}
    SPSS script {cmd:esec083digit.sps} provided by Eric Harrison at
    {browse "http://ekharrison.weebly.com/european-socio-economic-classification-esec.html"}.

{phang}
    OESCH labels
    {p_end}
{pmore}
    Stata script {cmd:iskooesch.ado} (version May 2018) by
    {browse "http://ideas.repec.org/c/boc/bocode/s458490.html":Kaiser (2018)}.


{marker examples}{...}
{title:Examples}

{pstd}
    Generate ISEI scores from ISCO-08 codes

        {com}. iscogen isei = isei(job)
        {txt}

{pstd}
    Translate ISCO-88 to ISCO-08:

        {com}. iscogen job08 = isco08(job), from(isco88)
        {txt}

{pstd}
    Transform 4 digit ISCO-08 unit groups to 2 digit submajor groups:

        {com}. iscogen isco2 = submajor(job)
        {txt}

{pstd}
    Generate EGP classes from ISCO-08 codes

        {com}. iscogen EGP = egp11(job selfemp nemployees), from(isco88)
        {txt}

{pstd}
    Assign ICSO-88(COM) labels to multiple variables

        {com}. iscoglbl isco88com job_mother job_father
        {txt}

{marker stored}{...}
{title:Stored results}

{pstd}
    {cmd:iscogen} stores the following {cmd:r()}:

{pstd} Scalars
    {p_end}
{p2colset 7 24 24 2}{...}
{p2col : {cmd:r(N)}}number of observations
    {p_end}
{p2col : {cmd:r(N_matched)}}number of matched observations
    {p_end}
{p2col : {cmd:r(N_unmatched)}}number of unmatched observations
    {p_end}
{p2col : {cmd:r(N_missing)}}number of missing observations
    {p_end}
{p2col : {cmd:r(N_invalid)}}number of invalid codes (only if {cmd:invalid} is specified)
    {p_end}
{p2col : {cmd:r(labeled)}}1 if labels have been assigned, 0 else
    {p_end}
{p2col : {cmd:r(string)}}1 if the generated variable is string format, 0 else
    {p_end}

{pstd} Macros
    {p_end}
{p2col : {cmd:r(varname)}}name of the generated variable
    {p_end}
{p2col : {cmd:r(from)}}classification of the input variable
    {p_end}
{p2col : {cmd:r(to)}}target classification
    {p_end}
{p2col : {cmd:r(isco)}}name of {it:isco} input variable
    {p_end}
{p2col : {cmd:r(sempl)}}name of {it:sempl} input variable
    {p_end}
{p2col : {cmd:r(supvis)}}name of {it:supvis} input variable
    {p_end}
{p2col : {cmd:r(invalid)}}list of invalid codes (only if {cmd:invalid} is specified)
    {p_end}


{pstd}
    {cmd:iscolbl} stores the following {cmd:r()}:

{pstd} Macros
    {p_end}
{p2col : {cmd:r(varlist)}}names of the labeled variables
    {p_end}
{p2col : {cmd:r(lblnames)}}names of the labels
    {p_end}


{marker references}{...}
{title:References}

{phang}
    Christoph, B. 2005. Zur Messung des Berufsprestiges: Aktualisierung der
    Magnitude-Prestigeskala auf die Berufsklassifikation ISCO88 [Measuring
    occupational prestige: updating the magnitude-prestige-scale to ISCO88]. ZUMA
    Nachrichten 29(57): 79-127. Available from
    {browse "https://nbn-resolving.org/urn:nbn:de:0168-ssoar-207543"}.
    {p_end}
{phang}
    Elias, P., M. Birch. 1994. Establishment of Community-Wide Occupational
    Statistics. ISCO 88 (COM). A Guide for Users. Institute for Employment Research,
    University of Warwick. Available from
    {browse "https://warwick.ac.uk/fac/soc/ier/research/classification/isco88/isco88.pdf"}.
    {p_end}
{phang}
    Erikson, R., J.H. Goldthorpe, L. Portocarero. 1979. Intergenerational Class Mobility
    in Three Western European Societies: England, France and Sweden. British Journal
    of Sociology 30(4): 415-441.
    {p_end}
{phang}
    Erikson, R., J.H. Goldthorpe, L. Portocarero. 1983. Intergenerational Class
    Mobility and the Convergence Thesis: England, France and Sweden. British
    Journal of Sociology 34(3): 303-343.
    {p_end}
{phang}
    Ganzeboom, H.B.G. 2010. International Standard Classification
    of Occupations. ISCO-08. With ISEI-08 scores. Last revised: July 27
    2010. Available from {browse "http://www.harryganzeboom.nl/isco08/"}.
    {p_end}
{phang}
    Ganzeboom, H.B.G., P.M. De Graaf, D.J. Treiman. 1992. A Standard
    International Socio-Economic Index of Occupational Status. Social Science
    Research 21: 1-56.
    {p_end}
{phang}
    Ganzeboom, H.B.G., D.J. Treiman. 1996. Internationally Comparable Measures
    of Occupational Status for the 1988 International Standard Classification
    of Occupations. Social Science Research 25: 201-239.
    {p_end}
{phang}
    Geis, Alfons (2011). Handbuch für die Berufsvercodung. Stand: März 2011. Mannheim: GESIS. Available
    from {browse "http://www.gesis.org/fileadmin/upload/dienstleistung/tools_standards/handbuch_der_berufscodierung_110304.pdf"}.
    {p_end}
{phang}
    Harrison, E., D. Rose. 2006. The European Socio-economic Classification
    (ESeC) User Guide. Institute for Social and Economic Research,
    University of Essex. Available from
    {browse "http://www.iser.essex.ac.uk/archives/esec/user-guide"}.
    {p_end}
{phang}
    Hendrickx, J. 2002a. isco: Stata module to recode 4 digit ISCO-68 occupational
    codes. Available from
    {browse "http://ideas.repec.org/c/boc/bocode/s425801.html"}.
    {p_end}
{phang}
    Hendrickx, J. 2002b. isko: Stata module to recode 4 digit ISCO-88 occupational
    codes. Available from
    {browse "http://ideas.repec.org/c/boc/bocode/s425802.html"}.
    {p_end}
{phang}
    Kaiser, S. 2018. oesch: Stata module to recode ISCO codes into Oesch class
    scheme. Available from
    {browse "http://ideas.repec.org/c/boc/bocode/s458490.html"}.
    {p_end}
{phang}
    Oesch, D. 2006a. Coming to Grips with a Changing Class Structure. An Analysis
    of Employment Stratification in Britain, Germany, Sweden and Switzerland. International
    Sociology 21(2): 263-288
    {p_end}
{phang}
    Oesch, D. 2006b. Redrawing the Class Map. Stratification and Institutions
    in Britain, Germany, Sweden and Switzerland. Palgrave Macmillan.
    {p_end}
{phang}
    Treiman, D.J. 1977. Occupational Prestige in Comparative Perspective. New
    York: Academic Press.
    {p_end}

{marker author}{...}
{title:Author}

{pstd}
    Ben Jann, University of Bern, ben.jann@soz.unibe.ch

{pstd}
    Thanks for citing this software as follows:

{pmore}
    Jann, B. (2019). iscogen: Stata module to translate ISCO codes. Available from
    {browse "http://ideas.repec.org/c/boc/bocode/s458665.html"}.

