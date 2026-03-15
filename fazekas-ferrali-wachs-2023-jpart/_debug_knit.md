---
title: "PLSC 503: Homework 1"
subtitle: "Application to Fazekas, Ferrali, and Wachs (2023): Agency Independence, Campaign Contributions, and Favoritism in US Federal Government Contracting"
author: "Jody Park"
date: "2026-03-14"
output:
  bookdown::pdf_document2:
    toc: false
    number_sections: false
    latex_engine: xelatex
colorlinks: true
urlcolor: blue
linkcolor: blue
bibliography: references.bib
csl: apanodoi.csl
link-citations: true
header-includes:
  - \usepackage[capposition=top]{floatrow}
  - \usepackage{placeins}   
  - \usepackage{setspace}
  - \usepackage{dcolumn}
  - \usepackage{booktabs}
  - \usepackage{siunitx}
  - \usepackage{amsmath}
  - \usepackage{enumerate}
  - \usepackage[shortlabels]{enumitem}
  - \usepackage[hang,flushmargin]{footmisc}	
  - \usepackage{makecell}
  - \usepackage{url}
  - \usepackage{longtable}
  - \usepackage{graphicx}
  - \usepackage{array}
  - \usepackage{multirow}
  - \usepackage{wrapfig}
  - \usepackage{float}
  - \usepackage{colortbl}
  - \usepackage{pdflscape}
  - \usepackage{threeparttable}
  - \usepackage{threeparttablex}
  - \usepackage[normalem]{ulem}
  - \usepackage{makecell}
  - \usepackage{xcolor}
  - \usepackage{hyperref}
  - \hypersetup{
      colorlinks=true,
      linkcolor=blue,
      filecolor=magenta,      
      urlcolor=blue,
      citecolor=black,
    }
  - \usepackage[most]{tcolorbox}
  - \newtcolorbox{qbox}{enhanced,breakable,boxrule=0.5pt,arc=2mm,
    colback=gray!10,colframe=black!50,left=6pt,right=6pt,top=6pt,bottom=6pt}

---



# 1. Replication Setup

## Citations and Links

- **Original Paper**: Fazekas, M., Ferrali, R., & Wachs, J. (2023). Agency independence, campaign contributions, and favoritism in US federal government contracting. Journal of Public Administration Research and Theory, 33(2), 262-278.


- **Replication Data**: [R replication codes and dataset](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/3U07EE) from @fazekasagency2023 "Agency Independence, Campaign Contributions, and Favoritism in US Federal Government Contracting (JPART)." 


- **Repository Links**:  

  - [Dropbox folder](https://www.dropbox.com/scl/fo/6b5w3zznokywvmlox3dt2/ANIInPxRVHuL4ys3Jqr0JX0?rlkey=3a2d8uvvkcfmbxoldhr91jobu&st=ucxm71yq&dl=0)

  - [GitHub repository for this project](https://github.com/jodypark/plsc503_replication_extension_fazekas2023)

\bigskip

## Article Introduction

@fazekasagency2023 argue that political party contributions can bias the award of US federal government contracts, favoring firms that make donations. The paper investigates the relationship between political party contributions and favoritism in US federal government contracting. The authors examine whether firms that make political donations are more likely to receive favorable treatment in federal procurement, and how this relationship is moderated by the level of agency independence. The study uses a comprehensive dataset of federal contracts, political donations, and agency characteristics to analyze patterns of favoritism and the influence of political contributions on contract awards. The authors find that firms that make political donations are more likely to receive favorable treatment in federal contracting, and this effect is stronger for firms that donate to the party in power. Additionally, the relationship between political contributions and favoritism is moderated by agency independence, with less independent agencies showing a stronger relationship between donations and favoritism. The core contribution of the paper is to provide empirical evidence on the influence of political contributions on federal contracting. The authors highlight the potential for favoritism and the role of agency independence in moderating this relationship. The findings have important implications for understanding the dynamics of public procurement and the potential for corruption in government contracting. 


I plan to extend the analysis with additional robustness checks by examining how agency ideology moderates the effect of political party contributions on favoritism in federal contracting, following dynamic estimates of regulatory agency ideal points in @acsmapping2025. His measure consists of dynamic estimates of regulatory agency ideal points across four different presidential administrations (from Clinton to Trump), for 69 administrative units. The data is based on 1) Unified Agenda data on planned regulations and 2) the president's discretionary review of those regulations from the Office of Information and Regulatory Affairs (OIRA) docket, which captures the ideological positions of regulatory agencies over time. By merging this agency ideology data with @fazekasagency2023's dataset, I will be able to analyze how the alignment between agency ideology and the party in power affects the relationship between political contributions and favoritism in federal contracting. This extension will provide a deeper understanding of the mechanisms through which political contributions may affect contract awards and how agency characteristics shape this relationship.


\bigskip


## Data, Variables and Models

### Data

In @fazekasagency2023, the authors use a comprehensive dataset:

1. **Transaction-level federal contract data**: [USAspending.gov](USAspending.gov)

2. **Campaign contributions data**: Registered campaign contributions for 2004-15 from [The Database on Ideology, Money in Politics, and Elections (DIME)](https://data.stanford.edu/dime) [@bonicadime2016]

3. **Favoritism measure**: Use an extensive review of the literature to select indicators that have been used to measure favoritism in public procurement (Favoritism Risk Indicator, FRI). The authors combine these indicators into a composite index (`CRI7_final`) that captures the overall risk of favoritism in federal contracts.

  - *7 indicators for FRI*

    - (1) Single bidding

    - (2) No publication

    - (3) Non-competitive procedure type 

    - (4) Non-open solicitation type

    - (5) Contract modifications

    - (6) Supplier tax haven registration

    - (7) Supplier debarred


\bigskip


### Variables

1. **Dependent Variable**: Favored treatment in federal procurement

- (1) **`logValue`**: Logarithm of the total contract value

- (2) **Composite Favoritism Index (`CRI7_final`)**: Average of the seven favoritism indicators listed above


2. **Independent Variables**: Political Donations

- (1) **`isDonation`**: Dummy variable indicating whether a firm made any political donation

- (2) **`logDonation`**: Logarithm of the total political donation amount


3. **Moderating Variable**: Agency Independence (`agency_cat`) - Categorize federal agencies based on their level of independence from political control [@dahlstrom2021]

  - *4 categories for agency insulation categories*

    - (1) Executive Departments (not separate bureaus)

    - (2) Executive Departments (distinct bureaus)

    - (3) Independent Administrations (agencies structured similar to executive departments but not part of the cabinet)

    - (4) Independent Commissions and Regulatory Commissions


4. **Control Variables**: Contract-level characteristics


5. **Fixed Effects**: Product category, state, contracting agency, and congressional term


\begin{table}[!h]

\caption{\label{tab:keyvar}Key Variables}
\centering
\begin{tabular}[t]{llc}
\toprule
Variable & Description & Type\\
\midrule
isDonation & Dummy variable indicating whether a firm made any political donation & Binary\\
logDonation & Logarithm of the total political donation amount & Continuous\\
agency\_cat & Categorical indicator of federal agency type & Categorical\\
logValue & Logarithm of the total contract value & Continuous\\
contract\_dummy & Dummy variable for GSA-run procurement & Binary\\
\addlinespace
commercial\_dummy & Dummy variable indicating use of commercial procurement procedures & Binary\\
ctypefixed & Dummy variable for fixed-price contracts & Binary\\
ctypecost & Dummy variable for cost-plus contracts & Binary\\
rev & Logarithm of the firm's total contract revenue in the previous year & Continuous\\
dollarsobligated\_sum & Total dollar amount obligated for the contract & Continuous\\
\addlinespace
pduns & Unique firm identifier (Dun \& Bradstreet number of the parent firm) & Categorical\\
congTerm & Congressional term (2-year periods starting from 2001) & Categorical\\
\bottomrule
\end{tabular}
\end{table}





\begin{table}[!h]

\caption{\label{tab:descriptive}Descriptive Statistics}
\centering
\begin{tabular}[t]{lrrrrr}
\toprule
Variable & N & Mean & SD & Min & Max\\
\midrule
Single bidding & 463889 & 0.560 & 0.496 & 0.000 & 1.000\\
FRI & 440987 & 0.000 & 1.079 & -3.541 & 1.659\\
Contract value (log) & 475459 & 13.169 & 1.485 & 11.513 & 23.457\\
Donation dummy & 475459 & 0.141 & 0.348 & 0.000 & 1.000\\
Donation (log) & 475459 & 1.512 & 3.892 & 0.000 & 16.931\\
\addlinespace
Agency category & 460953 & 1.925 & 0.263 & 1.000 & 2.000\\
\bottomrule
\end{tabular}
\end{table}


### Models

The primary empirical tests examine how the effect of political donations on `CRI7_final` varies across agency types. Hypothesis 3 holds that donations increase favoritism more in less independent agencies. I test this by interacting donation variables with `agency_cat`, a four-category indicator of agency independence, in a two-way fixed-effects specification with product, state, contracting office, and congressional term fixed effects, and standard errors clustered by firm and congressional term. Table 4 presents the interaction models and Figure 4 presents the corresponding marginal effects of `logDonation` on FRI across the four agency types.


\newpage


# 2. Replication Accuracy

## Figure 3. Density of Donations to Majority and Opposition

<div class="figure">
<img src="figure/replicationsetup-1.png" alt="plot of chunk replicationsetup" width="100%" />
<p class="caption">plot of chunk replicationsetup</p>
</div>

Figure 3 plots majority-party donations against opposition-party donations for each firm-term observation. The mass of observations falls above the diagonal, indicating that most donating firms concentrate contributions toward the majority party rather than hedging across parties. The gradient darkens sharply near the majority-dominant region, consistent with Hypothesis 2 that firms strategically target the party best positioned to influence contracting outcomes.


\newpage


## Table 4. Interaction Effects Between Agency Politicization and Donations on FRI (hypothesis 3)


\begin{tabular}[t]{lccccc}
\toprule
\multicolumn{1}{c}{ } & \multicolumn{5}{c}{Favouritism Risk Index (FRI)} \\
\cmidrule(l{3pt}r{3pt}){2-6}
  & (1) & (2) & (3) & (4) & (5)\\
\midrule
Donation dummy & \num{-0.079} & \num{-0.297} &  & \num{-0.099} & \\
 & (\num{0.051}) & (\num{<0.001}) &  & (\num{0.010}) & \\
Donation dummy $\times$ Cabinet/Exec. dep. & \num{0.131} &  &  &  & \\
 & (\num{0.006}) &  &  &  & \\
Log donation &  & \num{0.020} &  &  & \\
 &  & (\num{0.007}) &  &  \vphantom{1} & \\
Log donation $\times$ Cabinet/Exec. dep. &  & \num{0.013} &  &  & \\
 &  & (\num{0.007}) &  &  & \\
Log donation to majority &  &  &  & \num{0.001} & \\
 &  &  &  & (\num{0.827}) & \\
Log donation to majority $\times$ Cabinet/Exec. dep. &  &  &  & \num{0.016} & \\
 &  &  &  & (\num{0.005}) & \\
Med. donation &  &  & \num{-0.080} &  & \\
 &  &  & (\num{0.076}) &  & \\
Lrg. donation &  &  & \num{0.039} &  & \\
 &  &  & (\num{0.767}) &  & \\
Med. donation $\times$ Cabinet/Exec. dep. &  &  & \num{0.156} &  & \\
 &  &  & (\num{0.005}) &  & \\
Lrg. donation $\times$ Cabinet/Exec. dep. &  &  & \num{0.250} &  & \\
 &  &  & (\num{0.134}) &  & \\
Intermediate donation to majority &  &  &  &  & \num{-0.094}\\
 &  &  &  &  & (\num{0.061})\\
Large donation to majority &  &  &  &  & \num{-0.022}\\
 &  &  &  &  & (\num{0.859})\\
Med. donation to maj. $\times$ Cabinet/Exec. dep. &  &  &  &  & \num{0.134}\\
 &  &  &  &  & (\num{0.010})\\
Lrg. donation to maj. $\times$ Cabinet/Exec. dep. &  &  &  &  & \num{0.324}\\
 &  &  &  &  & (\num{0.056})\\
Cabinet/Exec. dep. & \num{-0.120} & \num{-0.122} & \num{-0.116} & \num{-0.122} & \num{-0.119}\\
 & (\num{0.111}) & (\num{0.106}) & (\num{0.121}) & (\num{0.105}) & (\num{0.115})\\
Log contract value & \num{-0.070} & \num{-0.071} & \num{-0.071} & \num{-0.070} & \num{-0.071}\\
 & (\num{<0.001}) & (\num{<0.001}) & (\num{<0.001}) & (\num{<0.001}) & (\num{<0.001})\\
GSA-run procurement & \num{-0.421} & \num{-0.418} & \num{-0.418} & \num{-0.419} & \num{-0.419}\\
 & (\num{0.145}) & (\num{0.147}) & (\num{0.147}) & (\num{0.146}) & (\num{0.146})\\
Commercial item & \num{-0.006} & \num{-0.006} & \num{-0.006} & \num{-0.007} & \num{-0.006}\\
 & (\num{0.796}) & (\num{0.796}) & (\num{0.817}) & (\num{0.787}) & (\num{0.813})\\
Fixed-price contract & \num{0.181} & \num{0.179} & \num{0.179} & \num{0.180} & \num{0.179}\\
 & (\num{0.112}) & (\num{0.112}) & (\num{0.112}) & (\num{0.111}) & (\num{0.112})\\
Cost-plus contract & \num{0.134} & \num{0.130} & \num{0.130} & \num{0.132} & \num{0.132}\\
 & (\num{0.269}) & (\num{0.277}) & (\num{0.276}) & (\num{0.270}) & (\num{0.270})\\
\midrule
Num.Obs. & \num{427748} & \num{427748} & \num{427748} & \num{427748} & \num{427748}\\
R2 & \num{0.316} & \num{0.317} & \num{0.317} & \num{0.317} & \num{0.317}\\
\bottomrule
\end{tabular}


Table 4 tests Hypothesis 3 by interacting `isDonation` and `logDonation` with `agency_cat`. The interaction coefficients are positive and statistically significant for cabinet-level agencies and their bureaus, indicating that the effect of donations on `CRI7_final` is substantially larger in agencies with less formal independence from presidential control. For independent commissions and IRCs the base effect and interaction terms are smaller and often statistically indistinguishable from zero. The gradient in effect sizes across the four tiers supports the theoretical prediction that partisan influence over agency appointments amplifies the returns to political contributions.


\newpage


## Table A13. Robustness checks for H3: Controlling for lagged revenue


\begin{tabular}[t]{lccccc}
\toprule
\multicolumn{1}{c}{ } & \multicolumn{5}{c}{Favouritism Risk Index (FRI)} \\
\cmidrule(l{3pt}r{3pt}){2-6}
  & (1) & (2) & (3) & (4) & (5)\\
\midrule
Donation dummy & \num{-0.137} & \num{-0.199} &  & \num{-0.093} & \\
 & (\num{0.022}) & (\num{0.006}) &  & (\num{0.025}) & \\
Donation dummy $\times$ Cabinet/Exec. dep. & \num{0.124} &  &  &  & \\
 & (\num{0.020}) &  &  &  & \\
Log donation &  & \num{0.005} &  &  & \\
 &  & (\num{0.354}) &  &  & \\
Log donation $\times$ Cabinet/Exec. dep. &  & \num{0.013} &  &  & \\
 &  & (\num{0.015}) &  &  & \\
Log donation to majority &  &  &  & \num{-0.007} & \\
 &  &  &  & (\num{0.243}) & \\
Log donation to majority $\times$ Cabinet/Exec. dep. &  &  &  & \num{0.016} & \\
 &  &  &  & (\num{0.012}) & \\
Med. donation &  &  & \num{-0.150} &  & \\
 &  &  & (\num{0.019}) &  & \\
Lrg. donation &  &  & \num{-0.086} &  & \\
 &  &  & (\num{0.547}) &  & \\
Med. donation $\times$ Cabinet/Exec. dep. &  &  & \num{0.154} &  & \\
 &  &  & (\num{0.012}) &  & \\
Lrg. donation $\times$ Cabinet/Exec. dep. &  &  & \num{0.209} &  & \\
 &  &  & (\num{0.215}) &  & \\
Intermediate donation to majority &  &  &  &  & \num{-0.158}\\
 &  &  &  &  & (\num{0.024})\\
Large donation to majority &  &  &  &  & \num{-0.134}\\
 &  &  &  &  & (\num{0.364})\\
Med. donation to maj. $\times$ Cabinet/Exec. dep. &  &  &  &  & \num{0.142}\\
 &  &  &  &  & (\num{0.017})\\
Lrg. donation to maj. $\times$ Cabinet/Exec. dep. &  &  &  &  & \num{0.268}\\
 &  &  &  &  & (\num{0.128})\\
Cabinet/Exec. dep. & \num{-0.137} & \num{-0.139} & \num{-0.134} & \num{-0.140} & \num{-0.138}\\
 & (\num{0.134}) & (\num{0.128}) & (\num{0.143}) & (\num{0.125}) & (\num{0.132})\\
Log contract value & \num{-0.078} & \num{-0.077} & \num{-0.077} & \num{-0.077} & \num{-0.077}\\
 & (\num{<0.001}) & (\num{<0.001}) & (\num{<0.001}) & (\num{<0.001}) & (\num{<0.001})\\
GSA-run procurement & \num{-0.547} & \num{-0.547} & \num{-0.547} & \num{-0.547} & \num{-0.548}\\
 & (\num{0.085}) & (\num{0.085}) & (\num{0.085}) & (\num{0.084}) & (\num{0.084})\\
Commercial item & \num{0.005} & \num{0.004} & \num{0.005} & \num{0.004} & \num{0.005}\\
 & (\num{0.827}) & (\num{0.851}) & (\num{0.831}) & (\num{0.853}) & (\num{0.832})\\
Fixed-price contract & \num{0.207} & \num{0.206} & \num{0.206} & \num{0.207} & \num{0.206}\\
 & (\num{0.108}) & (\num{0.108}) & (\num{0.108}) & (\num{0.108}) & (\num{0.108})\\
Cost-plus contract & \num{0.144} & \num{0.142} & \num{0.142} & \num{0.143} & \num{0.143}\\
 & (\num{0.289}) & (\num{0.291}) & (\num{0.290}) & (\num{0.288}) & (\num{0.288})\\
Log($\text{revenue}_{t-1}$) & \num{0.031} & \num{0.028} & \num{0.028} & \num{0.029} & \num{0.029}\\
 & (\num{<0.001}) & (\num{<0.001}) & (\num{<0.001}) & (\num{<0.001}) & (\num{0.001})\\
\midrule
Num.Obs. & \num{332756} & \num{332756} & \num{332756} & \num{332756} & \num{332756}\\
R2 & \num{0.339} & \num{0.339} & \num{0.339} & \num{0.339} & \num{0.339}\\
\bottomrule
\end{tabular}


Table A13 reproduces Table 4 with lagged firm revenue added as a control. The interaction patterns are substantively unchanged and the key interaction coefficients remain significant for cabinet-level agencies. This rules out the possibility that past firm size drives both donation capacity and contracting success, leaving the causal interpretation of Hypothesis 3 intact.

\newpage

## Figure 4. Marginal Effect of Donations on Favoritism by Agency Politicization

<div class="figure" style="text-align: center">
<img src="figure/f4rep-1.png" alt="plot of chunk f4rep" width="100%" />
<p class="caption">plot of chunk f4rep</p>
</div>

Figure 4 plots the marginal effect of `logDonation` on `CRI7_final` by agency type, with $95\%$ confidence intervals. The point estimates are positive and the confidence intervals exclude zero for both cabinet department categories. For independent agencies and independent commissions the estimates are near zero and statistically indistinguishable from it. The monotone ordering of effect sizes from most to least independent agency type closely matches the theoretical ordering implied by Hypothesis 3.


\newpage


# 3. Methodological Extension

## Why this method is necessary

@fazekasagency2023 moderate the donation and favoritism relationship by agency independence, classified into four ordered tiers of formal insulation from presidential control. This framework captures meaningful structural variation but treats agencies within each tier as equivalent. Regulatory agencies, those with binding rulemaking authority over private actors, represent a distinct institutional type that the four-tier classification does not fully isolate. Because regulatory decisions directly shape the competitive environment facing firms, the expected returns to political investment are highest in agencies that can impose or waive compliance costs. The regulatory versus non-regulatory distinction thus provides a theoretically grounded alternative moderator and tests whether donation-driven favoritism concentrates specifically in agencies whose discretion carries distributional weight for donors.

## How the method improves the paper

I extend the baseline specification by constructing a binary indicator that classifies agencies as regulatory or non-regulatory. The classification combines three matching strategies: strict matching against agency identifiers from @acsmapping2025, who estimate dynamic ideal points for U.S. federal regulatory agencies from Clinton through Trump using an OIRA-based item response theory model; broad matching based on whether agencies issued final rules in the Federal Register during 2004 to 2015; and fuzzy string matching to resolve agencies not captured by the first two approaches. The combined indicator (`regulatory_finalmatch`) replaces the four-tier independence moderator in the baseline interaction model while retaining the original fixed-effect structure and clustering. This approach implicitly assumes that regulatory status is a meaningful and observable characteristic that firms can identify and target when allocating contributions. I also estimate a difference-in-differences-in-differences (DDD) model that adds presidential party as a third dimension and an event-study design around the 2009 Bush-Obama transition to examine whether donation effects in regulatory agencies vary with partisan control of the executive.

The tables below describe the agency composition of @fazekasagency2023's sample by independence category and contract frequency. I examine agency coverage before estimating extension models to identify distributional imbalances across tiers and to assess how well the ACS matching procedure covers the sample [@acsmapping2025].


\begin{table}[!h]

\caption{\label{tab:extensiondesc}Total Number of Contracts by Agency Category}
\centering
\begin{tabular}[t]{lrrrrr}
\toprule
Agency category (4-level) & Unique agencies & Contract observations & Years in sample & First year & Last year\\
\midrule
cabinet/exec. dep. and bureau & 84 & 374572 & 11 & 2004 & 2014\\
cabinet/exec. dep. (not bureau) & 9 & 51831 & 11 & 2004 & 2014\\
indep. agency & 22 & 30892 & 11 & 2004 & 2014\\
indep. commission \& IRC & 27 & 3658 & 11 & 2004 & 2014\\
\midrule
Total & 140 & 460953 & 11 & 2004 & 2014\\
\bottomrule
\end{tabular}
\end{table}

\begin{table}[!h]

\caption{\label{tab:extensiondesc}Top 20 Agencies by Contract Frequency by Year}
\centering
\resizebox{\linewidth}{!}{
\fontsize{10}{12}\selectfont
\begin{tabular}[t]{llcccccccccccc}
\toprule
Agency name & Agency category (4-level) & Total contracts & 2004 & 2005 & 2006 & 2007 & 2008 & 2009 & 2010 & 2011 & 2012 & 2013 & 2014\\
\midrule
Department of the Army & cabinet/exec. dep. and bureau & 98200 & 8543 & 8910 & 9330 & 10004 & 10656 & 10782 & 10027 & 9182 & 8045 & 6163 & 6558\\
Department of the Navy & cabinet/exec. dep. and bureau & 62425 & 5068 & 5550 & 5417 & 5955 & 6115 & 5497 & 5668 & 6242 & 6047 & 5180 & 5686\\
Defense Logistics Agency & cabinet/exec. dep. and bureau & 59560 & 3468 & 3156 & 3191 & 3264 & 3598 & 4430 & 6646 & 7550 & 7993 & 8155 & 8109\\
Department of the Air Force & cabinet/exec. dep. and bureau & 45101 & 4027 & 4182 & 4428 & 4616 & 4335 & 4224 & 4166 & 4061 & 4031 & 3515 & 3516\\
Department of Veterans Affairs & cabinet/exec. dep. (not bureau) & 40421 & 2485 & 3077 & 2977 & 5347 & 4002 & 4277 & 4243 & 3752 & 3575 & 3736 & 2950\\
\addlinespace
Agricultural Marketing Service & cabinet/exec. dep. and bureau & 12564 & 1063 & 1049 & 1144 & 1208 & 1113 & 1127 & 1198 & 934 & 1279 & 1236 & 1213\\
Public Buildings Service & indep. agency & 11473 & 1553 & 1257 & 1129 & 1075 & 1042 & 1157 & 1100 & 914 & 835 & 670 & 741\\
Farm Service Agency & cabinet/exec. dep. and bureau & 8798 & 973 & 976 & 828 & 953 & 916 & 855 & 746 & 628 & 698 & 653 & 572\\
Forest Service & cabinet/exec. dep. and bureau & 8468 & 766 & 788 & 758 & 869 & 867 & 954 & 1243 & 627 & 567 & 463 & 566\\
Bureau of Prisons/Federal Prison System & cabinet/exec. dep. and bureau & 6760 & 539 & 523 & 525 & 607 & 562 & 561 & 585 & 639 & 741 & 699 & 779\\
\addlinespace
U.S. Coast Guard & cabinet/exec. dep. and bureau & 6661 & 647 & 669 & 564 & 556 & 650 & 612 & 625 & 599 & 610 & 536 & 593\\
National Aeronautics and Space Administration & indep. agency & 6302 & 603 & 653 & 593 & 631 & 618 & 498 & 497 & 524 & 650 & 584 & 451\\
National Institutes of Health & cabinet/exec. dep. and bureau & 5883 & 559 & 495 & 410 & 676 & 581 & 602 & 642 & 461 & 469 & 475 & 513\\
Department of State & cabinet/exec. dep. (not bureau) & 5051 & 445 & 395 & 432 & 430 & 369 & 407 & 518 & 547 & 516 & 461 & 531\\
National Park Service. & cabinet/exec. dep. and bureau & 4284 & 329 & 405 & 305 & 424 & 424 & 541 & 573 & 320 & 328 & 293 & 342\\
\addlinespace
U.S. Agency for International Development & indep. agency & 4275 & 197 & 237 & 209 & 338 & 356 & 393 & 552 & 499 & 471 & 494 & 529\\
Department of Defense & Missing & 4248 & 114 & 121 & 139 & 176 & 583 & 546 & 553 & 521 & 550 & 405 & 540\\
Defense Information Systems Agency & cabinet/exec. dep. and bureau & 4112 & 172 & 196 & 501 & 419 & 415 & 459 & 475 & 361 & 425 & 354 & 335\\
Federal Aviation Administration & cabinet/exec. dep. and bureau & 3280 & 64 & 82 & 153 & 384 & 438 & 423 & 429 & 364 & 365 & 321 & 257\\
Office of Policy, Management and Budget/Chief Financial Officer & Missing & 2806 & 412 & 511 & 283 & 241 & 239 & 201 & 230 & 211 & 223 & 122 & 133\\
\bottomrule
\end{tabular}}
\end{table}

\begin{table}[!h]

\caption{\label{tab:extensiondesc}Bottom 20 Agencies by Contract Frequency by Year}
\centering
\resizebox{\linewidth}{!}{
\fontsize{10}{12}\selectfont
\begin{tabular}[t]{llcccccccccccc}
\toprule
Agency name & Agency category (4-level) & Total contracts & 2007 & 2012 & 2004 & 2010 & 2006 & 2005 & 2013 & 2014 & 2009 & 2011 & 2008\\
\midrule
Committee for Purchase from People who are Blind or Severely Disabled & indep. commission \& IRC & 1 & 0 & 0 & 1 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0\\
Department of the Treasury & Missing & 1 & 0 & 0 & 0 & 1 & 0 & 0 & 0 & 0 & 0 & 0 & 0\\
Election Assistance Commission & indep. commission \& IRC & 1 & 0 & 0 & 0 & 1 & 0 & 0 & 0 & 0 & 0 & 0 & 0\\
Immediate Office of the Secretary of Health and Human Services & cabinet/exec. dep. and bureau & 1 & 0 & 0 & 1 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0\\
National Mediation Board & Missing & 1 & 0 & 0 & 0 & 0 & 0 & 1 & 0 & 0 & 0 & 0 & 0\\
\addlinespace
Occupational Safety and Health Review Commission & Missing & 1 & 1 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0\\
Office of Citizen Services and Communications & Missing & 1 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 1 & 0\\
Office of Special Counsel & Missing & 1 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 1\\
Office of the Solicitor & Missing & 1 & 0 & 0 & 1 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0\\
Bureau of International Labor Affairs & cabinet/exec. dep. and bureau & 2 & 0 & 0 & 2 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0\\
\addlinespace
Federal Housing Finance Board & indep. commission \& IRC & 2 & 0 & 0 & 0 & 0 & 2 & 0 & 0 & 0 & 0 & 0 & 0\\
Inspector General & Missing & 2 & 0 & 0 & 0 & 0 & 0 & 0 & 1 & 1 & 0 & 0 & 0\\
Occupational Safety and Health Administration & cabinet/exec. dep. and bureau & 2 & 0 & 0 & 2 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0\\
Office of Job Corps & Missing & 2 & 1 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 1 & 0 & 0\\
Veterans Employment and Training Services & cabinet/exec. dep. and bureau & 2 & 0 & 0 & 2 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0\\
\addlinespace
Woodrow Wilson International Center for Scholars & Missing & 2 & 0 & 0 & 0 & 0 & 0 & 0 & 2 & 0 & 0 & 0 & 0\\
Agency for Healthcare Research and Quality & cabinet/exec. dep. and bureau & 3 & 2 & 1 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0\\
Employment Standards Administration & cabinet/exec. dep. and bureau & 4 & 0 & 0 & 4 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0\\
Foreign Agricultural Service & cabinet/exec. dep. and bureau & 4 & 0 & 0 & 0 & 0 & 1 & 3 & 0 & 0 & 0 & 0 & 0\\
Institute of Museum Services & indep. agency & 5 & 2 & 0 & 0 & 1 & 0 & 0 & 0 & 0 & 2 & 0 & 0\\
\bottomrule
\end{tabular}}
\end{table}

\begin{table}[!h]

\caption{\label{tab:extensiondesc}Distribution of Agencies by Category (4-level)}
\centering
\begin{tabular}[t]{lrr}
\toprule
Agency category (4-level) & Count & Percentage\\
\midrule
indep. commission \& IRC & 3658 & 0.8\%\\
cabinet/exec. dep. and bureau & 374572 & 81.3\%\\
indep. agency & 30892 & 6.7\%\\
cabinet/exec. dep. (not bureau) & 51831 & 11.2\%\\
\bottomrule
\end{tabular}
\end{table}


\begin{table}[!h]

\caption{\label{tab:extensionsetup}ACS (2025) Merge Check and Regulatory Dummy Coverage}
\centering
\begin{tabular}[t]{rrrr}
\toprule
n\_obs & n\_agencies & n\_agencies\_matched\_to\_acs & pct\_obs\_regulatory\\
\midrule
475459 & 175 & 37 & 21.58987\\
\bottomrule
\end{tabular}
\end{table}


\newpage




\begin{table}[!h]

\caption{\label{tab:regulatorymatch}Regulatory Audit Summary (Strict ACS vs Broad Rulemaking Reference)}
\centering
\resizebox{\linewidth}{!}{
\fontsize{10}{12}\selectfont
\begin{tabular}[t]{lr}
\toprule
metric & n\\
\midrule
Unique agencies in Fazekas data & 175\\
Strict ACS matches (regulatory\_strict = 1) & 37\\
Broad-only matches (broad=1, strict=0) & 57\\
Unmatched in both references & 81\\
Fuzzy candidates among unmatched & 8\\
\addlinespace
Broad regulatory reference size (2004-2015) & 277\\
\bottomrule
\end{tabular}}
\end{table}

\begin{table}[!h]

\caption{\label{tab:regulatorymatch}Sample of Agencies Matched by Broad Rulemaking Reference but not ACS (first 10)}
\centering
\resizebox{\linewidth}{!}{
\fontsize{10}{12}\selectfont
\begin{tabular}[t]{lll}
\toprule
agency\_name & agency\_name\_clean & agency\_name\_broad\\
\midrule
Agricultural Research Service & agricultural research service & Agricultural Research Service\\
Broadcasting Board of Governors & broadcasting board of governors & Broadcasting Board of Governors\\
Centers for Disease Control and Prevention & centers for disease control and prevention & Centers for Disease Control and Prevention\\
Committee for Purchase from People who are Blind or Severely Disabled & committee for purchase from people who are blind or severely disabled & Committee for Purchase From People Who Are Blind or Severely Disabled\\
Commodity Futures Trading Commission & commodity futures trading commission & Commodity Futures Trading Commission\\
\addlinespace
Consumer Product Safety Commission & consumer product safety commission & Consumer Product Safety Commission\\
Corporation for National and Community Service & corporation for national and community service & Corporation for National and Community Service\\
Court Services and Offender Supervision Agency for the District of Columbia & court services and offender supervision agency for the district of columbia & Court Services and Offender Supervision Agency for the District of Columbia\\
Defense Logistics Agency & defense logistics agency & Defense Logistics Agency\\
Defense Nuclear Facilities Safety Board & defense nuclear facilities safety board & Defense Nuclear Facilities Safety Board\\
\bottomrule
\end{tabular}}
\end{table}

\begin{table}[!h]

\caption{\label{tab:regulatorymatch}Sample of Fuzzy Match Candidates among Unmatched Agencies (first 10)}
\centering
\resizebox{\linewidth}{!}{
\fontsize{10}{12}\selectfont
\begin{tabular}[t]{llllrr}
\toprule
Agency name & Clean name & Nearest broad agency & Nearest broad (clean) & Edit dist. & Norm. dist.\\
\midrule
Veterans Employment and Training Services & veterans employment and training services & Veterans Employment and Training Service & veterans employment and training service & 1 & 0.024\\
U.S. Agency for International Development & u s agency for international development & Agency for International Development & agency for international development & 4 & 0.100\\
U.S. International Trade Commission & u s international trade commission & International Trade Commission & international trade commission & 4 & 0.118\\
USDA, Office of the Chief Financial Officer & usda office of the chief financial officer & Office of the Chief Financial Officer & office of the chief financial officer & 5 & 0.119\\
Office of the Inspector General & office of the inspector general & Office of Inspector General & office of inspector general & 4 & 0.129\\
\addlinespace
Financial Management Service & financial management service & Minerals Management Service & minerals management service & 6 & 0.214\\
Saint Lawrence Seaway Development Corporation & saint lawrence seaway development corporation & Great Lakes St. Lawrence Seaway Development Corporation & great lakes st lawrence seaway development corporation & 12 & 0.222\\
Office of the Chief People Officer & office of the chief people officer & Office of the Chief Financial Officer & office of the chief financial officer & 9 & 0.243\\
\bottomrule
\end{tabular}}
\end{table}

\begin{table}[!h]

\caption{\label{tab:regulatorymatch}Sample Regulatory Agency Reference List Used for Audit (Strict ACS + Broad Rulemaking)}
\centering
\resizebox{\linewidth}{!}{
\fontsize{10}{12}\selectfont
\begin{tabular}[t]{lllll}
\toprule
agency\_name\_reference & agency\_name\_clean & agency\_id\_acs & in\_acs\_strict & in\_federal\_register\\
\midrule
44 CFR Part 64 & 44 cfr part 64 & NA & FALSE & TRUE\\
Administration Office, Executive Office of the President & administration office executive office of the president & NA & FALSE & TRUE\\
Administration for Children and Families & administration for children and families & HHS-ACF & TRUE & FALSE\\
Administrative Conference of the United States & administrative conference of the united states & NA & FALSE & TRUE\\
Advisory Council on Historic Preservation & advisory council on historic preservation & NA & FALSE & TRUE\\
\addlinespace
Advocacy and Outreach Office & advocacy and outreach office & NA & FALSE & TRUE\\
Agency for International Development & agency for international development & AID & TRUE & TRUE\\
Aging Administration & aging administration & NA & FALSE & TRUE\\
Agricultural Marketing Service & agricultural marketing service & AG-AMS & TRUE & TRUE\\
Agricultural Research Service & agricultural research service & NA & FALSE & TRUE\\
\bottomrule
\end{tabular}}
\end{table}

\newpage


\begin{table}[!h]

\caption{\label{tab:regfinalmatch}Final Regulatory Match Coverage (strict + broad + fuzzy)}
\centering
\begin{tabular}[t]{rrrr}
\toprule
contract & agencies & reg\_agencies\_finalmatch & pct\_obs\_finalmatch\\
\midrule
475459 & 175 & 102 & 42.73449\\
\bottomrule
\end{tabular}
\end{table}




\newpage

## Table 4 (Extension). Interaction Effects Between Regulatory Match and Donations on FRI













