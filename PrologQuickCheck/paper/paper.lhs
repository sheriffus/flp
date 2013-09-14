% -*- mode: latex; mode: folding -*-
\documentclass[runningheads,a4paper]{../../PaperStyles/llncs}
%% \documentclass[authoryear,9pt,preprint]{../../PaperStyles/acm_proc_article-sp}
%% \documentclass[authoryear,9pt,preprint]{../../PaperStyles/sigplanconf}

%include lhs2TeX.fmt
%include lhs2TeX.sty

%include polycode.fmt
%include paper.fmt

%include header.tex


\begin{document}
%%%%%%%%%%%
\newcommand{\papertitle}{{PrologCheck} -- property-based testing in {Prolog}}

\mainmatter  % start of an individual contribution

% first the title is needed
\title{\papertitle}

% a short form should be given in case it is too long for the running head
%% \titlerunning{Lecture Notes in Computer Science: Authors' Instructions}

\author{
  Cl\'audio Amaral$^{1,2}$ \and
  M\'ario Florido$^{1,2}$ \and
  V\'itor Santos Costa$^{1,3}$
}
\authorrunning{Cl\'audio Amaral \and M\'ario Florido \and V\'itor Santos Costa}

% the affiliations are given next; don't give your e-mail address
% unless you accept that it will be published
\institute{
DCC - Faculty of Science, University of Porto
\and LIACC - University of Porto \and CRACS - University of Porto
\\
\url{{coa,amf,vsc}@@dcc.fc.up.pt}}




%% \numberofauthors{3}

%% \author{
%% %% \conferenceinfo{Conf'13,} {Month 20, 2013, City, Country.}
%% %% \CopyrightYear{2013}
%% %% %\copyrightdata{978-1-4503-0860-1/11/09} %TBD
%% \alignauthor
%% Cl\'audio Amaral \\
%%        \affaddr{DCC - Faculty of Science, University of Porto}\\
%%        \affaddr{LIACC - University of Porto}\\
%%        \email{coa@@dcc.fc.up.pt}
%% \alignauthor
%% M\'ario Florido \\
%%        \affaddr{DCC - Faculty of Science, University of Porto}\\
%%        \affaddr{LIACC - University of Porto}\\
%%        \email{amf@@dcc.fc.up.pt}
%% \alignauthor
%% V\'itor Santos Costa \\
%%        \affaddr{DCC - Faculty of Science, University of Porto}\\
%%        \affaddr{CRACS - University of Porto}\\
%%        \email{vsc@@dcc.fc.up.pt}
%% }

%% \toctitle{Lecture Notes in Computer Science}
%% \tocauthor{Authors' Instructions}

\maketitle
\begin{abstract}
%include abstract.tex
\keywords{Prolog, Property-based Testing, Automatic Testing, Testing,
  Verification, Logic Programming, Predicate Specification}
\end{abstract}

%% % A category with the (minimum) three required fields
%% \category{H.4}{Information Systems Applications}{Miscellaneous}
%% % A category including the fourth, optional field follows...
%% \category{D.2.8}{Software Engineering}{Metrics}[complexity measures, performance measures]

%% \terms{Theory}

%% \keywords{ACM proceedings, \LaTeX, text tagging} % NOT required for Proceedings

%% \category{D.1.1}{Programming Techniques}{Applicative (Functional) Programming}
%% \category{D.1.6}{Programming Techniques}{Logic Programming}
%% \category{D.2.5}{Testing and Debugging}{Testing tools}
%% \category{F.3.3}{Logics and Meanings of Programs}{Studies of Program Constructs}[Type Structure]
%% \category{F.4.1}{Mathematical Logic and Formal Languages}{Mathematical Logic}

%% \terms{Languages, Verification}

%% \keywords{Prolog, Property-based Testing, Automatic Testing, Testing, Verification, Logic Programming}
% Metaprogramming, Domain Specific Languages

%include secIntro.tex
%include secRelWork.tex
%include secPlqc.tex
%include secPredSpec.tex
%include secImplementation.tex
%include secCaseStudies.tex
%include secConclusion.tex
%% %include sec3.tex
%% %include sec4.tex
%% %include sec5.tex
%% %include sec6.tex


\bibliographystyle{abbrv}
%% \bibliographystyle{abbrvnat}
\bibliography{plqc}
%% \bibliography{../../../gitroot/bibtex/jp,../../../gitroot/bibtex/genprog,../../../gitroot/bibtex/misc}


\end{document}
