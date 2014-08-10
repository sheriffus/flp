% -*- mode: latex; mode: folding -*-
\documentclass[runningheads,a4paper]{../../PaperStyles/llncs}

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
DCC - Faculty of Sciences, University of Porto
\and LIACC - University of Porto \and CRACS - University of Porto
\\
\url{{coa,amf,vsc}@@dcc.fc.up.pt}}


\maketitle
\begin{abstract}
%include abstract.tex
\end{abstract}



%include secIntro.tex
%include secRelWork.tex
%include secMotEx.tex
%include secPlqc.tex
%include secPredSpec.tex
%include secCaseStudies.tex
%include secConclusion.tex


\bibliographystyle{abbrv}
\bibliography{plqc}


\end{document}
