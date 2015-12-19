\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{uebblatt}[2013/03/29 LaTeX class]

\DeclareOption{entwurf}{\AtEndOfClass{
  \RequirePackage{draftwatermark}
  \definecolor{pink}{rgb}{0.95,0.9,0.95}
  \SetWatermarkText{\textsf{\textcolor{pink}{ENTWURF}}}
  \SetWatermarkScale{1}
}}

\ProcessOptions\relax

\LoadClass[a4paper,ngerman]{scrartcl}

\RequirePackage{ifxetex}
\ifxetex\else\RequirePackage[utf8]{inputenc}\fi
\RequirePackage[ngerman]{babel}
\RequirePackage{amsmath,amsthm,amssymb,amscd,stmaryrd,color,graphicx,environ,tabto}
\RequirePackage[all,2cell]{xy}
\RequirePackage{mathtools}
\RequirePackage{framed}
\RequirePackage[protrusion=true,expansion=true]{microtype}
\RequirePackage{multicol}
\RequirePackage{lmodern}

\RequirePackage{minted}
\newminted{haskell}{}
\definecolor{light-gray}{gray}{0.91}
\newmintinline{haskell}{fontsize=\footnotesize,bgcolor=light-gray}
\newmintinline{text}{fontsize=\footnotesize,bgcolor=light-gray}
\usemintedstyle{tango}

\RequirePackage{geometry}
\geometry{tmargin=2cm,bmargin=2cm,lmargin=3.1cm,rmargin=3.1cm}

\RequirePackage{hyperref}

\RequirePackage{tikz}
\usetikzlibrary{calc,shapes.callouts,shapes.arrows}
\newcommand{\hcancel}[5]{%
    \tikz[baseline=(tocancel.base)]{
        \node[inner sep=0pt,outer sep=0pt] (tocancel) {#1};
        \draw[red, line width=0.3mm] ($(tocancel.south west)+(#2,#3)$) -- ($(tocancel.north east)+(#4,#5)$);
    }%
}

\setlength\parskip{\medskipamount}
\setlength\parindent{0pt}

\newlength{\titleskip}
\setlength{\titleskip}{1.5em}
\renewcommand{\maketitle}[2]{%
  \thispagestyle{empty}%
  \begin{minipage}[b]{0.6 \linewidth}
    Erster Haskell-Workshop \\
    im OpenLab Augsburg \\
    {\footnotesize \href{http://github.com/curry-club-aux/haskell-workshop}{github.com/curry-club-aux/haskell-workshop}}
  \end{minipage}
  \hfill \includegraphics[width=6cm]{curry-club-logo.png}
  \vspace{0.3cm}

  \begin{center}
    \Large \textbf{Übungsblatt zu Haskell} \\[1em]
    \normalsize #2
  \end{center}
  \vspace{\titleskip}}

\renewcommand*\theenumi{\alph{enumi}}
\renewcommand*\theenumii{\arabic{enumii}}
\renewcommand{\labelenumi}{\theenumi)}
\renewcommand{\labelenumii}{\theenumii.}

\newlength{\aufgabenskip}
\setlength{\aufgabenskip}{1.5em}
\newcounter{aufgabennummer}
\newenvironment{aufgabe}[1]{
  \addtocounter{aufgabennummer}{1}
  \textbf{Aufgabe \theaufgabennummer{}.} \emph{#1} \par
}{\vspace{\aufgabenskip}}
\newenvironment{aufgabeE}[1]{\begin{aufgabe}{#1}\begin{enumerate}}{\end{enumerate}\end{aufgabe}}

\newcommand{\R}{\mathbb{R}}
\newcommand{\N}{\mathbb{N}}
\newcommand{\Z}{\mathbb{Z}}

\newcommand{\ghci}{\textcolor{lightgray}{Prelude>}}

\clubpenalty=10000
\widowpenalty=10000
\displaywidowpenalty=10000
