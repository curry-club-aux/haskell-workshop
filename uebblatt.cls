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
  \pagestyle{empty}%
  Universität Augsburg \hfill
  Prof. Marc Nieper-Wißkirchen \\
  Lehrstuhl für Algebra und Zahlentheorie \hfill
  Ingo Blechschmidt \\
  Wintersemester 2015/2016 \\[0em]

  \begin{center}
    \Large \textbf{Übungsblatt #1 zur Kommutativen Algebra} \\[1em]
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
\newenvironment{aufgabe}[2]{
  \addtocounter{aufgabennummer}{1}
  \textbf{Aufgabe \theaufgabennummer{}.} (#1) \emph{#2} \par
}{\vspace{\aufgabenskip}}
\newenvironment{aufgabe*}[1]{
  \addtocounter{aufgabennummer}{1}
  \textbf{Aufgabe \theaufgabennummer{}.} \emph{#1} \par
}{\vspace{\aufgabenskip}}
\newenvironment{aufgabeE}[1]{\begin{aufgabe}{#1}\begin{enumerate}}{\end{enumerate}\end{aufgabe}}

\clubpenalty=10000
\widowpenalty=10000
\displaywidowpenalty=10000

\newcommand{\lra}{\longrightarrow}
\newcommand{\lhra}{\ensuremath{\lhook\joinrel\relbar\joinrel\rightarrow}}
\newcommand{\thlra}{\relbar\joinrel\twoheadrightarrow}
\newcommand{\xra}[1]{\xrightarrow{#1}}

\newcommand{\ZZ}{\mathbb{Z}}
\newcommand{\FF}{\mathbb{F}}
\newcommand{\PP}{\mathbb{P}}
\renewcommand{\AA}{\mathbb{A}}
\newcommand{\A}{\mathcal{A}}
\newcommand{\B}{\mathcal{B}}
\newcommand{\C}{\mathcal{C}}
\newcommand{\D}{\mathcal{D}}
\newcommand{\E}{\mathcal{E}}
\newcommand{\F}{\mathcal{F}}
\newcommand{\G}{\mathcal{G}}
\newcommand{\M}{\mathcal{M}}
\renewcommand{\L}{\mathcal{L}}
\newcommand{\T}{\mathcal{T}}
\newcommand{\U}{\mathcal{U}}
\newcommand{\V}{\mathcal{V}}
\newcommand{\W}{\mathcal{W}}
\newcommand{\I}{\mathcal{I}}
\newcommand{\J}{\mathcal{J}}
\renewcommand{\H}{\mathcal{H}}
\renewcommand{\O}{\mathcal{O}}
\newcommand{\K}{\mathcal{K}}
\renewcommand{\P}{\mathcal{P}}
\newcommand{\R}{\mathcal{R}}
\renewcommand{\S}{\mathcal{S}}
\newcommand{\NN}{\mathbb{N}}
\newcommand{\LL}{\mathbb{L}}
\newcommand{\RR}{\mathbb{R}}
\newcommand{\QQ}{\mathbb{Q}}
\newcommand{\CC}{\mathbb{C}}
\newcommand{\GG}{\mathbb{G}}
\renewcommand{\SS}{\mathbb{S}}
\newcommand{\aaa}{\mathfrak{a}}
\newcommand{\bbb}{\mathfrak{b}}
\newcommand{\ppp}{\mathfrak{p}}
\newcommand{\qqq}{\mathfrak{q}}
\newcommand{\mmm}{\mathfrak{m}}
\newcommand{\nnn}{\mathfrak{n}}
\newcommand{\End}{\mathrm{End}}
\newcommand{\Ext}{\mathrm{Ext}}
\newcommand{\Tor}{\mathrm{Tor}}
\newcommand{\Hom}{\mathrm{Hom}}
\newcommand{\HOM}{\mathcal{H}\mathrm{om}}
\newcommand{\id}{\mathrm{id}}
\newcommand{\Id}{\mathrm{Id}}
\newcommand{\Aut}{\mathrm{Aut}}
\newcommand{\GL}{\mathrm{GL}}
\newcommand{\placeholder}{\underline{\quad}}
\newcommand{\smallplaceholder}{\underline{\ \ }}
\newcommand{\ul}[1]{\underline{#1}}
\newcommand{\Set}{\mathrm{Set}}
\newcommand{\Ab}{\mathrm{Ab}}
\newcommand{\Grp}{\mathrm{Grp}}
\newcommand{\Vect}{\mathrm{Vect}}
\newcommand{\Sh}{\mathrm{Sh}}
\newcommand{\PSh}{\mathrm{PSh}}
\newcommand{\AbSh}{\mathrm{AbSh}}
\newcommand{\AbPSh}{\mathrm{AbPSh}}
\newcommand{\Zar}{\mathrm{Zar}}
\newcommand{\Sch}{\mathrm{Sch}}
\newcommand{\sSet}{\mathrm{sSet}}
\newcommand{\Top}{\mathrm{Top}}
\newcommand{\op}{\mathrm{op}}
\newcommand{\pr}{\mathrm{pr}}
\newcommand{\Mod}{\mathrm{Mod}}
\newcommand{\lMod}[1]{#1\text{-}\mathrm{Mod}}
\newcommand{\rMod}[1]{\mathrm{Mod}\text{-}#1}
\newcommand{\Kom}{\mathrm{Kom}}
\newcommand{\Komb}{\mathrm{Kom}^\mathrm{b}}
\newcommand{\Met}{\mathrm{Met}}
\newcommand{\pt}{\mathrm{pt}}
\newcommand{\Ho}{\mathrm{Ho}}
\newcommand{\Cof}{\mathrm{Cof}}
\DeclareMathOperator{\Mor}{Mor}
\DeclareMathOperator{\sk}{sk}
\DeclareMathOperator{\Tr}{Tr}
\DeclareMathOperator{\Spec}{Spec}
\DeclareMathOperator*{\colim}{colim}
\DeclareMathOperator{\rank}{rank}
\DeclareMathOperator{\Ann}{Ann}
\DeclareMathOperator{\Int}{int}
\DeclareMathOperator{\Clos}{cl}
\DeclareMathOperator{\Kernel}{ker}
\DeclareMathOperator{\Coker}{coker}
\DeclareMathOperator{\Image}{im}
\DeclareMathOperator{\Ob}{Ob}
\DeclareMathOperator{\Sing}{Sing}
\newcommand{\Open}{\mathrm{Op}}
\newcommand{\?}{\,{:}\,}
\renewcommand{\_}{\mathpunct{.}\,}
\newcommand{\speak}[1]{\ulcorner\text{\textnormal{#1}}\urcorner}
\newcommand{\Ll}{:\Longleftrightarrow}
\newcommand{\notat}[1]{{!#1}}
\newcommand{\brak}[1]{{\llbracket{#1}\rrbracket}}
\newcommand{\defeq}{\vcentcolon=}
\newcommand{\ps}[1]{{#1}\llbracket{X}\rrbracket}

\newcommand{\stackhref}[1]{\href{http://stacks.math.columbia.edu/tag/#1}{#1}}
