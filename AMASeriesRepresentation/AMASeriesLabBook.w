 \UseRawInputEncoding
\documentclass[hyperref,idxtotoc]{labbook}
\usepackage{moreverb}
\usepackage{natbib}
\usepackage{hyperref}
\usepackage[space]{grffile}
\usepackage{graphicx}
\usepackage{enumitem}
\usepackage{color}
\usepackage{qtree}

\begin{document}

\frontmatter
\title{AMA Series}
\author{Gary Anderson }
\maketitle


\tableofcontents
\newexperiment{refactor}{Refactor Existing Code}

\newexperiment{parallelize}{Paralellize Existing Code}
\newexperiment{components}{Improve other component packages}
\newexperiment{publication}{Work on  Journal Articles}
\newexperiment{SeriesPaper}{AMASeries Representation Paper}
\newsubexperiment{SeriesGlossary}{AMASeries Representation Paper Glossary}
\newsubexperiment{restoreFunctionality}{Restore AMASeriesRepresentation Functionality}
\newexperiment{HPC}{High Performance Computing}
\newsubexperiment{GPUs}{Graphics Processing Units}
\newsubexperiment{FPGA}{Field Programmable Graphic Arrays}
\newsubexperiment{Glascow}{December 5, Glascow Presentation}
\newsubexperiment{toJulia}{Convert AMASeries Code to Julia}
\newsubexperiment{MmaGPUs}{Mathematica and GPUs}
\newexperiment{organize}{Generally Better Organize Things}
\newsubexperiment{orgLabBook}{Generally Better Organize Labbook}

\newsubexperiment{wstp}{Mathematica WSTP protocol for connecting to ``C''}


\mainmatter

\labday{Tuesday August 2O, 2019}

\experiment{refactor}

\begin{itemize}
\item redo labbook
\end{itemize}



@o genLabbookPDF.sh
@{
#!/bin/bash
nuweb  AMASeriesLabBook
pdflatex  AMASeriesLabBook
makeindex  AMASeriesLabBook
bibtex  AMASeriesLabBook
nuweb  AMASeriesLabBook
pdflatex  AMASeriesLabBook
pdflatex  AMASeriesLabBook
@}

\subexperiment{restoreFunctionality}
\begin{verbatim}
garyyrag@@garyyrag:~/git/AMASeriesRepresentation/AMASeriesRepresentation$ ls -ltr *.m* *.wl
ls: cannot access '*.wl': No such file or directory
-rw-r--r-- 1 garyyrag garyyrag 24560 Jun 22 00:34  AMAFedsBetterRBC.mth
-rw-r--r-- 1 garyyrag garyyrag  1717 Jun 22 00:34  csCompare.mth
-rw-r--r-- 1 garyyrag garyyrag  2749 Jun 22 00:34  createNewFuncs.mth
-rw-r--r-- 1 garyyrag garyyrag   551 Jun 22 00:34  chkConv.mth
-rw-r--r-- 1 garyyrag garyyrag  3700 Jun 22 00:34  canonicalNK.mth
-rw-r--r-- 1 garyyrag garyyrag  7303 Jun 22 00:34  buildUp.mth
-rw-r--r-- 1 garyyrag garyyrag 11176 Jun 22 00:34  betterRBCTrips.m
-rw-r--r-- 1 garyyrag garyyrag  4226 Jun 22 00:34  assessFEDS.mth
-rw-r--r-- 1 garyyrag garyyrag  4800 Jun 22 00:34  applyErrFormulaUnknown.mth
-rw-r--r-- 1 garyyrag garyyrag  4597 Jun 22 00:34  applyErrFormula.mth
-rw-r--r-- 1 garyyrag garyyrag  4217 Jun 22 00:34  applyErrFormulaCS.mth
-rw-r--r-- 1 garyyrag garyyrag  1747 Jun 22 00:34  anotherTryScript.mth
-rw-r--r-- 1 garyyrag garyyrag   457 Jun 22 00:34  doRegime.mth
-rw-r--r-- 1 garyyrag garyyrag 15965 Jun 22 00:34  doQL.mth
-rw-r--r-- 1 garyyrag garyyrag  4451 Jun 22 00:34  doHeatMap.mth
-rw-r--r-- 1 garyyrag garyyrag  4365 Jun 22 00:34  solveLargeEta.mth
-rw-r--r-- 1 garyyrag garyyrag  9552 Jun 22 00:34  slowKAdjust.mth
-rw-r--r-- 1 garyyrag garyyrag  9619 Jun 22 00:34  slowKAdjust4.mth
-rw-r--r-- 1 garyyrag garyyrag 13029 Jun 22 00:34  slowKAdjust3.mth
-rw-r--r-- 1 garyyrag garyyrag  2323 Jun 22 00:34  robust.mth
-rw-r--r-- 1 garyyrag garyyrag 10625 Jun 22 00:34  quickRegimes.mth
-rw-r--r-- 1 garyyrag garyyrag  9492 Jun 22 00:34  quasiLinear.m
-rw-r--r-- 1 garyyrag garyyrag  2391 Jun 22 00:34  notRobust.mth
-rw-r--r-- 1 garyyrag garyyrag 11913 Jun 22 00:34  nkZLB.m
-rw-r--r-- 1 garyyrag garyyrag  1706 Jun 22 00:34  judd2014Calcs.mth
-rw-r--r-- 1 garyyrag garyyrag  1508 Jun 22 00:34  harderRBC.mth
-rw-r--r-- 1 garyyrag garyyrag  1477 Jun 22 00:34  getResultsScaffold.mth
-rw-r--r-- 1 garyyrag garyyrag  3071 Jun 22 00:34  getResults.mth
-rw-r--r-- 1 garyyrag garyyrag  7496 Jun 22 00:34  getResultsCS.mth
-rw-r--r-- 1 garyyrag garyyrag  1489 Jun 22 00:34  getResultsApproxScaffold.mth
-rw-r--r-- 1 garyyrag garyyrag  1827 Jun 22 00:34  getResultsApprox.mth
-rw-r--r-- 1 garyyrag garyyrag  1537 Jun 22 00:34  forFinish.mth
-rw-r--r-- 1 garyyrag garyyrag  5832 Jun 22 00:34  fixCompSlackNow.mth
-rw-r--r-- 1 garyyrag garyyrag  6015 Jun 22 00:34  finishPaper.mth
-rw-r--r-- 1 garyyrag garyyrag  5355 Jun 22 00:34  verifyConv.mth
-rw-r--r-- 1 garyyrag garyyrag  1392 Jun 22 00:34  tryScript.mth
-rw-rw-r-- 1 garyyrag garyyrag  9381 Jun 24 02:37  firstRBCTrips.m
-rw-rw-r-- 1 garyyrag garyyrag  8696 Jun 24 02:37  firstRBCCSTrips.m
-rw-rw-r-- 1 garyyrag garyyrag 78774 Jun 24 02:37  AMASeriesRepresentation.m
-rw-r--r-- 1 garyyrag garyyrag  1898 Jun 24 02:56  varyTheK.mth
-rw-r--r-- 1 garyyrag garyyrag 22561 Aug 20 09:01  whittle.mth
garyyrag@@garyyrag:~/git/AMASeriesRepresentation/AMASeriesRepresentation$ 
\end{verbatim}

\begin{description}[style=nextline]
\item[AMAFedsBetterRBC.mth]

This loads without error
\begin{verbatim}
Get["AMAFedsBetterRBC.mth"]

In[4]:= $ContextPath

Out[4]= {firstRBCTrips`, AMAModel`, NumericAMA`, SymbolicAMA`, 
 
>    AMASeriesRepresentation`, MSNTO`, mathSmolyak`, ProtectedSymbols`, 
 
>    JLink`, URLUtilities`, PacletManager`, System`, Global`}

\end{verbatim}


The following also produce no errors.

\begin{verbatim}
  Print["msnto?"]
Needs["MSNTO`"]
Print["reading first"]
Get["firstRBCTrips`"]
Print["done reading first"]
\end{verbatim}
\end{description}


\begin{verbatim}

In[10]:= $ContextPath

Out[10]= {firstRBCTrips`, AMAModel`, NumericAMA`, SymbolicAMA`, 
 
>    AMASeriesRepresentation`, MSNTO`, mathSmolyak`, ProtectedSymbols`, 
 
>    JLink`, URLUtilities`, PacletManager`, System`, Global`}


\end{verbatim}

firstRBCTrips` loads, but this fails:
\begin{verbatim}
firstRBCGenModel[0.36,0.95,1,0,0.95,0.02];
\end{verbatim}

first error
\begin{verbatim}

Thread::tdlen: Objects of unequal length in {} + {0., 0., -0.948593, 0., 0.}
     cannot be combined.

\end{verbatim}

Third component of linModFirstRBCTrips corrupted
\begin{verbatim}
Inverse[{{} + {0., 0., -0.948593, 0., 0.}, 
 
>      {} + {0., 0.548964, 0.548964, 0., 0.}, 
 
>      {} + {0., 0., 0., 0.707107, 0.}, {} + {0., 0., 0., 0., 1.}, 
 
>      {} + {-0.688677, 0., 0., 0., 0.}}], 
 
>    {{0., 0., 0., 0., 0., 1.}, 
\end{verbatim}
Function should setup an AMASeriesRepresentation trips model.

symbolicComputeBPhiF called with matrices but fails

B doesn't seem right -- 

Turns out  h is 5 x 15, but b is 6 x 10,  to few large roots


\begin{verbatim}


In[39]:= firstRBCTrips`Private`evlsSymbRE

Out[39]= {0.94981, 0., 0., 0., 0., 0., 0., 0., 0., 0.}

In[43]:= Dimensions[firstRBCTrips`Private`zfSymbRE]

Out[43]= {5, 10}

\end{verbatim}


These seem to work to: no complaints, linMod seems fine
\begin{verbatim}
judd2014Calcs.mth:firstRBCGenModel[0.36,1.0,0.95,1.0,0.95,0.01]
\end{verbatim}
Following is the same in different file
\begin{verbatim}
slowKAdjust3.mth:firstRBCGenModel[0.36,1.0,0.95,1.0,0.95,0.01];
\end{verbatim}
Also works
\begin{verbatim}
solveLargeEta.mth:firstRBCGenModel[0.36,1.0,0.95,2.0,0.95,0.01];
\end{verbatim}
also seems fine
\begin{verbatim}
varyTheK.mth:firstRBCGenModel[0.36,1,0.95,1,0.95,0.01]
\end{verbatim}
also works
\begin{verbatim}
applyErrFormulaUnknown.mth:firstRBCGenModel[0.36,0.95,1,0,0.95,0.02];
\end{verbatim}



\experiment{SeriesPaper}

\subexperiment{SeriesGlossary}



Started glossary on AMASeriesFEDS using all \TeX\ strategy 

\subexperiment{GPUs}

\href{https://nextjournal.com/sdanisch/julia-gpu-programming}{Graphics Processing Unit in julia}

Doesn't find a nvidia capable device

\href{https://www.cyberciti.biz/faq/linux-tell-which-graphics-vga-card-installed/}{how to determin gpu info}

\begin{itemize}
\item     lspci command
\item     lshw command
\item     grep command
\item     update-pciids command
\item     GUI tools such as hardinfo and gnome-system-information command.
\end{itemize}
\begin{verbatim}
 lspci| grep -i --color 'vga\|3d|\|2d'
c1:00.0 VGA compatible controller: NVIDIA Corporation TU102 [GeForce RTX 2080 Ti Rev. A] (rev a1)
\end{verbatim}


\begin{verbatim}
garyyrag@@garyyrag:~/git/AMASeriesRepresentation/AMASeriesRepresentation$ sudo lspci -v -s c1:00.0
[sudo] password for garyyrag: 
c1:00.0 VGA compatible controller: NVIDIA Corporation TU102 [GeForce RTX 2080 Ti Rev. A] (rev a1) (prog-if 00 [VGA controller])
	Subsystem: ASUSTeK Computer Inc. Device 866a
	Flags: bus master, fast devsel, latency 0, IRQ 11, NUMA node 0
	Memory at d7000000 (32-bit, non-prefetchable) [size=16M]
	Memory at c0000000 (64-bit, prefetchable) [size=256M]
	Memory at d0000000 (64-bit, prefetchable) [size=32M]
	I/O ports at b000 [size=128]
	Expansion ROM at 000c0000 [disabled] [size=128K]
	Capabilities: [60] Power Management version 3
	Capabilities: [68] MSI: Enable- Count=1/1 Maskable- 64bit+
	Capabilities: [78] Express Legacy Endpoint, MSI 00
	Capabilities: [100] Virtual Channel
	Capabilities: [250] Latency Tolerance Reporting
	Capabilities: [258] L1 PM Substates
	Capabilities: [128] Power Budgeting <?>
	Capabilities: [420] Advanced Error Reporting
	Capabilities: [600] Vendor Specific Information: ID=0001 Rev=1 Len=024 <?>
	Capabilities: [900] #19
	Capabilities: [bb0] #15
	Kernel modules: nvidiafb, nouveau

\end{verbatim}

\begin{verbatim}
/1                         power          To Be Filled By O.E.M.
garyyrag@@garyyrag:~/git/AMASeriesRepresentation/AMASeriesRepresentation$ sudo lshw -short | grep -i --color display
/0/101/0                   display        TU102 [GeForce RTX 2080 Ti Rev. A]
garyyrag@@garyyrag:~/git/AMASeriesRepresentation/AMASeriesRepresentation$ 
\end{verbatim}


\begin{verbatim}
garyyrag@@garyyrag:~/git/AMASeriesRepresentation/AMASeriesRepresentation$ sudo lshw -class display
  *-display UNCLAIMED
       description: VGA compatible controller
       product: TU102 [GeForce RTX 2080 Ti Rev. A]
       vendor: NVIDIA Corporation
       physical id: 0
       bus info: pci@@0000:c1:00.0
       version: a1
       width: 64 bits
       clock: 33MHz
       capabilities: pm msi pciexpress vga_controller bus_master cap_list
       configuration: latency=0
       resources: memory:d7000000-d7ffffff memory:c0000000-cfffffff memory:d0000000-d1ffffff ioport:b000(size=128) memory:c0000-dffff
garyyrag@@garyyrag:~/git/AMASeriesRepresentation/AMASeriesRepresentation$ 
\end{verbatim}

\begin{verbatim}
 
garyyrag@@garyyrag:~/git/AMASeriesRepresentation/AMASeriesRepresentation$ nvcc -V check
nvcc: NVIDIA (R) Cuda compiler driver
Copyright (c) 2005-2017 NVIDIA Corporation
Built on Fri_Nov__3_21:07:56_CDT_2017
Cuda compilation tools, release 9.1, V9.1.85
\end{verbatim}

Problems with nvidia driver
\begin{verbatim}

garyyrag@@garyyrag:~/git/AMASeriesRepresentation/AMASeriesRepresentation$ nvidia-smi
NVIDIA-SMI has failed because it couldn't communicate with the NVIDIA driver. Make sure that the latest NVIDIA driver is installed and running.

\end{verbatim}

nvidia driver  not loaded

\begin{verbatim}
lsmod |grep nvidia
\end{verbatim}

\href{https://askubuntu.com/questions/1048274/ubuntu-18-04-stopped-working-with-nvidia-drivers}{perhaps this will solve problem}


\href{https://devtalk.nvidia.com/default/topic/1047416/linux/nvidia-driver-is-not-loaded-ubuntu-18-10-/}{I also took these steps}

will try this
\href{https://askubuntu.com/questions/61396/how-do-i-install-the-nvidia-drivers}{how to install on ubuntu 18.04}

\begin{verbatim}
 glxinfo -B
name of display: :1
display: :1  screen: 0
direct rendering: Yes
Extended renderer info (GLX_MESA_query_renderer):
    Vendor: VMware, Inc. (0xffffffff)
    Device: llvmpipe (LLVM 8.0, 256 bits) (0xffffffff)
    Version: 19.0.8
    Accelerated: no
    Video memory: 128609MB
    Unified memory: no
    Preferred profile: core (0x1)
    Max core profile version: 3.3
    Max compat profile version: 3.1
    Max GLES1 profile version: 1.1
    Max GLES[23] profile version: 3.0
OpenGL vendor string: VMware, Inc.
OpenGL renderer string: llvmpipe (LLVM 8.0, 256 bits)
OpenGL core profile version string: 3.3 (Core Profile) Mesa 19.0.8
OpenGL core profile shading language version string: 3.30
OpenGL core profile context flags: (none)
OpenGL core profile profile mask: core profile

OpenGL version string: 3.1 Mesa 19.0.8
OpenGL shading language version string: 1.40
OpenGL context flags: (none)

OpenGL ES profile version string: OpenGL ES 3.0 Mesa 19.0.8
OpenGL ES profile shading language version string: OpenGL ES GLSL ES 3.00

\end{verbatim}

\labday{Wednesday August 21, 2019}

\experiment{HPC}
\subexperiment{GPUs}

\begin{itemize}
\item 
\href{https://ubuntu-mate.community/t/grub-terminal-console-mode-error/7356/8}{Solved initial gpu problems.}  But still at startup still see message

"error: no  video mode activated"
\item will be important to compare julia/mathematica gpu ease of use and speed along with just parallized cpus
\item it appears very important for mathematica to send data  before operating see their CUDADot example
\end{itemize}

\subexperiment{toJulia}
Working through
\href{https://nextjournal.com/sdanisch/julia-gpu-programming}{Graphics Processing Unit in julia}

\begin{itemize}
\item  \href{https://github.com/JuliaAttic/CUDNN.jl}{warning about cudnn availability when building CuArrays but this package is being phased out}
\item working through Pkg.adds and using of packages
\end{itemize}


\subexperiment{MmaGPUs}

\begin{itemize}
\item \href{https://reference.wolfram.com/language/CUDALink/guide/CUDALink.html}{CUDALink`}
\item using CUDALink`, currently CUDAQ[] returns false
\item SystemInformation has ViceoCardPCIInformation Device NVIDIA TU102 GeForce RTX 2080 Ti Rev A(a1)
\item \href{https://mathematica.stackexchange.com/questions/195348/mathematica-12-supported-gpus}{appears to require a paclet update}
\item \href{http://support.wolfram.com/kb/12498}{should be able to do it from with Mma }  The download triggered by CUDAResourceInstall[Update->True]  is going pretty slowly
\item following \href{https://reference.wolfram.com/language/CUDALink/tutorial/Setup.html}{Wolfram CUDALink Setup}
\begin{verbatim}
export NVIDIADriverLibraryPath=/usr/lib/x86_64-linux-gnu/libnvidia-tls.so.430.40
export CUDA_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu/libcuda.so.430.40
\end{verbatim}
\item \href{https://reference.wolfram.com/language/CUDALink/tutorial/Overview.html}{CUDALink Overview}
\end{itemize}


\subexperiment{restoreFunctionality}
\begin{itemize}
\item find tests and build on them for firstRBCTrips  gen mod has 6 arguments state space  before errors dim 5
  \begin{itemize}
  \item whittle.mth dr function has 9=4+4+1 args  I think should be 11 =5+5+1
  \item verifyConv seems to have right dimensionality 
  \item varyTheK.mth 6 args to gen mod
  \item tryScript.mth not model specific. for cluster computing.
  \item slowKAdjust.mth,slowKAdjust4.mth gen has 5 arg 
  \item slowKAdjust4.mth gen has 6 xdim 5
  \item robust.mth  6 gen args 
  \item quickRegimes.mth working with regime switch
  \item quasiLinear not trips
  \item notrobust.mth  6 gen args 
  \item nkZLB.m no triples
  \item judd2014Calcs.mth  gen has 6 args
  \item getResultsScaffold.mth not model specific, cluster code
  \item getResults.mth AMAFedsBetter connection
  \item getResultsCS.mth  trips but complementary slackness application  7 args for gen
  \item getResultsApproxScaffold.mth cluster code
  \item getResultsApprox.mth AMAFedsBetter code
  \item forFinish.mth  cluster code
  \item fixCompSlackNow.mth  compslack code
  \item finishPaper.mth uses betterRBCTrips
  \end{itemize}
Last modified dates.  Recently modified mostg likely to work.

\begin{verbatim}
-rw-r--r-- 1 garyanderson staff  4597 Jun 24 00:59  applyErrFormula.mth
-rw-r--r-- 1 garyanderson staff  4217 Jun 24 00:59  applyErrFormulaCS.mth
-rw-r--r-- 1 garyanderson staff  4800 Jun 24 00:59  applyErrFormulaUnknown.mth
-rw-r--r-- 1 garyanderson staff  4226 Jun 24 00:59  assessFEDS.mth
-rw-r--r-- 1 garyanderson staff   551 Jun 24 00:59  chkConv.mth
-rw-r--r-- 1 garyanderson staff  1508 Jun 24 00:59  harderRBC.mth
-rw-r--r-- 1 garyanderson staff  1706 Jun 24 00:59  judd2014Calcs.mth
-rw-r--r-- 1 garyanderson staff  2391 Jun 24 00:59  notRobust.mth
-rw-r--r-- 1 garyanderson staff  2323 Jun 24 00:59  robust.mth
-rw-r--r-- 1 garyanderson staff  9552 Jun 24 00:59  slowKAdjust.mth
-rw-r--r-- 1 garyanderson staff 13029 Jun 24 00:59  slowKAdjust3.mth
-rw-r--r-- 1 garyanderson staff  9619 Jun 24 00:59  slowKAdjust4.mth
-rw-r--r-- 1 garyanderson staff  4365 Jun 24 00:59  solveLargeEta.mth
-rw-r--r-- 1 garyanderson staff  1392 Jun 24 00:59  tryScript.mth
-rw-r--r-- 1 garyanderson staff  1886 Jun 24 00:59  varyTheK.mth
-rw-r--r-- 1 garyanderson staff  5355 Jun 24 00:59  verifyConv.mth
-rw-r--r-- 1 garyanderson staff  6027 Jun 27 19:05  finishPaper.mth
-rw-r--r-- 1 garyanderson staff 15945 Jun 27 19:20  doQL.mth
\end{verbatim}
\end{itemize}

\begin{description}[style=nextline] 
\item[finishPaper.mth] \ 
  \begin{itemize}
  \item prepSmol works
  \item errBoundInfo[1*{1,1,1},2,10]  has problems
\begin{verbatim}

In[5]:= errBoundInfo[1*{1,1,1},2,10]

LaunchKernels::nodef: 
   Some subkernels are already running. Not launching default kernels again.
done approxAndBound{1, 1, 1}0.0000694

Part::partw: Part 1 of {} does not exist.

Part::partd: Part specification 
    parallelNestGenericIterREInterp[genFRExtFunc, 
\end{verbatim}
  \item bigger number of pts works better but has problems with msnto choice
\begin{verbatim}
errBoundInfo[1*{1,1,1},2,100]
\end{verbatim}
    \begin{itemize}
    \item
\begin{verbatim}
 MSNTO`Private`chooseLower[#1, #2, #3, 0.5] & , 
 
>                {{}, {0.0139635, 0.0159097, 0.0185343}, 
 
>                 {0.171802, 0.93915, -0.0110343}}]
\end{verbatim}
\begin{verbatim}
, 
 
>               MapThread[MSNTO`Private`chooseUpper[#1, #2, #3, 0.5] & , 
 
>                {{}, {0.0139635, 0.0159097, 0.0185343}, 
 
>                 {0.199729, 0.970969, 0.0260343}}]
\end{verbatim}
      chooseLower choose upper fail due to empt
    \end{itemize}
  \end{itemize}
\end{description}

\subexperiment{FPGA}
\begin{itemize}
\item investigate buying a julia/Mathematica oriented FPGA
  \begin{itemize}
  \item \href{https://community.wolfram.com/groups/-/m/t/563691}{ Using Redpitaya board with Mathematica ? } 4 yhears old  and signal processing not many examples or code
  \item
   \href{https://www.freelancer.com/projects/matlab-mathematica/fpga-implementation-using-xilinx-system/}{
FPGA implementation using xilinx system generator}  someone looking to hire programming expertise  (freelancer site)
\item \href{http://simonlab.uchicago.edu/FPGA.html}{U of Chicago development team}  pdf papers describing their work on various projects
  \end{itemize}
\end{itemize}
\labday{Thursday August 22, 2019}

\experiment{organize}
\subexperiment{orgLabBook}
\begin{itemize}
\item read labBook documentation -- available in in notability, 5 pages done
\item get KOMA-script English documentation  -- available in notability
\item emacs code to regenerate labbook in background
\end{itemize}
\experiment{refactor}
\subexperiment{restoreFunctionality}
\begin{itemize}
\item get basic code working
  \begin{itemize}
  \item buildUp.mth works from beginning to end even including Regimes on mac and ubuntu
  \item work on finishPaper.mth  AMAFedsBetterRBC.mth another another good candidate, but different definitions they seem to be ``standalone''
   {\color{red}
    May delete the following:
  \item do approxAndBound step by step in fixMma.wl
    \begin{itemize}
    \item seems to be calling parallelNestGenericIterREInterp missing last arg before opts integer or eval points
    \item calling with numIters=1 leads to calling compiled function with 3 args instead of 13 sounds like calling model equations
    \item buildUp.mth evaluateTripls of frFunc[[1,2]] works
    \item parallelMakeGenericInterpFuncs when using frFunc
      
\begin{verbatim}

theK=2
{dr,cedr}=
   genFRExtFunc, 
   linModBetterRBCTrips,{X0Z0, theK}, 
   eqnsCompiledBetterRBCTrips, sgSpecErg, 
   smolyakInterpolation, {},(*
"xVarRanges"->theRanges,*)"Traditional"->False]



\end{verbatim}
\item following works
\begin{verbatim}

theRes=parallelNestGenericIterREInterp[
   genFRExtFunc, 
   linModBetterRBCTrips,{X0Z0, theK}, 
   eqnsCompiledBetterRBCTrips, sgSpecErg, 
   smolyakInterpolation, {},3,(*
"xVarRanges"->theRanges,*)"Traditional"->False]
\end{verbatim}

    \end{itemize}
}
  \item functions in finishPaper.mth
     \begin{description}
 \item[dirNameString, fNameString]   Seem to work fine
 \item[prepSmol] produced no error/warniing messages
 \item[approxAndBound] parallelNestGenericIterREInterp does not evaluate  missing argument. number of iters or evalpts \label{appthur}
   \begin{itemize}
   \item added numiters to definition and passed down now get ``noSolutionFound''  message
   \end{itemize}
 \item[errBoundInfo] 
 \item[actualErrs] 
 \item[convDiscrep] 
 \item[actErrCP] 
     \end{description}
  \end{itemize}
\item get some tests setup
\item measure coverage
\end{itemize}
\experiment{HPC}
\subexperiment{GPUs}
\begin{itemize}
\item setup comparisons of julia and mathematica
  \begin{itemize}
  \item \href{https://cuda-tutorial.readthedocs.io/en/latest/tutorials/tutorial01/}{Tutorial 01: Say Hello to CUDA}

  \item will need timers for comparisons

    
\item    cudaDeviceSynchronize(); makes sure that main doesn't return before gpu completes


    
\begin{verbatim}
#include "stdio.h"
__global__ void cuda_hello(){
# if __CUDA_ARCH__>=200
    printf("Hello World from GPU!\n");
#endif  
}

int main() {
    cuda_hello<<<1,1>>>();
    cudaDeviceSynchronize();
    return 0;
}

\end{verbatim}
  \end{itemize}

\item identify GPUs components how to measure how julia and Mathematica compare, olthers?
found \href{https://nextjournal.com/sdanisch/julia-gpu-programming#writing-gpu-kernels}{programming GPU}
\item \href{https://juliagpu.github.io/CUDAnative.jl/stable/man/usage.html}{another programming alternative }
  CuArray does not seem to be defined need to do ``using CuArrays''

  Did not actually get CUDnative example to work
\item \href{https://reference.wolfram.com/cloudplatform/CUDALink/tutorial/Overview.html}{CUDALink Mathematica}
\end{itemize}

\labday{Friday August 23, 2019}

\experiment{SeriesPaper}
\begin{itemize}
\item work on glossary entries
  
\item moved all notation related newcommands to end of glossary commented out unused in newcommands.tex file
\end{itemize}

\experiment{refactor}
\subexperiment{restoreFunctionality}
\begin{itemize}
\item continue with approxAndBound function in finishPaper.mth  (see \ref{appthur})  nothing accomplished today
\end{itemize}

   \experiment{HPC}
   \subexperiment{GPUs}
   \begin{itemize}
   \item get computer science parameterization of GPU specs for determining performance  (\href{https://en.wikipedia.org/wiki/General-purpose_computing_on_graphics_processing_units}{wikipedia}
     \begin{itemize}
     \item vectorizing functions of 1,2,3,4 dimensional arguments  (SIMD)
     \item can have multi-level caches 

   \item find/develop some benchmarks for C/Julia/Mathematica for economists


     \end{itemize}
     \item  \href{https://www.nvidia.com/en-gb/geforce/graphics-cards/rtx-2080-ti/}{my GeForce RTX 2080 Ti:} 
     \begin{itemize}
     \item  \href{https://www.techpowerup.com/gpu-specs/geforce-rtx-2080-ti.c3305}{better spec writeup: mentions 544 tensor cores that improvd speed for machine learning}
       
     \item \href{https://www.nvidia.com/en-gb/data-center/tensorcore/}{tensor cores for linear algebra}
     \item newer NVIDIA V100 GPU Powered by Volta Tensor Cores may be better
     \item \href{https://devblogs.nvidia.com/gpu-computing-julia-programming-language/}{julia and tensor flow and gpu's}look into TensorFlow.jl
       
     \item example on site needed update on @@cuda line  ``threads=''
\item
 mathematica systeminformation has 
       \begin{itemize}
       \item total memory 10.7581GB,
       \item  clockrate 1650000, 
       \item core count 2176,
       \item supports compute capabilites 7.5
       \item gpu overlap 1
       \item max block dims (1024,1024,64)
       \item max threads/block 1024
       \item max shared mem per block 49152
       \item total constant memory 65536

       \item warp size 32
         \item maximum pitch  2147483647
\item Maximum Registers Per Block 65536
\item Texture Alignment 512
\item Multiprocessor Count 68
\item Execution Timeout 1
\item Integrated False
\item Can Map Host Memory True
\item Compute Mode "Default"
\item Texture1D Width 131072
\item Texture2D Width 131072
\item Texture2D Height 65536
\item Texture3D Width 16384
\item Texture3D Height 16384
\item Texture3D Depth 16384
\item Texture2D Array Width 32768
\item Texture2D Array Height 32768
\item Texture2D Array Slices 2048
\item Surface Alignment 512
\item Concurrent Kernels True
\item ECC Enabled False
\item TCC Enabled False
       \end{itemize}
   \item 


     \end{itemize}
   \end{itemize}


   \labday{Saturday August 24, 2019}

\experiment{refactor}
\subexperiment{restoreFunctionality}

\experiment{SeriesPaper}

   \experiment{HPC}
   \subexperiment{GPUs}


   @o someJuliaGPUStuff/nvidiaExampleDotCuArray.jl
   @{using CuArrays
     len = 400000
     from=[1.2,4.5,3.3,6.6]
     preX=rand(from, len)
     X = CuArray(preX)
f(x) = 3x^2 + 5x + 2
@@time X .= f.(2 .* X.^2 .+ 6 .* X.^3 .- sqrt.(X))
@}
\begin{itemize}


\item
\href{https://devblogs.nvidia.com/tensor-core-ai-performance-milestones/}{Volta perfomance measures}
\item 
\href{https://www.researchgate.net/publication/323722776_NVIDIA_Tensor_Core_Programmability_Performance_Precision}{requested this Volta Tensor Core research paper today}
\item \href{https://arxiv.org/pdf/1811.09736.pdf}{Accelerating Reduction and Scan Using Tensor Core Units}
\item \href{https://devblogs.nvidia.com/programming-tensor-cores-cuda-9/}{Tensor Core How to}
\item \href{https://developer.nvidia.com/automatic-mixed-precision}{nvidia
    designed to allow mixed precision for speedup}
  \href{https://docs.nvidia.com/deeplearning/sdk/mixed-precision-training/index.html}{an NVIDIA mixed precision training guide}  appears to be designed for 4x4x4 matrices  $D= A*B+C$  each  matrix 4x4  FP16  but C,D can be FP32
\item designed for deep neural nets DNNS  (resolve definitions recurrent and deep)
\item only GEMMs support Tensor Core
\item \href{https://en.wikipedia.org/wiki/Deep_learning}{deep learning -- (deep/recurrent/convolution) neural net}
\end{itemize}

\labday{Wednesday August 28, 2019}
\experiment{refactor}
\subexperiment{SeriesPaper}
\begin{itemize}
\item problems  with LaunchKernels and paralell distribute of definitions
\item code seems to work when these are disabled in finishPaper.mth
\item need to add numIters to errBoundInfo to pass to approxAndBound
\item no definition for genCheckPt or vals or phivals in errBoundInfo (vals not used)
\item actualErrs[tr[[-1,1]],{.4,1.0,0.0}] appears to work where tr comes from second argumentg of approxandbound
\begin{verbatim}

In[13]:= actualErrs[tr[[-1,1]],{.4,1.0,0.0}]

Out[13]= {-0.00858291, -0.00108232, -0.0629335, -0.00643092}

In[14]:= actualErrs[tr2[[-1,1]],{.4,1.0,0.0}]

Out[14]= {-0.00399545, -0.0058075, -0.0145583, -0.000610145}

\end{verbatim}
\item problems calculation solution to some equation for approx when get to {3,2,1} or {2,3,1}
\end{itemize}


\labday{Wednesday August 28, 2019}
\experiment{refactor}
\subexperiment{SeriesPaper}

\labday{Friday August 30, 2019}

\experiment{refactor}
\subexperiment{SeriesPaper}
\begin{itemize}
\item Work through call tree from bottom up
\end{itemize}


\begin{figure}[h]
  \centering
  
%\begin{tikzpicture}
%\tikzset{every tree node/.style={align=center}}  
% {\small
% \Tree [.nestInterp\ref{nestInterp}  !\qsetw{4cm}
% [.doInterp\ref{doInterp}
% [.genFindRootFuncs\ref{genFindRootFuncs} !\qsetw{2cm}
% [.genFindRootWorker\ref{genFindRootWorker} genZsForFindRoot\ref{genZsForFindRoot}
% [.genLilXkZkFunc\ref{genLilXkZkFunc} fSumC\ref{fSumC} genXtOfXtm1\ref{genXtOfXtm1} [.genXtp1OfXt\ref{genXtp1OfXt} ] ] ] ] 
% [.makeInterpFuncs\ref{makeInterpFuncs} !\qsetw{2cm} [.genInterpData\ref{genInterpData} evaluateTriple\ref{evaluateTriple} ] ] [.interpDataToFunc\ref{interpDataToFunc} 
%  ] ] ] 
% }
\framebox{
{\small
\Tree [.nestInterp  !\qsetw{4cm}
[.doInterp
[.genFindRootFuncs !\qsetw{2cm}
[.genFindRootWorker genZsForFindRoot
[.genLilXkZkFunc fSumC genXtOfXtm1 [.genXtp1OfXt ] ] ] ] 
[.makeInterpFuncs !\qsetw{2cm} [.genInterpData evaluateTriple ] ] [.interpDataToFunc 
 ] ] ] 
}}
%\end{tikzpicture}
  \caption{Function Call Tree}\label{calltree}
\end{figure}


   \experiment{HPC}
   \subexperiment{GPUs}
   \begin{itemize}
   \item work out problems with Mathematica GPU
     \begin{itemize}
     \item work through prerequisite packages verify functioning
       \begin{itemize}
       \item CCompilerDriver basic examples work
       \item SymbolicC basic examples work
       \item CUDALink`NVCCCompiler examples work \href{https://reference.wolfram.com/language/CUDALink/ref/NVCCCompiler.html}{but not many examples}
       \item \href{https://reference.wolfram.com/language/CCompilerDriver/tutorial/CreatingExecutable.html#277035586}{CCompilerDriver has problems} but 
         \href{https://mathematica.stackexchange.com/questions/67787/a-bug-fix-to-mathematica-10-0-1-0}{here is a possible fix}
       \end{itemize}

     \end{itemize}

   \end{itemize}

\labday{Saturday August 31, 2019}
\experiment{refactor}
\subexperiment{SeriesPaper}
\begin{itemize}
\item Work through call tree from bottom up
\end{itemize}


   \experiment{HPC}
   \subexperiment{GPUs}
\labday{Friday August 30, 2019}
\experiment{refactor}
\subexperiment{SeriesPaper}
\begin{itemize}
\item Work through call tree from bottom up
\end{itemize}


   \experiment{HPC}
   \subexperiment{GPUs}

\labday{Saturday August 31, 2019}
\experiment{refactor}
\subexperiment{SeriesPaper}
\begin{itemize}
\item Work through call tree from bottom up
\end{itemize}


   \experiment{HPC}
   \subexperiment{GPUs}
   \begin{itemize}
   \item problems getting nvcccompiler and cudaq
   \item I think I need to resolve why CCompilerDriver isn't working
   \end{itemize}

\subexperiment{wstp}
   \begin{itemize}
  \item    \href{https://mathematica.stackexchange.com/questions/91558/what-is-the-difference-between-mathlink-and-wstp-which-one-should-i-use}{``no difference'' wstp math link}
\item \href{https://reference.wolfram.com/language/tutorial/WSTPInterface4.html}{changes for mathlink to wstp}
\item \href{https://community.wolfram.com/groups/-/m/t/295101}{useful dialog between Szabolcs and WRI Steve Wilson around Mma 10 release time}
\item \href{http://library.wolfram.com/infocenter/Books/8516/MathLinkDevelopmentInC.pdf}{pdf instructions for mathlink}
\item \href{https://mathematica.stackexchange.com/questions/154633/portable-makefile-for-wscc-builds}{prtable makefiles reference to wstp and CCompilerDriver}
\item \href{https://mathematica.stackexchange.com/questions/154633/portable-makefile-for-wscc-builds/154651#154651}{another  how to for mathlink}
\item got examples to compile and connect to mathematica
  \begin{itemize}
  \item copied the wstp examples to mathlinkExper dir
  \item modified the makefile to myMake added libuuid dir and adjusted relative paths for build
  \item added libuuid to LD\_LIBRARY\_PATH in .bashrc
  \end{itemize}
\end{itemize}

\labday{Sunday September 1, 2019}
\experiment{refactor}
\subexperiment{SeriesPaper}

\labday{Monday September 2, 2019}
   \experiment{HPC}
   \subexperiment{GPUs}
   \begin{itemize}
   \item \href{https://www.pugetsystems.com/labs/hpc/How-To-Install-CUDA-10-1-on-Ubuntu-19-04-1405/}{implementing 10.1 install}  seemed to compile file except for errors related to volta (to be expected since my geforce 2080 dopesn't have volta?)
     \item mathematica still has problems showing CUDAQ true   now just keeps downloading from server 
     \item \href{https://mathematica.stackexchange.com/questions/3472/why-does-cudaq-from-cudalink-download-data-from-wolfram-servers}{will try some ideas from here}
     \item \href{https://www.tensorflow.org/install/gpu}{tensorflow and tensorco
re not strictly related}
\item \href{https://en.wikipedia.org/wiki/Tensor_processing_unit}{Google is promoting TPU's for ML applications }
     \end{itemize}
     \subexperiment{tf}
     \begin{itemize}
     \item \href{https://www.tensorflow.org/overview/}{TensorFlow overview}
     \item \href{https://www.tensorflow.org/api_docs/python/tf/linalg/eigh}{tensor flow linear algebra: eigenvalues}
     \item \href{https://github.com/pypa/pipenv/issues/2122}{fix path problems for using pip}
     \end{itemize}
     \labday{Tuesday September 4, 2019}
     \experiment{HPC}
     \begin{itemize}
     \item docker seems to work now

\begin{verbatim}
sudo docker run hello-world
Unable to find image 'hello-world:latest' locally
latest: Pulling from library/hello-world
1b930d010525: Pull complete 
Digest: sha256:451ce787d12369c5df2a32c85e5a03d52cbcef6eb3586dd03075f3034f10adcd
Status: Downloaded newer image for hello-world:latest

Hello from Docker!
This message shows that your installation appears to be working correctly.

To generate this message, Docker took the following steps:
 1. The Docker client contacted the Docker daemon.
 2. The Docker daemon pulled the "hello-world" image from the Docker Hub.
    (amd64)
 3. The Docker daemon created a new container from that image which runs the
    executable that produces the output you are currently reading.
 4. The Docker daemon streamed that output to the Docker client, which sent it
    to your terminal.

To try something more ambitious, you can run an Ubuntu container with:
 $ docker run -it ubuntu bash

Share images, automate workflows, and more with a free Docker ID:
 https://hub.docker.com/

For more examples and ideas, visit:
 https://docs.docker.com/get-started/

\end{verbatim}
     \item added docker group so can do

       docker run hello-world

       etc without sudo

     \item \href{https://www.reddit.com/r/docker/comments/5fs3ko/using_docker_to_distribute_consumer_software/}{some reddit observations about docker}  positive vibe about distributing software and testing software
     \item \href{https://www.pugetsystems.com/labs/hpc/How-To-Setup-NVIDIA-Docker-and-NGC-Registry-on-your-Workstation---Part-5-Docker-Performance-and-Resource-Tuning-1119/}{pointer to 5 really good ubuntu docker posts|}
     \end{itemize}

\experiment{GPUs}

\begin{itemize}
\item trying to get CUDA running in mathematica using the steps \href{https://reference.wolfram.com/language/CUDALink/tutorial/Setup.html}{outlined here}
\item
  \includegraphics[width=5.5in]{nvidia-setting.png}

\item Tried to install
\begin{verbatim}
CUDAResourcesInstall["~/Downloads/CUDAResources-Lin64-12.0.303.\
paclet"]
\end{verbatim}
  but gopt the message
\begin{verbatim}
PacletInstall::newervers: A paclet named CUDAResources with a newer 
version number (12.0.346) is already installed. If you wish to 
install an older version, use PacletUninstall to remove the 
existing version first, or call PacletInstall with IgnoreVersion->True.
\end{verbatim}
\item will try newer version for now
\item changing library paths to exclude : additions shifts cudaq to true
{\small
\begin{verbatim}
export NVIDIA_DRIVER_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu/libnvidia-tls.so.430.40
export CUDA_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu/libcuda.so.430.40


\end{verbatim}
}
\item \href{https://www.microway.com/knowledge-center-articles/comparison-of-nvidia-geforce-gpus-and-nvidia-tesla-gpus/}{comparison of GPUs performance Double precision single precision tensor core}
\item \href{https://medium.com/better-programming/install-tensorflow-1-13-on-ubuntu-18-04-with-gpu-support-239b36d29070}{install tensor flow 1.13 with gpu support}
\item \href{https://docs.aws.amazon.com/AmazonECS/latest/developerguide/docker-basics.html}{docker for AWS
\end{itemize}

\bibliographystyle{plainnat}
\bibliography{emds.bib}


\printindex
\end{document}



?