* [packages used in here](#packages-used-in-here)
	* [ctex](#ctex)
	* [hyperref](#hyperref)
	* [keyval](#keyval)
	* [bookmark](#bookmark)
	* [pdfpages](#pdfpages)
	* [xcolor](#xcolor)
	* [makeidx](#makeidx)
	* [geomety](#geomety)
	* [marginnote](#marginnote)
	* [listings](#listings)
	* [graphicx](#graphicx)
	* [comment](#comment)
	* [titlesec](#titlesec)
	* [amsthm amsmath amsfonts mathrsfs](#amsthm-amsmath-amsfonts-mathrsfs)
	* [float](#float)
	* [sidecap](#sidecap)
	* [wrapfig](#wrapfig)
	* [subcaption](#subcaption)

## packages used in here

sorting by adding date.

### ctex

**ctex** package provide chinese fonts support in LaTEX, and the default option for this package is
`ÙTF8`, which set default encoding to **UTF8**.

### hyperref

It provide **hyperlink** in LaTEX output file.

### keyval

useful when you need define a command or environment with some assigned arguments.

### bookmark

<div style="color: rgb(60, 186, 84)">Dependentcies</div> : **hyperref**

It can bookmark in document, and basic usage:

``` LaTeX
% setup commands
\bookmarksetup{<option>}
\bookmarksetupnext{<options>} % Only affect next \bookmark command

% used to bookmark
\bookmark{<option>}{title}
```

### pdfpages

It provide commands that can include whole pdfpage to single page

<div style="color: rgb(219, 50, 54)">Basic Usage:</div>  
``` LaTeX
\includepdf[<options>]{<file>}
```

### xcolor

It provide **color** support in LaTeX

<div style="color: rgb(219, 50, 54)">Some commands:</dev>  

* `\definecolor{<color name>}{<RGB or somes>}{<color value>}` define colors with this.
* `\color{<color name>}` used to apply color in documents.

### makeidx

It provide functional indexmake mechanism.

### geomety

Adjust page size, margin, head, foot etc, very amazing package

### marginnote

page margin note

### listings

**listings** system, which support many kind of options for adjusting 
**listing** format.

``` LaTeX
% Inline code
\lstinline|<code>|

% paragragh code
\lstlisting{<option>}

% file
\lstinputlisting[<options>]{<file>}`
```

### graphicx

This package provides an alternative interface to the LaTeX graphics functions.

<div style="color: rgb(219, 50, 54)">graphics usages:</div>  

+ `\includegraphics *[<key-val list>]{<file}`
+ `\includegraphics *[<llx, lly>][<urx, ury>]{<file>}`

some **key-val** options:

| Keys | Desc |
|:-----:-----:|
|width | width of the image, default unit is **bp**|
|height | height of the image |
|totalheight | height + depth |
|scale | scale factor |
|angle | rotate angle |

<div style="color:rgb(244, 194, 13);font-size: 110%">
`\rotatebox[<key-val list>]{<angle>}{<text>}`
</div>

### comment

This package provides comment environment.

``` LaTeX
\begin{comment}
A lot of things
\end{comment}
```

### titlesec

set format of different level titles.

### amsthm amsmath amsfonts mathrsfs

mathematic things

### float

provide float object, `\begin{figure} ... \end{figure}`.

### sidecap

side capture

### wrapfig
wrap figure

### subcaption

sub caption
