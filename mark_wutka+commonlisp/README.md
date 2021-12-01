# Advent of Code 2021 - Common Lisp

I am using SBCL (Steel Bank Common Lisp) on Linux and MacOS again this year.
Make sure you have [Quicklisp](https://www.quicklisp.org/beta/) installed.

As I did last year, rather than printing out results I just make functions
that return the result, usually named day*n*a and day*n*b. While I am
usually running them in Emacs with SLIME mode, you can also load them into
SBCL from the command-line and invoke the function from SBCL:
```
sbcl --load day1.lisp
* (day1a)
```

I typically use [Emacs SLIME](https://common-lisp.net/project/slime/) to
develop, as it gives me an interactive environment. One thing, though,
is that if you use control-c-control-k to compile the buffer and it
uses Quicklisp to load a package, the compile may fail the first time
you try to load it because the compiler doesn't invoke the ql:quickload
at the beginning of the file. Instead, the first time you open a file
with SLIME, if you do control-c-control-l to load the file instead of
compiling it, it will run the ql:quickload invocations at the beginning
of the file. (If anyone knows a smoother way to do this, please let me
know, I'm neither a Common Lisp nor a Quicklisp nor
a SLIME expert.

If you have questions/commands e-mail me at <mark@wutka.com> or on the
NashFP mailing list or Slack.  
