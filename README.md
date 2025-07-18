# [TFEB.ORG Lisp hax](https://github.com/tfeb/tfeb-lisp-hax "TFEB.org Lisp hax")
This repo contains a collection of small Common Lisp hacks I've written over the last thirty-odd years[^1].  Some of them are genuinely useful, some of them are little more than toys written long ago to prove a point on `comp.lang.lisp`.   Here, a *hack* is something that might be useful *in* a program rather than a [tool](../tfeb-lisp-tools/ "TFEB.ORG Lisp tools") to help manage program construction.  Although they are here bundled together into an ASDF system that's only for convenience: most of them are independent of each other.  I will probably add more over time.

## Contents
- [General](#general)
	- [Modules](#modules)
	- [Portability](#portability)
	- [Zero history](#zero-history)
	- [Versioning](#versioning)
	- [Naming conventions](#naming-conventions)
- [Collecting lists forwards and accumulating: collecting](#collecting-lists-forwards-and-accumulating-collecting)
	- [Collecting a single list: collecting / collect](#collecting-a-single-list-collecting--collect)
	- [Collecting multiple lists: with-collectors](#collecting-multiple-lists-with-collectors)
	- [Notes on collecting / with-collectors](#notes-on-collecting--with-collectors)
	- [General accumulators: with-accumulators](#general-accumulators-with-accumulators)
	- [Notes on with-accumulators](#notes-on-with-accumulators)
	- [Collecting or accumulating multiple values: collecting-values](#collecting-or-accumulating-multiple-values-collecting-values)
	- [Explicit collectors](#explicit-collectors)
	- [Notes on explicit collectors](#notes-on-explicit-collectors)
	- [Accumulating into vectors: with-vector-accumulators](#accumulating-into-vectors-with-vector-accumulators)
	- [The element-type and simple options for with-vector-accumulators](#the-element-type-and-simple-options-for-with-vector-accumulators)
	- [Notes & examples  for with-vector-accumulators](#notes--examples--for-with-vector-accumulators)
	- [Package, module](#package-module)
- [Wrapping method combination: wrapping-standard](#wrapping-method-combination-wrapping-standard)
	- [Performance](#performance)
	- [Package, module](#package-module-1)
- [Applicative iteration: iterate](#applicative-iteration-iterate)
	- [An example of iterating](#an-example-of-iterating)
	- [Notes](#notes)
	- [Package, module](#package-module-2)
- [Local dynamic state: dynamic-state](#local-dynamic-state-dynamic-state)
	- [Package, module](#package-module-3)
- [Memoizing functions: memoize](#memoizing-functions-memoize)
	- [The interface](#the-interface)
	- [Notes](#notes-1)
	- [Package, module](#package-module-4)
- [Abstract and final classes abstract-classes](#abstract-and-final-classes-abstract-classes)
	- [A note on the MOP](#a-note-on-the-mop)
	- [Package, module](#package-module-5)
- [Classes with only one instance: singleton-classes](#classes-with-only-one-instance-singleton-classes)
	- [Notes](#notes-2)
	- [Package, module](#package-module-6)
- [Case-sensitive forms: cs-forms](#case-sensitive-forms-cs-forms)
	- [Package, module](#package-module-7)
- [Reading forms in a package: read-package](#reading-forms-in-a-package-read-package)
	- [Package, module](#package-module-8)
- [Commenting forms: comment-form](#commenting-forms-comment-form)
	- [Package, module](#package-module-9)
- [Tracing macroexpansion: trace-macroexpand](#tracing-macroexpansion-trace-macroexpand)
	- [A simple example](#a-simple-example)
	- [How it works, caveats](#how-it-works-caveats)
	- [The interface](#the-interface-1)
	- [Package, module](#package-module-10)
- [Defining global functions with lexical environments: define-functions](#defining-global-functions-with-lexical-environments-define-functions)
	- [Function specifications](#function-specifications)
	- [Interface macros](#interface-macros)
	- [Package, module](#package-module-11)
- [Local bindings: binding](#local-bindings-binding)
	- [Notes](#notes-3)
	- [Package, module, dependencies](#package-module-dependencies)
- [Special strings: stringtable](#special-strings-stringtable)
	- [Examples](#examples)
	- [Reading special strings](#reading-special-strings)
	- [The interface](#the-interface-2)
	- [Notes](#notes-4)
	- [Package, module, dependencies](#package-module-dependencies-1)
- [Object accessors: object-accessors](#object-accessors-object-accessors)
	- [Package, module](#package-module-12)
- [Decomposing iteration: simple-loops](#decomposing-iteration-simple-loops)
	- [Package, module, dependencies](#package-module-dependencies-2)
- [Simple pattern matching: spam](#simple-pattern-matching-spam)
	- [matching and matchp](#matching-and-matchp)
	- [The predicate constructors](#the-predicate-constructors)
	- [Notes on spam](#notes-on-spam)
	- [Package, module, dependencies](#package-module-dependencies-3)
- [Metatronic macros](#metatronic-macros)
	- [Notes](#notes-5)
	- [Writing more complicated macros](#writing-more-complicated-macros)
	- [Package, module, dependencies](#package-module-dependencies-4)
- [Simple logging: slog](#simple-logging-slog)
	- [Almost a simple example](#almost-a-simple-example)
	- [Log entries](#log-entries)
	- [Logging functions](#logging-functions)
	- [Log destinations and slog-to](#log-destinations-and-slog-to)
	- [Log entry formats](#log-entry-formats)
	- [The logging macro](#the-logging-macro)
	- [File handling](#file-handling)
	- [Checking the log file map is sane](#checking-the-log-file-map-is-sane)
	- [Precision time](#precision-time)
	- [An example: rotating log files](#an-example-rotating-log-files)
	- [Notes](#notes-6)
	- [Package, module, dependencies](#package-module-dependencies-5)
- [Binding multiple values: let-values](#binding-multiple-values-let-values)
	- [Package, module, dependencies](#package-module-dependencies-6)
- [Processing declaration specifiers: process-declarations](#processing-declaration-specifiers-process-declarations)
	- [Terminology](#terminology)
	- [What you can do](#what-you-can-do)
	- [The interface](#the-interface-3)
	- [Examples](#examples-1)
	- [Package, module, dependencies](#package-module-dependencies-7)
- [Small utilities: utilities](#small-utilities-utilities)
	- [Package, module](#package-module-13)

## General
### Modules
Almost all of these hacks are independent modules: they live in their own little packages and are loadable standalone: if you just want one of them then you don't need to drag all the unrelated ones into your environment.  If you put them in the right place, then [`require-module`](https://github.com/tfeb/tfeb-lisp-tools#requiring-modules-with-searching-require-module "require-module") will find and load them for you: this is how I use them.  Some modules have dependencies on other modules: these are documented in the documentation below, and those modules will, if `require-module` was present at compile time, load their dependencies when loaded.

Each of the hacks now has its own little ASDF system declaration which should also declare its dependencies, for people who use ASDF.

The system itself provides `:org.tfeb.hax`: there is no `org.tfeb.hax` package however: each component lives in its own package with names like `org.tfeb.hax.*`.

### Portability
These tools purport to be portable Common Lisp, apart from a couple which depend on [Closer to MOP](https://github.com/pcostanza/closer-mop "Closer to MOP") on some platforms[^2].  If they're not that's either a bug in the tools or a bug in the CL implementation.  In the former case I very definitely want to know, and I am also willing to add workarounds for the latter although I may be unable to test them on implementations I don't use.

### Zero history
Many of these tools have long and varied histories.  However the parts of these histories that are still preserved are entangled with a lot of other code which is not public, so that history is not represented in the publication repo where you are probably reading this.  They're not listed in any sensible order: those up to `read-package` are all variously old and were collected and published here in early 2021: everything after that was just added chronologically, although some of those modules also have long prehistories.

### Versioning
Initially I was using a fairly strict version of [semantic versioning](https://semver.org/ "Semantic versioning"), where the major version number only changed on incompatible changes.  However since the hax are mostly independent this would mean complete new hax could appear with no major version change, and this has happened: `binding` appeared in 1.1.0 and `object-accessors` appeared in 2.1.0.  So in future major version will be *either* a complete new hack *or* an incompatible change to an old one.  New *features* in an existing hack will be a minor version, such as reified collectors which will appear in 2.2.0.

### Naming conventions
All of these tools make use of *domain-structured names*: packages, modules, features and so on have names which start with a DNS domain name in reverse order and then may continue to divide further.  In this case the prefix is `org.tfeb.hax`: `org.tfeb` is the DNS component and `hax` is the division within the DNS part.  See [the TFEB.ORG tools documentation](https://github.com/tfeb/tfeb-lisp-tools#naming-conventions "TFEB.ORG tools / Naming conventions") for a little more on this.

---

## Collecting lists forwards and accumulating: `collecting`
This is the oldest hack I think, dating back at least to 1989 and perhaps before that.  It's also probably the most useful.

It's quite often useful to be able to collect a list, in order, while walking over some possibly-large data structure.  `loop` has a `collect` clause which lets you do this if what you are doing is looping, but you are not always looping, and not everyone is fond of `loop`[^3].  You can pretty easily collect lists *backwards* and then remember to reverse them at the end, but that's frankly ugly and also seemed frighteningly expensive in the 1980s if you wanted to collect a large list.  `collecting` provides two macros which let you collect things into lists in the order in which they were collected.  They use tail-pointers which means they don't have to reverse or search down the lists they are building.

### Collecting a single list: `collecting` / `collect`
**`collecting`** establishes an environment in which there is a locally-defined function called **`collect`** which will collect its argument into a list, which list will be eventually returned from `collecting`.  `collect` is not defined outside the lexical scope of `collecting`.  So, for instance, given

```lisp
(defun find-numbers (l)
  (collecting
    (labels ((findem (it)
               (typecase it
                 (number (collect it))
                 (list
                  (dolist (e it)
                    (findem e))))))
      (findem l))))
```

then

```lisp
 > (find-numbers '(1 2 (3 4) (a b 6)))
(1 2 3 4 6)
```

`collect` returns its argument.

### Collecting multiple lists: `with-collectors`
**`with-collectors`** establishes an environment where multiple named list collectors are available, and will return as many values as there are collectors.  For example, given:

```lisp
(defun classify (l)
  (with-collectors (number other)
    (labels ((findem (it)
               (typecase it
                 (number (number it))
                 (list
                  (dolist (e it)
                    (findem e)))
                 (t (other it)))))
      (findem l))))
```

then

```lisp
 > (classify '(1 2 (3 4) (a b 6)))
(1 2 3 4 6)
(a b)
```

As with `collecting`, the local collector functions return their argument.

### Notes on `collecting` / `with-collectors`
The collection functions – `collect` or the functions defined by `with-collectors` – are declared inline, as are the functions defined by `with-accumulators`, and so should be very quick.  But they *are* local functions[^4]: you can return them.  So this devious trick works, as do tricks like it:

```lisp
(defun devious ()
  (collecting (collect #'collect)))

(defun peculiar (thing collector)
  (funcall (car collector) thing)
  (cdr collector))
```

and now

```lisp
> (let ((c (devious)))
    (peculiar 1 c)
    (peculiar 2 c))
(1 2)
```

It would be possible, perhaps, to gain some efficiency by declaring the collectors `dynamic-extent`, but I've chosen not to do that so the semantics is more coherent.  If you want the efficiency you can always make the appropriate declaration yourself:

```lisp
(with-collectors (c)
  (declare (dynamic-extent (function c)))
  ...)
```

will work, for instance.

`collecting` is older than `with-collectors` by more than a decade I think.  However it has an obvious definition as a shim on top of `with-collectors` and, finally, that now *is* its definition.

See `collect-into` below, which can be handed a local collector function as an argument and will do the right thing.  This means that, for instance this will work:

```lisp
(defun outer (x)
  (collecting
    (inner x #'collect)))

(defun inner (x collector)
  ...
  (collect-into collector ...)
  ...)
```

### General accumulators: `with-accumulators`
**`with-accumulators`** is a variation on the theme of `with-collectors`: it allows you to accumulate things based on any function and a secret accumulator variable.  `with-accumulators` takes a number of accumulator specifications as its first argument.   These can have either a simple form and a more general form which may be extended in future.

The simple form is `(accumulator operatior &optional initially)`.

- `accumulator` is the name of the local function for this accumulator, which takes one argument, the thing to accumulate.
- `operator` is the operator corresponding to the accumulator: this denotes a function which can take either two or no arguments: it is called with no arguments to initialise the variable underlying the accumulator if there is no `initially` value (this is the only case it is called with no arguments), or with the current value of the accumulator and the thing to be accumulated when the local function is called to accumulate something.
- `initially`, if given, is the initial value.  If it is not given the accumulator is initialised by calling the operator function with no arguments.

The more general form is `(accumulator operator &key initially type returner default by)`.

- `accumulator`, `operator` and `initially` are the same as before.
- `type` is a type specification which is used to declare the type of the underlying variable.
- `returner` denotes a function of one argument which, if given, is called with the final value of the accumulator: its return value is used instead of the value of the accumulator.
- `default`, if given, causes the accumulator function to have a single optional argument for which this expression provides the default value.
- `by`, if given, means that the accumulator takes no arguments and steps the value by `by`.  `by` and `default` can't both be given.
- There may be additional keyword arguments in future.

An example: let's say you want to walk some cons tree counting symbols:

```lisp
(defun count-symbols (tree)
  (with-accumulators ((s +))
    ;; equivalent to (s + 0) or (s + :initially 0 :type integer), say
    (labels ((walk (thing)
               (typecase thing
                 (null)
                 (symbol (s 1))
                 (cons (walk (car thing))
                       (walk (cdr thing)))
                 (t))))
      (walk tree))))
```

Then

```lisp
 > (count-symbols '(1 2 foo (bar 3)))
2
```

A more general function can count symbols and conses, with defaults:

```lisp
(defun count-symbols-and-conses (tree)
  (with-accumulators ((s + :default 1)
                      (c + :default 1))
    (labels ((walk (thing)
               (typecase thing
                 (null)
                 (symbol (s))
                 (cons (c)
                       (walk (car thing))
                       (walk (cdr thing)))
                 (t))))
      (walk tree))))
```

Then

```lisp
> (count-symbols-and-conses '(1 2 foo (bar 3)))
2
6
```

The accumulator functions are declared inline and return their argument, which is compatible with `collecting` / `with-collectors`.

### Notes on `with-accumulators`
There is no single-accumulator special case: it didn't seem useful as you need to specify the accumulation operator anyway.

The accumulation operator and returner are *names* which denote functions, not functions: they can be either symbols or a lambda expressions, they can't be functions.  Specifically it needs to be the case that `(operator ...)` is a valid form.  That means that if you do want to use some function you'd need to provide an operator which was, for instance `(lambda (into val) (funcall f into val))`.

Both `by` and `default` are forms which are evaluated at call time in the current lexical and dynamic environment (they're simply the defaults for arguments).  So this will return `3`:

```lisp
(let ((by 1))
  (with-accumulators ((a + :by by))
    (a)
    (setf by 2)
    (a)))
```

`with-accumulators` is very much newer than either of the other two macros, and may be more buggy.  It is certainly the case that new keywords may appear in accumulator specifications.

`with-accumulators` can implement `collecting` or `with-collectors`:

```lisp
(with-accumulators ((collect
                     (lambda (into it)
                       (let ((lit (list it)))
                         (if into
                             (setf (cdr (cdr into)) lit
                                   (cdr into) lit)
                           (setf into (cons lit lit))))
                       into)
                     :initially nil
                     :returner car))
  ...
  (collect something)
  ...)
```

This keeps the state in a cons in the obvious way, and then uses a returner function to return only the interesting bit of the cons.  `collecting` and `with-collectors` are not actually implemented in terms of `with-accumulators`, although they could be.

It's arguably the case that the accumulator functions should return the current value of the accumulator.  This is incompatible with what the list collector functions do, but perhaps might be more useful.  But, in fact, the right thing in that case would be for them to return what the returner function returns (because the accumulator value might be some internal state, as it is with the implementation of a version of `collecting`.  And I wanted to be able to assume that the returner function is called exactly once, so it's allowed to be destructive.

### Collecting or accumulating multiple values: `collecting-values`
Within the `with-collectors` or `with-accumulators` you can use this macro to collect multiple values.  It has two cases:

```lisp
(with-collectors (a b)
  ...
  (collecting-values (a b) (f ...))
  ...)
```

will collect two values from `(f ...)`: it is equivalent (and expands into):

```lisp
(multiple-value-bind (a b) (f ...)
  (a a)
  (b b))
```

On the other hand

```lisp
(with-collectors (a b)
  ...
  (collecting-values (a b)
    (f ...)
    (g ...))
  ...)
```

is equivalent to

```lisp
(multiple-value-bind (a b) (values (f ...) (g ...))
  (a a)
  (b b))
```

`collecting-values` doesn't actually care about whether it's within a suitable form: it just does its thing regardless.

### Explicit collectors
`collecting` and friends were inspired by facilities in Interlisp-D[^5], and specifically by `TCONC`, `DOCOLLECT` and `ENDCOLLECT`.  These collected things by maintaining an explicit cons where the car was the list being collected and the cdr was the last cons of that list.  The nice thing about this is that these conses can be passed around as variables.  So, at long last, here are equivalents of those functions in CL.

**`make-collector`** makes an object which can be used for collecting a list.  It takes two keyword arguments:

- `initial-contents` is the initial contents of the collector, the default being `()`;
- `copy` controls whether the initial contents is copied, with the default being `t`.

If you provide initial contents and ask for it not to be copied the list will be destructively modified.

**`collect-into`** collects into a collector.  It has two positional arguments:

* `collector` is the collector;
* `value` is the object to collect.

It returns its second argument.  If `collector` is a function it will simply call it with the second argument: this means it can be used with the local functions bound by `collecting` / `with-collactors` as well.

**`collector-contents`** returns the contents of a collector: the list being collected by that collector.   It has an optional argument, `appending`: if given this is appended to the value returned, either by using the tail pointer to modify the last cons of the list being built or by simply returning `appending` directly if nothing has been collected.  If `appending` is not given, the collector can still be used after this, and the list returned by `collector-contents` will be destructively modified in that case.  If `appending` is given then the collector is generally junk as the tail pointer is not updated: doing so would involve traversing `appending` and the whole point of this hack is to avoid doing that.  See `nconc-collector-onto` for a function which *does* update the tail pointer.

**`nconc-collectors`** destructively concatenates together one or more collectors, returning the first.   After this is called all of the collectors share a tail pointer and the head pointers of them point at the appropriate places on the combined list.  It is safe to update any one of the collectors, but after doing so the tail pointers of all the remaining ones will inevitably be junk.  So this is most useful as a fast way to, for instance, concatenate a collector onto another after which it is never used again.

**`nconc-collector-onto`** attaches a list to the end of a collector, updating the tail pointer of  the collector to point to the end of the list.  It returns the collector.  The list is not copied by this function, so collecting anything else into the collector will mutate it.  `nconc-collector-onto` necessarily takes time proportional to the length of the list:

```lisp
(nconc-collector-onto c l)
```

is equivalent to

```lisp
(dolist (e l c)
  (collect-into c e))
```

except that no new list structure is created.  See `collector-contents` for a function which does not update the tail pointer.

**`collector-empty-p`** returns true if its argument is a collector which is empty (note it doesn't perform any check that the argument actually is a valid collector object).

**`pop-collector`** returns the first element of a collector, and alters it so that the second element is now the first element.  It can be used to use collectors like queues.

### Notes on explicit collectors
Surprising things can happen if you share a single list between more than one collector without copying it:

```lisp
> (let ((c1 (make-collector)))
    (collect-into c1 1)
    (collect-into c1 2)
    (print (collector-contents c1))
    (let ((c2 (make-collector :initial-contents (collector-contents c1)
                              :copy nil)))
      (collect-into c2 3)
      (print (collector-contents c1))
      (print (collector-contents c2))
      (collect-into c1 4)
      (print (collector-contents c1))
      (print (collector-contents c2))
      (values)))

(1 2)
(1 2 3)
(1 2 3)
(1 2 4)
(1 2 4)
```

Generally you don't want to do this unless you know exactly what you're doing, when it can be, perhaps, useful.

The collector objects made by `make-collector` are conses, but I reserve the right to change their representation in the future: don't assume they will always be conses.

The optional second argument to `collector-contents` is a bit sneaky but can be really useful.

### Accumulating into vectors: `with-vector-accumulators`
`with-vector-accumulators` lets you accumulate elements into vectors painlessly.  A simple way of doing this would be to build a list, and then turn that into a suitable vector at the end.  `with-vector-accumulators` doesn't do that: rather it repeatedly adjusts the vector if needed.  That means it may cons a lot more, but it has options which, if you know how big the vector will be, allow you make it pretty quick[^6].

The syntax is `(with-vector-accumulator (&rest accumulators) ...)`.  Each accumulator can be one of three options:

- a symbol, `name`;
- a list of the form `(name &key for-vector start fill-pointer adjustable length extension element-type simple finalize)`;
- a list of the form `(name initial-value &key ...)` which is equivalent to `(name :for-vector initial-value ...)`.

Here

- `name` is bound to the name of a local function of one argument which will accumulate elements into the vector in order.
- `for-vector` is an initial value for the vector.  In general it will be mutated unless it is not adjustable and has zero length.
- `start` is the index of the first element to accumulate.
- `fill-pointer`is a valid fill pointer for the vector.  If it is a vector index, then it will be used as the first index, unless `start` is given.
- `adjustable`, if true, says the vector should be adjustable.
- `length` is the initial length of the vector, unless `for-vector` is given.  The default is 8.
- `extension` says how much to extend the vector by.  It can be a real greater than 1, or a function designator.  If it is a real the new length is computed as `(max (1+ l) (round (* l extension))`, where `l` is the old length.  This ensures the vector actually grows.  If it is a function designator, then the function is called with the old length and should return a new one.  The result of this isn't checked, but it had better be at least one more than the old length.  The default is 1.5.
- `element-type`specifies the element type of the vector. See [below](#the-element-type-and-simple-options-for-with-vector-accumulators "The element-type and simple options for with-vector-accumulators")
- `simple`says that the array is simple.  See [below](#the-element-type-and-simple-options-for-with-vector-accumulators "The element-type and simple options for with-vector-accumulators").
- `finalize`, default true, says to 'finalize' the vector: this means that it is adjusted to have length equal to one more than the last index.

`with-vector-accumulators` returns all the accumulators as multiple values.

### The `element-type` and `simple` options for `with-vector-accumulators`
These two arguments can be used to improve declarations.  But if you give them in such a way that they have an effect, *they will be trusted*: it's then up to you to make sure that things actually are what is assumed.

**`element-type`**, if given as a quoted expression, will cause array types to be declared as having that element type, and the local function's argument to be appropriately declared.  For instance `(with-vector-accumulators ((a :element-type 'fixnum)) ...)` will result in arrays being declared to have type `(simple-array fixnum (*))` and the local function's argument will be declared to be a `fixnum`.   This argument only does anything if its value is a quoted expression as it's used at macroexpansion time, obviously.

**`simple`** is useful when `for-vector` is provided (and is ignored if `adjustable` or `fill-pointer` are provided as anything other than a literal `nil`).  In a form like `(with-vector-accumulators ((a :for-vector v :element-type 'fixnum)) ...)` then the best declaration that can be assumed for `v` is that it's an `(array fixnum (*))`.  But in `(with-vector-accumulators ((a :for-vector v :simple t :element-type 'fixnum)) ...)` it will be declared as type `(simple-array fixnum (*))`.  `v` had better actually be a `simple-array` in this case.

Use of these options can result in a significant speedup but potentially make code less safe.

### Notes & examples  for `with-vector-accumulators`
A simple case is building a string:

```lisp
(defun alpha-string-from-stream (s)
  ;; Just the longest string of alphanumeric chars we can find
  (with-vector-accumulators ((r :element-type 'character))
    (do ((c (read-char s nil nil) (read-char s nil nil)))
        ((or (not c) (not (alphanumericp c))))
      (r c))))
```

Another example, on a Unixoid machine, might be to accumulate a given number of random numbers into a vector:

```lisp
(defun n-random-unsigned-bytes (n)
  (with-open-file (/dev/random "/dev/random" :element-type '(unsigned-byte 8))
    (with-vector-accumulators ((byte :element-type '(unsigned-byte 8)
                                     :length n
                                     :finalize nil))
      (dotimes (i n)
        (byte (read-byte /dev/random))))))
```

There's no need to finalize the vector because we know how many bytes it will be in advance.

You could of course write the above in the traditional way:

```lisp
(defun n-random-unsigned-bytes/traditional (n)
  (with-open-file (/dev/random "/dev/random" :element-type '(unsigned-byte 8))
    (let ((bytes (make-array n :element-type '(unsigned-byte 8))))
      (dotimes (i n)
        (setf (aref bytes i) (read-byte /dev/random)))
      bytes)))
```

But there are plenty of times where managing the index explicitly like this is inconvenient and you have no idea how many elements you'll find:

```lisp
(defun find-integers (tree)
  ;; For some reason you want a vector
  (with-vector-accumulators ((found :element-type 'integer))
    (labels ((findem (it)
               (typecase it
                 (integer (found it))
                 (list
                  (dolist (e it)
                    (findem e))))))
      (findem tree))))
```

If you want to provide a buffer, you really want drop hints about simpleness and types: don't write

```lisp
(defun something/reusing-buffer (... &key ... (buffer (make-array ...)))
  (with-vector-accumulators ((a buffer ...))
    ...))
```

But rather

```lisp
(defun something/reusing-buffer (... &key ... (buffer (make-array ...)))
  (with-vector-accumulators ((a buffer :simple t :element-type 'fixnum))
    ...))
```

for instance.  Of course the buffer actually has to a actually *be* a simple array[^7] with the appropriate element type.

In my experience arrays which are explicitly adjustable or which have fill pointers suck for performance.  Assuming arrays are contiguous in memory, then `adjust-array` almost certainly has to copy the entire array whenever it's growing an array: any savings from not copying the header are outweighed by the gain from faster element access.

Empirically the things that make `with-vector-accumulators` quick are:

- either don't pass it a vector but let it make one, or if you pass one make sure it is simple and say that it's simple.
- don't provide `fill-pointer` or `adjustable` options;
- tell it the element type as a quoted literal;
- tell it how many elements you will be accumulating if you know that;
- if you don't know, play around with extension and initial length;
- don't finalize if you can avoid it.

Doing these things it's often pretty close in performance to explicit make-a-vector-and-maintain-the-index code.

### Package, module
`collecting` lives in `org.tfeb.hax.collecting` and provides `:org.tfeb.hax.collecting`.  It depends on `utilities`.

## Wrapping method combination: `wrapping-standard`
The standard CLOS method combination defines several sorts of methods, and prescribes the order in which they get called:

1. the most-specific `:around` method, uses `call-next-method` to invoke possible further `:around` methods until it reaches the least-specific `:around` method, for which `call-next-method` invokes ...
	1. ... the most-specific `:before` method to the least-specific `:before` method in order, or ...
	2. ... the most-specific primary method, which can use `call-next-method` to invoke less-specific primary methods, after which ...
	3. ... the least-specific to most-specific `:after` methods run in order ...
2. ... and the remaining parts of the `:around` methods run in most-to-least-specific order.

Sometimes it is useful for a class fairly high up the tree to be able to wrap code around this whole process: for instance a class might want to establish a lock and be very sure that no child class could run code outwith the dynamic extent where the lock was established, or it might be responsible for managing a cache in which it stores the results of the remaining methods  This is what the  `wrapping-standard`method combination provides.

**The **`wrapping-standard`** method combination.** A generic function declared with `wrapping-standard` method combination supports a new method qualifier, `:wrapping`: these methods are exactly like `:around` methods except:

- they run outside `:around` methods;
- they run in *most-specific-last* order, which means that the most distant ancestor class gets to run its method *outside* all other methods.

So the ordering of methods is now


1. the least-specific `:wrapping` method, uses `call-next-method` to invoke further `:wrapping` methods until it reaches the most-specific `:wrapping` method, for  which `call-next-method` invokes ...
	1. the most-specific `:around` method, uses `call-next-method` to invoke possible further `:around` methods until it reaches the least-specific `:around` method, for which `call-next-method` invokes ...
		1. ... the most-specific `:before` method to the least-specific `:before` method in order, or ...
		2. ... the most-specific primary method, which can use `call-next-method` to invoke less-specific primary methods, after which ...
		3. ... the least-specific to most-specific `:after` methods run in order ...
	2. ... and the remaining parts of the `:around` methods run in most-to-least-specific order ...
2. ... and the remaining parts of the `:wrapping` methods run in least-to-most-specific order.

An example: given

```lisp
(defgeneric compute-thing (x &key)
  (:method-combination wrapping-standard))

(defvar *cache* (make-hash-table))

(defmethod compute-thing :wrapping ((x t) &key (force nil) (cache *cache*))
  (if (or force (not (gethash x cache)))
      (setf (gethash x cache) (call-next-method))
    (values (gethash x cache))))

(defmethod compute-thing :wrapping ((x number) &key)
  (multiple-value-prog1
      (progn
        (format t "~&wrapping (number)")
        (call-next-method))
    (format t "~&wrapped (number)~%")))

(defmethod compute-thing :around ((x t) &key)
  (multiple-value-prog1
      (progn
        (format t "~&>around (t)")
        (call-next-method))
    (format t "~&<around (t)~%")))

(defmethod compute-thing :around ((x number) &key)
  (multiple-value-prog1
      (progn
        (format t "~&>around (number)")
        (call-next-method))
    (format t "~&<around (number)")))
```

now

```lisp
 > (compute-thing 1)
wrapping (number)
>around (number)
>around (t)
(I am doing hard sums)
<around (t)
<around (number)
wrapped (number)
1

> (compute-thing 1)
1
```

### Performance
A long time ago I did some benchmarks of `wrapping-standard` and found no observable difference to the standard method combination.  If there are any they are very small.

### Package, module
`wrapping-standard` lives in `org.tfeb.hax.wrapping-standard` and provides `:org.tfeb.hax.wrapping-standard`.

## Applicative iteration: `iterate`
I've always liked Scheme's named-`let` construct.  It's pretty easy to provide a shim around `labels` in CL which is syntactically the same, but since CL doesn't promise to turn tail calls into jumps, it may cause stack overflows.  When I wrote `iterate` I was still using, part of the time, a Symbolics LispM, and they *didn't* turn tail calls into jumps.  So I wrote this little hack which, if it knew that the implementation did not handle tail-call elimination, and if the name of the local function contains `loop` (in any case) will compile 'calls' to it as explicit jumps.  Otherwise it turns them into the obvious `labels` construct.

Well, that's what it used to do: for a while I simply set the flag which controls whether it thinks an implementation supports tail-call elimination unilaterally to true, which means it will always create correct code, even if that code may cause stack overflows on implementations which don't eliminate tail calls[^8].  From 21st August 2021 the old code is now gone altogether (it is still available for inspection in old commits).

From March 2023 there are now four variants on this macro which provide various options of evaluation order and stepping.

**`iterate`** is the original macro: this is just like named-`let` in Scheme.  In particular it binds in parallel.  So

```lisp
(let ((x 1))
  (iterate n ((x 2) (y x))
    (values x y)))
```

evaluates to `2` and `1`.

```lisp
(iterate foo ((x 1)
              (y 2))
  ...
  (foo (+ x 1) (- y 1))
  ...)
```

simply turns into

```lisp
(labels ((foo (x y)
           ...
           (foo (+ x 1) (- y 1))
           ...))
  (foo 1 2))
```

**`iterate*`** is a variation of `iterate` which binds sequentially for the initial binding:

```lisp
(let ((x 1))
  (iterate* n ((x 2) (y x))
    (values x y)))
```

will evaluate to `2` and `2` (and the outer binding of `x` is now unused).  Recursive calls are just function calls and evaluate their arguments as you would expect in both cases.

Additionally there are now two  fancier macros: `iterating` and `iterating*`.  Both of these support a variation of `do`-style stepping arguments: a binding like `(v init)` will bind `v` to `init` and then, by default, step it also to `init`.  A binding like `(v init step)` will bind to `init` and then step it to `step`.  Note that this is *not the same* as `do`: for `do` `(v init)` will bind `v` to `init` and then to whatever its current value is.  To achieve this with `iterating` you need to say `(v init v)`.  See below for some rationale.

The local function now also takes keyword arguments with values which default to the stepping expressions.  So:

```lisp
(iterating n ((n 0 (1+ n)))
  (if (= n 10)
      n
    (n)))
```

Does what you think.  But you can also provide arguments to the local function:

```lisp
(iterating n ((o t) (i 0 (1+ i)))
  (print o)
  (when (< i 10)
    (n :o (if (evenp i) t nil))))
```

will print a succession of `t`s and `nil`s, for instance, `i` gets stepped automatically.

**`iterating`** is like `let` / `do`: all the binding that happens is in parallel.  So in particular, as with `iterate`:

```lisp
(let ((x 1))
  (iterating n ((x 2) (y x))
    (values x y)))
```

evaluates to `2` and `1`.  Similarly variable references in step forms are to the previous value of the variable:

```lisp
(iterating n ((i 2 (1+ i)) (j 1 i))
  (when (< i 10)
    (format t "~&~D ~D~%" i j)
    (n)))
```

will print `2 1` then `3 2` and so on.

**`iterating*`** is like `let*` / `do*`: all the binding that happens is sequential.  So in particular as with `iterate*`:

```lisp
(let ((x 1))
  (iterating* n ((x 2) (y x))
    (values x y)))
```

evaluates to `2`and `2` and the outer binding is unused.  Also, the step forms now refer to the new values of variables to their left:

```lisp
(iterating* n ((i 2 (1+ i)) (j i i))
  (when (< i 10)
    (format t "~&~D ~D~%" i j)
    (n)))
```

prints `2 2`, `3 3` and so on.  This also applies to the optional arguments to the local function:

```lisp
> (iterating* n ((i 2 (1+ i)) (j i i))
    (when (< i 10)
      (format t "~&~D ~D~%" i j)
      (if (evenp i)
          (n (+ i 3))
        (n :i (+ i 1)))))
2 2
5 5
6 6
9 9
```

Combined with `collecting`, `iterate` provides a surprisingly pleasant minimalist framework for walking over data structures in my experience, and i have use it extensively.  `iterate*` , `iterating` and `iterating*` are much newer and may be slightly experimental.

### An example of `iterating`
Here is a simple sieve of Eratosthones:

```lisp
(defun sieve (n)
  (declare (type (integer 2) n))
  (let ((l (isqrt n))
        (a (make-array (+ n 1) :element-type 'bit :initial-element 1)))
    (declare (type (integer 1) l)
             (type simple-bit-vector a))
    (iterating* next ((c 2 (1+ c))
                      (marking (<= c l))
                      (primes '() primes))
      (if (<= c n)
          (if (zerop (bit a c))
              ;; not a prime
              (next)
            ;; A prime
            (if marking
                (do ((m (* c c) (+ m c)))
                    ((> m n) (next :primes (cons c primes)))
                  (setf (bit a m) 0))
              (next :primes (cons c primes))))
        (nreverse primes)))))
```

### Notes
The init and step forms for `iterating` and `iterating*` have different semantics than for `do` and `do*`, but the same semantics as for `doing` from my `simple-loops` hack.  I am not sure that this is better -- simply being different is a bad thing -- but they work they way they do because I've ended up writing too many things which look like

```lisp
(do ((var <huge form> <same huge form>))
    (...)
  ...)
```

which using `iterating` would be

```lisp
(iterating next ((var <huge form>))
  ...
  (next))
```

This is one of the reasons that `iterating` is slightly experimental: I am not sure it's right and I won't know until I have used it more.

`iterating` and `iterating*` need to allow keyword arguments for what appears to be the local recursive function.   Keyword argument parsing is clearly slightly hairy in general, so what they do is expand to something like this (this is the expansion for `iterating`:

```lisp
(labels ((#:n (x)
           (flet ((n (&key ((:x #:x) 2))
                    (#:n #:x)))
             (declare (inline n))
             ...)))
  (#:n 2))
```

where all the gensyms that look the same are the same.  Both functions could have had the same name of course, but that seemed gratuitous, especially for anyone reading the macroexpansion.  The hope is that by using the little ancillary function, which is inlined, the keyword argument parsing will be faster.  However `iterating` & `iterating*` are meant to be expressive at the cost of perhaps being slow in some cases.

`iterating` & `iterating*` went through a brief larval stage where they used optional arguments, but keyword arguments are so much more compelling we decided the possible performance cost was worth paying.

### Package, module
`iterate` lives in `org.tfeb.hax.iterate` and provides `:org.tfeb.hax.iterate`.

## Local dynamic state: `dynamic-state`
Dynamic binding is something you don't want very often, but you always end up wanting it somewhere: when programming in languages such as Python I've ended up having to reinvent dynamic binding[^9].

But quite often what you really want is not *global* special variables – variables which exist at the top-level – but *local* special variables, which exist only in some dynamic scope.  This is easy to do in CL:

```lisp
(defun foo (arg)
  (let ((%error-count% 0))
    (declare (special %error-count%))
    (bar arg)
    (when (> %error-count% 0)
      (format *error-output* "~%~D error~:*~P~%" %error-count%))))

(defun bar (arg)
  (let ((%error-count% 12))
    ;; this binding is lexical
    (spon arg %error-count%)))

(defun spon (arg count)
  (declare (special %error-count%))     ;we want the dynamic variable
  (when (> arg count)
    (incf %error-count%)))
```

Then

```lisp
> (foo 1)
nil

> (foo 13)

1 error
nil
```

I wanted a mechanism to encapsulate this kind of approach to locally establishing one or more special variables, which bindings could then be accessed and mutated by functions possibly far down the call stack, while making sure that only the right variables were bound and accessed.  This is what `define-dynamic-state` does.

**`define-dynamic-state`** defines a pair of macros: one for establishing some dynamic state and one for accessing the same bit of state.  The names of the variables which can be bound in a given piece of dynamic state are known to the macros, which reduces the possibility of errors.  Giving the uses of dynamic state like this names also helps make code clearer.

Assume we want some dynamic state which allows us to keep track of error counts, as above.  We could define that like this:

```lisp
(define-dynamic-state (with-error-count with-error-count-access)
  %error-count%
  %error-threshold%)
```

This defines a pair of macros, `with-error-count` and `with-error-count-access` which are allowed to bind and access two locally special variables: `%error-count%` and `%error-threshold%`: any other variables will be a compile-time error.  We could use this to write this:

```lisp
(defun foo (n)
  (with-error-count ((%error-count% 0)
                     (%error-threshold% 3))
    (bar n)
    (when (> %error-count% %error-threshold%)
      (format *error-output* "~%~D error~:*~P~%" %error-count%))))

(defun bar (n)
  (let ((%error-count% (floor (/ n 2))))
    ;; this binding is lexical
    (loop repeat %error-count%
          do (spon n))))

(defun spon (n)
  (with-error-count-access (%error-count%)
    (loop repeat n
          do (incf %error-count%))))
```

And now

```lisp
> (foo 3)
nil

> (foo 4)

8 errors
nil
```

But, for instance, if you try this you'll get an error at compile-time (really, macroexpansion-time):

```lisp
> (with-error-count ((%errs% 3)) (bar 2))

Error: %errs% is not a valid dynamic state variable for with-error-count
```

`dynamic-state` doesn't do anything you couldn't do already: it just tries to make it a bit clearer and less error-prone, especially if you're using more than one bit of dynamic state.  That's exactly what it was originally written to do.

### Package, module
`dynamic-state` lives in `org.tfeb.hax.dynamic-state` and provides `:org.tfeb.hax.dynamic-state`.

## Memoizing functions: `memoize`
Memoization is a clever trick invented by Donald Michie[^10], and described in [Wikipedia](https://en.wikipedia.org/wiki/Memoization "Memoization").  By remembering the results of calls to the function, it can hugely increase performance of certain kinds of recursive function.  As an example

```lisp
(defun fibonacci (n)
  (cond
   ((= n 0) 0)
   ((= n 1) 1)
   ((> n 1) (+ (fibonacci (- n 1)) (fibonacci (- n 2))))
   ((< n 0) (error "no"))))
```

This function has a time complexity which is exponential in `n`, which means that it's essentially impossible to use it to compute the `n`th term of the Fibonacci sequence for `n` being `100`, say.  But it's also hugely repetitive: it calls itself on the same values an enormous number of times (in fact the number of times it calls itself with a given value is an element of the Fibonacci sequence!).  Well, if we could just remember the last value we could avoid all that.  And we can:

```lisp
> (memoize-function 'fibonacci)
fibonacci

> (time (fibonacci 100))
Timing the evaluation of (fibonacci 100)

User time    =        0.000
System time  =        0.000
Elapsed time =        0.000
Allocation   = 11440 bytes
0 Page faults
354224848179261915075
```

And now `fibonacci` calls itself precisely once for each value of `n`, remembering the result so it never needs to do it again.

Memoization is easy in principle, especially given hash tables, but slightly fiddly in practice:

- you need to decide which argument or arguments to use as the key;
- you need to define what it means for two keys to be the same, or in other words what sort of hash table to use;
- you need to make sure the compiler does not do any 'fast-call' of recursive calls, or tail-call elimination which will bypass the lookup of memoized values;
- you need to manage memoizing and unmemoizing, clearing memoized results and so on.

This is what `memoize` attempts to do.  Unfortunately it may not do a very good job of it: I wrote it a long time ago and made various mistakes in its initial implementation.  I *think* those mistakes are now, mostly, resolved, but I am not sure they are.

### The interface
**`def-memoized-function`** defines or redefines a memoized function.  Its syntax is the same as `defun` except that the function name may be either

- a function name (including a name of the form `(setf x)`);
- a list of a function name and some keyword options which control memoization.

In the second case the keyword options are those used by `memoize-function` (see below).  A simple example is

```lisp
(def-memoized-function fibonacci (n)
  (cond
   ((= n 0) 0)
   ((= n 1) 1)
   ((> n 1) (+ (fibonacci (- n 1)) (fibonacci (- n 2))))
   ((< n 0) (error "no"))))
```

A more complicated one: if we wanted to memoize a function which computes the nth Fibonacci number represented as a list:

```lisp
(defun peanoish-fib (l)
  (cond
   ((null l) l)
   ((null (cdr l)) l)
   (t (append (peanoish-fib (cdr l))
              (peanoish-fib (cddr l))))))
```

then we would need to compare the list argument with `equal`, not `eql`:

```lisp
(def-memoized-function (peanoish-fib :test #'equal) (l)
  (cond
   ((null l) l)
   ((null (cdr l)) l)
   (t (append (peanoish-fib (cdr l))
              (peanoish-fib (cddr l))))))
```

(This doesn't really help you very much, of course, since this function will allocate vast lists to represent large numbers.)

`def-memoized-function` uses `memoize-function` to do its work.  It will also make a `notinline` declamation for the function to disallow the compiler from optimising self-calls.

**`memoize-function`** will memoize a function.  It needs the *name* of the function in order to memoize it, so it can replace its definition.  Additionally it has two keyword arguments:

- `key` is the function which extracts the key for memoization from the arglist of the function (default `#'first`);
- `test` is the test function for the hash table which stores memoized values (default `#'eql`).

`memoize-function` maintains a list of memoized functions and will refuse to memoize a function which is already memoized.

**`unmemoize-function`** will unmemoize a memoized function: its argument is the function name, again.

**`unmemoize-functions`** will unmemoize all memoized functions.

**`clear-memoized-function`** will clear the memos for the function name it is given;

**`clear-all-memoized-functions`** will clear memos for all functions

**`function-memoized-p`** will tell you if a given function name is memoized.

**`memoized-labels`** is like `labels` but it will memoize all the local functions it defines.  As with `def-memoized-function` the function specifications can either be function names or lists of a function name and keywords as for `memoize-function`.  Note that this *doesn't* use `memoize-function` of course, because these are only local functions, but it does the same thing.  So, for instance

```lisp
(defun fibonacci (n)
  (check-type n (integer 0) "a natural")
  (memoized-labels ((fib (m)
                      (case m
                        ((0 1) m)
                        (otherwise
                         (+ (fib (- m 1))
                            (fib (- m 2)))))))
    (fib n)))
```

### Notes
`memoize` is old code: I don't use it very much, it has had some silly bugs and may still have.  I've recently fixed it so it should understand `(setf x)` function names, but I have not tested those fixes very much.  Memoized functions are unlikely to be thread-safe.

### Package, module
`memoize` lives in `org.tfeb.hax.memoize` and provides `:org.tfeb.hax.memoize`.

## Abstract and final classes `abstract-classes`
An abstract class is a class for which instances can't be made directly, although they may be able to be made for subclasses of it.  Here is a simple example:

```lisp
(defclass point ()
  ()
  (:metaclass abstract-class))

(defclass 2d-point (point)
  ((x :initform 0
      :initarg :x
      :accessor point-x)
   (y :initform 0
      :initarg :y
      :accessor point-y)))
```

With this, then:

```lisp
> (make-instance 'point)

Error: Trying to make an instance of point which is an abstract class
[...]

> (make-instance '2d-point :x 3 :y 2)
#<2d-point 40201CCBBB>
```

A final class is a class which may not be subclassed:

```lisp
(defclass 2d-point (point)
  ((x :initform 0
      :initarg :x
      :accessor point-x)
   (y :initform 0
      :initarg :y
      :accessor point-y))
  (:metaclass final-class))
```

And now any attempt to subclass `2d-point` will fail.

Because I got annoyed with `(defclass ... ... ... (:metaclass ...))`, there are a couple of defining macros:

- `define-abstract-class` is exactly the same as `defclass` with a suitable `abstract-class` metaclasss option;
- `define-final-class` is exactly the same as `defclass` with  a suitable `final-class` metaclass option.

### A note on the MOP
`abstract-classes` needs a tiny bit of the MOP.  For most platforms it uses [Closer to MOP](https://github.com/pcostanza/closer-mop "Closer to MOP") to avoid having to have implementation-dependent code.  However for platforms where `closer-mop:standard-class` is not `cl:standard-class`, `defclass` will, by default, create classes whose metaclass is `cl:standard-class`, while the `validate-superclass` methods will refer to `closer-mop:standard-class`[^11]  In the implementations I use where that is true I've relied on the implementation's MOP.  Currently this means LispWorks, although there may be others.

### Package, module
`abstract-classes` lives in `org.tfeb.hax.abstract-classes` and provides `:org.tfeb.hax.abstract-classes`.

## Classes with only one instance: `singleton-classes`
Sometimes it is useful to have classes for which there is only one instance.  One slightly questionable way of doing that is to specialise `make-instance` so that it simply looks up and returns the single instance.  That's what `singleton-classes` does: given

```lisp
(defclass single ()
  ()
  (:metaclass singleton-class))
```

then

```lisp
> (eq (make-instance 'single) (make-instance 'single))
t
```

There is no macro to make `singleton-class` classes, as there is for `abstract-classes` : perhaps there should be.

There is a function, `reset-singleton-classes` which will remove all the instances of singleton classes, so they will be recreated the next time `make-instance` is called.

### Notes
The approach of causing `make-instance` not always to make an instance is, I think, dubious: I think the whole thing was just a toy to show what was possible.  One thing to know is that, for instance, `initialize-instance` only happens when the single instance is created, which means that given

```lisp
(defclass one ()
  ((s :initarg :s
      :reader one-s))
  (:metaclass singleton-class))
```

then

```lisp
> (make-instance 'one :s 1)

#<one {1003D4E1F3}>
cl-user> (one-s (make-instance 'one :s 2))
1
```

I am not sure if that is correct, but it's how it works.

Finally,  classes whose metaclass is `singleton-classes` can be subclasses of `standard-class` classes and each other, but classes whose metaclass isn't `singleton-class` can't be subclasses of ones whose metaclass is (in other words you can't escape from being a singleton class by subclassing it).

`singleton-classes` has the same MOP problems as `abstract-classes` and gets around them in the same way.

### Package, module
`singleton-classes` lives in `org.tfeb.hax.singleton-classes` and provides `:org.tfeb.hax.singleton-classes`.

## Case-sensitive forms: `cs-forms`
There have been endless useless wars about case-sensitivity in CL, which I am uninterested in rehearsing.  `cs-forms` provides a mechanism to make a readtable which can read single forms case-sensitively, using, by default `#~` as the toggle.  While reading case-sensitively, `#~` will switch back to a case-insensitive readtable.

It does this by maintaining a pair of readtables, which have `#~` defined in such a way as to switch between them.   So

```lisp
(setf *readtable* (make-cs-form-readtable))
#<readtable 402000A18B>

> '(this #~that #~(the #~other))
(this |that| (|the| other))

> '(this #~that #~(the #~other And ON it goes))
(this |that| (|the| other |And| on |it| |goes|))
```

**`make-cs-form-readtable`** makes a readtable in which `#~` (where `~` may be specified) is defined to toggle case sensitivity.  It takes three keyword arguments:

- `from` is the readtable to copy.  If provided as `nil` this copies the standard readtable, if not provided it copies the current readtable;
- `to` is the readtable to copy into if provided;
- `toggle` is the dispatching macro character which should be used to toggle (default `#\~`).

`make-cs-form-readtable` will signal an error if the `toggle` character is already in use.

Note that this works by maintaining a *pair* of readtables: any other changes to the readtable being copied should be made before `make-cs-form-readtable` is called.

The main use of this might be to map between names in a language which uses lots of mixed-case identifiers and CL:

```lisp
(defvar *nmap*
  '((#~sillyName . better-name)
    (#~anotherOne . still-better)))
```

### Package, module
`cs-forms` lives in `org.tfeb.hax.cs-forms` and provides `:org.tfeb.hax.cs-forms`.

## Reading forms in a package: `read-package`
Symbolics LispMs had a nice syntax where package prefixes applied generally: `foo:(x y z)` meant either the same as `(foo:x foo:y foo:z)` or possibly `(foo::x foo::y foo::z)`, I'm not sure now.  This can't be done in standard CL as by the time the package prefix has been read you're already committed to reading a symbol.  But this hack lets you do something similar:

```lisp
> (defpackage :foo
    (:use))
#<The FOO package, 0/16 internal, 0/16 external>

> (setf *readtable* (make-read-package-readtable))
#<readtable 402000446B>

> '#@foo(a b c)
(foo::a foo::b foo::c)
```

In particular `#@<package> <form>` will read `<form>` in `<package>`.

This all works by a fairly nasty hack: the package is read as (probably) a symbol, and then looked up as a package name (although `'#@"FOO" (a b c)` will work as well).  To avoid an endless profusion of package name clutter, while the package name is read a secret package is current, and the package name read is uninterned from that package once it's been found.  That's horrible, but it works.

**`make-read-package-readtable`** has three keyword arguments:

- `from` is the readtable to copy.  If provided as `nil` this copies the standard readtable, if not provided it copies the current readtable;
- `to` is the readtable to copy into if provided;
- `at` is the dispatching macro character which should be used (default `#\@`).

 It may be that some current CL implementations can do the native Symbolics-style thing too, and if they can it's probably better than this hack.

When `*read-suppress*` is true, the reader simply calls `read` twice and returns `nil`.

### Package, module
`read-package` lives in `org.tfeb.hax.read-package` and provides `:org.tfeb.hax.read-package`.

## Commenting forms: `comment-form`
[Racket](https://racket-lang.org/ "Racket") has a nice `#;` readmacro which will comment the following form.  Now so does CL:

```lisp
> (setf *readtable* (make-comment-form-readtable))
#<readtable 4020003A13>

> #;
(defun foo ()
  (explode))
(print 1)

1
1
```

With an infix parameter, `#;` will skip that many forms.  Forms are skipped simply by binding `*read-suppress*`, the same way `#+` / `#-` do.  Indeed `#;` is equivalent to `#+(or)` apart from the infix character.

**`make-comment-form-readtable`** takes three keyword arguments:

- `from` is the readtable to copy.  If provided as `nil` this copies the standard readtable, if not provided it copies the current readtable;
- `to` is the readtable to copy into if provided;
- `at` is the dispatching macro character which should be used (default `#\@`).

### Package, module
`comment-form` lives in `org.tfeb.hax.comment-form` and provides `:org.tfeb.hax.comment-form`.

## Tracing macroexpansion: `trace-macroexpand`
It's sometimes pretty useful to understand what's going on during macroexpansion: `trace-macroexpand` lets you do that.

### A simple example
In a package where my `collecting` and `with-collectors` macros are available (see above).

```lisp
> (trace-macroexpand t)
nil

> (trace-macro collecting with-collectors)
(collecting with-collectors)

> (collecting (collect 1))
(collecting (collect 1))
 -> (with-collectors (collect) (collect 1))
(with-collectors (collect) (collect 1))
 -> (let (#:collect-var #:collect-tail) (flet # # ...) ...)
(1)

> (with-collectors (a b)
    (a 1)
    (b 2))
(with-collectors (a b) (a 1) ...)
 -> (let (#:a-var #:b-var #:a-tail ...) (flet # # ...) ...)
(1)
(2)

> (with-collectors (outer)
    (outer (with-collectors (inner)
             (inner 1)
             (inner 2))))
(with-collectors (outer) (outer #))
 -> (let (#:outer-var #:outer-tail) (flet # # ...) ...)
(with-collectors (inner) (inner 1) ...)
 -> (let (#:inner-var #:inner-tail) (flet # # ...) ...)
((1 2))
```

By default the values of `*print-length*` and `*print-level*` are `3` and `2` when tracing macroexpansion so the output is not enormous – what you often want to see is just that a macro *is* expanded, not all the details.  But we can control this:

```lisp
> (setf *trace-macroexpand-print-length* nil
        *trace-macroexpand-print-level* nil)
nil

> (collecting (collect 1))
(collecting (collect 1))
 -> (with-collectors (collect) (collect 1))
(with-collectors (collect) (collect 1))
 -> (let (#:collect-var #:collect-tail)
      (flet ((collect (org.tfeb.hax.collecting::it)
               (if #:collect-var
                   (setf (cdr #:collect-tail)
                         (list org.tfeb.hax.collecting::it)
                         #:collect-tail (cdr #:collect-tail))
                 (setf #:collect-tail (list org.tfeb.hax.collecting::it)
                       #:collect-var #:collect-tail))
               org.tfeb.hax.collecting::it))
        (declare (inline collect))
        (collect 1))
      (values #:collect-var))
(1)
```

You can then trace or untrace things in a similar way you can with `trace`:

```lisp
> (trace-macro)
(collecting with-collectors)

> (untrace-macro with-collectors)
(collecting)
```

And you can also turn tracing on and off globally:

```lisp
> (macroexpand-traced-p)
t

> (trace-macroexpand nil)
t

> (macroexpand-traced-p)
nil
```

Finally you can arrange to trace all macros in a package:

```lisp
> (trace-macro-package :org.tfeb.hax.collecting)
("ORG.TFEB.HAX.COLLECTING")

> (trace-macro)
nil

> (collecting (collect 1))
(collecting (collect 1))
 -> (with-collectors (collect) (collect 1))
(with-collectors (collect) (collect 1))
 -> (let (#:collect-var #:collect-tail) (flet # # ...) ...)
(1)
```

It can trace itself:

```lisp
> (trace-macro trace-macro)
(trace-macro)

CL-USER 52 > (trace-macro)
(trace-macro)
 -> (org.tfeb.hax.trace-macroexpand::trace-macros 'nil)
(trace-macro)
```

### How it works, caveats
All `trace-macroexpand` does is to install a hook on `*macroexpand-hook` and use this to drive the tracing.  It is careful to call any preexisting hook as well, so it does not interfere with anything else.  However, don't unilaterally change `*macroexpand-hook*` while macro tracing is active: turn it off first, as things will become confused otherwise.  If it detects bad states (for instance if tracing is off but the wrapped hook isn't `nil`, or if tracing seems to be on but the wrapped hook *is* `nil`) it will signal errors and there are restarts which may help recover.  But it's best to not get into these states.

Tracing output goes to `*trace-macroexpand-output*`, which is by default a synonym stream to `*trace-output*`.

### The interface
The interface is fairly large, as there are a reasonable number of options, some of which can be controlled in various ways.

**`trace-macroexpand`** turns macroexpansion tracing on or off, globally.  It has a single optional argument: if it is true (the default) it turns it on, if it is false it turns it off.  It returns the previous state.  Compiling or loading `trace-macroexpand` will unilaterally force macroexpansion tracing off, to avoid problems with tracing happening as the code which does the tracing is being redefined.

**`macroexpand-traced-p`** tells you if macroexpansion is off or on.  Again, the initial state is always off.

**`call/macroexpand-tracing`** calls a function with macroexpansion tracing on or off.  It's useful, for instance, for seeing what happens when you compile a file.  Its argument is a function of no arguments to call, and an optional second argument controls whether macroexpansion is on (the default) or off during the dynamic extent of the call.

**`with-macroexpand-tracing`** is a macro shim around `call/macroexpand-tracing`: use it as `(with-macroexpand-tracing () ...)` or `(with-macroexpand-tracing (state) ...)`:

```lisp
> (with-macroexpand-tracing () 1)
(with-macroexpand-tracing nil 1)
 -> (call/macroexpand-tracing (lambda # 1) t)
```

**`trace-macro`** is like `trace` for macros: you give it names of macros and it will tell the system that their expansion should be traced.  Like `trace` it's a macro.  It returns (expands to) the list of all traced macro names.

**`untrace-macro`** is like `untrace`, and like it it's a macro.

**`*trace-macroexpand-traced-names*`** is a list of the names of all macros being traced by name: it's what `trace-macro` & `untrace-macro` maintain.  You can change or bind this list at will.

**`trace-macro-package`** will turn tracing on for zero or more packages.  It takes any number of arguments, which are 'package designators', where this means one of:

- `t` meaning 'all packages';
- `nil` meaning 'whatever `*package*` is at the moment of tracing';
- a symbol or string which names a package;
- a package, denoting itself.

It returns the canonicalised list of package designators being traced: each of these is a string, `t` or `nil`.  `trace-macro-package` is a function, not a macro.

**`untrace-macro-package`** takes zero or more package designators to be untraced.  It is not an error if they are not being traced.  It returns the canonicalised list of package designators.  It is a function, not a macro.

**`*trace-macroexpand-traced-packages*`** is the canonical list of package designators maintained by the previous two functions.  You can bind or modify this.

**`*trace-macroexpand-print-length*`**, **`*trace-macroexpand-print-level*` and `*trace-macroexpand-print-cicrle*`** are the values of `*print-length*`, `*print-level*` and `*print-circle*` in effect during tracing.  By default they are `3`, `2` and the ambient value of `*print-circle*` when the system is loaded respectively.

**`*trace-macroexpand-output`** is the stream to which tracing goes.  By default it is a synonym stream to `*trace-output*`.

**`*trace-macroexpand-printer*`**, if it is not `nil`, should be a designator for a function of four arguments: the stream to trace on, the macro form, the expanded macro form and the environment: it will be called to print or otherwise record the expansion.  In this case no binding is done of printer control variables: the function is responsible for anything it wants to do.

**`*trace-macroexpand-maybe-trace*`** is a kill switch: if it is false no tracing will happen, whatever anything else may say.

**`*trace-macroexpand-trace-hook*`** , if non-`nil`, should be a function which controls whether tracing should happen, overriding everything else except `*trace-macroexpand-maybe-trace*`.

### Package, module
`trace-macroexpand` lives in `org.tfeb.hax.trace-macroexpand` and provides `:org.tfeb.hax.trace-macroexpand`.

## Defining global functions with lexical environments: `define-functions`
It's a little fiddly in CL to define global functions with non-empty lexical environments.  The obvious approach:

```lisp
(let ((c 0))
  (defun counter ()
    (prog1 c
      (incf c))))
```

Is problematic because the function definition will not generally be known about at compile-time.  It's also ugly, compared with the equivalent in Scheme[^12]:

```lisp
(define counter
  (let ((c 0))
    (λ ()
      (begin0
        c
        (set! c (+ c 1))))))
```

`define-functions` deals with this: you can say

```lisp
(define-function counter
  (let ((c 0))
    (lambda ()
      (prog1 c
        (incf c)))))
```

`define-function`, and `define-functions` on which it's built, arrange for suitable declamations to keep the compiler happy and expand into code which will define the function or functions properly at load-time.

### Function specifications
Because `define-function` & `define-functions` need to write a declamation of the type of the function and because you may want to add documentation, the argument to it is either:

- a function name;
- or a lambda list of the form `(function-name &key ftype documentation)` where the keyword arguments can be used to provide a more detailed declamation for the function's type, and/or documentation.

The macros are smart enough to realise that a list of the form `(setf x)` is a function name.

### Interface macros
**`define-function`** defines a single global function to be the result of a form.  It returns the name of the function being defined.

**`define-functions`** defines a number of functions.  Its first argument is a list of function specifications as above, and its remaining arguments are either

- a single form which should return as many values as there are functions to define;
- as many forms as there are functions to define.

`define-functions` can be used to define sets of global functions which share a lexical environment:

```lisp
(define-functions (inc dec)
  (let ((c 0))
    (values
     (lambda ()
       (incf c))
     (lambda ()
       (decf c)))))
```

It returns the names of the functions defined as multiple values.

There are also equivalents for macros, although these seem unlikely to be useful.

**`define-macro-function`** defines a macro.  A macro specification is either

- a macro name;
- a lambda list of the form `(macro-name &key documentation)` (there is no `ftype` option in this case).

Note that the second argument of `define-macro-function` should evaluate to a macro function: a function which takes two arguments, which are a macro form and an environment.

**`define-macro-functions`** is to `define-macro-function` as `define-functions` is to `define-function`.

Finally, we can use `trace-macroexpand` to poke at these:

```lisp
 > (setf *trace-macroexpand-print-length* nil
         *trace-macroexpand-print-level* nil)
nil

> (define-function counter
    (let ((c 0))
      (lambda ()
        (prog1 c
          (incf c)))))
(define-function counter (let ((c 0)) (lambda () (prog1 c (incf c)))))
 -> (define-functions (counter)
      (let ((c 0)) (lambda () (prog1 c (incf c)))))
(define-functions (counter) (let ((c 0)) (lambda () (prog1 c (incf c)))))
 -> (progn
      (declaim (ftype function counter))
      (multiple-value-bind (#:f0)
          (let ((c 0)) (lambda () (prog1 c (incf c))))
        (unless (functionp #:f0) (error "A function isn't: ~A" #:f0))
        (setf (fdefinition 'counter) #:f0))
      (values 'counter))
counter
```

### Package, module
`define-functions` lives in `org.tfeb.hax.define-functions` and provides `:org.tfeb.hax.define-functions`.

## Local bindings: `binding`
Different languages take different approaches to declaring – *binding* – variables and functions locally in code.

- CL requires `let`, `labels` &c, which is clear but involves extra indentation;
- Scheme allows local use of `define` which does not involve indentation, but does not allow it everywhere;
- Python allows local bindings anywhere but the scope is insane (bindings have function scope and are thus often visible before they appear textually) and variable binding is conflated with assignment which is just a horrible idea:
- some C compilers may allow variable declarations almost anywhere with their scope starting from the point of declaration and running to the end of the block – I am not sure what the standard supports however;
- Racket allows `define` in many more places than Scheme with the scope running from the `define` to the end of the appropriate block.

Racket is pretty clear how what it does works:

```lisp
...
(define foo ...)
...
```

turns into something like

```lisp
...
(letrec ([foo ...])
  ...)
```

I thought it would be fun to implement a form which does this for CL, and that's what `binding` does.

**`binding`** is a form, directly within whose body several special binding forms are available.  These forms are:

- `bind` will bind local variables or functions, corresponding to `let*` or `labels` respectively;
- `bind/macro` will bind local macros or symbol macros, corresponding to `macrolet` or `symbol-macrolet` respectively;
- `bind/values` will bind multiple values, corresponding to `multiple-value-bind`;
- `bind/destructuring` corresponds to `destructuring-bind`.

For `bind` the two cases are:

- `(bind var val)` will bind `var` to `val`using `let*`;
- `(bind (f ...) ...)` will create a local function `f` using `labels` (the function definition form is like Scheme's `(define (f ...) ...)` syntax).

For `bind/macro` the two cases are really the same although the expansions are different:

- `(bind/macro (m ...) ...)` turns into `(macrolet ((m (...) ...) ...)`;
- `(bind/macro m e)` turns into `(symbol-macrolet ((m e)) ...)`.

For `bind/values` there are also two cases:

- `(bind/values (...) form)` corresponds to `(multiple-value-bind (...) form ...)`
- `(bind-values (...) form ...)` corresponds to `(multiple-value-bind (...) (values form ...) ...)`.

`bind/destructuring` doesn't have any variants.

`bind/values` also has a special magic: if you use `nil` as the name of a 'variable', that binding is quietly ignored.  This helps in cases where, for instance, you need only some of the values of a form:

```lisp
(binding
  (bind/values (nil interesting) (two-valued-function))
  ... interesting ...)
```

All of these forms are coalesced to create the minimum number of binding constructs in the generated code (this is why `bind` corresponds to `let*`), so:

```lisp
(binding
  (print 1)
  (bind x 1)
  (bind y 2)
  (print 2)
  (bind (f1 v)
    (+ x v))
  (bind (f2 v)
    (+ y (f1 v)))
  (f2 1))
```

corresponds to

```lisp
(progn
  (print 1)
  (let* ((x 1) (y 2))
    (print 2)
    (labels ((f1 (v)
               (binding (+ x v)))
             (f2 (v)
               (binding (+ y (f1 v)))))
      (f2 1))))
```

and so on. `bind/values` and `bind/destructuring` are not coalesced as it makes no sense to do so.

### Notes
The bodies of local functions and macros bound by `binding` are themselves wrapped in  `binding` forms, but declarations are raised out of these forms.  So

```lisp
(binding
  (bind (f i)
    (declare (type fixnum i))
    (bind j (* i 2))
    (if (fixnump j)
        (f j)
      j))
  (f 1))
```

expands to

```lisp
(labels ((f (i)
           (declare (type fixnum i))
           (binding
             (bind j (* i 2))
             (if (fixnump j)
                 (f j)
               j))))
  (f 1))
```

and hence to

```lisp
(labels ((f (i)
           (declare (type fixnum i))
           (let* ((j (* i 2)))
             (if (fixnump j)
                 (f j)
               j))))
  (f 1))
```

Apart from this case,`bind` &c work *only* directly within `binding`: there is no code walker, intentionally so.  There are top-level definitions of `bind` &c as macros which signal errors at macroexpansion time.

I thought about using `_` (or symbols with that name) as the 'ignore this binding' for `bind/values` to be compatible with, for instance, Racket, but I decided that using `nil` could break no existing programs so was safer.

`binding` uses `iterate` to do iteration, so it relies on an implementation which can turn this into tail calls (or has a big enough stack, which will probably be the case for most practical source code).

### Package, module, dependencies
`binding` lives in `org.tfeb.hax.binding`and provides `:org.tfeb.hax.binding`.  `binding` depends on `collecting` and `iterate` at compile and run time.  If you load it as a module then, if you have [`require-module`](https://github.com/tfeb/tfeb-lisp-tools#requiring-modules-with-searching-require-module "require-module"), it will use that to try and load them if they're not there.  If it can't do that and they're not there you'll get a compile-time error.

## Special strings: `stringtable`
`format` has a very useful feature: there is a special format control 'tilde newline' which will cause `format` to skip both the newline and any following whitespace characters[^13].  This makes writing long format control strings much easier, which is useful since format control strings do tend to be long.  You can, then, use `(format nil ...)` as a way of simply creating a string with, if you want, newlines being ignored.

I wanted to do something that was both less and more than this: I wanted a way of writing literal strings such that it was possible to, for instance, ignore newlines to help source formatting, but  *without* involving `format` so I didn't have to worry about all the other format controls, or about explicitly trying to make sure `format` got called before runtime to avoid overhead.  I also wanted the possibility of being able to define my own special handlers in such strings, with all of this working at read time.

This is what stringtables do: they provide a way of reading literal strings where the string reader, as well as the usual escape character (which can be set), has zero or more 'special' characters which can cause user-defined functions to be called, all controlled by a stringtable object.  The whole thing is like a simplified version of the normal CL reader, with the stringtable object playing the role of the readtable (this is why they are called 'stringtables' of course).  In fact the various functions in the interface intentionally echo those of the normal reader interface: that interface is a bit clunky I think, but I thought it was better to be culturally compatible with it than invent something new.

A critical difference between stringtables and the reader is that the special character handlers in stringtables have *fallbacks*: these can be specified, but the default fallback is a function which simply returns the character, or the special character and the character.

The stringtable reader can be glued into the normal reader on a macro character, and there is a function to do this.  This uses `#"..."` by default, although it can use any other delimiter (for a while, it used `#/.../` as the default).  Although the system is now rather general, there is also a function which sets up a default special handler to skip newlines and following whitespace.

### Examples
Here's a tiny example, using the gluing functions to make `read` use stringtables and then to tell the default stringtable to use the newline skipper.

```lisp
 > (setf *readtable* (make-stringtable-readtable))
#<readtable 402002208B>

> (set-stringtable-newline-skipper)
#<stringtable 421017BA03>

> (read)
#"this is ~

a string with no newlines in it ~
      at all"
"this is a string with no newlines in it at all"
```

Here's another example which puts the stringtable reader on the interim `#/.../` syntax:

```lisp
> (setf *readtable* (make-stringtable-readtable :delimiter #\/))
#<readtable 4020022E5B>

> (set-stringtable-newline-skipper)
#<stringtable 4210196123>

> (read)
#/foo ~
 bar/
"foo bar"
```

### Reading special strings
The algorithm for reading a special string is:

- read a character, if it is the delimiter turn all the accumulated characters into a string;
- if it is the escape character (by default `#\\`) then read the next character and accumulate it (even if it is the delimiter);
- if is is a special character, then read the next character
	- if it is the delimiter this is an error (if you want to escape the delimiter, use the escape character);
	- otherwise find either its handler or the fallback handler for that special character, then call the handler with four arguments: the special character, the character after it, the delimiter and the stream;
	- the handler function should return: a character which is accumulated; a list of characters which are accumulated (this list may be empty); or a string, the characters of which are accumulated.  Any other return value is an error.

An end of file before an unescaped delimiter is reached is an error.

### The interface
**`*stringtable*`** is the current stringtable object, by analogy with `*readtable*`.  Initially it is set to a copy of the standard stringtable, which:

- uses `#\` as an escape character;
- has a single special character, `#\~`, with a default fallback function which returns either the subcharacter or the special character and the subcharacter, depending on `*stringtable-fallback-absorb-special*` (see below);
- has no handlers other than the fallback for the special character.

**`*stringtable-fallback-absorb-special*`** is a variable which controls the behaviour of the default fallback handler: if it is true then it will absorb the special character and simply return the subcharacter; if it is false it will return both the special character and the subcharacter.  User-defined fallback handlers are encouraged to respect this variable.  The default value is true.

**`copy-stringtable`** makes a copy of a stringtable.  It has three optional arguments.

- `from` is the stringtable to copy, which is by default `*stringtable*`.  If given as `nil` it will make a copy of the standard stringtable.
- `to` is the stringtable to copy into, which is `nil` by default, meaning to make a new stringtable;
- `nospecial` will cause the copy to have no special characters at all: in this case the only thing being copied is the escape character.

The argument convention for this function is clunky, but it is done this way for compatibility with `copy-readtable`.  Providing `nospecial` is the *only* way to make a stringtable with no special characters at all.

**`stringtable-escape-character`** is the accessor for the escape character of a stringtable.  The escape character can be set to `nil` as a special case, which is equivalent to there being no escape character.

**`make-stringtable-special-character`** makes a special character in a stringtable.  If there is already one there it will remove all of its subcharacters and optionally replace the fallback function.  It has one mandatory argument and two optional arguments:

- `character` is the special character;
- `fallback` is the fallback function, which if given should be a function of four arguments as described above, with the default being the standard fallback function also described above;
- `stringtable` is the stringtable, with the default being `*stringtable*`.

The function returns `t` by analogy with `make-dispatch-macro-character`.

**`get-stringtable-special-character`** gets the handler function for a special character and subcharacter in a stringtable.  It has two mandatory arguments and one optional argument:

- `character` is the special character, it is an error if this is not special in the stringtable;
- `subcharacter` is the subcharacter, which can be any character;
- `stringtable` is the stringtable, default `*stringtable*`.

The function returns two values:

- the handler function;
- true if the handler is not the fallback function, false if it is.

**`set-stringtable-special-character`** defines a handler for a special character and subcharacter pair.  It has three mandatory arguments and one optional argument:

- `character` is the special character, which must be special in the stringtable;
- `subcharacter` is the subcharacter, which can be any character;
- `function` is the handler, as described above;
- `stringtable` is the stringtable, default `*stringtable*`.

The function returns `t`.

**`read-string-with-stringtable`** is the interface to reading special strings.  It is intended to be called from a reader macro function.  It has one mandatory argument and two optional arguments:

- `delimiter` is the closing delimiter for the string, a character;
- `from` is the stream to read, by default `*standard-input*`;
- `stringtable` is the stringtable to use, by default `*stringtable*`.

It returns the string read, or signals an error if something went wrong.

The remaining two functions are not part of the core behaviour of the module, but make it easy to set up the useful common case (or my useful common case, anyway).

**`make-stringtable-readtable`** makes a readtable with a stringtable reader attached to a macro character.  It has three keyword arguments:

- `from` is the readtable to copy, with the same conventions as `copy-readtable` – the default is `*readtable*`, providing `nil` means 'copy the standard readtable';
- `to` is the readtable to copy into as for `copy-readtable` with the default being `nil` meaning 'make a new readtable';
- `delimiter` is the delimiter character, with the default being `#\"` (see below).

The returned readtable will have a macro character set up for the `delimiter` subcharacter of `#\#` which will read special strings with `delimiter`.

Note this function is not fully general: its purpose in life is to set up the common case.  It's perfectly possible to have special string readers on other characters, but if you want to do that you need to do it yourself.

**`set-stringtable-newline-skipper`** is a function which installs a suitable newline-skipper handler for a stringtable.  It has three keyword arguments:

- `stringtable` is the stringtable to install the skipper on, with the default being `*stringtable*`;
- `special-character`, default `#\~`, is the special character in the stringtable where the skipper should be defined – it is an error if this is not a special character in the stringtable;
- `white-warners`, default `t` will install 'warner' functions on the special character followed by whitespace other than newline.

This function will in fact install the skipper function on both `#ewline`, `#\Return` and `#\Linefeed` (even if some of those are the same character).  The 'white warner' functions get installed on `#\Space` and `#\Tab` and will do nothing, but will generate a warning: the aim of this is to detect the common mistake of a trailing space.

The function returns the stringtable.

This function relies on some slightly non-standard characters: I think they exist in all common implementations however.  If I find ones where they don't exist I will conditionalise the code for them.

### Notes
As mentioned above, a lot of the interface is trying to mirror the standard readtable interface, which is why it's a bit ugly.

I've talked about things 'being an error' above: in fact in most (I hope all) cases suitable conditions are signalled.

When `*read-suppress*` is true, then reading a special string will still call its handlers: this is necessary because the handlers can absorb arbitrary characters from the stream.  But the results of the handlers are simply ignored and no string is built.  An alternative would simply be not to call the handlers and assume they are well-behaved, but I decided not to do that.  User-defined handlers should, if need be, notice `*read-suppress*`and behave appropriately, for instance by not building a big list of characters to return when reading is suppressed.

Stringtables are intended to provide a way of reading literal strings with some slightly convenient syntax[^14]: it is *not* a system for, for instance, doing some syntactically-nicer or more extensible version of what `format` does.  There are other things which do that, I'm sure.

Originally the default delimiter for `make-stringtable-readtable` was `#\"`, as it is now .  For a while it was `#\/`, because I worried that `#"..."` would be likely to clash with other hacks,  but  `#/.../` finally seemed too obvious a syntax fir regular expressions to use for this.  You can always choose what you want to have.

### Package, module, dependencies
`stringtable` lives in `org.tfeb.hax.stringtable` and provides `:org.tfeb.hax.stringtable`.  `stringtable` depends on `collecting` and `iterate` at compile and run time.  If you load it as a module then, if you have [`require-module`](https://github.com/tfeb/tfeb-lisp-tools#requiring-modules-with-searching-require-module "require-module"), it will use that to try and load them if they're not there.  If it can't do that and they're not there you'll get a compile-time error.

## Object accessors: `object-accessors`
`with-accessors` & `with-slots` are pretty useful macros.  Since `symbol-macrolet` exists it's pretty easy to provide a similar facility for accessor functions for completely arbitrary objects.  That's what `with-object-accessors` does: it does exactly what `with-accessors` does, but for completely arbitrary objects and functions[^15].  As an example:

```lisp
(defun foo (c)
  (with-object-accessors (car cdr) c
    (setf car 1                         ;(setf (car c) 1)
          cdr 2)                        ;(setf (cdr c) 2)
    c))
```

As with `with-accessors` you can provide names which are different than the accessor names:

```lisp
(defstruct long-name-thingy
  long-name-slot)

(defun bar (lnt)
  (with-object-accessors ((s long-name-thingy-long-name-slot)) lnt
    ...
    (setf s 1)
    ...))
```

There is absolutely nothing special about `with-object-accessors`: it's just the obvious thing you would write using `symbol-macrolet`.  Its only reason to exist is so that it *does* exist: versions of it no longer have to be endlessly rewritten.  It is careful to evaluate the object only once, so `(with-object-accessors (car cdr) (cons 1 2) ...)` would work, say.

Additionally, `object-accessors` now provides a macro for named access to array elements.  This is `with-named-array-references`.  It has independent origin from `with-object-accessors` and takes its arguments in the opposite order.  That's slightly unfortunate.  Its syntax is

```lisp
(with-named-array-references (<array> [<type>]) ((<name> <index> ...) ...)
  ...)
```

The type declaration applies to the binding established for the array.  As an example:

```lisp
(with-named-array-references (state (array double-float (*))) ((x 0) (y 1) (vx 2) (vy 3))
  (incf x vx)
  (incf y vy)
  state)
```

will update some 2d state-vector for an object.

Note that the indices are not cached by this macro.  If you have a vector representing lots of object states you might want to define something like:

```lisp
(defmacro with-state-slots-at ((v n) &body decls/forms)
  ;; Named access to the state at n in the state vector
  (with-names (<xi> <yi> <vxi> <vyi>)
    `(let* ((,<xi> (* ,n 4))
            (,<yi> (+ ,<xi> 1))
            (,<vxi> (+ ,<yi> 1))
            (,<vyi> (+ ,<vxi> 1)))
       (declare (type array-index ,<xi> ,<yi> ,<vxi> ,<vyi>)
                (ignorable ,<xi> ,<yi> ,<vxi> ,<vyi>))
       (with-named-array-references (,v state-vector) ((x ,<xi>) (y ,<yi>)
                                                       (vx ,<vxi>) (vy ,<vyi>))
         (declare (ignorable x y vx vy))
         ,@decls/forms))))
```

This avoids recomputing the indices each time.

### Package, module
`object-accessors` depends on `utilities`, lives in `org.tfeb.hax.object-accessors` and provides `:org.tfeb.hax.object-accessors`.

## Decomposing iteration: `simple-loops`
Like a lot of people I have mixed feelings about `loop`.  For a long time I thought that, well, if I wasn't going to use `loop`, I'd need some other elaborate iteration system, although perhaps one which was more principled and extensible such as Richard C Waters' [Series](https://github.com/tfeb/series "Series")[^16].  And I am sure the CL community has invented other tools while I've not been watching.

But now I think that this is, perhaps, chasing a mirage: it might be better *not* to have some vast all-encompassing iteration tool, but instead a number of smaller, independent, components.  For a long time I have written

```lisp
(collecting
  (dotimes (...)
    ...
    (when ...
      (collect ...)
    ...))
```

in preference to `(loop ... when ... collect ... ...)` and `collecting` of course is more general:

```lisp
(collecting
  (iterate search (...)
    ...))
```

Can collect objects during a completely general recursive search, for instance.

`simple-loops` provides a number of simple loop constructs which can be used with tools like `collecting`, `iterate` and any other tool, as well as a general named escape construct.

**`doing`** and **`doing*`** are like `do` and `do*` except that bindings are more defaulted:

-  `<var>` means `(<var> nil nil)`;
- `(<var> <init/step>)` means `(<var> <init/step> <init/step>)`;
- `(<var> <init> <step>)` means what it currently does.

In addition the values returned are both more defaulted and more flexible:

- if no return value is specified then the current values of all the bindings are returned;
- if a single form is specified then all its values are returned (this is just like `do`);
- if there are more than a single form, then all the values from all of them are returned.

To get the same behaviour as, for instance, `(do (...) (<test>) ...)` you therefore need to say `(doing (...) (<test> nil) ...)`: `(doing (...) (<test>) ...)` will return the current values of all the variables.

**`passing`** and **`failing`** are while and until loops which bind variables with the same defaulting as `doing`: `(passing ((x ...) (y ...)) ...)` will iterate until `(and x y)` is true and then return the values of `x` and `y`.  `failing` will iterate until they are *not* all true and then return their values.

**`do-passing`** and **`do-failing`** are like `passing` and `failing` but the test is after the body, so they always iterate at least once.

There are starred forms of all these macros which bind sequentially, for a total of six macros.

**`looping`**  and **`looping*`**  are looping constructs with implicit stepping: `(looping ((a 1) b) ...)` will bind `a` to `1` and `b` to `nil` and then the values of the last form in the body is used to step the values.  There is no termination clause, but there is an implicit block named `nil`  The body of these forms is *not* wrapped in an implicit `tagbody` (it's a `progn` in fact), so you can't jump around in it like you can with `do`.  You can also use `escaping`.  Declarations at the start of the body are lifted to where they belong.  Initial bindings are in parallel for `looping`, in serial for `looping*`.  Here's a program which is not known to halt for all arguments:

```lisp
(defun collatz (n)
  (looping ((m n) (c 1))
    (when (= m 1)
      (return c))
    (values (if (evenp m)
                (/ m 2)
              (1+ (* m 3)))
            (+ c 1))))
```

**`looping/values`** and is a looping construct which use multiple values and then implicit stepping like `looping`.    Variables are bound as follows:

- `(looping/values ((<v> ...) <form>) ...)` will bind the `<v>`s to the multiple values of `<form>`.
- `(looping/values ((<v> ...) <form> ...) ...)` will bind the `<v>`s to the combined multiple values of all of the `<form>`s.

Once the variables are bound everything is exactly like `looping`: it is only the initial binding which is different.

**`looping/values*`** is like `looping/values` except that multiple sets of variables can be bound, each set being in the scope of all the previous sets.  So

```lisp
(looping/values* (((a b) (values 1 2))
                  ((c d) (values 3 a)))
  (return (values a b c d)))
```

will evaluate to `1 2 3 1` for instance (and will not loop at all).

**`escaping`** provides a general named escape construct.  `(escaping (<escaper> &rest <defaults>) ...)` binds `<escaper>` as a local function which will immediately return from `escaping`, returning either its arguments as multiple values, or the values of `<defaults>` as multiple values.  The forms in `<defaults>` are not evaluated if they are not used: if they are evaluated they're done in their lexical environment but in the dynamic environment where the escape function is called.

**`dolists`** is like `dolist` but for multiple lists.  Additionally it has more control over what value or values are returned.  The syntax is `(dolists ((<var> <list-form> [<result-form>]) ...) ...)`, and as many values are returned as there are `<result-form>`s.  Thus

```lisp
(dolists ((v1 '(1 2 3))
          (v2 '(1 2 3 4) v2))
  (format t "~&~S ~S~%" v1 v2))
```

will print each value of `v1` and `v2` with the last being `3` and `3`, and then return `4`.  There is a subtelty here: for `dolist` the variable is `nil` at the point the iiteration terminates: `(dolist (v '(1 2 3) v))` evaluates to `nil`.  `dolists` generalises this: at the point the iteration terminates at least one of the variables will be `nil`.  The variables which are *not* `nil`, corresponding to lists longer than the shortest list, will be bound the the vakue of the next element of their list.

As with `dolist` it is not specified whether the iteration variable is rebound for each iteration, or a single binding is mutated.

`escaping` is obviously a shim around `(block ... (return-from ...) ...)` and there are the same constraints on scope that blocks have: you can't call the escape function once control has left the form which established it.

The `passing` family of functions *aren't* named `while` because they're not actually `while` loops as the bind variables and also I didn't want to take such a useful and short name: everyone knows what `(while (= x 3) ...)` means.

`passing` and friends expand into `do` and `do*`, not `doing` and `doing*` but they use the same clause-parser that`doing` and `doing*` do so their clauses work the same way.

### Package, module, dependencies
`simple-loops` lives in `org.tfeb.hax.simple-loops` and provides `:org.tfeb.hax.simple-loops`.  It depends on `collecting`, `iterate`, and `utilities`.  If you load it as a module then, if you have [`require-module`](https://github.com/tfeb/tfeb-lisp-tools#requiring-modules-with-searching-require-module "require-module"), it will use that to try and load them if they're not there.  If it can't do that and they're not there you'll get a compile-time error.

## Simple pattern matching: `spam`
Quite often when writing robust macros you end up with code which has to protect `destructuring-bind`:

```lisp
(defmacro fooing ((&rest bindings) &body forms)
  (let ((effective-bindings
         (mapcar (lambda (binding)
                   (cond
                    ((symbolp binding)
                     ...)
                    ((and (listp binding)
                          (= (length binding 2)))
                     (destructuring-bind (var val) binding
                       (unless (symbolp var)
                         ...)
                       ...))
                    ...))
                 bindings)))
    ...))
```

This is painful to write.  A really nice solution to this is `destructuring-match`, a matching version of `destructuring-bind`, which enables pattern-matching macros:

```lisp
(defmacro fooing ((&rest bindings) &body forms)
  (let ((effective-bindings
         (mapcar (lambda (binding)
                   (destructuring-match binding
                     ((var)
                      (:when (symbolp var))
                      ...)
                     ((var val)
                      (:when (symblp var))
                      ...)
                     ...))
                 bindings)))
    ...))
```

and of course it's then easy to write `syntax-rules` / `syntax-case` style macros on top of something like this[^17]:

```lisp
(define-matching-macro fooing
  ((_ ((var val) . more ) &body forms)
   ...)
  (... ...))
```

Well, in the process of writing `destructuring-match` I found that you really need a lot of predicates to check lambda lists when parsing them before compiling them into code which will match suitable arglists.  So for instance you need to be able to check if the next element is either `&rest` or `&body`, the element after that is a variable name, and the element after *that* is `&key`.  The obvious thing to do was, rather than write all these expressions out by hand, to write predicate *constructors*:

```lisp
(head-matches (some-of (is '&rest) (is '&body))
              (var)
              (is '&key))
```

will return a predicate which does the above matching:

```lisp
> (let ((p (head-matches (some-of (is '&rest) (is '&body))
                         (var)
                         (is '&key))))
    (values (funcall p '(&body x &key))
            (funcall p '(&rest x &key))
            (funcall p '(&rest x &aux y))
            (funcall p '(x &aux y))))
t
t
nil
nil
```

Finally it is then easy to write a `matching` macro:

```lisp
(matching llt
  ((head-matches (some-of (is '&rest) (is '&body))
                 (var)
                 (is '&key))
   ...)
  ((head-matches (some-of (is '&rest) (is '&body))
                 (var))
   ...)
  ((head-matches (some-of (is '&rest) (is '&body))
                 (none-of (var)))
   (error "ill-formed &rest / &body"))
  ...
  (otherwise
   ...))
```

This approach turns out to be quite pleasant.

### `matching` and `matchp`
**`matching`** is a `case`-style macro: it successively matches the predicates which are the first element of each clause against an object, until one matches.   A clause like `(otherwise ...)` or `(t ...)` always matches (this means you can't bind a predicate to `otherwise`).  Any function can be used as a predicate:

```lisp
(matching thing
  (#'keywordp "keyword")
  (#'null "null")
  (#'symbolp "non-null, non-keyword symbol")
  (otherwise "something else"))
```

simply expands to the obvious thing:

```lisp
(let ((#:v thing))
  (cond ((funcall #'keywordp #:v) "keyword")
        ((funcall #'null #:v) "null")
        ((funcall #'symbolp #:v) "non-null, non-keyword symbol")
        (t "something else")))
```

Note that the predicates in `matching` clauses are *values* not function designators: you need to say `(matching x (#'keywordp ...) ...)` not `(matching x (keywordp ...) ...)`.

**`matchp`** is a function which checks if a predicate matches: `(matchp thing p)` is simply `(funcall p thing)`.

There is absolutely nothing clever about `matching` or `matchp`: `matching` just makes the syntax easier, and `matchp` exists because it should.

### The predicate constructors
This is the interesting part: `spam` has a bunch of functions which return useful predicates.

**`head-matches`** returns a predicate which matches a (possibly improper) list the first few of whose elements match the predicates which were the arguments of `head-matches`.  So `(head-matches #'realp #'realp)` will return a predicate which will match `(1 2)`, `(1 3.0 . t)`, but will fail for `()`, `(1)`, `"foo"` and so on.  It is a good way of constructing predicates which look ahead in something being parsed.

**`list-matches`** will return a predicate which matches a proper list whose elements match the predicates which are its arguments.  `(list-matches #'numberp #'keywordp)` will match a two-element list whose elements are a number and a keyword, in that order.

**`list*-matches`** is like `list-matches` but the *last* predicate in its arguments is matched against the tail of the list.  So for instance `(list-matches #'numberp #'consp)` will return a predicate which matches something whose first element is a number and which has at least one more element (but may not be a proper list).

**`cons-matches`** returns a predicate which matches a cons whose car & cdr match its two arguments.

**`list-of`** returns a predicate which matches a proper list each of whose elements matches its single argument.

**`repeating-list-of`** is quite useful: it returns a predicate which matches a proper list which consists of repeating sequences which match its arguments.  So, for instance `(repeating-list-of #'keywordp (any))` will return a predicate which matches a list of keywords & values (see below for `(any)`.

**`is`** returns a predicate which matches (with `eql`) its argument.

**`is-type`** returns a predicate which matches an object of the type given by its argument.

**`any`** returns a predicate which matches anything.

**`var`** returns a predicate which matches a variable: a nonconstant, nonkeyword symbol which is not a lambda list keyword (it is possible that lambda list keywords can be bound as variables, but don't do that).

**`lambda-list-keyword`** returns a predicate which matches a lambda list keyword.

**`some-of`** returns a predicate which matches if any of the predicates which are its arguments match.

**`all-of`** returns a predicate which matches if all of the predicates which are its arguments match.

**`none-of`** returns a predicate which matches if none of the predicates which are its arguments match.

### Notes on `spam`
It originated as part of a lambda list parser, and betrays that heritage to some extent.

*Any* predicate works: the predicates and predicate combinators it comes with are just the ones I wrote.  Many of the ones that don't exist don't exist because there already *are* predicates which match, for instance, keywords, or the empty list and so on.

There arguably should be spread versions of many of these combinators, so if you have a list of predicates you could say, for instance `(all-of* preds)` rather than `(apply #'all-of preds)`.  There might be in future.

### Package, module, dependencies
`spam` lives in `org.tfeb.hax.spam` and provides `:org.tfeb.hax.spam`.  It requires `utilities` and `simple-loops` and will attempt to load them if `require-module` is present.

## Metatronic macros
Or, recording angel.  From an idea by Zyni.

The usual approach to making CL macros less unhygienic[^18] means they tend to look like:

```lisp
(defmacro ... (...)
  (let ((xn (make-symbol "X"))
    `(... ,xn ...)))
```

Metatronic macros make a lot of this pain go away: just give the symbols you want to be gensymized names like `<x>` and everything is better:

```lisp
(defmacro/m with-file-lines ((line file) &body forms)
  `(with-open-file (<in> ,file)
     (do ((,line (read-line <in> nil <in>)
                (read-line <in> nil <in>)))
         ((eq ,line <in>))
       ,@forms)))
```

All that happens is that each symbol whose name looks like `<...>` is rewritten as a gensymized version of itself, with each identical symbol being rewritten to the same thing[^19].  As a special case, symbols whose names are `"<>"` are rewritten as unique gensymized symbols[^20].  The pattern symbols must match is controlled by a 'rewriter' function which can be changed if you don't like the default: see below.

With the above definition

```lisp
(with-file-lines (l "/tmp/x")
  (print l))
```

expands into

```lisp
(with-open-file (#:<in> "/tmp/x")
  (do ((l (read-line #:<in> nil #:<in>)
          (read-line #:<in> nil #:<in>)))
      ((eq l #:<in>))
    (print l)))
```

where, in this case, all the `#:<in>` symbols are the same symbol.

**`defmacro/m`** is like `defmacro` except that metatronic symbols are rewritten.

**`define-compiler-macro/m`** is like `define-compiler-macro` except that metatronic symbols are rewritten.

**`macrolet/m`** is like `macrolet` except that metatronic symbols are rewritten.

**`metatronize`** does the rewriting and could be used to implement similar macros.  It has one positional argument and three keyword arguments:

- `form` is the form to be rewritten;
- `rewrites`, if given, is a table of rewrites returned from a previous call to `metatronize`;
- `sharing`, if given, is a table with information on structure sharing from a previous call to `metatronize` which it will use to possibly share structure with the `form` argument to that previous call;
- `rewriter`, if given, is a function of one argument, a symbol, which should return either its argument and any value or a gensymized version of it and an indication of whether it should be stored in the rewrite table.  The default value is `*default-metatronize-rewriter*`.

`metatronize` returns four values:

- the rewritten form;
- a table of rewrites which can be handed to a later call to `metatronize`;
- a list of unique symbols, which are usually the symbols that symbols whose names are `<>`get rewritten to;
- a sharing table describing shared structure in the form.

**`*default-metatronize-symbol-rewriter*`** is bound to the default symbol rewriter used by `metatronize`.  Changing it will change the behaviour of `metatronize` and therefore of `defmacro/m` and `macrolet/m`.  Reloading `metatronic` will reset it if you break things.

**`rewrite-sources`** and **`rewrite-targets`** return a list of sources and targets from the rewrite table returned by `metatronize`.

### Notes
Macros written with `defmacro/m` and `macrolet/m` in fact metatronize symbols *twice*: once when the macro is defined, and then again when it is expanded, using lists of rewritten & unique symbols from the first metatronization to drive a `rewriter` function.  This ensures that each expansion has a unique set of gensymized symbols:  with the above definition of `with-file-lines`, then

```lisp
> (eq (caadr (macroexpand-1 '(with-file-lines (l "/tmp/x") (print l))))
      (caadr (macroexpand-1 '(with-file-lines (l "/tmp/x") (print l)))))
nil
```

If you inspect the expansion of `defmacro/m` forms carefully you can still infer what the names of the gensymized symbols will be before they are metatronized again, and hence subvert metatronization.  Don't do that.

One consequence of this double-metatronization is that you should not use metatronic variables in the arglists of metatronic macros, because the arglists can't be metatronized a second time.  An earlier version did allow this but at the cost of only metatronizing once.  The current version checks for this and will signal a `style-warning` (in fact a subclass of it) if it finds any.

`metatronize` and hence `defmacro/m` only looks at list structure: it does not look into arrays or structures and return suitable copies of them as doing that in general is absurdly hard.  If you want to rewrite the contents of literals then the best approach is to use `load-time-value` and a constructor to do this.

`metatronize` is *not a code walker*: it just blindly replaces some symbols with gensymized versions of them.  Metatronic macros are typically easier to make more hygienic than they would otherwise be but they are very far from being hygienic macros.

The tables used by`metatronize` are currently alists, which will limit its performance on vast structure.  They may not always be, but they probably will be since macro definitions are not usually vast.  Do not rely on them being alists, and in particular use `rewrite-sources` and `rewrite-targets` on the rewrite table.

`metatronize` does deal with sharing and circularity in list structure properly (but only in list structure).  Objects which are not lists and not metatronic symbols are not copied of course, so if they were previously the same they still will be in the copy.

### Writing more complicated macros
All `defmacro/m` does is use `metatronize` to walk over the forms in the body of the macro, rewriting symbols appropriately.  The expansion of the `with-file-lines` macro above is

```lisp
(defmacro with-file-lines ((line file) &body forms)
  (m2 (progn
        `(with-open-file (#:<in> ,file)
           (do ((,line (read-line #:<in> nil #:<in>)
                       (read-line #:<in> nil #:<in>)))
               ((eq ,line #:<in>))
             ,@forms)))
      '(#:<in>)
      'nil))
```

where `m2` is an internal function which knows how to rewrite specific symbols: this is done so that even if you look at the expansion you can't extract the gensyms.

This is fine for macros like that, but there's a common style of macro which looks like this:

```lisp
(defmacro iterate (name bindings &body body)
  (expand-iterate name bindings body nil))
```

Where`expand-iterate` is probably some function which expands variations on `iterate`[^21].  Well, the answer is that it's complicated.  But where, in a function like `expand-iterate`, you have code like:

```lisp
(let ((secret-name (make-symbol ...)))
  ...
  `(labels ((,secret-name ...))
     (,secret-name ...)))
```

You can replace this with

```lisp
(values
 (metatronize
  `(labels ((<secret-name> ...))
     (<secret-name> ...))))
```

for instance.  Here `values` is just suppressing the other values from `metatronize` which you don't need in this case.

However in general metatronic macros are far more useful for simple macros where there is no complicated expander function like this: that's what it was intended for.

### Package, module, dependencies
`metatronic` lives in and provides `:org.tfeb.hax.metatronic`.  It requires `utilities` and will attempt to load it if `require-module` is present.

## Simple logging: `slog`
`slog` is based on an two observations about the Common Lisp condition system:

- conditions do not have to represent errors, or warnings, but can just be a way of a program saying 'look, something interesting happened';
- handlers can decline to handle a condition, and in particular handlers are invoked *before the stack is unwound*.

Well, saying 'look, something interesting happened' is really quite similar to what logging systems do, and `slog` is built on this idea.

`slog` is the *simple* logging system: it provides a framework on which logging can be built but does not itself provide a vast category of log severities &c.  Such a thing could be built on top of `slog`, which aims to provide mechanism, not policy.

### Almost a simple example
Given

```lisp
(defvar *log-stream* nil)

(define-condition my-log-entry (simple-log-entry)
  ((priority :initform 0
             :initarg :priority
             :reader log-entry-priority)))

(defun foo (n)
  (logging ((my-log-entry
             (lambda (entry)
               (when (> (log-entry-priority entry) 1)
                 (slog-to *log-stream* entry))))
            (t
             "/tmp/my.log"))
    (bar n)))

(defun bar (n)
  (dotimes (i n)
    (if (evenp i)
        (slog "i is ~D" i)
      (slog 'my-log-entry
            :format-control "i is ~D"
            :format-arguments (list i)
            :priority i))))
```

then

```lisp
> (foo 10)
```

Will cause `/tmp/my.log` to have text like the following appended to it:

```lisp
3862463894.708 i is 0
3862463894.709 i is 1
3862463894.709 i is 2
3862463894.709 i is 3
3862463894.709 i is 4
3862463894.709 i is 5
3862463894.709 i is 6
3862463894.709 i is 7
3862463894.709 i is 8
3862463894.709 i is 9
```

The logging format is configurable of course: the numbers are high-precision universal-times, which in the implementation I am using are accurate to 1/1000s.

On the other hand, this:

```lisp
> (let ((*log-stream* *standard-output*))
    (foo 10))
```

Will cause `/tmp/my.log` to be appended to and the following to be printed:

```lisp
3862464054.581 i is 3
3862464054.581 i is 5
3862464054.581 i is 7
3862464054.581 i is 9
```

The same results could be obtained by this code:

```lisp
(defun foo (n)
  (logging ((my-log-entry
             (lambda (entry)
               (when (> (log-entry-priority entry) 1)
                 (slog-to *log-stream* entry)))
             "/tmp/my.log"))
    (bar n)))

(defun bar (n)
  (dotimes (i n)
    (slog 'my-log-entry
          :format-control "i is ~D"
          :format-arguments (list i)
          :priority (if (oddp i) i 0))))
```

In this case `logging` will log to two destinations for `my-log-entry`.

### Log entries
**`log-entry`** is a condition type which should be an ancestor of all log entry conditions.  It has a single reader function: `log-entry-internal-time`, which will retrieve the internal real time when the log entry was created.  Log formatters use this.

**`simple-log-entry`** is a subtype of both `log-entry` and `simple-condition`: it's the default log entry type when the `datum` argument to the two logging functions is a string.

**`once-only-log-entry`** is a condition type which will be logged to at most one destination.

### Logging functions
**`(slog datum [arguments ...])`** takes arguments which denote a condition of default type `simple-log-entry` and signals that condition.  The sense in which the 'arguments denote a condition' is exactly the same as for `signal` &c, except that the default condition type is the value of `*default-log-entry-type*`.

**`(slog-to destination datum [arguments ...])`** creates a log entry as `slog` does, but then rather than signalling it logs it directly to `destination`.  `slog-to` is what ends up being called when logging destinations are specified by the `logging` macro, but you can also call it yourself.

**`*default-log-entry-type*`** is the condition class used by `slog` when its first argument is a string.  Its default value is `simple-log-entry` but you can bind or assign to it to control what class is used.

### Log destinations and `slog-to`
The `logging` macro and the `slog-to` generic function know about *log destinations*.  Some types of these are predefined, but you can extend the notion of what a log destination is either by defining methods on `slog-to` (see below for caveats) or, perhaps better, by providing a *fallback destination handler* which `slog-to` will call for destination handlers it does not have specialised methods for.  This fallback handler can be bound dynamically.

The destinations that `slog` knows about already are:

- streams -- the report is written to the stream;
- strings or pathnames designate filenames which are opened if needed, with any needed directories being created, and then the report written to the resulting stream (see below on file handling);
- function designators -- the function is called to handle the entry;
- the symbol `nil` causes the entry to be discarded;
- any other destination type invokes the fallback handler (see below).

Generally `slog-to` tries to return the condition object, however in the case where the destination is a function it returns whatever the function returns, and in the case where it calls the fallback handler its value is whatever that returns.  It's probably not completely safe to rely on its return value.

You can define methods on `slog-to` to handle other destination classes, or indeed log entry classes.  Caveats:

- `slog-to` has an argument precedence order of `(datum destination)`, which is the inverse of the default;
- methods on `slog-to` should be defined only for classes you define, and not for any standard CL classes or any classes defined by `slog` itself.

**`*fallback-log-destination-handler*`** is the fallback log destination handler.  If bound to a function, then `slog-to` will, by default, call this function with three arguments: the destination, the log entry, and a list of other arguments passed to `slog-to`.  The function is assumed to handle the entry and its return value or values are returned by `slog-to`.  The default value of this variable is `nil` which will cause `slog-to`to signal an error.

### Log entry formats
In the case of destinations which end up as streams, the format of what is written into the stream is controlled by `*log-entry-formatter*`.

**`*log-entry-formatter*`** is bound to a function of two arguments: a destination stream and the log entry.  It is responsible for writing the log entry to the stream.  The default value of this writes lines which consist of a high-precision version of the universal time (see below), a space, and then the printed version of the log entry, as defined by its report function.  This variable can be redefined or bound to be any other function.

**`default-log-entry-formatter`** is a function which returns the default value of `*log-entry-formatter*`.

If you want to have fine-grained control over log entry formats then two possible approaches are:

- you could make the value of `*log-entry-formatter*` be a generic function which can then dispatch on its second argument to return an appropriate log format;
- and/or you could define methods on `slog-to` for suitable `log-entry` subclasses which can select entry formats appropriately.

The second approach allows you, for instance, to select locale-specific formats by passing keyword arguments to specify non-default locales to `slog-to`, rather than just relying on its class alone[^22].

### The `logging` macro
**`(logging ([(typespec destination ...) ...]) form ...)`** establishes dynamic handlers for log entries which will log to the values of the specified destinations.  Each `typespec` is as for `handler-bind`, except that the type `t` is rewritten as `log-entry`, which makes things easier to write.  Any type mentioned in `typespec` must be a subtype of `log-entry`.  The value of each destination is then found, with special handling for pathnames (see below) and these values are used as the destinations for calls to `slog-to`.  As an example the expansion of the following form:

```lisp
(logging ((t "foo"
             *log-stream*))
  ...)
```

is similar to this:

```lisp
(closing-opened-log-files ()
  (handler-bind ((log-entry
                  (let ((d1 (canonicalize-destination "foo"))
                        (d2 (canonicalize-destination *log-stream*)))
                    (lambda (e)
                      (slog-to d1 e)
                      (slog-to d2 e)))))
  ...))
```

where `d1`, `d2` and `e` are gensyms, and the (internal) `canonicalize-destination` deals with destinations which represent files.  See below for `closing-opened-log-files`.

The result of this is that `logging` behaves as if the destinations were lexically scoped.  So for instance this will not 'work':

```lisp
(defvar *my-destination* nil)

(let ((*my-destination* "my.log"))
  (logging ((t *my-destination*))
    (foo)))

(defun foo ()
  (setf *my-destination* nil)
  (slog "foo"))
```

The log output will still go to `my.log`.  If you want this sort of behaviour it's easy to get: just use a function as a destination:

```lisp
(let ((*my-destination* "my.log"))
  (logging ((t (lambda (e)
                 (slog-to *my-destination* e))))
    (foo)))
```

And now modifications or bindings of `*my-destination*` will take effect.

One important reason for this behaviour of `logging` is to deal with this problem:

```lisp
(logging ((t "foo.log"))
  (foo))

(defun foo ()
  (slog "foo")
  ... change what file "foo.log" would refer to ...
  (slog "bar"))
```

Because the absolute pathname of `foo.log`is computed and stored at the point of the logging macro you won't end up logging to multiple files: all the log entries will go to whatever the canonical version of `foo.log` was at the point that `logging` appeared.

Finally note that calls to `slog` are completely legal but will do nothing outside the dynamic extent of a `logging` macro[^23], but `slog-to` will work quite happily and will write log entries.  This includes when given pathname arguments: it is perfectly legal to write code which just calls `(slog-to "/my/file.log" "a message")`.  See below on file handling.

Note that destinations which correspond to files (pathnames and strings) are opened by `logging`, with any needed directories being created and so on.  This means that if those files *can't* be opened then you'll get an error immediately, rather than on the first call to `slog`.

### File handling
For log destinations which correspond to files, `slog` goes to some lengths to try and avoid open streams leaking away and to make sure there is a single stream open to each log file.  This is not as simple as it looks as `slog-to` can take a destination argument which is a filename, so that users don't have to write a mass of code which handles streams, and there's no constraint that `slog-to` must be called within the dynamic extent of a `logging` form.

Behind the scenes there is a stack of log file objects which know both their true pathnames (from `truename`), as well as the corresponding stream if they're open, thus providing a map between truenames and streams.  When given destinations which are file designators both `logging` and `slog-to` will first probe files to see if they exist, and use the corresponding  true name (or the pathname if the file is not found) to find or create suitable entries in this map.  There are then a number of utilities to deal with this map.

**`closing-opened-log-files`** is a macro which establishes a saved state of the map between true names and streams.  On exit it will close any log file streams added to the map within its dynamic extent (using `unwind-protect` in the obvious way) and restore the saved state (this is all done using special variables in the obvious way so is thread-safe).  So for instance

```lisp
(closing-opened-log-files ()
  (slog-to "foo.log" "here"))
```

will close the open stream to `foo.log` if `slog-to` opened it.  The full syntax is

```lisp
(closing-opened-log-files (&key reporter abort)
  ...)
```

Where `abort`is simply passed to the `close` calls.  `reporter`, if given, should be a function which will be called with the name of each file closed.

`logging` expands into something which uses `closing-opened-log-files`.

**`current-log-files`** is a function which will return two values: lists of the true names of currently open log files, and of currently closed log files.   It has a single keyword argument:

-`all`, if given as true will  cause it to return the elements of the whole list, while otherwise it will return lists just back to the closest surrounding `closing-opened-log-files`.

**`close-open-log-files`** will close currently open log files, returning two lists: the list of files that were open and the list of files which were already closed.  It takes four keyword arguments:

- `abort`is passed as the `abort` keyword to `close`;
- `all` is as for `current-log-files`;
- `reset`, if given, will reset the map to the save point of the nearest `closing-open-log-files` (or completely if there is no nearest, or if `all` is given);
- `test` should be a function of one argument, a pathname, which will cause only those log files for which the function succeeds to be closed.

You can't use both `test` and `reset` because it would leak streams.

**`flush-open-log-file-streams`** will flush open log file streams to their files.   It returns a list of the filenames streams were flushed to.  It has three keyword arguments:

- `all` is as for `current-log-files`;
- `wait`, if true, will cause it to call `finish-output` rather than `force-output`;
- `test` is as for `close-open-log-files`, and only the files which pass the test will be flushed.

Note that it is perfectly OK to close a log file stream whenever you like: the system will simply reopen the file if it needs to.  This is fine for instance:

```lisp
(logging ((t "/tmp/file.log"))
  (slog "foo")
  (close-open-log-files)
  (slog "bar"))
```

What will happen is that the handler will open the file when handling the first condition raised by `slog`, the file will be closes, and then the handler will reopen it.  You can see this at work: given

```lisp
(defun print-log-files (prefix)
  (multiple-value-bind (opened closed) (current-log-files)
    (format t "~&~Aopen   ~{~A~^ ~}~%" prefix opened)
    (format t "~&~Aclosed ~{~A~^ ~}~%" prefix closed)))
```

then

```lisp
> (logging ((t "/tmp/file.log"))
    (print-log-files "before ")
    (slog "foo")
    (print-log-files "after slog ")
    (close-open-log-files)
    (print-log-files "after close ")
    (slog "bar")
    (print-log-files "after close "))
before open
before closed
after slog open   /tmp/file.log
after slog closed
after close open
after close closed /tmp/file.log
after close open   /tmp/file.log
after close closed
nil
```

If you call `slog-to` with a filename destination*outside* the dynamic extent of `logging` or `closing-opened-log-files` then you perhaps want to call `(close-open-log-files :reset t)` every once in a while as well (the `reset` argument doesn't really matter, but it cleans up the  map).

**`log-file-truename`** is a little utility function: it will return the `truename` of a filename designator, making sure any needed directories are created and creating & opening the file if need be.  It's useful just so user code does not need to duplicate what `slog` does, and particularly if you're calling `slog-to` explicitly in situations like:

```lisp
(let ((logname (log-file-truename "my-log.log")))
  ... perhaps change directory etc ...
  (slog-to logname ...)
  ...)
```

Finally note that all this mechanism is only about filename destinations: if you log to stream destinations you need to manage the streams as you normally would.  The purpose of it is so you can simply not have to worry about the streams but just specify what log file(s) you want written.

### Checking the log file map is sane
**`log-files-sane-p`** is a function which will check lists of files for sanity.  It has two arguments: a list of true names of open log files, and a list for closed log files.  These are just the two lists that `current-log-files` returns.  It will return `nil` if there is some problem such as duplicate entries, or files appearing on both lists.  This should not be able to happen, but it's better to be able to know if it does.  In addition it has two keyword arguments:

- `warn` will signal a warning for each wrong thing it finds;
- `error` will cause it to signal an error rather than returning `nil`.

So, for instance

```lisp
(multiple-value-bind (opened closed) (current-log-files :all t)
  (log-files-sane-p opened closed :warn t :error t))
```

would cause a suitable error (preceded by warnings) if anything was wrong.

### Precision time
`slog` makes an attempt to write log entries with a 'precision' version of CL's universal time.  It does this by calibrating internal real time against universal time when loaded (this means that loading `slog` takes at least a second and can take up to two seconds or more) and then using this calibration to record times with the precision of internal time.  There is one function which is exposed for this.

**`get-precision-universal-time`** returns three values:

- the best idea of the precise universal time it can work out, by default as a rational number;
- the rate of the precision time 'clock' in ticks per second (see below);
 - the number of decimal places to which this quantity should be formatted if printed in units of seconds.

The last value is just `(ceiling (log rate 10))` where `rate` is the second value, but it saves working it out (and if you don't supply the rate this is all computed at compile time).

It has three keyword arguments:

- `it` is the internal real time for to return the precision time for, which by default is `(get-internal-real-time)`;
- `type` tells you what type to return, and can be one of
	- `rational` or `ratio` return a rational,
	- `float` or `double-float` return a double float,
	- `single-float` returns a single float,
	- `short-float` returns a short float;
- `rate` specifies the rate at which the prevision time clock ticks, with the default currently being the minimum of `internal-time-units-per-second` and `1000`;
- `chide`, if given will chide you about silly choices for the other arguments, in particular if you request a `rate` more than `internal-time-units-per-second`, or a silly float format.

*Do not use the `single-float` or `short-float` types*: single floats don't have enough precision to be useful!

The default rate is the *minimum* of `internal-time-units-per-second` and `1000`: so precision time is supposed to be accurate to a millisecond at most.  It seems  obvious that the rate should instead be `internal-time-units-per-second` which may be much higher for, say, SBCL, but the effective tick rate for SBCL on at least some platforms is much much lower than `internal-time-units-per-second`, so making the default `rate` be that results in printing times with at least three junk digits at the end.

For example:

```lisp
> (get-precision-universal-time)
3862739452211/1000
1000
3

> (get-precision-universal-time :type 'double-float)
3.862739466635D9
1000
3

> (get-precision-universal-time :type 'single-float :chide t)
Warning: single-float is almost certainly not precise enough to be useful
3.8629166E9
1000
3
```

There are some sanity tests for this code which are run on loading `slog`, because I'm not entirely convinced I have got it right.  If you get warnings on load this means it is almost certainly wrong.

You can use `get-precision-universal-time` to write your own formatters, using `log-entry-internal-time` to get the time the entry was created.

**`reset-precision-time-offsets`** resets, or just reports, the offsets for precision time.  Resetting can be necessary to reset the calibration, for instance when an image is saved and reloaded.  It returns four values:

- the universal time at which the clock ticked;
- the corresponding internal time;
- the previous value of the universal time at which the clock ticked;
- the previous corresponding internal time.

`reset-prevision-time-offsets` takes two keyword arguments:

- `report-only` says just to report the four values rather than resetting the internal parameters;
- `tries` tells it how many times to try: if it takes more than a single attempt there will be a warning, and an error if more than `tries`.  The default value is `3`.

`reset-precision-time-offsets` takes at least second to run: it's the function that gets called when `slog` is loaded.

A function defined as

```lisp
(defun ts ()
  (multiple-value-bind (ut0 it0 put0 pit0) (reset-precision-time-offsets :report-only t)
    (values (float (- ut0 (/ it0 internal-time-units-per-second)) 1.0d0)
            (float (- put0 (/ pit0 internal-time-units-per-second)) 1.0d0))))
```

Should, unless the precision time needs to be reset, return two very close values, and probably two values which are actually the same.

### An example: rotating log files
```lisp
(defun rotate-log-files (&key (all nil))
  ;; Obviously this would want to be more careful in real life
  (flet ((rotate-one (file)
           (let ((rotated (make-pathname :type "old"
                                         :defaults file)))
               (when (probe-file rotated)
                 (delete-file rotated))
               (when (probe-file file)
                 (rename-file file rotated)))))
    (multiple-value-bind (newly-closed already-closed) (close-open-log-files :all all)
      (dolist (a already-closed)
        (rotate-one a))
      (dolist (n newly-closed)
        (rotate-one n)))))
```

### Notes
A previous version of `slog` handled files rather differently: it tried to delay creating or opening them as long as possible, and thus needed to be able to find an absolute pathname for a file which it had not opened or created, requiring it to have a notion of the current directory which was better than `*default-pathname-defaults*`.  This is now all gone: the current version simply treats pathnames as they are.

Condition objects are meant to be immutable: from the [CLHS](http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_5.htm "define-condition")

> The consequences are unspecified if an attempt is made to assign the slots by using `setf`.

So `once-only-log-entry` gets around this by storing a mutable cons in one of its slots which records if it's been logged.

I'm not completely convinced by the precision time code.

`slog` is `slog` not `log` because `log` is `log`.  `slog-to` is named in sympathy.

Logging to pathnames rather than explicitly-managed streams may be a little slower, but seems now to be pretty close.

### Package, module, dependencies
`slog` lives in and provides `:org.tfeb.hax.slog`. It requires `utilities`, `collecting`, `spam` and `metatronic`and will attempt to load them if `require-module` is present.

## Binding multiple values: `let-values`
`let-values` provides four forms which let you bind multiple values in a way similar to `let`: you can bind several sets of them in one form.  The main benefit of this is saving indentation.

**`let-values`** is like `let` but for multiple values.  Each clause consists of a list of variables and an initform, rather than a single variable.   The variables are bound to the multiple values returned by the form.  Variables are bound in parallel as for `let`.  The initform may be omitted which is equivalent to it being `nil`.  Each binding clause may be

| clause                | equivalent to    |
| --------------------- | ---------------- |
| `((var ...) [form]))` |                  |
| `(var [form])`        | `((var) [form])` |
| `var`                 | `((var))`        |
[Forms of possible clause]

**`let*-values`** is like `let*` but for multiple values.  Each clause is bound within the scope of the previous clauses.

**`let-values*`** is a variant of `let-values` which has the semantics of `multiple-value-call`: Each clause may have a number of initforms, and the variables are bound to the combined values of all of the forms.

**`let*-values*`** is to `let-values*` as `let*-values` is to `let-values`.

Both `let-values*` and `let*-values*` are fussy about the number of values, because `multiple-value-call` is: `(let-values* (((a))) ...)` is an error, as is `(let-values (((a) 1 2)) ...)`.

Declarations should be handled properly (`(declare (type fixnum ...))` is better than `(declare (fixnum ...))` but both should work).  As an example

```lisp
(let-values (((a b) (floor 2.4)) ((c d) (floor 8.5)))
  (declare (integer a c) (single-float b d))
  (values (+ a b) (+ c d)))
 -> (multiple-value-bind (#:a #:b)
        (floor 2.4)
      (declare (type integer #:a))
      (declare (type single-float #:b))
      (multiple-value-bind (#:c #:d)
          (floor 8.5)
        (declare (type integer #:c))
        (declare (type single-float #:d))
        (let ((a #:a) (b #:b) (c #:c) (d #:d))
          (declare (integer a c) (single-float b d))
          (values (+ a b) (+ c d)))))
```

In the `let*`-style cases the declarations will apply to all duplicate variables.

`special` declarations are an interesting case for sequential binding forms.  Consider this form:

```lisp
(let*-values (((a) 1)
              ((b c) (f)))
  (declare (special a))
  ...)
```

Now, without knowing what `f` does, it could refer to the dynamic binding of `a`.  So the special declaration for `a` needs to be made for the temporary binding as well, unless it is in the final group of bindings.  The starred forms now do this.

### Package, module, dependencies
`let-values` lives in and provides `:org.tfeb.hax.let-values`.  It requires `spam`, `collecting`, `iterate` `utilities` and `process-declarations`,  and will attempt to load them if `require-module` is present.

## Processing declaration specifiers: `process-declarations`
When writing macros it's useful to be able to process declaration specifiers in a standardised way.  In particular it's common to want to select all specifiers which mention a variable and perhaps create equivalent ones which refer to some new variable introduced by the macro.

Things in this system are *probably not stable*.  I want to publish it so other things can rely on it, but its details may change.

### Terminology
A `declare` expression[^24] is of the form `(declare <declaration-specifier> ...)`, where a `<declaration-specifier>` is of the form `(<declaration-identifier> ...)`.  A `<declaration-identifier>` is a symbol, which may be the name of a type[^25]: `(<type> ...)` is a shorthand for the canonical `(type <type> ...)`.  There are a number of standard identifiers, and nonstandard ones can be portably declared as valid identifiers.  This terminology is the same as that used in the standard.

### What you can do
`process-declaration-specifier` lets you call a function on a processed declaration specifier, with the function being handed information which tells it the identifier (canonicalised in the case of type declarations), which variable and function names it applies to if any, and a function which will construct a similar specifier for possibly-different sets of variable or function names or other information.  Other information can be handed to the function.

`processing-declaration-specifier` is a slightly restricted macro version of `process-declaration-specifier`.

Methods on `process-declaration-identifier` allow you to define handlers for declaration identifiers which describe how the function should be called.  Suitable methods exist for all standard declaration identifiers, so for normal usage you will never need to write methods on this generic function.

### The interface
**`process-declaration-specifier`**  calls a function on a processed declaration specifier.  It takes two positional arguments and any number of keyword arguments.  Positional arguments are:

- the declaration specifier
- the function to call on the processed specifier

The only keyword argument used by the function itself is `environment`, which specifies an optional environment object.  This can be used to infer things about types at compile time.  All keyword arguments, including `environment` are passed to the function being called.

The given function is called with two positional arguments and a number of keyword arguments, including all those given to `process-declaration-specifier` itself.  Positional arguments are

- the canonical declaration identifier, which for a declaration specifier like `(fixnum ...)` will be `type`.
- a function which will construct a suitable declaration specifier of the same kind as the one being processed.

All other arguments to the given function are keyword arguments and they will always include

- `variable-names`, a list of variable names for this specifier, if any;
- `function-names`, a list of function names for this specifier, if any;
- `specifier`, the canonical version of the whole specifier;
- `original-specifier`, the original specifier, which may differ for shorthand type specifiers;
- `specifier-body`, the cdr of `specifier`;
- `environment`, the environment, or `nil`

In addition any keyword arguments passed to `process-declaration-specifier` are passed to the function.

The above keyword arguments are all the arguments passed to functions for standard declaration identifiers.  Non-standard ones may pass additional arguments, based on methods on `process-declaration-identifier`.

The second argument to the function is itself a function, whose job is to construct a declaration specifier of the same kind as the one being processed, but usually for other arguments.  This function again takes zero or more keyword arguments: which arguments it accepts depend on the declaration identifier.  For the standard identifiers here are the arguments.

- `type`: `variable-names`, a list of variable names.
- `ftype`: `function-names`, a list of function names.
- `special` and `dynamic-extent`: `variable-names`, a list of variable names.
- `inline` and `notinline`: `function-names`, a list of function names.
- `ignore` and `ignorable`: both `variable-names` and `function-names` are allowed.
- All other standard declaration identifiers have constructor functions which take no arguments and simply return the canonical specifier.

Constructor functions are fussy about their arguments: they *don't* allow unknown keyword arguments.  This means you get an error rather than silently get the wrong anser.

`process-declaration-specifier` returns what the function called returns.

**`processing-declaration-specifier`** is a slightly restruicted macro version of `process-declaration-specifier`.  It is probably unstable.  The syntax of this form is

```lisp
(processing-declaration-specifier (<specifier>
                                   &key
                                   identifier
                                   constructor
                                   bindings
                                   environment)
  <body> ...)
```

Here `<specifier>` is the specifier to process, and `<body>` is the code.  The keyword arguments let you say which variables you want to bind:

- `identifier` is a variable which will be bound to the identifier of the specifier;
- `constructor`, if given, names a local function which will construct a new specifier, wrapping the second argument to the function called by `process-declaration-specifier`;
- `bindings` if given is a number of keyword-argument specifiers, as for the function called by `process-declaration-specifier`;
- `environment`, if given, is an environment object.

See below for how this macro can be used.

**`process-declaration-identifier`** is a generic function used to implement `process-declaration-specifier` for specific declaration identifiers.  *Unless you want to support non-standard identifiers, you do not need to implement methods on this function*.  You should not implement methods for any standard identifiers, or a fallback method, as these already exist.

The generic function is called with two positional arguments and any number of keyword arguments (the GF itself has `&allow-other-keys` so methods don't need to say this themselves).  The positional arguments are:

- the declaration identifier, which shoul be used for dispatch;
- the function handed to `process-declaration-specifier`.

Keyword arguments include all those specified above, as well as any user-provided arguments to `process-declaration-specifier`.  Note that the values of all the keyword arguments are the default ones, so for instance `variable-names` will be `()`: methods are responsible for providing non-default values where needed.  Note that CL's keyword-argument parsing, which  allows repeated keywords and takes the value of the leftmost one, makes this easy.

Methods on the generic function are responsible for arranging for the user function to be called in the right way, and in particular with an appropriate constructor function.

Methods should return the values of the user function.

### Examples
All the above sounds pretty obscure, but it's much easier to use than it seems.  Here is a typical case: I want to create equivalent `type` declarations for 'hidden' variables corresponding to a number of variables in a macro.  Here is a function which will do this, using `collecting` for result accumulation.

```lisp
(defun type-declarations-for-varmap (decls varmap environment)
  ;; DECLS is the declarations, VARMAP the alist from (var .
  ;; secret-var) and ENVIRONMENT the macro environment or NIL
  `(declare
    ,@(collecting
        (dolist (decl decls)
          (dolist (specifier (rest decl))
            (process-declaration-specifier
             specifier
             (lambda (identifier constructor &key variable-names &allow-other-keys)
               (case identifier
                 (type                      ;only care about these
                  (let ((mapped-variables
                         (collecting
                           (dolist (v variable-names)
                             (let ((found (assoc v varmap)))
                               ;; only care if there is a mapped variable
                               (when found (collect (cdr found))))))))
                    (collect (funcall constructor :variable-names mapped-variables))))))
             :environment environment))))))
```

Or, using `processing-declaration-specifier`:

```lisp
(defun type-declarations-for-varmap (decls varmap environment)
  ;; DECLS is the declarations, VARMAP the alist from (var .
  ;; secret-var) and ENVIRONMENT the macro environment or NIL
  `(declare
    ,@(collecting
        (dolist (decl decls)
          (dolist (specifier (rest decl))
            (processing-declaration-specifier (specifier
                                               :identifier identifier
                                               :constructor maker
                                               :bindings ((variable-names '()))
                                               :environment environment)
              (case identifier
                (type                      ;only care about these
                 (let ((mapped-variables
                        (collecting
                          (dolist (v variable-names)
                            (let ((found (assoc v varmap)))
                              ;; only care if there is a mapped variable
                              (when found (collect (cdr found))))))))
                   (collect (maker :variable-names mapped-variables)))))))))))
```

And now, for either version:

```lisp
> (type-declarations-for-varmap
   '((declare (optimize speed))
     (declare (ignorable a b)
              (fixnum a)
              (type float b c)))
   '((a . secret-a)
     (b . secret-b))
   nil)
(declare (type fixnum secret-a) (type float secret-b))
```

It is generally never necessary to write methods on `process-declaration-identifier`: if you need to you should look at the source to see what they look like.

### Package, module, dependencies
`process-declarations` lives in and provides `:org.tfeb.hax.process-declarations`.  It needs `utilities` and will attempt to load it if `require-module` is present.

## Small utilities: `utilities`
This is used both by other hax and by other code I've written.  Things in this system *may not be stable*: it should be considered mostly-internal.  However, changes to it *are* reflected in the version number of things, since other systems can depend on things in it.  Almost all of the utilities are thing which help writing macros.

Here is what it currently provides.

- `parse-docstring-body` parses the body of a function with possible documentation and declarations into three values: docstring, list of declarations and remaining forms.  With luck it now dies this correctly (the docstring and declarations can be intermingled).
- `parse-simple-body` is like `parse-docstring-body` but it does not handle docstrings & only returns two values.
- `symbolify` makes symbols by concatenating string designators, and optionally interns them.  If its first argument is a package designator, or `t` meaning the value of `*package*`, it will intern the result of concatenating the remaining string designators in that package.  If it is `nil` it will return an uninterned symbol.  This is a slightly odd argument convention but I can't think of a better one.  Example: if `s` is the symbol `foo` then `(symbolify nil s "-P")` will return an uninterned symbol whose name is `FOO-P`.
- `stringify` is `symbolify` but it just makes strings.  It doesn't have the special first argument, or course: all its arguments are string designators.
- `with-names` binds variables to uninterned symbols with the same name by default: `(with-names (<foo>) ...)`will bind `<foo>` to a fresh uninterned symbol with name `"<FOO>"`.  `(with-names ((<foo> foo)) ...)` will bind `<foo>` to a fresh uninterned symbol with name `"FOO"`.
- `thunk` makes anonymous functions with no arguments: `(thunk ...)` is `(lambda () ...)`.
- `thunk*` makes anonymous functions which take an arbitrary number of arguments and ignore them all.
- `valid-type-specifier-p` attempts to answer the question 'is something a valid type specifier?'.  It does this, normally[^26], by checking that `(typep nil <thing>)` does not signal an error, which I think is a loophole-free way of answering this question (SBCL, again, says incorrectly that`(subtypep <thing> t)`is true for any `<thing>` which looks even slightly like a type specifier).  There is an optional second argument which is an environment object handed to `typep`: using this lets it answer the question for the compilation environment: see [this CLHS issue](https://www.lispworks.com/documentation/HyperSpec/Issues/iss334.htm "Issue `SUBTYPEP-ENVIRONMENT:ADD-ARG` Summary").
- `canonicalize-declaration-specifier` attempts to turn the shorthand `(<type> ...)` declaration specifier into a canonical `(type <type> ...)`.  It does this using `valid-type-specifier-p`.  Its optional second argument is an environent object passed to `valid-type-specifier-p`.  The spec says that a declaration identifier is 'one of the symbols [...]; *or a symbol which is the name of a type*' [my emphasis.  This means that a declaration specifier like `((integer 0) ...)` is not legal.  Several implementations accept these however, so this function blindly turns such things into type declaration specifiers.  It returns a second value which will be false for one of these, true otherwise.

### Package, module
The utilities live in and provide `:org.tfeb.hax.utilities`.

---

The TFEB.ORG Lisp hax are copyright 1989-2025 Tim Bradshaw.  See `LICENSE` for the license.

---

[^1]:	The initial documentation for these hacks was finished on 20210120 at 18:26 UTC: one hour and twenty-six minutes after Joe Biden became president of the United States.

[^2]:	  I expect both `abstract-classes` and `singleton-classes` might have portability problems around the MOP, and I'd welcome fixes to these.

[^3]:	The modern form of `loop` also did not portably exist when `collecting` was written.

[^4]:	Once upon a time they were local macros, because I didn't trust ancient CL compilers to inline functions, with good reason I think.

[^5]:	Information on Interlisp can be found at [interlisp.org](https://interlisp.org/), and the Interlisp-D reference manual is [here](https://interlisp.org/docs/IRM.pdf) (PDF link).

[^6]:	Empirically, this implementation is as fast or faster than implementations which use an intermediate list or smaller vector buffers which get copied into the final vector at the end.

[^7]:	Note that a `simple-vector` is *not* the same thing as a one-dimensional `simple-array`: a `simple-vector` is a one-dimensional `simple-array` whose element type is `t`.  `svref` is therefore *not* the way to get fast vector access: `aref` on a suitably-declared `simple-array` is.

[^8]:	If you are using such an implementation, well, sorry.

[^9]:	 Fortunately, and a bit surprisingly to me, Python has facilities which let you do this fairly pleasantly.  Something on my todo list is to make this implementation public.

[^10]:	See [* 'Memo' Functions and Machine Learning*](https://doi.org/10.1038%2F218019a0 "'Memo' Functions and Machine Learning"), Donald Michie, Nature 218 (5136): 19–22.  [PDF copy](https://www.cs.utexas.edu/users/hunt/research/hash-cons/hash-cons-papers/michie-memo-nature-1968.pdf "'Memo' Functions and Machine Learning").

[^11]:	And I was not willing to put in explicit extra methods for `validate-superclass` for `cl:standard-class` since the whole purpose of using Closer to MOP was to avoid that kind of nausea.

[^12]:	In fact, Racket.

[^13]:	Or, optionally, not to skip the newline but to skip any whitespace following it.

[^14]:	As an example of this, it would be quite possible to define a special handler which meant that, for instance `#/this is ~U+1234+ an arbitrary Unicode character/`would work.

[^15]:	It's quite possible that `with-accessors` will work for completely arbitrary objects and accessors already of course, but I don't think you can portably rely on this.

[^16]:	This link is to my own copy.

[^17]:	Of course these macros are still not hygenic.

[^18]:	I am reasonably sure that fully hygienic macros are not possible in CL without extensions to the language or access to the guts of the implementation.

[^19]:	So, in particular `foo:<x>` and `bar:<x>` will be rewritten as distinct gensymized versions of themselves.

[^20]:	So `(apply #'eq (metatronize '(<x> <x>)))` is true but `(apply #'eq (metatronize '(<> <>)))` is false.

[^21]:	This is in fact how `iterate` and `iterate*` work now, with `iterating` and `iterating*` sharing another expansion function.

[^22]:	An interim version of `slog` had a generic function, `log-entry-formatter` which was involved in this process with the aim of being able to select formats more flexibly, but it did not in fact add any useful flexibility.

[^23]:	Well: you could write your own `handler-bind` / `handler-case` forms, but don't do that.

[^24]:	All of this applies to `proclaim` and `declaim` as well.

[^25]:	Some implementations allow non-atomic type specifiers: this is not strictly conforming but supported by this code.

[^26]:	SBCL has its own version of this function, so that's used for SBCL.