# [TFEB.ORG Lisp hax](https://github.com/tfeb/tfeb-lisp-hax "TFEB.org Lisp hax")
This repo contains a collection of small Common Lisp hacks I've written over the last thirty-odd years[^1].  Some of them are genuinely useful, some of them are little more than toys written long ago to prove a point on `comp.lang.lisp`.  Although they are here bundled together into an ASDF system that's only for convenience: most of them are independent of each other.  I will probably add more over time.

## General
### Modules
Almost all of these hacks are independent modules: they live in their own little packages and are loadable standalone: if you just want one of them then you don't need to drag all the unrelated ones into your environment.  If you put them in the right place, then [`require-module`](https://github.com/tfeb/tfeb-lisp-tools#requiring-modules-with-searching-require-module "require-module") will find and load them for you: this is how I use them.  Exceptions are:

- `binding`, which depends on `collecting` and `iterate`, and will try to use `require-module` to load them if they're not already known when it's compiled or loaded.
- `stringtable`, which depends on  `collecting` and `iterate`, and will attempt the same trick.

The system itself provides `:org.tfeb.hax`: there is no `org.tfeb.hax` package however: each component lives in its own package with names like `org.tfeb.hax.*`.

### Portability
These tools purport to be portable Common Lisp, apart from a couple which depend on [Closer to MOP](https://github.com/pcostanza/closer-mop "Closer to MOP") on some platforms[^2].  If they're not that's either a bug in the tools or a bug in the CL implementation.  In the former case I very definitely want to know, and I am also willing to add workarounds for the latter although I may be unable to test them on implementations I don't use.

### Zero history
Most of these tools have long and varied histories.  However the parts of these histories that are still preserved are entangled with a lot of other code which is not public, so that history is not represented in the publication repo where you are probably reading this.

### Naming conventions
All of these tools make use of *domain-structured names*: packages, modules, features and so on have names which start with a DNS domain name in reverse order and then may continue to divide further.  In this case the prefix is `org.tfeb.hax`: `org.tfeb` is the DNS component and `hax` is the division within the DNS part.  See [the TFEB.ORG tools documentation](https://github.com/tfeb/tfeb-lisp-tools#naming-conventions "TFEB.ORG tools / Naming conventions") for a little more on this.

----

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

`collecting` is older than `with-collectors` by more than a decade I think.  However it has an obvious definition as a shim on top of `with-collectors` and, finally, that now *is* its definition.

### General accumulators: `with-accumulators`
**`with-accumulators`** is a variation on the theme of `with-collectors`: it allows you to accumulate things based on any function and a secret accumulator variable.  `with-accumulators` takes a number of accumulator specifications as its first argument.   These can have either a simple form and a more general form which may be extended in future.

The simple form is `(accumulator operatior &optional initially)`.

- `accumulator` is the name of the local function for this accumulator, which takes one argument, the thing to accumulate.
- `operator` is the operator corresponding to the accumulator: this denotes a function which can take either two or no arguments: it is called with no arguments to initialise the variable underlying the accumulator if there is no `initially` value (this is the only case it is called with no arguments), or with the current value of the accumulator and the thing to be accumulated when the local function is called to accumulate something.
- `initially`, if given, is the initial value.  If it is not given the accumulator is initialised by calling the operator function with no arguments.

The more general form is `(accumulator operator &key initially type returner)`.

- `accumulator`, `operator` and `initially` are the same as before.
- `type` is a type specification which is used to declare the type of the underlying variable.
- `returner` denotes a function of one argument which, if given, is called with the final value of the accumulator: its return value is used instead of the value of the accumulator.
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

A more general function can count symbols and conses:

```lisp
(defun count-symbols-and-conses (tree)
  (with-accumulators ((s +)
                      (c +))
    (labels ((walk (thing)
               (typecase thing
                 (null)
                 (symbol (s 1))
                 (cons (c 1)
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

### Package, module
`collecting` lives in `org.tfeb.hax.collecting` and provides `:org.tfeb.hax.collecting`.

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
I've always liked Scheme's named-`let` construct.  It's pretty easy to provide a shim around `labels` in CL which is syntactically the same, but since CL doesn't promise to turn tail calls into jumps, it may cause stack overflows.  When I wrote `iterate` I was still using, part of the time, a Symbolics LispM, and they *didn't* turn tail calls into jumps.  So I wrote this little hack which, if it knows that the implementation does not handle tail-call elimination, and if the name of the local function contains `loop` (in any case) will compile 'calls' to it as explicit jumps.  Otherwise it turns them into the obvious `labels` construct.
  
Well, that's what it used to do: the flag which controls whether it thinks an implementation supports tail-call elimination is now always set to true, which means it will always create correct code, even if that code may cause stack overflows on implementations which don't eliminate tail calls[^5].  The old code is still there in case anyone wants to look at it.

There is a single macro: **`iterate`**[^6]:

```lisp
(iterate foo ((x 1)
              (y 2))
  ...
  (foo (+ x 1) (- y 1))
  ...)
```

turns into

```lisp
(labels ((foo (x y)
           ...
           (foo (+ x 1) (- y 1))
           ...))
  (foo 1 2))
```

Combined with `collecting`, `iterate` provides a surprisingly pleasant minimalist framework for walking over data structures in my experience.

### Package, module
`iterate` lives in `org.tfeb.hax.iterate` and provides `:org.tfeb.hax.iterate`.

## Local dynamic state: `dynamic-state`
Dynamic binding is something you don't want very often, but you always end up wanting it somewhere: when programming in languages such as Python I've ended up having to reinvent dynamic binding[^7].

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
Memoization is a clever trick invented by Donald Michie[^8], and described in [Wikipedia](https://en.wikipedia.org/wiki/Memoization "Memoization").  By remembering the results of calls to the function, it can hugely increase performance of certain kinds of recursive function.  As an example

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
`abstract-classes` needs a tiny bit of the MOP.  For most platforms it uses [Closer to MOP](https://github.com/pcostanza/closer-mop "Closer to MOP") to avoid having to have implementation-dependent code.  However for platforms where `closer-mop:standard-class` is not `cl:standard-class`, `defclass` will, by default, create classes whose metaclass is `cl:standard-class`, while the `validate-superclass` methods will refer to `closer-mop:standard-class`[^9]  In the implementations I use where that is true I've relied on the implementation's MOP.  Currently this means LispWorks, although there may be others.

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

### Reading forms in a package: `read-package`
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

Tracing output goes to `*trace-output*`.

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

**`*trace-macroexpand-print-length*`** and **`*trace-macroexpand-print-level*`** are the values of `*print-length*` and `*print-level*` in effect during tracing.  By default they are `3` and `2` respectively.

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

Is problematic because the function definition will not generally be known about at compile-time.  It's also ugly, compared with the equivalent in Scheme[^10]:

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
`format` has a very useful feature: there is a special format control 'tilde newline' which will cause `format` to skip both the newline and any following whitespace characters[^11].  This makes writing long format control strings much easier, which is useful since format control strings do tend to be long.  You can, then, use `(format nil ...)` as a way of simply creating a string with, if you want, newlines being ignored.

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
	- the handler function should return: a character which is accumulated; a list of characters which are accumulated (this list may be empty); or a string, the characters of which are accumulate.  Any other return value is an error.

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

I've talked about things 'being an error' above: in fact in most (I hope all) cases suitable conditions are signaled

Stringtables are intended to provide a way of reading literal strings with some slightly convenient syntax[^12]: it is *not* a system for, for instance, doing some syntactically-nicer or more extensible version of what `format` does.  There are other things which do that, I'm sure.

Originally the default delimiter for `make-stringtable-readtable` was `#\"`, as it is now .  For a while it was `#\/`, because I worried that `#"..."` would be likely to clash with other hacks,  but  `#/.../` finally seemed too obvious a syntax fir regular expressions to use for this.  You can always choose what you want to have.

### Package, module, dependencies
`stringtable` lives in `org.tfeb.hax.stringtable` and provides `:org.tfeb.hax.stringtable`.  `stringtable` depends on `collecting` and `iterate` at compile and run time.  If you load it as a module then, if you have [`require-module`](https://github.com/tfeb/tfeb-lisp-tools#requiring-modules-with-searching-require-module "require-module"), it will use that to try and load them if they're not there.  If it can't do that and they're not there you'll get a compile-time error.

----

The TFEB.ORG Lisp hax are copyright 1989-2021 Tim Bradshaw.  See `LICENSE` for the license.

----

[^1]:	The initial documentation for these hacks was finished on 20210120 at 18:26 UTC: one hour and twenty-six minutes after Joe Biden became president of the United States.

[^2]:	  I expect both `abstract-classes` and `singleton-classes` might have portability problems around the MOP, and I'd welcome fixes to these.

[^3]:	The modern form of `loop` also did not portably exist when `collecting` was written.

[^4]:	Once upon a time they were local macros, because I didn't trust ancient CL compilers to inline functions, with good reason I think.

[^5]:	If you are using such an implementation, well, sorry.

[^6]:	`iterate` was once called `taglet` and given that it's not particularly about iteration that might be a better name for it: I'm not going to change it back now though.

[^7]:	 Fortunately, and a bit surprisingly to me, Python has facilities which let you do this fairly pleasantly.  Something on my todo list is to make this implementation public.

[^8]:	See [* 'Memo' Functions and Machine Learning*](https://doi.org/10.1038%2F218019a0 "'Memo' Functions and Machine Learning"), Donald Michie, Nature 218 (5136): 19–22.  [PDF copy](https://www.cs.utexas.edu/users/hunt/research/hash-cons/hash-cons-papers/michie-memo-nature-1968.pdf "'Memo' Functions and Machine Learning").

[^9]:	And I was not willing to put in explicit extra methods for `validate-superclass` for `cl:standard-class` since the whole purpose of using Closer to MOP was to avoid that kind of nausea.

[^10]:	In fact, Racket.

[^11]:	Or, optionally, not to skip the newline but to skip any whitespace following it.

[^12]:	As an example of this, it would be quite possible to define a special handler which meant that, for instance `#/this is ~U+1234+ an arbitrary Unicode character/`would work.