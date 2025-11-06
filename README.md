# Coeffect-Sandbox

It is often the case that the programs we write expect some kind of *resource* to be available. This could be some hardware device such as a GPS, a sensor, or a clock, or it could be something more advanced like a variable with some sensitivity/security value indicating when it is allowed to be used.

While effect type systems have been in circulation for quite some time now (these track what a program *does* to its environment), [coeffects](https://tomasp.net/coeffects/) are a recent innovation which track what a program *needs* from an environment. They are in essence the *dual* of effects. 

This repository provide sample implementations (in OCaml) of programming languages utilizing coeffect type systems for some interesting use-cases.