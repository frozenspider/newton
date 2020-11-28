newton
======

Project developed to assist with simplification and search for approximate solution of power equation systems.

Currently does the following:

* Computes a fundamental solution of a linear inequations system
  * Uses Motzkin-Burger algorithm
* Computes minimal convex hull (referred as Newton polyhedron, but can work with fractions) for the given polynomial
  * Computes all N-faces and normal cones for highest-dimension faces
  * Generates the lookup table between polynomial terms (polyhedron vertices) and normal vectors
  * Can visualize 2D and 3D polys
* Has utility calculation methods
  * These includes matrces operations like inversion, determinant and minors computation

Also contains unfinished work for the said simplification itself.
Note that theory becomes somewhat vague at this point.


Building
--------

Java binary can be assembled via SBT by running `sbt buildDistr` command (will create a jar-file in `_build` subfolder and copy Java3D libs there if possible).
GUI can then be launched via `java -jar newton-<VER>.jar`.
Main class is `org.newtonpolyhedron.NewtonEntry`.


Usage
-----

See instructions in [English](USAGE.md) or [Russian](USAGE.ru.md).


Authors
-------

* Alexander Abdugafarov (https://github.com/frozenspider/) - programming
* Elvira Settarova - math
* Alexander Orkin - math
* Ahmadjon Soleev - supervision, math

Developed primarily for Samarkand State University
