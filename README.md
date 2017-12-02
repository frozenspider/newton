newton
======

Project developed to assist with simplification and search for approximate solution of polynomial equation systems.

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

Java binary can be assembled via SBT by running `sbt assembly` command (will create a jar-file in `_build` subfolder).
GUI can then be launched via `java -jar newton-<VER>.jar`.
Main class is `org.newtonpolyhedron.NewtonEntry`.

In order for visualization to work, appropriate `j3dcore-ogl` from Java3D v1.5.1 needs to be manuallly put into classpath.


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
