newton
======

Project developed to assist in simplification and searching for approximate solution of power equations systems.

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


Building and usage
------------------

Java binary can be assembled via SBT by running `sbt assembly` command (will create a jar-file in `_build` subfolder).
GUI can then be launched via `java -jar newton-<VER>.jar`.
Main class is `org.newtonpolyhedron.NewtonEntry`.

In order for visualization to work, appropriate `j3dcore-ogl` from Java3D v1.5.1 needs to be manuallly put into classpath.

(Detailed usage info is available in Russian)


Authors
-------

* Alexander Abdugafarov (https://github.com/frozenspider/) - programming
* Elvira Settarova - math
* Alexander Orkin - math
* Ahmadjon Soleev - supervision, math

Developed primarily for Samarkand State University
