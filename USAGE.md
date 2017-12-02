* All values are separated by either space or a line break.
* Commas generally aren't used.
* Decimal separator is a dot.
* Fraction are written as m/n
* File content after `@` line is ignored (can be used to leave comments)



Cone mode
---------

Solves a system of linear inequations, with RHS being `<= 0`.
Similar to previous mode, but instead of point coordinates specify variable coefficients.
Common limits are not used (since system of equations is singular)



Polyhedron mode
---------------

Computes a minimal convex hull of given points
and the lookup table (aka table of correspondence) for normal vectors, source points and polyhedron faces.

Input format, sequentially:

* Space dimension
* (Optional) Coefficients for common equations for all systems.
If used, line before and after should be a single `$`
* (Optional) Solution basis.
If used, line before and after should be a single `#`
* Points description

Example 1:
```
2
1 0
0 1
0 0
```
Example 2:
```
3
#
1 0 0
0 1 0
0 0 1
#
1 1 1
4 0 0
0 4 0
0 0 4
2 0 2
```



Polyhedron Cones Intersection mode
----------------------------------

Searches for intersections between normal cones for several polyhedrons.
Polyhedrons should have the same dimension (n), and there shouldn't be less than n-1 of them
(no less than 2 in 3D space, etc.)

Input format, sequentially:

* Space dimension
* Polyhedrons, separated by `%`

Example:
```
3
9 0 0
0 8 0
0 0 7
3 2 1
%
3 0 0
0 4 0
0 0 5
1 2 2
```



Matrix Determinant mode
-----------------------

Computes a determinant of a square matrix or its minor.

Input format, sequentially:

* Square matrix dimension
* Two numbers - 0-based indices of elements, for which minor is computed.
-1 if determinant is computed
* Matrix description

Example:
```
4
-1 -1
1 0 0 0
1 2 0 0
0 0 3 0
0 0 0 4
@
This is a comment. This matrix determinant is 24
```


Matrix Inverse mode
-------------------

Computes an inverse matrix for a given square matrix.

Input format, sequentially:

* Square matrix dimension
* Matrix description

Example:
```
3
 1 0 0
-3 1 0
 0 0 1
@
This is a comment. Inverse matrix is
1 0 0
3 1 0
0 0 1
```



Unimodular "Alpha"-matrix mode
------------------------------

Computes the unimodular matrix ("Alpha") in the following way:

* Rows and columns matrices are created, initialized as identity matrices
* Given matrix is converted to diagonal form via linear transformations.
All rows transformations are mirrored by row matrix, same for columns.
* Rows and columns matrices are inverted and multiplied together

Input format, sequentially:

* Square matrix dimension
* Matrix description.
Last line may be missing, in this case it's treated as zero vector.

Example:
```
3
1 3 4
3 4 2
@
This is a comment. The result is as follows:
1  3  4
3 10 14
0  0  1
```



Power Transformation mode
-------------------------

(NOTE: So far, only 3D case is supported)

Executes one step of power transformation (computation of approximation)
for the selected members of the given polynomial equations

Input format, sequentially:

* k, number of variables in polynomials
* k-1 polynomials, separated by `%` line. Each polynomial is described as a sequence of monomials in form
(coefficient, power_1 power_2 ... power_k), one per line
* `#` line
* Space-separated 0-based indices of monomials selected for approximation for each polynomial.
Number of lines is equal to number of polynomials

Example:
```
3
40,  1 1 1
25,  4 0 0
-25, 0 4 0
-1,  0 0 4
+16, 2 0 2
%
-1, 0 1 1
-1, 2 0 1
-1, 3 1 0
-1, 2 1 1
#
0 1 3 4
0 1
```
