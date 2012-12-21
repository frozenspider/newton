package org.newtonpolyhedron.solve.cone;

/*-
 * 1. Create a Canvas3D object
 * 2. Create a VirtualUniverse object
 * 3. Create a Locale object, attaching it to the VirtualUniverse object
 * 4. Construct a view branch graph
 * 		a. Create a View object
 * 		b. Create a ViewPlatform object
 * 		c. Create a PhysicalBody object
 * 		d. Create a PhysicalEnvironment object
 * 		e. Attach ViewPlatform, PhysicalBody, PhysicalEnvironment, and Canvas3D objects to View object
 * 5. Construct content branch graph(s)
 * 6. Compile branch graph(s)
 * 7. Insert subgraphs into the Locale
 */
import static java.math.BigInteger.*;
import static org.newtonpolyhedron.utils.ArithUtils.*;

import java.io.PrintWriter;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;

import org.fs.utils.structure.wrap.Pair;
import org.newtonpolyhedron.entity.vector.AbstractVector;
import org.newtonpolyhedron.entity.vector.IntVector;
import org.newtonpolyhedron.utils.MatrixUtils;

/** @author Alexander "FS" Abdugafarov */
// TODO: Определение многогранника по восходящей по таблице соответствия
public class ConeSolverImpl implements ConeSolver {
	
	@Override
	public List <IntVector> solve(
			final List <IntVector> inequations,
			final List <IntVector> basis,
			final int dim,
			final PrintWriter output) {
		final List <IntVector> fundamentalSolution = coneSolve(inequations, basis, dim, output);
		final int rank = MatrixUtils.getRank(MatrixUtils.fromIntVector(inequations));
		removeZeroProductSolutions(inequations, fundamentalSolution, rank);
		return fundamentalSolution;
	}
	
	private static void removeZeroProductSolutions(
			final List <IntVector> base,
			final List <IntVector> testing,
			final int rank) {
		if (base.isEmpty() || testing.isEmpty()) return;
		for (final Iterator <IntVector> iter = testing.iterator(); iter.hasNext();) {
			final IntVector vec = iter.next();
			int toZero = 0;
			for (final IntVector basePt : base) {
				if (isZero(basePt.dotProduct(vec))) {
					toZero++;
				}
			}
			if (toZero < rank - 1) {
				iter.remove();
			}
		}
	}
	
	private static List <IntVector> coneSolve(
			final List <IntVector> eqSys,
			final List <IntVector> wishfulBasis,
			final int dim,
			final PrintWriter output) {
		final List <IntVector> eqSysMutable = new ArrayList <IntVector>(eqSys);
		{
			final IntVector zeroPoint = IntVector.empty(dim);
			if (!eqSysMutable.get(0).equals(zeroPoint)) {
				eqSysMutable.add(0, zeroPoint);
			}
		}
		List <IntVector> basis = getInitialBasis(dim, wishfulBasis);
		List <IntVector> fundSol = new ArrayList <IntVector>();
		
		for (int i = 1; i < eqSysMutable.size(); i++) {
			final IntVector currEq = eqSysMutable.get(i);
			if (nonZeroValsExists(basis, currEq)) {
				final Pair <List <IntVector>, List <IntVector>> pair = solveEqSystem_withBasis(
						currEq, basis, fundSol);
				basis = pair.getFirst();
				fundSol = pair.getSecond();
			} else {
				final List <IntVector> eqSysPart = eqSysMutable.subList(0, i);
				fundSol = solveEqSystem_withoutBasis(currEq, eqSysPart, fundSol, dim);
			}
			output.println("\n === Step " + i + " ===");
			fundSolAndBasisOutput(basis, fundSol, output);
		}
		
		
		fundSol = wrap(fundSol);
		
		final ListIterator <IntVector> resultIter = fundSol.listIterator();
		while (resultIter.hasNext()) {
			final IntVector currResult = resultIter.next();
			switch (valudateSolution(currResult, eqSysMutable)) {
				case NOT_CONFORMS:
					resultIter.remove();
					break;
				case CONFORMS_NEG:
					resultIter.set(currResult.negate());
					break;
				case CONFORMS_POS:
					// NOOP
					break;
			}
		}
		
		fundSol = wrap(fundSol);
		
		return fundSol;
	}
	
	private static List <IntVector> getInitialBasis(
			final int dim,
			final List <IntVector> wishfulBasis) {
		final List <IntVector> basis = new ArrayList <IntVector>();
		if (wishfulBasis == null || wishfulBasis.isEmpty()) { // TODO: remove isEmpty check?
			for (int currDim = 0; currDim < dim; currDim++) {
				basis.add(IntVector.empty(dim).withValueAt(currDim, ONE));
			}
		} else {
			basis.addAll(wishfulBasis);
		}
		return basis;
	}
	
	private static boolean nonZeroValsExists(final List <IntVector> basis, final IntVector eq) {
		for (final IntVector currBasesVec : basis) { // Calculating l(u[i])
			if (!isZero(eq.dotProduct(currBasesVec))) return true;
		}
		return false;
	}
	
	/**
	 * One iteration of an inequation system solving for ???
	 * 
	 * @param currEq
	 * @param tempBasis
	 *            solution basis, cannot be empty
	 * @param fundSol
	 * @return new basis and fundamental solutions
	 */
	private static Pair <List <IntVector>, List <IntVector>> solveEqSystem_withBasis(
			final IntVector currEq,
			final List <IntVector> basis,
			final List <IntVector> fundSol) {
		if (basis.isEmpty()) throw new RuntimeException("Empty basis");
		
		final List <IntVector> newBasis = new ArrayList <IntVector>();
		final List <IntVector> newFundSols = new ArrayList <IntVector>();
		
		// Pre-calculate values of l(u) AND reverse vector of an old basis
		final Pair <IntVector, List <IntVector>> temp = calculateLAndTempBasis(currEq, basis);
		final IntVector l = temp.getFirst();
		final List <IntVector> tempBasis = temp.getSecond();
		
		final int currActiveIdx = findBasisWorkingElementIndex(l);
		
		final BigInteger lActive = l.get(currActiveIdx);
		final IntVector basisActive = tempBasis.get(currActiveIdx);
		
		// ADD NEW BASIS LINE
		for (int i = 0; i < tempBasis.size(); i++) {
			if (i == currActiveIdx) {
				continue;
			}
			final IntVector basisCurr = tempBasis.get(i);
			final BigInteger lCurr = l.get(i);
			
			// New basis element
			// newBasisVec = basis[i] * l[cai] - basis[cai] * l[i]
			final IntVector newBasisElement = basisCurr.multiply(lActive).subtract(
					basisActive.multiply(lCurr)).getReduced();
			
			newBasis.add(newBasisElement);
		}
		
		
		newFundSols.add(basisActive);
		
		
		// ADD NEW FUNDAMENTAL SOLUTION LINE
		for (final IntVector fundSolCurr : fundSol) {
			// l(v[n]) element from current fundSol
			final BigInteger lVn = currEq.dotProduct(fundSolCurr);
			
			// New fundSol element
			// basis[cai] * lVn - fundSol[i] * l[cai];
			final IntVector newFundSol = basisActive.multiply(lVn).subtract(
					fundSolCurr.multiply(lActive)).getReduced();
			newFundSols.add(newFundSol);
		}
		
		return Pair.make(newBasis, wrap(newFundSols));
	}
	
	/**
	 * Calculating l(u[i]). If necessary, inverses first fitting basis vector.
	 * 
	 * @param currEq
	 *            selected (in)equation
	 * @param basis
	 *            current basis
	 * @return l(u[i]) vector and modified basis to be used with it
	 */
	private static Pair <IntVector, List <IntVector>> calculateLAndTempBasis(
			final IntVector currEq,
			final List <IntVector> basis) {
		final List <BigInteger> lBase = new ArrayList <BigInteger>();
		final List <IntVector> tempBasis = new ArrayList <IntVector>();
		boolean found = false;
		for (final IntVector basisVec : basis) {
			final BigInteger lBaseCurr = currEq.dotProduct(basisVec);
			boolean inverse = false;
			if (!found && !isZero(lBaseCurr)) {
				inverse = greater(lBaseCurr, ZERO);
				found = true;
			}
			tempBasis.add(!inverse ? basisVec : basisVec.negate());
			lBase.add(!inverse ? lBaseCurr : lBaseCurr.negate());
		}
		return Pair.make( //
				new IntVector(lBase), // Non-reduced
				tempBasis //
		);
	}
	
	/**
	 * Returns an index of a working element index of l(u[i]) - the first non-zero.
	 * 
	 * @param l
	 *            l(u[i]) vector
	 * @return working element index, or -1 if vector is zero-sized.
	 */
	private static int findBasisWorkingElementIndex(final IntVector l) {
		for (int i = 0; i < l.getDim(); i++) {
			if (!isZero(l.get(i))) return i;
		}
		return -1;
	}
	
	/**
	 * Solves an equation system if basis is exhausted.
	 * 
	 * @param currEq
	 *            current equation
	 * @param eqSysPart
	 *            part of equation system, from first to current equation exclusive (btw, why part?)
	 * @param fundSol
	 *            fundamental solution
	 * @param dim
	 *            space dimension
	 * @return new fundamental solution
	 */
	private static List <IntVector> solveEqSystem_withoutBasis(
			final IntVector currEq,
			final List <IntVector> eqSysPart,
			final List <IntVector> fundSol,
			final int dim) {
		final List <IntVector> fundSolZero = new ArrayList <IntVector>();
		final List <IntVector> fundSolMinus = new ArrayList <IntVector>();
		final List <IntVector> fundSol_Plus = new ArrayList <IntVector>();
		final List <IntVector> fundSolCombined = new ArrayList <IntVector>();
		
		for (final IntVector currFundSol : fundSol) {
			final BigInteger dotProd = currEq.dotProduct(currFundSol);
			
			if (isZero(dotProd)) {
				fundSolZero.add(currFundSol);
			} else if (less(dotProd, ZERO)) {
				fundSolMinus.add(currFundSol);
			} else {
				fundSol_Plus.add(currFundSol);
			}
		}
		
		for (final IntVector cNegative : fundSolMinus) {
			for (final IntVector cPositive : fundSol_Plus) {
				final boolean shouldCombine;
				if (dim == 2 || fundSol.size() == 2) {
					shouldCombine = true;
				} else {
					final List <IntVector> zeroEqs = new ArrayList <IntVector>();
					final List <Integer> amtsOfZeros = new ArrayList <Integer>();
					
					for (final IntVector selEq : eqSysPart) {
						if (areZeros(selEq.dotProduct(cPositive), selEq.dotProduct(cNegative))) {
							zeroEqs.add(selEq);
						}
					}
					zeroEqs.remove(0); // Skip leading [0,0,0] vector (is it necessary?)
					
					for (final IntVector currFundSol : fundSol) {
						int zeros = 0;
						for (final IntVector zeroEq : zeroEqs) {
							if (true //
									&& !currFundSol.equals(cPositive)
									&& !currFundSol.equals(cNegative)
									&& isZero(zeroEq.dotProduct(currFundSol))) {
								zeros++;
							}
						}
						amtsOfZeros.add(zeros);
					}
					shouldCombine = !amtsOfZeros.contains(zeroEqs.size());
				}
				if (shouldCombine) {
					final BigInteger lNegative = currEq.dotProduct(cNegative);
					final BigInteger lPositive = currEq.dotProduct(cPositive);
					
					final IntVector combinedSol = cNegative.multiply(lPositive).subtract(
							cPositive.multiply(lNegative)).getReduced();
					
					final ValidationResult validation = valudateSolution(combinedSol, eqSysPart);
					if (validation == ValidationResult.CONFORMS_POS) {
						fundSolCombined.add(combinedSol);
					} else if (validation == ValidationResult.CONFORMS_NEG) {
						fundSolCombined.add(combinedSol.negate());
					} else
						// Impossible
						throw new RuntimeException("Somehow, non-conforming +/- combination");
				}
			}
		}
		final List <IntVector> newFundSol = new ArrayList <IntVector>();
		newFundSol.addAll(fundSolZero);
		newFundSol.addAll(fundSolMinus);
		newFundSol.addAll(fundSolCombined);
		return wrap(newFundSol);
	}
	
	private static ValidationResult valudateSolution(
			final IntVector cVec,
			final List <IntVector> eqSysPart) {
		
		// We don't need to skip leading [0,0,0] vector
		
		boolean conform = true;
		for (final IntVector eq : eqSysPart) {
			if (greater(eq.dotProduct(cVec), ZERO)) {
				conform = false;
				break;
			}
		}
		if (conform) return ValidationResult.CONFORMS_POS;
		
		//
		// Negative check
		//
		conform = true;
		final IntVector cVecNeg = cVec.negate();
		for (final IntVector eq : eqSysPart) {
			if (greater(eq.dotProduct(cVecNeg), ZERO)) {
				conform = false;
				break;
			}
		}
		if (conform) return ValidationResult.CONFORMS_NEG;
		
		return ValidationResult.NOT_CONFORMS;
	}
	
	/**
	 * Removes duplicates and zero vectors. Doesn't cause side effects.
	 * 
	 * @param vecList
	 *            input
	 * @return refined input
	 */
	protected static List <IntVector> wrap(final List <IntVector> vecList) {
		if (vecList.isEmpty()) return vecList;
		final int dim = vecList.get(0).getDim();
		
		// Removing duplicates
		final Set <IntVector> vectorSet = new LinkedHashSet <IntVector>(vecList);
		
		// Removing zero vectors
		vectorSet.remove(IntVector.empty(dim));
		
		return new ArrayList <IntVector>(vectorSet);
	}
	
	//
	//
	//
	private static void fundSolAndBasisOutput(
			final List <IntVector> basis,
			final List <IntVector> fundSol,
			final PrintWriter output) {
		output.println("=== Basis: ===");
		if (basis.size() > 0) {
			for (final IntVector currBasis : basis) {
				output.println(currBasis);
			}
		} else {
			output.println(" (none)");
		}
		output.println("=== Fundamental Solution: ===");
		for (final IntVector currSol : fundSol) {
			output.println(currSol);
		}
	}
	
	private static enum ValidationResult {
		CONFORMS_POS, CONFORMS_NEG, NOT_CONFORMS
	}
}