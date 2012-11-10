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
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;

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
			final PrintWriter output) throws InterruptedException {
		final List <IntVector> fundamentalSolution = coneSolve(new ArrayList <IntVector>(
				inequations), basis, dim, output);
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
			final List <IntVector> eqSysMutable,
			final List <IntVector> wishfulBasis,
			final int dim,
			final PrintWriter output) throws InterruptedException {
		{
			final IntVector zeroPoint = IntVector.empty(dim);
			if (!eqSysMutable.get(0).equals(zeroPoint)) {
				eqSysMutable.add(0, zeroPoint);
			}
		}
		final List <IntVector> basis = getInitialBasis(dim, wishfulBasis);
		final List <IntVector> fundSol = new ArrayList <IntVector>();
		
		for (int i = 1; i < eqSysMutable.size(); i++) {
			final IntVector currEq = eqSysMutable.get(i);
			if (nonZeroValsExists(basis, currEq)) {
				solveEqSystem_withBasis(currEq, basis, fundSol);
			} else {
				final List <IntVector> eqSysPart = eqSysMutable.subList(0, i);
				solveEqSystem_withoutBasis(currEq, eqSysPart, fundSol, dim);
			}
			output.println("\n === Step " + i + " ===");
			fundSolAndBasisOutput(basis, fundSol, output);
			if (Thread.interrupted())
				throw new InterruptedException(MessageFormat.format(
						"coneSolve - eqs cycle. i = {0}/{1}", i, eqSysMutable.size()));
		}
		
		
		wrap(fundSol);
		
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
		
		wrap(fundSol);
		
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
	 * @param basis
	 *            solution basis, cannot be empty
	 * @param fundSol
	 */
	private static void solveEqSystem_withBasis(
			final IntVector currEq,
			final List <IntVector> basis,
			final List <IntVector> fundSol) {
		if (basis.isEmpty()) throw new RuntimeException("Empty basis");
		
		final List <IntVector> newBasis = new ArrayList <IntVector>();
		final List <IntVector> newFundSols = new ArrayList <IntVector>();
		
		// Pre-calculate values of l(u) AND reverse vector of an old basis
		final IntVector l = calculateLAndReverseBasisIfNeeded(currEq, basis);
		final int currActiveIdx = findBasisWorkingElementIndex(l);
		
		final BigInteger lActive = l.get(currActiveIdx);
		final IntVector basisActive = basis.get(currActiveIdx);
		
		// ADD NEW BASIS LINE
		for (int i = 0; i < basis.size(); i++) {
			if (i == currActiveIdx) {
				continue;
			}
			final IntVector basisCurr = basis.get(i);
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
		
		wrap(newFundSols);
		
		basis.clear();
		basis.addAll(newBasis);
		fundSol.clear();
		fundSol.addAll(newFundSols);
	}
	
	/**
	 * Calculating l(u[i]). If necessary, replaces first fitting basis vector with negated one.
	 * 
	 * @param currEq
	 *            selected (in)equation
	 * @param basis
	 *            current basis
	 * @return l(u[i]) vector
	 */
	private static IntVector calculateLAndReverseBasisIfNeeded(
			final IntVector currEq,
			final List <IntVector> basis) {
		final List <BigInteger> lBase = new ArrayList <BigInteger>();
		boolean found = false;
		for (final ListIterator <IntVector> basisIter = basis.listIterator(); basisIter.hasNext();) {
			final IntVector basisVec = basisIter.next();
			BigInteger lBaseCurr = currEq.dotProduct(basisVec);
			if (!found && !isZero(lBaseCurr)) {
				if (greater(lBaseCurr, ZERO)) {
					lBaseCurr = lBaseCurr.negate();
					basisIter.set(basisVec.negate());
				}
				found = true;
			}
			lBase.add(lBaseCurr);
		}
		return new IntVector(lBase); // Non-reduced
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
	
	private static void solveEqSystem_withoutBasis(
			final IntVector currEq,
			final List <IntVector> eqSysPart,
			final List <IntVector> fundSol,
			final int dim) {
		final List <IntVector> fundSol_Zero = new ArrayList <IntVector>();
		final List <IntVector> fundSol_Minus = new ArrayList <IntVector>();
		final List <IntVector> fundSol_Plus = new ArrayList <IntVector>();
		final List <IntVector> fundSol_Combined = new ArrayList <IntVector>();
		
		for (final IntVector currFundSol : fundSol) {
			final BigInteger dotProd = currEq.dotProduct(currFundSol);
			
			if (isZero(dotProd)) {
				fundSol_Zero.add(currFundSol);
			} else if (less(dotProd, ZERO)) {
				fundSol_Minus.add(currFundSol);
			} else {
				fundSol_Plus.add(currFundSol);
			}
		}
		
		for (final IntVector cNegative : fundSol_Minus) {
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
						fundSol_Combined.add(combinedSol);
					} else if (validation == ValidationResult.CONFORMS_NEG) {
						fundSol_Combined.add(combinedSol.negate());
					} else
						// Impossible
						throw new RuntimeException("Somehow, non-conforming +/- combination");
				}
			}
		}
		fundSol.clear();
		fundSol.addAll(fundSol_Zero);
		fundSol.addAll(fundSol_Minus);
		fundSol.addAll(fundSol_Combined);
		wrap(fundSol);
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
	 * Removes duplicates and zero vectors.
	 * 
	 * @param vecList
	 */
	protected static void wrap(final List <IntVector> vecList) {
		if (vecList.isEmpty()) return;
		final int dim = vecList.get(0).getDim();
		
		// Removing duplicates
		final Set <IntVector> vectorSet = new LinkedHashSet <IntVector>(vecList);
		
		// Removing zero vectors
		vectorSet.remove(IntVector.empty(dim));
		
		// Changing initial collection
		vecList.clear();
		vecList.addAll(vectorSet);
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