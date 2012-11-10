package org.newtonpolyhedron.utils;

import java.util.ArrayList;
import java.util.List;

import javax.vecmath.Point3d;

import org.newtonpolyhedron.entity.vector.FractionVector;
import org.newtonpolyhedron.entity.vector.IntVector;

public class PointUtils {
	
	public static Point3d toPoint3d(final FractionVector p) {
		return new Point3d(p.get(0).doubleValue(), p.getDim() > 1 ? p.get(1).doubleValue() : 0,
				p.getDim() > 2 ? p.get(2).doubleValue() : 0);
	}
	
	/**
	 * Creates a vector list, each element of which is produced by subtracting point with given
	 * index from the rest (and excluding it).
	 * 
	 * @param points
	 *            source list.
	 * @param indexToSubtract
	 *            index of point to subtract and exclude.
	 * @return vector list of size {@code n-1}, with {@code i}'th point subtracted from all other.
	 */
	public static List <IntVector> copySubtractPointAsInt(
			final List <FractionVector> points,
			final int indexToSubtract) {
		final List <IntVector> result = new ArrayList <IntVector>(points.size() - 1);
		final FractionVector currPoint = points.get(indexToSubtract);
		for (int i = 0; i < points.size(); i++) {
			final FractionVector tempPoint = points.get(i);
			if (i == indexToSubtract) {
				continue;
			}
			result.add(IntVector.fromFractions(tempPoint.subtract(currPoint)));
		}
		return result;
	}
}
