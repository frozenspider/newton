package org.newtonpolyhedron.entity;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.fs.utils.ObjectUtils;


public class Surface implements Comparable <Surface> {
	
	/** Indices of a points, forming the border. Never <code>null</code>. */
	private final List <Integer>	pointIdxList;
	/**
	 * Indices of a upper dimension surfaces, for which this border is common. Never
	 * <code>null</code>.
	 */
	private final List <Integer>	upperDimSurfacesIdxList;
	
	/**
	 * Note: copies the collections content, all arguments can be <code>null</code>
	 * 
	 * @param pointIdxList
	 *            surface forming points
	 * @param upperDimSurfacesIdxList
	 *            upper dimension surfaces containing this one
	 */
	public Surface(
			final Collection <Integer> pointIdxList,
			final Collection <Integer> upperDimSurfacesIdxList) {
		this.pointIdxList = pointIdxList == null ? new ArrayList <Integer>(1)
				: new ArrayList <Integer>(pointIdxList);
		Collections.sort(this.pointIdxList);
		this.upperDimSurfacesIdxList = upperDimSurfacesIdxList == null ? new ArrayList <Integer>(1)
				: new ArrayList <Integer>(upperDimSurfacesIdxList);
	}
	
	public int size() {
		return pointIdxList.size();
	}
	
	public void addUpperDimSurfaceIndices(final List <Integer> upperDimSurfacesIdxList) {
		for (final Integer bordersIdx : upperDimSurfacesIdxList) {
			if (!this.upperDimSurfacesIdxList.contains(bordersIdx)) {
				this.upperDimSurfacesIdxList.add(bordersIdx);
			}
		}
		Collections.sort(this.upperDimSurfacesIdxList);
	}
	
	public List <Integer> getPointIdxList() {
		return pointIdxList;
	}
	
	public List <Integer> getUpperDimSurfacesIdxList() {
		return upperDimSurfacesIdxList;
	}
	
	@Override
	public int hashCode() {
		return getPointIdxList().hashCode() * 17;
	}
	
	@Override
	public boolean equals(final Object obj) {
		if (!(obj instanceof Surface)) return false;
		final Surface cast = (Surface) obj;
		return getPointIdxList().equals(cast.getPointIdxList());
	}
	
	public boolean deepEquals(final Object obj) {
		if (!(obj instanceof Surface)) return false;
		final Surface cast = (Surface) obj;
		return getPointIdxList().equals(cast.getPointIdxList())//
				&& getUpperDimSurfacesIdxList().equals(cast.getUpperDimSurfacesIdxList());
	}
	
	@Override
	public String toString() {
		final StringBuilder result = new StringBuilder();
		result.append('{');
		for (final Integer pointIdx : pointIdxList) {
			result.append(' ');
			result.append(pointIdx);
			result.append(',');
		}
		result.append(" }");
		if (!upperDimSurfacesIdxList.isEmpty()) {
			result.append("  / ");
			for (final Integer borderIdx : upperDimSurfacesIdxList) {
				result.append(' ');
				result.append(borderIdx);
				result.append(',');
			}
		}
		return result.toString();
	}
	
	@Override
	public int compareTo(Surface o) {
		int minSz = Math.min(pointIdxList.size(), o.pointIdxList.size());
		for (int i = 0; i < minSz; ++i) {
			int cmp = ObjectUtils.compare(pointIdxList.get(i), o.pointIdxList.get(i));
			if (cmp != 0) return cmp;
			
		}
		return ObjectUtils.compare(pointIdxList.size(), o.pointIdxList.size());
	}
}
