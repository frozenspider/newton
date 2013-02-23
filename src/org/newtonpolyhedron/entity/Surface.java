package org.newtonpolyhedron.entity;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.fs.utils.ObjectUtils;

public class Surface implements Comparable <Surface> {
	
	/** Indices of a points, forming the border. Never <code>null</code>. */
	private final List <Integer>	pointIdxList;
	/** Upper dimension surfaces, for which this border is common. Never <code>null</code>. */
	private final List <Surface>	upperDimSurfaces;
	
	/**
	 * Note: copies the collections content, all arguments can be <code>null</code>
	 * 
	 * @param pointIdxList
	 *            surface forming points
	 * @param upperDimSurfacesIdxList
	 *            upper dimension surfaces containing this one
	 */
	public Surface(final Collection <Integer> pointIdxList, final Collection <Surface> upperDimSurfacesIdxList) {
		this.pointIdxList = pointIdxList == null ? new ArrayList <Integer>(1) : new ArrayList <Integer>(pointIdxList);
		Collections.sort(this.pointIdxList);
		this.upperDimSurfaces = upperDimSurfacesIdxList == null ? new ArrayList <Surface>(1) : new ArrayList <Surface>(
				upperDimSurfacesIdxList);
	}
	
	/** @return number of points in surface */
	public int size() {
		return pointIdxList.size();
	}
	
	public void addUpperDimSurfaces(final List <Surface> upperDimSurfaces) {
		for (final Surface surface : upperDimSurfaces) {
			if (!this.upperDimSurfaces.contains(surface)) {
				this.upperDimSurfaces.add(surface);
			}
		}
		Collections.sort(this.upperDimSurfaces);
	}
	
	public List <Integer> getPointIdxList() {
		return Collections.unmodifiableList(pointIdxList);
	}
	
	public List <Surface> getUpperDimSurfacesList() {
		return Collections.unmodifiableList(upperDimSurfaces);
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
	
	@Override
	public String toString() {
		final StringBuilder result = new StringBuilder();
		result.append('{');
		result.append(StringUtils.join(pointIdxList, ", "));
		result.append("}");
		return result.toString();
	}
	
	public String makeString(List <Surface> allUpperDimSurfaces) {
		final StringBuilder result = new StringBuilder();
		result.append('{');
		result.append(StringUtils.join(pointIdxList, ", "));
		result.append("}");
		if (!upperDimSurfaces.isEmpty()) {
			result.append(" / ");
			List <Integer> upperDimSurfacesIdxList = new ArrayList <Integer>();
			for (Surface surface : upperDimSurfaces) {
				int idx = allUpperDimSurfaces.indexOf(surface);
				if (idx < 0) {
					throw new RuntimeException("Upper dimension surface is missing"
							+ " from its all upper dimension surfaces list");
				}
				upperDimSurfacesIdxList.add(idx);
			}
			result.append(StringUtils.join(upperDimSurfacesIdxList, ", "));
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
