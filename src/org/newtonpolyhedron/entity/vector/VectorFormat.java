package org.newtonpolyhedron.entity.vector;


public interface VectorFormat<Component extends Comparable <Component>,Vector extends AbstractVector <Component, Vector>> {
	
	public Component[] createArrayOfZeros(int length);
	
	public Component parseElement(String src);
	
	public Vector makeVector(Component[] components);
}
