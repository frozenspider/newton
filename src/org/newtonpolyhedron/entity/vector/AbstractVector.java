package org.newtonpolyhedron.entity.vector;

import java.util.Arrays;

public abstract class AbstractVector <T extends Comparable <T>,SELF extends AbstractVector <T, SELF>> implements
		Comparable <SELF> {
	
	protected final T	components[];
	
	public AbstractVector(final T[] components) {
		this.components = components;
	}
	
	public int getDim() {
		return this.components.length;
	}
	
	//
	// Arithmetic
	//
	public final SELF negate() {
		final T[] copy = createArrayOfLength(components.length);
		for (int i = 0; i < copy.length; i++) {
			copy[i] = negate(components[i]);
		}
		return create(copy);
	}
	
	public final SELF add(final SELF otherVector) {
		if (this.components.length != otherVector.components.length)
			throw new IllegalArgumentException(String.format("This vector has size %d, other - %d",
					this.components.length, otherVector.components.length));
		final T[] copy = createArrayOfLength(components.length);
		for (int i = 0; i < copy.length; i++) {
			copy[i] = add(components[i], otherVector.components[i]);
		}
		return create(copy);
	}
	
	public final SELF subtract(final SELF otherVector) {
		if (this.components.length != otherVector.components.length)
			throw new IllegalArgumentException(String.format("This vector has size %d, other - %d",
					this.components.length, otherVector.components.length));
		final T[] copy = createArrayOfLength(components.length);
		for (int i = 0; i < copy.length; i++) {
			copy[i] = subtract(components[i], otherVector.components[i]);
		}
		return create(copy);
	}
	
	public final SELF multiply(final T coeff) {
		final T[] copy = createArrayOfLength(components.length);
		for (int i = 0; i < copy.length; i++) {
			copy[i] = multiply(components[i], coeff);
		}
		return create(copy);
	}
	
	public final SELF multiply(final SELF otherVector) {
		if (this.components.length != otherVector.components.length)
			throw new IllegalArgumentException(String.format("This vector has size %d, other - %d",
					this.components.length, otherVector.components.length));
		final T[] copy = createArrayOfLength(components.length);
		for (int i = 0; i < copy.length; i++) {
			copy[i] = multiply(components[i], otherVector.components[i]);
		}
		return create(copy);
	}
	
	public final SELF divide(final SELF otherVector) {
		if (this.components.length != otherVector.components.length)
			throw new IllegalArgumentException(String.format("This vector has size %d, other - %d",
					this.components.length, otherVector.components.length));
		final T[] copy = createArrayOfLength(components.length);
		divide(otherVector.components, copy);
		return create(copy);
	}
	
	/**
	 * Computes the dot product of two vectors (i.e.
	 * <code>a<sub>0</sub>b<sub>0</sub> + <code>a<sub>1</sub>b<sub>1</sub> + ...</code>)
	 * 
	 * @param otherVector
	 * @return dot product value as big integer
	 */
	public final T dotProduct(final SELF otherVector) {
		if (this.components.length != otherVector.components.length)
			throw new IllegalArgumentException(String.format("This vector has size %d, other - %d",
					this.components.length, otherVector.components.length));
		final T[] target = createArrayOfLength(components.length);
		for (int i = 0; i < target.length; i++) {
			target[i] = multiply(components[i], otherVector.components[i]);
		}
		T result = getZeroNumber();
		for (final T comp : target) {
			result = add(result, comp);
		}
		return result;
	}
	
	//
	// Abstract Arithmetic
	//
	protected abstract T negate(T value);
	
	protected abstract T add(T one, T two);
	
	protected abstract T subtract(T one, T two);
	
	protected abstract T multiply(T one, T two);
	
	protected abstract void divide(T[] divisor, T[] copyResult);
	
	protected abstract T getZeroNumber();
	
	//
	// Get/Set
	//
	public T[] getContentCopy() {
		final T[] copy = createArrayOfLength(components.length);
		System.arraycopy(components, 0, copy, 0, components.length);
		return copy;
	}
	
	public T get(final int idx) {
		return this.components[idx];
	}
	
	public SELF withValueAt(final int idx, final T value) {
		final T[] copy = Arrays.copyOf(this.components, this.components.length);
		copy[idx] = value;
		return create(copy);
	}
	
	//
	// Utility
	//
	protected abstract SELF create(T[] data);
	
	protected abstract T[] createArrayOfLength(int len);
	
	//
	// Misc
	//
	@Override
	public String toString() {
		final StringBuilder t = new StringBuilder("[ ");
		for (final T component : components) {
			t.append(component);
			t.append(" ");
		}
		t.append("]");
		return t.toString();
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + Arrays.hashCode(components);
		return result;
	}
	
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) return true;
		if (obj == null) return false;
		if (getClass() != obj.getClass()) return false;
		final AbstractVector <?, ?> other = (AbstractVector <?, ?>) obj;
		if (!Arrays.equals(components, other.components)) return false;
		return true;
	}
	
	@Override
	public int compareTo(final SELF o) {
		if (components.length < o.components.length) return 1;
		if (components.length > o.components.length) return -1;
		for (int i = 0; i < components.length; ++i) {
			final int cmp = components[i].compareTo(o.components[i]);
			if (cmp != 0) return cmp;
		}
		return 0;
	}
	
	
}
