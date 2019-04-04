/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.interrep.topography;

import java.util.Collection;

import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * Vector3 math class.
 * 
 * @author ttevendale
 */
public class Vector3 {

    /**
     * The x coordinate of this vector.
     */
    private double x;

    /**
     * The y coordinate of this vector.
     */
    private double y;

    /**
     * The z coordinate of this vector.
     */
    private double z;

    /**
     * default constructor
     */
    public Vector3() {

        this.x = 0;
        this.y = 0;
        this.z = 0;
    }

    /**
     * constructor
     * 
     * @param vector The vector to make this vector from.
     */
    public Vector3(Vector3 vector) {

        this.x = vector.x;
        this.y = vector.y;
        this.z = vector.z;
    }

    /**
     * constructor
     * 
     * @param x The x coordinate of this vector.
     * @param y The y coordinate of this vector.
     * @param z The z coordinate of this vector.
     */
    public Vector3(double x, double y, double z) {

        this.x = x;
        this.y = y;
        this.z = z;
    }

    /**
     * Adds the vector to this vector.
     * 
     * @param vector The vector to be added.
     */
    public void add(Vector3 vector) {

        this.x += vector.x;
        this.y += vector.y;
        this.z += vector.z;
    }

    /**
     * Adds the scalar to this vector.
     * 
     * @param scalar The scalar to be added.
     */
    public void addScalar(double scalar) {

        this.x += scalar;
        this.y += scalar;
        this.z += scalar;
    }

    /**
     * Adds two vectors together.
     * 
     * @param vector1 The first vector to be added.
     * @param vector2 The second vector to be added.
     * @return The added vector.
     */
    public static Vector3 addVectors(Vector3 vector1, Vector3 vector2) {

        Vector3 newVector = new Vector3();
        newVector.x = vector1.x + vector2.x;
        newVector.y = vector1.y + vector2.y;
        newVector.z = vector1.z + vector2.z;

        return newVector;
    }

    /**
     * Subtracts the vector from this vector.
     * 
     * @param vector The vector to be subtracted.
     */
    public void sub(Vector3 vector) {

        this.x -= vector.x;
        this.y -= vector.y;
        this.z -= vector.z;
    }

    /**
     * Subtracts the scalar from this vector.
     * 
     * @param scalar The scalar to be subtracted.
     */
    public void subScalar(double scalar) {

        this.x -= scalar;
        this.y -= scalar;
        this.z -= scalar;
    }

    /**
     * Subtracts the two vectors.
     * 
     * @param vector1 The vector to be subtracted from.
     * @param vector2 The vector to be subtracted.
     * @return The subtracted vector.
     */
    public static Vector3 subVectors(Vector3 vector1, Vector3 vector2) {

        Vector3 newVector = new Vector3();
        newVector.x = vector1.x - vector2.x;
        newVector.y = vector1.y - vector2.y;
        newVector.z = vector1.z - vector2.z;

        return newVector;
    }

    /**
     * Multiplies the vector to this vector.
     * 
     * @param vector The vector to be multiplied.
     */
    public void multiply(Vector3 vector) {

        this.x *= vector.x;
        this.y *= vector.y;
        this.z *= vector.z;
    }

    /**
     * Multiplies the scalar to this vector. NOTE: This function was using isFinite (not
     * Number.isFinite) javascript function if isFinite came back as false the vector values woulbe
     * be set to zero.
     * 
     * @param scalar The scalar to be multiplied.
     */
    public void multiplyScalar(double scalar) {

        this.x *= scalar;
        this.y *= scalar;
        this.z *= scalar;
    }

    /**
     * Multiplies the two vectors.
     * 
     * @param vector1 The first vector to be multiplied.
     * @param vector2 The second vector to be multiplied.
     * @return The multiplied vector.
     */
    public static Vector3 multiplyVectors(Vector3 vector1, Vector3 vector2) {

        Vector3 newVector = new Vector3();
        newVector.x = vector1.x * vector2.x;
        newVector.y = vector1.y * vector2.y;
        newVector.z = vector1.z * vector2.z;

        return newVector;
    }

    /**
     * Divides the vector from this vector.
     * 
     * @param vector the vector to be divided.
     */
    public void divide(Vector3 vector) {

        if (vector.getX() == 0 || vector.getY() == 0 || vector.getZ() == 0) {

            throw new ArithmeticException("Cannot divide by zero.");
        }

        this.x /= vector.x;
        this.y /= vector.y;
        this.z /= vector.z;
    }

    /**
     * Divides this vector by the scalar.
     * 
     * @param scalar The scalar to divide by.
     */
    public void divideScalar(double scalar) {

        if (scalar == 0) {

            throw new ArithmeticException("Cannot divide by zero.");
        }

        this.multiplyScalar(1 / scalar);
    }

    /**
     * Negates this vector.
     */
    public void negate() {

        this.x = -this.x;
        this.y = -this.y;
        this.z = -this.z;
    }

    /**
     * Dot product of the vector and this vector.
     * 
     * @param vector The vector to calculate the dot product of.
     * @return The dot product of the vectors.
     */
    public double dot(Vector3 vector) {

        return this.x * vector.x + this.y * vector.y + this.z * vector.z;
    }

    /**
     * Calculates a vector on the line created by the passed-in vector and this vector. NOTE: in
     * order to be between the two vectors alpha has to be between 0 and 1.
     * 
     * @param vector The vector used to lerp.
     * @param alpha The alpha to determine where the new vector is created.
     * @return The lerped vector.
     */
    public Vector3 lerp(Vector3 vector, double alpha) {

        return Vector3.lerpVectors(this, vector, alpha);
    }

    /**
     * Calculates a vector on the line created by the passed-in vectors. NOTE: in order to be
     * between the two vectors alpha has to be between 0 and 1.
     * 
     * @param vector1 The start vector of the line.
     * @param vector2 The end vector of the line.
     * @param alpha The alpha to determine where the new vector is created.
     * @return The lerped vector.
     */
    public static Vector3 lerpVectors(Vector3 vector1, Vector3 vector2, double alpha) {

        Vector3 newVector = Vector3.subVectors(vector2, vector1);
        newVector.multiplyScalar(alpha);
        newVector.add(vector1);

        return newVector;
    }

    /**
     * Crosses the vector with this vector.
     * 
     * @param vector The vector to cross.
     */
    public void cross(Vector3 vector) {

        Vector3 vector2 = new Vector3(this);
        this.x = (vector2.y * vector.z) - (vector2.z * vector.y);
        this.y = (vector2.z * vector.x) - (vector2.x * vector.z);
        this.z = (vector2.x * vector.y) - (vector2.y * vector.x);
    }

    /**
     * Calculates the cross of the vectors.
     * 
     * @param vector1 The first vector to cross.
     * @param vector2 The second vector to cross.
     * @return The crossed vector.
     */
    public static Vector3 crossVectors(Vector3 vector1, Vector3 vector2) {

        Vector3 newVector = new Vector3();
        newVector.x = (vector1.y * vector2.z) - (vector1.z * vector2.y);
        newVector.y = (vector1.z * vector2.x) - (vector1.x * vector2.z);
        newVector.z = (vector1.x * vector2.y) - (vector1.y * vector2.x);

        return newVector;
    }

    /**
     * Calculates the distance between the vector and this vector.
     * 
     * @param vector The vector to check the distance against.
     * @return The distance between the vectors.
     */
    public double distanceTo(Vector3 vector) {

        return Math.sqrt(this.distanceToSquared(vector));
    }

    /**
     * Calculates the distance between the vector and this vector squared.
     * 
     * @param vector The vector to check the distance against.
     * @return The distance between the vectors squared.
     */
    public double distanceToSquared(Vector3 vector) {

        double dx = this.x - vector.x;
        double dy = this.y - vector.y;
        double dz = this.z - vector.z;

        return dx * dx + dy * dy + dz * dz;
    }

    /**
     * Checks if two vectors are equal.
     * 
     * @param obj The object to check.
     * @return True if the object equals this vector, false otherwise.
     */
    @Override
    public boolean equals(Object obj) {

        boolean result = false;
        if (obj instanceof Vector3) {

            Vector3 vector = (Vector3)obj;
            if (Math.abs(this.x - vector.getX()) < .00000000001 && Math.abs(this.y - vector.getY()) < .00000000001 && Math.abs(this.z - vector.getZ()) < .00000000001) {

                result = true;
            }
        }
        return result;
    }

    /**
     * HashCode operation.
     * 
     * @return A hashcode of the data.
     */
    @Override
    public int hashCode() {

        return new HashCodeBuilder(95, 41).append(this.x).append(this.y).append(this.z).toHashCode();
    }

    /**
     * Calculates the closest vector from this vector.
     * 
     * @param vectors The collection of vectors to check aganisnt.
     * @return The closest vector to this vector.
     */
    public Vector3 closestVector(Collection<Vector3> vectors) {

        Vector3 result = null;
        Double distance = null;

        for (Vector3 vector : vectors) {

            double tempDistance = this.distanceToSquared(vector);
            if (distance == null || tempDistance < distance) {

                distance = tempDistance;
                result = vector;
            }
        }

        return result;
    }

    /**
     * Getter for the x coordinate.
     * 
     * @return The vector's x coordinate.
     */
    public double getX() {

        return this.x;
    }

    /**
     * Getter for the y coordinate.
     * 
     * @return The vector's y coordinate.
     */
    public double getY() {

        return this.y;
    }

    /**
     * Getter for the z coordinate.
     * 
     * @return The vector's z coordinate.
     */
    public double getZ() {

        return this.z;
    }
}
