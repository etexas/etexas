/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */

package org.etexascode.interrep.datamodel;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.interrep.datamodel.interfaces.ILaneNode;

/**
 * The lane node model.
 * 
 * @author ablatt
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement
public class LaneNode implements Serializable, ILaneNode {

    /** Serial ID. */
    @XmlTransient
    private static final long serialVersionUID = -7691826838934008131L;

    /** The x coordinate. */
    @XmlElement
    protected double x = 0.0;

    /** The y coordinate. */
    @XmlElement
    protected double y = 0.0;

    /** The z coordinate. */
    @XmlElement
    protected double z = 0.0;

    /** The width. */
    @XmlElement
    protected double width = 0.0;

    /** Constructor. */
    public LaneNode() {}

    /**
     * Constructor.
     * 
     * @param x The x coordinate.
     * @param y The y coordinate.
     */
    public LaneNode(double x, double y) {
        this.x = x;
        this.y = y;
        z = 0;
        width = 1;
    }

    /**
     * Constructor.
     * 
     * @param x The x coordinate.
     * @param y The y coordinate.
     * @param z The z coordinate.
     * @param width The width value.
     */
    public LaneNode(double x, double y, double z, double width) {
        this.x = x;
        this.y = y;
        this.z = z;
        this.width = width;
    }

    /**
     * Duplicate constructor.
     * 
     * @param node The node to duplicate.
     */
    public LaneNode(ILaneNode node) {
        this.x = node.getX();
        this.y = node.getY();
        this.z = node.getZ();
        this.width = node.getWidth();
    }

    /** String representation of lane node. */
    @Override
    public String toString() {
        StringBuilder ret = new StringBuilder();

        ret.append("x = ");
        ret.append(x);
        ret.append("\n");

        ret.append("y = ");
        ret.append(y);
        ret.append("\n");

        ret.append("z = ");
        ret.append(z);
        ret.append("\n");

        ret.append("width = ");
        ret.append(width);
        ret.append("\n");

        return ret.toString();
    }

    /** Checks if two lane nodes are equal. */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof LaneNode) {
            LaneNode rhs = (LaneNode)obj;
            return new EqualsBuilder().append(x, rhs.x).append(y, rhs.y).append(z, rhs.z).append(width, rhs.width).isEquals();
        }
        return false;
    }

    /**
     * HashCode operation.
     * 
     * @return A hashcode of the data.
     */
    @Override
    public int hashCode() {
        return new HashCodeBuilder(29, 41).append(x).append(y).append(z).append(width).toHashCode();
    }

    /**
     * Gets the x coordinate.
     * 
     * @return The x coordinate.
     */
    public double getX() {
        return x;
    }

    /**
     * Sets the x coordinate.
     * 
     * @param x The new x coordinate.
     */
    public void setX(double x) {
        this.x = x;
    }

    /**
     * Gets the y coordinate.
     * 
     * @return The y coordinate.
     */
    public double getY() {
        return y;
    }

    /**
     * Sets the y coordinate.
     * 
     * @param y The new y coordinate.
     */
    public void setY(double y) {
        this.y = y;
    }

    /**
     * Gets the z coordinate.
     * 
     * @return The z coordinate.
     */
    public double getZ() {
        return z;
    }

    /**
     * Sets the z coordinate.
     * 
     * @param z The new z coordinate.
     */
    public void setZ(double z) {
        this.z = z;
    }

    /**
     * Gets the width.
     * 
     * @return The width.
     */
    @Override
    public double getWidth() {
        return width;
    }

    /**
     * Sets the width.
     * 
     * @param width The new width.
     */
    public void setWidth(double width) {
        this.width = width;
    }
}
