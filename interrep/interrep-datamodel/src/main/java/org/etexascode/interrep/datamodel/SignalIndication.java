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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.CoberturaIgnore;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;

/**
 * The signal indication model.
 * 
 * @author ablatt
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement
public class SignalIndication implements ISignalIndication {

    /** Serial ID. */
    @XmlTransient
    private static final long serialVersionUID = 2914168026124667461L;

    /** Color enumeration. */
    @XmlType
    @XmlEnum
    public enum Color {
        GREEN,
        RED,
        YELLOW,
        NONE
    };

    /** Type enumeration. */
    @XmlType
    @XmlEnum
    public enum Type {
        BALL,
        LEFT_ARROW,
        RIGHT_ARROW,
        STRAIGHT_ARROW,
        UTURN_ARROW,
        STOP_SIGN,
        YIELD_SIGN,
        UNCONTROLLED,
        UNKNOWN
    };

    /** State enumeration. */
    @XmlType
    @XmlEnum
    public enum State {
        STEADY,
        FLASHING,
        SOFT
    };

    /** Signal ID. */
    @XmlElement
    private int laneId;

    /** Color Indication. */
    @XmlElement
    private Color colorIndication;

    /** Type indication. */
    @XmlElement
    private Type typeIndication;

    /** State indication. */
    @XmlElement
    private State stateIndication;

    /** Time to change. */
    @XmlElement
    private double timeToChange;

    /** String representation. */
    @Override
    public String toString() {
        StringBuilder ret = new StringBuilder();

        ret.append("laneId = ");
        ret.append(laneId);
        ret.append("\n");
        ret.append("colorIndication = ");
        String col = colorIndication == null ? "null" : colorIndication.name();
        ret.append(col);
        ret.append("\n");
        ret.append("typeIndication = ");
        String typ = typeIndication == null ? "null" : typeIndication.name();
        ret.append(typ);
        ret.append("\n");
        ret.append("stateIndication = ");
        String sta = stateIndication == null ? "null" : stateIndication.name();
        ret.append(sta);
        ret.append("\n");
        ret.append("timeTochange = ");
        ret.append(timeToChange);
        ret.append("\n");

        return ret.toString();
    }

    /** Check if two signal indications are equal. */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof SignalIndication) {
            SignalIndication rhs = (SignalIndication)obj;
            return new EqualsBuilder().append(laneId, rhs.laneId).append(colorIndication, rhs.colorIndication).append(typeIndication, rhs.typeIndication).append(stateIndication, rhs.stateIndication)
                    .append(timeToChange, rhs.timeToChange).isEquals();
        }
        return false;
    }

    /**
     * HashCode operation.
     * 
     * @return A hashcode of the data.
     */
    @CoberturaIgnore
    @Override
    public int hashCode() {
        return new HashCodeBuilder(61, 67).append(laneId).append(colorIndication).append(typeIndication).append(stateIndication).append(timeToChange).toHashCode();
    }

    /**
     * Gets the lane ID.
     * 
     * @return The lane ID.
     */
    @Override
    public int getLaneId() {
        return laneId;
    }

    /**
     * Sets the lane ID.
     * 
     * @param laneId The new lane ID.
     */
    public void setLaneId(int laneId) {
        this.laneId = laneId;
    }

    /**
     * Gets the color indication.
     * 
     * @return The color indication.
     */
    @Override
    public Color getColorIndication() {
        return colorIndication;
    }

    /**
     * Sets the color indication.
     * 
     * @param colorIndication The new color indication.
     */
    public void setColorIndication(Color colorIndication) {
        this.colorIndication = colorIndication;
    }

    /**
     * Gets the type indication.
     * 
     * @return The type indication.
     */
    @Override
    public Type getTypeIndication() {
        return typeIndication;
    }

    /**
     * Sets the type indication.
     * 
     * @param typeIndication The type indication.
     */
    public void setTypeIndication(Type typeIndication) {
        this.typeIndication = typeIndication;
    }

    /**
     * Gets the state indication.
     * 
     * @return The state indication.
     */
    @Override
    public State getStateIndication() {
        return stateIndication;
    }

    /**
     * Sets the state indication.
     * 
     * @param stateIndication The new state indication.
     */
    public void setStateIndication(State stateIndication) {
        this.stateIndication = stateIndication;
    }

    /**
     * Gets the time to change.
     * 
     * @return The time to change.
     */
    @Override
    public double getTimeToChange() {
        return timeToChange;
    }

    /**
     * Sets the time to change.
     * 
     * @param timeToChange The new time to change.
     */
    public void setTimeToChange(double timeToChange) {
        this.timeToChange = timeToChange;
    }
}