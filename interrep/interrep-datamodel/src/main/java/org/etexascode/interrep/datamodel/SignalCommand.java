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

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.CoberturaIgnore;

/**
 * The Signal Command class.
 * 
 * @author jrutherford
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class SignalCommand extends Command implements Serializable {

    /** Serial ID. */
    @XmlTransient
    private static final long serialVersionUID = -1176639313058357218L;

    /** Signal Operations. */
    @XmlTransient
    public static final int CHANGE_SIGNAL = 1;

    @XmlTransient
    public static final int HOLD_SIGNAL = 2;

    /**
     * Factory method for signal change commands.
     * 
     * @param changeTime A time in seconds of when to change the signal.
     * @return A signal change command.
     */
    public static SignalCommand createChangeCommand(double changeTime) {

        return new SignalCommand(CHANGE_SIGNAL, changeTime);
    }

    /**
     * Factory method for signal hold commands.
     * 
     * @param holdTime A time in seconds to hold the signal.
     * @return A signal hold command.
     */
    public static SignalCommand createHoldCommand(double holdTime) {
        return new SignalCommand(HOLD_SIGNAL, holdTime);
    }

    /** The command to perform. */
    @XmlElement
    private int signalCommand;

    /** Time to do the given command. (seconds) */
    @XmlElement
    private double time;

    /** Constructor. Intentionally left blank. */
    public SignalCommand() {}

    /**
     * Signal command constructor taking in the command and the time for the command.
     * 
     * @param command The signal command.
     * @param time The time for the command.
     */
    public SignalCommand(int command, double time) {

        if (command != CHANGE_SIGNAL && command != HOLD_SIGNAL) {

            throw new IllegalArgumentException(String.format("The value \"%d\" is not a recognized signal command.", command));
        }

        this.signalCommand = command;
        this.time = time;
    }

    @Override
    public String getDescription() {

        switch (signalCommand) {

            case SignalCommand.CHANGE_SIGNAL:
                return String.format("Request that signals change their state in %.2f seconds", time);

            case SignalCommand.HOLD_SIGNAL:
                return String.format("Request that signals hold their state for an additional %.2f seconds", time);

            default:
                throw new IllegalStateException(String.format("The value \"%d\" is not a recognized signal command.", signalCommand));
        }
    }

    /**
     * Gets the command to execute.
     * 
     * @return Command constant from above.
     */
    public int getSignalCommand() {
        return signalCommand;
    }

    /**
     * Gets the time to do the given command.
     * 
     * @return The time.
     */
    public double getTime() {
        return time;
    }

    /**
     * Equality check for unit testing.
     * 
     * @param o The object to compare to.
     * @return True/False the object is equal.
     */
    @Override
    public boolean equals(Object o) {
        if (o instanceof SignalCommand) {
            SignalCommand sc = (SignalCommand)o;
            return (this.signalCommand == sc.signalCommand) && (this.time == sc.time);

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
        return new HashCodeBuilder(41, 43).append(signalCommand).append(time).toHashCode();
    }

    @Override
    public String toString() {
        StringBuilder add = new StringBuilder();
        add.append("SignalCommand [command=");
        add.append(signalCommand);
        add.append(", time=");
        add.append(time);
        add.append("]");
        return add.toString();
    }
}
