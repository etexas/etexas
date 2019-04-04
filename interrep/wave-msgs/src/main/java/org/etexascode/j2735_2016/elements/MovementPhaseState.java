/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2018 Harmonia Holdings Group, LLC
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
package org.etexascode.j2735_2016.elements;

import java.util.Objects;

import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;
import org.etexascode.j2735_2016.util.UPERInteger;

/**
 * The movement phase state element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class MovementPhaseState implements UnalignedPackedEncodingRules {

    /**
     * The enum of MovementPhaseState. This is created inside of the class because
     * MovementPhaseState is an enum but I needed class controls to properly handle it.
     * 
     * @author ttevendale
     */
    public enum MovementPhase {
        UNAVAILABLE(0),
        DARK(1),
        STOP_THEN_PROCEED(2),
        STOP_AND_REMAIN(3),
        PRE_MOVEMENT(4),
        PERMISSIVE_MOVEMENT_ALLOWED(5),
        PROTECTED_MOVEMENT_ALLOWED(6),
        PERMISSIVE_CLEARANCE(7),
        PROTECTED_CLEARANCE(8),
        CAUTION_CONFLICTING_TRAFFIC(9);

        /**
         * The integer representation of the enumeration.
         */
        private int number;

        /**
         * Creates a new <code>MovementPhase</code> with the specified number.
         * 
         * @param number The integer number to set.
         */
        private MovementPhase(int number) {

            this.number = number;
        }

        /**
         * Gets the integer representation of this enumeration.
         * 
         * @return The number.
         */
        public int getNumber() {

            return number;
        }
    }

    /**
     * The number of bits that cover a movement phase state.
     */
    public static final int NUM_BITS = 4;

    /**
     * The movement phase enumeration.
     */
    private MovementPhase enumeration;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public MovementPhaseState() {

        enumeration = MovementPhase.UNAVAILABLE;
    }

    /**
     * A constructor for the movement phase state element.
     * 
     * @param enumeration The movement phase enumeration to set.
     */
    public MovementPhaseState(MovementPhase enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    /**
     * A getter for the movement phase enumeration.
     * 
     * @return The movement phase enumeration.
     */
    public MovementPhase getEnumeration() {

        return enumeration;
    }

    /**
     * A setter for the movement phase enumeration.
     * 
     * @param enumeration The movement phase enumeration to set.
     */
    public void setEnumeration(MovementPhase enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    @Override
    public String encodeUPER() {

        return UPERInteger.encode(enumeration.getNumber(), 0, MovementPhaseState.NUM_BITS);
    }

    @Override
    public String decodeUPER(String bits) {

        if (MovementPhaseState.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for an MovementPhaseState element (%d)", MovementPhaseState.NUM_BITS));
        }

        int stateNum = UPERInteger.decode(bits.substring(0, MovementPhaseState.NUM_BITS), 0);
        boolean found = false;
        for (MovementPhase movementPhase : MovementPhase.values()) {

            if (stateNum == movementPhase.getNumber()) {

                enumeration = movementPhase;
                found = true;
            }
        }
        if (!found) {

            throw new IllegalArgumentException("The bits supplied did not match any known MovementPhase enumeration");
        }

        return bits.substring(MovementPhaseState.NUM_BITS);
    }

    @Override
    public int hashCode() {

        return Objects.hashCode(enumeration);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof MovementPhaseState)) {

            return false;
        }
        MovementPhaseState element = (MovementPhaseState)object;
        return this.enumeration.equals(element.enumeration);
    }
}
