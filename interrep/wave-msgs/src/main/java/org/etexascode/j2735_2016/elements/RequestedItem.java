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
 * The requested item element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class RequestedItem implements UnalignedPackedEncodingRules {

    /**
     * The enum of RequestedItem. This is created inside of the class because RequestedItem is an
     * enum but I needed class controls to properly handle it.
     * 
     * @author ttevendale
     */
    public enum Item {
        RESERVED(0),
        A(1), // consisting of 2 elements: lights (ExteriorLights) and lightBar (LightbarInUse)
        B(2), // consisting of: wipers (a SEQUENCE)
        C(3), // consisting of: brakeStatus (BrakeSystemStatus)
        D(4), // consisting of 2 elements: brakePressure (BrakeAppliedPressure and roadFriction
              // (CoefficientOfFriction)
        E(5), // consisting of 4 elements: sunData (SunSensor), rainData (RainSensor) airTemp
              // (AmbientAirTemperature), and airPres (AmbientAirPressure)
        F(6), // consisting of: steering (a SEQUENCE)
        G(7), // consisting of: accelSets (a SEQUENCE)
        I(8), // consisting of: fullPos (FullPositionVector)
        J(9), // consisting of: position2D (Position2D)
        K(10), // consisting of: position3D (Position3D)
        L(11), // consisting of 2 elements: speedHeadC (SpeedandHeadingConfidence)
        M(12), // consisting of: vehicleData (a SEQUENCE)
        N(13), // consisting of: vehicleIdent (VehicleIdent)
        O(14), // consisting of: weatherReport (a SEQUENCE)
        P(15), // consisting of: breadcrumbs (PathHistory)
        Q(16); // consisting of: GNSSStatus (GNSSstatus)

        /**
         * The integer representation of the enumeration.
         */
        private int number;

        /**
         * Creates a new <code>Item</code> with the specified number.
         * 
         * @param number The integer number to set.
         */
        private Item(int number) {

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
     * The number of bits that cover a requested item.
     */
    public static final int NUM_BITS = 6;

    /**
     * The item enumeration.
     */
    private Item enumeration;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public RequestedItem() {

        enumeration = Item.RESERVED;
    }

    /**
     * A constructor for the requested item element.
     * 
     * @param enumeration The item enumeration to set.
     */
    public RequestedItem(Item enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    /**
     * A getter for the item enumeration.
     * 
     * @return The item enumeration.
     */
    public Item getEnumeration() {

        return enumeration;
    }

    /**
     * A setter for the item enumeration.
     * 
     * @param enumeration The item enumeration to set.
     */
    public void setEnumeration(Item enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    @Override
    public String encodeUPER() {

        String extensionBit = "0";
        return extensionBit + UPERInteger.encode(enumeration.getNumber(), 0, RequestedItem.NUM_BITS - 1);
    }

    @Override
    public String decodeUPER(String bits) {

        if (RequestedItem.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a RequestedItem element (%d)", RequestedItem.NUM_BITS));
        }

        String requestedItemBits = bits.substring(0, RequestedItem.NUM_BITS);

        if (requestedItemBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The RequestedItem extension is not supported");
        }

        int itemNum = UPERInteger.decode(requestedItemBits.substring(1), 0);
        boolean found = false;
        for (Item requestedItem : Item.values()) {

            if (itemNum == requestedItem.getNumber()) {

                enumeration = requestedItem;
                found = true;
            }
        }
        if (!found) {

            throw new IllegalArgumentException("The bits supplied did not match any known RequestedItem enumeration");
        }

        return bits.substring(RequestedItem.NUM_BITS);
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
        if (!(object instanceof RequestedItem)) {

            return false;
        }
        RequestedItem element = (RequestedItem)object;
        return this.enumeration.equals(element.enumeration);
    }
}
