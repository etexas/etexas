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

/**
 * The lane attributes parking element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class LaneAttributesParking implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover a lane attributes parking.
     */
    public static final int NUM_BITS = 16;

    /**
     * The parking revocable lane status.
     */
    private boolean parkingRevocableLane = false;

    /**
     * The parallel parking in use status.
     */
    private boolean parallelParkingInUse = false;

    /**
     * The head in parking in use status.
     */
    private boolean headInParkingInUse = false;

    /**
     * The do not park zone status.
     */
    private boolean doNotParkZone = false;

    /**
     * The parking for bus use status.
     */
    private boolean parkingForBusUse = false;

    /**
     * The parking for taxi use status.
     */
    private boolean parkingForTaxiUse = false;

    /**
     * The no public parking use status.
     */
    private boolean noPublicParkingUse = false;

    /**
     * A getter for the parking revocable lane status.
     * 
     * @return The parking revocable lane status.
     */
    public boolean isParkingRevocableLane() {

        return parkingRevocableLane;
    }

    /**
     * A setter for the parking revocable lane status.
     * 
     * @param parkingRevocableLane The parking revocable lane status to set.
     */
    public void setParkingRevocableLane(boolean parkingRevocableLane) {

        this.parkingRevocableLane = parkingRevocableLane;
    }

    /**
     * A getter for the parallel parking in use status.
     * 
     * @return The parallel parking in use status.
     */
    public boolean isParallelParkingInUse() {

        return parallelParkingInUse;
    }

    /**
     * A setter for the parallel parking in use status.
     * 
     * @param parallelParkingInUse The parallel parking in use status to set.
     */
    public void setParallelParkingInUse(boolean parallelParkingInUse) {

        this.parallelParkingInUse = parallelParkingInUse;
    }

    /**
     * A getter for the head in parking in use status.
     * 
     * @return The head in parking in use status.
     */
    public boolean isHeadInParkingInUse() {

        return headInParkingInUse;
    }

    /**
     * A setter for the head in parking in use status.
     * 
     * @param headInParkingInUse The head in parking in use status to set.
     */
    public void setHeadInParkingInUse(boolean headInParkingInUse) {

        this.headInParkingInUse = headInParkingInUse;
    }

    /**
     * A getter for the do not park zone status.
     * 
     * @return The do not park zone status.
     */
    public boolean isDoNotParkZone() {

        return doNotParkZone;
    }

    /**
     * A setter for the do not park zone status.
     * 
     * @param doNotParkZone The do not park zone status to set.
     */
    public void setDoNotParkZone(boolean doNotParkZone) {

        this.doNotParkZone = doNotParkZone;
    }

    /**
     * A getter for the parking for bus use status.
     * 
     * @return The parking for bus use status.
     */
    public boolean isParkingForBusUse() {

        return parkingForBusUse;
    }

    /**
     * A setter for the parking for bus use status.
     * 
     * @param parkingForBusUse The parking for bus use status to set.
     */
    public void setParkingForBusUse(boolean parkingForBusUse) {

        this.parkingForBusUse = parkingForBusUse;
    }

    /**
     * A getter for the parking for taxi use status.
     * 
     * @return The parking for taxi use status.
     */
    public boolean isParkingForTaxiUse() {

        return parkingForTaxiUse;
    }

    /**
     * A setter for the parking for taxi use status.
     * 
     * @param parkingForTaxiUse The parking for taxi use status to set.
     */
    public void setParkingForTaxiUse(boolean parkingForTaxiUse) {

        this.parkingForTaxiUse = parkingForTaxiUse;
    }

    /**
     * A getter for the no public parking use status.
     * 
     * @return The no public parking use status.
     */
    public boolean isNoPublicParkingUse() {

        return noPublicParkingUse;
    }

    /**
     * A setter for the no public parking use status.
     * 
     * @param noPublicParkingUse The no public parking use status to set.
     */
    public void setNoPublicParkingUse(boolean noPublicParkingUse) {

        this.noPublicParkingUse = noPublicParkingUse;
    }

    @Override
    public String encodeUPER() {

        StringBuilder laneAttributesParking = new StringBuilder(16);
        laneAttributesParking.append(parkingRevocableLane ? '1' : '0');
        laneAttributesParking.append(parallelParkingInUse ? '1' : '0');
        laneAttributesParking.append(headInParkingInUse ? '1' : '0');
        laneAttributesParking.append(doNotParkZone ? '1' : '0');
        laneAttributesParking.append(parkingForBusUse ? '1' : '0');
        laneAttributesParking.append(parkingForTaxiUse ? '1' : '0');
        laneAttributesParking.append(noPublicParkingUse ? '1' : '0');
        laneAttributesParking.append("000000000");

        return laneAttributesParking.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (LaneAttributesParking.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a LaneAttributesParking element (%d)", LaneAttributesParking.NUM_BITS));
        }

        String laneAttributesParkingBits = bits.substring(0, LaneAttributesParking.NUM_BITS);

        if (!"000000000".equals(laneAttributesParkingBits.substring(7, 16))) {

            throw new IllegalArgumentException("The bits 7 - 15 must be set to zero, because the reserved bits are not currently in use.");
        }

        parkingRevocableLane = laneAttributesParkingBits.charAt(0) == '1';
        parallelParkingInUse = laneAttributesParkingBits.charAt(1) == '1';
        headInParkingInUse = laneAttributesParkingBits.charAt(2) == '1';
        doNotParkZone = laneAttributesParkingBits.charAt(3) == '1';
        parkingForBusUse = laneAttributesParkingBits.charAt(4) == '1';
        parkingForTaxiUse = laneAttributesParkingBits.charAt(5) == '1';
        noPublicParkingUse = laneAttributesParkingBits.charAt(6) == '1';

        return bits.substring(LaneAttributesParking.NUM_BITS);
    }

    @Override
    public int hashCode() {

        return Objects.hash(parkingRevocableLane, parallelParkingInUse, headInParkingInUse, doNotParkZone, parkingForBusUse, parkingForTaxiUse, noPublicParkingUse);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof LaneAttributesParking)) {

            return false;
        }
        LaneAttributesParking element = (LaneAttributesParking)object;
        return this.parkingRevocableLane == element.parkingRevocableLane
                && this.parallelParkingInUse == element.parallelParkingInUse
                && this.headInParkingInUse == element.headInParkingInUse
                && this.doNotParkZone == element.doNotParkZone
                && this.parkingForBusUse == element.parkingForBusUse
                && this.parkingForTaxiUse == element.parkingForTaxiUse
                && this.noPublicParkingUse == element.noPublicParkingUse;
    }
}
