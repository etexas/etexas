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
 * The lane attributes tracked vehicle element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class LaneAttributesTrackedVehicle implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover a lane attributes tracked vehicle.
     */
    public static final int NUM_BITS = 16;

    /**
     * The spec revocable lane status.
     */
    private boolean specRevocableLane = false;

    /**
     * The spec commuter rail road track status.
     */
    private boolean specCommuterRailRoadTrack = false;

    /**
     * The spec light rail road track status.
     */
    private boolean specLightRailRoadTrack = false;

    /**
     * The spec heavy rail road track status.
     */
    private boolean specHeavyRailRoadTrack = false;

    /**
     * The spec other rail type status.
     */
    private boolean specOtherRailType = false;

    /**
     * A getter for the spec revocable lane status.
     * 
     * @return The spec revocable lane status.
     */
    public boolean isSpecRevocableLane() {

        return specRevocableLane;
    }

    /**
     * A setter for the spec revocable lane status.
     * 
     * @param specRevocableLane The spec revocable lane status to set.
     */
    public void setSpecRevocableLane(boolean specRevocableLane) {

        this.specRevocableLane = specRevocableLane;
    }

    /**
     * A getter for the spec commuter rail road track status.
     * 
     * @return The spec commuter rail road track status.
     */
    public boolean isSpecCommuterRailRoadTrack() {

        return specCommuterRailRoadTrack;
    }

    /**
     * A setter for the spec commuter rail road track status.
     * 
     * @param specCommuterRailRoadTrack The spec commuter rail road track status to set.
     */
    public void setSpecCommuterRailRoadTrack(boolean specCommuterRailRoadTrack) {

        this.specCommuterRailRoadTrack = specCommuterRailRoadTrack;
    }

    /**
     * A getter for the spec light rail road track status.
     * 
     * @return The spec light rail road track status.
     */
    public boolean isSpecLightRailRoadTrack() {

        return specLightRailRoadTrack;
    }

    /**
     * A setter for the spec light rail road track status.
     * 
     * @param specLightRailRoadTrack The spec light rail road track status to set.
     */
    public void setSpecLightRailRoadTrack(boolean specLightRailRoadTrack) {

        this.specLightRailRoadTrack = specLightRailRoadTrack;
    }

    /**
     * A getter for the spec heavy rail road track status.
     * 
     * @return The spec heavy rail road track status.
     */
    public boolean isSpecHeavyRailRoadTrack() {

        return specHeavyRailRoadTrack;
    }

    /**
     * A setter for the spec heavy rail road track status.
     * 
     * @param specHeavyRailRoadTrack The spec heavy rail road track status to set.
     */
    public void setSpecHeavyRailRoadTrack(boolean specHeavyRailRoadTrack) {

        this.specHeavyRailRoadTrack = specHeavyRailRoadTrack;
    }

    /**
     * A getter for the spec other rail type status.
     * 
     * @return The spec other rail type status.
     */
    public boolean isSpecOtherRailType() {

        return specOtherRailType;
    }

    /**
     * A setter for the spec other rail type status.
     * 
     * @param specOtherRailType The spec other rail type status to set.
     */
    public void setSpecOtherRailType(boolean specOtherRailType) {

        this.specOtherRailType = specOtherRailType;
    }

    @Override
    public String encodeUPER() {

        StringBuilder laneAttributesTrackedVehicle = new StringBuilder(16);
        laneAttributesTrackedVehicle.append(specRevocableLane ? '1' : '0');
        laneAttributesTrackedVehicle.append(specCommuterRailRoadTrack ? '1' : '0');
        laneAttributesTrackedVehicle.append(specLightRailRoadTrack ? '1' : '0');
        laneAttributesTrackedVehicle.append(specHeavyRailRoadTrack ? '1' : '0');
        laneAttributesTrackedVehicle.append(specOtherRailType ? '1' : '0');
        laneAttributesTrackedVehicle.append("00000000000");

        return laneAttributesTrackedVehicle.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (LaneAttributesTrackedVehicle.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a LaneAttributesTrackedVehicle element (%d)", LaneAttributesTrackedVehicle.NUM_BITS));
        }

        String laneAttributesTrackedVehicleBits = bits.substring(0, LaneAttributesTrackedVehicle.NUM_BITS);

        if (!"00000000000".equals(laneAttributesTrackedVehicleBits.substring(5, 16))) {

            throw new IllegalArgumentException("The bits 5 - 15 must be set to zero, because the reserved bits are not currently in use.");
        }

        specRevocableLane = laneAttributesTrackedVehicleBits.charAt(0) == '1';
        specCommuterRailRoadTrack = laneAttributesTrackedVehicleBits.charAt(1) == '1';
        specLightRailRoadTrack = laneAttributesTrackedVehicleBits.charAt(2) == '1';
        specHeavyRailRoadTrack = laneAttributesTrackedVehicleBits.charAt(3) == '1';
        specOtherRailType = laneAttributesTrackedVehicleBits.charAt(4) == '1';

        return bits.substring(LaneAttributesTrackedVehicle.NUM_BITS);
    }

    @Override
    public int hashCode() {

        return Objects.hash(specRevocableLane, specCommuterRailRoadTrack, specLightRailRoadTrack, specHeavyRailRoadTrack, specOtherRailType);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof LaneAttributesTrackedVehicle)) {

            return false;
        }
        LaneAttributesTrackedVehicle element = (LaneAttributesTrackedVehicle)object;
        return this.specRevocableLane == element.specRevocableLane
                && this.specCommuterRailRoadTrack == element.specCommuterRailRoadTrack
                && this.specLightRailRoadTrack == element.specLightRailRoadTrack
                && this.specHeavyRailRoadTrack == element.specHeavyRailRoadTrack
                && this.specOtherRailType == element.specOtherRailType;
    }
}
