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
 * The lane attributes bike element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class LaneAttributesBike implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover a lane attributes bike.
     */
    public static final int NUM_BITS = 16;

    /**
     * The bike revocable lane status.
     */
    private boolean bikeRevocableLane = false;

    /**
     * The pedestrian use allowed status.
     */
    private boolean pedestrianUseAllowed = false;

    /**
     * The bike fly over lane status.
     */
    private boolean bikeFlyOverLane = false;

    /**
     * The fixed cycle time status.
     */
    private boolean fixedCycleTime = false;

    /**
     * The bi directional cycle times status.
     */
    private boolean biDirectionalCycleTimes = false;

    /**
     * The isolated by barrier status.
     */
    private boolean isolatedByBarrier = false;

    /**
     * The unsignalized segments present status.
     */
    private boolean unsignalizedSegmentsPresent = false;

    /**
     * A getter for the bike revocable lane status.
     * 
     * @return The bike revocable lane status.
     */
    public boolean isBikeRevocableLane() {

        return bikeRevocableLane;
    }

    /**
     * A setter for the bike revocable lane status.
     * 
     * @param bikeRevocableLane The bike revocable lane status to set.
     */
    public void setBikeRevocableLane(boolean bikeRevocableLane) {

        this.bikeRevocableLane = bikeRevocableLane;
    }

    /**
     * A getter for the pedestrian use allowed status.
     * 
     * @return The pedestrian use allowed status.
     */
    public boolean isPedestrianUseAllowed() {

        return pedestrianUseAllowed;
    }

    /**
     * A setter for the pedestrian use allowed status.
     * 
     * @param pedestrianUseAllowed The pedestrian use allowed status to set.
     */
    public void setPedestrianUseAllowed(boolean pedestrianUseAllowed) {

        this.pedestrianUseAllowed = pedestrianUseAllowed;
    }

    /**
     * A getter for the bike fly over lane status.
     * 
     * @return The bike fly over lane status.
     */
    public boolean isBikeFlyOverLane() {

        return bikeFlyOverLane;
    }

    /**
     * A setter for the bike fly over lane status.
     * 
     * @param bikeFlyOverLane The bike fly over lane status to set.
     */
    public void setBikeFlyOverLane(boolean bikeFlyOverLane) {

        this.bikeFlyOverLane = bikeFlyOverLane;
    }

    /**
     * A getter for the fixed cycle time status.
     * 
     * @return The fixed cycle time status.
     */
    public boolean isFixedCycleTime() {

        return fixedCycleTime;
    }

    /**
     * A setter for the fixed cycle time status.
     * 
     * @param fixedCycleTime The fixed cycle time status to set.
     */
    public void setFixedCycleTime(boolean fixedCycleTime) {

        this.fixedCycleTime = fixedCycleTime;
    }

    /**
     * A getter for the bi directional cycle times status.
     * 
     * @return The bi directional cycle times status.
     */
    public boolean isBiDirectionalCycleTimes() {

        return biDirectionalCycleTimes;
    }

    /**
     * A setter for the bi directional cycle times status.
     * 
     * @param biDirectionalCycleTimes The bi directional cycle times status to set.
     */
    public void setBiDirectionalCycleTimes(boolean biDirectionalCycleTimes) {

        this.biDirectionalCycleTimes = biDirectionalCycleTimes;
    }

    /**
     * A getter for the isolated by barrier status.
     * 
     * @return The isolated by barrier status.
     */
    public boolean isIsolatedByBarrier() {

        return isolatedByBarrier;
    }

    /**
     * A setter for the isolated by barrier status.
     * 
     * @param isolatedByBarrier The isolated by barrier status to set.
     */
    public void setIsolatedByBarrier(boolean isolatedByBarrier) {

        this.isolatedByBarrier = isolatedByBarrier;
    }

    /**
     * A getter for the unsignalized segments present status.
     * 
     * @return The unsignalized segments present status.
     */
    public boolean isUnsignalizedSegmentsPresent() {

        return unsignalizedSegmentsPresent;
    }

    /**
     * A setter for the unsignalized segments present status.
     * 
     * @param unsignalizedSegmentsPresent The unsignalized segments present status to set.
     */
    public void setUnsignalizedSegmentsPresent(boolean unsignalizedSegmentsPresent) {

        this.unsignalizedSegmentsPresent = unsignalizedSegmentsPresent;
    }

    @Override
    public String encodeUPER() {

        StringBuilder laneAttributesBike = new StringBuilder(16);
        laneAttributesBike.append(bikeRevocableLane ? '1' : '0');
        laneAttributesBike.append(pedestrianUseAllowed ? '1' : '0');
        laneAttributesBike.append(bikeFlyOverLane ? '1' : '0');
        laneAttributesBike.append(fixedCycleTime ? '1' : '0');
        laneAttributesBike.append(biDirectionalCycleTimes ? '1' : '0');
        laneAttributesBike.append(isolatedByBarrier ? '1' : '0');
        laneAttributesBike.append(unsignalizedSegmentsPresent ? '1' : '0');
        laneAttributesBike.append("000000000");

        return laneAttributesBike.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (LaneAttributesBike.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a LaneAttributesBike element (%d)", LaneAttributesBike.NUM_BITS));
        }

        String laneAttributesBikeBits = bits.substring(0, LaneAttributesBike.NUM_BITS);

        if (!"000000000".equals(laneAttributesBikeBits.substring(7, 16))) {

            throw new IllegalArgumentException("The bits 7 - 15 must be set to zero, because the reserved bits are not currently in use.");
        }

        bikeRevocableLane = laneAttributesBikeBits.charAt(0) == '1';
        pedestrianUseAllowed = laneAttributesBikeBits.charAt(1) == '1';
        bikeFlyOverLane = laneAttributesBikeBits.charAt(2) == '1';
        fixedCycleTime = laneAttributesBikeBits.charAt(3) == '1';
        biDirectionalCycleTimes = laneAttributesBikeBits.charAt(4) == '1';
        isolatedByBarrier = laneAttributesBikeBits.charAt(5) == '1';
        unsignalizedSegmentsPresent = laneAttributesBikeBits.charAt(6) == '1';

        return bits.substring(LaneAttributesBike.NUM_BITS);
    }

    @Override
    public int hashCode() {

        return Objects.hash(bikeRevocableLane, pedestrianUseAllowed, bikeFlyOverLane, fixedCycleTime, biDirectionalCycleTimes, isolatedByBarrier, unsignalizedSegmentsPresent);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof LaneAttributesBike)) {

            return false;
        }
        LaneAttributesBike element = (LaneAttributesBike)object;
        return this.bikeRevocableLane == element.bikeRevocableLane
                && this.pedestrianUseAllowed == element.pedestrianUseAllowed
                && this.bikeFlyOverLane == element.bikeFlyOverLane
                && this.fixedCycleTime == element.fixedCycleTime
                && this.biDirectionalCycleTimes == element.biDirectionalCycleTimes
                && this.isolatedByBarrier == element.isolatedByBarrier
                && this.unsignalizedSegmentsPresent == element.unsignalizedSegmentsPresent;
    }
}
