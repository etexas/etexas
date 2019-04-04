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
 * The lane attributes sidewalk element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class LaneAttributesSidewalk implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover a lane attributes sidewalk.
     */
    public static final int NUM_BITS = 16;

    /**
     * The sidewalk revocable lane status.
     */
    private boolean sidewalkRevocableLane = false;

    /**
     * The bicycle use allowed status.
     */
    private boolean bicycleUseAllowed = false;

    /**
     * The sidewalk fly over lane status.
     */
    private boolean sidewalkFlyOverLane = false;

    /**
     * The walk bikes status.
     */
    private boolean walkBikes = false;

    /**
     * A getter for the sidewalk revocable lane status.
     * 
     * @return The sidewalk revocable lane status.
     */
    public boolean isSidewalkRevocableLane() {

        return sidewalkRevocableLane;
    }

    /**
     * A setter for the sidewalk revocable lane status.
     * 
     * @param sidewalkRevocableLane The sidewalk revocable lane status to set.
     */
    public void setSidewalkRevocableLane(boolean sidewalkRevocableLane) {

        this.sidewalkRevocableLane = sidewalkRevocableLane;
    }

    /**
     * A getter for the bicycle use allowed status.
     * 
     * @return The bicycle use allowed status.
     */
    public boolean isBicycleUseAllowed() {

        return bicycleUseAllowed;
    }

    /**
     * A setter for the bicycle use allowed status.
     * 
     * @param bicycleUseAllowed The bicycle use allowed status to set.
     */
    public void setBicycleUseAllowed(boolean bicycleUseAllowed) {

        this.bicycleUseAllowed = bicycleUseAllowed;
    }

    /**
     * A getter for the sidewalk fly over lane status.
     * 
     * @return The sidewalk fly over lane status.
     */
    public boolean isSidewalkFlyOverLane() {

        return sidewalkFlyOverLane;
    }

    /**
     * A setter for the sidewalk fly over lane status.
     * 
     * @param sidewalkFlyOverLane The sidewalk fly over lane status to set.
     */
    public void setSidewalkFlyOverLane(boolean sidewalkFlyOverLane) {

        this.sidewalkFlyOverLane = sidewalkFlyOverLane;
    }

    /**
     * A getter for the walk bikes status.
     * 
     * @return The walk bikes status.
     */
    public boolean isWalkBikes() {

        return walkBikes;
    }

    /**
     * A setter for the walk bikes status.
     * 
     * @param walkBikes The walk bikes status to set.
     */
    public void setWalkBikes(boolean walkBikes) {

        this.walkBikes = walkBikes;
    }

    @Override
    public String encodeUPER() {

        StringBuilder laneAttributesSidewalk = new StringBuilder(16);
        laneAttributesSidewalk.append(sidewalkRevocableLane ? '1' : '0');
        laneAttributesSidewalk.append(bicycleUseAllowed ? '1' : '0');
        laneAttributesSidewalk.append(sidewalkFlyOverLane ? '1' : '0');
        laneAttributesSidewalk.append(walkBikes ? '1' : '0');
        laneAttributesSidewalk.append("000000000000");

        return laneAttributesSidewalk.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (LaneAttributesSidewalk.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a LaneAttributesSidewalk element (%d)", LaneAttributesSidewalk.NUM_BITS));
        }

        String laneAttributesSidewalkBits = bits.substring(0, LaneAttributesSidewalk.NUM_BITS);

        if (!"000000000000".equals(laneAttributesSidewalkBits.substring(4, 16))) {

            throw new IllegalArgumentException("The bits 4 - 15 must be set to zero, because the reserved bits are not currently in use.");
        }

        sidewalkRevocableLane = laneAttributesSidewalkBits.charAt(0) == '1';
        bicycleUseAllowed = laneAttributesSidewalkBits.charAt(1) == '1';
        sidewalkFlyOverLane = laneAttributesSidewalkBits.charAt(2) == '1';
        walkBikes = laneAttributesSidewalkBits.charAt(3) == '1';

        return bits.substring(LaneAttributesSidewalk.NUM_BITS);
    }

    @Override
    public int hashCode() {

        return Objects.hash(sidewalkRevocableLane, bicycleUseAllowed, sidewalkFlyOverLane, walkBikes);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof LaneAttributesSidewalk)) {

            return false;
        }
        LaneAttributesSidewalk element = (LaneAttributesSidewalk)object;
        return this.sidewalkRevocableLane == element.sidewalkRevocableLane
                && this.bicycleUseAllowed == element.bicycleUseAllowed
                && this.sidewalkFlyOverLane == element.sidewalkFlyOverLane
                && this.walkBikes == element.walkBikes;
    }
}
