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
 * The lane attributes crosswalk element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class LaneAttributesCrosswalk implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover a lane attributes crosswalk.
     */
    public static final int NUM_BITS = 16;

    /**
     * The crosswalk revocable lane status.
     */
    private boolean crosswalkRevocableLane = false;

    /**
     * The bicycle use allowed status.
     */
    private boolean bicycleUseAllowed = false;

    /**
     * The x walk fly over lane status.
     */
    private boolean xWalkFlyOverLane = false;

    /**
     * The fixed cycle time status.
     */
    private boolean fixedCycleTime = false;

    /**
     * The bi directional cycle times status.
     */
    private boolean biDirectionalCycleTimes = false;

    /**
     * The push to walk button status.
     */
    private boolean pushToWalkButton = false;

    /**
     * The audio support status.
     */
    private boolean audioSupport = false;

    /**
     * The rf signal request present status.
     */
    private boolean rfSignalRequestPresent = false;

    /**
     * The unsignalized segments present status.
     */
    private boolean unsignalizedSegmentsPresent = false;

    /**
     * A getter for the crosswalk revocable lane status.
     * 
     * @return The crosswalk revocable lane status.
     */
    public boolean isCrosswalkRevocableLane() {

        return crosswalkRevocableLane;
    }

    /**
     * A setter for the crosswalk revocable lane status.
     * 
     * @param crosswalkRevocableLane The crosswalk revocable lane status to set.
     */
    public void setCrosswalkRevocableLane(boolean crosswalkRevocableLane) {

        this.crosswalkRevocableLane = crosswalkRevocableLane;
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
     * A getter for the x walk fly over lane status.
     * 
     * @return The x walk fly over lane status.
     */
    public boolean isXWalkFlyOverLane() {

        return xWalkFlyOverLane;
    }

    /**
     * A setter for the x walk fly over lane status.
     * 
     * @param xWalkFlyOverLane The x walk fly over lane status to set.
     */
    public void setXWalkFlyOverLane(boolean xWalkFlyOverLane) {

        this.xWalkFlyOverLane = xWalkFlyOverLane;
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
     * A getter for the push to walk button status.
     * 
     * @return The push to walk button status.
     */
    public boolean hasPushToWalkButton() {

        return pushToWalkButton;
    }

    /**
     * A setter for the push to walk button status.
     * 
     * @param pushToWalkButton The push to walk button status to set.
     */
    public void setPushToWalkButton(boolean pushToWalkButton) {

        this.pushToWalkButton = pushToWalkButton;
    }

    /**
     * A getter for the audio support status.
     * 
     * @return The audio support status.
     */
    public boolean hasAudioSupport() {

        return audioSupport;
    }

    /**
     * A setter for the audio support status.
     * 
     * @param audioSupport The audio support status to set.
     */
    public void setAudioSupport(boolean audioSupport) {

        this.audioSupport = audioSupport;
    }

    /**
     * A getter for the rf signal request present status.
     * 
     * @return The rf signal request present status.
     */
    public boolean isRfSignalRequestPresent() {

        return rfSignalRequestPresent;
    }

    /**
     * A setter for the rf signal request present status.
     * 
     * @param rfSignalRequestPresent The rf signal request present status to set.
     */
    public void setRfSignalRequestPresent(boolean rfSignalRequestPresent) {

        this.rfSignalRequestPresent = rfSignalRequestPresent;
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

        StringBuilder laneAttributesCrosswalk = new StringBuilder(16);
        laneAttributesCrosswalk.append(crosswalkRevocableLane ? '1' : '0');
        laneAttributesCrosswalk.append(bicycleUseAllowed ? '1' : '0');
        laneAttributesCrosswalk.append(xWalkFlyOverLane ? '1' : '0');
        laneAttributesCrosswalk.append(fixedCycleTime ? '1' : '0');
        laneAttributesCrosswalk.append(biDirectionalCycleTimes ? '1' : '0');
        laneAttributesCrosswalk.append(pushToWalkButton ? '1' : '0');
        laneAttributesCrosswalk.append(audioSupport ? '1' : '0');
        laneAttributesCrosswalk.append(rfSignalRequestPresent ? '1' : '0');
        laneAttributesCrosswalk.append(unsignalizedSegmentsPresent ? '1' : '0');
        laneAttributesCrosswalk.append("0000000");

        return laneAttributesCrosswalk.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (LaneAttributesCrosswalk.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a LaneAttributesCrosswalk element (%d)", LaneAttributesCrosswalk.NUM_BITS));
        }

        String laneAttributesCrosswalkBits = bits.substring(0, LaneAttributesCrosswalk.NUM_BITS);

        if (!"0000000".equals(laneAttributesCrosswalkBits.substring(9, 16))) {

            throw new IllegalArgumentException("The bits 9 - 15 must be set to zero, because the reserved bits are not currently in use.");
        }

        crosswalkRevocableLane = laneAttributesCrosswalkBits.charAt(0) == '1';
        bicycleUseAllowed = laneAttributesCrosswalkBits.charAt(1) == '1';
        xWalkFlyOverLane = laneAttributesCrosswalkBits.charAt(2) == '1';
        fixedCycleTime = laneAttributesCrosswalkBits.charAt(3) == '1';
        biDirectionalCycleTimes = laneAttributesCrosswalkBits.charAt(4) == '1';
        pushToWalkButton = laneAttributesCrosswalkBits.charAt(5) == '1';
        audioSupport = laneAttributesCrosswalkBits.charAt(6) == '1';
        rfSignalRequestPresent = laneAttributesCrosswalkBits.charAt(7) == '1';
        unsignalizedSegmentsPresent = laneAttributesCrosswalkBits.charAt(8) == '1';

        return bits.substring(LaneAttributesCrosswalk.NUM_BITS);
    }

    @Override
    public int hashCode() {

        return Objects.hash(crosswalkRevocableLane, bicycleUseAllowed, xWalkFlyOverLane, fixedCycleTime, biDirectionalCycleTimes, pushToWalkButton, audioSupport, rfSignalRequestPresent,
                unsignalizedSegmentsPresent);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof LaneAttributesCrosswalk)) {

            return false;
        }
        LaneAttributesCrosswalk element = (LaneAttributesCrosswalk)object;
        return this.crosswalkRevocableLane == element.crosswalkRevocableLane
                && this.bicycleUseAllowed == element.bicycleUseAllowed
                && this.xWalkFlyOverLane == element.xWalkFlyOverLane
                && this.fixedCycleTime == element.fixedCycleTime
                && this.biDirectionalCycleTimes == element.biDirectionalCycleTimes
                && this.pushToWalkButton == element.pushToWalkButton
                && this.audioSupport == element.audioSupport
                && this.rfSignalRequestPresent == element.rfSignalRequestPresent
                && this.unsignalizedSegmentsPresent == element.unsignalizedSegmentsPresent;
    }
}
