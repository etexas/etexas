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
 * The intersection status object element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class IntersectionStatusObject implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover a intersection status object.
     */
    public static final int NUM_BITS = 16;

    /**
     * Is manual control enabled.
     */
    private boolean manualControlEnabled = false;

    /**
     * Is stop time activated status.
     */
    private boolean stopTimeActivated = false;

    /**
     * Is failure flash.
     */
    private boolean failureFlash = false;

    /**
     * Is preempt active.
     */
    private boolean preemptActive = false;

    /**
     * Is signal priority active.
     */
    private boolean signalPriorityActive = false;

    /**
     * Is fixed time operation.
     */
    private boolean fixedTimeOperation = false;

    /**
     * Is traffic dependent operation.
     */
    private boolean trafficDependentOperation = false;

    /**
     * Is standby operation.
     */
    private boolean standbyOperation = false;

    /**
     * Is failure mode.
     */
    private boolean failureMode = false;

    /**
     * Is off.
     */
    private boolean off = false;

    /**
     * Is recent map message update.
     */
    private boolean recentMapMessageUpdate = false;

    /**
     * Is recent change in map assigned lane IDs used.
     */
    private boolean recentChangeInMapAssignedLaneIdsUsed = false;

    /**
     * Is no valid map available at this time.
     */
    private boolean noValidMapAvailableAtThisTime = false;

    /**
     * Is no valid spat available at this time.
     */
    private boolean noValidSpatAvailableAtThisTime = false;

    /**
     * A getter for manual control enabled status.
     * 
     * @return The manual control enabled status.
     */
    public boolean isManualControlEnabled() {

        return manualControlEnabled;
    }

    /**
     * A setter for the manual control enabled status.
     * 
     * @param manualControlEnabled The manual control enabled status to set.
     */
    public void setManualControlEnabled(boolean manualControlEnabled) {

        this.manualControlEnabled = manualControlEnabled;
    }

    /**
     * A getter for stop time activated status.
     * 
     * @return The stop time activated status.
     */
    public boolean isStopTimeActivated() {

        return stopTimeActivated;
    }

    /**
     * A setter for the stop time activated status.
     * 
     * @param stopTimeActivated The stop time activated status to set.
     */
    public void setStopTimeActivated(boolean stopTimeActivated) {

        this.stopTimeActivated = stopTimeActivated;
    }

    /**
     * A getter for failure flash status.
     * 
     * @return The failure flash status.
     */
    public boolean isFailureFlash() {

        return failureFlash;
    }

    /**
     * A setter for the failure flash status.
     * 
     * @param failureFlash The failure flash status to set.
     */
    public void setFailureFlash(boolean failureFlash) {

        this.failureFlash = failureFlash;
    }

    /**
     * A getter for preempt active status.
     * 
     * @return The preempt active status.
     */
    public boolean isPreemptActive() {

        return preemptActive;
    }

    /**
     * A setter for the preempt active status.
     * 
     * @param preemptActive The preempt active status to set.
     */
    public void setPreemptActive(boolean preemptActive) {

        this.preemptActive = preemptActive;
    }

    /**
     * A getter for signal priority active status.
     * 
     * @return The signal priority active status.
     */
    public boolean isSignalPriorityActive() {

        return signalPriorityActive;
    }

    /**
     * A setter for the signal priority active status.
     * 
     * @param signalPriorityActive The signal priority active status to set.
     */
    public void setSignalPriorityActive(boolean signalPriorityActive) {

        this.signalPriorityActive = signalPriorityActive;
    }

    /**
     * A getter for fixed time operation status.
     * 
     * @return The fixed time operation status.
     */
    public boolean isFixedTimeOperation() {

        return fixedTimeOperation;
    }

    /**
     * A setter for the fixed time operation status.
     * 
     * @param fixedTimeOperation The fixed time operation status to set.
     */
    public void setFixedTimeOperation(boolean fixedTimeOperation) {

        this.fixedTimeOperation = fixedTimeOperation;
    }

    /**
     * A getter for traffic dependent operation status.
     * 
     * @return The traffic dependent operation status.
     */
    public boolean isTrafficDependentOperation() {

        return trafficDependentOperation;
    }

    /**
     * A setter for the traffic dependent operation status.
     * 
     * @param trafficDependentOperation The traffic dependent operation status to set.
     */
    public void setTrafficDependentOperation(boolean trafficDependentOperation) {

        this.trafficDependentOperation = trafficDependentOperation;
    }

    /**
     * A getter for standby operation status.
     * 
     * @return The standby operation status.
     */
    public boolean isStandbyOperation() {

        return standbyOperation;
    }

    /**
     * A setter for the standby operation status.
     * 
     * @param standbyOperation The standby operation status to set.
     */
    public void setStandbyOperation(boolean standbyOperation) {

        this.standbyOperation = standbyOperation;
    }

    /**
     * A getter for failure mode status.
     * 
     * @return The failure mode status.
     */
    public boolean isFailureMode() {

        return failureMode;
    }

    /**
     * A setter for the failure mode status.
     * 
     * @param failureMode The failure mode status to set.
     */
    public void setFailureMode(boolean failureMode) {

        this.failureMode = failureMode;
    }

    /**
     * A getter for off status.
     * 
     * @return The off status.
     */
    public boolean isOff() {

        return off;
    }

    /**
     * A setter for the off status.
     * 
     * @param off The off status to set.
     */
    public void setOff(boolean off) {

        this.off = off;
    }

    /**
     * A getter for recent map message update status.
     * 
     * @return The recent map message update status.
     */
    public boolean isRecentMapMessageUpdate() {

        return recentMapMessageUpdate;
    }

    /**
     * A setter for the recent map message update status.
     * 
     * @param recentMapMessageUpdate The recent map message update status to set.
     */
    public void setRecentMapMessageUpdate(boolean recentMapMessageUpdate) {

        this.recentMapMessageUpdate = recentMapMessageUpdate;
    }

    /**
     * A getter for recent change in map assigned lane IDs used status.
     * 
     * @return The recent change in map assigned lane IDs used status.
     */
    public boolean isRecentChangeInMapAssignedLaneIdsUsed() {

        return recentChangeInMapAssignedLaneIdsUsed;
    }

    /**
     * A setter for the recent change in map assigned lane IDs used status.
     * 
     * @param recentChangeInMapAssignedLaneIdsUsed The recent change in map assigned lane IDs used
     *        status to set.
     */
    public void setRecentChangeInMapAssignedLaneIdsUsed(boolean recentChangeInMapAssignedLaneIdsUsed) {

        this.recentChangeInMapAssignedLaneIdsUsed = recentChangeInMapAssignedLaneIdsUsed;
    }

    /**
     * A getter for no valid map available at this time status.
     * 
     * @return The no valid map available at this time status.
     */
    public boolean isNoValidMapAvailableAtThisTime() {

        return noValidMapAvailableAtThisTime;
    }

    /**
     * A setter for the no valid map available at this time status.
     * 
     * @param noValidMapAvailableAtThisTime The no valid map available at this time status to set.
     */
    public void setNoValidMapAvailableAtThisTime(boolean noValidMapAvailableAtThisTime) {

        this.noValidMapAvailableAtThisTime = noValidMapAvailableAtThisTime;
    }

    /**
     * A getter for no valid spat available at this time status.
     * 
     * @return The no valid spat available at this time status.
     */
    public boolean isNoValidSpatAvailableAtThisTime() {

        return noValidSpatAvailableAtThisTime;
    }

    /**
     * A setter for the no valid spat available at this time status.
     * 
     * @param noValidSpatAvailableAtThisTime The no valid spat available at this time status to set.
     */
    public void setNoValidSpatAvailableAtThisTime(boolean noValidSpatAvailableAtThisTime) {

        this.noValidSpatAvailableAtThisTime = noValidSpatAvailableAtThisTime;
    }

    @Override
    public String encodeUPER() {

        StringBuilder intersectionStatusStr = new StringBuilder(16);

        intersectionStatusStr.append(manualControlEnabled ? '1' : '0');
        intersectionStatusStr.append(stopTimeActivated ? '1' : '0');
        intersectionStatusStr.append(failureFlash ? '1' : '0');
        intersectionStatusStr.append(preemptActive ? '1' : '0');
        intersectionStatusStr.append(signalPriorityActive ? '1' : '0');
        intersectionStatusStr.append(fixedTimeOperation ? '1' : '0');
        intersectionStatusStr.append(trafficDependentOperation ? '1' : '0');
        intersectionStatusStr.append(standbyOperation ? '1' : '0');
        intersectionStatusStr.append(failureMode ? '1' : '0');
        intersectionStatusStr.append(off ? '1' : '0');
        intersectionStatusStr.append(recentMapMessageUpdate ? '1' : '0');
        intersectionStatusStr.append(recentChangeInMapAssignedLaneIdsUsed ? '1' : '0');
        intersectionStatusStr.append(noValidMapAvailableAtThisTime ? '1' : '0');
        intersectionStatusStr.append(noValidSpatAvailableAtThisTime ? '1' : '0');
        intersectionStatusStr.append("00");

        return intersectionStatusStr.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (IntersectionStatusObject.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for an IntersectionStatusObject element (%d)", IntersectionStatusObject.NUM_BITS));
        }

        String intersectionStatusBits = bits.substring(0, IntersectionStatusObject.NUM_BITS);

        if (!"00".equals(intersectionStatusBits.substring(14, 16))) {

            throw new IllegalArgumentException("The bits 14 and 15 must be set to zero, because the reserved bits are not currently in use.");
        }

        manualControlEnabled = intersectionStatusBits.charAt(0) == '1';
        stopTimeActivated = intersectionStatusBits.charAt(1) == '1';
        failureFlash = intersectionStatusBits.charAt(2) == '1';
        preemptActive = intersectionStatusBits.charAt(3) == '1';
        signalPriorityActive = intersectionStatusBits.charAt(4) == '1';
        fixedTimeOperation = intersectionStatusBits.charAt(5) == '1';
        trafficDependentOperation = intersectionStatusBits.charAt(6) == '1';
        standbyOperation = intersectionStatusBits.charAt(7) == '1';
        failureMode = intersectionStatusBits.charAt(8) == '1';
        off = intersectionStatusBits.charAt(9) == '1';
        recentMapMessageUpdate = intersectionStatusBits.charAt(10) == '1';
        recentChangeInMapAssignedLaneIdsUsed = intersectionStatusBits.charAt(11) == '1';
        noValidMapAvailableAtThisTime = intersectionStatusBits.charAt(12) == '1';
        noValidSpatAvailableAtThisTime = intersectionStatusBits.charAt(13) == '1';

        return bits.substring(IntersectionStatusObject.NUM_BITS);
    }

    @Override
    public int hashCode() {

        return Objects.hash(manualControlEnabled, stopTimeActivated, failureFlash, preemptActive, signalPriorityActive, fixedTimeOperation, trafficDependentOperation, standbyOperation,
                failureMode, off, recentMapMessageUpdate, recentChangeInMapAssignedLaneIdsUsed, noValidMapAvailableAtThisTime, noValidSpatAvailableAtThisTime);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof IntersectionStatusObject)) {

            return false;
        }

        IntersectionStatusObject element = (IntersectionStatusObject)object;
        return this.manualControlEnabled == element.manualControlEnabled
                && this.stopTimeActivated == element.stopTimeActivated
                && this.failureFlash == element.failureFlash
                && this.preemptActive == element.preemptActive
                && this.signalPriorityActive == element.signalPriorityActive
                && this.fixedTimeOperation == element.fixedTimeOperation
                && this.trafficDependentOperation == element.trafficDependentOperation
                && this.standbyOperation == element.standbyOperation
                && this.failureMode == element.failureMode
                && this.off == element.off
                && this.recentMapMessageUpdate == element.recentMapMessageUpdate
                && this.recentChangeInMapAssignedLaneIdsUsed == element.recentChangeInMapAssignedLaneIdsUsed
                && this.noValidMapAvailableAtThisTime == element.noValidMapAvailableAtThisTime
                && this.noValidSpatAvailableAtThisTime == element.noValidSpatAvailableAtThisTime;
    }
}
