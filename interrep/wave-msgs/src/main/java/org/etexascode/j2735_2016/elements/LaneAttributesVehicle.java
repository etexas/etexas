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
 * The lane attributes vehicle element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class LaneAttributesVehicle implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover a lane attributes vehicle.
     */
    public static final int NUM_BITS = 9;

    /**
     * The vehicle revocable lane status.
     */
    private boolean vehicleRevocableLane = false;

    /**
     * The vehicle fly over lane status.
     */
    private boolean vehicleFlyOverLane = false;

    /**
     * The hov lane use only status.
     */
    private boolean hovLaneUseOnly = false;

    /**
     * The restricted to bus use status.
     */
    private boolean restrictedToBusUse = false;

    /**
     * The restricted to taxi use status.
     */
    private boolean restrictedToTaxiUse = false;

    /**
     * The restricted from public use status.
     */
    private boolean restrictedFromPublicUse = false;

    /**
     * The IR beacon coverage status.
     */
    private boolean iRbeaconCoverage = false;

    /**
     * The permission on request status.
     */
    private boolean permissionOnRequest = false;

    /**
     * A getter for the vehicle revocable lane status.
     * 
     * @return The vehicle revocable lane status.
     */
    public boolean isVehicleRevocableLane() {

        return vehicleRevocableLane;
    }

    /**
     * A setter for the vehicle revocable lane status.
     * 
     * @param vehicleRevocableLane The vehicle revocable lane status to set.
     */
    public void setVehicleRevocableLane(boolean vehicleRevocableLane) {

        this.vehicleRevocableLane = vehicleRevocableLane;
    }

    /**
     * A getter for the vehicle fly over lane status.
     * 
     * @return The vehicle fly over lane status.
     */
    public boolean isVehicleFlyOverLane() {

        return vehicleFlyOverLane;
    }

    /**
     * A setter for the vehicle fly over lane status.
     * 
     * @param vehicleFlyOverLane The vehicle fly over lane status to set.
     */
    public void setVehicleFlyOverLane(boolean vehicleFlyOverLane) {

        this.vehicleFlyOverLane = vehicleFlyOverLane;
    }

    /**
     * A getter for the hov lane use only status.
     * 
     * @return The hov lane use only status.
     */
    public boolean isHovLaneUseOnly() {

        return hovLaneUseOnly;
    }

    /**
     * A setter for the hov lane use only status.
     * 
     * @param hovLaneUseOnly The hov lane use only status to set.
     */
    public void setHovLaneUseOnly(boolean hovLaneUseOnly) {

        this.hovLaneUseOnly = hovLaneUseOnly;
    }

    /**
     * A getter for the restricted to bus use status.
     * 
     * @return The restricted to bus use status.
     */
    public boolean isRestrictedToBusUse() {

        return restrictedToBusUse;
    }

    /**
     * A setter for the restricted to bus use status.
     * 
     * @param restrictedToBusUse The restricted to bus use status to set.
     */
    public void setRestrictedToBusUse(boolean restrictedToBusUse) {

        this.restrictedToBusUse = restrictedToBusUse;
    }

    /**
     * A getter for the restricted to taxi use status.
     * 
     * @return The restricted to taxi use status.
     */
    public boolean isRestrictedToTaxiUse() {

        return restrictedToTaxiUse;
    }

    /**
     * A setter for the restricted to taxi use status.
     * 
     * @param restrictedToTaxiUse The restricted to taxi use status to set.
     */
    public void setRestrictedToTaxiUse(boolean restrictedToTaxiUse) {

        this.restrictedToTaxiUse = restrictedToTaxiUse;
    }

    /**
     * A getter for the restricted from public use status.
     * 
     * @return The restricted from public use status.
     */
    public boolean isRestrictedFromPublicUse() {

        return restrictedFromPublicUse;
    }

    /**
     * A setter for the restricted from public use status.
     * 
     * @param restrictedFromPublicUse The restricted from public use status to set.
     */
    public void setRestrictedFromPublicUse(boolean restrictedFromPublicUse) {

        this.restrictedFromPublicUse = restrictedFromPublicUse;
    }

    /**
     * A getter for the IR beacon coverage status.
     * 
     * @return The IR beacon coverage status.
     */
    public boolean hasIRbeaconCoverage() {

        return iRbeaconCoverage;
    }

    /**
     * A setter for the IR beacon coverage status.
     * 
     * @param iRbeaconCoverage The IR beacon coverage status to set.
     */
    public void setIRbeaconCoverage(boolean iRbeaconCoverage) {

        this.iRbeaconCoverage = iRbeaconCoverage;
    }

    /**
     * A getter for the permission on request status.
     * 
     * @return The permission on request status.
     */
    public boolean hasPermissionOnRequest() {

        return permissionOnRequest;
    }

    /**
     * A setter for the permission on request status.
     * 
     * @param permissionOnRequest The permission on request status to set.
     */
    public void setPermissionOnRequest(boolean permissionOnRequest) {

        this.permissionOnRequest = permissionOnRequest;
    }

    @Override
    public String encodeUPER() {

        StringBuilder laneAttributesVehicle = new StringBuilder(9);
        laneAttributesVehicle.append('0');
        laneAttributesVehicle.append(vehicleRevocableLane ? '1' : '0');
        laneAttributesVehicle.append(vehicleFlyOverLane ? '1' : '0');
        laneAttributesVehicle.append(hovLaneUseOnly ? '1' : '0');
        laneAttributesVehicle.append(restrictedToBusUse ? '1' : '0');
        laneAttributesVehicle.append(restrictedToTaxiUse ? '1' : '0');
        laneAttributesVehicle.append(restrictedFromPublicUse ? '1' : '0');
        laneAttributesVehicle.append(iRbeaconCoverage ? '1' : '0');
        laneAttributesVehicle.append(permissionOnRequest ? '1' : '0');

        return laneAttributesVehicle.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (LaneAttributesVehicle.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a LaneAttributesVehicle element (%d)", LaneAttributesVehicle.NUM_BITS));
        }

        String laneAttributesVehicleBits = bits.substring(0, LaneAttributesVehicle.NUM_BITS);

        if (laneAttributesVehicleBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The LaneAttributesVehicle extension is not supported");
        }

        vehicleRevocableLane = laneAttributesVehicleBits.charAt(1) == '1';
        vehicleFlyOverLane = laneAttributesVehicleBits.charAt(2) == '1';
        hovLaneUseOnly = laneAttributesVehicleBits.charAt(3) == '1';
        restrictedToBusUse = laneAttributesVehicleBits.charAt(4) == '1';
        restrictedToTaxiUse = laneAttributesVehicleBits.charAt(5) == '1';
        restrictedFromPublicUse = laneAttributesVehicleBits.charAt(6) == '1';
        iRbeaconCoverage = laneAttributesVehicleBits.charAt(7) == '1';
        permissionOnRequest = laneAttributesVehicleBits.charAt(8) == '1';

        return bits.substring(LaneAttributesVehicle.NUM_BITS);
    }

    @Override
    public int hashCode() {

        return Objects.hash(vehicleRevocableLane, vehicleFlyOverLane, hovLaneUseOnly, restrictedToBusUse, restrictedToTaxiUse, restrictedFromPublicUse, iRbeaconCoverage, permissionOnRequest);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof LaneAttributesVehicle)) {

            return false;
        }
        LaneAttributesVehicle element = (LaneAttributesVehicle)object;
        return this.vehicleRevocableLane == element.vehicleRevocableLane
                && this.vehicleFlyOverLane == element.vehicleFlyOverLane
                && this.hovLaneUseOnly == element.hovLaneUseOnly
                && this.restrictedToBusUse == element.restrictedToBusUse
                && this.restrictedToTaxiUse == element.restrictedToTaxiUse
                && this.restrictedFromPublicUse == element.restrictedFromPublicUse
                && this.iRbeaconCoverage == element.iRbeaconCoverage
                && this.permissionOnRequest == element.permissionOnRequest;
    }
}
