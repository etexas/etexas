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
 * The lane sharing element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class LaneSharing implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover a lane sharing.
     */
    public static final int NUM_BITS = 10;

    /**
     * The overlapping lane description provided status.
     */
    private boolean overlappingLaneDescriptionProvided = false;

    /**
     * The multiple lanes treated as one lane status.
     */
    private boolean multipleLanesTreatedAsOneLane = false;

    /**
     * The other non motorized traffic types status.
     */
    private boolean otherNonMotorizedTrafficTypes = false;

    /**
     * The individual motorized vehicle traffic status.
     */
    private boolean individualMotorizedVehicleTraffic = false;

    /**
     * The bus vehicle traffic status.
     */
    private boolean busVehicleTraffic = false;

    /**
     * The taxi vehicle traffic status.
     */
    private boolean taxiVehicleTraffic = false;

    /**
     * The pedestrians traffic status.
     */
    private boolean pedestriansTraffic = false;

    /**
     * The cyclist vehicle traffic status.
     */
    private boolean cyclistVehicleTraffic = false;

    /**
     * The tracked vehicle traffic status.
     */
    private boolean trackedVehicleTraffic = false;

    /**
     * The pedestrian traffic status.
     */
    private boolean pedestrianTraffic = false;

    /**
     * A getter for the overlapping lane description provided status.
     * 
     * @return The overlapping lane description provided status.
     */
    public boolean isOverlappingLaneDescriptionProvided() {

        return overlappingLaneDescriptionProvided;
    }

    /**
     * A setter for the overlapping lane description provided status.
     * 
     * @param overlappingLaneDescriptionProvided The overlapping lane description provided status to
     *        set.
     */
    public void setOverlappingLaneDescriptionProvided(boolean overlappingLaneDescriptionProvided) {

        this.overlappingLaneDescriptionProvided = overlappingLaneDescriptionProvided;
    }

    /**
     * A getter for the multiple lanes treated as one lane status.
     * 
     * @return The multiple lanes treated as one lane status.
     */
    public boolean isMultipleLanesTreatedAsOneLane() {

        return multipleLanesTreatedAsOneLane;
    }

    /**
     * A setter for the multiple lanes treated as one lane status.
     * 
     * @param multipleLanesTreatedAsOneLane The multiple lanes treated as one lane status to set.
     */
    public void setMultipleLanesTreatedAsOneLaneStatus(boolean multipleLanesTreatedAsOneLane) {

        this.multipleLanesTreatedAsOneLane = multipleLanesTreatedAsOneLane;
    }

    /**
     * A getter for the other non motorized traffic types status.
     * 
     * @return The other non motorized traffic types status.
     */
    public boolean isOtherNonMotorizedTrafficTypes() {

        return otherNonMotorizedTrafficTypes;
    }

    /**
     * A setter for the other non motorized traffic types status.
     * 
     * @param otherNonMotorizedTrafficTypes The other non motorized traffic types status to set.
     */
    public void setOtherNonMotorizedTrafficTypes(boolean otherNonMotorizedTrafficTypes) {

        this.otherNonMotorizedTrafficTypes = otherNonMotorizedTrafficTypes;
    }

    /**
     * A getter for the individual motorized vehicle traffic status.
     * 
     * @return The individual motorized vehicle traffic status.
     */
    public boolean isIndividualMotorizedVehicleTraffic() {

        return individualMotorizedVehicleTraffic;
    }

    /**
     * A setter for the individual motorized vehicle traffic status.
     * 
     * @param individualMotorizedVehicleTraffic The individual motorized vehicle traffic status to
     *        set.
     */
    public void setIndividualMotorizedVehicleTraffic(boolean individualMotorizedVehicleTraffic) {

        this.individualMotorizedVehicleTraffic = individualMotorizedVehicleTraffic;
    }

    /**
     * A getter for the bus vehicle traffic status.
     * 
     * @return The bus vehicle traffic status.
     */
    public boolean isBusVehicleTraffic() {

        return busVehicleTraffic;
    }

    /**
     * A setter for the bus vehicle traffic status.
     * 
     * @param busVehicleTraffic The bus vehicle traffic status to set.
     */
    public void setBusVehicleTraffic(boolean busVehicleTraffic) {

        this.busVehicleTraffic = busVehicleTraffic;
    }

    /**
     * A getter for the taxi vehicle traffic status.
     * 
     * @return The taxi vehicle traffic status.
     */
    public boolean isTaxiVehicleTraffic() {

        return taxiVehicleTraffic;
    }

    /**
     * A setter for the taxi vehicle traffic status.
     * 
     * @param taxiVehicleTraffic The taxi vehicle traffic status to set.
     */
    public void setTaxiVehicleTraffic(boolean taxiVehicleTraffic) {

        this.taxiVehicleTraffic = taxiVehicleTraffic;
    }

    /**
     * A getter for the pedestrians traffic status.
     * 
     * @return The pedestrians traffic status.
     */
    public boolean isPedestriansTraffic() {

        return pedestriansTraffic;
    }

    /**
     * A setter for the pedestrians traffic status.
     * 
     * @param pedestriansTraffic The pedestrians traffic status to set.
     */
    public void setPedestriansTraffic(boolean pedestriansTraffic) {

        this.pedestriansTraffic = pedestriansTraffic;
    }

    /**
     * A getter for the cyclist vehicle traffic status.
     * 
     * @return The cyclist vehicle traffic status.
     */
    public boolean isCyclistVehicleTraffic() {

        return cyclistVehicleTraffic;
    }

    /**
     * A setter for the cyclist vehicle traffic status.
     * 
     * @param cyclistVehicleTraffic The cyclist vehicle traffic status to set.
     */
    public void setCyclistVehicleTraffic(boolean cyclistVehicleTraffic) {

        this.cyclistVehicleTraffic = cyclistVehicleTraffic;
    }

    /**
     * A getter for the tracked vehicle traffic status.
     * 
     * @return The tracked vehicle traffic status.
     */
    public boolean isTrackedVehicleTraffic() {

        return trackedVehicleTraffic;
    }

    /**
     * A setter for the tracked vehicle traffic status.
     * 
     * @param trackedVehicleTraffic The tracked vehicle traffic status to set.
     */
    public void setTrackedVehicleTraffic(boolean trackedVehicleTraffic) {

        this.trackedVehicleTraffic = trackedVehicleTraffic;
    }

    /**
     * A getter for the pedestrian traffic status.
     * 
     * @return The pedestrian traffic status.
     */
    public boolean isPedestrianTraffic() {

        return pedestrianTraffic;
    }

    /**
     * A setter for the pedestrian traffic status.
     * 
     * @param pedestrianTraffic The pedestrian traffic status to set.
     */
    public void setPedestrianTraffic(boolean pedestrianTraffic) {

        this.pedestrianTraffic = pedestrianTraffic;
    }

    @Override
    public String encodeUPER() {

        StringBuilder laneSharing = new StringBuilder(10);
        laneSharing.append(overlappingLaneDescriptionProvided ? '1' : '0');
        laneSharing.append(multipleLanesTreatedAsOneLane ? '1' : '0');
        laneSharing.append(otherNonMotorizedTrafficTypes ? '1' : '0');
        laneSharing.append(individualMotorizedVehicleTraffic ? '1' : '0');
        laneSharing.append(busVehicleTraffic ? '1' : '0');
        laneSharing.append(taxiVehicleTraffic ? '1' : '0');
        laneSharing.append(pedestriansTraffic ? '1' : '0');
        laneSharing.append(cyclistVehicleTraffic ? '1' : '0');
        laneSharing.append(trackedVehicleTraffic ? '1' : '0');
        laneSharing.append(pedestrianTraffic ? '1' : '0');

        return laneSharing.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (LaneSharing.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a LaneSharing element (%d)", LaneSharing.NUM_BITS));
        }

        String laneSharingBits = bits.substring(0, LaneSharing.NUM_BITS);

        overlappingLaneDescriptionProvided = laneSharingBits.charAt(0) == '1';
        multipleLanesTreatedAsOneLane = laneSharingBits.charAt(1) == '1';
        otherNonMotorizedTrafficTypes = laneSharingBits.charAt(2) == '1';
        individualMotorizedVehicleTraffic = laneSharingBits.charAt(3) == '1';
        busVehicleTraffic = laneSharingBits.charAt(4) == '1';
        taxiVehicleTraffic = laneSharingBits.charAt(5) == '1';
        pedestriansTraffic = laneSharingBits.charAt(6) == '1';
        cyclistVehicleTraffic = laneSharingBits.charAt(7) == '1';
        trackedVehicleTraffic = laneSharingBits.charAt(8) == '1';
        pedestrianTraffic = laneSharingBits.charAt(9) == '1';

        return bits.substring(LaneSharing.NUM_BITS);
    }

    @Override
    public int hashCode() {

        return Objects.hash(overlappingLaneDescriptionProvided, multipleLanesTreatedAsOneLane, otherNonMotorizedTrafficTypes, individualMotorizedVehicleTraffic, busVehicleTraffic,
                taxiVehicleTraffic, pedestriansTraffic, cyclistVehicleTraffic, trackedVehicleTraffic, pedestrianTraffic);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof LaneSharing)) {

            return false;
        }
        LaneSharing element = (LaneSharing)object;
        return this.overlappingLaneDescriptionProvided == element.overlappingLaneDescriptionProvided
                && this.multipleLanesTreatedAsOneLane == element.multipleLanesTreatedAsOneLane
                && this.otherNonMotorizedTrafficTypes == element.otherNonMotorizedTrafficTypes
                && this.individualMotorizedVehicleTraffic == element.individualMotorizedVehicleTraffic
                && this.busVehicleTraffic == element.busVehicleTraffic
                && this.taxiVehicleTraffic == element.taxiVehicleTraffic
                && this.pedestriansTraffic == element.pedestriansTraffic
                && this.cyclistVehicleTraffic == element.cyclistVehicleTraffic
                && this.trackedVehicleTraffic == element.trackedVehicleTraffic
                && this.pedestrianTraffic == element.pedestrianTraffic;
    }
}
