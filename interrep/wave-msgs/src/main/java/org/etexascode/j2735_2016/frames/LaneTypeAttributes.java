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
package org.etexascode.j2735_2016.frames;

import java.util.Objects;

import org.etexascode.j2735_2016.elements.LaneAttributesBarrier;
import org.etexascode.j2735_2016.elements.LaneAttributesBike;
import org.etexascode.j2735_2016.elements.LaneAttributesCrosswalk;
import org.etexascode.j2735_2016.elements.LaneAttributesParking;
import org.etexascode.j2735_2016.elements.LaneAttributesSidewalk;
import org.etexascode.j2735_2016.elements.LaneAttributesStriping;
import org.etexascode.j2735_2016.elements.LaneAttributesTrackedVehicle;
import org.etexascode.j2735_2016.elements.LaneAttributesVehicle;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;
import org.etexascode.j2735_2016.util.UPERInteger;

/**
 * The lane type attributes frame (Choice) for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class LaneTypeAttributes implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the choice portion of the lane type attributes.
     */
    public static final int NUM_BITS = 4;

    /**
     * The lane attributes vehicle frame.
     */
    private LaneAttributesVehicle vehicle;

    /**
     * The lane attributes crosswalk frame.
     */
    private LaneAttributesCrosswalk crosswalk;

    /**
     * The lane attributes bike frame.
     */
    private LaneAttributesBike bikeLane;

    /**
     * The lane attributes sidewalk frame.
     */
    private LaneAttributesSidewalk sidewalk;

    /**
     * The lane attributes barrier frame.
     */
    private LaneAttributesBarrier median;

    /**
     * The lane attributes striping frame.
     */
    private LaneAttributesStriping striping;

    /**
     * The lane attributes tracked vehicle frame.
     */
    private LaneAttributesTrackedVehicle trackedVehicle;

    /**
     * The lane attributes parking frame.
     */
    private LaneAttributesParking parking;

    /**
     * A constructor setup only for decoding purposes.
     */
    public LaneTypeAttributes() {

        vehicle = null;
        crosswalk = null;
        bikeLane = null;
        sidewalk = null;
        median = null;
        striping = null;
        trackedVehicle = null;
        parking = null;
    }

    /**
     * A constructor for the lane type attributes frame for the lane attributes vehicle choice.
     * 
     * @param vehicle The lane attributes vehicle frame.
     */
    public LaneTypeAttributes(LaneAttributesVehicle vehicle) {

        this.vehicle = Objects.requireNonNull(vehicle);
    }

    /**
     * A constructor for the lane type attributes frame for the lane attributes crosswalk choice.
     * 
     * @param crosswalk The lane attributes crosswalk frame.
     */
    public LaneTypeAttributes(LaneAttributesCrosswalk crosswalk) {

        this.crosswalk = Objects.requireNonNull(crosswalk);
    }

    /**
     * A constructor for the lane type attributes frame for the lane attributes bike choice.
     * 
     * @param bikeLane The lane attributes bike frame.
     */
    public LaneTypeAttributes(LaneAttributesBike bikeLane) {

        this.bikeLane = Objects.requireNonNull(bikeLane);
    }

    /**
     * A constructor for the lane type attributes frame for the lane attributes sidewalk choice.
     * 
     * @param sidewalk The lane attributes sidewalk frame.
     */
    public LaneTypeAttributes(LaneAttributesSidewalk sidewalk) {

        this.sidewalk = Objects.requireNonNull(sidewalk);
    }

    /**
     * A constructor for the lane type attributes frame for the lane attributes barrier choice.
     * 
     * @param median The lane attributes barrier frame.
     */
    public LaneTypeAttributes(LaneAttributesBarrier median) {

        this.median = Objects.requireNonNull(median);
    }

    /**
     * A constructor for the lane type attributes frame for the lane attributes striping choice.
     * 
     * @param striping The lane attributes striping frame.
     */
    public LaneTypeAttributes(LaneAttributesStriping striping) {

        this.striping = Objects.requireNonNull(striping);
    }

    /**
     * A constructor for the lane type attributes frame for the lane attributes tracked vehicle
     * choice.
     * 
     * @param trackedVehicle The lane attributes tracked vehicle frame.
     */
    public LaneTypeAttributes(LaneAttributesTrackedVehicle trackedVehicle) {

        this.trackedVehicle = Objects.requireNonNull(trackedVehicle);
    }

    /**
     * A constructor for the lane type attributes frame for the lane attributes parking choice.
     * 
     * @param parking The lane attributes parking frame.
     */
    public LaneTypeAttributes(LaneAttributesParking parking) {

        this.parking = Objects.requireNonNull(parking);
    }

    /**
     * A getter for the lane attributes vehicle frame.
     * 
     * @return The lane attributes vehicle frame.
     */
    public LaneAttributesVehicle getVehicle() {

        return vehicle;
    }

    /**
     * A getter for the lane attributes crosswalk frame.
     * 
     * @return The lane attributes crosswalk frame.
     */
    public LaneAttributesCrosswalk getCrosswalk() {

        return crosswalk;
    }

    /**
     * A getter for the lane attributes bike frame.
     * 
     * @return The lane attributes bike frame.
     */
    public LaneAttributesBike getBikeLane() {

        return bikeLane;
    }

    /**
     * A getter for the lane attributes sidewalk frame.
     * 
     * @return The lane attributes sidewalk frame.
     */
    public LaneAttributesSidewalk getSidewalk() {

        return sidewalk;
    }

    /**
     * A getter for the lane attributes barrier frame.
     * 
     * @return The lane attributes barrier frame.
     */
    public LaneAttributesBarrier getMedian() {

        return median;
    }

    /**
     * A getter for the lane attributes striping frame.
     * 
     * @return The lane attributes striping frame.
     */
    public LaneAttributesStriping getStriping() {

        return striping;
    }

    /**
     * A getter for the lane attributes trackedvehicle frame.
     * 
     * @return The lane attributes tracked vehicle frame.
     */
    public LaneAttributesTrackedVehicle getTrackedVehicle() {

        return trackedVehicle;
    }

    /**
     * A getter for the lane attributes parking frame.
     * 
     * @return The lane attributes parking frame.
     */
    public LaneAttributesParking getParking() {

        return parking;
    }

    @Override
    public String encodeUPER() {

        StringBuilder laneTypeAttributes = new StringBuilder();
        laneTypeAttributes.append('0');

        if (vehicle != null) {

            laneTypeAttributes.append(UPERInteger.encode(0, 0, LaneTypeAttributes.NUM_BITS - 1));
            laneTypeAttributes.append(vehicle.encodeUPER());
        }
        else if (crosswalk != null) {

            laneTypeAttributes.append(UPERInteger.encode(1, 0, LaneTypeAttributes.NUM_BITS - 1));
            laneTypeAttributes.append(crosswalk.encodeUPER());
        }

        else if (bikeLane != null) {

            laneTypeAttributes.append(UPERInteger.encode(2, 0, LaneTypeAttributes.NUM_BITS - 1));
            laneTypeAttributes.append(bikeLane.encodeUPER());
        }

        else if (sidewalk != null) {

            laneTypeAttributes.append(UPERInteger.encode(3, 0, LaneTypeAttributes.NUM_BITS - 1));
            laneTypeAttributes.append(sidewalk.encodeUPER());
        }

        else if (median != null) {

            laneTypeAttributes.append(UPERInteger.encode(4, 0, LaneTypeAttributes.NUM_BITS - 1));
            laneTypeAttributes.append(median.encodeUPER());
        }

        else if (striping != null) {

            laneTypeAttributes.append(UPERInteger.encode(5, 0, LaneTypeAttributes.NUM_BITS - 1));
            laneTypeAttributes.append(striping.encodeUPER());
        }

        else if (trackedVehicle != null) {

            laneTypeAttributes.append(UPERInteger.encode(6, 0, LaneTypeAttributes.NUM_BITS - 1));
            laneTypeAttributes.append(trackedVehicle.encodeUPER());
        }

        else if (parking != null) {

            laneTypeAttributes.append(UPERInteger.encode(7, 0, LaneTypeAttributes.NUM_BITS - 1));
            laneTypeAttributes.append(parking.encodeUPER());
        }
        else {

            throw new IllegalStateException("None of the instance variables were initialized.");
        }

        return laneTypeAttributes.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (LaneTypeAttributes.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a LaneTypeAttributes frame (%d)", LaneTypeAttributes.NUM_BITS));
        }

        String laneTypeAttributesChoiceBits = bits.substring(0, LaneTypeAttributes.NUM_BITS);

        if (laneTypeAttributesChoiceBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The LaneTypeAttributes extension is not supported");
        }

        bits = bits.substring(LaneTypeAttributes.NUM_BITS);

        int choice = UPERInteger.decode(laneTypeAttributesChoiceBits.substring(1), 0);

        switch (choice) {

            case 0:
                vehicle = new LaneAttributesVehicle();
                bits = vehicle.decodeUPER(bits);
                break;
            case 1:
                crosswalk = new LaneAttributesCrosswalk();
                bits = crosswalk.decodeUPER(bits);
                break;
            case 2:
                bikeLane = new LaneAttributesBike();
                bits = bikeLane.decodeUPER(bits);
                break;
            case 3:
                sidewalk = new LaneAttributesSidewalk();
                bits = sidewalk.decodeUPER(bits);
                break;
            case 4:
                median = new LaneAttributesBarrier();
                bits = median.decodeUPER(bits);
                break;
            case 5:
                striping = new LaneAttributesStriping();
                bits = striping.decodeUPER(bits);
                break;
            case 6:
                trackedVehicle = new LaneAttributesTrackedVehicle();
                bits = trackedVehicle.decodeUPER(bits);
                break;
            case 7:
                parking = new LaneAttributesParking();
                bits = parking.decodeUPER(bits);
                break;
            default:
                throw new IllegalArgumentException(String.format("There should not be any more possible integers that would be returned, but received %d", choice));
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(vehicle, crosswalk, bikeLane, sidewalk, median, striping, trackedVehicle, parking);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof LaneTypeAttributes)) {

            return false;
        }
        LaneTypeAttributes frame = (LaneTypeAttributes)object;
        return Objects.equals(this.vehicle, frame.vehicle)
                && Objects.equals(this.crosswalk, frame.crosswalk)
                && Objects.equals(this.bikeLane, frame.bikeLane)
                && Objects.equals(this.sidewalk, frame.sidewalk)
                && Objects.equals(this.median, frame.median)
                && Objects.equals(this.striping, frame.striping)
                && Objects.equals(this.trackedVehicle, frame.trackedVehicle)
                && Objects.equals(this.parking, frame.parking);
    }
}
