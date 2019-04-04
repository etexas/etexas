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

import org.etexascode.j2735_2016.elements.DeltaAngle;
import org.etexascode.j2735_2016.elements.MergeDivergeNodeAngle;
import org.etexascode.j2735_2016.elements.RoadwayCrownAngle;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;
import org.etexascode.j2735_2016.util.UPERInteger;

/**
 * The lane data attribute frame (Choice) for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class LaneDataAttribute implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the choice portion of the lane data attribute.
     */
    public static final int NUM_BITS = 4;

    /**
     * The delta angle element.
     */
    private DeltaAngle pathEndPointAngle;

    /**
     * The lane crown point center roadway crown angle element.
     */
    private RoadwayCrownAngle laneCrownPointCenter;

    /**
     * The lane crown point left roadway crown angle element.
     */
    private RoadwayCrownAngle laneCrownPointLeft;

    /**
     * The lane crown point right roadway crown angle element.
     */
    private RoadwayCrownAngle laneCrownPointRight;

    /**
     * The merge diverge node angle element.
     */
    private MergeDivergeNodeAngle laneAngle;

    /**
     * The speed limit list frame.
     */
    private SpeedLimitList speedLimits;

    /**
     * A constructor setup only for decoding purposes.
     */
    public LaneDataAttribute() {

        pathEndPointAngle = null;
        laneCrownPointCenter = null;
        laneCrownPointLeft = null;
        laneCrownPointRight = null;
        laneAngle = null;
        speedLimits = null;
    }

    /**
     * A constructor for the lane data attribute frame for the delta angle choice.
     * 
     * @param pathEndPointAngle The delta angle element.
     */
    public LaneDataAttribute(DeltaAngle pathEndPointAngle) {

        this.pathEndPointAngle = Objects.requireNonNull(pathEndPointAngle);
    }

    /**
     * A constructor for the lane data attribute frame for the roadway crown angle choice depicted
     * by an integer.
     * 
     * @param roadwayCrownAngle The roadway crown angle element.
     * @param choice The integer which depicts the roadway crown angle element to use. 0 =
     *        laneCrownPointCenter, 1 = laneCrownPointLeft, 2 = laneCrownPointRight.
     */
    public LaneDataAttribute(RoadwayCrownAngle laneCrownPoint, int choice) {

        Objects.requireNonNull(laneCrownPoint);

        switch (choice) {

            case 0:
                this.laneCrownPointCenter = laneCrownPoint;
                break;
            case 1:
                this.laneCrownPointLeft = laneCrownPoint;
                break;
            case 2:
                this.laneCrownPointRight = laneCrownPoint;
                break;
            default:
                throw new IllegalArgumentException("Invalid choice, the choice must be either 0 (laneCrownPointCenter), 1 (laneCrownPointLeft), or 2(laneCrownPointRight).");
        }
    }

    /**
     * A constructor for the lane data attribute frame for the merge diverge node angle choice.
     * 
     * @param laneAngle The merge diverge node angle element.
     */
    public LaneDataAttribute(MergeDivergeNodeAngle laneAngle) {

        this.laneAngle = Objects.requireNonNull(laneAngle);
    }

    /**
     * A constructor for the lane data attribute frame for the speed limits choice.
     * 
     * @param speedLimits The delta angle element.
     */
    public LaneDataAttribute(SpeedLimitList speedLimits) {

        this.speedLimits = Objects.requireNonNull(speedLimits);
    }

    /**
     * A getter for the delta angle element.
     * 
     * @return The delta angle element.
     */
    public DeltaAngle getPathEndPointAngle() {

        return pathEndPointAngle;
    }

    /**
     * A getter for the lane crown point center roadway crown angle element.
     * 
     * @return The lane crown point center roadway crown angle element.
     */
    public RoadwayCrownAngle getLaneCrownPointCenter() {

        return laneCrownPointCenter;
    }

    /**
     * A getter for the lane crown point left roadway crown angle element.
     * 
     * @return The lane crown point left roadway crown angle element.
     */
    public RoadwayCrownAngle getLaneCrownPointLeft() {

        return laneCrownPointLeft;
    }

    /**
     * A getter for the lane crown point right roadway crown angle element.
     * 
     * @return The lane crown point right roadway crown angle element.
     */
    public RoadwayCrownAngle getLaneCrownPointRight() {

        return laneCrownPointRight;
    }

    /**
     * A getter for the merge diverge node angle element.
     * 
     * @return The merge diverge node angle.
     */
    public MergeDivergeNodeAngle getLaneAngle() {

        return laneAngle;
    }

    /**
     * A getter for the speed limit list frame.
     * 
     * @return The speed limit list frame.
     */
    public SpeedLimitList getSpeedLimits() {

        return speedLimits;
    }

    @Override
    public String encodeUPER() {

        StringBuilder laneDataAttributeBits = new StringBuilder();
        laneDataAttributeBits.append('0');

        if (pathEndPointAngle != null) {

            laneDataAttributeBits.append(UPERInteger.encode(0, 0, LaneDataAttribute.NUM_BITS - 1));
            laneDataAttributeBits.append(pathEndPointAngle.encodeUPER());
        }
        else if (laneCrownPointCenter != null) {

            laneDataAttributeBits.append(UPERInteger.encode(1, 0, LaneDataAttribute.NUM_BITS - 1));
            laneDataAttributeBits.append(laneCrownPointCenter.encodeUPER());
        }
        else if (laneCrownPointLeft != null) {

            laneDataAttributeBits.append(UPERInteger.encode(2, 0, LaneDataAttribute.NUM_BITS - 1));
            laneDataAttributeBits.append(laneCrownPointLeft.encodeUPER());
        }
        else if (laneCrownPointRight != null) {

            laneDataAttributeBits.append(UPERInteger.encode(3, 0, LaneDataAttribute.NUM_BITS - 1));
            laneDataAttributeBits.append(laneCrownPointRight.encodeUPER());
        }
        else if (laneAngle != null) {

            laneDataAttributeBits.append(UPERInteger.encode(4, 0, LaneDataAttribute.NUM_BITS - 1));
            laneDataAttributeBits.append(laneAngle.encodeUPER());
        }
        else if (speedLimits != null) {

            laneDataAttributeBits.append(UPERInteger.encode(5, 0, LaneDataAttribute.NUM_BITS - 1));
            laneDataAttributeBits.append(speedLimits.encodeUPER());
        }
        else {

            throw new IllegalStateException("None of the instance variables were initialized.");
        }

        return laneDataAttributeBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (LaneDataAttribute.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a LaneDataAttribute frame (%d)", LaneDataAttribute.NUM_BITS));
        }

        String laneDataAttributeChoiceBits = bits.substring(0, LaneDataAttribute.NUM_BITS);

        if (laneDataAttributeChoiceBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The LaneDataAttribute extension is not supported");
        }

        bits = bits.substring(LaneDataAttribute.NUM_BITS);

        int choice = UPERInteger.decode(laneDataAttributeChoiceBits.substring(1), 0);

        switch (choice) {

            case 0:
                pathEndPointAngle = new DeltaAngle();
                bits = pathEndPointAngle.decodeUPER(bits);
                break;
            case 1:
                laneCrownPointCenter = new RoadwayCrownAngle();
                bits = laneCrownPointCenter.decodeUPER(bits);
                break;
            case 2:
                laneCrownPointLeft = new RoadwayCrownAngle();
                bits = laneCrownPointLeft.decodeUPER(bits);
                break;
            case 3:
                laneCrownPointRight = new RoadwayCrownAngle();
                bits = laneCrownPointRight.decodeUPER(bits);
                break;
            case 4:
                laneAngle = new MergeDivergeNodeAngle();
                bits = laneAngle.decodeUPER(bits);
                break;
            case 5:
                speedLimits = new SpeedLimitList();
                bits = speedLimits.decodeUPER(bits);
                break;
            case 6:
                throw new IllegalArgumentException("The LaneDataAttribute regional extension is not supported");
            default:
                throw new IllegalArgumentException(String.format("The bits supplied do not connect with any known frame or element, received %d", choice));
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(pathEndPointAngle, laneCrownPointCenter, laneCrownPointLeft, laneCrownPointRight, laneAngle, speedLimits);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof LaneDataAttribute)) {

            return false;
        }
        LaneDataAttribute frame = (LaneDataAttribute)object;
        return Objects.equals(this.pathEndPointAngle, frame.pathEndPointAngle)
                && Objects.equals(this.laneCrownPointCenter, frame.laneCrownPointCenter)
                && Objects.equals(this.laneCrownPointLeft, frame.laneCrownPointLeft)
                && Objects.equals(this.laneCrownPointRight, frame.laneCrownPointRight)
                && Objects.equals(this.laneAngle, frame.laneAngle)
                && Objects.equals(this.speedLimits, frame.speedLimits);
    }
}
