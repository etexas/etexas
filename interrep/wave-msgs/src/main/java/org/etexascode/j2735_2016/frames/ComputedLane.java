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

import org.etexascode.j2735_2016.elements.Angle;
import org.etexascode.j2735_2016.elements.LaneID;
import org.etexascode.j2735_2016.elements.ScaleB12;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The computed lane frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class ComputedLane implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the computed lane.
     */
    public static final int NUM_BITS = 5;

    /**
     * The lane ID element.
     */
    private LaneID referenceLaneId;

    /**
     * The computed lane offset x axis frame.
     */
    private ComputedLaneOffsetXAxis offsetXAxis;

    /**
     * The computed lane offset y axis frame.
     */
    private ComputedLaneOffsetYAxis offsetYAxis;

    /**
     * The angle element. (OPTIONAL)
     */
    private Angle rotateXY;

    /**
     * The x axis scale b12 element. (OPTIONAL)
     */
    private ScaleB12 scaleXAxis;

    /**
     * The y axis scale b12 element. (OPTIONAL)
     */
    private ScaleB12 scaleYAxis;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public ComputedLane() {

        referenceLaneId = new LaneID();
        offsetXAxis = new ComputedLaneOffsetXAxis();
        offsetYAxis = new ComputedLaneOffsetYAxis();
    }

    /**
     * A constructor for the computed lane frame for all required fields.
     * 
     * @param referenceLaneId The lane ID element.
     * @param offsetXAxis The computed lane offset x axis frame.
     * @param offsetYAxis The computed lane offset y axis frame.
     */
    public ComputedLane(LaneID referenceLaneId, ComputedLaneOffsetXAxis offsetXAxis, ComputedLaneOffsetYAxis offsetYAxis) {

        this.referenceLaneId = Objects.requireNonNull(referenceLaneId);
        this.offsetXAxis = Objects.requireNonNull(offsetXAxis);
        this.offsetYAxis = Objects.requireNonNull(offsetYAxis);
    }

    /**
     * A constructor for the intersection reference ID frame for all required fields (primitive).
     * 
     * @param referenceLaneId The lane ID value.
     * @param offsetXAxis The computed lane offset x axis frame.
     * @param offsetYAxis The computed lane offset y axis frame.
     */
    public ComputedLane(int referenceLaneId, ComputedLaneOffsetXAxis offsetXAxis, ComputedLaneOffsetYAxis offsetYAxis) {

        this.referenceLaneId = new LaneID(referenceLaneId);
        this.offsetXAxis = Objects.requireNonNull(offsetXAxis);
        this.offsetYAxis = Objects.requireNonNull(offsetYAxis);
    }

    /**
     * A getter for the lane ID element.
     * 
     * @return The lane ID element.
     */
    public LaneID getReferenceLaneId() {

        return referenceLaneId;
    }

    /**
     * A setter for the lane ID element.
     * 
     * @param referenceLaneId The lane ID element to set.
     */
    public void setReferenceLaneId(LaneID referenceLaneId) {

        this.referenceLaneId = Objects.requireNonNull(referenceLaneId);
    }

    /**
     * A setter for the lane ID element.
     * 
     * @param referenceLaneId The lane ID value to set.
     */
    public void setReferenceLaneId(int referenceLaneId) {

        this.referenceLaneId.setValue(referenceLaneId);
    }

    /**
     * A getter for the computed lane offset x axis frame.
     * 
     * @return The computed lane offset x axis frame.
     */
    public ComputedLaneOffsetXAxis getOffsetXAxis() {

        return offsetXAxis;
    }

    /**
     * A setter for the computed lane offset x axis frame.
     * 
     * @param offsetXAxis The computed lane offset x axis frame.
     */
    public void setOffsetXAxis(ComputedLaneOffsetXAxis offsetXAxis) {

        this.offsetXAxis = Objects.requireNonNull(offsetXAxis);
    }

    /**
     * A getter for the computed lane offset y axis frame.
     * 
     * @return The computed lane offset y axis frame.
     */
    public ComputedLaneOffsetYAxis getOffsetYAxis() {

        return offsetYAxis;
    }

    /**
     * A setter for the computed lane offset y axis frame.
     * 
     * @param offsetYAxis The computed lane offset y axis frame.
     */
    public void setOffsetYAxis(ComputedLaneOffsetYAxis offsetYAxis) {

        this.offsetYAxis = Objects.requireNonNull(offsetYAxis);
    }

    /**
     * A getter for the angle element.
     * 
     * @return The angle element.
     */
    public Angle getRotateXY() {

        return rotateXY;
    }

    /**
     * A setter for the angle element.
     * 
     * @param rotateXY The angle element to set.
     */
    public void setRotateXY(Angle rotateXY) {

        this.rotateXY = rotateXY;
    }

    /**
     * A setter for the angle element.
     * 
     * @param rotateXY The angle value to set.
     */
    public void setRotateXY(int rotateXY) {

        if (this.rotateXY == null) {

            this.rotateXY = new Angle();
        }
        this.rotateXY.setValue(rotateXY);
    }

    /**
     * A getter for the x axis scale b12 element.
     * 
     * @return The x axis scale b12 element.
     */
    public ScaleB12 getScaleXAxis() {

        return scaleXAxis;
    }

    /**
     * A setter for the x axis scale b12 element.
     * 
     * @param scaleXAxis The x axis scale b12 element to set.
     */
    public void setScaleXAxis(ScaleB12 scaleXAxis) {

        this.scaleXAxis = scaleXAxis;
    }

    /**
     * A setter for the x axis scale b12 element.
     * 
     * @param scaleXAxis The x axis scale b12 value to set.
     */
    public void setScaleXAxis(int scaleXAxis) {

        if (this.scaleXAxis == null) {

            this.scaleXAxis = new ScaleB12();
        }
        this.scaleXAxis.setValue(scaleXAxis);
    }

    /**
     * A getter for the y axis scale b12 element.
     * 
     * @return The y axis scale b12 element.
     */
    public ScaleB12 getScaleYAxis() {

        return scaleYAxis;
    }

    /**
     * A setter for the y axis scale b12 element.
     * 
     * @param scaleYAxis The y axis scale b12 element to set.
     */
    public void setScaleYAxis(ScaleB12 scaleYAxis) {

        this.scaleYAxis = scaleYAxis;
    }

    /**
     * A setter for the y axis scale b12 element.
     * 
     * @param scaleYAxis The y axis scale b12 value to set.
     */
    public void setScaleYAxis(int scaleYAxis) {

        if (this.scaleYAxis == null) {

            this.scaleYAxis = new ScaleB12();
        }
        this.scaleYAxis.setValue(scaleYAxis);
    }

    @Override
    public String encodeUPER() {

        StringBuilder computedLaneBits = new StringBuilder();
        StringBuilder optionalBits = new StringBuilder();

        // we aren't planning on having the extension usable so setting to off.
        optionalBits.append('0');

        computedLaneBits.append(referenceLaneId.encodeUPER());
        computedLaneBits.append(offsetXAxis.encodeUPER());
        computedLaneBits.append(offsetYAxis.encodeUPER());

        if (rotateXY != null) {

            optionalBits.append('1');
            computedLaneBits.append(rotateXY.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (scaleXAxis != null) {

            optionalBits.append('1');
            computedLaneBits.append(scaleXAxis.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (scaleYAxis != null) {

            optionalBits.append('1');
            computedLaneBits.append(scaleYAxis.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        // we aren't planning on having the regional extension usable so setting to off.
        optionalBits.append('0');

        computedLaneBits.insert(0, optionalBits);

        return computedLaneBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (ComputedLane.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a ComputedLane frame (%d)", ComputedLane.NUM_BITS));
        }

        String computedLaneOptionalBits = bits.substring(0, ComputedLane.NUM_BITS);
        bits = bits.substring(ComputedLane.NUM_BITS);

        if (computedLaneOptionalBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The ComputedLane extension is not supported");
        }

        if (computedLaneOptionalBits.charAt(4) != '0') {

            throw new IllegalArgumentException("The ComputedLane regional extension is not supported");
        }

        bits = referenceLaneId.decodeUPER(bits);
        bits = offsetXAxis.decodeUPER(bits);
        bits = offsetYAxis.decodeUPER(bits);

        if (computedLaneOptionalBits.charAt(1) == '1') {

            rotateXY = new Angle();
            bits = rotateXY.decodeUPER(bits);
        }

        if (computedLaneOptionalBits.charAt(2) == '1') {

            scaleXAxis = new ScaleB12();
            bits = scaleXAxis.decodeUPER(bits);
        }

        if (computedLaneOptionalBits.charAt(3) == '1') {

            scaleYAxis = new ScaleB12();
            bits = scaleYAxis.decodeUPER(bits);
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(referenceLaneId, offsetXAxis, offsetYAxis, rotateXY, scaleXAxis, scaleYAxis);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof ComputedLane)) {

            return false;
        }
        ComputedLane frame = (ComputedLane)object;
        return this.referenceLaneId.equals(frame.referenceLaneId)
                && this.offsetXAxis.equals(frame.offsetXAxis)
                && this.offsetYAxis.equals(frame.offsetYAxis)
                && Objects.equals(this.rotateXY, frame.rotateXY)
                && Objects.equals(this.scaleXAxis, frame.scaleXAxis)
                && Objects.equals(this.scaleYAxis, frame.scaleYAxis);
    }
}
