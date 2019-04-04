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

import org.etexascode.j2735_2016.elements.AdvisorySpeedType;
import org.etexascode.j2735_2016.elements.AdvisorySpeedType.Advisory;
import org.etexascode.j2735_2016.elements.RestrictionClassID;
import org.etexascode.j2735_2016.elements.SpeedAdvice;
import org.etexascode.j2735_2016.elements.SpeedConfidence;
import org.etexascode.j2735_2016.elements.SpeedConfidence.SpeedPrecision;
import org.etexascode.j2735_2016.elements.ZoneLength;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The advisory speed frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class AdvisorySpeed implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the advisory speed.
     */
    public static final int NUM_BITS = 6;

    /**
     * The advisory speed type element.
     */
    private AdvisorySpeedType type;

    /**
     * The speed advice element. (OPTIONAL)
     */
    private SpeedAdvice speed;

    /**
     * The speed confidence element. (OPTIONAL)
     */
    private SpeedConfidence confidence;

    /**
     * The zone length element. (OPTIONAL)
     */
    private ZoneLength distance;

    /**
     * The restriction class ID element. (OPTIONAL)
     */
    private RestrictionClassID restrictedClass;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public AdvisorySpeed() {

        type = new AdvisorySpeedType();
    }

    /**
     * A constructor for the advisory speed frame for all required fields.
     * 
     * @param type The advisory speed type element.
     */
    public AdvisorySpeed(AdvisorySpeedType type) {

        this.type = Objects.requireNonNull(type);
    }

    /**
     * A constructor for the advisory speed frame for all required fields (primitive).
     * 
     * @param type The advisory enumeration.
     */
    public AdvisorySpeed(Advisory type) {

        this.type = new AdvisorySpeedType(Objects.requireNonNull(type));
    }

    /**
     * A getter for the advisory speed type element.
     * 
     * @return The advisory speed type element.
     */
    public AdvisorySpeedType getType() {

        return type;
    }

    /**
     * A setter for the advisory speed type element.
     * 
     * @param type The advisory speed type element to set.
     */
    public void setType(AdvisorySpeedType type) {

        this.type = Objects.requireNonNull(type);
    }

    /**
     * A setter for the advisory speed type element. Allows enumeration data to be passed to the
     * enumeration element.
     * 
     * @param type The advisory enumeration to be set in the element.
     */
    public void setType(Advisory type) {

        this.type.setEnumeration(Objects.requireNonNull(type));
    }

    /**
     * A getter for the speed advice element.
     * 
     * @return The speed advice element.
     */
    public SpeedAdvice getSpeed() {

        return speed;
    }

    /**
     * A setter for the speed advice element.
     * 
     * @param speed The speed advice element to set.
     */
    public void setSpeed(SpeedAdvice speed) {

        this.speed = speed;
    }

    /**
     * A setter for the speed advice element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param speed The speed advice value to be set in the element.
     */
    public void setSpeed(int speed) {

        if (this.speed == null) {

            this.speed = new SpeedAdvice();
        }
        this.speed.setValue(speed);
    }

    /**
     * A getter for the speed confidence element.
     * 
     * @return The speed confidence element.
     */
    public SpeedConfidence getConfidence() {

        return confidence;
    }

    /**
     * A setter for the speed confidence element.
     * 
     * @param confidence The speed confidence element to set.
     */
    public void setConfidence(SpeedConfidence confidence) {

        this.confidence = confidence;
    }

    /**
     * A setter for the speed confidence element. Allows enumeration data to be passed to the
     * enumeration element.
     * 
     * @param confidence The speed precision enumeration to be set in the element.
     */
    public void setConfidence(SpeedPrecision confidence) {

        if (this.confidence == null) {

            this.confidence = new SpeedConfidence();
        }
        this.confidence.setEnumeration(Objects.requireNonNull(confidence));
    }

    /**
     * A getter for the zone length element.
     * 
     * @return The zone length element.
     */
    public ZoneLength getDistance() {

        return distance;
    }

    /**
     * A setter for the zone length element.
     * 
     * @param distance The zone length element to set.
     */
    public void setDistance(ZoneLength distance) {

        this.distance = distance;
    }

    /**
     * A setter for the zone length element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param distance The zone length value to be set in the element.
     */
    public void setDistance(int distance) {

        if (this.distance == null) {

            this.distance = new ZoneLength();
        }
        this.distance.setValue(distance);
    }

    /**
     * A getter for the restriction class ID element.
     * 
     * @return The restriction class ID element.
     */
    public RestrictionClassID getRestrictedClass() {

        return restrictedClass;
    }

    /**
     * A setter for the restriction class ID element.
     * 
     * @param restrictedClass The restriction class ID element to set.
     */
    public void setRestrictedClass(RestrictionClassID restrictedClass) {

        this.restrictedClass = restrictedClass;
    }

    /**
     * A setter for the restriction class ID element. Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param restrictedClass The restriction class ID value to be set in the element.
     */
    public void setRestrictedClass(int restrictedClass) {

        if (this.restrictedClass == null) {

            this.restrictedClass = new RestrictionClassID();
        }
        this.restrictedClass.setValue(restrictedClass);
    }

    @Override
    public String encodeUPER() {

        StringBuilder advisorySpeedBits = new StringBuilder();
        StringBuilder optionalBits = new StringBuilder();

        // we aren't planning on having the extension usable so setting to off.
        optionalBits.append('0');

        advisorySpeedBits.append(type.encodeUPER());

        if (speed != null) {

            optionalBits.append('1');
            advisorySpeedBits.append(speed.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (confidence != null) {

            optionalBits.append('1');
            advisorySpeedBits.append(confidence.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (distance != null) {

            optionalBits.append('1');
            advisorySpeedBits.append(distance.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (restrictedClass != null) {

            optionalBits.append('1');
            advisorySpeedBits.append(restrictedClass.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        // we aren't planning on having the regional extension usable so setting to off.
        optionalBits.append('0');
        advisorySpeedBits.insert(0, optionalBits);

        return advisorySpeedBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (AdvisorySpeed.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for an AdvisorySpeed frame (%d)", AdvisorySpeed.NUM_BITS));
        }

        String advisorySpeedOptionalBits = bits.substring(0, AdvisorySpeed.NUM_BITS);
        bits = bits.substring(AdvisorySpeed.NUM_BITS);

        if (advisorySpeedOptionalBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The AdvisorySpeed extension is not supported");
        }

        if (advisorySpeedOptionalBits.charAt(5) != '0') {

            throw new IllegalArgumentException("The AdvisorySpeed regional extension is not supported");
        }

        bits = type.decodeUPER(bits);

        if (advisorySpeedOptionalBits.charAt(1) == '1') {

            speed = new SpeedAdvice();
            bits = speed.decodeUPER(bits);
        }

        if (advisorySpeedOptionalBits.charAt(2) == '1') {

            confidence = new SpeedConfidence();
            bits = confidence.decodeUPER(bits);
        }

        if (advisorySpeedOptionalBits.charAt(3) == '1') {

            distance = new ZoneLength();
            bits = distance.decodeUPER(bits);
        }

        if (advisorySpeedOptionalBits.charAt(4) == '1') {

            restrictedClass = new RestrictionClassID();
            bits = restrictedClass.decodeUPER(bits);
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(type, speed, confidence, distance, restrictedClass);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof AdvisorySpeed)) {

            return false;
        }
        AdvisorySpeed frame = (AdvisorySpeed)object;
        return this.type.equals(frame.type)
                && Objects.equals(this.speed, frame.speed)
                && Objects.equals(this.confidence, frame.confidence)
                && Objects.equals(this.distance, frame.distance)
                && Objects.equals(this.restrictedClass, frame.restrictedClass);
    }
}
