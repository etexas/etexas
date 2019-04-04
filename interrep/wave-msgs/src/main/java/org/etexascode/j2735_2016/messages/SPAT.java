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
package org.etexascode.j2735_2016.messages;

import java.util.Objects;

import org.etexascode.j2735_2016.elements.MinuteOfTheYear;
import org.etexascode.j2735_2016.frames.IntersectionStateList;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The signal phase and timing message for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class SPAT implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the SPAT.
     */
    public static final int NUM_BITS = 4;

    /**
     * The minute of the year element. (OPTIONAL)
     */
    private MinuteOfTheYear timeStamp;

    /**
     * The intersection state list frame.
     */
    private IntersectionStateList intersections;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public SPAT() {

        intersections = new IntersectionStateList();
    }

    /**
     * A constructor for the SPAT message for all required fields.
     * 
     * @param intersections The intersection state list frame.
     */
    public SPAT(IntersectionStateList intersections) {

        this.intersections = Objects.requireNonNull(intersections);
    }

    /**
     * A getter for the minute of the year element.
     * 
     * @return The minute of the year element.
     */
    public MinuteOfTheYear getTimeStamp() {

        return timeStamp;
    }

    /**
     * A setter for the minute of the year element.
     * 
     * @param timeStamp The minute of the year element to set.
     */
    public void setTimeStamp(MinuteOfTheYear timeStamp) {

        this.timeStamp = timeStamp;
    }

    /**
     * A setter for the minute of the year element. Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param timeStamp The minute of the year value to be set in the element.
     */
    public void setTimeStamp(int timeStamp) {

        if (this.timeStamp == null) {

            this.timeStamp = new MinuteOfTheYear();
        }
        this.timeStamp.setValue(timeStamp);
    }

    /**
     * A getter for the intersection state list frame.
     * 
     * @return The intersection state list frame.
     */
    public IntersectionStateList getIntersections() {

        return intersections;
    }

    /**
     * A setter for the intersection state list frame.
     * 
     * @param intersections The intersection state list frame to set.
     */
    public void setIntersections(IntersectionStateList intersections) {

        this.intersections = Objects.requireNonNull(intersections);
    }

    @Override
    public String encodeUPER() {

        StringBuilder spatBits = new StringBuilder();
        StringBuilder optionalBits = new StringBuilder();

        // we aren't planning on having the extension usable so setting to off.
        optionalBits.append('0');

        if (timeStamp != null) {

            optionalBits.append('1');
            spatBits.append(timeStamp.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        // we aren't planning on having either the descriptive name or regional extension usable so
        // setting both to off.
        optionalBits.append("00");

        spatBits.append(intersections.encodeUPER());

        spatBits.insert(0, optionalBits);

        return spatBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (SPAT.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a SPAT frame (%d)", SPAT.NUM_BITS));
        }

        String spatOptionalBits = bits.substring(0, SPAT.NUM_BITS);
        bits = bits.substring(SPAT.NUM_BITS);

        if (spatOptionalBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The SPAT extension is not supported");
        }

        if (spatOptionalBits.charAt(2) != '0') {

            throw new IllegalArgumentException("The SPAT descriptive name is not supported");
        }

        if (spatOptionalBits.charAt(3) != '0') {

            throw new IllegalArgumentException("The SPAT regional extension is not supported");
        }

        if (spatOptionalBits.charAt(1) == '1') {

            timeStamp = new MinuteOfTheYear();
            bits = timeStamp.decodeUPER(bits);
        }

        bits = intersections.decodeUPER(bits);

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(timeStamp, intersections);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof SPAT)) {

            return false;
        }
        SPAT message = (SPAT)object;
        return Objects.equals(this.timeStamp, message.timeStamp)
                && this.intersections.equals(message.intersections);
    }
}
