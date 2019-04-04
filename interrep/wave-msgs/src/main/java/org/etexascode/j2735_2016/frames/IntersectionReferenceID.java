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

import org.etexascode.j2735_2016.elements.IntersectionID;
import org.etexascode.j2735_2016.elements.RoadRegulatorID;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The intersection reference ID frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class IntersectionReferenceID implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the intersection reference ID.
     */
    public static final int NUM_BITS = 1;

    /**
     * The road regulator ID element. (OPTIONAL)
     */
    private RoadRegulatorID region;

    /**
     * The intersection ID element.
     */
    private IntersectionID id;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public IntersectionReferenceID() {

        id = new IntersectionID();
    }

    /**
     * A constructor for the intersection reference ID frame for all required fields.
     * 
     * @param id The intersection ID element.
     */
    public IntersectionReferenceID(IntersectionID id) {

        this.id = Objects.requireNonNull(id);
    }

    /**
     * A constructor for the intersection reference ID frame for all required fields (primitive).
     * 
     * @param id The intersection ID value.
     */
    public IntersectionReferenceID(int id) {

        this.id = new IntersectionID(id);
    }

    /**
     * A getter for the road regulator ID element.
     * 
     * @return The road regulator ID element.
     */
    public RoadRegulatorID getRegion() {

        return region;
    }

    /**
     * A setter for the road regulator ID element.
     * 
     * @param region The road regulator ID element to set.
     */
    public void setRegion(RoadRegulatorID region) {

        this.region = region;
    }

    /**
     * A setter for the road regulator ID element. Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param region The road regulator ID value to be set in the element.
     */
    public void setRegion(int region) {

        if (this.region == null) {

            this.region = new RoadRegulatorID();
        }
        this.region.setValue(region);
    }

    /**
     * A getter for the intersection ID element.
     * 
     * @return The intersection ID element.
     */
    public IntersectionID getId() {

        return id;
    }

    /**
     * A setter for the intersection ID element.
     * 
     * @param id The intersection ID element to set.
     */
    public void setId(IntersectionID id) {

        this.id = Objects.requireNonNull(id);
    }

    /**
     * A setter for the intersection ID element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param id The intersection ID value to be set in the element.
     */
    public void setId(int id) {

        this.id.setValue(id);
    }

    @Override
    public String encodeUPER() {

        StringBuilder intersectionReferenceIdBits = new StringBuilder();
        StringBuilder optionalBits = new StringBuilder();

        if (region != null) {

            optionalBits.append('1');
            intersectionReferenceIdBits.append(region.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        intersectionReferenceIdBits.append(id.encodeUPER());

        intersectionReferenceIdBits.insert(0, optionalBits);

        return intersectionReferenceIdBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (IntersectionReferenceID.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for an IntersectionReferenceID frame (%d)", IntersectionReferenceID.NUM_BITS));
        }

        String interseectionReferenceIdOptionalBits = bits.substring(0, IntersectionReferenceID.NUM_BITS);
        bits = bits.substring(IntersectionReferenceID.NUM_BITS);

        if (interseectionReferenceIdOptionalBits.charAt(0) == '1') {

            region = new RoadRegulatorID();
            bits = region.decodeUPER(bits);
        }

        bits = id.decodeUPER(bits);

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(region, id);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof IntersectionReferenceID)) {

            return false;
        }
        IntersectionReferenceID frame = (IntersectionReferenceID)object;
        return Objects.equals(this.region, frame.region)
                && this.id.equals(frame.id);
    }
}
