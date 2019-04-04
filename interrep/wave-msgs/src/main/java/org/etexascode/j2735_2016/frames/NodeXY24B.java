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

import org.etexascode.j2735_2016.elements.OffsetB12;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The node xy 24b frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class NodeXY24B implements UnalignedPackedEncodingRules {

    /**
     * The x offset b12 element.
     */
    private OffsetB12 x;

    /**
     * The y offset b12 element.
     */
    private OffsetB12 y;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public NodeXY24B() {

        this.x = new OffsetB12();
        this.y = new OffsetB12();
    }

    /**
     * A constructor for the node xy 24b.
     * 
     * @param x The x offset b12 element.
     * @param y The y offset b12 element.
     */
    public NodeXY24B(OffsetB12 x, OffsetB12 y) {

        this.x = Objects.requireNonNull(x);
        this.y = Objects.requireNonNull(y);
    }

    /**
     * A constructor for the node xy 24b way which allows primitive/enumerated data to be passed to
     * all primitive/enumerated elements.
     * 
     * @param x The x coordinate value.
     * @param y The y coordinate value.
     */
    public NodeXY24B(int x, int y) {

        this.x = new OffsetB12(x);
        this.y = new OffsetB12(y);
    }

    /**
     * A getter for the x offset b12 element.
     * 
     * @return The x offset b12 element.
     */
    public OffsetB12 getX() {

        return x;
    }

    /**
     * A setter for the x offset b12 element.
     * 
     * @param x The x offset b12 element to set.
     */
    public void setX(OffsetB12 x) {

        this.x = Objects.requireNonNull(x);
    }

    /**
     * A setter for the x offset b12 element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param x The x coordinate value to be set in the element.
     */
    public void setX(int x) {

        this.x.setValue(x);
    }

    /**
     * A getter for the y offset b12 element.
     * 
     * @return The y offset b12 element.
     */
    public OffsetB12 getY() {

        return y;
    }

    /**
     * A setter for the y offset b12 element.
     * 
     * @param y The y offset b12 element to set.
     */
    public void setY(OffsetB12 y) {

        this.y = Objects.requireNonNull(y);
    }

    /**
     * A setter for the y offset b12 element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param y The y coordinate value to be set in the element.
     */
    public void setY(int y) {

        this.y.setValue(y);
    }

    @Override
    public String encodeUPER() {

        StringBuilder nodeXY24Bits = new StringBuilder();

        nodeXY24Bits.append(x.encodeUPER());
        nodeXY24Bits.append(y.encodeUPER());

        return nodeXY24Bits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        bits = x.decodeUPER(bits);
        bits = y.decodeUPER(bits);

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(x, y);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof NodeXY24B)) {

            return false;
        }
        NodeXY24B frame = (NodeXY24B)object;
        return this.x.equals(frame.x)
                && this.y.equals(frame.y);
    }
}
