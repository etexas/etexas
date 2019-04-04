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

import org.etexascode.j2735_2016.elements.VehicleLength;
import org.etexascode.j2735_2016.elements.VehicleWidth;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The vehicle size frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class VehicleSize implements UnalignedPackedEncodingRules {

    /**
     * The vehicle width element.
     */
    private VehicleWidth width;

    /**
     * The vehicle length element.
     */
    private VehicleLength length;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public VehicleSize() {

        this.width = new VehicleWidth();
        this.length = new VehicleLength();
    }

    /**
     * A constructor for the vehicle size.
     * 
     * @param width The vehicle width element.
     * @param length The vehicle length element.
     */
    public VehicleSize(VehicleWidth width, VehicleLength length) {

        this.width = Objects.requireNonNull(width);
        this.length = Objects.requireNonNull(length);
    }

    /**
     * A constructor for the vehicle size which allows primitive/enumerated data to be passed to all
     * primitive/enumerated elements.
     * 
     * @param width The vehicle width value.
     * @param length The vehicle length value.
     */
    public VehicleSize(int width, int length) {

        this.width = new VehicleWidth(width);
        this.length = new VehicleLength(length);
    }

    /**
     * A getter for the vehicle width element.
     * 
     * @return The vehicle width element.
     */
    public VehicleWidth getWidth() {

        return width;
    }

    /**
     * A setter for the vehicle width element.
     * 
     * @param width The vehicle width element to set.
     */
    public void setWidth(VehicleWidth width) {

        this.width = Objects.requireNonNull(width);
    }

    /**
     * A setter for the vehicle width element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param width The vehicle width value to be set in the element.
     */
    public void setWidth(int width) {

        this.width.setValue(width);
    }

    /**
     * A getter for the vehicle length element.
     * 
     * @return The vehicle length element.
     */
    public VehicleLength getLength() {

        return length;
    }

    /**
     * A setter for the vehicle length element.
     * 
     * @param length The vehicle length element to set.
     */
    public void setLength(VehicleLength length) {

        this.length = Objects.requireNonNull(length);
    }

    /**
     * A setter for the vehicle length element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param length The vehicle length value to be set in the element.
     */
    public void setLength(int length) {

        this.length.setValue(length);
    }

    @Override
    public String encodeUPER() {

        StringBuilder vehicleSize = new StringBuilder();

        vehicleSize.append(width.encodeUPER());
        vehicleSize.append(length.encodeUPER());

        return vehicleSize.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        bits = width.decodeUPER(bits);
        bits = length.decodeUPER(bits);

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(width, length);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof VehicleSize)) {

            return false;
        }
        VehicleSize frame = (VehicleSize)object;
        return this.width.equals(frame.width)
                && this.length.equals(frame.length);
    }
}
