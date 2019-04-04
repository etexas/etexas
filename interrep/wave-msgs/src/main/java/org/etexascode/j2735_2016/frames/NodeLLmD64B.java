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

import org.etexascode.j2735_2016.elements.Latitude;
import org.etexascode.j2735_2016.elements.Longitude;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The node longitude latitude micro degree 64b frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class NodeLLmD64B implements UnalignedPackedEncodingRules {

    /**
     * The longitude element.
     */
    private Longitude longitude;

    /**
     * The latitude element.
     */
    private Latitude latitude;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public NodeLLmD64B() {

        this.longitude = new Longitude();
        this.latitude = new Latitude();
    }

    /**
     * A constructor for the node longitude latitude micro degree 64b.
     * 
     * @param longitude The longitude element.
     * @param latitude The latitude element.
     */
    public NodeLLmD64B(Longitude longitude, Latitude latitude) {

        this.longitude = Objects.requireNonNull(longitude);
        this.latitude = Objects.requireNonNull(latitude);
    }

    /**
     * A constructor for the node longitude latitude micro degree 64b which allows
     * primitive/enumerated data to be passed to all primitive/enumerated elements.
     * 
     * @param longitude The longitude value.
     * @param latitude The latitude value.
     */
    public NodeLLmD64B(long longitude, int latitude) {

        this.longitude = new Longitude(longitude);
        this.latitude = new Latitude(latitude);
    }

    /**
     * A getter for the longitude element.
     * 
     * @return The longitude element.
     */
    public Longitude getLongitude() {

        return longitude;
    }

    /**
     * A setter for the longitude element.
     * 
     * @param longitude The longitude element to set.
     */
    public void setLongitude(Longitude longitude) {

        this.longitude = Objects.requireNonNull(longitude);
    }

    /**
     * A setter for the longitude element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param longitude The longitude value to be set in the element.
     */
    public void setLongitude(long longitude) {

        this.longitude.setValue(longitude);
    }

    /**
     * A getter for the latitude element.
     * 
     * @return The latitude element.
     */
    public Latitude getLatitude() {

        return latitude;
    }

    /**
     * A setter for the latitude element.
     * 
     * @param latitude The latitude element to set.
     */
    public void setLatitude(Latitude latitude) {

        this.latitude = Objects.requireNonNull(latitude);
    }

    /**
     * A setter for the latitude element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param latitude The latitude value to be set in the element.
     */
    public void setLatitude(int latitude) {

        this.latitude.setValue(latitude);
    }

    @Override
    public String encodeUPER() {

        StringBuilder nodeLLmD64Bits = new StringBuilder();

        nodeLLmD64Bits.append(longitude.encodeUPER());
        nodeLLmD64Bits.append(latitude.encodeUPER());

        return nodeLLmD64Bits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        bits = longitude.decodeUPER(bits);
        bits = latitude.decodeUPER(bits);

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(longitude, latitude);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof NodeLLmD64B)) {

            return false;
        }
        NodeLLmD64B frame = (NodeLLmD64B)object;
        return this.longitude.equals(frame.longitude)
                && this.latitude.equals(frame.latitude);
    }
}
