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

import org.etexascode.j2735_2016.elements.Elevation;
import org.etexascode.j2735_2016.elements.Latitude;
import org.etexascode.j2735_2016.elements.Longitude;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The position 3d frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class Position3D implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the position 3d.
     */
    public static final int NUM_BITS = 3;

    /**
     * The latitude element.
     */
    private Latitude latitude;

    /**
     * The longitude element.
     */
    private Longitude longitude;

    /**
     * The elevation element. (OPTIONAL)
     */
    private Elevation elevation;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public Position3D() {

        latitude = new Latitude();
        longitude = new Longitude();
    }

    /**
     * A constructor for the position 3d frame for all required fields.
     * 
     * @param latitude The latitude element.
     * @param longitude The longitude element.
     */
    public Position3D(Latitude latitude, Longitude longitude) {

        this.latitude = Objects.requireNonNull(latitude);
        this.longitude = Objects.requireNonNull(longitude);
    }

    /**
     * A constructor for the position 3d frame for all required fields (primitive).
     * 
     * @param latitude The latitude value.
     * @param longitude The longitude value.
     */
    public Position3D(int latitude, long longitude) {

        this.latitude = new Latitude(latitude);
        this.longitude = new Longitude(longitude);
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
     * A getter for the elevation element.
     * 
     * @return The elevation element.
     */
    public Elevation getElevation() {

        return elevation;
    }

    /**
     * A setter for the elevation element.
     * 
     * @param elevation The elevation element to set.
     */
    public void setElevation(Elevation elevation) {

        this.elevation = elevation;
    }

    /**
     * A setter for the elevation element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param elevation The elevation value to be set in the element.
     */
    public void setElevation(int elevation) {

        if (this.elevation == null) {

            this.elevation = new Elevation();
        }
        this.elevation.setValue(elevation);
    }

    @Override
    public String encodeUPER() {

        StringBuilder position3dBits = new StringBuilder();
        StringBuilder optionalBits = new StringBuilder();

        // we aren't planning on having the extension usable so setting to off.
        optionalBits.append('0');
        position3dBits.append(latitude.encodeUPER());
        position3dBits.append(longitude.encodeUPER());

        if (elevation != null) {

            optionalBits.append('1');
            position3dBits.append(elevation.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        // we aren't planning on having the regional extension usable so setting to off.
        optionalBits.append('0');

        position3dBits.insert(0, optionalBits);

        return position3dBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (Position3D.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a Position3D frame (%d)", Position3D.NUM_BITS));
        }

        String position3dOptionalBits = bits.substring(0, Position3D.NUM_BITS);
        bits = bits.substring(Position3D.NUM_BITS);

        if (position3dOptionalBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The Position3D extension is not supported");
        }

        if (position3dOptionalBits.charAt(2) != '0') {

            throw new IllegalArgumentException("The Position3D regional extension is not supported");
        }

        bits = latitude.decodeUPER(bits);
        bits = longitude.decodeUPER(bits);

        if (position3dOptionalBits.charAt(1) == '1') {

            elevation = new Elevation();
            bits = elevation.decodeUPER(bits);
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(latitude, longitude, elevation);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof Position3D)) {

            return false;
        }
        Position3D frame = (Position3D)object;
        return this.latitude.equals(frame.latitude)
                && this.longitude.equals(frame.longitude)
                && Objects.equals(this.elevation, frame.elevation);
    }
}
